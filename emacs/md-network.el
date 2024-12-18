;; -*- lexical-binding: t -*-

;; needed for with-timeout
(require 'timer)

(defvar md-server-clients nil)
(defvar md-server-eval-timeout 60)
(defvar md-server-connect-hook nil)
(defvar md-server-disconnect-hook nil)
(defvar md-server-pending-actions nil)
(defvar md-server-execute-pending-timer nil)
(defvar md-server-port 23233)
(defvar-local md-server-log-count 10000)

;; TODO: apparently emacs 29.1 added with-undo-amalgamate which makes
;; everything inside a block count as one undo action, which would be
;; an improvement. Really we want it around the multiple actions
;; triggered by an utterance though which may be a mix of elisp and
;; key shorcuts, so not perfect.

;; Yes this is a mutex,
;; necessary because if executing any actions
;; causes more IO the server filter may run again,
;; causing us to process more input,
;; causing more actions to execute!
(defvar md-executing-actions-p nil)

;; hacked around this for now
;; ;; You are rightly wondering why this is here. The reason
;; ;; is that both emacs and mandimus are single threaded,
;; ;; and they use blocking communication because.. I'm lazy
;; ;; and haven't fixed that yet. When you have the visual bell
;; ;; enabled and scroll to the end of a buffer using a foot
;; ;; pedal, without this you will get an infinite loop. Mandimus
;; ;; sends a down arrow keydown event, then you hit the end of
;; ;; the buffer, then emacs showing the visual bell prevents it
;; ;; from ever taking data off the socket, then mandimus blocks
;; ;; waiting for a reply from emacs, so then the pedal up event
;; ;; is never received, so then the down arrow continues to be
;; ;; held, which triggers the visual bell AGAIN..
 (setq ring-bell-function #'ignore)

(defun md-execute-pending ()
  (unless (or md-executing-actions-p
              ;; try to make sure all pending keystrokes are drained
              ;; before executing commands. the hope is this will make
              ;; things less racey when emacs is a remote X11 window,
              ;; racing between keyboard events from X11 and commands
              ;; from the mandimus socket.
              ;; Makes everything hang and timeout terribly.
;;              (input-pending-p)
              )
    (let ((md-executing-actions-p t))
      (unwind-protect
          (while md-server-pending-actions
            (funcall (pop md-server-pending-actions)))
        (setq md-server-pending-actions nil)
        ;; (when md-server-execute-pending-timer
          ;; (cancel-timer md-server-execute-pending-timer))
        ;; (setq md-server-execute-pending-timer nil)
        ))))

(defun md-server-start nil
  (interactive)
  (unless (process-status "mandimus-eval-server")
    (make-network-process :name "mandimus-eval-server"
                          :server t
                          :service md-server-port
                          :buffer "*mandimus-server*"
                          :family 'ipv4
                          :sentinel 'md-server-sentinel
                          :filter 'md-server-filter)
    (setq md-server-clients '())))

(defun md-server-stop nil
  (interactive)
  (while md-server-clients
    (delete-process (car (car md-server-clients)))
    (setq md-server-clients (cdr md-server-clients)))
  (when md-server-execute-pending-timer
    (cancel-timer md-server-execute-pending-timer))
  (setq md-server-pending-actions nil)
  (delete-process "mandimus-eval-server")
  (kill-buffer "*mandimus-server*")
  (setq md-executing-actions-p nil))

(defun md-server-restart ()
  (message "restarting mandimus server")
  (when (process-status "mandimus-eval-server")
    (md-server-stop))
  (md-server-start))

(defun md-server-filter (proc string)
  (or (catch 'restart
        (let ((pending (assoc proc md-server-clients))
              message
              index
              command
              result)
          ;;create entry if required
          (unless pending
            (setq md-server-clients (cons (cons proc "") md-server-clients))
            (setq pending  (assoc proc md-server-clients)))
          (setq message (concat (cdr pending) string))
          (while (setq index (string-match "\n" message))
            (setq index (1+ index))
            (setq command (substring message 0 index))
            ;; we don't actually execute the actions while we're in the filter
            ;; because that tends to cause disconnects
            (setq md-server-pending-actions
                  (append md-server-pending-actions
                          (list (lambda ()
                                  ;; (message "runing")
                                  (setq result
                                        (condition-case err
                                            (eval (car (read-from-string command)))
                                          (user-error (message "Error: %s" (error-message-string err)))
                                          (error (message "Mandimus error: [%S] in [%S]" (error-message-string err) command))))
                                  (let ((print-length nil))
                                    (setq result (format "%S" result)))
                                  ;; because newline is the protocol delimeter we have to nuke it
                                  (setq result (replace-regexp-in-string "\n" "" result))
                                  (setq result (concat result "\n"))
                                  (let ((status (process-status proc)))
                                    (if (eq status 'open)
                                        (progn
                                          (process-send-string proc result)
                                          (md-server-log command proc))
                                      ;; not really user error but don't want back traces
                                      (user-error "Couldn't run command, server status: %S" status)))))))
            (md-execute-pending)
            ;; (unless md-server-execute-pending-timer
            ;;   ;; (message "scheduling")
            ;;   (setq md-server-execute-pending-timer
            ;;         ;; you don't want to do this on idle timer,
            ;;         ;; because then you can't to do C-c commands
            ;;         ;; when shell output is scrolling by
            ;;        (run-with-timer 0 1 #'md-execute-pending)
            ;;           ;; (run-with-idle-timer 0 t #'md-execute-pending)
            ;;         ))
            (unwind-protect
                (setq message (substring message index))
              (setcdr pending message)))
          t))
      (md-server-restart)))

(defun md-server-sentinel (proc msg)
  (cond
   ((string= msg "connection broken by remote peer\n")
    (setq md-servers-clients (assq-delete-all proc md-server-clients))
    (md-server-log (format "client %s has quit" proc))
    (run-hooks 'md-server-disconnect-hook))
   ((string-match-p "open from .*\n" msg)
    (run-hooks 'md-server-connect-hook))))

;;from server.el
(defun md-server-log (string &optional client)
  "If a *mandimus-server* buffer exists, write STRING to it for logging purposes."
  ;; (if (get-buffer "*mandimus-server*")
  ;;     (with-current-buffer "*mandimus-server*"
  ;;       (unless buffer-read-only
  ;;         (read-only-mode t))
  ;;       (let ((inhibit-read-only t))
  ;;         (setq md-server-log-count (1+ md-server-log-count))
  ;;         (when (> md-server-log-count 10000)
  ;;             (goto-char (point-min))
  ;;             (delete-region (point-min) (save-excursion (end-of-line))))
  ;;         (goto-char (point-max))
  ;;         (insert (current-time-string)
  ;;                 (if client (format " %s:" client) " ")
  ;;                 string)
  ;;         (or (bolp) (newline)))))
  )

;; always run so mandimus can hook up
(condition-case nil
    (md-server-start)
  (file-error (message "ERROR: Mandimus server already running!")))

;; (md-server-start)
;; (md-server-stop)

(require 'buffer-tail)
(toggle-buffer-tail "*mandimus-server*" "on")

(defun etc-check-substring (a b)
  (string-match-p (regexp-quote a) b))


;; This makes it possible to figure out how to connect emacs just from
;; looking at the X window properties of the frame.
(defun md-network-annotate-frames (&optional new-frame)
  (when (getenv "DISPLAY")
    (dolist (f (frame-list))
      (when (window-system f) ;; daemon mode creates frame not associated w/ windowing system!
        (unless (etc-check-substring "mandimus[" (format "%s" frame-title-format))
          ;; get the information into the window title where xpra can find it
          ;; (setq-default frame-title-format "%b")
          (when (stringp (default-value 'frame-title-format)) ;; if it's a string turn it into a list
            (setq-default frame-title-format (list (default-value 'frame-title-format)))
            (message "setting default frame title format %s" frame-title-format))
          (let ((new-list (copy-tree (default-value 'frame-title-format))))
            (message "trying to add to title format " (list " mandimus[" (system-name) ":" (number-to-string  md-server-port) "]"))
            (add-to-list 'new-list '(:eval (list " mandimus[" (system-name) ":" (number-to-string  md-server-port) "]")) t)
            (setq-default frame-title-format new-list))
          )
        ;; these are the ones that matter if you ask xdotool for the
        ;; currently selected window
        (when (functionp #'x-change-window-property)
          (x-change-window-property "mandimus_server_host" (system-name) f nil nil nil)
          (x-change-window-property "mandimus_server_port" (number-to-string md-server-port) f nil nil nil)
          ;; these matter if you run xprop on the frame. shouldn't
          ;; actually matter for mandimus but here just to make things
          ;; more understandable when debugging.
          (x-change-window-property "mandimus_server_host" (system-name) f nil nil t)
          (x-change-window-property "mandimus_server_port" (number-to-string md-server-port) f nil nil t))))))

(add-hook 'after-make-frame-functions #'md-network-annotate-frames)
;; only annotating after frame creation doesn't seem to be sufficient
;; not sure why. observed when using with remote X forwarding
(add-hook 'focus-out-hook #'md-network-annotate-frames)
