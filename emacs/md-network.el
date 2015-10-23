;; -*- lexical-binding: t -*-

;; needed for with-timeout
(require 'timer)

(defvar md-server-clients nil)
(defvar md-server-eval-timeout 60)
(defvar md-server-connect-hook nil)
(defvar md-server-disconnect-hook nil)
(defvar md-server-pending-actions nil)
(defvar md-server-execute-pending-timer nil)

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
;; (setq ring-bell-function #'ignore)

(defun md-execute-pending ()
  (unwind-protect
      (while md-server-pending-actions
        (funcall (pop md-server-pending-actions)))
    (setq md-server-pending-actions nil)
    (when md-server-execute-pending-timer
      (cancel-timer md-server-execute-pending-timer))
    (setq md-server-execute-pending-timer nil)))

(defun md-server-start nil
  (interactive)
  (unless (process-status "mandimus-eval-server")
    (make-network-process :name "mandimus-eval-server"
                          :server t
                          :service 23233
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
  (delete-process "mandimus-eval-server"))

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
            (unless md-server-execute-pending-timer
              ;; (message "scheduling")
              (setq md-server-execute-pending-timer (run-with-idle-timer 0 t #'md-execute-pending)))
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
  (if (get-buffer "*mandimus-server*")
      (with-current-buffer "*mandimus-server*"
        (unless buffer-read-only
          (read-only-mode t))
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (insert (current-time-string)
                  (if client (format " %s:" client) " ")
                  string)
          (or (bolp) (newline))))))

;; always run so mandimus can hook up
(condition-case nil
    (md-server-start)
  (file-error (message "ERROR: Mandimus server already running!")))

(require 'buffer-tail)
(toggle-buffer-tail "*mandimus-server*" "on")
