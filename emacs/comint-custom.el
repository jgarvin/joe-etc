(setq-default
 ;;tramp-default-method "ssh"          ; uses ControlMaster
 comint-scroll-to-bottom-on-input t  ; always insert at the bottom
 comint-scroll-to-bottom-on-output nil ; always add output at the bottom
 comint-scroll-show-maximum-output t ; scroll to show max possible output
 comint-completion-autolist t     ; show completion list when ambiguous
 comint-input-ignoredups t           ; no duplicates in command history
 comint-completion-addsuffix t       ; insert space/slash after file completion
 comint-buffer-maximum-size 20000    ; max length of the buffer in lines
 comint-prompt-read-only t         ; if this is t, it breaks shell-command
 comint-get-old-input (lambda () "") ; what to run when i press enter on a
                                        ; line above the current prompt
 comint-input-ring-size 5000         ; max shell history size
 protect-buffer-bury-p nil
 )

(defvar-local etc-next-truncate-allowed-timer nil)

(defun etc-comint-truncate ()
  (when (and comint-buffer-maximum-size
             (> (buffer-size) comint-buffer-maximum-size)
             (get-buffer-process (current-buffer)))
    (comint-truncate-buffer)
    (unless etc-next-truncate-allowed-timer
      (setq etc-next-truncate-allowed-timer
            (run-at-time 1 1 #'etc-clear-truncate-timer (current-buffer))))))

(defun etc-cancel-truncate-timer ()
  (when (timerp etc-next-truncate-allowed-timer)
    (cancel-timer etc-next-truncate-allowed-timer)
    (setq etc-next-truncate-allowed-timer nil)))

(add-hook 'kill-buffer-hook #'etc-cancel-truncate-timer)

(defun etc-clear-truncate-timer (buffer)
  ;; technically we should never have to check if buffer is killed
  ;; because our kill-buffer-hook should take care of making sure
  ;; that the timer is canceled... but it happens anyway sometimes,
  ;; not sure why.
  (when (buffer-name buffer)
    (with-current-buffer buffer
      (etc-cancel-truncate-timer)
      (etc-comint-truncate))))

(defun etc-setup-delayed-truncate (&optional unused)
  (unless etc-next-truncate-allowed-timer
    (etc-comint-truncate)))

;; If you truncate continously it causes too much performance trouble,
;; the truncation starves I/O to all other processes, so mandimus gets
;; disconnected.
(add-hook 'comint-output-filter-functions #'etc-setup-delayed-truncate)

(defun etc-comint-mode-hook ()
  (toggle-truncate-lines 1)
  ;; Scrolling performance is heavily adversely affected without
  ;; a defer time. We want tailing logs to be fast.
  (make-local-variable 'jit-lock-defer-timer)
  (set (make-local-variable 'jit-lock-defer-time) 0.25)
  ;; prevent jump scrolling on new lines
  (set (make-local-variable 'scroll-conservatively) 10))



(add-hook 'comint-mode-hook #'etc-comint-mode-hook)

(defun enter-again-if-enter ()
  "Make the return key select the current item in minibuf and shell history isearch.
An alternate approach would be after-advice on isearch-other-meta-char."
  (when (and (not isearch-mode-end-hook-quit)
             (equal (this-command-keys-vector) [13])) ; == return
    (cond ((active-minibuffer-window) (minibuffer-complete-and-exit))
          ;; ((member (buffer-name) my-shells) (comint-send-input))
          )))
(add-hook 'isearch-mode-end-hook 'enter-again-if-enter)

(defadvice comint-previous-matching-input
    (around suppress-history-item-messages activate)
  "Suppress the annoying 'History item : NNN' messages from shell history isearch.
If this isn't enough, try the same thing with
comint-replace-by-expanded-history-before-point."
  (let ((old-message (symbol-function 'message)))
    (unwind-protect
      (progn (fset 'message 'ignore) ad-do-it)
      (fset 'message old-message))))

(defadvice comint-send-input (around go-to-end-of-multiline activate)
  "When I press enter, jump to the end of the *buffer*, instead of the end of
the line, to capture multiline input. (This only has effect if
`comint-eol-on-send' is non-nil."
  (flet ((end-of-line () (end-of-buffer)))
    ad-do-it))
;; not sure why, but comint needs to be reloaded from the source (*not*
;; compiled) elisp to make the above advise stick.
(load "comint.el.gz")

;; comint history code taken from
;; https://oleksandrmanzyuk.wordpress.com/2011/10/23/a-persistent-command-history-in-emacs/

(defun comint-write-history-on-exit (process event)
  (comint-write-input-ring)
  (let ((buf (process-buffer process)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (insert (format "\nProcess %s %s" process event))))))

(defun turn-on-comint-history ()
  ;; shell already has its own history and handling
  (unless (derived-mode-p 'shell-mode)
    (let ((process (get-buffer-process (current-buffer))))
      (when process
        (setq comint-input-ring-file-name
              (format "~/.emacs.d/%s-history" major-mode))
        (comint-read-input-ring)
        (set-process-sentinel process
                              #'comint-write-history-on-exit)))))

(defun mapc-buffers (fn)
  (mapc (lambda (buffer)
          (with-current-buffer buffer
            (funcall fn)))
        (buffer-list)))

(defun comint-write-input-ring-all-buffers ()
  (mapc-buffers 'comint-write-input-ring))

(setq etc-save-comint-hist-timer
      (run-with-idle-timer 1 t #'comint-write-input-ring-all-buffers))
;; (cancel-timer etc-save-comint-hist-timer)

(add-hook 'comint-mode-hook #'turn-on-comint-history)
(add-hook 'kill-buffer-hook #'comint-write-input-ring)
(add-hook 'kill-emacs-hook #'comint-write-input-ring-all-buffers)

; make completion buffers disappear after 3 seconds.
;; (add-hook 'completion-setup-hook
;;           (lambda () (run-at-time 3 nil
;;                                   (lambda () (delete-windows-on "*Completions*")))))

;; The undo history should be cleared when we send input. We shouldn't be able
;; to undo things that were inserted by the process, only things we typed.
(defun etc-clear-comint-undo (&rest args)
  (setq buffer-undo-list nil))

;; output from the underlying process shouldn't be undoable
(defun etc-prevent-undo-recording-comint (original-function &rest args)
  (let ((buffer-undo-list t))
    (apply original-function args)))

;; had this originally then got rid of, does it cause issues?
;; TODO: change which shells it applies to...
;; maybe need a new buffer category for these output shells?
(defun make-my-shell-output-read-only (text)
  "Add to comint-output-filter-functions to make stdout read only in my shells."
  ;;(if (member (buffer-name) my-shells)
      (let ((inhibit-read-only t)
            (output-end (process-mark (get-buffer-process (current-buffer)))))
        (put-text-property comint-last-output-start output-end 'read-only t)))
  ;;)
;;(remove-hook 'comint-output-filter-functions 'make-my-shell-output-read-only)

(advice-add #'comint-send-input :after #'etc-clear-comint-undo)
(advice-add #'comint-output-filter :around #'etc-prevent-undo-recording-comint)
