(custom-set-variables
 ;;'(tramp-default-method "ssh")          ; uses ControlMaster
 '(comint-scroll-to-bottom-on-input t)  ; always insert at the bottom
 '(comint-scroll-to-bottom-on-output nil) ; always add output at the bottom
 '(comint-scroll-show-maximum-output t) ; scroll to show max possible output
 '(comint-completion-autolist t)     ; show completion list when ambiguous
 '(comint-input-ignoredups t)           ; no duplicates in command history
 '(comint-completion-addsuffix t)       ; insert space/slash after file completion
 '(comint-buffer-maximum-size 20000)    ; max length of the buffer in lines
 '(comint-prompt-read-only t)         ; if this is t, it breaks shell-command
 '(comint-get-old-input (lambda () "")) ; what to run when i press enter on a
                                        ; line above the current prompt
 '(comint-input-ring-size 5000)         ; max shell history size
 '(protect-buffer-bury-p nil)
)

;; things like 'less' only work in real terminals
(setenv "PAGER" "cat")

;;(setq process-adaptive-read-buffering t)
(defun etc-shell-mode-hook ()
  (toggle-truncate-lines 1)
  ;; Scrolling performance is heavily adversely affected without
  ;; a defer time. We want tailing logs to be fast.
  (make-local-variable 'jit-lock-defer-timer)
  (set (make-local-variable 'jit-lock-defer-time) 0.25)
    ;; make it so I can hit enter on error messages from gcc
  ;; to open the file at that location
  (compilation-shell-minor-mode 1)
  (shell-dirtrack-mode -1)
  (dirtrack-mode 1)
  (setq dirtrack-list '("^[^@:\n]+@[^:\n]+:\\([^]]+\\)][$#]" 1)))

(add-hook 'shell-mode-hook #'etc-shell-mode-hook)

(defvar-local etc-next-truncate-allowed-timer nil)

(defun etc-comint-truncate ()
  (when (and comint-buffer-maximum-size
             (> (buffer-size) comint-buffer-maximum-size))
    (comint-truncate-buffer)
    (setq etc-next-truncate-allowed-timer
          (run-at-time 1 1 #'etc-clear-truncate-timer (current-buffer)))))

(defun etc-cancel-truncate-timer ()
  (when (timerp etc-next-truncate-allowed-timer)
    (cancel-timer etc-next-truncate-allowed-timer)
    (setq etc-next-truncate-allowed-timer nil)))

(add-hook 'kill-buffer-hook #'etc-cancel-truncate-timer)

(defun etc-clear-truncate-timer (buffer)
  (with-current-buffer buffer
    (etc-cancel-truncate-timer)
    (etc-comint-truncate)))

(defun etc-setup-delayed-truncate (&optional unused)
  (unless etc-next-truncate-allowed-timer
    (etc-comint-truncate)))

;; If you truncate continously it causes too much performance trouble,
;; the truncation starves I/O to all other processes, so mandimus gets
;; disconnected.
(add-hook 'comint-output-filter-functions #'etc-setup-delayed-truncate)

;; (defun make-my-shell-output-read-only (text)
;;   "Add to comint-output-filter-functions to make stdout read only in my shells."
;;   ;; (if (member (buffer-name) my-shells)
;;       (let ((inhibit-read-only t)
;;             (output-end (process-mark (get-buffer-process (current-buffer)))))
;;         (put-text-property comint-last-output-start output-end 'read-only t)))
;; ;; )
;; (add-hook 'comint-output-filter-functions 'make-my-shell-output-read-only)

; interpret and use ansi color codes in shell output windows
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; make it harder to kill my shell buffers
;;(require 'protbuf)
;;(add-hook 'shell-mode-hook 'protect-process-buffer-from-kill-mode)

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
