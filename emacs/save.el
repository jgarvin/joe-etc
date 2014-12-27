;disable backup
(setq backup-inhibited t)
;disable auto save
(setq auto-save-default nil)

;; builtin autosave randomly stops working for no reason,
;; so implement my own
(setq etc-save-timer
      (run-at-time "1 sec" 1
                   (lambda ()
                     (when (reduce (lambda (a b) (or a b))
                                   (mapcar (lambda (b)
                                             (and (buffer-modified-p b)
                                                  (not (null (buffer-file-name b)))))
                                           (buffer-list)))
                       (save-some-buffers t nil)
                       (message nil)))))

;; (dolist (timer timer-list)
;;   (when (string-match "mapcar" (format "%S" timer))
;;     (cancel-timer timer)))

;; save when emacs loses focus
(when
   (and (featurep 'x) window-system)
 (defvar on-blur--saved-window-id 0 "Last known focused window.")
 (defvar on-blur--timer nil "Timer refreshing known focused window.")
 (defun on-blur--refresh ()
   "Runs on-blur-hook if emacs has lost focus."
   (let* ((active-window (x-window-property
                          "_NET_ACTIVE_WINDOW" nil "WINDOW" 0 nil t))
          (active-window-id (if (numberp active-window)
                                active-window
                              (string-to-number
                               (format "%x%04x"
                                       (car active-window)
                                       (cdr active-window)) 16)))
          (emacs-window-id (string-to-number
                            (frame-parameter nil 'outer-window-id))))
     (when (and
            (= emacs-window-id on-blur--saved-window-id)
            (not (= active-window-id on-blur--saved-window-id)))
       (run-hooks 'on-blur-hook))
     (setq on-blur--saved-window-id active-window-id)
     (run-with-timer 1 nil 'on-blur--refresh)))
 (add-hook 'on-blur-hook #'(lambda () (save-some-buffers t nil)))
 (on-blur--refresh))

;; make sure autosave calls after-save-hook, for some reason
;; doesn't by default
(defadvice do-auto-save (after after-auto-save activate)
  (run-hooks 'after-save-hook))
(defadvice save-some-buffers (after after-save-some-buffers activate)
  (run-hooks 'after-save-hook))

;; autosave under all these circumstances too, never want to save
;; manually
(defadvice switch-to-buffer (before save-buffer-now activate)
  (when buffer-file-name (save-buffer) (message nil)))
(defadvice other-window (before other-window-now activate)
  (when buffer-file-name (save-buffer) (message nil)))
(defadvice other-frame (before other-frame-now activate)
  (when buffer-file-name (save-buffer) (message nil)))

;; ;; filter annoying messages
;; (defvar message-filter-regexp-list '("^(No changes need to be saved)$"
;;                                      "^Saving file .*$"
;;                                      "^Wrote .*$")
;;   "filter formatted message string to remove noisy messages")

;; (setq message-filter-regexp-list '("^(No changes need to be saved)$"
;;                                    "^Saving file .*"
;;                                    "^Wrote .*"
;;                                    ;;"^End of buffer$"
;;                                    ;;"^Beginning of buffer$"
;;                                    "^Mark set$"))

;; (defvar etc-last-message "")
;; (defvar etc-message-hook nil)

;; ;; TODO: filtering 'Mark set' prevents mark from appearing?!
;; (defadvice message (around message-filter-by-regexp activate)
;;   (if (not (ad-get-arg 0))
;;       ad-do-it
;;     (let ((formatted-string (apply 'format (ad-get-args 0)))
;;           (deactivate-mark nil)
;;           (inhibit-read-only t))
;;       (if (and (stringp formatted-string)
;;                (some (lambda (re) (string-match re formatted-string)) message-filter-regexp-list))
;;           (save-excursion
;;             (set-buffer "*Messages*")
;;             (goto-char (point-max))
;;             (insert formatted-string "\n"))
;;         (progn
;;           (setq etc-last-message formatted-string)
;;           (run-hooks etc-message-hook)
;;           (ad-set-args 0 `("%s" ,formatted-string))
;;           ad-do-it)))))
;; ;;(message "test")
;; (remove-function :around 'message)
;; (ad-unadvise 'message)

;; (defadvice message (around message-save-to-var activate)
;;   (if (not (ad-get-arg 0))
;;       ad-do-it
;;     (let ((formatted-string (apply 'format (ad-get-args 0))))
;;       (when (stringp formatted-string)
;;         (setq etc-last-message formatted-string))
;;       ad-do-it)))

;; so I can't be tempted to do by hand
(global-unset-key "\C-x\C-s")
