;disable backup
(setq backup-inhibited t)
;disable auto save
(setq auto-save-default nil)

(defun etc-buffer-needs-saving (&optional b)
  (unless b
    (setq b (current-buffer)))
  (and (buffer-modified-p b)
       (buffer-file-name b)))

(defun etc-save-if-necessary ()
  (with-demoted-errors
    (when (reduce (lambda (a b) (or a b))
                (mapcar (lambda (b)
                          (etc-buffer-needs-saving b))
                        (buffer-list)))
    (save-some-buffers t nil))))

;; builtin autosave randomly stops working for no reason,
;; so implement my own
;; (setq etc-save-timer
;;       (run-at-time t 1 #'etc-save-if-necessary))

;;(cancel-timer etc-save-timer)

;; (dolist (v timer-list)
;;   (when (string-match-p "etc-save-if-necessary" (format "%S" v))
;;     (cancel-timer v)))

;; autosave under all these circumstances too, never want to save
;; manually
(add-hook 'focus-out-hook #'etc-save-if-necessary)
(add-hook 'focus-in-hook #'etc-save-if-necessary)
(defadvice switch-to-buffer (before save-buffer-now activate)
  (etc-save-if-necessary))
(defadvice other-window (before other-window-now activate)
  (etc-save-if-necessary))
(defadvice other-frame (before other-frame-now activate)
  (etc-save-if-necessary))

;; so I can't be tempted to do by hand
(global-unset-key "\C-x\C-s")

;; (dolist (timer timer-list)
;;   (when (string-match "etc-save-if-necessary" (format "%S" timer))
;;     (cancel-timer timer)))
