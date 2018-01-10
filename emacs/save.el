;disable backup
(setq backup-inhibited t)
;disable auto save
(setq auto-save-default nil)

(defvar etc-already-saving nil)

(defun etc-buffer-needs-saving (&optional b)
  (unless b
    (setq b (current-buffer)))
  (and (buffer-modified-p b)
       (buffer-file-name b)
       (not (md-special-buffer-p b))))

;; check if any buffers need saving before actually
;; saving them, to avoid spurious messages.
(defun etc-save-if-necessary ()
  (unless (or etc-already-saving
              (minibufferp))
    (let ((etc-already-saving t))
      (ignore-errors
        (when (reduce (lambda (a b) (or a b))
                      (remove-if-not #'etc-buffer-needs-saving (buffer-list))
                      :initial-value nil)
          (save-some-buffers t #'etc-buffer-needs-saving))))))

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
