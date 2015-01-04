;; projectile slows things down if you call (projectile-current-project-files)
;; constantly, so we cache the results
(defvar md-projectile-files nil)
(defvar md-updating-projectile-files nil)

(defun md-get-projectile-files-impl ()
  (condition-case nil
      (if (or (equal major-mode 'dired-mode)
              buffer-file-name)
          (projectile-current-project-files) nil)
    (error nil)))

(defun md-update-projectile-files ()
  (unless (or md-updating-projectile-files
              (not (eq (current-buffer) (window-buffer)))
              (window-minibuffer-p)
              (minibufferp))
    (let ((md-updating-projectile-files t))
      (setq md-projectile-files (md-get-projectile-files-impl)))))
          
(add-hook 'buffer-list-update-hook #'md-update-projectile-files)
;;(add-hook 'buffer-list-update-hook #'md-update-projectile-files)
