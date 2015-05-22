;; projectile slows things down if you call (projectile-current-project-files)
;; constantly, so we cache the results
(defvar md-projectile-files nil)

(defun md-get-projectile-files-impl ()
  (condition-case nil
      (if (or (derived-mode-p 'dired-mode)
              buffer-file-name)
          (projectile-current-project-files) nil)
    (error nil)))

(defun md-update-projectile-files ()
  (when (and (boundp 'projectile-mode) projectile-mode
             ;; doing while minibuffer window is up annoys TRAMP sudo
             (not (active-minibuffer-window)))
    (let ((md-update-projectile-files t))
      (setq md-projectile-files (md-get-projectile-files-impl)))))

(add-hook 'md-window-selection-hook #'md-update-projectile-files)
