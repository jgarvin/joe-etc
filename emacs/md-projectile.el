;; projectile slows things down if you call (projectile-current-project-files)
;; constantly, so we cache the results
(defvar md-projectile-files nil)
(defvar md-projectile-files-last-project nil)
(defvar md-projectile-files-lazy-timer nil)
;; projectile freezes sudo prompts if you query for this when they're up
(defvar md-projectile-projects nil)
;; protect against recursive invocation
(defvar md-updating-projectile-files nil)

(defun md-get-projectile-files-impl ()
  (condition-case nil
      (if (or (derived-mode-p 'dired-mode)
              buffer-file-name)
          (projectile-current-project-files) nil)
    (error nil)))

(defun md-update-projectile-files ()
  (when (and (boundp 'projectile-mode) projectile-mode
             ;; doing while minibuffer window is up annoys TRAMP sudo
             (not (active-minibuffer-window))
             (not md-updating-projectile-files))
    (let ((md-updating-projectile-files t)
          (current-project (etc-get-project)))
      (setq md-projectile-projects (projectile-relevant-known-projects))
      (when (not (string= current-project md-projectile-files-last-project))
        (setq md-projectile-files-last-project current-project)
        (setq md-projectile-files (md-get-projectile-files-impl))))))

(defun md-lazy-projectile-files-update ()
  (md-run-when-idle-once 'md-projectile-files-lazy-timer #'md-update-projectile-files 0.75 nil))

(add-hook 'md-window-selection-hook #'md-lazy-projectile-files-update)
