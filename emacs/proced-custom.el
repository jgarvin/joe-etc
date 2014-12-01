(defun proced-settings ()
  (setq proced-auto-update-interval 1)
  (proced-toggle-auto-update t))

(add-hook 'proced-mode-hook 'proced-settings)
