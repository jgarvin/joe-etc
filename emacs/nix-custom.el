(defun etc-nix-misc ()
  (setq c-hungry-delete-key t)
  (local-set-key (kbd "C-d") 'c-hungry-delete-forward)
  (local-set-key (kbd "<delete>") 'c-hungry-delete-forward)
  (local-set-key (kbd "<backspace>") 'c-hungry-delete-backwards)
  )

(add-hook 'nix-mode-hook #'etc-nix-misc)
