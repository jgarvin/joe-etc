(defun etc-elisp-setup ()
  (setq my-indent-size 4)
  (setq indent-tabs-mode nil)
  (setq default-tab-width my-indent-size)
  (setq tab-width my-indent-size)

  (require 'cc-mode)

  (local-set-key (kbd "C-d") 'c-hungry-delete-forward)
  (local-set-key (kbd "<delete>") 'c-hungry-delete-forward)
  (local-set-key (kbd "<backspace>") 'c-hungry-delete-backwards)
  )

(add-hook 'emacs-lisp-mode-hook 'etc-elisp-setup)
