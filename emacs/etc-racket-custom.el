(require 'racket-mode)

(defun etc-racket-mode-hook ()
  (subword-mode 1)
  (setq c-hungry-delete-key t)
  (local-set-key (kbd "C-d") 'c-hungry-delete-forward)
  (local-set-key (kbd "<delete>") 'c-hungry-delete-forward)
  (local-set-key (kbd "<backspace>") 'c-hungry-delete-backwards)
  (setq indent-tabs-mode nil)
  (etc-set-indent-preference 4)
  (sp-pair "'" nil :actions :rem)
  (sp-pair "`" nil :actions :rem)
  ;; (define-key racket-mode-map (kbd "C-c ") nil)
  )

(add-hook 'racket-mode-hook #'etc-racket-mode-hook)

