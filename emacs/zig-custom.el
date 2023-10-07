(use-package
  zig-mode
  :ensure t
  )

(add-hook 'zig-mode-hook #'lsp-deferred)

(setq lsp-zig-zls-executable "~/zls/zig-out/bin/zls")

(define-key zig-mode-map (kbd "M-.") 'lsp-find-definition)
(define-key zig-mode-map (kbd "M-,") 'lsp-find-references)

(define-key zig-mode-map (kbd "C-h g") #'lsp-describe-thing-at-point)


;; some personal preferences
(defun etc-zig-misc ()
  (subword-mode 1)
  (setq c-hungry-delete-key t)
  (local-set-key (kbd "C-d") 'c-hungry-delete-forward)
  (local-set-key (kbd "<delete>") 'c-hungry-delete-forward)
  (local-set-key (kbd "<backspace>") 'c-hungry-delete-backwards)
  )

(add-hook 'zig-mode-hook #'etc-zig-misc)
