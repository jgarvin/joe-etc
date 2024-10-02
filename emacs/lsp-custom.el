(use-package
  lsp-mode
  :ensure t)

(setq lsp-file-watch-threshold 500)

(global-set-key (kbd "C-h g") #'lsp-describe-thing-at-point)

;; without this ccls will put reference counts next to every variable and function
(setq lsp-lens-enable nil)

(setq lsp-enable-on-type-formatting nil)

(setq lsp-enable-indentation nil)

