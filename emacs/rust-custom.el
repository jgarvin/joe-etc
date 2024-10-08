;;; package -- rust customizations
;;; commentary:
;;; Code:

;; install the basic language mode which gives syntax highlighting
;; and the ability to format the code
(use-package
  rust-mode
  :ensure t)

;; for editing cargo files
(use-package
  toml-mode
  :ensure t)

;; (use-package
;;   flycheck-rust
;;   :ensure t)

;; (with-eval-after-load 'rust-mode
;;   (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; display compile errors under the code where they happen
;; comment out for now because it has trouble with mandimus snippets
;;(use-package
;;  flycheck-inline
;;  :ensure t)

;; (with-eval-after-load 'flycheck
;;   (add-hook 'flycheck-mode-hook #'flycheck-inline-mode))

;;(remove-hook 'flycheck-mode-hook #'flycheck-inline-mode)

;; (global-flycheck-mode 0)
;; alternatively to enable only for rust:
;;(add-hook 'rust-mode-hook #'flycheck-mode)


;; Hook up the rust language server which enables code completion and
;; other nice features.
;; note that you first must run the following shell command:
;; $ rustup component add rls rust-analysis rust-src
(setq lsp-rust-server 'rust-analyzer)

;;(use-package
;;   lsp-ui
;;   :ensure t)

;;(global-ls)

(add-hook 'rust-mode-hook #'lsp)
;;(remove-hook 'lsp-mode-hook #'lsp-ui-mode)

(define-key rust-mode-map (kbd "M-.") 'lsp-find-definition)
(define-key rust-mode-map (kbd "M-,") 'lsp-find-references)






;; some personal preferences
(defun etc-rust-misc ()
  (subword-mode 1)
  (setq c-hungry-delete-key t)
  (local-set-key (kbd "C-d") 'c-hungry-delete-forward)
  (local-set-key (kbd "<delete>") 'c-hungry-delete-forward)
  (local-set-key (kbd "<backspace>") 'c-hungry-delete-backwards)
  )

(add-hook 'rust-mode-hook #'etc-rust-misc)

(setq lsp-enable-snippet nil)

;; hack to workaround https://github.com/flycheck/flycheck-inline/issues/7
(defun flycheck-relevant-error-other-file-p (x)
  nil)

;; otherwise sub crates inside workspaces count as distinct projects
(setq projectile-project-root-files (delete "Cargo.toml" projectile-project-root-files))

(define-key rust-mode-map (kbd "C-c e") #'lsp-rust-analyzer-expand-macro)

(setq rust-format-on-save nil)

;; disable error underlining because it is always broken
(setq-default lsp-diagnostic-package nil)
(setq lsp-diagnostic-package nil)

;; necessary for analyzer to know about proc macros
(setq lsp-rust-analyzer-cargo-load-out-dirs-from-check t)
(setq lsp-rust-analyzer-proc-macro-enable t)
(setq lsp-rust-analyzer-cargo-run-build-scripts t)

;; don't try to pair squote used for lifetime specifiers
(sp-local-pair 'rust-mode "'" nil :actions nil)