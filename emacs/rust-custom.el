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


;; enable syntax checking error underlining that will occur every time
;; you save.
;; superseded by language server support!
(use-package
  flycheck-rust
  :ensure t)

(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; display compile errors under the code where they happen
(use-package
  flycheck-inline
  :ensure t)

(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-inline-mode))


;; Hook up the rust language server which enables code completion and
;; other nice features.
;; note that you first must run the following shell command:
;; $ rustup component add rls rust-analysis rust-src
;; (use-package lsp-mode
;;  :ensure t
;;  :config
;;  (setq lsp-print-io t)
;;  ;; (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls"))
;;  (setenv "RUST_BACKTRACE" "full")
;;  (setenv "RUST_LOG" "rls::=debug")
;;  ;; Fix problem seems to be caused by upgrading lsp-mode package to v3.
;;  (unless (fboundp 'lsp-rust-enable)
;;  (defun lsp-rust-enable ()
;;  (require 'lsp-clients)
;;  ;; We don't realy need lsp-rust-rls-command for now, but we will support it
;;  (when (boundp 'lsp-rust-rls-command)
;;  (lsp-register-client
;;  (make-lsp-client :new-connection (lsp-stdio-connection lsp-rust-rls-command)
;;  :major-modes '(rust-mode)
;;  :server-id 'rls
;;  :notification-handlers (lsp-ht ("window/progress" 'lsp-clients--rust-window-progress)))))
;;  (lsp)))
;;  )

(use-package
  lsp-mode
  :ensure t)

(add-hook 'rust-mode-hook #'lsp)

;; (use-package
;;   lsp-rust
;;   :ensure t
;;   ;;:pin melpa-stable
;;   )

;; (defun etc-activate-rust-lsp ()
;;   (setq lsp-rust-rls-command '("rustup" "run" "stable" "rls"))
;;   (require 'lsp-rust))

;; (add-hook 'rust-mode-hook #'etc-activate-rust-lsp)

;; (use-package
;;   lsp-flycheck
;;   :ensure t)

;; (global-flycheck-mode 0)
;; alternatively to enable only for rust:
(add-hook 'rust-mode-hook #'flycheck-mode)

(define-key rust-mode-map (kbd "M-.") 'lsp-find-definition)
(define-key rust-mode-map (kbd "M-,") 'lsp-find-references)


(defun etc-rust-misc ()
  (subword-mode 1)
  (setq c-hungry-delete-key t)
  (local-set-key (kbd "C-d") 'c-hungry-delete-forward)
  (local-set-key (kbd "<delete>") 'c-hungry-delete-forward)
  (local-set-key (kbd "<backspace>") 'c-hungry-delete-backwards)
  )

(add-hook 'rust-mode-hook #'etc-rust-misc)