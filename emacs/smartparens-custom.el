(use-package
  smartparens
  :ensure t)

(smartparens-global-mode 1)
(show-smartparens-global-mode 1)
(sp-local-pair '(emacs-lisp-mode) "'" "'" :actions nil)
(sp-local-pair '(emacs-lisp-mode) "`" "`" :actions nil)
;; w/o this highlighting text and pressing parens doesn't wrap in C++
;; https://github.com/Fuco1/smartparens/issues/840
(require 'cc-mode)
(dolist (key '("(" ")" "{" "}")) (define-key c-mode-base-map (kbd key) nil) )

