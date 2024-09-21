(use-package
  smartparens
  :ensure t)

(smartparens-global-mode 1)
(show-smartparens-global-mode 1)

(define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
(define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)
(define-key smartparens-mode-map (kbd "C-M-d") 'sp-down-sexp)
(define-key smartparens-mode-map (kbd "C-M-<down>") 'sp-down-sexp)
(define-key smartparens-mode-map (kbd "C-S-d") 'sp-backward-down-sexp)
(define-key smartparens-mode-map (kbd "C-M-o") 'sp-up-sexp)
(define-key smartparens-mode-map (kbd "C-M-<up>") 'sp-up-sexp)
(define-key smartparens-mode-map (kbd "C-S-u") 'sp-backward-up-sexp)
(define-key smartparens-mode-map (kbd "C-M-u") 'sp-unwrap-sexp)
(define-key smartparens-mode-map (kbd "C-M-n") 'sp-next-sexp)
(define-key smartparens-mode-map (kbd "C-M-p") 'sp-previous-sexp)
(define-key smartparens-mode-map (kbd "C-M-<home>") 'sp-beginning-of-sexp)
(define-key smartparens-mode-map (kbd "C-M-<end>") 'sp-end-of-sexp)
(define-key smartparens-mode-map (kbd "C-M-x") 'sp-transpose-sexp)
(define-key smartparens-mode-map (kbd "C-M-y") 'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-M-g") 'sp-backward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-M-v") 'sp-forward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-M-k") 'sp-backward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-M-j") 'sp-splice-sexp)
(define-key smartparens-mode-map (kbd "C-M-i") 'sp-split-sexp)
(define-key smartparens-mode-map (kbd "C-M-z") 'sp-rewrap-sexp)

;; w/o this highlighting text and pressing parens doesn't wrap in C++
;; https://github.com/Fuco1/smartparens/issues/840
(require 'cc-mode)
(dolist (key '("(" ")" "{" "}")) (define-key c-mode-base-map (kbd key) nil) )

