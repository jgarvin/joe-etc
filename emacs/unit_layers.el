;; keep paste consistently available on the same key across all modes

(global-set-key (kbd "C-M-y") #'yank)
(define-key smartparens-mode-map (kbd "C-M-l") 'sp-beginning-of-sexp)
(define-key smartparens-mode-map (kbd "C-M-/") 'sp-end-of-sexp)
(define-key smartparens-mode-map (kbd "C-M-i") 'sp-next-sexp)
(define-key smartparens-mode-map (kbd "C-c C-M-i") 'sp-transpose-sexp)
(define-key smartparens-mode-map (kbd "C-M-n") 'sp-previous-sexp)
;;(define-key smartparens-mode-map (kbd "C-c C-M-n") 'sp-backward-transpose-sexp)
(define-key smartparens-mode-map (kbd "C-M-u") 'sp-down-sexp)
(define-key smartparens-mode-map (kbd "C-M-o") 'sp-up-sexp)
(define-key smartparens-mode-map (kbd "C-M-c") 'sp-unwrap-sexp)
(define-key smartparens-mode-map (kbd "C-M-r") 'sp-rewrap-sexp)
(define-key smartparens-mode-map (kbd "C-M-j") 'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-c C-M-j") 'sp-backward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-M-'") 'sp-forward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-c C-M-'") 'sp-backward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp)
(define-key smartparens-mode-map (kbd "C-M-p") 'sp-split-sexp)
(define-key smartparens-mode-map (kbd "C-M-/") 'sp-splice-sexp)

;; do I really want it all on one hand? so that you can more easily chain actions

;; have to decide what double taps key are
;;(define-key smartparens-mode-map (kbd "C-S-u") 'sp-backward-up-sexp)
;;(define-key smartparens-mode-map (kbd "C-S-d") 'sp-backward-down-sexp)

