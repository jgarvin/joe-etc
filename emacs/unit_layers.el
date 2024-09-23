;; what does double tap correspond to, since it still needs to map to
;; a key? prefix the whole thing with C-c?
(global-set-key (kbd "C-SPC") #'etc-set-mark-or-expand-region)

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
(define-key smartparens-mode-map (kbd "C-M-u") 'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-c C-M-u") 'sp-backward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-M-p") 'sp-forward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-c C-M-p") 'sp-backward-barf-sexp)

;; do

;; have to decide what double taps key are
(define-key smartparens-mode-map (kbd "C-M-j") 'sp-splice-sexp)
(define-key smartparens-mode-map (kbd "C-M-i") 'sp-split-sexp)
;;(define-key smartparens-mode-map (kbd "C-S-u") 'sp-backward-up-sexp)
;;(define-key smartparens-mode-map (kbd "C-S-d") 'sp-backward-down-sexp)

