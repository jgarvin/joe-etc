(add-to-list 'load-path "~/etc/smartparens")
(require 'smartparens-config)
(smartparens-global-mode 1)
(show-smartparens-global-mode t)

;; Commented out hotkeys need free keyboard shortcuts not taken
;; by XMonad.

;; Related problem is figuring out how to make pedals useful when using voice

;; my homerow
;; a
;; s
;; h
;; t
;; g*
;; y*
;; n
;; e
;; o
;; i

;; available keys:
;; C-M-tab
;; C-M-]
;; C-M-left
;; C-M-right
;; C-M-up
;; C-M-down

;; TODO: figure out what to change xmonad binds to for next/prev window
;; freeing up e and i
;; TODO: add split, rewrap

(define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
(define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)
(define-key smartparens-mode-map (kbd "C-M-d") 'sp-down-sexp)
(define-key smartparens-mode-map (kbd "C-S-d") 'sp-backward-down-sexp)
(define-key smartparens-mode-map (kbd "C-M-o") 'sp-up-sexp)
(define-key smartparens-mode-map (kbd "C-S-u") 'sp-backward-up-sexp)
(define-key smartparens-mode-map (kbd "C-M-n") 'sp-next-sexp)
(define-key smartparens-mode-map (kbd "C-M-p") 'sp-previous-sexp)
(define-key smartparens-mode-map (kbd "C-<home>") 'sp-beginning-of-sexp)
(define-key smartparens-mode-map (kbd "C-<end>") 'sp-end-of-sexp)
(define-key smartparens-mode-map (kbd "C-M-t") 'sp-transpose-sexp)
(define-key smartparens-mode-map (kbd "C-M-y") 'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-M-g") 'sp-splice-sexp)


;; without these you can't dive into a string
(add-to-list 'sp-navigate-consider-stringlike-sexp 'c++-mode)
(add-to-list 'sp-navigate-consider-stringlike-sexp 'emacs-lisp-mode)
(add-to-list 'sp-navigate-consider-stringlike-sexp 'sh-mode)
(add-to-list 'sp-navigate-consider-stringlike-sexp 'shell-mode)

;; but python has a bug: https://github.com/Fuco1/smartparens/issues/473
;;(add-to-list 'sp-navigate-consider-stringlike-sexp 'python-mode)
;;(add-to-list 'sp-navigate-consider-stringlike-sexp 'inferior-python-mode)
