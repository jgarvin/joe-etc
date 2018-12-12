(use-package
  smartparens
  :ensure t)

;; (add-to-list 'load-path "~/etc/smartparens")
(require 'smartparens-config)
(smartparens-global-mode 1)
(show-smartparens-global-mode 1)

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

;; ;; TODO: figure out what to change xmonad binds to for next/prev window
;; ;; freeing up e and i
;; ;; TODO: add split, rewrap

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



;; without these you can't dive into a string -- allegedly obsolete in newer smartparens versions
;;(add-to-list 'sp-navigate-consider-stringlike-sexp 'c++-mode)
;;(add-to-list 'sp-navigate-consider-stringlike-sexp 'emacs-lisp-mode)
;;(add-to-list 'sp-navigate-consider-stringlike-sexp 'sh-mode)
;;(add-to-list 'sp-navigate-consider-stringlike-sexp 'shell-mode)

;; but python has a bug: https://github.com/Fuco1/smartparens/issues/473
;;(add-to-list 'sp-navigate-consider-stringlike-sexp 'python-mode)
;;(add-to-list 'sp-navigate-consider-stringlike-sexp 'inferior-python-mode)

;; (defadvice sp-show--pair-function (around sp-show--pair-function-disable-large activate)
;;   (unless (or (> (buffer-size) (* 1 256 1024))
;;               (not smartparens-mode)) ;; why is this necessary? strangely is.
;;     ad-do-it))
