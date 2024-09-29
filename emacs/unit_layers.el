;; keep paste consistently available on the same key across all modes
(global-set-key (kbd "C-M-y") #'yank)

;; TODO: reverse transpose doesn't undo transpose, shouldn't work that
;; way

;; TODO: double taps for begin and end could also be for moving
;; current subunit to beginning/end of parent unit

;; TODO: next/previous should never imply up/down

;; (defun etc-current-sexp ()
;;   (let* ((candidate-sexp (sp-get-sexp))
;;          (candidate-beg (plist-get candidate-sexp :beg))
;;          (candidate-end (plist-get candidate-sexp :end)))
;;     (cond
;;      ((= (point) candidate-beg)))))

;; C-c corresponds to a double tap
(define-key smartparens-mode-map (kbd "C-M-l") #'sp-beginning-of-sexp)
(define-key smartparens-mode-map (kbd "C-M-/") #'sp-end-of-sexp)
(define-key smartparens-mode-map (kbd "C-M-n") #'sp-previous-sexp)
(define-key smartparens-mode-map (kbd "C-c C-M-n") #'etc-backward-transpose-sexp)
(define-key smartparens-mode-map (kbd "C-M-i") #'sp-next-sexp)
(define-key smartparens-mode-map (kbd "C-c C-M-i") #'sp-transpose-sexp)
(define-key smartparens-mode-map (kbd "C-M-e") #'sp-down-sexp)
(define-key smartparens-mode-map (kbd "C-M-o") #'sp-up-sexp)
(define-key smartparens-mode-map (kbd "C-M-c") #'sp-unwrap-sexp)
(define-key smartparens-mode-map (kbd "C-M-r") #'sp-rewrap-sexp)
(define-key smartparens-mode-map (kbd "C-M-'") #'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-c C-M-'") #'sp-backward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-M-,") #'sp-forward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-c C-M-,") #'sp-backward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-M-k") #'etc-cut-sexp)
(define-key smartparens-mode-map (kbd "C-M-p") #'sp-split-sexp)
(define-key smartparens-mode-map (kbd "C-M-;") #'sp-splice-sexp)
(define-key smartparens-mode-map (kbd "C-M-j") #'etc-copy-sexp)

(defun etc-backward-transpose-sexp ()
  (interactive)
  (sp-transpose-sexp)
  (sp-previous-sexp)
  (sp-previous-sexp)
  (sp-next-sexp))

(defun etc-copy-sexp ()
  (interactive)
  (save-excursion
    (sp-up-sexp)
    (etc-kill #'kill-ring-save 'sexp)))

(defun etc-cut-sexp ()
  (interactive)
  (save-excursion
    (sp-up-sexp)
    (etc-kill #'kill-region 'sexp)))

(defun etc-kill (action thing)
  (let ((bounds (bounds-of-thing-at-point thing)))
    (funcall action (car bounds) (cdr bounds))))

