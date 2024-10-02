;; keep paste consistently available on the same key across all modes
;;(global-set-key (kbd "C-M-y") #'yank)
;;(global-unset-key (kbd "C-M-y"))

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
(define-key smartparens-mode-map (kbd "C-c C-M-i") #'etc-transpose-sexp)
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
(define-key smartparens-mode-map (kbd "C-M-\\") #'sp-splice-sexp)
(define-key smartparens-mode-map (kbd "C-M-;") #'etc-comment-sexp)
(define-key smartparens-mode-map (kbd "C-M-j") #'etc-copy-sexp)
(define-key smartparens-mode-map (kbd "C-M-y") #'etc-duplicate-sexp)

;; (equal o (plist-get (sp-get-thing) :beg) (point))
;; if we are on the opener, then that is the current sexp
;; if we are not on the opener, then it is the containing sexp
;; expect that differs from the node like view
;; beginning of sexp or beginning of symbol?

(defun debug-dump ()
  (interactive)
  (message "%s" (buffer-substring (plist-get (sp-get-thing) :beg) (plist-get (sp-get-thing) :end))))

(global-set-key (kbd "C-M-z") #'debug-dump)

(defun etc-sp-get-thing ()
  (let ((sexp (sp-get-thing)))
    (if (and (md-likely-followed-by-closer (1- (point)))
             (not (md-likely-preceded-by-opener (1+ (point)))))
        (save-excursion
          (goto-char (1- (point)))
          (sp-get-thing))
      (sp-get-thing))))

(defun etc-transpose-sexp ()
  (interactive)
  (let* ((sexp (etc-sp-get-thing))
         (beg (plist-get sexp :beg))
         (end (plist-get sexp :end)))
    (when (= (point) beg)
      (goto-char end))
    (sp-transpose-sexp)
    (goto-char (- (point) (- end beg)))))

(defun etc-backward-transpose-sexp ()
  (interactive)
  (let* ((sexp (etc-sp-get-thing))
         (beg (plist-get sexp :beg))
         (end (plist-get sexp :end)))
    (when (= (point) end)
      (goto-char beg))
    (sp-transpose-sexp)
    (sp-previous-sexp)
    (sp-previous-sexp)
    (sp-next-sexp)))

;; how to decide what curren sexp is?
;; for purposes of (sp-get-thing) symbols count
;; but moving up doesn't consider a symbol to be one

;; (hello +)

(defun etc-duplicate-sexp ()
  (interactive)
  (let* ((sexp (sp-get-thing))
         (beg (plist-get sexp :beg))
         (end (plist-get sexp :end)))
    (kill-ring-save beg end)
    (goto-char end)
    (insert "\n")
    (let ((current-prefix-arg '(4)))  ; Simulates pressing C-u
      (call-interactively 'yank))))

(defun etc-comment-sexp ()
  (interactive)
  (let* ((sexp (sp-get-thing))
         (beg (plist-get sexp :beg))
         (end (plist-get sexp :end)))
    (comment-or-uncomment-region beg end)))

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
