;; modset-u is like a prefix argument, but without using the actual
;; emacs prefix mechanism so that is still available through a
;; conventional C-u
(define-key smartparens-mode-map (kbd "C-M-l") #'sp-beginning-of-sexp)
(define-key smartparens-mode-map (kbd "C-M-/") #'sp-end-of-sexp)
(define-key smartparens-mode-map (kbd "C-M-n") #'sp-previous-sexp)
(define-key smartparens-mode-map (kbd "C-M-u C-M-n") #'etc-backward-transpose-sexp)
(define-key smartparens-mode-map (kbd "C-M-i") #'sp-next-sexp)
(define-key smartparens-mode-map (kbd "C-M-u C-M-i") #'etc-transpose-sexp)
(define-key smartparens-mode-map (kbd "C-M-e") #'sp-down-sexp)
(define-key smartparens-mode-map (kbd "C-M-o") #'sp-up-sexp)
(define-key smartparens-mode-map (kbd "C-M-c") #'sp-unwrap-sexp)
(define-key smartparens-mode-map (kbd "C-M-r") #'sp-rewrap-sexp)
(define-key smartparens-mode-map (kbd "C-M-'") #'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-M-u C-M-'") #'sp-backward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-M-,") #'sp-forward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-M-u C-M-,") #'sp-backward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-M-k") #'etc-cut-sexp)
(define-key smartparens-mode-map (kbd "C-M-p") #'sp-split-sexp)
(define-key smartparens-mode-map (kbd "C-M-\\") #'sp-splice-sexp)
(define-key smartparens-mode-map (kbd "C-M-;") #'etc-comment-sexp)
(define-key smartparens-mode-map (kbd "C-M-j") #'etc-copy-sexp)
(define-key smartparens-mode-map (kbd "C-M-y") #'etc-duplicate-sexp)
;; (define-key smartparens-mode-map (kbd "C-M-SPC") #'sp-mark-sexp)

;; my hand really hates the double taps
;; should duplicate apply to the containing sexp?
;; should mark apply to the containing sexp?
;; forward should go after then to beginning of next, likewise for backwards
;; up should always go to beginning, not after

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
