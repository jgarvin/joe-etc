;; modset-u is like a prefix argument, but without using the actual
;; emacs prefix mechanism so that is still available through a
;; conventional C-u
(define-key smartparens-mode-map (kbd "C-M-l") #'sp-beginning-of-sexp)
(define-key smartparens-mode-map (kbd "C-M-/") #'sp-end-of-sexp)
(define-key smartparens-mode-map (kbd "C-M-n") #'etc-previous-sexp)
(define-key smartparens-mode-map (kbd "C-M-u C-M-n") #'etc-backward-transpose-sexp)
(define-key smartparens-mode-map (kbd "C-M-i") #'etc-next-sexp)
(define-key smartparens-mode-map (kbd "C-M-u C-M-i") #'etc-transpose-sexp)
(define-key smartparens-mode-map (kbd "C-M-e") #'sp-down-sexp)
(define-key smartparens-mode-map (kbd "C-M-o") #'sp-backward-up-sexp)
(define-key smartparens-mode-map (kbd "C-M-c") #'sp-unwrap-sexp)
(define-key smartparens-mode-map (kbd "C-M-r") #'sp-rewrap-sexp)
(define-key smartparens-mode-map (kbd "C-M-'") #'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-M-u C-M-'") #'sp-backward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-M-,") #'sp-forward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-M-u C-M-,") #'sp-backward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-M-k") #'sp-kill-sexp)
(define-key smartparens-mode-map (kbd "C-M-u C-M-k") #'etc-cut-sexp)
(define-key smartparens-mode-map (kbd "C-M-s") #'sp-split-sexp)
(define-key smartparens-mode-map (kbd "C-M-p") #'etc-flip-travel-point-sexp)
(define-key smartparens-mode-map (kbd "C-M-\\") #'sp-splice-sexp)
(define-key smartparens-mode-map (kbd "C-M-;") #'etc-comment-sexp)
(define-key smartparens-mode-map (kbd "C-M-j") #'etc-copy-sexp)
(define-key smartparens-mode-map (kbd "C-M-y") #'etc-duplicate-sexp)
;; (define-key smartparens-mode-map (kbd "C-M-SPC") #'sp-mark-sexp)

;; line to the left doesn't make sense, so does that do indentation?
;;
;; todo: could be nice to have
;; editing mersions for left/right,
;; to pull the line out or push into
;; a block, barf/slurp style? I
;; guess those could go on the keys
;; already dedicated to those though
(global-set-key (kbd "M-L") #'beginning-or-indentation)
(global-set-key (kbd "M-?") #'end-or-trailing)
(global-set-key (kbd "M-Y") #'etc-duplicate-line)
(global-set-key (kbd "M-N") #'etc-previous-less-indented-line)
(global-set-key (kbd "M-I") #'etc-next-more-indented-line)
(global-set-key (kbd "M-E") #'next-line)
(global-set-key (kbd "M-U M-E") #'drag-stuff-down)
(global-set-key (kbd "M-O") #'previous-line)
(global-set-key (kbd "M-U M-O") #'drag-stuff-up)
(global-set-key (kbd "M-J") #'md-copy-line)
(global-set-key (kbd "M-P") #'etc-flip-travel-point-line)
(global-set-key (kbd "M-K") #'md-cut-line)
(global-set-key (kbd "M-:") #'etc-comment-line)

;; beginning-or-indentation-toggle ??? used to be quite useful
;; what should mark mark?
;; should we infer travel side from point position? if so we have to prevent moving forward from bringing us to the end

(defun debug-dump ()
  (interactive)
  (message "\"%s\" -- \"%s\""
           (buffer-substring (plist-get (sp-get-thing t) :beg) (plist-get (sp-get-thing t) :end))
           (buffer-substring (plist-get (etc-sp-get-thing) :beg) (plist-get (etc-sp-get-thing) :end))))

;; (global-set-key (kbd "C-M-z") #'debug-dump)
;; (global-set-key (kbd "C-M-z") #'sp-previous-sexp)
(defvar-local etc-travel-side t) ;; t is front, nil is back

(defun etc-flip-travel-point ()
  (setq etc-travel-side (not etc-travel-side)))

;;;;;;;;;;;;;;;;;;;;;;;; LINE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; line duplicate isn't duplicating from beginning
;; likewise ideally reversing an indentation change would go back to the same line

(defun etc-previous-less-indented-line ()
  (interactive)
  (md-find-indentation-change -1 '<))

(defun etc-next-more-indented-line ()
  (interactive)
  (md-find-indentation-change 1 '>))

(defun etc-comment-line ()
  (interactive)
  (etc-kill #'comment-or-uncomment-region 'line))

(defun etc-duplicate-line ()
  (interactive)
  ;; save-excursion inserts a marker into the text, it doesn't just
  ;; remember the file offset, so it's important that we move to the
  ;; next line before we yank, otherwise the line on top will be the
  ;; duplicate one, and the marker will be pushed down to the next
  ;; line.
  ;;
  ;; Also annoyingly forward-line will not preserve the column, so we
  ;; have to manually save it.
  ;;
  ;; WARNING: if you change this function be sure it behaves the same
  ;; whether cursor is at the beginning of the line or not, first
  ;; versions did not.
  (let ((col (current-column)))
    (save-excursion
      (beginning-of-line)
      (kill-ring-save (line-beginning-position) (line-beginning-position 2))
      (forward-line)
      (beginning-of-line)
      (yank))
    (forward-line)
    (move-to-column col)))

(defun etc-flip-travel-point-line ()
  (interactive)
  (etc-flip-travel-point)
  (let* ((beg (save-excursion (beginning-or-indentation) (point)))
         (end (line-end-position))
         (travel-point (if etc-travel-side beg end)))
    (goto-char travel-point)))

;;;;;;;;;;;;;;;;;;;;;;;; SEXP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun etc-flip-travel-point-sexp ()
  (interactive)
  (etc-flip-travel-point)
  (let* ((sexp (etc-sp-get-thing))
         (beg (plist-get sexp :beg))
         (end (plist-get sexp :end))
         (travel-point (if etc-travel-side beg end)))
    (goto-char (if etc-travel-side beg end))))

(defun etc-next-sexp ()
  (interactive)
  (let* ((sexp (etc-sp-get-thing))
         (beg (plist-get sexp :beg))
         (end (plist-get sexp :end))
         (original-point (point))
         (travel-point (if etc-travel-side beg end))
         (up-point (save-excursion (sp-backward-up-sexp) (point)))
         (end-point (save-excursion (sp-end-of-sexp) (point))))
    (dh 'up 'b up-point)
    (cond
     ((= end-point (point)) nil)
     ((not (= travel-point (point))) (goto-char travel-point))
     (t
      (goto-char beg)
      (sp-next-sexp)
      (let* ((sexp (etc-sp-get-thing))
             (new-beg (plist-get sexp :beg))
             (new-end (plist-get sexp :end)))
        (goto-char (if etc-travel-side new-beg new-end))
        (when (= (point) up-point)
          (goto-char (if etc-travel-side beg end))))))))

(defun etc-previous-sexp ()
  (interactive)
  (let* ((sexp (etc-sp-get-thing))
         (beg (plist-get sexp :beg))
         (end (plist-get sexp :end))
         (original-point (point))
         (travel-point (if etc-travel-side beg end))
         (up-point (save-excursion (sp-up-sexp) (point)))
         (up-back-point (save-excursion (sp-backward-up-sexp) (point)))
         (begin-point (save-excursion (sp-beginning-of-sexp) (point))))
    (cond
     ((= begin-point (point)) nil)
     ((not (= travel-point (point))) (goto-char travel-point))
     (t
      (goto-char end)
      (sp-previous-sexp)
      (let* ((sexp (etc-sp-get-thing))
             (beg (plist-get sexp :beg))
             (end (plist-get sexp :end)))
        (goto-char (if etc-travel-side beg end)))
      (when (or (= (point) up-point) (= (point) up-back-point))
        (goto-char original-point))))))

(defun point-on-whitespace-p ()
  "Return `t` if the character at point is whitespace, `nil` otherwise."
  (save-excursion
    (looking-at "\\s-")))

(defun etc-sp-get-thing ()
  (if (or (point-on-whitespace-p) (md-likely-followed-by-closer (point)))
      (sp-get-thing t)
    (sp-get-thing)))

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

;;;;;;;;;;;;;;;;;;;;;;; UTILITY ;;;;;;;;;;;;;;;;;

(defun etc-kill (action thing)
  (let ((bounds (bounds-of-thing-at-point thing)))
    (funcall action (car bounds) (cdr bounds))))