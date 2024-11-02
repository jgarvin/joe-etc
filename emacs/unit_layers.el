(defun etc-forward-word (arg)
  (interactive "^p")
  (let ((p (point))
        (line (line-number-at-pos)))
    (forward-word arg)
    (when (not (= line (line-number-at-pos)))
      (goto-char p))))

(defun etc-forward-symbol (arg)
  (interactive "^p")
  (let ((p (point))
        (line (line-number-at-pos)))
    (forward-symbol arg)
    (when (not (= line (line-number-at-pos)))
      (goto-char p))))

(defun etc-backward-word ()
  (interactive)
  (etc-forward-word -1))

(defun etc-backward-symbol ()
  (interactive)
  (etc-forward-symbol -1))

;; NAVIGATION layer
(global-set-key (kbd "C-<left>") #'etc-backward-word)
(global-set-key (kbd "C-M-<left>") #'etc-backward-symbol)
(global-set-key (kbd "C-<right>") #'etc-forward-word)
(global-set-key (kbd "C-M-<right>") #'etc-forward-symbol)
(global-set-key (kbd "C-<next>") #'md-get-next-instance-of-symbol)
(global-set-key (kbd "C-<prior>") #'md-get-previous-instance-of-symbol)
(global-set-key (kbd "M-<left>") #'previous-buffer)
(global-set-key (kbd "M-<right>") #'next-buffer)
(define-key smartparens-mode-map (kbd "C-M-l") #'sp-beginning-of-sexp)
(define-key smartparens-mode-map (kbd "C-M-/") #'sp-end-of-sexp)
(define-key smartparens-mode-map (kbd "C-M-n") #'sp-previous-sexp)
(define-key smartparens-mode-map (kbd "C-M-i") #'sp-next-sexp)
(define-key smartparens-mode-map (kbd "C-M-e") #'sp-down-sexp)
(define-key smartparens-mode-map (kbd "C-M-d") #'sp-backward-down-sexp)
(define-key smartparens-mode-map (kbd "C-M-o") #'sp-up-sexp)
(define-key smartparens-mode-map (kbd "C-M-b") #'sp-backward-up-sexp)
;;(global-set-key (kbd "C-M-<left>") #'hs-hide-all)
;;(global-set-key (kbd "C-M-<right>") #'hs-toggle-hiding)


;; EDITING LAYER

(defun etc-bounds-of-thing-at-point (unit)
  "Similar to 'thing-at-point except that 'sexp gives the enclosing sexp."
  (cond
      ((eq 'sexp unit)
       (cons
        (plist-get (sp-get-enclosing-sexp) :beg)
        (plist-get (sp-get-enclosing-sexp) :end)))
      (t (bounds-of-thing-at-point unit))))

(defun etc-apply-to-unit (action thing)
  (let ((bounds (etc-bounds-of-thing-at-point thing)))
    (funcall action (car bounds) (cdr bounds))))

(defmacro etc-gen-commands (unit)
  `(progn
     (defun ,(intern (concat "etc-copy-" (symbol-name (cadr unit)))) ()
       ,(format "Copy the current %s to kill ring." (symbol-name (cadr unit)))
       (interactive)
       (etc-apply-to-unit #'kill-ring-save ',(cadr unit)))

     (defun ,(intern (concat "etc-cut-" (symbol-name (cadr unit)))) ()
       ,(format "Kill the current %s." (symbol-name (cadr unit)))
       (interactive)
       (etc-apply-to-unit #'kill-region ',(cadr unit)))

     (defun ,(intern (concat "etc-comment-" (symbol-name (cadr unit)))) ()
       ,(format "Comment the current %s." (symbol-name (cadr unit)))
       (interactive)
       (etc-apply-to-unit #'comment-or-uncomment-region ',(cadr unit)))

     (defun ,(intern (concat "etc-indent-left-" (symbol-name (cadr unit)))) ()
       ,(format "Comment the current %s." (symbol-name (cadr unit)))
       (interactive)
       (etc-apply-to-unit #'python-indent-shift-left ',(cadr unit)))

     (defun ,(intern (concat "etc-indent-right-" (symbol-name (cadr unit)))) ()
       ,(format "Comment the current %s." (symbol-name (cadr unit)))
       (interactive)
       (etc-apply-to-unit #'python-indent-shift-right ',(cadr unit)))

     (defun ,(intern (concat "etc-select-" (symbol-name (cadr unit)))) ()
       ,(format "Mark the current %s." (symbol-name (cadr unit)))
       (interactive)
       (let ((start (car (etc-bounds-of-thing-at-point ',(cadr unit))))
             (end (cdr (etc-bounds-of-thing-at-point ',(cadr unit)))))
         (goto-char start)
         (set-mark end)
         (setq mark-active t)
         (activate-mark)))))

(dolist (unit '(word line symbol sexp paragraph buffer))
  (eval `(etc-gen-commands ',unit)))

(defun etc-backward-transpose-sexp ()
  (interactive)
  (goto-char (car (bounds-of-thing-at-point 'sexp)))
  (sp-transpose-sexp)
  (sp-previous-sexp))

;; Line finger 0 (Left index)
(global-set-key (kbd "M-B") #'etc-cut-line)
(global-set-key (kbd "M-T") #'etc-copy-line)
(global-set-key (kbd "M-C") #'etc-select-line)
(global-set-key (kbd "M-G") #'etc-comment-line)

;; Line finger 1 (Left middle)
(global-set-key (kbd "M-*") #'etc-indent-left-line) ;; shift override so use *
(global-set-key (kbd "M-T") #'drag-stuff-up)
(global-set-key (kbd "M-C") #'drag-stuff-down)
(global-set-key (kbd "M-M") #'etc-indent-rIght-line)

;; Block finger (Left ring)
(global-set-key (kbd "M-Q") #'etc-cut-paragraph) ;; no letter bound here so use Q
(global-set-key (kbd "M-S") #'etc-copy-paragraph)
(global-set-key (kbd "M-D") #'etc-select-paragraph)
(global-set-key (kbd "M-V") #'etc-comment-paragraph)

;; Buffer finger (Left pinky)
(global-set-key (kbd "M-#") #'etc-cut-buffer) ;; no letter bound here so use #
(global-set-key (kbd "M-A") #'etc-copy-buffer)
(global-set-key (kbd "M-Q") #'etc-select-buffer)
(global-set-key (kbd "M-Z") #'etc-comment-buffer)

;; Sexp finger 0 (Right index)
(global-set-key (kbd "M-Y") #'etc-cut-sexp)
(global-set-key (kbd "M-N") #'etc-copy-sexp)
(global-set-key (kbd "M-L") #'etc-select-sexp)
(global-set-key (kbd "M-J") #'etc-comment-sexp)

;; Sexp finger 1 (Right middle)
(global-set-key (kbd "M-\"") #'sp-backward-barf-sexp)
(global-set-key (kbd "M-E") #'sp-backward-slurp-sexp)
(global-set-key (kbd "M-U") #'sp-forward-slurp-sexp)
(global-set-key (kbd "M-:") #'sp-forward-barf-sexp)
(global-set-key (kbd "M->") #'sp-splice-sexp)

;; Sexp finger 2 (Right ring)
(global-set-key (kbd "M-O") #'etc-backward-transpose-sexp)
(global-set-key (kbd "M-P") #'sp-transpose-sexp)
(global-set-key (kbd "M-<") #'sp-split-sexp)

;; Sexp finger 3 (Right pinky)
(global-set-key (kbd "M-\\") #'sp-unwrap-sexp)
(global-set-key (kbd "M-I") #'sp-rewrap-sexp)
(global-set-key (kbd "M-?") #'sp-wrap-round)


;; (defun etc-indent-shift-left ()
;;   (interactive)
;;   (let ((beg (if (region-active-p) (region-beginning) (line-beginning-position)))
;;         (end (if (region-active-p) (region-end) (line-end-position))))
;;     (apply #'python-indent-shift-left
;;            (adjust-bounds-to-line (region-beginning) (region-end)))))

;; (defun etc-indent-shift-right ()
;;   (interactive)
;;   (apply #'python-indent-shift-right
;;          (adjust-bounds-to-line (region-beginning) (region-end))))

;; (defun adjust-bounds-to-line (beg end)
;;   "Adjust BEG and END to the beginning and end of their respective lines.

;; If BEG is not at the beginning of a line, move it to the beginning.
;; If END is not at the end of a line, move it to the end.
;; Returns a cons cell (NEW-BEG . NEW-END)."
;;   (let (new-beg new-end)
;;     ;; Adjust beg
;;     (save-excursion
;;       (goto-char beg)
;;       (setq new-beg (if (bolp)
;;                         beg
;;                       (line-beginning-position))))
;;     ;; Adjust end
;;     (save-excursion
;;       (goto-char end)
;;       (setq new-end (if (eolp)
;;                         end
;;                       (line-end-position))))
;;     ;; Return the new bounds
;;     (list new-beg new-end)))


;; (defun grow-region-to-lines ()
;;   "Expand the active region to encompass entire lines.
;; If a region is active, adjust the start to the beginning of the first line
;; and the end to the end of the last line in the region.
;; If no region is active, select the entire current line."
;;   (interactive)
;;   (if (use-region-p)
;;       ;; Case 1: Active region exists
;;       (let (start end)
;;         ;; Determine the start position: beginning of the first line in the region
;;         (save-excursion
;;           (goto-char (region-beginning))
;;           (setq start (line-beginning-position)))

;;         ;; Determine the end position: end of the last line in the region
;;         (save-excursion
;;           (goto-char (region-end))
;;           ;; If the region ends exactly at the beginning of a line (i.e., between lines),
;;           ;; adjust to include the previous line's end.
;;           (when (= (point) (line-beginning-position))
;;             (forward-line -1))
;;           (setq end (line-end-position)))

;;         ;; Update the region to the new boundaries
;;         (set-mark start)
;;         (goto-char end)
;;         (activate-mark))
;;     ;; Case 2: No active region; select the current line
;;     (let (start end)
;;       (save-excursion
;;         (setq start (line-beginning-position))
;;         (setq end (line-end-position)))
;;       (set-mark start)
;;       (goto-char end)
;;       (activate-mark))))

;; (defun etc-setup-folding ()
;;   (hs-minor-mode)
;;   (hs-hide-all))

;; (add-hook 'prog-mode-hook #'etc-setup-folding)

