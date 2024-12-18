(defun backward-symbol (&optional arg)
  (interactive "p")
  (forward-symbol (* -1 (or arg 1))))

;; NAVIGATION layer
(global-set-key (kbd "C-<left>") #'backward-word)
(global-set-key (kbd "C-M-<left>") #'backward-symbol)
(global-set-key (kbd "C-<right>") #'forward-word)
(global-set-key (kbd "C-M-<right>") #'forward-symbol)
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
    (if (equal bounds (cons nil nil))
        (user-error "No %s at point" thing)
      (funcall action (car bounds) (cdr bounds)))))

(defmacro etc-gen-commands (unit)
  `(progn
     (defun ,(intern (concat "etc-copy-" (symbol-name (cadr unit)))) ()
       ,(format "Copy the current %s to kill ring." (symbol-name (cadr unit)))
       (interactive)
       (etc-apply-to-unit #'kill-ring-save ',(cadr unit))
       (unless (eq ',(cadr unit) 'buffer)
         (goto-char (cdr (etc-bounds-of-thing-at-point ',(cadr unit)))))
       ;; (when (and (eobp) (equal ',(cadr unit) 'line) (not (looking-at-p "^\\s-*$")))
       ;;   (insert "\n"))
       )

     (defun ,(intern (concat "etc-cut-" (symbol-name (cadr unit)))) ()
       ,(format "Kill the current %s." (symbol-name (cadr unit)))
       (interactive)
       (etc-apply-to-unit #'kill-region ',(cadr unit)))

     (defun ,(intern (concat "etc-comment-" (symbol-name (cadr unit)))) ()
       ,(format "Comment the current %s." (symbol-name (cadr unit)))
       (interactive)
       (if (use-region-p)
           (comment-or-uncomment-region (region-beginning) (region-end))
         (etc-apply-to-unit #'comment-or-uncomment-region ',(cadr unit))
         (goto-char (cdr (etc-bounds-of-thing-at-point ',(cadr unit))))))

     (defun ,(intern (concat "etc-indent-left-" (symbol-name (cadr unit)))) ()
       ,(format "Comment the current %s." (symbol-name (cadr unit)))
       (interactive)
       (if (use-region-p)
           (python-indent-shift-left (region-beginning) (region-beginning))
         (etc-apply-to-unit #'python-indent-shift-left ',(cadr unit))))

     (defun ,(intern (concat "etc-indent-right-" (symbol-name (cadr unit)))) ()
       ,(format "Comment the current %s." (symbol-name (cadr unit)))
       (interactive)
       (if (use-region-p)
           (python-indent-shift-right (region-beginning) (region-beginning))
         (etc-apply-to-unit #'python-indent-shift-right ',(cadr unit))))

     (defun ,(intern (concat "etc-select-" (symbol-name (cadr unit)))) ()
       ,(format "Mark the current %s." (symbol-name (cadr unit)))
       (interactive)
       (let ((start (car (etc-bounds-of-thing-at-point ',(cadr unit))))
             (end (cdr (etc-bounds-of-thing-at-point ',(cadr unit)))))
         (if (eq start nil)
             (user-error "No %s at point." ',(cadr unit))
           (goto-char start)
           (set-mark end)
           (setq mark-active t)
           (activate-mark))))))

(dolist (unit '(word line symbol sexp paragraph buffer))
  (eval `(etc-gen-commands ',unit)))

(defun etc-backward-transpose-sexp ()
  (interactive)
  (let ((start (car (bounds-of-thing-at-point 'sexp))))
    (if (eq start nil)
        (user-error "No sexp at point.")
      (goto-char start)
      (sp-transpose-sexp)
      (sp-previous-sexp))))

;; need some kind of smart paste that does select and paste in one
;; step, that's much more common for me

;; Line finger 0 (Left index)
(global-set-key (kbd "M-B") #'etc-cut-line)
(global-set-key (kbd "M-T") #'etc-copy-line)
(global-set-key (kbd "M-C") #'etc-select-line)
(global-set-key (kbd "M-G") #'etc-comment-line)
(global-set-key (kbd "M-W") #'etc-cut-symbol)

;; Line finger 1 (Left middle)
(global-set-key (kbd "M-*") #'python-indent-shift-left) ;; shift override so use *
(global-set-key (kbd "M-H") #'drag-stuff-up)
(global-set-key (kbd "M-R") #'drag-stuff-down)
(global-set-key (kbd "M-M") #'python-indent-shift-right)
(global-set-key (kbd "M-X") #'etc-cut-word)

;; Block finger (Left ring)
(global-set-key (kbd "M-Q") #'etc-cut-paragraph) ;; no letter bound here so use Q
(global-set-key (kbd "M-S") #'etc-copy-paragraph)
(global-set-key (kbd "M-D") #'etc-select-paragraph)
(global-set-key (kbd "M-V") #'etc-comment-paragraph)

;; Buffer finger (Left pinky)
(global-set-key (kbd "M-#") #'etc-cut-buffer) ;; no letter bound here so use #
(global-set-key (kbd "M-A") #'etc-copy-buffer)
(global-set-key (kbd "M-Z") #'etc-select-buffer)

;; Sexp finger 0 (Right index)
(global-set-key (kbd "M-Y") #'etc-cut-sexp)
(global-set-key (kbd "M-N") #'etc-copy-sexp)
(global-set-key (kbd "M-L") #'etc-select-sexp)
(global-set-key (kbd "M-J") #'etc-comment-sexp)
(global-set-key (kbd "M-F") #'etc-copy-symbol)

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
(global-set-key (kbd "M-+") #'sp-unwrap-sexp)
(global-set-key (kbd "M-I") #'sp-rewrap-sexp)
(global-set-key (kbd "M-?") #'sp-wrap-round)

;; (defun etc-forward-word (arg)
;;   (interactive "^p")
;;   (let ((p (point))
;;         (line (line-number-at-pos)))
;;     (forward-word arg)
;;     (when (not (= line (line-number-at-pos)))
;;       (goto-char p))))

;; (defun etc-forward-symbol (arg)
;;   (interactive "^p")
;;   (let ((p (point))
;;         (line (line-number-at-pos)))
;;     (forward-symbol arg)
;;     (when (not (= line (line-number-at-pos)))
;;       (goto-char p))))

;; (defun etc-backward-word ()
;;   (interactive)
;;   (etc-forward-word -1))

;; (defun etc-backward-symbol ()
;;   (interactive)
;;   (etc-forward-symbol -1))
