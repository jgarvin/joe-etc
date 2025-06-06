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
(define-key Buffer-menu-mode-map (kbd "M-<right>") nil)
(define-key Buffer-menu-mode-map (kbd "M-<left>") nil)

(define-key smartparens-mode-map (kbd "C-M-l") #'sp-beginning-of-sexp)
(define-key smartparens-mode-map (kbd "C-M-/") #'sp-end-of-sexp)
(define-key smartparens-mode-map (kbd "C-M-_") #'sp-end-of-sexp) ;; needed for terminal mode
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
(global-set-key (kbd "M-0") #'etc-cut-line)
(global-set-key (kbd "M-1") #'etc-copy-line)
(global-set-key (kbd "M-2") #'etc-select-line)
(global-set-key (kbd "M-3") #'etc-comment-line)
(global-set-key (kbd "M-4") #'etc-cut-symbol)

;; Line finger 1 (Left middle)
(global-set-key (kbd "M-5") #'python-indent-shift-left)
(global-set-key (kbd "M-6") #'drag-stuff-up)
(global-set-key (kbd "M-7") #'drag-stuff-down)
(global-set-key (kbd "M-8") #'python-indent-shift-right)
(global-set-key (kbd "M-9") #'etc-cut-word)

;; to define sh mode
(require 'sh-script)

;; Block finger (Left ring)
(global-set-key (kbd "C-M-g") #'etc-cut-paragraph)
(global-set-key (kbd "C-M-q") #'etc-copy-paragraph)
(define-key sh-mode-map (kbd "C-M-q") nil)
(define-key c++-mode-map (kbd "C-M-q") nil)
(global-set-key (kbd "C-M-x") #'etc-select-paragraph)
(global-set-key (kbd "C-M-y") #'etc-comment-paragraph)
(define-key c++-mode-map (kbd "C-M-y") nil)

;; Buffer finger (Left pinky)
(global-set-key (kbd "C-M-z") #'etc-cut-buffer)
(global-set-key (kbd "S-<f1>") #'etc-copy-buffer)
(global-set-key (kbd "S-<f2>") #'etc-select-buffer)

;; Sexp finger 0 (Right index)
(global-set-key (kbd "S-<f3>") #'etc-cut-sexp)
(global-set-key (kbd "S-<f4>") #'etc-copy-sexp)
(global-set-key (kbd "S-<f5>") #'etc-select-sexp)
(global-set-key (kbd "S-<f6>") #'etc-comment-sexp)
(global-set-key (kbd "S-<f7>") #'etc-copy-symbol)

;; Sexp finger 1 (Right middle)
(global-set-key (kbd "S-<f8>")  #'sp-backward-barf-sexp)
(global-set-key (kbd "S-<f9>")  #'sp-backward-slurp-sexp)
(global-set-key (kbd "S-<f10>") #'sp-forward-slurp-sexp)
(global-set-key (kbd "S-<f11>") #'sp-forward-barf-sexp)
(global-set-key (kbd "S-<f12>") #'sp-splice-sexp)

;; Sexp finger 2 (Right ring)
(global-set-key (kbd "C-<f1>") #'etc-backward-transpose-sexp)
(global-set-key (kbd "C-<f2>") #'sp-transpose-sexp)
(global-set-key (kbd "C-<f3>") #'sp-split-sexp)

;; Sexp finger 3 (Right pinky)
(global-set-key (kbd "C-<f4>") #'sp-unwrap-sexp)
(global-set-key (kbd "C-<f5>") #'sp-rewrap-sexp)
(global-set-key (kbd "C-<f6>") #'sp-wrap-round)

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
