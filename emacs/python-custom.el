;; sp-select-next-thing isn't the right function, if there's a nested
;; instance nearby we'll try aligning that instead which is wrong. We
;; want "select containing sexp"

(defun align-dict ()
  "Align elements in the python dictionary cursor is inside of around colon."
  (interactive)
  (undo-boundary)
  (save-excursion 
    (single-pair-only-sexp "{" 'sp-select-next-thing)
    (align-regexp (region-beginning) (region-end) "\\(\\s-*\\):")))

(defun align-list ()
  "Align elements in the python list."
  (interactive)
  (undo-boundary)
  (save-excursion 
    (single-pair-only-sexp "[" 'sp-select-next-thing)
    (align-regexp (region-beginning) (region-end) "\\(\\s-*\\),")))

(defun line-needs-indenting ()
  ;; we only want to reindent the existing line if it's not indented properly
  ;; ...but it should pick closest indentation?
  (/= (mod (current-indentation) python-indent-offset) 0))

;; maybe these should be advice rather than new functions? don't know

(defun maybe-reindent-then-newline-and-indent ()
  (interactive)
  (when (line-needs-indenting)
    (indent-according-to-mode))
  (newline-and-indent))

(defun open-line-and-maybe-indent ()
  (interactive)
  (let ((indent-level (current-indentation))
	(needed-indenting (line-needs-indenting)))
    (when needed-indenting
      (indent-according-to-mode))
    (open-line 1)
    (save-excursion
      (next-line)
      ;; if we didn't need indentation, restore the
      ;; existing level of it
      (when (not needed-indenting)
	(indent-line-to indent-level)))))

;; (defun pycustom-yank-and-indent ()
;;   (interactive)
;;   (yank)
;;   (save-excursion
;;     ;; workaround for python mode
;;     ;; indentation doesn't work if existing indentation isn't in the region
;;     (exchange-point-and-mark)
;;     (beginning-of-line)
;;     (call-interactively 'indent-region)))

;; TODO: If I yank from start of line, then don't add extra indentation levels?
(defun pycustom-yank-and-indent ()
  ;; indentation doesn't work if existing indentation isn't in the region
  (interactive)
  (yank)
  (save-excursion
    (let ((start (save-excursion (goto-char (region-beginning)) (point-at-bol)))
	  (end   (save-excursion (goto-char (region-end)) (point-at-eol))))
      ;; often when copy and pasting a block the cursor will be on the next line,
      ;; which we don't actually intend to affect
      (when (save-excursion (not (re-search-backward "[^[:blank:]]" (point-at-bol) t)))
	(setq end (save-excursion (previous-line) (point-at-eol))))
      (goto-char start)
      (while (< (point) end)
	(indent-according-to-mode)
	(next-line)))))

(defun in-leading-whitespace ()
  (interactive)
  (let ((p (point)))
    (save-excursion
      (beginning-of-line)
      (if (re-search-forward "[^[:blank:]]" (point-at-eol) t)
	  (> (point) p)
	t))))

;; when killing a whole line, preserve indentation when
;; moving the line up, unlike other modes
(defun pycustom-kill-and-indent (&optional ARG)
  (interactive)
  (if (in-leading-whitespace)
    (let (indent-level needed-indenting)
      (save-excursion
	(next-line)
	(setq indent-level (current-indentation))
	(setq needed-indenting (line-needs-indenting)))
      (kill-line ARG)
      (if (not needed-indenting)
	  (indent-line-to indent-level)
	(indent-according-to-mode)))
    (kill-line ARG)))

(add-hook
 'python-mode-hook
 (lambda ()
   (local-set-key (kbd "RET") 'maybe-reindent-then-newline-and-indent)
   (local-set-key (kbd "C-o") 'open-line-and-maybe-indent)
   (local-set-key (kbd "C-y") 'pycustom-yank-and-indent)
   (local-set-key (kbd "C-k") 'pycustom-kill-and-indent)))
