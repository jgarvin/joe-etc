;;; -*- lexical-binding: t -*-

;; the default behavior for triple quotes of putting the closing
;; quotes on their own line is annoying, this disables it
(setq python-fill-docstring-style nil)

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

;; TODO: when region starts and ends on the same line, we do want to indent
(defun pycustom-yank-and-indent ()
  ;; indentation doesn't work if existing indentation isn't in the region
  (interactive)
  (yank)
  (when (save-excursion (not (re-search-backward "[^[:blank:]]" (point-at-bol) t)))
    (setq end (save-excursion (previous-line) (point-at-eol))))
  (goto-char (region-beginning))
  (while (< (point) (region-end))
    (indent-according-to-mode)
    (next-line)))

;; (defun pycustom-yank-and-indent ()
;;   ;; indentation doesn't work if existing indentation isn't in the region
;;   (interactive)
;;   (yank)
;;   (save-excursion
;;     (let ((start (save-excursion (goto-char (region-beginning)) (point-at-bol)))
;;           (end   (save-excursion (goto-char (region-end)) (point-at-eol))))
;;       ;; often when copy and pasting a block the cursor will be on the next line,
;;       ;; which we don't actually intend to affect
;;       (when (save-excursion (not (re-search-backward "[^[:blank:]]" (point-at-bol) t)))
;;         (setq end (save-excursion (previous-line) (point-at-eol))))
;;       (goto-char start)
;;       (while (< (point) end)
;;         (indent-according-to-mode)
;;         (next-line)))))


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
          (line-move 1 t)
          (setq indent-level (current-indentation))
          (setq needed-indenting (line-needs-indenting)))
        (kill-line ARG)
        (if (not needed-indenting)
            (indent-line-to indent-level)
          (indent-according-to-mode)))
    (kill-line ARG)))

(defun python-advice (old new)
  (advice-add old :before-until
              (lambda ()
                (if (derived-mode-p 'python-mode)
                    (or (funcall new) t)
                  nil))))

;;(advice-remove #'yank-and-indent)

(python-advice #'yank-and-indent #'pycustom-yank-and-indent)
(python-advice #'open-line-and-indent #'open-line-and-maybe-indent)
(python-advice #'kill-and-indent #'pycustom-kill-and-indent)
(python-advice #'reindent-then-newline-and-indent #'maybe-reindent-then-newline-and-indent)

(defun etc-python-setup ()
  (subword-mode 1)
  (when (fboundp 'company-mode)
    ;; requires an inferior python process, which I don't want to setup
    (company-mode 0)))

(add-hook 'python-mode-hook 'etc-python-setup)

;; enable for typeshed files
(setq auto-mode-alist
      (append
       ;; File name (within directory) starts with a dot.
       '(("\\.pyi\\'" . python-mode)
         ("\\.py\\'" . python-mode))
       auto-mode-alist))

;; prefer backwards kill word behavior... but A-<backspace> does this
;; already so maybe I should reevaluate. Apparently both kill words by
;; default!
(define-key python-mode-map (kbd "C-<backspace>") nil)

;; we setup alt left and right to do this instead, simpler and useful in many modes
(define-key python-mode-map (kbd "C-c <") nil)
(define-key python-mode-map (kbd "C-c >") nil)

(setq py-underscore-word-syntax-p nil)

;; without this arguments in multiline argument lists are double indented
(setq python-indent-def-block-scale 1)