;; -*- lexical-binding: t -*-

(require 'cl)
(require 'eldoc) ;; for getting documentation strings

(defvar md-snippet-list nil)
(defvar-local md-snippet-overlays nil)
(defvar-local md-current-glyph -1)
(defvar md-snippet-mode nil)
(defvar md-sn-timer nil)

(defconst md-glyphs
  '(?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M ?N ?O ?P ?Q ?R ?S ?T ?U ?V ?X ?Y ?Z))

(defconst md-glyph-face
  (list :box (list :color "red" :line-width -3)
        :foreground "yellow"))

;; We insert a unicode nuclear symbol into the buffer as a placeholder. You
;; should never see it because an overlay will change it to be drawn as a letter
;; inside a box. This is highly unlikely to be a valid character in most
;; programming languages or projects. You can always change it to something else
;; if you work for the IAEA. We have to have a placeholder character instead of
;; just an overlay in order to be able put the cursor over the graphic.
(defconst md-placeholder #x2622)

(cl-defstruct md-snippet
  (name nil :read-only t)
  (contents nil :read-only t)
  (context t :read-only t))

(defun md-sn-destroy-overlays ()
  (when md-snippet-overlays
    (mapc #'delete-overlay md-snippet-overlays)
    (setq md-snippet-overlays nil))
  (when md-sn-timer
    (cancel-timer md-sn-timer)
    (setq md-sn-timer nil)))
;; (md-sn-destroy-overlays)

(defun md-pos-is-ours (pos o)
  (eq (get-text-property pos 'md-glyph-overlay) o))

(defun md-clear-slot (pos o)
  ;; we have to verify we are dealing with the intended
  ;; overlay because old overlays can get left in the
  ;; buffer from undo and other actions.
  (when (md-pos-is-ours pos o)
    (save-excursion
      (goto-char pos)
      (delete-char 1)))
  (delete-overlay o)
  (setq md-snippet-overlays (delq o md-snippet-overlays)))
  
(defun md-get-glyph ()
  (setq md-current-glyph (% (1+ md-current-glyph) (length md-glyphs)))
  md-current-glyph)

(defun md-setup-glyph (location)
  (interactive)
  (let* ((start location)
         (end (1+ location))
         (o (get-text-property location 'md-glyph-overlay))
         (glyph-choice (get-text-property location 'md-glyph-choice))
         (glyph))
    (unless glyph-choice
      (setq glyph-choice (md-get-glyph))
      (put-text-property start end 'md-glyph-choice glyph-choice))
    (setq glyph (char-to-string (nth glyph-choice md-glyphs)))
    (unless (and o (overlay-buffer o))
      (setq o (make-overlay start end nil t nil))
      (push o md-snippet-overlays)
      (put-text-property start end 'md-glyph-overlay o)
      ;; we only make an overlay so we can detect insert-in-front-hooks.
      ;; supposedly it's also a text property, but that doesn't seem to
      ;; work at least in emacs 24.4.1.
      (overlay-put o 'insert-in-front-hooks
                   (list
                    (lambda (&rest unused) (md-clear-slot start o)))))
    (unless (string= glyph (get-char-property location 'display))
      (put-text-property start end
                         'display
                         (propertize glyph
                                     'face md-glyph-face 'font-lock-face md-glyph-face)))))

(defun md-add-glyph-properties (&optional start end)
  (unless start
    (setq start (window-start)))
  (unless end
    (setq end (window-end)))
  (save-excursion
    (goto-char start)
    (while (and (< (point) end)
                (re-search-forward (char-to-string md-placeholder)
                                   end 1))
      (md-setup-glyph (match-beginning 0))))
  (cancel-timer md-sn-timer)
  (setq md-sn-timer nil))

(defun md-sn-schedule-update ()
  (unless md-sn-timer
    (setq md-sn-timer (run-with-idle-timer 0.25 nil #'md-add-glyph-properties))))

(defun md-sn-scroll (w new-start)
  (when (eq w (selected-window))
    (md-sn-schedule-update)))

(defun md-sn-change (beg end len)
  (md-sn-schedule-update))

(defun md-sn-setup ()
  (add-hook 'window-scroll-functions #'md-sn-scroll)
  (add-hook 'md-window-selection-hook #'md-sn-schedule-update)
  (add-hook 'after-change-functions #'md-sn-change)
  (setq md-snippet-mode t))

(defun md-sn-teardown ()
  (remove-hook 'window-scroll-functions #'md-sn-scroll)
  (remove-hook 'md-window-selection-hook #'md-sn-schedule-update)
  (remove-hook 'after-change-functions #'md-sn-change)
  (md-sn-destroy-overlays)
  (setq md-snippet-mode nil))

(defun md-toggle-snippet-mode (&optional arg)
  (interactive)
  (cond
   ((and (not arg) md-snippet-mode) (md-sn-teardown))
   ((not md-snippet-mode) (md-sn-setup))))

(cl-defun md-add-snippet (&key name contents context)
  (add-to-list 'md-snippet-list
               (make-md-snippet :name name :contents contents :context context)
               nil
               (lambda (x y)
                 (and
                  (equal (md-snippet-name x) (md-snippet-name y))
                  (equal (md-snippet-context x) (md-snippet-context y))))))

(defun md-insert-snippet (name)
  (interactive)
  (let ((candidate (find name md-snippet-list
                         :key #'md-snippet-name
                         :test #'equal)))
    (when candidate
      (let ((contents (md-snippet-contents candidate))
            (start (point)))
        (insert
         (replace-regexp-in-string "\\$[0-9]+" (char-to-string md-placeholder)
                                   contents))
        (md-add-glyph-properties start (point))))))

;; this is the most horrible code ever, forgive me sexp gods
(defun md-gen-elisp-snippet-contents (sym)
  (let ((doc (substring-no-properties
              (eldoc-get-fnsym-args-string sym)))
        (func-name (format "%s" sym))
        (arg-doc))
    (save-match-data
      (string-match ": " doc)
      ;; remove function name from args
      (setq arg-doc (substring doc (match-end 0)))
      ;; remove leading/trailing parens
      (setq arg-doc (substring arg-doc 1 (1- (length arg-doc))))
      ;; allow one rest argument
      (setq arg-doc
            (replace-regexp-in-string "&rest [A-Z]+" "REST" arg-doc))
      ;; get rid of optional args
      (when (string-match "&optional" arg-doc)
        (setq arg-doc
              (substring arg-doc 0 (match-beginning 0))))
      ;; get rid of kw args
      (when (string-match "&key" arg-doc)
        (setq arg-doc
              (substring arg-doc 0 (match-beginning 0))))
        ;; (setq arg-doc (substring arg-doc 0
        ;;                          (- (match-beginning 0) 1))))
      (setq arg-doc
            (replace-regexp-in-string "\\([A-Z0-9\\-]+\\)\\(\\.\\.\\.\\)?"
                                      (char-to-string md-placeholder)
                                      arg-doc))
      (setq arg-doc
            (replace-regexp-in-string "\\[[^]]*\\]"
                                      ""
                                      arg-doc))
      ;; clean up stray spaces
      (setq arg-doc
            (replace-regexp-in-string "[[:space:]]+" " " arg-doc))
      (setq arg-doc
            (replace-regexp-in-string "( " "(" arg-doc))
      (setq arg-doc
            (replace-regexp-in-string " )" ")" arg-doc))
      (setq arg-doc
            (replace-regexp-in-string "\\` '" "" arg-doc))
      (setq arg-doc
            (replace-regexp-in-string " \\'" "" arg-doc))
      (concat "(" func-name
              (if (> (length arg-doc) 0) " " "")
              arg-doc
              ")"))))

(defun md-gen-elisp-snippet (sym)
  (md-add-snippet :name (format "%s" sym)
                  :contents (md-gen-elisp-snippet-contents sym)
                  :context '(derived-mode-p 'emacs-lisp-mode)))

;; (md-gen-elisp-snippet 'dotimes)
;; (md-gen-elisp-snippet 'string-match)
;; (md-gen-elisp-snippet 'format)
;; (md-gen-elisp-snippet 'make-md-snippet)
;; (md-gen-elisp-snippet 'interactive)
;; (format "%s" 'dotimes)

(md-add-snippet :name "dotimes"
                :contents "(dotimes ($1 $2) $3)"
                :context '(derived-mode-p 'emacs-lisp-mode))

;;(md-insert-snippet "fboundp")
(md-toggle-snippet-mode t)
;;(md-toggle-snippet-mode nil)
