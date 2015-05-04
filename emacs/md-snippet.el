;; -*- lexical-binding: t -*-

(require 'cl)
(require 'eldoc) ;; for getting documentation strings

(defvar md-snippet-list nil)
(defvar md-global-overlay-list nil)
(defvar-local md-snippet-overlays nil)
(defvar-local md-current-glyph -1)
(defvar md-snippet-mode nil)
(defvar md-sn-timer nil)

(defconst md-glyphs
  '(?A ?B ?C ?D ?E ?F ?G ?H ?I ?J ?K ?L ?M ?N ?O ?P ?Q ?R ?S ?T ?U ?V ?W ?X ?Y ?Z
       ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9
       ?^ ?\# ?\ ?> ?\; ?\" ?* ?\' ?% ?\\ ?\` ?$ ?[ ?_ ?\, ?{ ?:
       ?! ?- ?\( ?\| ?~ ?\. ?? ?= ?\) ?< ?} ?& ?/ ?@ ?+ ?]))

(defvar-local md-glyphs-in-use nil)

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

(defun md-sn-cancel-timer ()
  (when md-sn-timer
    (cancel-timer md-sn-timer)
    (setq md-sn-timer nil)))

(defun md-sn-destroy-overlays ()
  (let ((temp (copy-sequence md-global-overlay-list)))
    (when temp
      (mapc #'md-sn-destroy-overlay temp)
      (setq md-global-overlay-list nil)
      (setq md-snippet-overlays nil))
    (setq md-glyphs-in-use nil)
    (md-sn-cancel-timer)))
;; (md-sn-destroy-overlays)

(defun md-pos-is-ours (o)
  (condition-case nil
      (let ((start (overlay-start o))
            (end (overlay-end o)))
        (when (and start end)
          (string= (buffer-substring-no-properties start end)
                   (char-to-string md-placeholder))))
    (args-out-of-range nil)))

(defun md-sn-destroy-overlay (o &optional keep-in-list)
  (setq md-glyphs-in-use
              (cl-remove (string-to-char (overlay-get o 'display))
                         md-glyphs-in-use :test 'equal :count 1))
  (delete-overlay o)
  (unless keep-in-list
    (setq md-snippet-overlays (delq o md-snippet-overlays))))

(defun md-clear-slot (o)
  ;;(message "clearing")
  ;; we have to verify we are dealing with the intended
  ;; overlay because old overlays can get left in the
  ;; buffer from undo and other actions.
  (let ((inhibit-modification-hooks t))
    (when (md-pos-is-ours o)
      (save-excursion
        (goto-char (overlay-start o))
        (delete-char 1)))
    (md-sn-destroy-overlay o)))

(defun md-sn-destroyed-invalid-overlays ()
  (let ((invalid))
    (dolist (i md-snippet-overlays)
      (when (not (md-pos-is-ours i))
        (push i invalid)))
    (mapc #'md-sn-destroy-overlay invalid)))

(defun md-sn-report-overlays ()
  (interactive)
  (message "---------------------")
  (dolist (i md-snippet-overlays)
    (message "Overlay: %S" i)
    (message "Ours: %S" (md-pos-is-ours i))
    (message "Snippet: %S" (buffer-substring-no-properties (overlay-start i) (overlay-end i))))
  (message "---------------------"))

(defun md-get-glyph ()
  "Get an unused glyph if possible, otherwise just start cycling them."
  (let ((candidates (cl-remove-if (lambda (x) (member x md-glyphs-in-use)) md-glyphs)))
    (if candidates
        (progn
          ;; (message "candidates: %S" candidates)
          ;; (message "checking positon of %S in %S" (car candidates) md-glyphs)
          (push (car candidates) md-glyphs-in-use)
          (position (car candidates) md-glyphs :test 'equal))
      (setq md-current-glyph (% (1+ md-current-glyph) (length md-glyphs)))
      md-current-glyph)))

(defun md-sn-get-overlay (location)
  (let ((candidates (remove-if (lambda (x) (/= location (overlay-start x))) md-snippet-overlays)))
    (assert (<= (length candidates) 1))
    (when candidates
      (car candidates))))

(defun md-setup-glyph (location)
  (interactive)
  (let* ((start location)
         (end (1+ location))
         (o (md-sn-get-overlay location))
         (glyph-choice (when o (overlay-get o 'md-glyph-choice)))
         (glyph))
    (unless glyph-choice
        (setq glyph-choice (md-get-glyph)))
    (setq glyph (char-to-string (nth glyph-choice md-glyphs)))
    (unless (and o (overlay-buffer o)
                 (string= glyph (overlay-get o 'display)))
      (when o
        (md-sn-destroy-overlay o))
      (setq o (make-overlay start end nil t nil))
      (push o md-global-overlay-list)
      (push o md-snippet-overlays)
      ;; we only make an overlay so we can detect insert-in-front-hooks.
      ;; supposedly it's also a text property, but that doesn't seem to
      ;; work at least in emacs 24.4.1.
      ;; update: it doesn't work because font-lock clobbers this property, wtf?
      (overlay-put o 'evaporate t)
      (overlay-put o 'md-glyph-choice glyph-choice)
      (overlay-put o 'insert-in-front-hooks
                   (list
                    (lambda (&rest unused)
                      (unless undo-in-progress
                        (md-clear-slot o)))))
      (overlay-put o
                   'display
                   (propertize glyph
                               'face md-glyph-face 'font-lock-face md-glyph-face)))))

(defun md-add-glyph-properties (&optional start end)
  ;;(message "Adding glyphs in window %S" (selected-window))
  (md-sn-destroyed-invalid-overlays)
  (unless start
    (setq start (window-start)))
  (unless end
    (setq end (window-end)))
  (let ((last-glyph))
    (save-excursion
      (goto-char end)
      (while (and (> (point) start)
                  (re-search-backward (char-to-string md-placeholder)
                                      start 1))
        (md-setup-glyph (match-beginning 0))
        (setq last-glyph (match-beginning 0))))
    (md-sn-cancel-timer)
    last-glyph))

(defun md-sn-schedule-update ()
  (unless (or md-sn-timer
              (not (= 0 (recursion-depth)))
              (not (eq (current-buffer) (window-buffer)))
              buffer-read-only
              (minibufferp))
    (let ((w (selected-window)))
      (let ((win-check-closure
             (lambda ()
               (unwind-protect
                   (when (eq w (selected-window))
                     (md-add-glyph-properties))
                 (md-sn-cancel-timer)))))
        ;; The idle timer has to be marked as repeating even though we cancel
        ;; it every time it actually executes. This is because of a race
        ;; condition not handled in the emacs API. If an error occurs triggering
        ;; the debugger then timers won't run. If the timer isn't marked repeating
        ;; it then never runs. If it never runs then it never clears the md-sn-timer
        ;; variable. If it never clears that variable then the check at the top
        ;; of this function to avoid double timers never passes, and we never
        ;; get to set the timer again.
        (setq md-sn-timer (run-with-idle-timer 0.25 t win-check-closure))))))

(defun md-sn-scroll (w new-start)
  (when (eq w (selected-window))
    (md-sn-schedule-update)))

(defun md-sn-change (beg end len)
  (md-sn-schedule-update))

(defun md-sn-setup ()
  (message "Starting md-snippet...")
  (add-hook 'window-scroll-functions #'md-sn-scroll)
  (add-hook 'md-window-selection-hook #'md-sn-schedule-update)
  (add-hook 'after-change-functions #'md-sn-change)
  (setq md-snippet-mode t))

(defun md-sn-teardown ()
  (message "Stopping md-snippet...")
  (remove-hook 'window-scroll-functions #'md-sn-scroll)
  (remove-hook 'md-window-selection-hook #'md-sn-schedule-update)
  (remove-hook 'after-change-functions #'md-sn-change)
  (md-sn-destroy-overlays)
  (setq md-snippet-mode nil))

(defun md-snippet-mode-activate (arg)
  (interactive)
  (cond
   ((and md-snippet-mode (= 0 arg)) (md-sn-teardown))
   ((and (not md-snippet-mode) (= 1 arg)) (md-sn-setup))))
  
(defun md-compare-snippets (x y)
  (and
   (equal (md-snippet-name x) (md-snippet-name y))
   (equal (md-snippet-context x) (md-snippet-context y))))

(cl-defun md-add-snippet (&key name contents context)
  (let ((snippet (make-md-snippet :name name :contents contents :context context))) 
    (add-to-list 'md-snippet-list snippet nil #'md-compare-snippets)))

(cl-defun md-replace-snippet (&key name contents context)
  (let ((snippet (make-md-snippet :name name :contents contents :context context))) 
    (setq md-snippet-list
          (remove-if (lambda (x) (md-compare-snippets snippet x)) md-snippet-list))
    (add-to-list 'md-snippet-list snippet nil #'md-compare-snippets)))

(defun md-make-snippets (context snippets)
  (dolist (snip snippets)
    (md-replace-snippet :context context :name (car snip) :contents (cadr snip))))

(defun md-get-snippet-names (&optional mode)
  (unless mode
    (setq mode major-mode))
  (mapcar #'md-snippet-name
          (-filter (lambda (x) (eval (md-snippet-context x))) md-snippet-list)))

(defun md-insert-snippet (name)
  (interactive)
  (when md-snippet-mode
    (let ((candidate (remove-if (lambda (x)
                                  (or (not (string= (md-snippet-name x) name))
                                      (not (eval (md-snippet-context x)))))
                                md-snippet-list)))
      (if candidate
          (md--insert-snippet-impl (car candidate))
        (user-error "Can't find snippet with name \"%s\" in current context" name)))))

(defun md--insert-snippet-impl (snippet)
  (let ((contents (md-snippet-contents snippet))
        (start)
        (jump-point)
        (end)
        (arg-text)
        (indentation (concat "\n    " (make-string (current-indentation) ? )))
        (delete-selection-mode-enabled delete-selection-mode)
        (region-bounds
         ;; TODO: should also work on sexps!
         (cond
          ((use-region-p) (cons (region-beginning) (region-end)))
          ((thing-at-point 'symbol) (bounds-of-thing-at-point 'symbol))
          ((or (md-likely-followed-by-closer (point))
               (md-likely-preceded-by-opener (1+ (point))))
           (cons (plist-get (sp-get-thing) :beg)
                 (plist-get (sp-get-thing) :end))))))
    (unwind-protect
        (progn
          ;; delete selection mode messes with our substitution of
          ;; the marked region for the first argument 
          (delete-selection-mode 0)
          (setq start (make-marker))
          (set-marker start (point))
          (when region-bounds
            (setq arg-text (buffer-substring (car region-bounds) (cdr region-bounds)))
            (delete-region (car region-bounds) (cdr region-bounds)))
          (setq contents (replace-regexp-in-string "\\$[0-9]+" (char-to-string md-placeholder)
                                                   contents))
          (when (derived-mode-p 'python-mode)
            (setq contents (replace-regexp-in-string "\n    " indentation contents)))
          (md-insert-text contents t nil)
          (setq end (make-marker))
          (set-marker end (point))
          (unless (derived-mode-p 'python-mode)
            (flet ((deactivate-mark nil))
              (indent-region (marker-position start) (marker-position end))))
          (setq jump-point (md-add-glyph-properties (marker-position start) (point)))
          (when jump-point
            (set-window-point nil jump-point))
          (when region-bounds
            (deactivate-mark)
            (md-insert-text arg-text t nil)
            (goto-char (marker-position start))
            (when (re-search-forward (char-to-string md-placeholder) (marker-position end) 1)
              (goto-char (1- (point))))))
      (delete-selection-mode (if delete-selection-mode-enabled 1 0)))))

(defun md-sn-find-slot (c)
  (let ((candidates (-filter (lambda (x)
                               (= (upcase c)
                                  (nth (overlay-get x 'md-glyph-choice) md-glyphs)))
                             md-snippet-overlays)))
    (setq candidates (sort candidates
                           (lambda (x y) (< (abs (- (point) (overlay-start x)))
                                            (abs (- (point) (overlay-start y)))))))
    (if candidates
        (set-window-point nil (overlay-start (car candidates)))
      (error "No slot for that letter"))))

(defun md-get-overlay-glyph-idx (o)
  (position (string-to-char (overlay-get o 'display)) md-glyphs))

(defun md-sn-next-slot ()
  (interactive)
  "If point is over a slot, go to the next highest slot, if not
go to the highest slot (most recent)."
  (md-sn-destroyed-invalid-overlays)
  (setq md-snippet-overlays
        (cl-sort md-snippet-overlays #'<
                 :key #'md-get-overlay-glyph-idx))
  (setq md-snippet-overlays (nreverse md-snippet-overlays))
  (let ((on-overlay)
        (next-attempt))
    (setq on-overlay (-filter (lambda (x) (memq x md-snippet-overlays)) (overlays-at (point))))
    (when on-overlay
      ;; we cleaned invalid overlays at the start so each point should contain
      ;; exactly 0 or 1 snippet overlays
      (assert (= 1 (length on-overlay)))
      (setq on-overlay (car on-overlay)))
    (if on-overlay
        (let ((idx (md-get-overlay-glyph-idx on-overlay)))
          ;; if we're already on an overlay, find the next lowest glyph
          (setq next-attempt (% (1- idx) (length md-glyphs)))
          (while (/= next-attempt idx)
            (condition-case nil
                (progn
                  (md-sn-find-slot (nth next-attempt md-glyphs))
                  (setq next-attempt idx))
              (error (setq next-attempt (% (1- next-attempt) (length md-glyphs)))))))
      ;; if we're not on an overlay find the highest glyph
      (md-sn-find-slot (nth (md-get-overlay-glyph-idx
                             (car md-snippet-overlays))
                            md-glyphs)))))

(defun md-sn-drop-slot ()
  (interactive)
  (md-insert-text (char-to-string md-placeholder) t nil))

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
  (md-add-snippet 
   :name (format "%s" sym)
   :contents (md-gen-elisp-snippet-contents sym)
   :context '(derived-mode-p 'emacs-lisp-mode)))

(defun md-insert-call-snippet (n)
  (interactive) 
  (let* ((separator (if (derived-mode-p 'emacs-lisp-mode) " " ", "))
         (c (concat "(" (mapconcat (lambda (x) (format "$%d" x)) (number-sequence 1 n) separator)
                    ")")))
    (when (not (derived-mode-p 'emacs-lisp-mode))
      ;; most non- lisps have function calls of the format f(x, y) 
      (setq c (concat "$0" c)))
    (md--insert-snippet-impl
     (make-md-snippet
      :name "funk call"
      :contents c
      :context nil))))

(md-make-snippets
 t
 '(("path" "$1/$2")
   ("band" "$1 | $2")))

(md-make-snippets
 '(derived-mode-p 'emacs-lisp-mode)
 '(("conned" "(cond\n($1)\n($2))")
   ("defun" "(defun $1 ($2) $3)")))

(md-replace-snippet
 :name "let"
 :contents "(let (($1)) $2)"
 :context '(derived-mode-p 'emacs-lisp-mode))

(md-replace-snippet
 :name "setq"
 :contents "(setq $1 $2)"
 :context '(derived-mode-p 'emacs-lisp-mode))

(md-replace-snippet
 :name "setq-default"
 :contents "(setq-default $1 $2)"
 :context '(derived-mode-p 'emacs-lisp-mode))

(md-replace-snippet
 :name "message"
 :contents "(message \"$1\" $2)"
 :context '(derived-mode-p 'emacs-lisp-mode))

(md-replace-snippet
 :name "and"
 :contents "(and $1 $2)"
 :context '(derived-mode-p 'emacs-lisp-mode))

(md-replace-snippet
 :name "or"
 :contents "(or $1 $2)"
 :context '(derived-mode-p 'emacs-lisp-mode))

(md-replace-snippet
 :name "lambda"
 :contents "(lambda ($1) $2)"
 :context '(derived-mode-p 'emacs-lisp-mode))

;; most non-lisp languages share a lot of tiny snippets,
;; like infix operators
(defun generic-programming-context ()
  (when (and (derived-mode-p 'prog-mode)
             (not (derived-mode-p 'emacs-lisp-mode))) t))


(md-replace-snippet
 :name "plus"
 :contents "$1 + $2"
 :context '(generic-programming-context))

(md-replace-snippet
 :name "minus"
 :contents "$1 - $2"
 :context '(generic-programming-context))

(md-replace-snippet
 :name "multiply"
 :contents "$1 * $2"
 :context '(generic-programming-context)) 

(md-replace-snippet
 :name "mod"
 :contents "$1 % $2"
 :context '(generic-programming-context)) 

(md-replace-snippet
 :name "divide"
 :contents "$1 / $2"
 :context '(generic-programming-context))

(md-replace-snippet
 :name "assign"
 :contents "$1 = $2"
 :context '(generic-programming-context))

;;(md-insert-snippet "dotimes")

(md-snippet-mode-activate 1)
;;(md-snippet-mode-activate 0)

(load-file "~/etc/emacs/md-cpp-snippet.el")
(load-file "~/etc/emacs/md-python-snippet.el")