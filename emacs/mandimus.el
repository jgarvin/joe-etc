(require 'cl)
(require 'dash)
(require 'ring)

(defvar md-recent-ring nil)
(defvar md-recent-ring-size 20)

(defvar md-mic-state nil)
(defvar md-in-utterance nil)
(defvar md-start-utterance-hooks nil)
(defvar md-end-utterance-hooks nil)

;; make a selection hook that doesn't fire under circumstances we don't care about
(defvar md-window-selection-hook nil)
(defvar md-last-value-pair nil)
(defvar md-inhibit-window-selection-hooks nil)
(defvar md-last-selected-window nil)

(global-set-key '[md-dummy-event] 'md-ignore)

(defun md-ignore ()
  (interactive)
  (setq this-command last-command))

(defun md-generate-noop-input-event ()
  "Create an input event that does nothing. We use this after inserting text so
that elisp that normally detects input events to determine that the user has
inserted text will fire, e.g. company-mode putting the pop-up away."
  ;; trick taken from company-mode ;)
  (push 'md-dummy-event unread-command-events))

(defun md-safe-cancel-timer (v)
  (when (symbol-value v)
      (when (timerp (symbol-value v))
        (cancel-timer (symbol-value v)))
      (set v nil)))

(defun md--run-timer-func (v f args)
  (unwind-protect
      ;; as far as I can tell (current-buffer)
      ;; is not reliable from an idle timer, you never know
      ;; what you will get, this is a better default
      (apply f args)
    (md-safe-cancel-timer v)))

(defun md-cancel-timer-after-executing (v f local args)
  ;; as far as I can tell (current-buffer)
  ;; is not reliable from an idle timer, you never know
  ;; what you will get, window-buffer is a better default
  (let ((local (or local (window-buffer (selected-window)))))
    (with-current-buffer local
      (md--run-timer-func v f args))))

(defun md-run-when-idle-once (v f seconds &optional local &rest args)
  "Setup an idle timer to run F after SECONDS of idle time, passing
it ARGS. Store the timer in V. Additional calls with the same V will
reset the timer, pushing the event further into the future. This is
the behavior you want if you want a function to only execute once
emacs has \"settled.\" Also workaround an issue with emacs entering
debug mode causing timers to die."
  (md-safe-cancel-timer v)
  ;; The idle timer has to be marked as repeating even though we cancel
  ;; it every time it actually executes. This is because of a race
  ;; condition not handled in the emacs API. If an error occurs triggering
  ;; the debugger then timers won't run. If the timer isn't marked repeating
  ;; it then never runs. If it never runs then it never clears the timer
  ;; variable. If it never clears that variable then the check at the top
  ;; of this function to avoid double timers never passes, and we never
  ;; get to set the timer again.
  (when local
    (setq local (current-buffer)))
  (set v (run-with-idle-timer (time-add (seconds-to-time seconds)
                                        (or (current-idle-time)
                                            '(0 0 0 0))) t
                                            #'md-cancel-timer-after-executing v f local args)))

(defun md-go-to-next (str)
  (interactive)
  (let ((p))
    (save-excursion
      (ignore-errors (end-of-thing 'symbol))
      (if (re-search-forward (concat "\\_<" (regexp-quote str) "\\_>"))
          (setq p (point))
        (user-error "No further instance of string: %s" str)))
    (when p
      (goto-char p)
      (beginning-of-thing 'symbol))))

(defun md-go-to-previous (str)
  (interactive)
  (let ((p))
    (save-excursion
      (ignore-errors (beginning-of-thing 'symbol))
      (if (re-search-backward (concat "\\_<" (regexp-quote str) "\\_>"))
          (setq p (point))
        (user-error "No preceding instance of string: %s" str)))
    (when p
      (goto-char p))))

(defun md-get-next-instance-of-symbol ()
  (interactive)
  (let ((sym (thing-at-point 'symbol)))
    (if sym
        (md-go-to-next sym)
      (user-error "No symbol under point."))))

(defun md-get-previous-instance-of-symbol ()
  (interactive)
  (let ((sym (thing-at-point 'symbol)))
    (if sym
        (progn
          ;; (message "looking for symbol:%s" sym)
          (md-go-to-previous sym))
      (user-error "No symbol under point."))))

(defun md-line-is-blank ()
  (save-excursion
    (beginning-of-line)
    (unless (re-search-forward "[^[:blank:]]" (point-at-eol) t) t)))

(defun md-find-indentation-change (&optional dir compare)
  (interactive)
  (unless dir (setq dir 1))
  (unless compare (setq compare '>))
  (let ((found nil)
        (target-to-exceed (if (md-line-is-blank) 0 (current-indentation))))
    (catch 'outer
      (save-excursion
        (while t
          (forward-line dir) ;; once unconditional otherwise <=,>= will match current line
          (cond
           ((and (not (md-line-is-blank))
                 (funcall compare (current-indentation) target-to-exceed))
            (setq found (point))
            (throw 'outer nil))
           ((bobp) (throw 'outer nil))
           ((eobp) (throw 'outer nil))))))
    (if found
        (progn
          (goto-char found)
          (beginning-of-line)
          (back-to-indentation))
      (error "No line found"))))

(defun mandimus-word-event (words)
  (setq mandimus-last-word-event words)
  (force-mode-line-update))

(defvar md-cursor-color nil)

(defun md-update-cursor-color (color)
  (setq md-cursor-color color)
  (set-cursor-color color))

(defun md-check-start-utterance ()
  (unless md-in-utterance
    ;;(message "Starting utterance")
    (setq md-in-utterance t)
    (run-hooks 'md-start-utterance-hooks)))

(defun md-check-end-utterance ()
  (when md-in-utterance
    ;;(message "Ending utterance")
    (setq md-in-utterance nil)
    (run-hooks 'md-end-utterance-hooks)))

(add-hook 'md-server-connect-hook #'md-check-end-utterance)
(add-hook 'md-server-disconnect-hook #'md-check-end-utterance)

(defun md-new-mic-state-impl (state)
  ;;(message "Mic state: %s" state)
  (cond
   ((string= state "on")
    (md-update-cursor-color "green")
    (md-check-end-utterance))
   ((string= state "success")
    (md-update-cursor-color "green")
    (md-check-end-utterance))
   ((string= state "off")
    (md-update-cursor-color "brown")
    (md-check-end-utterance))
   ((string= state "sleeping")
    (md-update-cursor-color "yellow")
    (md-check-end-utterance))
   ((string= state "disconnected")
    (md-update-cursor-color "orange")
    (md-check-end-utterance))
   ((string= state "server-disconnected")
    (md-update-cursor-color "purple")
    (md-check-end-utterance))
   ((string= state "thinking")
    (md-update-cursor-color "blue")
    (md-check-start-utterance))
   ((string= state "failure")
    (md-update-cursor-color "red")
    (md-check-end-utterance))
   (t
    (message "Unknown mic state: %s" state)
    (md-update-cursor-color md-startup-cursor-color)
    (md-check-end-utterance)))
  (setq md-mic-state state))

(defun md-new-mic-state (state)
  (dolist (frame (frame-list))
    (with-selected-frame frame
      (md-new-mic-state-impl state))))

(defun md-give-new-frame-color (frame)
  (when frame
    (with-selected-frame frame
      (md-update-cursor-color (or md-cursor-color
                                  (face-attribute 'cursor :background))))))

(add-hook 'after-make-frame-functions 'md-give-new-frame-color)

(defun md-previous-whitespace-separated-thing ()
  (interactive)
  (if (re-search-backward "[^[:blank:]][[:blank:]]" (point-at-bol) t)
      (goto-char (+ (point) 1))
    (beginning-of-line)))

(defun md-next-whitespace-separated-thing ()
  (interactive)
  (if (re-search-forward "[[:blank:]][^[:blank:]]" (point-at-eol) t)
      (goto-char (- (point) 1))
    (end-of-line)))

(defun md-select-minibuffer ()
  (interactive)
  (select-window (minibuffer-window)))

(defun md-new-line-anywhere ()
  (interactive)
  (end-of-line)
  (reindent-then-newline-and-indent))

(defun md-open-line-anywhere ()
  (interactive)
  (beginning-of-line)
  (open-line-and-indent))

(defun md-causes-move (f)
  (let ((p (point)))
    (save-excursion
      (funcall f)
      (not (equal (point) p)))))

(defun md-at-start-of-erc-input-line ()
  (not (md-causes-move 'erc-bol)))

(defun md-beginning-of-input ()
  (if (derived-mode-p 'erc-mode)
      (not (md-causes-move 'erc-bol))
    (bobp)))

(defun md-get-most-recently-modified-file (dir regex)
  (let ((best))
    (dolist (info (directory-files-and-attributes dir t regex t))
      (let* ((file  (nth 0 info))
	     (mtime (nth 6 info))
	     (high  (nth 0 mtime))
	     (low   (nth 1 mtime)))
	(when (or (not best)
		  (< (nth 1 best) high)
		  (and (= (nth 1 best) high) (< (nth 2 best) low)))
	  (setq best (list file high low)))))
    (nth 0 best)))

(defvar-local md-most-recent-check-timer nil "Buffer local timer.")

(defun md-cancel-recent-timer ()
  (when (timerp md-most-recent-check-timer)
    (cancel-timer md-most-recent-check-timer)))

(add-hook 'kill-buffer-hook 'md-cancel-recent-timer)

(defun md-setup-most-recent-check (dir regex)
  ;; prevent redundant timers.. shouldn't happen
  (unless (timerp md-most-recent-check-timer)
    (setq md-most-recent-check-timer
	  (run-with-idle-timer 1 t 'md-open-if-not-most-recent (current-buffer) dir regex))))

(defun md-open-if-not-most-recent (buf dir regex)
  (interactive)
  ;; even though the timer *variable* is buffer local, the timer still
  ;; executes regardless of what buffer we're in, so we have to take
  ;; care of basing our checks on the window that's actually currently
  ;; displaying the buffer
  (let ((most-recent (md-get-most-recently-modified-file dir regex))
        (window (get-buffer-window buf 'visible)))
    (when (and window
               (not (string= (buffer-file-name buf) most-recent)))
      (with-selected-window window
        (find-alternate-file most-recent)
        (md-setup-most-recent-check dir regex)))))

(defun md-open-most-recent-file (dir regex)
  (interactive)
  ;;(message "how about this")
  (find-file (md-get-most-recently-modified-file dir regex))
  ;;(message "and this?")
  (md-setup-most-recent-check dir regex))

(defun md-open-most-recent-client-log ()
  (interactive)
  (md-open-most-recent-file "~/dragonshare/log" "client-[^.]*.log"))

(defun md-open-most-recent-server-log ()
  (interactive)
  (md-open-most-recent-file "/tmp" "server-[^.]*.log"))

;; (md-open-most-recent-file "/tmp" "server-[^.]*.log")
;; (md-get-most-recently-modified-file "/tmp" "server-[^.]*.log")

(defun md-backward-kill-word ()
  (interactive)
  (let ((p (point))
        (p2 (progn
              (backward-word)
              (point))))
    (save-restriction
      (narrow-to-region p p2)
      (goto-char (point-min))
      (while (re-search-forward "[^][()<>{};]" nil t) (replace-match "" nil t)))
    ;;(kill-word 1)
    (save-excursion
      (just-one-space)
      (delete-trailing-whitespace (beginning-of-line) (end-of-line)))))

(defun md-forward-kill-word ()
  (interactive)
  (let ((p (point))
        (p2 (progn
              (forward-word)
              ;;(backward-word)
              (point))))
    (save-restriction
      (narrow-to-region (min p p2) (max p p2))
      (goto-char (point-min))
      (while (re-search-forward "[^][()<>{};]" nil t) (replace-match "" nil t)))
    ;;(kill-word 1)
    (save-excursion
      (just-one-space)
      (delete-trailing-whitespace (beginning-of-line) (end-of-line)))))

(defun md-copy-word ()
  (interactive)
  (save-excursion
    (forward-word)
    (backward-word)
    (set-mark (point))
    (forward-word)
    (kill-ring-save (region-beginning) (region-end))))

(defun md-copy-paragraph ()
  (interactive)
  (save-excursion
    (forward-paragraph)
    (backward-paragraph)
    (set-mark (point))
    (forward-paragraph)
    (kill-ring-save (region-beginning) (region-end))))

(defun md-cut-paragraph ()
  (interactive)
  (save-excursion
    (forward-paragraph)
    (backward-paragraph)
    (set-mark (point))
    (forward-paragraph)
    (kill-region (region-beginning) (region-end))))

(defun md-mark-thing (thing)
  (with-selected-window (if (eq (window-buffer) (current-buffer))
                            (selected-window)
                          (get-buffer-window nil t))
    (let ((b (bounds-of-thing-at-point thing)))
      (set-window-point (get-buffer-window) (car b))
      (set-mark (point))
      (set-window-point (get-buffer-window) (cdr b)))))

(defun md-copy-line ()
  "Copy the whole line that point is on and move to the beginning of the next line.
    Consecutive calls to this command append each line to the
    kill-ring."
  (interactive)
  (let ((beg (line-beginning-position 1))
	(end (line-beginning-position 2)))
    (if (eq last-command 'quick-copy-line)
	(kill-append (buffer-substring beg end) (< end beg))
      (kill-new (buffer-substring beg end))))
  (beginning-of-line 2))

(defun md-cut-line ()
  "Cut the whole line that point is on.  Consecutive calls to this command append each line to the kill-ring."
  (interactive)
  (let ((beg (line-beginning-position 1))
	(end (line-beginning-position 2)))
    (if (eq last-command 'quick-cut-line)
	(kill-append (buffer-substring beg end) (< end beg))
      (kill-new (buffer-substring beg end)))
    (delete-region beg end))
  (beginning-of-line 1)
  (setq this-command 'quick-cut-line))


;(mapcar 'buffer-name (buffer-list))

(defun md-create-temp-file (prefix)
  (let ((temporary-file-directory "~/temp"))
    (if (not (file-exists-p temporary-file-directory))
        (make-directory temporary-file-directory))
    (find-file (make-temp-file prefix))))

(defun md-temp-file (prefix &optional window-id)
  (let ((temporary-file-directory "~/temp"))
    (if (not (file-exists-p temporary-file-directory))
	(make-directory temporary-file-directory))
    (select-frame (make-frame))
    (find-file (make-temp-file prefix))
    (insert ";; " (if window-id window-id ""))
    (newline)
    (newline)))

;(md-temp-file "test")
;;(message "now testing agan")
(defun md-filter (condp lst)
  (delq nil
	(mapcar (lambda (x) (and (funcall condp x) x)) lst)))


;; TODO: should be no-op on blank lines
(defun md-go-to-cliff ()
  (interactive)
  (end-of-line)
  (backward-char))

;; adapted from zap-up-to-char
(defun md-copy-up-to-char (arg char)
  "Copy up to, but not including ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found.
Ignores CHAR at point."
  (interactive "p\ncCopy up to char: ")
  (let ((direction (if (>= arg 0) 1 -1)))
    (kill-ring-save (point)
		 (progn
		   (forward-char direction)
		   (unwind-protect
		       (search-forward (char-to-string char) nil nil arg)
		     (backward-char direction))
		   (point)))))

(defun md-get-column (p)
  (save-excursion
    (goto-char p)
    (current-column)))

(defun md-skip-invisible-and-spaces ()
  (beginning-of-line)
  (while (and
          (not (eolp))
          (or (looking-at "[[:space:]\n]")
              (plist-get (text-properties-at (point)) 'invisible)
              ;; (invisible-p (point))
              ))
    (goto-char (1+ (point)))))

(defun md-get-relative-column (p)
  (save-excursion
    (goto-char p)
    (md-skip-invisible-and-spaces)
    ;;(back-to-indentation)
    (let ((distance (- (md-get-column p) (md-get-column (point)))))
      (assert (>= distance 0))
      distance)))

(defun md-vertical-biased-distance (a b)
  (sqrt (+ (expt (* 2 (md-get-relative-column b)) 10)
           (expt (abs (- (line-number-at-pos a) (line-number-at-pos b))) 2))))

(defun md-find-line-starting-with-char (arg char)
  (interactive "p\ncGo to line starting with char: ")
  (let* ((direction (if (>= arg 0) 1 -1))
         (end (if (= direction 1) (point-max) (point-min)))
         (compare (if (= direction 1) '< '>))
         (origin (point))
         (start-of-line)
         (closest-distance most-positive-fixnum)
         (line-extent-func (if (= direction 1) #'end-of-line #'beginning-of-line))
         (closest-point))
    (save-excursion
      (while (funcall compare (point) end)
        (message "point %S end %S" (point) end)
        (forward-line direction)
        ;;(beginning-of-line)
        ;;(back-to-indentation)
        (md-skip-invisible-and-spaces)
        (setq start-of-line (point))
        ;; skip over invisible characters, this is needed to work right
        ;; with dired+, which makes its summary info toggle'able.
        ;; techincally we look at the invisible property being true period
        ;; rather than being a toggle... dired might just need special
        ;; treatment in the future since this could conflict with other
        ;; modes.
        (while (and (re-search-forward (char-to-string char) (point-at-eol) t)
                    (plist-get (text-properties-at (point)) 'invisible)))
        ;; if we found something, record it
        (when (/= (point) start-of-line)
          (let ((distance (md-vertical-biased-distance origin (point))))
            (when (< distance closest-distance)
              (setq closest-point start-of-line)
              (setq closest-distance distance))))
        ;; without this it's possbile point never reaches point-min/max
        (funcall line-extent-func)))
    (when closest-point
      (goto-char closest-point))))

(defun md-move-up-to-symbol-starting-with-char (arg char)
  (interactive "p\ncMove up to symbol starting with char: ")
  (let ((direction (if (>= arg 0) 1 -1)))
    (forward-symbol direction)
    (unwind-protect
        (re-search-forward (concat "\\_<" (regexp-quote (char-to-string char))) nil nil arg)
      (forward-symbol (* -1 direction)))
    (point)))

(defun md-move-up-to-char (arg char)
  (interactive "p\ncMove up to char: ")
  (let ((direction (if (>= arg 0) 1 -1)))
    (forward-char direction)
    (unwind-protect
        (search-forward (char-to-string char) nil nil arg)
      (when (= direction 1) (backward-char direction)))
    (point)))

(defun md-move-up-to-char-same-line (arg char)
  (interactive "p\ncMove up to char: ")
  (let ((direction (if (>= arg 0) 1 -1))
        (eol (if (>= arg 0) (point-at-eol) (point-at-bol))))
    (when (= (point) eol)
      (user-error "%s of line, no characters follow." (if (>= arg 0) "End" "Start")))
    (forward-char direction)
    (unwind-protect
        (condition-case err
            (search-forward (char-to-string char) eol nil arg)
          (search-failed (user-error "No instance of [%s] %s point on current line." (char-to-string char)
                                     (if (>= arg 0) "after" "before"))))
      (when (= direction 1) (backward-char direction)))
    (point)))

(defun md-move-up-to-char-after-line (arg char)
  (interactive "p\ncMove up to char: ")
  (let ((direction (if (>= arg 0) 1 -1)))
    (forward-line direction)
    (unwind-protect
        (condition-case err
            (search-forward (char-to-string char) nil nil arg)
          (search-failed (user-error "No instance of [%s] %s current line." (char-to-string char)
                                     (if (>= arg 0) "after" "before"))))
      (when (= direction 1) (backward-char direction)))
    (point)))

(defun md-current-path ()
  (if (derived-mode-p 'dired-mode)
      list-buffers-directory
    buffer-file-name))

(defun md-run-window-selection-hooks ()
  (unless (or md-inhibit-window-selection-hooks
              ;; for some reason getting projectile files triggers buffer-list-update-hook
              (and (boundp 'md-updating-projectile-files) md-updating-projectile-files)
              ;; opening any old temporary buffer in the background fires buffer-update-list hook,
              ;; when what we're really after is when we select a new window or the buffer in the
              ;; current window changes. note you can't rely on (current-buffer) to be the currently
              ;; displayed window inside buffer-list-update hook, so we use the circumlocution:
              ;; (window-buffer (selected-window)). Also we avoid redundant events by saving the
              ;; last buffer/window pair.
              (and md-last-value-pair (and (eq (window-buffer (selected-window)) (car md-last-value-pair))
                                           (eq (selected-window) (cdr md-last-value-pair)))))
    (let ((md-inhibit-window-selection-hooks t))
      (setq md-last-value-pair (cons (window-buffer (selected-window)) (selected-window)))
      (run-hooks 'md-window-selection-hook))))

(add-hook 'buffer-list-update-hook #'md-run-window-selection-hooks)

(defun md-down-screenful ()
  (interactive)
  (let ((p (point)))
    (while (and (> (/ (window-body-height) 2) (count-lines (point) p))
                (not (eobp)))
      (forward-line 1))))

(defun md-up-screenful ()
  (interactive)
  (let ((p (point)))
    (while (and (> (/ (window-body-height) 2) (count-lines (point) p))
                (not (bobp)))
      (forward-line -1))))

;; (defun md-wrap-sexp ()
;;   (interactive)
;;   (sp-rewrap-sexp))

;; needed for zap-up-to-char
(require 'misc)

(load-file "~/etc/emacs/md-buffer-picking.el")
(load-file "~/etc/emacs/md-text.el")
(load-file "~/etc/emacs/md-network.el")
(load-file "~/etc/emacs/md-token.el")
(load-file "~/etc/emacs/md-projectile.el")
(load-file "~/etc/emacs/md-belt-impl.el")
(load-file "~/etc/emacs/md-symbol-picker.el")
(load-file "~/etc/emacs/md-undo.el")
(load-file "~/etc/emacs/md-snippet.el")
(load-file "~/etc/emacs/md-homophones.el")
(load-file "~/etc/emacs/md-navigation.el")
(load-file "~/etc/emacs/md-edit.el")