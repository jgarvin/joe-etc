;; we do this because C-g will break emacsclient eval
;; requests. I tried rebinding C-g to a wrapper, but my
;; wrapper wouldn't trigger when emacs was in the server
;; code. Turns out C-g is special somehow, and just binding
;; a different key to the same function doesn't have
;; that behavior!
(global-set-key (kbd "C-S-g") 'keyboard-quit)

(defun md-line-is-blank ()
  (interactive)
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

(setq md-startup-cursor-color (face-attribute 'cursor :background))

(defvar md-cursor-color md-startup-cursor-color)

(defun md-update-cursor-color (color)
  (setq md-cursor-color color)
  (set-cursor-color color))

(defun md-new-mic-state-impl (state)
  (cond
   ((string= state "on") (md-update-cursor-color "green"))
   ((string= state "success") (md-update-cursor-color "green"))
   ((string= state "off") (md-update-cursor-color "brown"))
   ((string= state "sleeping") (md-update-cursor-color "yellow"))
   ((string= state "disconnected") (md-update-cursor-color "orange"))
   ((string= state "server-disconnected") (md-update-cursor-color "purple"))
   ((string= state "thinking") (md-update-cursor-color "blue"))
   ((string= state "failure") (md-update-cursor-color "red"))
   (t
    (message "Unknown mic state: %s" state)
    (md-update-cursor-color md-startup-cursor-color))))

(defun md-new-mic-state (state)
  (dolist (frame (frame-list))
    (with-selected-frame frame
      (md-new-mic-state-impl state))))

(defun md-give-new-frame-color (frame)
  (with-selected-frame frame
    (md-update-cursor-color md-cursor-color)))

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
  (if (equal major-mode 'erc-mode)
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
      (save-window-excursion
	(select-window window)
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

(defun md-need-capitalization ()
  (interactive)
  (cond
   ((not (member major-mode '(text-mode fundamental-mode erc-mode))) nil)
   ((md-beginning-of-input) t)
   ((save-excursion
    (re-search-backward "[^[:blank:]]" (max (- (point) 10000) (point-min)) t)
    ;; string-match when it matches can return 0, which we still want to be true,
    ;; so yeah double negation
    (not (not (string-match "[?!.]" (char-to-string (char-after)))))) t)
   (t nil)))

(defun md-need-space (str)
  (let
      ;; the presence of these before point mean we shouldn't include a space
      ((space-inhibiting-before-characters
	(if (member major-mode '(text-mode fundamental-mode erc-mode))
	    "[[:blank:]\n\"_@[{(/\\-]"
	  "[[:blank:]\n\"'_@[{(/\\.!-]"))
       ;; these being the first character we're going to insert shouldn't include a space
       (space-inhibiting-characters
	(if (member major-mode '(text-mode fundamental-mode erc-mode))
	    "[?!.]"
	  "[)\]}]")))
    (cond
     ((bobp) nil)
     (isearch-mode nil)
     ((and (equal major-mode 'erc-mode) (md-at-start-of-erc-input-line)) nil)
     ((string-match space-inhibiting-characters (char-to-string (aref str 0))) nil)
     ((save-excursion
	(re-search-backward space-inhibiting-before-characters (- (point) 1) t)) nil)
     (t t))))

(defun md-backward-kill-word ()
  (interactive)
  (backward-word)
  (kill-word 1))

(defun md-forward-kill-word ()
  (interactive)
  (forward-word)
  (backward-word)
  (kill-word 1))

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

(defun md-mark-word ()
  (interactive)
  (forward-word)
  (backward-word)
  (set-mark (point))
  (forward-word))

(defun md-mark-line ()
  (interactive)
  (beginning-of-line)
  (set-mark (point))
  (end-of-line))

(defun md-mark-paragraph ()
  (interactive)
  (forward-paragraph)
  (backward-paragraph)
  (next-line)
  (set-mark (point))
  (forward-paragraph))

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

(defun md-get-active-erc-nicknames ()
  (md-filter (lambda (x) (not (erc-lurker-p x))) (erc-get-channel-nickname-list)))

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

(require 'cl)

(defun md-get-relative-column (p)
  (save-excursion
    (goto-char p)
    (back-to-indentation)
    (let ((distance (- (md-get-column p) (md-get-column (point)))))
      (assert (>= distance 0))
      distance)))
    
(defun md-vertical-biased-distance (a b)
  (sqrt (+ (expt (* 2 (md-get-relative-column b)) 4)
           (expt (abs (- (line-number-at-pos a) (line-number-at-pos b))) 2))))

(defun md-find-line-starting-with-char (arg char)
  (interactive "p\ncGo to line starting with char: ")
  (let* ((direction (if (>= arg 0) 1 -1))
         (end (if (= direction 1) (point-max) (point-min)))
         (compare (if (= direction 1) '< '>))
         (origin (point))
         (start-of-line)
         (closest-distance most-positive-fixnum)
         (closest-point))
    (save-excursion
      (while (funcall compare (point) end)
        (forward-line direction)
        (back-to-indentation)
        (setq start-of-line (point))
        (when (re-search-forward (char-to-string char) (point-at-eol) t)
          (let ((distance (md-vertical-biased-distance origin (point))))
            (when (< distance closest-distance)
              (setq closest-point start-of-line)
              (setq closest-distance distance))))
        ;; without this it's possbile point never reaches point-min
        (beginning-of-line)))
    (when closest-point
      (goto-char closest-point))))
    
(defun md-move-up-to-symbol-starting-with-char (arg char)
  (interactive "p\ncMove up to symbol starting with char: ")
  (let ((direction (if (>= arg 0) 1 -1)))
    (forward-symbol direction)
    (unwind-protect
        (re-search-forward (concat "\\_<" (char-to-string char)) nil nil arg)
      (forward-symbol (* -1 direction)))
    (point)))

(defun md-move-up-to-char (arg char)
  (interactive "p\ncMove up to char: ")
  (let ((direction (if (>= arg 0) 1 -1)))
    (forward-char direction)
    (unwind-protect
        (search-forward (char-to-string char) nil nil arg)
      (backward-char direction))
    (point)))

(defun md-current-path ()
  (if (derived-mode-p 'dired-mode)
      list-buffers-directory
    buffer-file-name))

;; (defun md-wrap-sexp ()
;;   (interactive)
;;   (sp-rewrap-sexp))

;; needed for zap-up-to-char
(require 'misc)

(load-file "~/etc/emacs/md-network.el")
(load-file "~/etc/emacs/token.el")
