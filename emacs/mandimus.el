(defun mandimus-word-event (words)
  (setq mandimus-last-word-event words))

(setq md-startup-cursor-color (face-attribute 'cursor :background))

(defvar md-cursor-color md-startup-cursor-color)

(defun md-update-cursor-color (color)
  (setq md-cursor-color color)
  (set-cursor-color color))

(defun md-new-mic-state-impl (state)
  (cond
   ((string= state "on") (md-update-cursor-color "green"))
   ((string= state "off") (md-update-cursor-color "red"))
   ((string= state "sleeping") (md-update-cursor-color "yellow"))
   ((string= state "disconnected") (md-update-cursor-color "orange"))
   ((string= state "server-disconnected") (md-update-cursor-color "purple"))
   (t (md-update-cursor-color md-startup-cursor-color))))

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
  (re-search-backward "[[:blank:]]" (point-at-bol) t))

(defun md-next-whitespace-separated-thing ()
  (interactive)
  (re-search-forward "[[:blank:]]" (point-at-eol) t))

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

(load-file "~/etc/emacs/token.el")
