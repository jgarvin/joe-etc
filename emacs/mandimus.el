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
	  "")))
    (cond
     ((bobp) nil)
     ((and (equal major-mode 'erc-mode) (md-at-start-of-erc-input-line)) nil)
     (isearch-mode nil)
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
