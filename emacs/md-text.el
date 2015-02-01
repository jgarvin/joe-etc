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

(defun md-matching-pairs ()
  (let ((result))
    (if (and (boundp 'sp-local-pairs)
             sp-local-pairs)
        (dolist (i sp-local-pairs)
          (when (equal (plist-get i :close) (plist-get i :open))
            (push (plist-get i :close) result)))
      (setq result (list "\"")))
    result))

(defun md-char-regex (l)
  (regexp-opt-group (sort (mapcar #'char-to-string l) 'string=)))
  
(defun md-get-matching-opening-regexp ()
  (regexp-opt (md-matching-pairs)))

(defun md-likely-preceded-by-opener (pos)
  "If the characters preceding pos form a delimeter in a pair
where the opening and closing delimeters differ, then we can know
with certainty that the character is an opener and return t.

When the delimeter is part of a pair where the opening and closing
strings are the same, like quotes, We use a heuristic to decide 
if a delimeter is an opener or a closer. If the character prior is
whitespace, then it's probably an opener. If the string prior
is an opener, then we're likely an opener too.

If the string preceeding pos isn't part of any pair, then returns nil."
  (save-excursion
    (goto-char pos)
    (let ((preceding-unmatching-opener)
          (preceding-matching-opener))
      (if (setq preceding-unmatching-opener
                (sp--looking-back (sp--get-opening-regexp)))
          (goto-char (match-beginning 0))
        (when (setq preceding-matching-opener
                    (sp--looking-back (md-get-matching-opening-regexp)))
          (goto-char (match-beginning 0))))
      (cond
       ;; We have a distinct opener, return t
       (preceding-unmatching-opener t)
       ;; We have neither kind of opener, return nil
       ((not preceding-matching-opener) nil)
       ;; We have a matching opener with nothing in front,
       ;; must be an opener, return t
       ((bobp) t)
       ;; We have a matching opener preceded by whitespace,
       ;; probably an opener, return t
       ((looking-back "[[:space:]\n]" 1) t)
       ;; We have a matching opener preceded by an identical
       ;; matching opener, so this is actually the closer,
       ;; return nil
       ((looking-back (regexp-quote (buffer-substring (match-beginning 0) (match-end 0)))) nil)
       ;; We have a matching opener preceded by another,
       ;; probably this is one too, return t
       ((md-likely-preceded-by-opener (point)) t)))))

(defun md-likely-followed-by-closer (pos)
  "If the characters preceding pos form a delimeter in a pair
where the opening and closing delimeters differ, then we can know
with certainty that the character is an opener and return t.

When the delimeter is part of a pair where the opening and closing
strings are the same, like quotes, We use a heuristic to decide 
if a delimeter is an opener or a closer. If the character prior is
whitespace, then it's probably an opener. If the string prior
is an opener, then we're likely an opener too.

If the string preceeding pos isn't part of any pair, then returns nil."
  (save-excursion
    (goto-char pos)
    (let ((following-unmatching-closer)
          (following-matching-closer))
      (if (setq following-unmatching-closer
                (sp--looking-at (sp--get-closing-regexp)))
          (goto-char (match-end 0))
        (when (setq following-matching-closer
                    (sp--looking-at (md-get-matching-opening-regexp)))
          (goto-char (match-end 0))))
      (cond
       ;; We have a distinct closer, return t
       (following-unmatching-closer t)
       ;; We have neither kind of closer, return nil
       ((not following-matching-closer) nil)
       ;; We have a matching closer with nothing after,
       ;; must be a closer, return t
       ((eobp) t)
       ;; We have a matching closer followed by whitespace,
       ;; probably a closer, return t
       ((looking-at "[[:space:]\n]") t)
       ;; We have a matching closer followed by an identical
       ;; matching closer, so this is actually the closer,
       ;; return nil
       ((looking-at (regexp-quote (buffer-substring (match-beginning 0) (match-end 0)))) nil)
       ;; We have a matching closer followed by another,
       ;; probably this is one too, return t
       ((md-likely-followed-by-closer (point)) t)))))

(defun md-space-inhibiting-before-chars ()
  (let ((l (if (derived-mode-p 'prog-mode)
               (list ?  ?\n ?\t ?_ ?@ ?\[ ?\{ ?\( ?/ ?\\ ?- ?\] ?. ?! ?\# ?\$)
             (list ?  ?\n ?\t ?_ ?@ ?\[ ?\{ ?\( ?/ ?\\ ?- ?\] ?. ?\# ?\$))))
    (when (derived-mode-p 'emacs-lisp-mode)
      (push ?\' l)
      (push ?\, l)
      (push ?: l))
    (when (derived-mode-p 'python-mode)
      (setq l (delq ?\# l)))      
    (md-char-regex l)))

(defun md-space-inhibiting-after-chars ()
  (let ((l (list ?  ?\n ?\t ?_ ?\] ?\) ?\} ?\/ ?\\ ?- ?. ?? ?! ?\,)))  
    (when (derived-mode-p 'emacs-lisp-mode)
      (setq l (delq ?\, l)))
    (md-char-regex l)))

(defun md-need-space (str)
  (let ((space-inhibiting-before-characters (md-space-inhibiting-before-chars))
        (space-inhibiting-characters (md-space-inhibiting-after-chars)))
    (cond
     ((bobp) nil)
     ((and (equal major-mode 'erc-mode) (md-at-start-of-erc-input-line)) nil)
     (isearch-mode nil)
     ((md-likely-preceded-by-opener (point)) nil)
     ((string-match space-inhibiting-characters (char-to-string (aref str 0))) nil)
     ((save-excursion
        (re-search-backward space-inhibiting-before-characters (1- (point)) t)) nil)
     (t t))))

(defun md-need-space-after (str)
  (let ((space-inhibiting-after-characters (md-space-inhibiting-after-chars))
        (space-inhibiting-characters (md-space-inhibiting-before-chars)))
    (cond
     ((eobp) nil)
     (isearch-mode nil)
     ;; we don't need to insert a space in front of the snippet character 
     ((and (boundp 'md-placeholder) (equal (char-after) md-placeholder)) nil)
     ((md-likely-followed-by-closer (point)) nil)
     ((string-match space-inhibiting-characters (char-to-string (aref str (1- (length str))))) nil)
     ((save-excursion
        (re-search-forward space-inhibiting-after-characters (1+ (point)) t)) nil)
     (t t))))

(defun md-insert-text (text check-spaces check-capitals)
  (interactive)
  (assert (stringp text))
  ;; we may be inserting text that originally came from a read
  ;; only portion of the buffer,for example using the symbol picker 
  (remove-text-properties 0 (length text) (list 'read-only nil) text)
  (when (and check-capitals
             (md-need-capitalization))
    (setq text (concat (char-to-string (upcase (aref text 0)))
                       (subseq text 1))))
  (when (or (not md-recent-ring) (/= md-recent-ring-size (ring-size md-recent-ring)))
    (setq md-recent-ring (make-ring md-recent-ring-size)))
  (ring-insert md-recent-ring text)
  (when (and check-spaces
             (md-need-space text))
    (setq text (concat " " text)))
  (when (and check-spaces
             (md-need-space-after text))
    ;;(message "appending space") 
    (setq text (concat text " ")))
  (let ((p (point)))
    (insert text)
    (save-excursion
      (when (and (save-excursion
                   (re-search-forward "[[:space:]\n]" (1+ (point)) t)))
        (just-one-space))
      (goto-char p)
      (when (and (save-excursion
                   (re-search-backward "[[:space:]]" (1- (point)) t))
                 (not (md-at-start-of-erc-input-line))
                 (not (bolp))
                 (md-causes-move #'back-to-indentation))
        (just-one-space))))
  ;; close the company pop-up window
  (when (fboundp #'company-cancel)
    (company-cancel 'abort)))
 
