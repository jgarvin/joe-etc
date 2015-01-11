(require 'dash)

(defconst md-color-list
      '("red"
        "green"
        "white"
        "purple"
        "yellow"
        "orange"
        "deep pink"))

(defvar md-hl-overlays nil)
(defvar md-hl-timer nil)

(defun md-pick-unbiased-letter (s rand-byte)
  ;; this implementation is stupid slow
  (let ((letter-list))
    (dotimes (i (length s))
      (let ((letter (downcase (aref s i))))
        (when (and (string-match "[a-z]\\|[A-Z]" (char-to-string letter))
                   (not (member letter letter-list)))
          (push letter letter-list))))
    (let* ((choice (mod rand-byte (length letter-list)))
          (letter (char-to-string (nth choice letter-list))))
      (string-match (concat (regexp-quote (upcase letter)) "\\|" (regexp-quote letter)) s))))

(defun md-get-hash-byte (hash byte)
  (string-to-number (substring hash (* 2 byte) (* 2 (+ 1 byte))) 16))

(defun md-string-mods (s)
  (let* ((hash (md5 s))
         (char-choice (md-pick-unbiased-letter s (md-get-hash-byte hash 0)))
         (color-choice (nth (mod (md-get-hash-byte hash 1) (length md-color-list)) md-color-list)))
    (cons char-choice color-choice)))

(defun randomly-underline (s)
  (let* ((mods (md-string-mods s))
         (char-choice (car mods))
         (color-choice (cdr mods))
         (new-s (copy-sequence s))
         (prop-list (list :underline color-choice :weight 'ultra-bold)))
    (add-text-properties char-choice (+ 1 char-choice) (list 'face prop-list 'font-lock-face prop-list) new-s)
    new-s))

(defun count-collisions (strings)
  (let ((sym-table (make-hash-table :test 'equal))
        (highest-collision-count 0)
        (total-collision-count 0)
        (filter-count 0))
    (dolist (s strings)
      (if (or (md-filter-symbol s nil nil)
              (not (string-match "[a-z]\\|[A-Z]" s)))
          (incf filter-count)
        (let* ((mods (md-string-mods s))
               (key (cons (downcase (substring s (car mods) (1+ (car mods)))) (cdr mods)))
               (entry (gethash key sym-table)))
          (if entry
              (progn
                (puthash key (1+ entry) sym-table)
                (incf total-collision-count)
                (setq highest-collision-count (max (1+ entry) highest-collision-count)))
            (puthash key 0 sym-table)))))
    (message "total collisions: %S" total-collision-count)
    (message "filter count: %S" filter-count)
    (message "num strings: %S" (length strings))
    (message "final table:\n%S" sym-table)
    highest-collision-count))

(defun collision-test ()
    (let ((l))
      (dotimes (i 500)
        (push (format "%S" (aref obarray (random (length obarray)))) l))
      (count-collisions l)))

(defun average (f times)
  (let ((c 0))
    (dotimes (i times)
      (setq c (+ c (funcall f))))
    (/ c times)))

(defun md-highlight-symbols ()
  (when md-hl-overlays
    (mapc #'delete-overlay md-hl-overlays)
    (setq md-hl-overlays nil))
  (when md-hl-timer
    (cancel-timer md-hl-timer)
    (setq md-hl-timer nil))
  (let ((start (window-start))
        (end (window-end))
        (sym-start))
    (save-excursion
      (goto-char start)
      (while (and (re-search-forward "\\_<" end 1)
                  (< (point) end))
        (setq sym-start (point))
        (re-search-forward "\\_>" end 1)
        (let ((sym (buffer-substring-no-properties sym-start (point))))
          (unless (or (md-filter-symbol sym sym-start (point))
                      (not (string-match "[a-z]\\|[A-Z]" sym)))
            (let* ((mods (md-string-mods sym))
                   (char-choice (car mods))
                   (color-choice (cdr mods))
                   (o (make-overlay (+ sym-start char-choice) (+ sym-start char-choice 1))))
              (overlay-put o 'face (list :underline color-choice :weight 'ultra-bold))
              (push o md-hl-overlays))))))))

(defun md-hl-closest-overlay (x y)
  (< (abs (- (overlay-start x) (point)))
     (abs (- (overlay-start y) (point)))))

(defun md-hl-pick-symbol (letter color)
  (interactive)
  (let ((candidates (-filter
                     (lambda (x) (and (string= letter (buffer-substring (overlay-start x)
                                                                        (1+ (overlay-start x))))
                                      (string= color (plist-get (overlay-get x 'face) :underline))))
                     md-hl-overlays)))
    (when candidates
      (setq candidates (sort candidates #'md-hl-closest-overlay))
      (let* ((choice (car candidates))
             (sym (save-excursion
                    (goto-char (overlay-start choice))
                    (thing-at-point 'symbol))))
        (md-insert-text sym t nil)))))

;;(md-hl-pick-symbol "t" "green")

(defun md-hl-schedule-update ()
  (unless md-hl-timer
    (setq md-hl-timer (run-with-idle-timer 0.25 nil #'md-highlight-symbols))))

(defun md-hl-scroll (w new-start)
  (when (eq w (selected-window))
    (md-hl-schedule-update)))

(defun md-hl-change (beg end len)
  (md-hl-schedule-update))

(add-hook 'window-scroll-functions #'md-hl-scroll)
(add-hook 'md-window-selection-hook #'md-hl-schedule-update)
(add-hook 'after-change-functions #'md-hl-change)
