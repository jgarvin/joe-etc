;; -*- lexical-binding: t -*-

(require 'dash)

(defvar md-initial-vec-size 500)
(defvar md-max-tokens 200)
(defvar giant-buffer-size (* 1 1024 1024))
(defvar giant-line (* 4 4096))
(defvar md-safe-scan-limit 5000)
(defvar-local md-sym-tracking-vec (make-vector md-initial-vec-size 0))
(defvar-local md-sym-tracking-capacity 0)
(defvar-local md-refresh-timer nil)
(defvar-local md-symbols-cache nil)
(defvar md-symbols-cache-refresh-hook nil)
(defvar md-mode-keywords nil)
(defvar md-min-symbol-length 3)
(defvar md-max-symbol-length 50)
(defvar md-nick-scan-limit 5000)
(defvar-local md-active-erc-nicknames nil)

(defun md-register-mode-keywords (mode keywords)
  (let ((entry (assoc mode md-mode-keywords)))
    (if entry
        (setcdr entry keywords)
      (push (cons mode keywords) md-mode-keywords))))

(defun md-safe-start ()
  (if (< giant-buffer-size (buffer-size))
      (max 0 (- (point) 10000))
    (save-excursion
      (goto-char (point-min))
      (point))))
(byte-compile 'md-safe-start)

(defun md-safe-stop ()
  (if (< giant-buffer-size (buffer-size))
      (min (buffer-size) (+ (point) 10000))
    (save-excursion
      (goto-char (point-max))
      (point))))
(byte-compile 'md-safe-stop)

(defun md-normalize (value lowest highest)
  (/ (float (- value lowest)) (- highest lowest)))
(byte-compile 'md-normalize)

(defun md-score-token (frequency min-frequency max-frequency
                                 distance min-distance max-distance)
  (sqrt (+ (/ 1 (expt (md-normalize frequency min-frequency max-frequency) 2))
           (expt (* 0.5 (md-normalize distance min-distance max-distance)) 2))))
(byte-compile 'md-score-token)

(defvar md-symbol-filter-faces nil)
(setq md-symbol-filter-faces
      '(font-lock-constant-face
        font-lock-comment-face
        font-lock-doc-face
        font-lock-string-face
        font-lock-builtin-face
        font-lock-keyword-face
        erc-nick-default-face
        erc-nick-my-face
        erc-notice-face))

(defun md-get-all-faces (sym-start sym-end)
  (let ((i sym-start)
        (result))
    (when (< sym-end sym-start)
      (setq sym-start sym-end)
      (setq sym-end i))
    (while (and (< i sym-end) (null result))
      (let ((face-or-faces (get-char-property i 'face)))
        (unless (listp face-or-faces)
          (setq face-or-faces (list face-or-faces)))
        (setq result (nconc result face-or-faces))
        (setq i (next-char-property-change i sym-end))))
    result))

(defun md-get-faces-at-pos (text pos)
  "Get the font faces at TEXT."
  (get-char-property pos 'face text))
(byte-compile 'md-get-faces-at-pos)

(defun md-string-contains-faces (sym-start sym-end faces)
  (let ((i sym-start)
        (result))
    (when (< sym-end sym-start)
      (setq sym-start sym-end)
      (setq sym-end i))
    (while (and (< i sym-end) (null result))
      (let ((face-or-faces (get-char-property i 'face)))
        (unless (listp face-or-faces)
          (setq face-or-faces (list face-or-faces)))
        (when (-intersection face-or-faces faces)
          (setq result t)))
      (setq i (next-char-property-change i sym-end)))
    result))
(byte-compile 'md-string-contains-faces)

;; taken from: http://stackoverflow.com/a/11848341/50385
(defun md-how-many-str (regexp str &optional max)
  (save-match-data
    (loop with start = 0
          for count from 0
          while (and (string-match regexp str start) (or (not max) (< count max)))
          do (setq start (match-end 0))
          finally return count)))
(byte-compile 'md-how-many-str)

(defun md-filter-symbol (sym sym-start sym-end &optional dont-check-max dont-check-faces)
  (let ((entry)
        (non-ws-char-count
         (md-how-many-str "[^\\\n[:space:]]" sym
                          (+ 1 md-min-symbol-length))))
    (cond
     ((< non-ws-char-count md-min-symbol-length)  t)
     ((and (not dont-check-max) (> non-ws-char-count md-max-symbol-length)) t)
     ((= (md-how-many-str "[^0-9]" sym 1) 0) t)
     ((and (setq entry (assoc major-mode md-mode-keywords))
           (member sym (cdr entry))) t)
     ((and (not dont-check-faces)
           sym-start
           sym-end
           (md-string-contains-faces sym-start sym-end md-symbol-filter-faces)) t)
     (t nil))))
(byte-compile 'md-filter-symbol)

;; (with-current-buffer "#emacs"
;;   (save-excursion
;;     (setq temp (point))
;;     (forward-symbol 1)
;;     (md-string-contains-faces temp (point) md-symbol-filter-faces)))

;; (with-current-buffer "#python"
;;   (save-excursion
;;     (setq temp (point))
;;     (forward-symbol 1)
;;     (md-filter-symbol (buffer-substring-no-properties temp (point)) temp (point))))

(defun md-quick-sort (vec p q pred)
  (let ((r))
    (when (< p q)
      (setq r (md-partition vec p q pred))
      (md-quick-sort vec p r pred)
      (md-quick-sort vec (+ r 1) q pred)))
  vec)
(byte-compile 'md-quick-sort)

(defun md-partition (vec p q pred)
  (let ((x (aref vec p))
        (i p)
        (j (+ p 1)))
    (while (< j q)
      (when (funcall pred (aref vec j) x)
        (setq i (+ i 1))
        (md-swap vec i j))
      (setq j (+ j 1)))
    (md-swap vec i p)
    i))
(byte-compile 'md-partition)

(defun md-swap (vec a b)
  (let ((temp (aref vec a)))
    (aset vec a (aref vec b))
    (aset vec b temp)))
(byte-compile 'md-swap)

;; TODO: This would be faster if it used a heap
;; TODO: only need one search and use match-beginning/end
;; TODO: only run on changed parts of buffer? hard to 'subtract out'
(defun md-get-symbols-impl (start end)
  "Get all the symbols between start and end"
  (save-match-data
    (save-excursion
      (let ((sym-counts (make-hash-table :test 'equal))
            (sym-start)
            (current-sym)
            (starting-point (point))
            (min-distance most-positive-fixnum)
            (max-distance 0)
            (max-frequency 0)
            (vec-size 0))
        (goto-char start)
        (beginning-of-line)
        (condition-case nil
            (while (> end (point))
              (re-search-forward "\\_<" end)
              (setq sym-start (point))
              (re-search-forward "\\_>" end)
              (setq current-sym (buffer-substring-no-properties sym-start (point)))
              (when (not (md-filter-symbol current-sym sym-start (point)))
                (set-text-properties 0 (length current-sym) nil current-sym)
                (let ((entry (gethash current-sym sym-counts))
                      (cur-distance (abs (- (point) starting-point))))
                  (if entry
                      (progn
                        (let ((inner-vec (aref md-sym-tracking-vec entry)))
                          (aset inner-vec 1 (+ (aref inner-vec 1) 1))
                          (aset inner-vec 2 (min (aref inner-vec 2) cur-distance))))
                    (progn
                      (when (>= vec-size md-sym-tracking-capacity)
                        (let ((new-vec (make-vector (max (* md-sym-tracking-capacity 2) 1) 0))
                              (i 0))
                          (while (< i vec-size)
                            (aset new-vec i (aref md-sym-tracking-vec i))
                            (setq i (+ i 1)))
                          (setq md-sym-tracking-vec new-vec)
                          (setq md-sym-tracking-capacity (length md-sym-tracking-vec))))
                      (aset md-sym-tracking-vec vec-size (vector current-sym 1 cur-distance 0))
                      (puthash current-sym vec-size sym-counts)
                      (setq entry vec-size)
                      (setq vec-size (+ vec-size 1))))
                  (let* ((inner-vec (aref md-sym-tracking-vec entry))
                         (frequency (aref inner-vec 1))
                         (distance (aref inner-vec 2)))
                    (setq max-frequency (max max-frequency frequency))
                    (setq min-distance (min min-distance distance))
                    (setq max-distance (max max-distance distance))))))
          (search-failed nil))
        (let ((i 0))
          (while (< i vec-size)
            (let* ((inner-vec (aref md-sym-tracking-vec i))
                   (frequency (aref inner-vec 1))
                   (distance (aref inner-vec 2)))
              (aset inner-vec 3 (md-score-token frequency 0 max-frequency distance min-distance max-distance)))
            (setq i (+ i 1))))
        (md-quick-sort md-sym-tracking-vec 0 vec-size (lambda (a b) (> (aref a 3) (aref b 3))))
        (let ((i 0)
              (results '()))
          (while (and (< i vec-size) (< i md-max-tokens))
            (let ((inner-vec (aref md-sym-tracking-vec i)))
              (setq results (cons (aref inner-vec 0) results)))
            (setq i (+ i 1)))
          results)))))
(byte-compile 'md-get-symbols-impl)

(defun md-safe-get-symbols-impl (start end)
  (let ((distance (abs (- end start)))
        (safe-start start)
        (safe-end end))
    (when (> distance md-safe-scan-limit)
      (setq safe-start (max (point-min) (- (point) md-safe-scan-limit)))
      (setq safe-end (min (point-max) (+ (point) md-safe-scan-limit))))
    (md-get-symbols-impl safe-start safe-end)))
(byte-compile 'md-safe-get-symbols-impl)

(defun md-refresh-symbol-cache (buf)
  (with-current-buffer buf
    (setq md-symbols-cache (md-safe-get-symbols-impl (point-min) (point-max)))
    ;;(message "running hook")
    (run-hooks 'md-symbols-cache-refresh-hook)
    (when md-refresh-timer
      (cancel-timer md-refresh-timer)
      (setq md-refresh-timer nil))))
(byte-compile 'md-refresh-symbol-cache)

;; TODO: put back var for md-updating-projectile-files
;; TODO: why is running hook constantly printing?

(defun md-refresh-symbols (&optional _start _end _old-length)
  (unless (or md-refresh-timer
              (window-minibuffer-p)
              (minibufferp)
              (not (eq (current-buffer) (window-buffer)))
              ;; for some reason getting projectile files triggers buffer-list-update-hook
              (and (boundp 'md-updating-projectile-files) md-updating-projectile-files))
    ;; (message "** %S" (car (last (buffer-list))))
    ;; (message "** %S" (with-current-buffer (car (last (buffer-list))) (buffer-string)))
    (setq md-refresh-timer
          (run-with-idle-timer 0.5 nil (lambda () (md-refresh-symbol-cache (current-buffer)))))))
(byte-compile 'md-refresh-symbols)

(add-hook 'after-change-functions 'md-refresh-symbols)
(add-hook 'md-window-selection-hook 'md-refresh-symbols)

(defun md-find-nearest-word-impl (word start end)
  (setq sites '())
  (save-excursion
    (goto-char start)
    (catch 'break
      (while (> end (point))
	(setq result (search-forward word end t))
	(when (not result)
	    (throw 'break nil))
	(setq sites (cons result sites)))))
  (setq sites
	(sort sites
	      (lambda (x y)
		(< (abs (- (point) x)) (abs (- (point) y))))))
  (if sites
      (goto-char (nth 0 sites))
    (error "No instance of word.")))

(defun md-find-nearest-word-buffer (word)
  (md-find-nearest-word-impl word (md-safe-start) (md-safe-stop)))

(defun md-find-nearest-word-window (word)
  (md-find-nearest-word-impl word (window-start) (window-end)))

(defun md-get-active-erc-nicknames (&optional max-results)
  (when (equal major-mode 'erc-mode)
    (let ((candidates (erc-get-channel-nickname-list)))
      (when candidates
        (let* ((regex (concat "\\(" (mapconcat (lambda (x) (concat "\\<" (regexp-quote x) "\\>")) candidates "\\|") "\\)"))
               (scan-limit (max (point-min) (- (point-max) (max md-nick-scan-limit (- (window-end) (window-start))))))
               (scan-start (point-max))
               (presence (make-hash-table :test 'equal))
               (results))
          (save-excursion
            (goto-char (point-max))
            (while (and (not (bobp))
                        (> (point) scan-limit)
                        (or (not max-results) (< (length results) max-results))
                        (re-search-backward regex scan-limit 1))
              (when (md-string-contains-faces (match-beginning 0) (match-end 0) '(erc-nick-default-face))
                (let* ((match (match-string-no-properties 0))
                       (entry (gethash match presence))
                       (cur-distance (- scan-start (point))))
                  (unless entry
                    (puthash match 1 presence)
                    (push match results))))))
          (nreverse results))))))

(defun md-channel-buffer-p ()
  (and (equal major-mode 'erc-mode)
       ;; channels only, no server buffers
       (equal 0 (string-match "#.*" (buffer-name)))))

(defun md-update-active-nicks ()
  (when (and (md-channel-buffer-p)
             (erc-buffer-visible (current-buffer))
             (< 1 (buffer-size)))
    (save-restriction
      ;; erc-insert-post-hook runs in a narrowed buffer
      (widen)
      (setq md-active-erc-nicknames (md-get-active-erc-nicknames 50)))))

(add-hook 'erc-insert-post-hook #'md-update-active-nicks)
(add-hook 'md-window-selection-hook #'md-update-active-nicks)
;;(remove-hook 'erc-insert-post-hook #'md-update-active-nicks)
;;(remove-hook 'md-window-selection-hook #'md-update-active-nicks)


