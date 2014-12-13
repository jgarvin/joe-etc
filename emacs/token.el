;; -*- lexical-binding: t -*-

(setq giant-buffer-size (* 1 1024 1024))
(setq giant-line (* 4 4096))

(defun md-safe-start ()
  (if (< giant-buffer-size (buffer-size))
      (max 0 (- (point) 10000))
    (save-excursion
      (beginning-of-buffer)
      (point))))

(defun md-safe-stop ()
  (if (< giant-buffer-size (buffer-size))
      (min (buffer-size) (+ (point) 10000))
    (save-excursion
      (end-of-buffer)
      (point))))

;; TODO
;; alright, now that we have this machinery...
;; have python call this stuff
;; extract the words
;; subset match spoken words against word list
;; then call find-nearest-word or just insert it
;; maybe filter words in strings? or comments? yes for 'toke'
;; since those things are not tokens
;;
;; for jumping we only need the words in the window
;; for insertion we want the more general possibilities...
;;
;; 'toke <tok>' jumps directly to token
;; 'line <tok>' jumps to start of line
;; 'post toke <tok>'
;; 'post line <tok>'

(defun md-get-symbols (start end)
  "Get all the symbols between start and end"
  (save-excursion
    (let ((words '())
          (sym-start))
      (goto-char start)
      (beginning-of-line)
      (condition-case nil
        (while (> end (point))
          (re-search-forward "\\_<" end)
          (setq sym-start (point))
          (re-search-forward "\\_>" end)
          (setq words (nconc words (list (buffer-substring-no-properties sym-start (point))))))
        (search-failed nil))
      (delete-dups words)
      words)))

(require 'subr-x) ;; for hash-table-keys

(defun md-normalize (value lowest highest)
  (/ (float (- value lowest)) (- highest lowest)))

(defun md-score-token (frequency min-frequency max-frequency
                                 distance min-distance max-distance)
  (sqrt (+ (/ 1 (expt (md-normalize frequency min-frequency max-frequency) 2))
           (expt (* 0.5 (md-normalize distance min-distance max-distance)) 2))))

;; (md-normalize 50 0 100)
;; (md-score-token 1 0 1 1 0 1)

(defvar md-max-tokens 200)
(setq md-max-tokens 200)

(defun md-filter-symbol (sym)
  (cond
   ((< (length sym) 3) t)
   (t nil)))

;; do one regex search, then use match-string
;; store data in array, and only store indexes
;; in the hash table?
;; build vector as we go, sort vector at the
;; end

(defun md-get-symbols-frequency (start end)
  "Get all the symbols between start and end"
  (save-excursion
    (let ((sym-counts (make-hash-table :test 'equal))
          (sym-start)
          (current-sym)
          (starting-point (point))
          (min-distance most-positive-fixnum)
          (max-distance 0)
          (max-frequency 0)
          (count-list '()))
      (goto-char start)
      (beginning-of-line)
      (condition-case nil
          (while (> end (point))
            (re-search-forward "\\_<" end)
            (setq sym-start (point))
            (re-search-forward "\\_>" end)
            (setq current-sym (buffer-substring-no-properties sym-start (point)))
            (when (not (md-filter-symbol current-sym))
              (let* ((entry (gethash current-sym sym-counts))
                     (cur-distance (abs (- (point) starting-point)))
                     (frequency (if entry (+ (car entry) 1) 1))
                     (distance (if entry (min (cdr entry) cur-distance) cur-distance)))
                (puthash current-sym `(,frequency . ,distance) sym-counts)
                (setq max-frequency (max max-frequency frequency))
                (setq min-distance (min min-distance distance))
                (setq max-distance (max max-distance distance)))))
        (search-failed nil))
      (maphash (lambda (key value)
                 (let ((frequency (car value))
                       (distance (cdr value)))
                   (setq count-list (cons `(,key . ,(md-score-token frequency
                                                                           0
                                                                           max-frequency
                                                                           distance
                                                                           min-distance
                                                                           max-distance)) count-list))))
               sym-counts)
      (setq count-list (sort count-list (lambda (a b) (< (cdr a) (cdr b)))))
;;      (message "count-list %S" count-list)
      (let ((l (length count-list)))
        (when (> l md-max-tokens)
          (setq count-list (nbutlast count-list (- l md-max-tokens)))))
      (mapcar 'car count-list))))

(defvar md-safe-scan-limit 5000)
(md-get-symbols-frequency (point-min) (point-max))

(defun md-safe-get-symbols (start end)
  (let ((distance (abs (- end start)))
        (safe-start start)
        (safe-end end))
    (when (> distance md-safe-scan-limit)
      (setq safe-start (max (point-min) (- (point) md-safe-scan-limit)))
      (setq safe-end (min (point-max) (+ (point) md-safe-scan-limit))))
    (md-get-symbols safe-start safe-end)))

(defun md-safe-get-symbols-frequency (start end)
  (let ((distance (abs (- end start)))
        (safe-start start)
        (safe-end end))
    (when (> distance md-safe-scan-limit)
      ;; (message "Greater than scan limit")
      (setq safe-start (max (point-min) (- (point) md-safe-scan-limit)))
      (setq safe-end (min (point-max) (+ (point) md-safe-scan-limit))))
      ;; (message "Limits %d %d" safe-start safe-end))
    (md-get-symbols-frequency safe-start safe-end)))

(byte-compile 'md-get-symbols)
(byte-compile 'md-safe-get-symbols)
(byte-compile 'md-get-symbols-frequency)
(byte-compile 'md-safe-get-symbols-frequency)

;; (etc-profile-func
;;  (lambda ()
;;    (dotimes (n 10000)
;;      (md-get-symbols-frequency (window-start) (window-end)))))
;; (benchmark 10000 '(md-get-symbols-frequency (window-start) (window-end)))
;; (benchmark 10000 '(md-safe-get-symbols (window-start) (window-end)))
;; (benchmark 10000 '(md-get-symbols (point-min) (point-max)))
;; (benchmark 1 '(md-get-window-words))
;; (benchmark 1 '(md-get-symbols-frequency (window-start) (window-end)))
;; (benchmark 1 '(md-get-symbols-frequency (point-min) (point-max)))

;; (message "%S" (md-get-symbols (window-start) (window-end)))
;; (message "%S" (md-get-symbols-frequency (window-start) (window-end)))
;; (message "%S" (md-get-symbols-frequency (point-min) (point-max)))
;; (message "%S" (md-safe-get-symbols (window-start) (window-end)))

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

