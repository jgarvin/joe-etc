;; -*- lexical-binding: t -*-

(defvar md-initial-vec-size 500)
(defvar md-max-tokens 200)
(defvar giant-buffer-size (* 1 1024 1024)))
(defvar giant-line (* 4 4096))
(defvar md-safe-scan-limit 5000)
(defvar-local md-sym-tracking-vec (make-vector md-initial-vec-size 0))
(defvar-local md-sym-tracking-capacity 0)

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

;; taken from: https://gist.github.com/Wilfred/f7d61b7cdf9fdbb1d11c
(defun md-get-faces (text)
  "Get the font faces at TEXT."
  ;;(message "function actually ran")
  (remq nil
        (list
         (get-char-property 0 'read-face-name text)
         (get-char-property 0 'face text)
         (plist-get (text-properties-at 0 text) 'face))))
(byte-compile 'md-get-faces)

;; TODO: filter language keywords
;; TODO: in text modes filter most common words
(defun md-quick-sort (vec p q pred)
  (let ((r))
    (when (< p q)
      (setq r (md-partition vec p q pred))
      (md-quick-sort vec p r pred)
      (md-quick-sort vec (+ r 1) q pred)))
  vec)
(byte-compile 'md-quick-sort)

(defun md-filter-symbol (sym)
  (cond
   ((< (length sym) 3) t)
   ((not (= (string-to-number sym) 0)) t)
   ((let ((face-list (md-get-faces sym)))
      (or
       (memq 'font-lock-constant-face face-list)
       (memq 'font-lock-builtin-face face-list)
       (memq 'font-lock-keyword-face face-list))) t)
   (t nil)))
(byte-compile 'md-filter-symbol)

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

(setq gc-cons-threshold (* 1 800000))

;; TODO: This would be faster if it used a heap
;; TODO: only need one search and use match-beginning/end
;; TODO: Return a reused vector to avoid more GC
(defun md-get-symbols (start end)
  "Get all the symbols between start and end"
  (save-excursion
    (let ((sym-counts (make-hash-table :test 'equal))
          (sym-start)
          (current-sym)
          (starting-point (point))
          (min-distance most-positive-fixnum)
          (max-distance 0)
          (max-frequency 0)
          ;;(vec (make-vector md-initial-vec-size 0))
          ;;(vec md-sym-tracking-vec)
          (vec-size 0))
          ;;(vec-capacity md-initial-vec-size))
      (goto-char start)
      (beginning-of-line)
      (condition-case nil
          (while (> end (point))
            (re-search-forward "\\_<" end)
            (setq sym-start (point))
            (re-search-forward "\\_>" end)
            (setq current-sym (buffer-substring sym-start (point)))
            ;;(message "hello %S" (text-properties-at 0 current-sym))
            (when (not (md-filter-symbol current-sym))
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
      (md-quick-sort md-sym-tracking-vec 0 vec-size (lambda (a b) (< (aref a 3) (aref b 3))))
      (let ((i 0)
            (results '()))
        (while (and (< i vec-size) (< i md-max-tokens))
          (let ((inner-vec (aref md-sym-tracking-vec i)))
            (setq results (nconc results `(,(aref inner-vec 0)))))
          (setq i (+ i 1)))
        results))))
(byte-compile 'md-get-symbols)

(defun md-safe-get-symbols (start end)
  (let ((distance (abs (- end start)))
        (safe-start start)
        (safe-end end))
    (when (> distance md-safe-scan-limit)
      ;; (message "Greater than scan limit")
      (setq safe-start (max (point-min) (- (point) md-safe-scan-limit)))
      (setq safe-end (min (point-max) (+ (point) md-safe-scan-limit))))
      ;; (message "Limits %d %d" safe-start safe-end))
    (md-get-symbols safe-start safe-end)))
(byte-compile 'md-safe-get-symbols)


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

;;(defun md-)

;; (etc-profile-func
;;  (lambda ()
;;    (dotimes (n 10000)
;;      (md-get-symbols-frequency (window-start) (window-end)))))
;; (etc-profile-func
;;  (lambda ()
;;    (dotimes (n 10000)
;;      (md-get-symbols-frequency-vec (window-start) (window-end)))))
;; (benchmark 10000 '(md-safe-get-symbols (window-start) (window-end)))
;; (benchmark 10000 '(md-get-symbols (point-min) (point-max)))
;; (benchmark 1 '(md-get-symbols (window-start) (window-end)))
;; (benchmark 1 '(md-get-symbols (point-min) (point-max)))

;; (message "%S" (md-get-symbols (window-start) (window-end)))
;; (message "%S" (md-get-symbols-frequency (window-start) (window-end)))
;; (message "%S" (md-get-symbols-frequency (point-min) (point-max)))
;; (message "%S" (md-safe-get-symbols (window-start) (window-end)))
