;; -*- lexical-binding: t -*-

(setq giant-buffer-size (* 1 1024 1024))
(setq giant-line (* 4 4096))

(defun safe-start ()
  (if (< giant-buffer-size (buffer-size))
      (max (buffer-size) (- (point) 10000))
    (save-excursion
      (beginning-of-buffer)
      (point))))

(defun safe-stop ()
  (if (< giant-buffer-size (buffer-size))
      (max (buffer-size) (- (point) 10000))
    (save-excursion
      (end-of-buffer)
      (point))))

(defun iter-words-impl (action start end)
  (save-excursion
    (goto-char start)
    (beginning-of-line)
    (while (> end (point))
      (funcall action (thing-at-point 'word t))
      (forward-word))))

(defun iter-buffer-words (action)
  (iter-words-impl action (safe-start) (safe-stop)))

(defun iter-window-words (action)
  (iter-words-impl action (window-start) (window-stop)))

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

(defun get-buffer-words ()
  (setq words '())
  (iter-buffer-words
   (lambda (word)
     (setq words (cons word words))))
  (delete-dups words)
  words)

(defun get-window-words ()
  (setq words '())
  (iter-buffer-words
   (lambda (word)
     (setq words (cons word words))))
  (delete-dups words)
  words)

;;(search-forward "buffer" (safe-stop))

(defun find-nearest-word-impl (word start end)
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

(defun find-nearest-word-buffer (word)
  (find-nearest-word-impl word (safe-start) (safe-stop)))

(defun find-nearest-word-window (word)
  (find-nearest-word-impl word (window-start) (window-end)))

