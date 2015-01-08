(setq md-color-list
      '("red"
        "green"
        "white"
        "purple"
        "yellow"
        "orange"
        "deep pink"))

(defun md-get-hash-byte (hash byte)
  (string-to-number (substring hash (* 2 byte) (* 2 (+ 1 byte))) 16))

(defun md-string-mods (s)
  (let* ((hash (md5 s))
         (char-choice (mod (md-get-hash-byte hash 0) (length s)))
         (color-choice (nth (mod (md-get-hash-byte hash 1) (length md-color-list)) md-color-list)))
    (cons char-choice color-choice)))

(defun md-string-mods (s)
  (let* ((hash (md5 s))
         (color-choice (nth (mod (md-get-hash-byte hash 1) (length md-color-list)) md-color-list)))
    (cons (cons (aref s 0) (aref s (- (length s) 1))) color-choice)))

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
        (highest-collision-count 0))
    (dolist (s strings)
      (let* ((mods (md-string-mods s))
             (entry (gethash mods sym-table)))
        (if entry
            (progn
              (puthash mods (1+ entry) sym-table)
              (setq highest-collision-count (max (1+ entry) highest-collision-count)))
          (puthash mods 0 sym-table))))
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

(average #'collision-test 500)

(dotimes (i 500)
  (insert "\n" (randomly-underline (format "%S" (aref obarray (random (length obarray)))))))

