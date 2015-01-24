(require 'dash)

(defconst md-color-list
      '("red"
        "green"
        "white"
        "purple"
        "yellow"
        "orange"))

(defconst md-mark-list
  '(#x030a ;; b̊
    #x031a ;; b̚
    ;;#x0338 ;; b̸
    #x030f ;; b̏
    ;; #x033d ;; b̽
    ;;#x0309
    nil
    ))

(defvar md-hl-overlays nil)
(defvar md-hl-timer nil)
(defvar md-symbol-picker-mode nil)
;;(defvar md-lookup-table nil)

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
         (color-choice (nth (mod (md-get-hash-byte hash 1) (length md-color-list)) md-color-list))
         (mark-choice (nth (mod (md-get-hash-byte hash 2) (length md-mark-list)) md-mark-list)))
    (list char-choice color-choice mark-choice)))

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

(defun md-hl-destroy-overlays ()
  (when md-hl-overlays
    (mapc #'delete-overlay md-hl-overlays)
    (setq md-hl-overlays nil))
  (when md-hl-timer
    (cancel-timer md-hl-timer)
    (setq md-hl-timer nil)))

(defun md-highlight-symbols ()
  (md-hl-destroy-overlays)
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
          (unless (or (md-filter-symbol sym sym-start (point) nil t)
                      (not (string-match "[a-z]\\|[A-Z]" sym)))
            (let* ((mods (md-string-mods sym))
                   (char-choice (nth 0 mods))
                   (color-choice (nth 1 mods))
                   (mark-choice (nth 2 mods))
                   (char (buffer-substring (+ sym-start char-choice) (+ sym-start char-choice 1)))
                   (o (make-overlay (+ sym-start char-choice) (+ sym-start char-choice 1))))
              ;; Displaying propertized text works better than using the overlay face
              ;; property. Org-mode had issues with it where it would make text with
              ;; diacritical marks huge
              (overlay-put o 'display
                           (md-hl-generate-text char mark-choice color-choice))
              (push o md-hl-overlays))))))))

(defun md-hl-closest-overlay (x y)
  (< (abs (- (overlay-start x) (point)))
     (abs (- (overlay-start y) (point)))))

(defun md-hl-generate-text (letter mark color)
    (propertize
     (concat letter (if mark (char-to-string mark) ""))
     'face (list :underline color)
     'font-lock-face (list :underline color)))

(defun md-hl-insert-symbol (letter mark color)
  (interactive)
  (let ((choice (md-hl-pick-symbol letter mark color)))
    (md-insert-text (buffer-substring (car choice) (cdr choice)) t nil)))

(defun md-hl-jump-symbol (letter mark color)
  (interactive)
  (let ((choice (md-hl-pick-symbol letter mark color)))
    (set-window-point nil (car choice))))

(defun md-hl-pick-symbol (letter mark color)
  "returns position of closest symbol matching targeting information"
  (interactive)
  (let* ((choice (md-hl-generate-text (downcase letter) mark color))
         (candidates (-filter
                      (lambda (x)
                        (let ((s (overlay-get x 'display)))
                          (and (string= color (plist-get (get-text-property 0 'face s) :underline))
                               (string= choice (downcase s)))))
                      md-hl-overlays)))
    (when candidates
      (setq candidates (sort candidates #'md-hl-closest-overlay))
      (let* ((choice (car candidates)))
        (save-excursion
          (goto-char (overlay-start choice))
          (bounds-of-thing-at-point 'symbol))))))

;; (md-hl-pick-symbol "r" #x31a "orange")   
;;(md-hl-insert-symbol "r" #x31a "orange") 

(defun md-hl-schedule-update ()
  (unless md-hl-timer
    ;; The idle timer has to be marked as repeating even though we cancel
    ;; it every time it actually executes. This is because of a race
    ;; condition not handled in the emacs API. If an error occurs triggering
    ;; the debugger then timers won't run. If the timer isn't marked repeating
    ;; it then never runs. If it never runs then it never clears the md-sn-timer
    ;; variable. If it never clears that variable then the check at the top
    ;; of this function to avoid double timers never passes, and we never
    ;; get to set the timer again.
    (setq md-hl-timer (run-with-idle-timer 0.25 t #'md-highlight-symbols))))

(defun md-hl-scroll (w new-start)
  (when (eq w (selected-window))
    (md-hl-schedule-update)))

(defun md-hl-change (beg end len)
  (md-hl-schedule-update))

(defun md-hl-setup ()
  (add-hook 'window-scroll-functions #'md-hl-scroll)
  (add-hook 'md-window-selection-hook #'md-hl-schedule-update)
  (add-hook 'after-change-functions #'md-hl-change)
  (setq md-symbol-picker-mode t))

(defun md-hl-teardown ()
  (remove-hook 'window-scroll-functions #'md-hl-scroll)
  (remove-hook 'md-window-selection-hook #'md-hl-schedule-update)
  (remove-hook 'after-change-functions #'md-hl-change)
  (md-hl-destroy-overlays)
  (setq md-symbol-picker-mode nil))

(defun md-toggle-symbol-picker-mode (&optional arg)
  (interactive)
  (cond
   ((and (not arg) md-symbol-picker-mode) (md-hl-teardown))
   ((not md-symbol-picker-mode) (md-hl-setup))))

(md-toggle-symbol-picker-mode t)
;; (md-toggle-symbol-picker-mode nil)
