(defun md-select-minibuffer ()
  (select-window (minibuffer-window)))

;; we don't actually do text insertion here, we let xdotool typing
;; take care of that because (insert) doesn't play well with the
;; minibuffer no matter what arsenal of frame/window selecting
;; madness I try
(defun md-need-space ()
  (and
   ;; not at start of buffer
   (char-before)
   ;; not following white space or new line
   (not (save-excursion (re-search-backward "[[:blank:]\n]" (- (point) 1) t)))))

