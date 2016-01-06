(defun md-get-window-glyph (w)
  (let* ((fp (frame-parameters (window-frame w)))
         (frame-top (cdr (assoc 'top fp)))
         (frame-left (cdr (assoc 'left fp)))
         (window-top (window-top-line w))
         (window-left (window-left-column w))
         (hash (md5 (format "%S %S %S %S" frame-top frame-left window-top window-left)))
         (glyph (nth (% (string-to-number (substring hash 0 2) 16) (length md-glyphs)) md-glyphs)))
    (char-to-string glyph)))

(setq-default mode-line-format
              (append
               (list '(:eval (propertize (format "[%s] " (md-get-window-glyph (get-buffer-window))) 'face 'font-lock-constant-face)))
               mode-line-format))

(defun md-select-window-with-glyph (chosen-glyph)
  (interactive "cWindow glyph: ")
  (dolist (f (frame-list))
    (dolist (w (window-list f 0))
      (when (equal (downcase (md-get-window-glyph w)) (downcase (char-to-string chosen-glyph)))
        (select-frame-set-input-focus f)
        (select-window w)))))

(global-set-key (kbd "M-d") #'md-select-window-with-glyph)

;; (md-select-window-with-glyph ?p)
;; (md-select-window-with-glyph ?u)