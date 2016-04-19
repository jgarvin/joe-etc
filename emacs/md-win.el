;; (defvar-local md-eshell-disable-command-intercept nil)

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
        (ignore-errors
          (when (equal (downcase (md-get-window-glyph w)) (downcase (char-to-string chosen-glyph)))
            (select-frame-set-input-focus f)
            (select-window w))))))

(global-set-key (kbd "M-d") #'md-select-window-with-glyph)
(require 'hexl)
(define-key hexl-mode-map (kbd "M-d") #'md-select-window-with-glyph) ;; not sure why this is necessary

;; (require 'eshell)
;; (defadvice eshell-self-insert-command (around md-eshell-bug-workaround activate)
;;   (unless md-eshell-disable-command-intercept
;;     (when (or (and (symbolp last-command-event)
;;                    (get last-command-event 'ascii-character))
;;               (characterp last-command-event))
;;       ad-do-it)))

;; (ad-deactivate #'eshell-self-insert-command)

;; (md-select-window-with-glyph ?p)
;; (md-select-window-with-glyph ?u)

;;(kill-buffer "*eshell*<7>")