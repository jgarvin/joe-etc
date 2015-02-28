(defvar etc-already-shrinking nil)

(defun etc-shrink-help ()
  (unless etc-already-shrinking
    (let ((etc-already-shrinking t))
      (dolist (name-regex (list "\\*Help <[^>]+>\\*" "\\*Ido Completions\\*"))
        (dolist (w (window-list))
          (when (string-match-p name-regex (buffer-name (window-buffer w)))
            (fit-window-to-buffer w (/ (frame-height) 2))))))))

(add-hook 'window-configuration-change-hook #'etc-shrink-help)

(defun etc-help-mode-hook ()
  (when (eq (window-frame (get-buffer-window nil 1)) (selected-frame))
    (let* ((name (format "*Help <%s>*" (frame-parameter (selected-frame) 'outer-window-id)))
           (buf (get-buffer name)))
      (when buf
        (kill-buffer buf))
      (rename-buffer name))))

(add-hook 'help-mode-hook #'etc-help-mode-hook)
