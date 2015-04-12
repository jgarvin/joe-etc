(defun maybe-load-template ()
  (interactive)
  (when (eq 1 (point-max)) ;; new empty file
    (cond
     ((string-match "\\.py$" (buffer-file-name)) (insert-file "~/etc/emacs/template.py"))
     ((string-match "\\.sh$" (buffer-file-name)) (insert-file "~/etc/emacs/template.sh")))
    (goto-char (point-max))))

(add-hook 'find-file-hooks 'maybe-load-template)
