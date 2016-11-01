(defun etc-diff-mode ()
  (diff-auto-refine-mode 1))

(add-hook 'diff-mode #'etc-diff-mode)
