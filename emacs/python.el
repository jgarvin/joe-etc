(defun align-dict ()
  "Align elements in the python dictionary cursor is inside of around colon."
  (interactive)
  (save-excursion 
    (single-pair-only-sexp "{" 'sp-select-next-thing)
    (align-regexp (region-beginning) (region-end) "\\(\\s-*\\):")))
