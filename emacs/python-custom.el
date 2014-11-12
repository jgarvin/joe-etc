;; sp-select-next-thing isn't the right function, if there's a nested
;; instance nearby we'll try aligning that instead which is wrong. We
;; want "select containing sexp"

(defun align-dict ()
  "Align elements in the python dictionary cursor is inside of around colon."
  (interactive)
  (undo-boundary)
  (save-excursion 
    (single-pair-only-sexp "{" 'sp-select-next-thing)
    (align-regexp (region-beginning) (region-end) "\\(\\s-*\\):")))

(defun align-list ()
  "Align elements in the python list."
  (interactive)
  (undo-boundary)
  (save-excursion 
    (single-pair-only-sexp "[" 'sp-select-next-thing)
    (align-regexp (region-beginning) (region-end) "\\(\\s-*\\),")))
