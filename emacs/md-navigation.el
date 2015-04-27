(defun md-beginning-or-indentation-toggle ()
  "Toggle between beginning of line and first non-whitespace-char."
  (interactive)
  (cond ((bolp) (back-to-indentation))
        ((save-excursion (skip-chars-backward "[:space:]") (bolp)) ; At indentation.
         (forward-line 0))
        (t (back-to-indentation))))