(defun md-sql-context ()
  (derived-mode-p 'sql-mode 'sql-interactive-mode))

(md-make-snippets
 '(md-sql-context)
 '(("select" "SELECT $1 FROM $2")
   ("order by" "ORDER BY $1")
   ("group by" "group BY $1")
   ("limit" "LIMIT $1")
   ("count" "COUNT($1)")
   ("insert" "INSERT INTO $1($2) VALUES $3")
   ("where" "WHERE $2")))
