(md-make-snippets
 '(derived-mode-p 'eshell-mode)
 '(
   ("for" "for $1 in $2 { $3 }")
   ("lisp" "$($1)")
   ("stood out" "$1 > $2")
   ("stood err" "$1 2> $2")
   ("stood both" "$1 &> $2")
   ("line count" "wc -l")
   ("host" "cd /$USER@g$1:/")
   ("CD" "cd $1")
   ))