(md-make-snippets
 '(derived-mode-p 'sh-mode 'shell-mode)
 '(
   ("if" "if [ $1 ]; then\n    $2\nfi")
   ("stood out" "$1 > $2")
   ("stood err" "$1 2> $2")
   ("stood both" "$1 &> $2")
   ))
