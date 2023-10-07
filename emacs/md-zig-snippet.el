(md-make-snippets
 '(derived-mode-p 'zig-mode)
 '(
   ("const" "const $1")
   ("var" "var $1")
   ("function" "fn $1($2) $3 {\n    $4\n}")
   ("import" "@import(\"$1\")")
   ("assign" "$1 = $2;")
   ("a sign" "$1 = $2;")
   ("annotate" "$1: $2")
   ("array" "[$1]$2")
))