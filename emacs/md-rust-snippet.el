(md-make-snippets
 '(derived-mode-p 'rust-mode)
 '(
   ("function" "fn $1($2) -> $3 {\n    $4\n}")
   ("main" "fn main() {\n    $1\n}")
   ("print" "println!($1);")
   ("and" "$1 && $2")
   ("assign" "$1 = $2;")
   ("comment" "// $1")
))
