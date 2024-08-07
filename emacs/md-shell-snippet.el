(md-make-snippets
 '(derived-mode-p 'sh-mode 'shell-mode)
 '(
   ("stood out" "$1 > $2")
   ("stood err" "$1 2> $2")
   ("stood both" "$1 &> $2")
   ("comment" "# $1")
   ("export" "export $1=$2")
   ("CD" "cd $1")
   ("case" "case $1 in\n    $2)\n        $3\n        ;;\n    *)\n        echo >&2 $4\n        exit 1\n        ;;\nesac")
   ("todo" "# TODO: $1")
   ))

(md-make-snippets
 '(derived-mode-p 'shell-mode)
 '(
   ("if" "if [ $1 ]; then $2; fi")
   ("for" "for i in $1; do $2; done")
   ("while" "while $1; do $2 done")
   ))

(md-make-snippets
 '(derived-mode-p 'sh-mode)
 '(
   ("if" "if [ $1 ]; then\n    $2\nfi")
   ("for" "for i in $1; do\n    $2\ndone")
   ("while" "while $1; do\n    $2\ndone")
   ))
