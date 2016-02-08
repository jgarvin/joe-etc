(md-make-snippets
 '(derived-mode-p 'sh-mode 'shell-mode 'eshell-mode)
 '(
   ("said" "sed 's/$1/$2/g' $3")
   ("tee" "tee $1")
   ("grep" "grep \"$1\"")
   ("line count" "wc -l")
   ("move" "mv $1 $2")
   ("copy" "cp $1 $2")
   ("see D", "cd $1")
   ("GDB" "gdb --args $1")
   ("diff" "diff -u $1 $2")
   ("man" "man $1")
   ))
