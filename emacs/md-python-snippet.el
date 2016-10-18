(defun python-context ()
  (derived-mode-p 'python-mode 'inferior-python-mode))

(md-replace-snippet
 :name "flat for"
 :contents "$1 for $2 in $3"
 :context '(python-context))

(md-replace-snippet
 :name "flat if"
 :contents "$1 if $2"
 :context '(python-context))

(md-replace-snippet
 :name "flat else"
 :contents "else $2"
 :context '(python-context))

(md-replace-snippet
 :name "flat if else"
 :contents "$1 if $2 else $3"
 :context '(python-context))

(md-replace-snippet
 :name "if"
 :contents "if $1:\n    $2"
 :context '(python-context))

(md-replace-snippet
 :name "if else"
 :contents "if $1:\n    $2\nelse:\n    $3"
 :context '(python-context))

(md-replace-snippet
 :name "from"
 :contents "from $1 import $2"
 :context '(python-context))

(md-replace-snippet
 :name "import"
 :contents "import $1"
 :context '(python-context))

(md-replace-snippet
 :name "for"
 :contents "for $1 in $2:\n    $3"
 :context '(python-context))

(md-replace-snippet
 :name "log info"
 :contents "log.info(\"$1\")"
 :context '(python-context))

(md-replace-snippet
 :name "log debug"
 :contents "log.debug(\"$1\")"
 :context '(python-context))

(md-replace-snippet
 :name "log warning"
 :contents "log.warning(\"$1\")"
 :context '(python-context))

(md-replace-snippet
 :name "log error"
 :contents "log.error(\"$1\")"
 :context '(python-context))

(md-replace-snippet
 :name "self"
 :contents "self.$1"
 :context '(python-context))

(md-replace-snippet
 :name "list comp"
 :contents "[$1 for $2 in $3]"
 :context '(python-context))

(md-replace-snippet
 :name "string item"
 :contents "\"$1\","
 :context '(python-context))

(md-make-snippets
 '(python-context)
 '(("default" "$1=$2")
   ("item" "$1 : $2")
   ("set comp" "{$1 for $2 in $3}")
   ("len" "len($1)")
   ("def" "def $1($2):\n    $3")
   ("method" "def $1(self, $2):\n    $3")
   ("while" "while $1:\n    $2")
   ("and" "$1 and $2")
   ("or" "$1 or $2")
   ("else" "else:\n    $1")
   ("print" "print $1")
   ("init" "def __init__(self):\n    $3")
   ("frac" "$1 % $2")
   ("return" "return $1")
   ("dir" "dir($1)")
   ("equals" "$1 == $2")
   ("not equals" "$1 != $2")
   ("assert" "assert($1)")
   ("todo" "# TODO: $1")
   ("main" "if __name__ == \"__main__\":\n    $1")
   ("class" "class $1($2):\n    $3")
   ("comment" "# $1")
   ("to do" "# TODO: $1")
   ("format" "$1.format($2)")
   ("stir" "str($1)")
   ("type" "type($1)")
   ("enumerate" "enumerate($1)")
   ("try else" "try:\n    $1\nelse:\n    $2")
   ("try except" "try:\n    $1\nexcept $2:\n    $3")
   ("try finally" "try:\n    $1\nfinally:\n    $2")
   ("with" "with $1:\n    $2")
   ("min" "min($1, $2)")
   ("max" "max($1, $2)")
   ("open" "open($1, $2)")
   ("with as" "with $1 as $2:\n    $3")
   ("as" "$1 as $2")
   ("not" "not $1")
   ("raise" "raise $1")
   ("is instance" "isinstance($1, $2)")
   ("sort" "sort($1)")
   ("lambda" "lambda $1: $2")
   ("arrow" "$1 -> $2")
   ("annotate" "$1: $2")
   ("help" "help($1)")
   ))