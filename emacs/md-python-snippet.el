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
   ))