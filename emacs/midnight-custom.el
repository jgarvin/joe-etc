(require 'midnight)

(midnight-delay-set 'midnight-delay "4:30am")

;; This prevents trying to kill the startup emacsclient
(add-to-list 'clean-buffer-list-kill-never-buffer-names (user-login-name))

(dolist (i '("*Packages*"
             "*Find*"
             "*magit-diff*"
             "*magit-log*"
             "*Backtrace*"
             "*grep*"
             "*Completions*"))
  (add-to-list 'clean-buffer-list-kill-never-buffer-names i))

(add-to-list 'clean-buffer-list-kill-regexps "compile|.*?|.*")
(add-to-list 'clean-buffer-list-kill-regexps "run|.*?|.*")


