(require 'midnight)

(midnight-delay-set 'midnight-delay "4:30am")

;; This prevents trying to kill the startup emacsclient
(add-to-list 'clean-buffer-list-kill-never-buffer-names (user-login-name))

(dolist (i '("*Packages*"
             "*Find*"
             "*magit-diff*"
             "*magit-log*"
             "*Backtrace*"
             "*grep*"))
  (add-to-list 'clean-buffer-list-kill-never-buffer-names i))

(add-to-list 'clean-buffer-list-kill-regexps "compile|.*?|.*")
(add-to-list 'clean-buffer-list-kill-regexps "run|.*?|.*")

;; found emacs crashing thousands of frames deep inside
;; collection, so try collecting eagerly to avoid in the future
(add-hook 'midnight-hook #'garbage-collect)