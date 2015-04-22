(require 'midnight)

(midnight-delay-set 'midnight-delay "4:30am")

;; This prevents trying to kill the startup emacsclient
(add-to-list 'clean-buffer-list-kill-never-buffer-names (user-login-name))

