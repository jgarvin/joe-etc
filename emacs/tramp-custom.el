(require 'tramp)

(setq tramp-default-method "ssh")

;; By default tramp uses its own control master settings.
;; We really want control master sessions shared between
;; emacs and shells. Otherwise my dirtrack-mode hacks to
;; get remote tracking break, the GUI will hang when the
;; first remote prompt is displayed.
(setq tramp-use-ssh-controlmaster-options nil)

;; use remote environment variables when in remote folders
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

;; example: [user@host ~]$
;; tramp-shell-prompt-pattern
;;(setq fedora-shell-pattern "\\(\\[.+?@.+? .+?\\] *\\)*")



;; tramp-shell-prompt-pattern

;; (setq tramp-verbose 10)
;; (setq tramp-verbose 3)

;; shell-prompt-pattern

;; (string-match-p "\\(?:^\\|\\)[^]#$%>\n]*#?[]#$%>] *\\(\\[[0-9;]*[a-zA-Z] *\\)*" "[jgarvin@tcs-login-1 ~]$ ")


;;(string-match-p "\\(\\[.+?@.+? .+?\\]\\)" "[jg@a b] ")
;;(string-match-p "\[\]" "[jgarvin@tcs-login-1 ~]$ ")
;; (string-match-p "\\(TERM = (.*)\\|Terminal type\\? \\[.*\\]\\)\\s-*" "[jgarvin@tcs-login-1 ~]$ ")
;; (string-match-p "\\(TERM = (.*)\\|Terminal type\\? \\[.*\\]\\)\\s-*" "jgarvin@ghost12:/$ ")

;; (string-match-p "\\[.*?@.*?:<.*?>\\]" "jgarvin@ghost12:/$")
