(setq tramp-default-method "ssh")

;; By default tramp uses its own control master settings.
;; We really want control master sessions shared between
;; emacs and shells. Otherwise my dirtrack-mode hacks to
;; get remote tracking break, the GUI will hang when the
;; first remote prompt is displayed.
(setq tramp-use-ssh-controlmaster-options nil)


