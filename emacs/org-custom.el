(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(setq org-agenda-files (list "~/work/mandimus/tasks.org"
                             "~/etc/etc.org"))

(setq org-startup-folded nil)
(setq org-agenda-inhibit-startup nil)
(setq org-catch-invisible-edits 'smart)
