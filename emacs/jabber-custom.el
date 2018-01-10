(use-package
  jabber
  :ensure t
  :pin melpa)

(require 'jabber)
(jabber-switch-to-roster-buffer)

(setq jabber-account-list (\`
                           (((\, gmail-user-name)
                             (:network-server . "talk.google.com")
                             (:connection-type . starttls)))))
