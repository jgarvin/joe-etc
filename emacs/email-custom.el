(use-package
  bbdb
  :ensure t
  :pin melpa)

(use-package
  helm-bbdb
  :ensure t
  :pin melpa)

;; see the documentation for the package to generate the file for importing
(use-package
  gmail2bbdb
  :ensure t
  :pin melpa-stable)
(autoload 'gmail2bbdb-import-file "gmail2bbdb" nil t nil)

(setq gnus-select-method '(nnimap "gmail"
                                  (nnimap-address "imap.gmail.com")
                                  (nnimap-server-port 993)
                                  (nnimap-stream ssl)))

(global-set-key (kbd "C-c e") #'gnus)

(setq gnus-use-full-window nil)

(setq send-mail-function 'smtpmail-send-it)
(setq smftpmail-smtp-service 587)
(setq smtpmail-smtp-server "smtp.gmail.com")

;; I never seem to exit emacs cleanly, just always use the autosave file
(setq gnus-always-read-dribble-file t)

(require 'bbdb)
(bbdb-initialize)
(setq
 bbdb-offer-save 1                        ;; 1 means save-without-asking

 bbdb-use-pop-up t                        ;; allow popups for addresses
 bbdb-electric-p t                        ;; be disposable with SPC
 bbdb-popup-target-lines  1               ;; very small

 bbdb-dwim-net-address-allow-redundancy t ;; always use full name
 bbdb-quiet-about-name-mismatches 2       ;; show name-mismatches 2 secs

 bbdb-always-add-address t                ;; add new addresses to existing...
 ;; ...contacts automatically
 bbdb-canonicalize-redundant-nets-p t     ;; x@foo.bar.cx => x@bar.cx

 bbdb-completion-type nil                 ;; complete on anything

 bbdb-complete-name-allow-cycling t       ;; cycle through matches
 ;; this only works partially

 bbbd-message-caching-enabled t           ;; be fast
 bbdb-use-alternate-names t               ;; use AKA


 bbdb-elided-display t                    ;; single-line addresses

 ;; auto-create addresses from mail
 bbdb/mail-auto-create-p 'bbdb-ignore-some-messages-hook
 bbdb-ignore-some-messages-alist ;; don't ask about fake addresses
 ;; NOTE: there can be only one entry per header (such as To, From)
 ;; http://flex.ee.uec.ac.jp/texi/bbdb/bbdb_11.html

 '(( "From" . "no.?reply\\|DAEMON\\|daemon\\|facebookmail\\|twitter"))
)

(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)

;; (defun etc-summary-mode-setup ()
;;   (gnus-summary-sort-by-date)
;;   (goto-char (point-max))
;;   (previous-line))

;; (defun etc-group-mode-setup ()
;;   ;; list all folders regardless of whether they have read messages
;;   (gnus-group-list-all-groups 5))

;; ;(add-hook 'gnus-summary-prepared-hook 'etc-summary-mode-setup)
;; (add-hook 'gnus-group-prepared-hook 'etc-group-mode-setup)

;; ;;@see http://www.emacswiki.org/emacs/GnusGmail#toc1
;; ;; (setq gnus-select-method '(nntp "news.gmane.org")) ;; if you read news groups
;;  ;(setq gnus-select-method '()) ;; if you read news groups

;; ;; ask encyption password once
;; ;;(setq epa-file-cache-passphrase-for-symmetric-encryption t)

;; (require 'smtpmail)
;; (setq smtpmail-auth-credentials "~/.authinfo")

;; (setq gnus-ignored-newsgroups "")

;; (setq message-send-mail-function 'smtpmail-send-it
;;       smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
;;       ;; smtpmail-auth-credentials '(("smtp.gmail.com" 587
;;       ;;                              user-mail-address nil))
;;       smtpmail-default-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-service 587
;;       ;; gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]"
;;       )

;; (setq send-mail-function 'smtpmail-send-it)

;; ;; ;;@see http://gnus.org/manual/gnus_397.html
;; ;; (add-to-list 'gnus-secondary-select-methods
;; ;;              '(nnimap "gmail"
;; ;;                       (nnimap-address "imap.gmail.com")
;; ;;                       (nnimap-server-port 993)
;; ;;                       (nnimap-stream ssl)
;; ;;                       (nnir-search-engine imap)
;; ;;                       (nnimap-authinfo-file "~/.authinfo")
;; ;;                       ; @see http://www.gnu.org/software/emacs/manual/html_node/gnus/Expiring-Mail.html
;; ;;                       ;; press 'E' to expire email
;; ;;                       (nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")
;; ;;                       (nnmail-expiry-wait 90)))

;; (setq gnus-select-method '(nnimap "gmail"
;;                                   (nnimap-address "imap.gmail.com")
;;                                   (nnimap-server-port 993)
;;                                   (nnimap-stream ssl)
;;                                   (nnir-search-engine imap)
;;                                   (nnimap-authinfo-file "~/.authinfo")
;;                                         ; @see http://www.gnu.org/software/emacs/manual/html_node/gnus/Expiring-Mail.html
;;                                   ;; press 'E' to expire email
;;                                   (nnmail-expiry-target "nnimap+gmail:[Gmail]/Trash")
;;                                   (nnmail-expiry-wait 90)))

;; (setq gnus-thread-sort-functions
;;       '((not gnus-thread-sort-by-date)
;;         (not gnus-thread-sort-by-number)))

;; ; NO 'passive
;; (setq gnus-use-cache t)

;; ;; Fetch only part of the article if we can.  I saw this in someone
;; ;; else's .gnus
;; (setq gnus-read-active-file 'some)

;; ;; Tree view for groups.  I like the organisational feel this has.
;; (add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; ;; Threads!  I hate reading un-threaded email -- especially mailing
;; ;; lists.  This helps a ton!
;; (setq gnus-summary-thread-gathering-function 'gnus-gather-threads-by-subject)

;; ;; Also, I prefer to see only the top level message.  If a message has
;; ;; several replies or is part of a thread, only show the first
;; ;; message.  'gnus-thread-ignore-subject' will ignore the subject and
;; ;; look at 'In-Reply-To:' and 'References:' headers.
;; (setq gnus-thread-hide-subtree t)
;; (setq gnus-thread-ignore-subject t)

;; ;; You need install the command line brower 'w3m' and Emacs plugin 'w3m'
;; (setq mm-text-html-renderer 'shr)

;; ;; http://www.gnu.org/software/emacs/manual/html_node/gnus/_005b9_002e2_005d.html
;; (setq gnus-use-correct-string-widths nil)

;; ;; without this gnus always asks how much of my huge inbox to show
;; (setq gnus-large-newsgroup 100)
;;  (setq gnus-parameters
;;        '((".*"
;;           (display . 100))
;;          ))

;; ;; workaround for "Inital sync" message happening every time
;; (setq gnus-read-active-file nil)

;; ;; BBDB: Address list
;; ;; (add-to-list 'load-path "/where/you/place/bbdb/")
;; ;; (require 'bbdb)
;; ;; (bbdb-initialize 'message 'gnus 'sendmail)
;; ;; (setq bbdb-file "~/.bbdb") ;; OPTIONAL, because I'm sharing my ~/.emacs.d
;; ;; (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
;; ;; (setq bbdb/mail-auto-create-p t
;; ;;       bbdb/news-auto-create-p t)

;; ;; ;; auto-complete emacs address using bbdb's own UI
;; ;; (add-hook 'message-mode-hook
;; ;;           '(lambda ()
;; ;;              (flyspell-mode t)
;; ;;              (local-set-key "<TAB>" 'bbdb-complete-name)))
