;; store passwords in file outside git ;)
(load "~/.ercpass")
(require 'erc-services)
(erc-services-mode 1)
(setq erc-prompt-for-nickserv-password nil)
(setq erc-nickserv-passwords
      `((freenode     ((,freenode-nick . ,freenode-nick-pass)))))

;; without this will autojoin channels before identifying
(setq erc-autojoin-timing 'ident)

;; auto-resize the text wrapping based on frame size
(make-variable-buffer-local 'erc-fill-column)
(add-hook 'window-configuration-change-hook 
          '(lambda ()
             (save-excursion
               (walk-windows
                (lambda (w)
                  (let ((buffer (window-buffer w)))
                    (set-buffer buffer)
                    (when (eq major-mode 'erc-mode)
                      (setq erc-fill-column (- (window-width w) 2)))))))))

;; when wrapping use a little indent to make things clear
(setq erc-fill-prefix "    ")

;; make it easy to use mandimus to jump to chat window
(make-variable-buffer-local 'frame-title-format)
(add-hook 'erc-mode-hook
          (lambda ()
            (setq frame-title-format "chat: %b")))

;; replace ERC> with #emacs>
(setq erc-prompt (lambda () (concat "[" (buffer-name) "]")))

(defadvice erc-display-prompt (after conversation-erc-display-prompt activate)
  "Insert last recipient after prompt."
  (let ((previous 
         (save-excursion 
           (if (and (search-backward-regexp (concat "^[^<]*<" erc-nick ">") nil t)
                    (search-forward-regexp (concat "^[^<]*<" erc-nick ">" 
                                                   " *\\([^:]*: ?\\)") nil t))
               (match-string 1)))))
    ;; when we got something, and it was in the last 3 mins, put it in
    (when (and 
           previous 
           (> 180 (time-to-seconds 
                   (time-since (get-text-property 0 'timestamp previous)))))
      (set-text-properties 0 (length previous) nil previous)
      (insert previous))))

;; recognize urls like: Try foo.org!
(setq erc-button-url-regexp
      "\\([-a-zA-Z0-9_=!?#$@~`%&*+\\/:;,]+\\.\\)+[-a-zA-Z0-9_=!?#$@~`%&*+\\/:;,]*[-a-zA-Z0-9\\/]")

(erc-autojoin-mode 1)
(setq erc-autojoin-channels-alist
      '(("freenode.net" "#emacs" "#python" "##c++" "##linux")))

(defun irc-maybe ()
  "Connect to IRC."
  (interactive)
  (when (y-or-n-p "IRC? ")
    (erc :server "irc.freenode.net" :port 6667 :nick freenode-nick)))

;; automagic ghosting
(defun erc-ghost-maybe (server nick)
  "Send GHOST message to NickServ if NICK ends with `erc-nick-uniquifier'.
The function is suitable for `erc-after-connect'."
  (when (string-match (format "\\(.*?\\)%s+$" erc-nick-uniquifier) nick)
    (let ((nick-orig (match-string 1 nick))
          (password erc-session-password))
      (when (y-or-n-p (format "Current nick is '%s'. Do you want to ghost?"
                              nick))
        (erc-message "PRIVMSG" (format "NickServ GHOST %s %s"
                                       nick-orig password))
        (erc-cmd-NICK nick-orig)
        (erc-message "PRIVMSG" (format "NickServ identify %s %s"
                                       nick-orig password))))))
(add-hook 'erc-after-connect 'erc-ghost-maybe)

;; ;; keep prompt and text at the bottom
;; ;;(erc-scrolltobottom-mode 1)
;; ;; (add-to-list 'erc-mode-hook (lambda ()
;; ;;                               (set (make-local-variable 'scroll-conservatively) 100)))

;; ;; make nicknames nice colors
(require 'erc-hl-nicks)

;; no need to see these from people not talking
(setq erc-lurker-hide-list '("JOIN" "PART" "QUIT"))

                                        ;(erc-update-modules)
