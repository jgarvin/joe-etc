(require 'erc-services)
(erc-services-mode 1)
(setq erc-prompt-for-nickserv-password nil)
;; (setq erc-nickserv-passwords
;;       `((libera     ((,freenode-nick . ,freenode-nick-pass)))
;;         ;;(freenode     ((,freenode-nick . ,freenode-nick-pass)))
;;         (oftc  ((,freenode-nick . ,freenode-nick-pass)))
;;         (mozilla ((,freenode-nick . ,freenode-nick-pass)))))

(add-to-list 'erc-nickserv-alist (list 'mozilla "NickServ!services@ircservices.mozilla.org" "This\\s-nickname\\s-is\\s-registered\\s-and\\s-protected" "NickServ" "IDENTIFY" nil nil "Password\\s-accepted"))

;; try to prevent disconnects?
(setq erc-server-send-ping-timeout 240)
(setq erc-server-send-ping-interval 15)

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
                    (when (derived-mode-p 'erc-mode)
                      (setq erc-fill-column (- (window-width w) 2)))))))))

;; when wrapping use a little indent to make things clear
(setq erc-fill-prefix "    ")

(defun etc-erc-mode-hook ()
  (set (make-local-variable 'scroll-step) 1)
  (set (make-local-variable 'scroll-conservatively) 1)
  (set (make-local-variable 'scroll-margin) 0)
  (visual-line-mode 0) ;; erc does its own wrapping, gets confused
  )

(add-hook 'erc-mode-hook #'etc-erc-mode-hook)

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
      '(
        ("libera.chat" "#emacs" "#python" "##traders" "##c++" "##linux"
         "#perl6" "#racket" "##rust" "#redo")
        ;;("freenode.net" "#emacs" "#python" "##traders" "##c++" "##linux"
        ;; "#perl6" "#racket" "##rust")
        ("oftc.net" "#perf")))

;; don't automatically switch to joined channels, that's just annoying
(setq erc-join-buffer 'bury)

(defun irc-maybe ()
  "Connect to IRC."
  (interactive)
  (when t ;; (y-or-n-p "IRC? ")
    (erc-tls :server "irc.libera.chat" :port 6697 :nick freenode-nick)
    ;;(erc-tls :server "irc.freenode.net" :port 6697 :nick freenode-nick)
    (erc-tls :server "irc.oftc.net" :port 6697 :nick freenode-nick)))

(defun filter-server-buffers ()
  (delq nil
        (mapcar
         (lambda (x) (and (erc-server-buffer-p x) x))
         (buffer-list))))

(defun stop-irc ()
  "Disconnects from all irc servers"
  (interactive)
  (dolist (buffer (filter-server-buffers))
    (message "Server buffer: %s" (buffer-name buffer))
    (when (get-buffer-process buffer)
      (with-current-buffer buffer
        (erc-quit-server "Asta la vista"))))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'erc-mode)
        (kill-buffer buffer)))))

;; automagic ghosting, assume my nick, and assume I do want to ghost
(defun erc-ghost-maybe (server nick)
  "Send GHOST message to NickServ if NICK ends with `erc-nick-uniquifier'.
The function is suitable for `erc-after-connect'."
  (when (string-match (format "\\(.*?\\)%s+$" erc-nick-uniquifier) nick)
    (let ((nick-orig freenode-nick)
          (password erc-session-password)
          (channel-list (mapcar 'buffer-name (erc-channel-list nil))))
      (when ;; (y-or-n-p (format "Current nick is '%s'. Do you want to ghost?"
            ;;                   nick)
          t
        ;; nowadays a lot of channels won't let you change your nick while in
        ;; a channel, so we leave all channels and rejoin
        (erc-with-all-buffers-of-server server 'erc-channel-p
                                        (erc-part-from-channel "ghosting nick"))
        (erc-message "PRIVMSG" (format "NickServ GHOST %s %s"
                                       nick-orig password))
        (erc-cmd-NICK nick-orig)
        (erc-message "PRIVMSG" (format "NickServ identify %s %s"
                                       nick-orig password))
        (dolist (name channel-list)
          (erc-join-channel name))))))
(add-hook 'erc-after-connect 'erc-ghost-maybe)

;; ;; make nicknames nice colors
(require 'erc-hl-nicks)

;; no need to see these from people not talking
(setq erc-lurker-hide-list '("JOIN" "PART" "QUIT"))

;; Interpret mIRC-style color commands in IRC chats
(setq erc-interpret-mirc-color t)

;; Kill buffers for channels after /part
(setq erc-kill-buffer-on-part t)
;; Kill buffers for private queries after quitting the server
(setq erc-kill-queries-on-quit t)
;; Kill buffers for server messages after quitting the server
(setq erc-kill-server-buffer-on-quit t)


;; (push erc-scroll-to-bottom erc-modules)
;; (setq erc-modules (remq 'erc-scroll-to-bottom erc-modules))
;; (erc-update-modules)


;; ------------------ custom scroll to bottom code

(defun erc-display-line-1 (string buffer)
  "Display STRING in `erc-mode' BUFFER.
Auxiliary function used in `erc-display-line'.  The line gets filtered to
interpret the control characters.  Then, `erc-insert-pre-hook' gets called.
If `erc-insert-this' is still t, STRING gets inserted into the buffer.
Afterwards, `erc-insert-modify' and `erc-insert-post-hook' get called.
If STRING is nil, the function does nothing."
  (when string
    (with-current-buffer (or buffer (process-buffer erc-server-process))
      (let ((insert-position (or (marker-position erc-insert-marker)
				 (point-max))))
	(let ((string string) ;; FIXME! Can this be removed?
	      (buffer-undo-list t)
	      (inhibit-read-only t))
	  (unless (string-match "\n$" string)
	    (setq string (concat string "\n"))
	    (when (erc-string-invisible-p string)
	      (erc-put-text-properties 0 (length string)
				       '(invisible intangible) string)))
	  (erc-log (concat "erc-display-line: " string
			   (format "(%S)" string) " in buffer "
			   (format "%s" buffer)))
	  (setq erc-insert-this t)
	  (run-hook-with-args 'erc-insert-pre-hook string)
	  (if (null erc-insert-this)
	      ;; Leave erc-insert-this set to t as much as possible.  Fran
	      ;; Litterio <franl> has seen erc-insert-this set to nil while
	      ;; erc-send-pre-hook is running, which should never happen.  This
	      ;; may cure it.
	      (setq erc-insert-this t)
	    (save-excursion ;; to restore point in the new buffer
	      (save-restriction
		(widen)
		(goto-char insert-position)
		(insert-before-markers string)
		;; run insertion hook, with point at restored location
		(save-restriction
		  (narrow-to-region insert-position (point))
		  (run-hooks 'erc-insert-modify-hook)
		  (run-hooks 'erc-insert-post-hook)
		  (when erc-remove-parsed-property
		    (remove-text-properties (point-min) (point-max)
					    '(erc-parsed nil))))))))
	(erc-update-undo-list (- (or (marker-position erc-insert-marker)
				     (point-max))
				 insert-position)))
      (run-hooks 'erc-display-post-hook)))) ;;; this line and only this line was added

(defvar erc-display-post-hook nil
  "New hook!")

(defun damd-erc-display-post-hook ()
  (let ((windows (get-buffer-window-list (current-buffer) nil 'visible)))
    (dolist (w windows)
      (when (>= (point) erc-input-marker)
        (with-selected-window w
          (recenter -1))))))
(add-hook 'erc-display-post-hook 'damd-erc-display-post-hook)

(defun damd-erc-send-post-hook ()
  (when (>= (point) erc-input-marker)
    (goto-char (point-max))
    (widen)
    (recenter -1)))
(add-hook 'erc-send-post-hook 'damd-erc-send-post-hook)

(defun damd-window-configuration-change-hook ()
  (when (and (derived-mode-p 'erc-mode)
             (and erc-input-marker (>= (point) erc-input-marker)))
    (recenter -1)))
(add-hook 'window-configuration-change-hook 'damd-window-configuration-change-hook)

;;(setq auto-window-vscroll nil)
