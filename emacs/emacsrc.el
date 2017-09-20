(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                           ("marmalade" . "https://marmalade-repo.org/packages/")
                           ("melpa-stable" . "http://stable.melpa.org/packages/")
                           ("melpa" . "https://melpa.org/packages/")))
  (when (not (package-installed-p 'use-package))
    (package-refresh-contents)
    (package-install 'use-package)))

(require 'use-package)

(use-package
  dash
  :ensure t
  )

(use-package
  helm
  :ensure t
  :pin melpa-stable)

(use-package
  perl6-mode
  :ensure t)

(use-package
  racket-mode
  :ensure t)

(use-package
  haskell-mode
  :ensure t
  :pin melpa-stable)

(use-package
  magit
  :ensure t
  :pin melpa-stable)

(use-package
  magit-gerrit
  :ensure t
  :pin melpa
  )
(require 'magit-gerrit)

(use-package
  projectile
  :ensure t
  :pin melpa-stable)


(use-package
  helm-projectile
  :ensure t
  :pin melpa-stable)

(use-package
  erc-hl-nicks
  :ensure t
  :pin melpa-stable)

(use-package
  realgud
  :ensure t
  :pin melpa-stable)

(use-package
  goto-chg
  :ensure t
)

(use-package
  helm-swoop
  :ensure t
  :pin melpa-stable)

(use-package
  undo-tree
  :ensure t
)

(use-package
  expand-region
  :ensure t
  :pin melpa-stable)

(use-package
  string-inflection
  :ensure t)

(use-package
  f
  :ensure t
  :pin melpa-stable)

;;(use-package
;;  protobuf-mode
;;  :ensure t
;;  :pin melpa-stable)

(use-package
  helm-gtags
  :ensure t
  :pin melpa-stable)

(load-file "~/etc/emacs/smartparens-custom.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-view-program-selection
   (quote
    (((output-dvi style-pstricks)
      "Evince")
     (output-dvi "Evince")
     (output-pdf "Evince")
     (output-html "Evince"))))
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "f0b0710b7e1260ead8f7808b3ee13c3bb38d45564e369cbe15fc6d312f0cd7a0" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(ediff-split-window-function (quote split-window-horizontally))
 '(haskell-mode-hook (quote (turn-on-haskell-indent)))
 '(package-selected-packages
   (quote
    (magit-gerrit magit use-package undo-tree string-inflection smartparens realgud racket-mode perl6-mode haskell-mode goto-chg f expand-region erc-hl-nicks)))
 '(safe-local-variable-values
   (quote
    ((eval add-hook
	   (quote after-save-hook)
	   (lambda nil
	     (shell-command
	      (format "rsync -av %s %s/dragonshare/NatLink/NatLink/MacroSystem"
		      (buffer-file-name)
		      (getenv "HOME"))))
	   nil t)
     (eval add-hook
	   (quote after-save-hook)
	   (lambda nil
	     (shell-command
	      (format "touch %s/dragonshare/NatLink/NatLink/MacroSystem/_dfly_client.py"
		      (getenv "HOME"))))
	   nil t)
     (eval add-hook
	   (quote after-save-hook)
	   (lambda nil
	     (shell-command
	      (format "rsync -av %s %s/dragonshare/NatLink/NatLink/MacroSystem/_%s"
		      (buffer-file-name)
		      (getenv "HOME")
		      (buffer-name))))
	   nil t)
     (eval add-hook
	   (quote after-save-hook)
	   (lambda nil
	     (shell-command
	      (format "rsync -av %s %s/dragonshare/NatLink/NatLink/MacroSystem/_%s"
		      (buffer-file-name)
		      (getenv "HOME")
		      (buffer-name)))))))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(if (< emacs-major-version 24)
    (load-file "~/etc/emacs/cl-lib-0.3.el")
  (require 'cl))

(setq require-final-newline nil)
(setq mode-require-final-newline nil)

;; don't like losing things
(setq kill-ring-max 10000)

(require 'profiler)
(setq profiler-max-stack-depth 64)
(defun etc-profile-func (func &rest args)
  (call-interactively #'profiler-stop)
  (call-interactively #'profiler-start)
  (apply func args)
  (call-interactively #'profiler-report)
  (call-interactively #'profiler-stop))

(defmacro etc-profile (&rest body)
  `(progn
     (call-interactively #'profiler-stop)
     (call-interactively #'profiler-start)
     ,@body
     (call-interactively #'profiler-report)
     (call-interactively #'profiler-stop)))

;; Enable debugging
(setq-default debug-on-error t)
(setq message-log-max t)

;; When running a local install of emacs, still pull in officially
;; installed packages.
(when (and (file-directory-p "/usr/share/emacs/site-lisp")
           (not (memq "/usr/share/emacs/site-lisp" load-path)))
  (add-to-list 'load-path "/usr/share/emacs/site-lisp"))

(add-to-list 'load-path "~/etc/dash")
(require 'dash)
(eval-after-load "dash" '(dash-enable-font-lock))
;; accidentally suspending emacs is super annoying
(when (getenv "DISPLAY")
  (global-unset-key (kbd "C-z")))

;; store passwords in file outside git ;)
(if (file-exists-p "~/.emacspass")
    (load "~/.emacspass")
  (message "No ~/.emacspass file found!"))

(load-file "~/etc/emacs/smartparens-custom.el")
;;(load-file "~/etc/emacs/ido-custom.el")
(load-file "~/etc/emacs/yasnippet-custom.el")
(load-file "~/etc/emacs/save.el")
(load-file "~/etc/emacs/pair.el")

(load-file "~/etc/emacs/python-custom.el")
(load-file "~/etc/emacs/perl6-custom.el")
(load-file "~/etc/emacs/dired-custom.el")
(load-file "~/etc/emacs/erc-custom.el")
(load-file "~/etc/emacs/term-custom.el")
(load-file "~/etc/emacs/comint-custom.el")
(load-file "~/etc/emacs/shell-custom.el")

(load-file "~/etc/emacs/projectile-custom.el")
(load-file "~/etc/emacs/proced-custom.el")
(load-file "~/etc/emacs/email-custom.el")
(load-file "~/etc/emacs/w3m-custom.el")
(load-file "~/etc/emacs/c-common.el")
(load-file "~/etc/emacs/elisp-custom.el")
(load-file "~/etc/emacs/ack-custom.el")
(load-file "~/etc/emacs/org-custom.el")

(load-file "~/etc/emacs/persist-custom.el")
(load-file "~/etc/emacs/buffer-tail.el")
(load-file "~/etc/emacs/log-custom.el")
(load-file "~/etc/emacs/unicode-custom.el")
(load-file "~/etc/emacs/help-custom.el")
(load-file "~/etc/emacs/tramp-custom.el")

(load-file "~/etc/emacs/template-custom.el")
(load-file "~/etc/emacs/build-custom.el")
(load-file "~/etc/emacs/midnight-custom.el")
(load-file "~/etc/emacs/last-change-custom.el")
(load-file "~/etc/emacs/helm-custom.el")
(load-file "~/etc/emacs/linum-custom.el")
(load-file "~/etc/emacs/visual-line-custom.el")
(load-file "~/etc/emacs/visible-mark-custom.el")
(load-file "~/etc/emacs/eshell-custom.el")
(load-file "~/etc/emacs/gc-custom.el")
(load-file "~/etc/emacs/dabbrev-custom.el")
(load-file "~/etc/emacs/hide-show-custom.el")
(load-file "~/etc/emacs/magit-custom.el")
(load-file "~/etc/emacs/diff-custom.el")

(load-file "~/etc/emacs/gui.el")


(setq-default indent-tabs-mode nil)

(delete-selection-mode 1)

(defun etc-set-repeat-rate ()
  ;; Set the keyboard repeat rate to be a lot faster
  ;; technically this should be triggered by some sort of udev
  ;; event because this resets if you unplug and replug in the kbd.
  ;; instead we just run it once a minute, blame laziness :p
  (call-process "xset" nil nil nil "r" "rate" "200" "60"))
(run-with-timer 0 60 #'etc-set-repeat-rate)

(defun etc-translate-hack ()
  ;; doing this on a timer seems to be the only way to get
  ;; it to work on startup...  after-init-hook doesn't work,
  ;; neither does just putting top level in emacsrc
  (keyboard-translate ?\C-t ?\C-x)
  (keyboard-translate ?\C-x ?\C-t))
(run-with-timer 0 2 #'etc-translate-hack)

(when (getenv "DISPLAY")
  ;; Make emacs use the normal clipboard
  (setq x-select-enable-clipboard t)
  (setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
  (setq x-selection-timeout 300)
  ;; When remotely logging in, need to remap alt for emacs keybindings to work
  (when (not (string= (nth 0 (split-string (nth 1 (split-string (getenv "DISPLAY") ":")) "\\.")) "0"))
    (setq x-alt-keysym 'meta)))

(setq make-backup-files nil)

(add-to-list 'load-path "~/etc/drag-stuff")
(add-to-list 'load-path "~/etc/emacs")

(require 'winpoint)
(window-point-remember-mode 1)

(require 'drag-stuff)
(drag-stuff-global-mode 1)


(require 'undo-tree)
(global-undo-tree-mode)
;;(define-key undo-tree-map (kbd "C-_"))
(global-unset-key (kbd "C-_"))
(define-key undo-tree-map (kbd "C-_") nil)
(define-key undo-tree-map (kbd "C-_") nil)
(define-key undo-tree-map (kbd "C-/") #'undo-tree-undo)
(define-key undo-tree-map (kbd "M-/") #'undo-tree-redo)
(setq-default undo-limit 1000000)
(setq-default undo-strong-limit 1000000)

;; Show me the region until I do something on it
(setq transient-mark-mode t)

;; Make killing the line also delete it
(setq kill-whole-line t)

;; when on a TAB, the cursor has the TAB length
(setq-default x-stretch-cursor t)

;; quiet, please! No dinging!
(setq visible-bell t)
(setq ring-bell-function 'ding)

;; should get used to using delete key on kineses
(global-unset-key "\C-d")
(global-set-key [delete] 'delete-char)

(defun end-or-trailing (&optional n)
  "Move cursor to end of this line or to its indentation.
  If in middle of of this line, move to last nonwhitespace character on line.
  If at last nonwhitespace character of this line, move to end of line.
  With arg N, move backward to the end of the Nth next line.
  Interactively, N is the prefix arg."
  (interactive "P")
  (cond
   ((or (eolp) n)
    (line-move (prefix-numeric-value (or n 1)))
    (end-of-visual-line)
    (skip-chars-backward "[:space:]"))
   ((save-excursion
      (skip-chars-forward "[:space:]")
      (eolp)) ; At start of trailing whitespace
    (end-of-visual-line))
   (t
    (end-of-visual-line)
    (skip-chars-backward "[:space:]"))))

(defun back-to-visual-indentation ()
  "Move point to the first non-whitespace character on this line."
  (interactive "^")
  (beginning-of-visual-line 1)
  (skip-syntax-forward " "
                       (save-excursion (end-of-visual-line) (point))
                       ;;(line-end-position)
                       )
  ;; Move back over chars that have whitespace syntax but have the p flag.
  (backward-prefix-chars))

(defun beginning-or-indentation (&optional n)
  "Move cursor to beginning of this line or to its indentation.
  If at indentation position of this line, move to beginning of line.
  If at beginning of line, move to beginning of previous line.
  Else, move to indentation position of this line.
  With arg N, move backward to the beginning of the Nth previous line.
  Interactively, N is the prefix arg."
  (interactive "P")
  (let ((p (point)))
    (cond
     ((= p (save-excursion (beginning-of-visual-line) (point)))
      (line-move -1)
      (back-to-visual-indentation))
     ((= p (save-excursion (back-to-visual-indentation) (point)))
      (beginning-of-visual-line))
     (t (back-to-visual-indentation)))))

;; (global-set-key "\C-a" 'beginning-or-indentation)
;; (global-set-key "\C-e" 'end-or-trailing)
(global-unset-key "\C-a")
(global-unset-key "\C-e")

;; lets try this for awhile
(global-set-key (kbd "<home>") 'beginning-or-indentation)
(global-set-key (kbd "<end>") 'end-or-trailing)



;; turns out don't want this since i have kinesis, C-backspace
;; is better
;;(global-set-key "\C-w" 'backward-kill-word)
;;(global-set-key "\C-x\C-k" 'kill-region)

(global-set-key (kbd "S-SPC") 'dabbrev-expand)
(global-set-key (kbd "M-SPC") 'dabbrev-expand)
(setq dabbrev-case-fold-search nil)
(global-unset-key (kbd "M-/"))

;; Don't use alt-x, use C-x C-m, alt is a pain, and use ido for it
(global-set-key
 "\C-x\C-m"
 (lambda ()
   (interactive)
   (call-interactively
    (intern
     (ido-completing-read
      "M-x "
      (all-completions "" obarray 'commandp))))))

;; Emacs won't load shell-script-mode for zsh automatically
(setq auto-mode-alist
      (append
       ;; File name (within directory) starts with a dot.
       '(("zshrc" . shell-script-mode)
         ("\\.do\\'" . shell-script-mode))
       auto-mode-alist))

;; (setq auto-mode-alist
;;       (append
;;        ;; File name (within directory) starts with a dot.
;;        '(("\\.json\\'" . js2-mode))
;;        auto-mode-alist))

(setq auto-mode-alist
      (append
       ;; File name (within directory) starts with a dot.
       '(("\\.cpp.cog\\'" . c++-mode)
         ("\\.hpp.cog\\'" . c++-mode)
         ("\\.incl$'" . c++-mode))
       auto-mode-alist))

;; For most modes I'm coding, I don't want line wrap
(setq-default truncate-lines t)

;; So I can delete it
(setq show-trailing-whitespace t)

(add-hook 'sh-mode-hook
          (lambda () (setq indent-tabs-mode nil)))

;; TODO: Filter untabify for makefiles

(setq indent-tabs-mode nil)

;; Most useful binding ever
(global-set-key (kbd "M-/") 'comment-or-uncomment-region) ;; C-S-_ does undo already

;; By default compilation frame is half the window. Yuck.
(setq compilation-window-height 8)

;; keep the window focused on the messages during compilation
(setq compilation-scroll-output t)

;; Keep the highlight on the compilation error
(setq next-error-highlight t)

(defun buffer-mode (buffer-or-string)
  "Returns the major mode associated with a buffer."
  (save-excursion
    (set-buffer buffer-or-string)
    major-mode))

;; When compiling, make the compile window go away when finished if there are no errors
;; (setq compilation-finish-function
;;       (lambda (buf str)
;;         (if (string-match "ack:.*" (buffer-name buf))
;;             (let ((cur-window (selected-window)))
;;               (select-window (get-buffer-window buf))
;;               (goto-char 0)
;;               (select-window cur-window))
;;           (if (string-match "exited abnormally" str)
;;               ;;there were errors
;;               (message "compilation errors, press C-x ` to visit")
;;             ;;no errors, make the compilation window go away in 0.5 seconds
;;             (run-at-time 0.5 nil 'delete-windows-on buf)
;;             (message "NO COMPILATION ERRORS!")))))

(global-set-key "\M-j" 'previous-buffer)
(global-set-key "\M-k" 'next-buffer)

;; In programming modes indent when yanking
(dolist (command '(yank yank-pop))
   (eval `(defadvice ,command (after indent-region activate)
            (and (not current-prefix-arg)
                 (derived-mode-p 'prog-mode)
                 (let ((mark-even-if-inactive transient-mark-mode))
                   (indent-region (region-beginning) (region-end) nil))))))

(defun open-line-and-indent ()
  (interactive)
  ;;(message "how about this version")
  (indent-according-to-mode)
  (open-line 1)
  (save-excursion
    (next-line)
    (indent-according-to-mode)))

(defun kill-and-indent (&optional ARG)
  (interactive)
  (kill-visual-line ARG)
  (indent-according-to-mode))

(global-set-key (kbd "RET") #'reindent-then-newline-and-indent)
(global-set-key (kbd "C-o") #'open-line-and-indent)
(global-set-key (kbd "C-k") #'kill-and-indent)

(setq auto-mode-alist
      (cons '("\\.make\\'" . makefile-gmake-mode) auto-mode-alist))

(add-hook 'asm-mode-hook
          (lambda ()
            (local-set-key (kbd "RET") 'newline)))

;; If I'm searching and I hit backspace, I mean backspace dammit.
(define-key isearch-mode-map '[backspace] 'isearch-delete-char)

(defun close-frame-or-exit ()
  "Tries to close the current frame, if it's the only one left just exits."
  (interactive)
  (if (= (length (frame-list)) 1)
      (save-buffers-kill-emacs)
(global-set-key "\C-x\C-c" 'close-frame-or-exit)
    (delete-frame)))

;; Close windows, not emacs.

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "|")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers
(setq uniquify-buffer-name-style 'post-forward)

(global-set-key (kbd "C-x K") 'kill-other-buffers-of-this-file-name)

(defun kill-other-buffers-of-this-file-name (&optional buffer)
  "Kill all other buffers visiting files of the same base name."
  (interactive "bBuffer to make unique: ")
  (setq buffer (get-buffer buffer))
  (cond ((buffer-file-name buffer)
         (let ((name (file-name-nondirectory (buffer-file-name buffer))))
           (loop for ob in (buffer-list)
                 do (if (and (not (eq ob buffer))
                             (buffer-file-name ob)
                             (let ((ob-file-name (file-name-nondirectory (buffer-file-name ob))))
                               (or (equal ob-file-name name)
                                   (string-match (concat name "\\.~.*~$") ob-file-name))) )
                        (kill-buffer ob)))))
        (default (message "This buffer has no file name."))))

;; can use M-r to toggle instead
;; (global-set-key "\C-s" 'isearch-forward)
;; (global-set-key "\C-r" 'isearch-backward)

(global-set-key (kbd "C-%") 'query-replace-regexp)
(global-set-key "\M-%" 'query-replace)

;; Threshold after which we consider the file to be large
;; and don't want to do anything too expensive.
(setq uncomfortable-buffer-size (* 10 1024 1024))
(setq uncomfortable-line-size (* 10 1024))

;; Make more notepad like out of the box
(setq default-major-mode 'text-mode)
(setq text-mode-hook        ; Enable auto-fill-mode
      '(lambda ()
         (when (buffer-file-name)
           (let ((ext (file-name-extension (buffer-file-name))))
             (when (and (not (or (string-equal ext "tc")
                                 (string-equal ext "in")
                                 (string-equal ext "tmp")
                                 (string-equal ext "log")))
                        (< (buffer-size) uncomfortable-buffer-size))
               (visual-line-mode 1))))))

;; Taken from Trey Jackson's answer on superuser.com
;; http://superuser.com/questions/205420/how-can-i-interrupt-emacs-opening-a-large-file
(defun my-find-file-check-make-large-file-read-only-hook ()
  ;; Long lines make emacs slow, use first line to judge if we should wrap.
  ;; Not perfect, but good enough for big JSON dumps
  (when (> (- (point-at-eol) (point-at-bol)) uncomfortable-line-size)
    (visual-line-mode 1)
    (message "Large line: Enabling visual line mode to preserve sanity."))
  ;; Turn lots of other stuff off
  (when (> (buffer-size) uncomfortable-buffer-size)
    (setq buffer-read-only t)
    (setq auto-save-default nil)
    (buffer-disable-undo)
    (fundamental-mode)
    (font-lock-mode -1)
    (linum-mode 0)
    (smartparens-mode 0)
    (setq md-enable-symbol-refresh nil)
    (auto-revert-mode 0)
    (setq global-auto-revert-ignore-buffer t)
    (message "Large buffer: Undo disabled, made read only, autosave disabled.")))
(add-hook 'find-file-hooks 'my-find-file-check-make-large-file-read-only-hook)

(defun my-delete-leading-whitespace (start end)
  "Delete whitespace at the beginning of each line in region."
  (interactive "*r")
  (save-excursion
    (if (not (bolp)) (forward-line 1))
    (delete-whitespace-rectangle (point) end nil)))
(global-set-key "\C-x\C-h" 'my-delete-leading-whitespace)

;;(add-to-list 'global-auto-revert-ignore-modes 'special-mode)
(setq global-auto-revert-non-file-buffers nil)
(global-auto-revert-mode 1)

(modify-frame-parameters nil '((wait-for-wm . nil)))

;; Can enable again once I reinstall markdown mode, marmelade is down...
;; (autoload 'markdown-mode "markdown-mode.el"
;;   "Major mode for editing Markdown files" t)
;; (setq auto-mode-alist
;;       (cons '("\\.md" . markdown-mode) auto-mode-alist))

;; lets you delete camelcase words one at a time
(add-hook 'js-mode-hook #'subword-mode)
(subword-mode t)
;; for consistency have in minibuffer too
;;(add-hook 'minibuffer-setup-hook #'subword-mode)

(defvar-local mandimus-last-word-event "")

(defvar-local etc-had-process nil)

;; use setq-default to set it for /all/ modes
(setq-default mode-line-format
              (list
               ;; the buffer name; the file name as a tool tip
               '(:eval (propertize "%b " 'face 'font-lock-warning-face
                                   'help-echo (buffer-file-name)))

               ;; line and column
               "(" ;; '%02' to set to 2 chars at least; prevents flickering
               (propertize "%02l" 'face 'font-lock-type-face) ","
               (propertize "%02c" 'face 'font-lock-type-face)
               ") "

               ;; relative position, size of file
               "["
               (propertize "%p" 'face 'font-lock-comment-face) ;; % above top
               "/"
               (propertize "%I" 'face 'font-lock-comment-face) ;; size
               "] "

               ;; the current major mode for the buffer.
               "["

               '(:eval (propertize "%m" 'face 'font-lock-string-face
                                   'help-echo buffer-file-coding-system))
               '(:eval (when (derived-mode-p 'term-mode)
                         (if (term-in-line-mode)
                             (propertize " - Line"
                                         'face 'font-lock-string-face
                                         'help-echo "Terminal is in line mode")
                           (propertize " - Char"
                                       'face 'font-lock-string-face
                                       'help-echo "Terminal is in char mode"))))
               "] "

               "[" ;; insert vs overwrite mode, input-method in a tooltip
               '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
                                   'face 'font-lock-variable-name-face
                                   'help-echo (concat "Buffer is in "
                                                      (if overwrite-mode "overwrite" "insert") " mode")))

               ;; is this buffer read-only?
               '(:eval (when buffer-read-only
                         (concat ","  (propertize "RO"
                                                  'face 'font-lock-type-face
                                                  'help-echo "Buffer is read-only"))))
               "] "
               '(:eval
                 (let ((p (get-buffer-process (current-buffer))))
                   (when (or p etc-had-process)
                     (setq etc-had-process t)
                     (concat " ["  (propertize (if (process-live-p p) "Running." "Stopped.")
                                               'face
                                               (if (process-live-p p) 'font-lock-string-face 'info-menu-star)
                                               'help-echo "Buffer has a process associated with it.")
                             "] "))))

               ;; add the time, with the date and the emacs uptime in the tooltip
               '(:eval (propertize (format-time-string "%H:%M")
                                   'help-echo
                                   (concat (format-time-string "%c; ")
                                           (emacs-uptime "Uptime:%hh"))))
               ;; was this buffer modified since the last save?
               '(:eval (when (buffer-modified-p)
                         (concat " "  (propertize "Mod"
                                                  'face 'font-lock-warning-face
                                                  'help-echo "Buffer has been modified"))))

               " -- "
               '(:eval (when mandimus-last-word-event
                         (propertize (format "%S" mandimus-last-word-event)
                                     'face 'font-lock-function-name-face
                                     'help-echo "Words last interpreted by Dragon")))
               ;; i don't want to see minor-modes; but if you want, uncomment this:
               ;;minor-mode-alist  ;; list of minor modes
               " %-" ;; fill with '-'
               ))


;; Ostensibly this is for letting you insert the results of minibuffer
;; commands into other minibuffer commands, but I like it because it
;; just lets me use the damn thing without getting the error about
;; "Command attempted to use minibuffer while in minibuffer"
;;(setq enable-recursive-minibuffers t)

;; We always want a gigantic mark ring
(setq-default mark-ring-max 65535)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;
;; ace jump mode major function
;;
(add-to-list 'load-path "~/etc/ace-jump-mode")
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
(autoload
  'ace-jump-line-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
;; you can select the key you prefer to
(define-key global-map (kbd "M-RET") 'ace-jump-mode)
(define-key shell-mode-map (kbd "M-RET") nil)
(define-key global-map (kbd "S-<return>") 'ace-jump-line-mode)
;;(global-set-key (kbd "M-RET") 'ace-jump-mode)

;;
;; enable a more powerful jump back function from ace jump mode
;;
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
;;(define-key global-map (kbd "C-x SPC") 'rectangle-mark-mode)
(setq ace-jump-mode-scope 'window)

(setq find-file-wildcards t)

(defun get-point (symbol &optional arg)
  "get the point"
  (funcall symbol arg)
  (point)
  )

(defun copy-thing (begin-of-thing end-of-thing &optional arg)
  "copy thing between beg & end into kill ring"
  (save-excursion
    (let ((beg (get-point begin-of-thing 1))
          (end (get-point end-of-thing arg)))
      (copy-region-as-kill beg end)))
  )

(defun copy-word (&optional arg)
  "Copy words at point into kill-ring"
  (interactive "P")
  (copy-thing 'backward-word 'forward-word arg)
  ;;(paste-to-mark arg)
  )

(defun etc-set-mark-or-expand-region (&optional arg)
  (interactive "P")
  (cond
   ((not mark-active) (set-mark-command arg))
   (t (er/expand-region (prefix-numeric-value arg)))))

(require 'expand-region)
(global-set-key (kbd "C-SPC") #'etc-set-mark-or-expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "M-=") 'er/contract-region)

;; (require 'change-inner)
;; (global-set-key (kbd "M-i") 'change-inner)
;; (global-set-key (kbd "M-o") 'change-outer)

(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (replace-regexp-in-string (rx (or (: bos (* (any " \t\n")))
                                    (: (* (any " \t\n")) eos)))
                            ""
                            str))

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program
      (chomp (shell-command-to-string "~/etc/utils/pick_best_browser")))

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 500)
(setq recentf-max-saved-items 500)
(global-set-key "\C-c\ \C-e" 'recentf-open-files)

;; never what I want, almost always a typo. Why would you put this
;; right next to the key for a new frame?
(global-unset-key (kbd "C-x 5 1"))

;; more useful than th default version
(global-set-key (kbd "M-z") 'zap-up-to-char)

(defun macroexpand-point (sexp)
  (interactive (list (sexp-at-point)))
  (with-output-to-temp-buffer "*el-macroexpansion*"
    (pp (macroexpand-all sexp)))
  (with-current-buffer "*el-macroexpansion*" (emacs-lisp-mode)))

(defun unkillable-scratch-buffer ()
	(if (equal (buffer-name (current-buffer)) "*scratch*")
	    (progn
	      (delete-region (point-min) (point-max))
	      nil)
	  t))
(add-hook 'kill-buffer-query-functions 'unkillable-scratch-buffer)

;; cancel minibuffer prompts when I switch focus
(defun etc-abort-minibuffer ()
  (when (or (> (minibuffer-depth) 1)
            (minibuffer-prompt))
    (abort-recursive-edit)))
(add-hook 'focus-out-hook #'etc-abort-minibuffer)

(global-set-key "\C-xg" #'magit-status)
(global-set-key (kbd "C-c i") #'magit-blame-mode)

;; otherwise big C++ buffers take forever
(setq font-lock-support-mode 'jit-lock-mode)
(setq jit-lock-defer-time 0.032) ;; 30fps
(setq jit-lock-stealth-time 2) ;; wait 2s before lazily doing the rest
(setq jit-lock-stealth-nice 0.5)


(defun etc-shell-command ()
  (interactive)
  (if (use-region-p)
      (call-interactively #'shell-command-on-region)
    (call-interactively #'shell-command)))

(defun etc-delete-other-windows ()
  (interactive)
  (delete-other-windows)
  (unless (derived-mode-p 'erc-mode)
    ;; don't interfere with erc scroll-to-bottom
    (recenter-top-bottom)))

;; much more convenient to reach
(global-set-key (kbd "C-]") #'etc-delete-other-windows)
(global-set-key (kbd "M-]") #'abort-edit-recursive)
(global-unset-key (kbd "C-x 1"))

(global-set-key (kbd "C-<return>") #'find-file-at-point)

;; enables focus follows mouse, needed for head tracking
;; FIXME: disabled for now, when not using head tracking
;; causes surprising behavior when interacting w/ xmonad
;; warping the pointer to the center of the window on focus
(setq mouse-autoselect-window nil)
;; makes emacs aware of my window manager behavior, not sure what this gets me
(setq focus-follows-mouse t)

;; the default of ten is annoying for debugging
(setq print-length 100)

;; I prefer scrolling to always move point
(global-set-key (kbd "<next>") #'md-down-screenful)
(global-set-key (kbd "<prior>") #'md-up-screenful)

;; let ctrl-backspace/delete skip over punctuation
(global-set-key (kbd "C-<backspace>") #'md-backward-kill-word)
(global-set-key (kbd "C-<delete>") #'md-forward-kill-word)

;; with speech recognition we don't want to have to
;; constantly recenter the window around point, so
;; we actually *want* jumpy scrolling rather than
;; smooth
(setq scroll-step 10)
(setq scroll-margin 0)
(setq auto-window-vscroll nil)

(defun etc-maybe-recenter ()
  (unless (or (derived-mode-p 'erc-mode 'term-mode 'shell-mode 'eshell-mode)
              (not (eq (get-buffer-window (current-buffer) t) (selected-window)))
              (equal (window-point) (point-max))
              (region-active-p))
    ;; don't interfere with erc scroll-to-bottom
    (recenter)))

;; setting to zero makes scrolling downwards slow,
;; according to profiler because of line-move-partial
(setq scroll-conservatively 1)
;; so instead we setup on an idle timer
(defvar etc-recenter-timer nil)
(progn
  (when etc-recenter-timer
    (cancel-timer etc-recenter-timer))
  (setq etc-recenter-timer nil)
  (setq etc-recenter-timer (run-with-idle-timer 0.25 t #'etc-maybe-recenter)))
;; (cancel-timer etc-recenter-timer)

;; Without an active region, assume we want to copy/paste
;; symbols, unless we're on a opener/closer in which case
;; assume we want the sexp/string.
(global-set-key (kbd "C-w") #'md-kill-symbol-or-sexp-or-region)
(global-set-key (kbd "M-w") #'md-copy-symbol-or-sexp-or-region)

;; couldn't get this to work with existing theme....
;; (require 'smart-mode-line)
;; (sml/setup)
;; (sml/apply-theme 'powerline)

;; (let ((cyberpunk-blue-5 "#4c83ff")
;;       (cyberpunk-gray-5 "#333333"))
;;   (set-face-attribute 'mode-line nil
;;                       :foreground cyberpunk-blue-5
;;                       :background cyberpunk-gray-5
;;                       :box '(:line-width -1)))

;; see if this helps with window popups any
(setq display-buffer-reuse-frames t)

;; Show matching parentheses
;; disable, conflicts with smartparens highlighting
(show-paren-mode 0)

;; Two spaces is heresy! ;)
(setq sentence-end-double-space nil)

(defun etc-reopen-with-sudo ()
  (interactive)
  (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name)))
(global-set-key (kbd "C-c o s") #'etc-reopen-with-sudo)

;; (require 'string-inflection)
;; (global-set-key (kbd "C-c y") #'string-inflection-cycle)
;; (global-set-key (kbd "C-c m s") #'string-inflection-underscore)
;; (global-set-key (kbd "C-c m c") #'string-inflection-camelcase)
;; (global-set-key (kbd "C-c m l") #'string-inflection-lower-camelcase)
;; (global-set-key (kbd "C-c m u") #'string-inflection-upcase)

;; force myself to use C-i so I don't stretch my left pinky
;;(global-unset-key (kbd "<tab>"))

(global-set-key (kbd "C-M-u") #'other-window)
(global-set-key (kbd "C-S-M-u") #'etc-delete-other-windows)

(defun etc-prior ()
  (interactive)
  (setq unread-command-events (listify-key-sequence "\M-p")))

(defun etc-future ()
  (interactive)
  (setq unread-command-events (listify-key-sequence "\M-n")))

;; (global-set-key (kbd "S-<up>") #'etc-prior)
;; (global-set-key (kbd "S-<down>") #'etc-future)
;; (global-unset-key (kbd "S-<up>") #'etc-prior)
;; (global-unset-key (kbd "S-<down>"))

(define-key drag-stuff-mode-map (kbd "M-<right>") nil)
(define-key drag-stuff-mode-map (kbd "M-<left>") nil)
(global-set-key (kbd "M-<left>") #'beginning-or-indentation)
(global-set-key (kbd "M-<right>") #'end-or-trailing)
(global-set-key (kbd "C-M-<left>") #'sp-beginning-of-sexp)
(global-set-key (kbd "C-M-<right>") #'sp-end-of-sexp)

(global-set-key (kbd "M-;") #'comment-or-uncomment-region)

;; image mode crashes in Motif mode
;; Can't use GTK/Athena because they randomly lockup
;; ... I hate you emacs
(setq auto-mode-alist (rassq-delete-all 'image-mode auto-mode-alist))
(setq magic-mode-alist (rassq-delete-all 'image-mode magic-mode-alist))
(setq magic-fallback-mode-alist (rassq-delete-all 'image-mode magic-fallback-mode-alist))

;;(load-file "~/etc/emacs/etc-racket-custom.el")

;; at the bottom so it has best chance of getting in hooks
(load-file "~/etc/emacs/mandimus.el")
(load-file "~/etc/emacs/md-belt-custom.el")
(load-file "~/etc/emacs/md-company-custom.el")
(put 'dired-find-alternate-file 'disabled nil)

;; for emacsclient
(server-start)
