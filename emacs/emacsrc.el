;; MOST ESSENTIAL BINDINGS FIRST IN CASE EXTENSIONS FAIL TO LOAD

(global-set-key (kbd "C-<home>") #'beginning-of-buffer)
(global-set-key (kbd "C-<end>") #'end-of-buffer)

;; should get used to using delete key
(global-unset-key "\C-d")
(global-set-key [delete] 'delete-char)

(global-set-key "\C-a" 'beginning-or-indentation)
(global-set-key "\C-e" 'end-or-trailing)

(when (not (string= (system-name) "eruv"))
  (global-unset-key "\C-a")
  (global-unset-key "\C-e"))

;; lets try this for awhile
(global-set-key (kbd "<home>") 'beginning-or-indentation)
(global-set-key (kbd "<end>") 'end-or-trailing)

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
     (completing-read-default
      "M-x "
      (all-completions "" obarray 'commandp))))))

;; Most useful binding ever
(global-set-key (kbd "M-/") 'comment-or-uncomment-region) ;; C-S-_ does undo already

(global-set-key "\M-j" 'previous-buffer)
(global-set-key "\M-k" 'next-buffer)

(global-set-key (kbd "C-%") 'query-replace-regexp)
(global-set-key "\M-%" 'query-replace)

;; never what I want, almost always a typo. Why would you put this
;; right next to the key for a new frame?
(global-unset-key (kbd "C-x 5 1"))

;; more useful than th default version
(global-set-key (kbd "M-z") 'zap-up-to-char)

(defun etc-delete-other-windows ()
  (interactive)
  (delete-other-windows)
  (unless (derived-mode-p 'erc-mode)
    ;; don't interfere with erc scroll-to-bottom
    (recenter-top-bottom)))

;; much more convenient to reach
(global-set-key (kbd "C-]") #'etc-delete-other-windows)
(global-set-key (kbd "M-]") #'abort-edit-recursive)
(global-set-key (kbd "C-x 1") #'etc-delete-other-windows)

(defun etc-smart-find-file-at-point ()
  "Uses projectile find file at point unless not in a project."
  (interactive)
  (let* ((filename-at-point (thing-at-point 'filename))
        (guess (replace-regexp-in-string ":?\\([0-9]+:\\)?\\([0-9]+:\\)?\\'" "" filename-at-point))) ;; remove trailing line numbers and ":"
    (message "guess: %s" guess)
    ;; (message "better guess: %s" (concat (projectile-project-root) "/source/" guess))
    (if (and filename-at-point (file-exists-p filename-at-point))
        (find-file filename-at-point)
      (if (and guess (file-exists-p guess))
          (progn
            (find-file guess))
        (if (and (projectile-project-p)
                 (file-exists-p (concat (projectile-project-root) guess)))
            (find-file (concat (projectile-project-root) guess))
          (if (and (projectile-project-p)
                   (file-exists-p (concat (projectile-project-root) "/source/" guess)))
              (find-file (concat (projectile-project-root) "/source/" guess))
            (with-most-recent-project
                (message "testing two")
              (if (projectile-project-p) ;; can be false if there is no most recent project
                  (let* ((project-files (projectile-current-project-files))
                         (files (projectile-select-files project-files)))
                    (if files
                        (find-file (concat (projectile-project-root) (car files)))
                      (user-error "Couldn't find file relative to current buffer or in most recent project.")))
                (user-error "Couldn't find file relative to current buffer and no most recent project to search."))
              )))))))

(global-set-key (kbd "C-<return>") #'etc-smart-find-file-at-point)

(global-set-key (kbd "M-;") #'comment-or-uncomment-region)

(when (file-exists-p "~/gentoo/usr/share/emacs/site-lisp/site-gentoo")
    (load "~/gentoo/usr/share/emacs/site-lisp/site-gentoo"))

(when (>= emacs-major-version 24)
  (require 'package)
  (setq package-user-dir (concat "~/.emacs.d/packages-" (md5 (emacs-version))))
  (package-initialize)
  (setq package-archives '(
                           ("gnu" . "http://elpa.gnu.org/packages/")
                           ;;("marmalade" . "https://marmalade-repo.org/packages/")
;;                           ("melpa-stable" . "https://stable.melpa.org/packages/")
                           ("melpa" . "https://melpa.org/packages/")
                           )
        package-archive-priorities
      '(;;("melpa-stable" . 10)
        ("gnu"     . 0)
        ("melpa"        . 5)))
  (when (not (package-installed-p 'use-package))
    (package-refresh-contents)
    (package-install 'use-package)))

(require 'use-package)

(global-set-key (kbd "M-,") #'xref-find-references)

(load-file "~/etc/emacs/smartparens-custom.el")
(load-file "~/etc/emacs/window_bind.el")
(load-file "~/etc/emacs/unit_layers.el")
(load-file "~/etc/emacs/etc-debug-overlay.el")
(load-file "~/etc/emacs/lsp-custom.el")
(load-file "~/etc/emacs/etc-windmove.el")

(use-package
  drag-stuff
  :ensure t
  )

(define-key drag-stuff-mode-map (kbd "M-<up>") #'drag-stuff-up)
(define-key drag-stuff-mode-map (kbd "M-<down>") #'drag-stuff-down)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; LESS ESSENTIAL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package
  dash
  :ensure t
  )

(use-package
  ace-jump-mode
  :ensure t
  )

(use-package
  dockerfile-mode
  :ensure t
  )

(use-package
  free-keys
  :ensure t
  )

;; builtin python mode doesn't support python 3.10 match/case
;; (use-package
;;   python-mode
;;   :ensure t
;;   )

;;(use-package
;;  smart-parens
;;  :ensure t
;;  )

;; (use-package
;;   perl6-mode
;;   :ensure t)

;; (use-package
;;   racket-mode
;;   :ensure t)

(use-package
  haskell-mode
  :ensure t
  )

(use-package
  magit
  :ensure t
  )

;; (use-package
;;   magit-gerrit
;;   :ensure t
;;   :pin melpa
;;   )
;; (require 'magit-gerrit)

(use-package
  projectile
  :ensure t
  )

;; binding mysteriously disappeared after some upgrade
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
;;(global-set-key (kbd "C-c p p") #'projectile-switch-project)

(load-file "~/etc/emacs/projectile-bookmarks.el")

(use-package
  erc-hl-nicks
  :ensure t
  )

 (use-package
   realgud
   :ensure t
   )

(use-package
  goto-chg
  :ensure t
)

(use-package
  undo-tree
  :ensure t
  )
(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

(use-package
  expand-region
  :ensure t
  )

(require 'expand-region)
(global-set-key (kbd "C-SPC") #'etc-set-mark-or-expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "M-=") 'er/contract-region)

(use-package
  string-inflection
  :ensure t)

(use-package
  f
  :ensure t
  )

;;(use-package
;;  docker-tramp
;;  :ensure t
;;  )

;;(use-package
;;  protobuf-mode
;;  :ensure t
;;  )

;; (use-package smart-hungry-delete
;;   :ensure t
;;   :bind (("<backspace>" . smart-hungry-delete-backward-char)
;; 		 ("C-d" . smart-hungry-delete-forward-char))
;;   :defer nil ;; dont defer so we can add our functions to hooks
;;   :config (smart-hungry-delete-add-default-hooks)
;;   )

;;(global-set-key (kbd "<backspace>") #'delete-backward-char)
;;(global-set-key (kbd "C-d") #'delete-forward-char)

;; (defun etc-disable-smart-hungry-backward (orig-fun &rest args)
;;   (if (get-buffer-process (current-buffer))
;;       (apply #'delete-backward-char args)
;;     (apply orig-fun args)))

;; (defun etc-disable-smart-hungry-forward (orig-fun &rest args)
;;   (if (get-buffer-process (current-buffer))
;;       (apply #'delete-forward-char args)
;;     (apply orig-fun args)))


;; (advice-remove 'smart-hungry-delete-backward-char #'etc-disable-smart-hungry-backward)
;; (advice-remove 'smart-hungry-delete-forward-char #'etc-disable-smart-hungry-forward)

(when (not window-system)
  (xterm-mouse-mode))

(defun etc-ignore-bug (orig-fun &rest args)
  (condition-case e
      (apply orig-fun args)
    (error
     (user-error "Ignoring the problem: %s" e)
     nil)))

(advice-add 'c-forward-sws :around #'etc-ignore-bug)
(advice-add 'c-font-lock-enum-body :around #'etc-ignore-bug)
(advice-add 'python-shell-comint-end-of-output-p :around #'etc-ignore-bug)
(advice-add 'python-shell-output-filter :around #'etc-ignore-bug)
(advice-add 'c-state-balance-parens-backwards :around #'etc-ignore-bug)
(advice-add 'c-forward-decl-or-cast-1 :around #'etc-ignore-bug)


;;(load-file "~/etc/emacs/smartparens-custom.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-view-program-selection
   '(((output-dvi style-pstricks)
      "Evince")
     (output-dvi "Evince")
     (output-pdf "Evince")
     (output-html "Evince")))
 '(c-noise-macro-names '("constexpr"))
 '(custom-safe-themes
   '("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "f0b0710b7e1260ead8f7808b3ee13c3bb38d45564e369cbe15fc6d312f0cd7a0" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default))
 '(ediff-split-window-function 'split-window-horizontally)
 '(haskell-mode-hook '(turn-on-haskell-indent))
 '(ignored-local-variable-values
   '((eval font-lock-add-keywords nil
           `((,(concat "("
                       (regexp-opt
                        '("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl")
                        t)
                       "\\_>")
              1 'font-lock-variable-name-face)))
     (vc-prepare-patches-separately)
     (diff-add-log-use-relative-names . t)
     (vc-git-annotate-switches . "-w")))
 '(package-selected-packages
   '(free-keys filladapt lsp-ivy orderless vertico xclip xterm-color lsp-mode zig-mode minimap dockerfile-mode async smartparens lsp-ui visible-mark counsel docker-tramp ein counsel-projectile counsel-tramp counsel-gtags ivy-hydra ivy flycheck-rust toml-mode lsp-flycheck rust-mode smart-hungry-delete sqlup-mode helm-ag julia-shell julia-repl julia-mode helm-bbdb gmail2bbdb jabber jabber-mode bbdb magit use-package undo-tree string-inflection realgud racket-mode perl6-mode haskell-mode goto-chg f expand-region erc-hl-nicks))
 '(safe-local-variable-values
   '((eval add-hook 'after-save-hook
           (lambda nil
             (shell-command
              (format "rsync -av %s %s/dragonshare/NatLink/NatLink/MacroSystem"
                      (buffer-file-name)
                      (getenv "HOME"))))
           nil t)
     (eval add-hook 'after-save-hook
           (lambda nil
             (shell-command
              (format "touch %s/dragonshare/NatLink/NatLink/MacroSystem/_dfly_client.py"
                      (getenv "HOME"))))
           nil t)
     (eval add-hook 'after-save-hook
           (lambda nil
             (shell-command
              (format "rsync -av %s %s/dragonshare/NatLink/NatLink/MacroSystem/_%s"
                      (buffer-file-name)
                      (getenv "HOME")
                      (buffer-name))))
           nil t)
     (eval add-hook 'after-save-hook
           (lambda nil
             (shell-command
              (format "rsync -av %s %s/dragonshare/NatLink/NatLink/MacroSystem/_%s"
                      (buffer-file-name)
                      (getenv "HOME")
                      (buffer-name)))))))
 '(send-mail-function 'smtpmail-send-it)
 '(tramp-default-proxies-alist
   '(("^192\\.168\\.68\\.65$"
      #("^root$" 1 5
        (tramp-default t))
      #("/sudo:root@192.168.68.65:" 0 6
        (tramp-ad-hoc t)
        6 10
        (tramp-ad-hoc t tramp-default t)
        10 25
        (tramp-ad-hoc t)))
     ("^192\\.168\\.68\\.65$"
      #("^root$" 1 5
        (tramp-default t))
      #("/ssh:teamslice@192.168.68.65:" 0 29
        (tramp-ad-hoc t)))))
 '(tramp-save-ad-hoc-proxies t)
 '(tramp-syntax 'default nil (tramp))
 '(warning-suppress-log-types '((comp)))
 '(warning-suppress-types '((comp))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 89)) (:foreground "#d3d3d3" :background "#000000")))))

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
;;(setq-default debug-on-error t)
(setq message-log-max 100000)

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

;; Helm had too many performance issues
(when nil
  (load-file "~/etc/emacs/helm-custom.el")
  (load-file "~/etc/emacs/helm-ag-custom.el"))

;; Could never get relative line numbers to work with ivy
(when t
   (load-file "~/etc/emacs/ivy-custom.el"))

;; vertico indexed mode works great, but eager directory deletion is missing
;;(load-file "~/etc/emacs/etc-vertico.el")

;;(load-file "~/etc/emacs/ido-custom.el")
(load-file "~/etc/emacs/yasnippet-custom.el")
(load-file "~/etc/emacs/save.el")
(load-file "~/etc/emacs/pair.el")

(load-file "~/etc/emacs/python-custom.el")
(load-file "~/etc/emacs/perl6-custom.el")
(load-file "~/etc/emacs/erc-custom.el")
(load-file "~/etc/emacs/term-custom.el")
(load-file "~/etc/emacs/comint-custom.el")
(load-file "~/etc/emacs/shell-custom.el")
(load-file "~/etc/emacs/memoize.el")
(load-file "~/etc/emacs/projectile-custom.el")
(load-file "~/etc/emacs/proced-custom.el")
;;(load-file "~/etc/emacs/email-custom.el")
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
(load-file "~/etc/emacs/linum-custom.el")
(load-file "~/etc/emacs/visual-line-custom.el")
(load-file "~/etc/emacs/visible-mark-custom.el")
(load-file "~/etc/emacs/eshell-custom.el")
(load-file "~/etc/emacs/gc-custom.el")
(load-file "~/etc/emacs/dabbrev-custom.el")
(load-file "~/etc/emacs/hide-show-custom.el")
(load-file "~/etc/emacs/magit-custom.el")
(load-file "~/etc/emacs/diff-custom.el")
;;(load-file "~/etc/emacs/jabber-custom.el")
(load-file "~/etc/emacs/julia-custom.el")
(load-file "~/etc/emacs/dired-custom.el")
(load-file "~/etc/emacs/sql-custom.el")
(load-file "~/etc/emacs/rust-custom.el") ;; something in here causes 100% load...
(load-file "~/etc/emacs/vhdl-custom.el")
(load-file "~/etc/emacs/ein-custom.el")
(load-file "~/etc/emacs/zig-custom.el")

(load-file "~/etc/emacs/gui.el")


(setq-default indent-tabs-mode nil)

(delete-selection-mode 1)

(defun etc-set-repeat-rate ()
  ;; Set the keyboard repeat rate to be a lot faster
  ;; technically this should be triggered by some sort of udev
  ;; event because this resets if you unplug and replug in the kbd.
  ;; instead we just run it once a minute, blame laziness :p
  (let ((default-directory "/")) ;; guaranteed to exist so I don't get an error, xset doesn't touch current directory anyway
    (call-process "xset" nil nil nil "r" "rate" "200" "60")))

(defun etc-translate-hack ()
  ;; doing this on a timer seems to be the only way to get
  ;; it to work on startup...  after-init-hook doesn't work,
  ;; neither does just putting top level in emacsrc
  (keyboard-translate ?\C-t ?\C-x)
  (keyboard-translate ?\C-x ?\C-t))
(run-with-timer 0 2 #'etc-translate-hack)

(when (getenv "DISPLAY")
  ;; Make emacs use the normal clipboard
  ;;(setq x-select-enable-clipboard t)
  ;;(setq interprogram-paste-function 'gui-get-primary-selection)
  ;;(setq x-selection-timeout 300)
  ;; When remotely logging in, need to remap alt for emacs keybindings to work
  (when (not (string= (nth 0 (split-string (nth 1 (split-string (getenv "DISPLAY") ":")) "\\.")) "0"))
    (setq x-alt-keysym 'meta))
  )

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

(defun end-or-trailing (&optional n)
  "Move cursor to end of this line or to its indentation.
  If in middle of of this line, move to last nonwhitespace character on line.
  If at last nonwhitespace character of this line, move to end of line.
  With arg N, move backward to the end of the Nth next line.
  Interactively, N is the prefix arg."
  (interactive "P")
  (cond
   (n
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
  (let ((p (point))
        (begin-line (save-excursion (beginning-of-visual-line) (point))))
    (cond
     ((= p begin-line)
      nil
      ;; (line-move -1)
      ;; (back-to-visual-indentation)
      )
     ((= p (save-excursion (back-to-visual-indentation) (point)))
      (beginning-of-visual-line))
     ((= begin-line (save-excursion (skip-chars-backward "[:space:]" begin-line) (point)))
      (beginning-of-visual-line))
     (t (back-to-visual-indentation)))))




;; turns out don't want this since i have kinesis, C-backspace
;; is better
;;(global-set-key "\C-w" 'backward-kill-word)
;;(global-set-key "\C-x\C-k" 'kill-region)

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

(setq auto-mode-alist
      (append
       ;; File name (within directory) starts with a dot.
       '(("\\.bzl\\'" . python-mode)
         ("BUILD" . python-mode))
       auto-mode-alist))

;; For most modes I'm coding, I don't want line wrap
(setq-default truncate-lines t)

;; So I can delete it
(setq show-trailing-whitespace t)

(add-hook 'sh-mode-hook
          (lambda () (setq indent-tabs-mode nil)))

;; TODO: Filter untabify for makefiles

(setq indent-tabs-mode nil)

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

(setq compilation-window-height 16)

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

;; In programming modes indent when yanking
;; (dolist (command '(yank yank-pop md-kill-symbol-or-sexp-or-region))
;;   (eval `(defadvice ,command (after indent-region activate)
;;            (and (not current-prefix-arg)
;;                 (derived-mode-p 'prog-mode)
;;                 (let ((mark-even-if-inactive transient-mark-mode))
;;                   (if (and (region-active-p) (not (= (region-beginning) (region-end))))
;;                       ;; when we have just cut a region the region beginning and end are the same,
;;                       ;; in this case just indents the current line wherever we are
;;                       (if (derived-mode-p 'python-mode)
;;                           (indent-region (line-beginning-position) (line-end-position) nil)
;;                         (indent-region (region-beginning) (region-end) nil))))))))

;;; (advice-remove 'ad-Advice-yank #'yank)
;; (ad-remove-advice 'yank 'around 'ad-Advice-yank)
;; (ad-activate 'yank)

(delete-selection-mode 1)

(defun indenting-yank (&optional arg)
  (interactive "*P")
  (if (use-region-p)
      (delete-region (region-beginning) (region-end)))
  (call-interactively #'yank)
  (indent-region (region-beginning) (region-end)))

(defun indenting-yank-pop (&optional arg)
  (interactive "p")
  (call-interactively #'yank-pop)
  (indent-region (region-beginning) (region-end)))

(global-set-key (kbd "C-y") #'indenting-yank)
(global-set-key (kbd "M-y") #'indenting-yank-pop)

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

(defun etc-prog-mode-hook ()
  (local-set-key (kbd "RET") #'reindent-then-newline-and-indent)
  (local-set-key (kbd "C-o") #'open-line-and-indent)
  (local-set-key (kbd "C-k") #'kill-and-indent))

(add-hook 'prog-mode-hook #'etc-prog-mode-hook)

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


;; Threshold after which we consider the file to be large
;; and don't want to do anything too expensive.
(setq uncomfortable-buffer-size (* 10 1024 1024))
(setq uncomfortable-line-size (* 10 1024))

;; Make more notepad like out of the box
(setq default-major-mode 'text-mode)
(setq text-mode-hook
      '(lambda ()
         (when (buffer-file-name)
           (let ((ext (file-name-extension (buffer-file-name))))
             (when (and (not (or (string-equal ext "tc")
                                 (string-equal ext "in")
                                 (string-equal ext "tmp")
                                 (string-equal ext "log")))
                        (< (buffer-size) uncomfortable-buffer-size))
               ;; make text automatically wrap when the line gets too long
               ;; -- disabled because it's not intelligent enough to understand when text is an emacs interface element
               ;; (refill-mode 1)
               (auto-fill-mode 1)
               ;; render long lines of text by wrapping them
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
    (show-smartparens-mode 0)
    (setq md-enable-symbol-refresh nil)
    (auto-revert-mode 0)
    (setq global-auto-revert-ignore-buffer t)
    (when (fboundp 'display-line-numbers-mode)
      (display-line-numbers-mode 0))
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

;; I don't know how this first made it into my config, but I can't
;; imagine it is necessary anymore. Apparently was a hack for very old
;; window managers.
;; (modify-frame-parameters nil '((wait-for-wm . nil)))

;; Can enable again once I reinstall markdown mode, marmelade is down...
;; (autoload 'markdown-mode "markdown-mode.el"
;;   "Major mode for editing Markdown files" t)
;; (setq auto-mode-alist
;;       (cons '("\\.md" . markdown-mode) auto-mode-alist))

;; lets you delete camelcase words one at a time
;; (add-hook 'js-mode-hook #'subword-mode)
(global-subword-mode)
;; for consistency have in minibuffer too
;;(add-hook 'minibuffer-setup-hook #'subword-mode)

(defvar-local mandimus-last-word-event "")

(defvar-local etc-had-process nil)

;; use setq-default to set it for /all/ modes
(setq-default mode-line-format
              (list
               ;; name of the current project
               '(:eval (when  (and (or (not default-directory)
                                       (not (file-remote-p default-directory)))
                                   (etc-projectile-project-p))
                         (concat (propertize (etc-get-project)
                                                  'face 'font-lock-doc-string-face
                                                  'help-echo "Name of current project."))))

               ;; the buffer name; the file name as a tool tip
               '(:eval (propertize " %b " 'face 'font-lock-warning-face
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
;;(add-to-list 'load-path "~/etc/ace-jump-mode")
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
(define-key message-mode-map (kbd "M-RET") nil)
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
(setq ace-jump-mode-scope 'visible)

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

;; (require 'recentf)
;; (recentf-mode 1)
;; (setq recentf-max-menu-items 500)
;; (setq recentf-max-saved-items 500)
;; (global-set-key "\C-c\ \C-e" 'recentf-open-files)


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
;;(setq font-lock-support-mode 'jit-lock-mode)
;;(setq jit-lock-defer-time 0.032) ;; 30fps
;;(setq jit-lock-stealth-time 2) ;; wait 2s before lazily doing the rest
;;(setq jit-lock-stealth-nice 0.5)


(defun etc-shell-command ()
  (interactive)
  (if (use-region-p)
      (call-interactively #'shell-command-on-region)
    (call-interactively #'shell-command)))

;; (string-trim (thing-at-point 'filename) nil ":?\\([0-9]+:\\)?\\([0-9]+:\\)?")

;; enables focus follows mouse, needed for head tracking
;; FIXME: disabled for now, when not using head tracking
;; causes surprising behavior when interacting w/ xmonad
;; warping the pointer to the center of the window on focus
(setq mouse-autoselect-window t)
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

(defvar etc-last-input-event nil
  "Stores the last input event for use in `etc-maybe-recenter`.")

(defun etc-store-last-input-event ()
  "Store the last input event before the command is executed."
  (setq etc-last-input-event last-input-event))

(add-hook 'pre-command-hook #'etc-store-last-input-event)

(defun etc-maybe-recenter ()
  (unless (or
           ;; don't interfere with erc scroll-to-bottom
           (derived-mode-p 'erc-mode 'term-mode 'shell-mode 'eshell-mode)
           (not (eq (get-buffer-window (current-buffer) t) (selected-window)))
           (equal (window-point) (point-max))
           (region-active-p)
           (memq (event-basic-type etc-last-input-event) ;; don't recenter on scroll wheel
                 '(wheel-up wheel-down mouse-4 mouse-5))
           )
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
(electric-pair-mode 0)

;; Two spaces is heresy! ;)
(setq sentence-end-double-space nil)

(defun etc-reopen-with-sudo ()
  (interactive)
  (find-alternate-file (concat "/sudo::|" buffer-file-name)))

(defun etc-reopen-with-sudo ()
  (interactive)
  (let* ((vec (tramp-dissect-file-name (buffer-file-name (current-buffer))))
       (method (tramp-file-name-method vec))
       (user (tramp-file-name-user vec))
       (host (tramp-file-name-host vec))
       (localname (tramp-file-name-localname vec)))
    (message "localname=%s" localname)
    (find-alternate-file
     (if (tramp-tramp-file-p (buffer-file-name (current-buffer)))
         (concat (format "/%s:%s@%s|sudo::%s" method user host localname))
       (concat "/sudo:root@localhost:" buffer-file-name)))))

(global-set-key (kbd "C-c o s") #'etc-reopen-with-sudo)

(require 'string-inflection)
(global-set-key (kbd "C-c y") #'string-inflection-cycle)
(global-set-key (kbd "C-c m s") #'string-inflection-underscore)
(global-set-key (kbd "C-c m c") #'string-inflection-camelcase)
(global-set-key (kbd "C-c m l") #'string-inflection-lower-camelcase)
(global-set-key (kbd "C-c m u") #'string-inflection-upcase)

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

;; image mode crashes in Motif mode
;; Can't use GTK/Athena because they randomly lockup
;; ... I hate you emacs
;;(setq auto-mode-alist (rassq-delete-all 'image-mode auto-mode-alist))
;;(setq magic-mode-alist (rassq-delete-all 'image-mode magic-mode-alist))
;;(setq magic-fallback-mode-alist (rassq-delete-all 'image-mode magic-fallback-mode-alist))

;;(load-file "~/etc/emacs/etc-racket-custom.el")

;; at the bottom so it has best chance of getting in hooks
(load-file "~/etc/emacs/mandimus.el")
(load-file "~/etc/emacs/md-belt-custom.el")
(load-file "~/etc/emacs/md-company-custom.el")
(put 'dired-find-alternate-file 'disabled nil)

;; if I kill the same thing three times only put one entry in the kill ring
(setq kill-do-not-save-duplicates t)

;; for emacsclient
;;(setenv "XDG_RUNTIME_DIR" "") ;; so server doesn't use it
(server-start)

(defvar etc-auto-revert-buffers-running nil)
(defadvice auto-revert-buffers (around etc-auto-revert-dont-recurse)
  (unless etc-auto-revert-buffers-running
    (let ((etc-auto-revert-buffers-running t))
      ad-do-it)))

(ad-activate 'auto-revert-buffers)

;;; It is the opposite of fill-paragraph
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

 ;; Handy key definition
(define-key global-map "\M-Q" 'unfill-paragraph)

(global-set-key (kbd "C-c .") #'completion-at-point)


;; w/o this remote X emacs is VERY slow while mark is active, every
;; keystroke sends all the text into the clipboard!
(setq select-active-regions nil)

(defun etc-full-profile ()
  (interactive)
  (profiler-stop)
  (profiler-start 'cpu+mem))

(defun etc-full-profile-report ()
  (interactive)
  (profiler-report))

(global-set-key (kbd "<f6>") #'etc-full-profile)
(global-set-key (kbd "<f7>") #'etc-full-profile-report)

(defun delete-trailing-whitespace-except-current-line ()
    (unless buffer-read-only
      (delete-trailing-whitespace (point-min) (line-beginning-position))
      (save-excursion
        (forward-line)
        (delete-trailing-whitespace (point) (point-max)))))

;; delete trailing white space on every line but the current line,
;; which is annoying when you combine automatic saving on window
;; switch with writing python code
(add-hook 'write-file-hooks 'delete-trailing-whitespace-except-current-line nil nil)

;; enable color and compilation windows
;; https://stackoverflow.com/a/63710493/50385
(use-package
  xterm-color
  :ensure t
  )
(require 'xterm-color)
(setq compilation-environment '("TERM=xterm-256color"))
(defun my/advice-compilation-filter (f proc string)
  (funcall f proc (xterm-color-filter string)))
(advice-add 'compilation-filter :around #'my/advice-compilation-filter)

(setq-default tab-width 4)










(defcustom tramp-file-name-with-method "sudo"
  "Which method to be used in `tramp-file-name-with-sudo'."
  :group 'tramp
  :type '(choice (const "su")
                 (const "sudo")
                 (const "doas")
                 (const "ksu")))

(defun tramp-file-name-with-sudo (filename)
  "Convert FILENAME into a multi-hop file name with \"sudo\".
An alternative method could be chosen with `tramp-file-name-with-method'."
  (setq filename (expand-file-name filename))
  (if (tramp-tramp-file-p filename)
      (with-parsed-tramp-file-name filename nil
        (if (and (tramp-multi-hop-p v)
                 (not (string-equal method tramp-file-name-with-method)))
            (tramp-make-tramp-file-name
             (make-tramp-file-name
              :method (tramp-find-method tramp-file-name-with-method nil host)
              :user (tramp-find-user tramp-file-name-with-method nil host)
              :host (tramp-find-host tramp-file-name-with-method nil host)
              :localname localname :hop (tramp-make-tramp-hop-name v)))
          (tramp-user-error v "Multi-hop with `%s' not applicable" method)))
    (tramp-make-tramp-file-name
     (make-tramp-file-name
      :method tramp-file-name-with-method :localname filename))))

(defun tramp-revert-buffer-with-sudo ()
  "Revert current buffer to visit with \"sudo\" permissions.
An alternative method could be chosen with `tramp-file-name-with-method'.
If the buffer visits a file, the file is replaced.
If the buffer runs `dired', the buffer is reverted."
  (interactive)
  (cond
   ((buffer-file-name)
    (find-alternate-file (tramp-file-name-with-sudo (buffer-name))))
   ((derived-mode-p 'dired-mode)
    (setq default-directory (tramp-file-name-with-sudo default-directory)
          list-buffers-directory
          (tramp-file-name-with-sudo list-buffers-directory))
    (if (consp dired-directory)
        (setcar
         dired-directory (tramp-file-name-with-sudo (car dired-directory)))
      (setq dired-directory (tramp-file-name-with-sudo dired-directory)))
    (revert-buffer))))

;;(setq tramp-verbose 3)

;;;;;;;; wayland stuff ;;;;;;;;;;;;;;;;;;

(when
    (and (getenv "DISPLAY")
         (not (getenv "WAYLAND_DISPLAY")))
  (run-with-timer 0 60 #'etc-set-repeat-rate))

;; (when (getenv "WAYLAND_DISPLAY")
;;  ;; without this middle click doesn't work
;;  ;; https://github.com/doomemacs/doomemacs/issues/5219#issuecomment-877282638
;;  ;; (use-package xclip
;;  ;;   :ensure t
;;  ;;   :config
;;  ;;   (setq xclip-program "wl-copy")
;;  ;;   (setq xclip-select-enable-clipboard t)
;;  ;;   (setq xclip-mode t)
;;  ;;   (setq xclip-method (quote wl-copy)))

;;  ;; Without this, copy and pasting from other wayland apps into
;;  ;; emacs-pgtk doesn't work.
;;  ;; https://www.emacswiki.org/emacs/CopyAndPaste#h5o-4
;;  (setq wl-copy-process nil)
;;  (defun wl-copy (text)
;;    (setq wl-copy-process (make-process :name "wl-copy"
;;                                        :buffer nil
;;                                :command '("wl-copy" "-f" "-n")
;;                                :connection-type 'pipe
;;                                :noquery t))
;;    (process-send-string wl-copy-process text)
;;    (process-send-eof wl-copy-process))
;;  (defun wl-paste ()
;;    (if (and wl-copy-process (process-live-p wl-copy-process))
;;        nil ; should return nil if we're the current paste owner
;;      (shell-command-to-string "wl-paste -n | tr -d \r")))
;;  (setq interprogram-cut-function 'wl-copy)
;;  (setq interprogram-paste-function 'wl-paste))

;; (when (getenv "WAYLAND_DISPLAY")
;;   (use-package xclip
;;     :ensure t
;;     :config
;;     (setq xclip-program "wl-copy")
;;     (setq xclip-select-enable-clipboard t)
;;     (setq xclip-mode t)
;;     (setq xclip-method (quote wl-copy)))

;;   (defun wl-copy (text)
;;     "Copy TEXT to the Wayland clipboard."
;;     (let ((process-connection-type nil))
;;       (let ((proc (start-process "wl-copy" "*Messages*" "wl-copy")))
;;         (process-send-string proc text)
;;         (process-send-eof proc))))

;;   (defun wl-paste ()
;;     "Paste text from the Wayland clipboard."
;;     (shell-command-to-string "wl-paste -n"))

;;   (setq interprogram-cut-function 'wl-copy)
;;   (setq interprogram-paste-function 'wl-paste))


;; bizarre issue with emacs-pgtk where shift+space can't be detected
;; unless you set these.
;;
;; https://www.reddit.com/r/emacs/comments/osscfd/pgtk_emacswaylandgnome_no_shiftspace/
(setq pgtk-use-im-context-on-new-connection nil)
(pgtk-use-im-context nil)

(add-to-list 'auto-mode-alist '("\\.log\\." . fundamental-mode))
