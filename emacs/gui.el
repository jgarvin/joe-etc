;; Color theme
;; (add-to-list 'load-path "~/etc/emacs/color-theme-6.6.0")
;; (require 'color-theme)
;; (setq color-theme-is-global t)
;; (color-theme-initialize)
(load-file "~/etc/emacs/cyberpunk-theme.el")

;; starting up in daemon mode without frames will
;; crash if we try to set this... maybe set on
;; creation of first frame?

(defvar etc-font-choice nil)

(require 'faces)
(require 'frame)

;; have to do this as a frame functon or daemon doesn't work
(defun etc-customize-frame (new-frame)
  (when (getenv "DISPLAY")
;;    (setq etc-font-choice "DejaVu Sans Mono-12")
    ;;(setq my-font-choice "Consolas-14")
    
    ;; and can't call this or emacsclient -c crashes, wtf
    ;;(set-face-attribute 'default t :font etc-font-choice)
    
    (set-frame-font etc-font-choice t t)))

(add-hook 'after-make-frame-functions #'etc-customize-frame)

;; Turn off GUI parts
(when (functionp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (functionp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (functionp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(fset 'yes-or-no-p 'y-or-n-p) ;; Make all "yes or no" prompts be "y or n" instead

;; Stop this crazy blinking cursor
(blink-cursor-mode 0)

;; Show column number in the mode line
(column-number-mode 1)

;; Show current buffer name in titlebar (instead of emacs@whatever)
(setq frame-title-format "%b")

;; Highlight current line subtly, makes it easier to find cursor
(global-hl-line-mode)
(set-face-background hl-line-face "grey13")

(setq-default auto-revert-verbose nil)
