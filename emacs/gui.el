;; Color theme
(add-to-list 'load-path "~/etc/emacs/color-theme-6.6.0")
(require 'color-theme)
(setq color-theme-is-global t)
(color-theme-initialize)
(load-file "~/etc/emacs/cyberpunk-theme.el")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 105 :family "Consolas" :embolden f)))))

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


