;; Color theme
;; (add-to-list 'load-path "~/etc/emacs/color-theme-6.6.0")
;; (require 'color-theme)
;; (setq color-theme-is-global t)
;; (color-theme-initialize)
(if (string= (system-name) "eruv")
    (progn
      (load-file "~/etc/emacs/cyberpunk-theme.el")
      (set-face-attribute 'mode-line nil
                          :foreground "white"
                          :background "grey20"))
  (load-theme 'manoj-dark)
  (set-face-attribute 'mode-line nil
                    :foreground (face-attribute 'mode-line-inactive :foreground)
                    :background (face-attribute 'mode-line-inactive :background))
  )


;; (load-theme)

;; starting up in daemon mode without frames will
;; crash if we try to set this... maybe set on
;; creation of first frame?

(require 'faces)
(require 'frame)

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(fset 'yes-or-no-p 'y-or-n-p) ;; Make all "yes or no" prompts be "y or n" instead

;; Don't use modal popup dialogs, they block all of emacs and prevent mandimus from working
(setq use-dialog-box nil)

;; Stop this crazy blinking cursor
(blink-cursor-mode 0)

;; Hide column number in the mode line
(column-number-mode 0)

;; Show current buffer name in titlebar (instead of emacs@whatever)
(setq frame-title-format "%b")

(setq-default auto-revert-verbose nil)


;; Highlight current line subtly, makes it easier to find cursor
(global-hl-line-mode)
(set-face-background hl-line-face "grey13")


;; https://stackoverflow.com/a/49861904/50385
;;(setq split-width-threshold nil)
;;(setq split-height-threshold (- (window-width) 10))
(setq split-height-threshold 80)
;; (defun count-visible-buffers (&optional frame)
;;   "Count how many buffers are currently being shown. Defaults to selected frame."
;;   (length (mapcar #'window-buffer (window-list frame))))
;; (defun do-not-split-more-than-two-windows (window &optional horizontal)
;;   (if (and horizontal (> (count-visible-buffers) 1))
;;       nil
;;     t))
;; (advice-add 'window-splittable-p :before-while #'do-not-split-more-than-two-windows)


;; better to do stuff that changes frame size in ~/.Xresources


(defvar etc-font-choice nil)
(setq etc-font-choice "DejaVu Sans Mono-12")

;; have to do this as a frame functon or daemon doesn't work
(defun etc-customize-frame (new-frame)
  (when (getenv "DISPLAY")
    (when (window-system new-frame) ;; daemon mode creates frame not associated w/ windowing system!
      (set-frame-font etc-font-choice t t))))

(add-hook 'after-make-frame-functions #'etc-customize-frame)

;; Turn off GUI parts
(when (functionp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (functionp 'menu-bar-mode)
  (menu-bar-mode -1)) ;; (menu-bar-mode 1)
(when (functionp 'scroll-bar-mode)
  (scroll-bar-mode -1))


;; tooltips super slow over remote connections for some reason?
;; https://stackoverflow.com/questions/12730416/disable-help-when-mouse-hovers-on-modeline
;;(tooltip-mode nil)                      ; one-line help text in the echo area
(setq tooltip-use-echo-area t)          ; multi-line help text in the echo area
