;; Color theme
;; (add-to-list 'load-path "~/etc/emacs/color-theme-6.6.0")
;; (require 'color-theme)
;; (setq color-theme-is-global t)
;; (color-theme-initialize)
;; (if (string= (system-name) "eruv")
;;     (progn
;;       (load-file "~/etc/emacs/cyberpunk-theme.el")
;;       (set-face-attribute 'mode-line nil
;;                           :foreground "white"
;;                           :background "grey20"))
;;   (load-theme 'manoj-dark)
;;   (set-face-attribute 'mode-line nil
;;                     :foreground (face-attribute 'mode-line-inactive :foreground)
;;                     :background (face-attribute 'mode-line-inactive :background))
;;   )

(if (string= (system-name) "eruv2")
    (use-package material-theme
      :config
      ;; Use dark variant to match your Materia theme
      (load-theme 'material t)

      ;; Set cursor color to match Materia blue accent
      (set-cursor-color "#2196F3")

      ;; Customize specific faces to match Materia better
      (custom-set-faces
       ;; Mode line (status bar) to match Materia
       '(mode-line ((t (:background "#323232" :foreground "#FFFFFF" :box (:line-width 1 :color "#2196F3")))))
       '(mode-line-inactive ((t (:background "#262626" :foreground "#888888" :box (:line-width 1 :color "#555555")))))

       ;; Selection color to match Materia blue
       '(region ((t (:background "#2196F3" :foreground "#FFFFFF"))))

       ;; Fix any dark blue on dark background issues
       '(font-lock-comment-face ((t (:foreground "#8BC34A"))))
       '(font-lock-function-name-face ((t (:foreground "#82AAFF"))))
       '(font-lock-keyword-face ((t (:foreground "#C792EA"))))
       '(font-lock-string-face ((t (:foreground "#FFCB6B"))))
       '(font-lock-constant-face ((t (:foreground "#89DDFF"))))
       '(font-lock-variable-name-face ((t (:foreground "#EEFFFF"))))

       ;; Highlight current line
       '(hl-line ((t (:background "#37474F")))))
      :ensure t))

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
(setq etc-font-choice "Iosevka")

;; have to do this as a frame functon or daemon doesn't work
(defun etc-customize-frame (new-frame)
  (when (getenv "DISPLAY")
    (when (window-system new-frame) ;; daemon mode creates frame not associated w/ windowing system!
      (set-frame-font (font-spec :family etc-font-choice) t t)))
  )

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

;; Resize the whole frame, and not only a window
;; https://stackoverflow.com/a/60641769
(defun acg/zoom-frame (&optional amt)
  "Increaze FRAME font size by amount AMT. Defaults to selected
frame if FRAME is nil, and to 1 if AMT is nil."
  (interactive "p")
  (dolist (frame (frame-list))
    ;; emacs creates a weird extra frame that is never visible but
    ;; still shows up invisible frame list when you start emacs in
    ;; daemon mode
    (unless (string-equal "initial_terminal" (terminal-name frame))
      (message "%S" frame)
      (let* ((font (face-attribute 'default :font frame))
             (size (font-get font :size))
             (amt (or amt 1))
             (new-size (+ size amt)))
        (message "before")
        (set-frame-font (font-spec :size new-size) t `(,frame))
        (message "Frame's font new size: %d" new-size)
        (message "after")
        ))))

(defun acg/zoom-frame-out (&optional amt)
  "Call `acg/zoom-frame' with negative argument."
  (interactive "p")
  (acg/zoom-frame (- (or amt 1))))

(global-set-key (kbd "C-x C-+") 'acg/zoom-frame)
(global-set-key (kbd "C-x C--") 'acg/zoom-frame-out)
(global-set-key (kbd "<C-down-mouse-4>") 'acg/zoom-frame)
(global-set-key (kbd "<C-down-mouse-5>") 'acg/zoom-frame-out)

(dolist (frame (frame-list))
  (etc-customize-frame frame))
