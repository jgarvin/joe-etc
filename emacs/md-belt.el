(require 'cl) ;; remove-if-not

(defun md-insert-belt-text (text color)
  (put-text-property 0 (length text) 'face `(:underline t :foreground ,color) text)
  (put-text-property 0 (length text) 'font-lock-face `(:underline t :foreground ,color) text)
  (insert text))

(defun md-refresh-belt (window)
  (save-window-excursion
    (select-window window)
    (let ((inhibit-read-only t)
          (text "hello world")
          (color (window-parameter window (quote md-belt-color))))
      (erase-buffer)
      (md-insert-belt-text (concat text (make-string (- (window-total-width window) (length text) 2) ?-)) color)
      (goto-char (point-max)))))

(defun md-create-belt-window (frame name color)
  (unless (remove-if-not (lambda (w) (string= (buffer-name (window-buffer w)) name)) (window-list frame))
    (split-window (selected-window) 1 (quote above))
    (switch-to-buffer (get-buffer-create (concat "*" name "-" (format "%S" (frame-parameter frame 'md-belt-id)) "*")))
    (let ((new-window (get-buffer-window)))
      (set-window-dedicated-p new-window t)
      (set-window-parameter new-window 'no-other-window t)
      (set-window-parameter new-window 'md-is-belt-window t)
      (set-window-parameter new-window 'md-refresh-belt-function t)
      (set-window-parameter new-window 'md-belt-color color)
      (read-only-mode t)
      (md-refresh-belt new-window)
      (other-window 1))))

(defun md-frame-setup (frame)
  (unless (frame-parameter frame 'md-belt-id)
    (set-frame-parameter frame 'md-belt-id (gensym)))
  (md-create-belt-window frame "frequent-belt" "red")
  (md-create-belt-window frame "recent-belt" "yellow")
  (md-create-belt-window frame "nearest-belt" "green")
  (dolist (window (window-list frame))
    (when (window-parameter window 'md-is-belt-window)
      (md-refresh-belt window))))

;;(window-parameter (selected-window) 'md-is-belt-window)

(defun md-window-configuration-hook ()
  (dolist (frame (frame-list))
    (md-frame-setup frame)))

;;(md-frame-setup (selected-frame))

;; (frame-list)
;; (md-create-belt-window (selected-frame) "*frequent-belt*" "red")
;; (md-create-belt-window (selected-frame) "*recent-belt*" "yellow")
;; (md-create-belt-window (selected-frame) "*nearest-belt*" "green")

;;(md-delete-other-windows t)
;; (insert (format "%S"(symbol-function 'md-delete-other-windows)))

(defun md-delete-other-windows (arg)
  "Delete other windows as long as they're not dedicated, unless a prefix is provided."
  (interactive "P")
  (dolist (window (window-list))
    (when (not (eq window (selected-window)))
      (when (or arg (not (window-parameter window (quote md-is-belt-window))))
        (delete-window window)))))

(global-set-key (kbd "C-x 1") 'md-delete-other-windows)
  
;;(add-hook 'window-configuration-change-hook 'md-window-configuration-hook)
;;(remove-hook 'window-configuration-change-hook 'md-window-configuration-hook)
;;(add-hook 'after-make-frame-functions 'md-window-configuration-hook)
;;(remove-hook 'after-make-frame-functions 'md-window-configuration-hook)
