(require 'cl) ;; remove-if-not

(defun md-insert-belt-text (text color)
  (put-text-property 0 (length text) (quote face) (list (quote :underline) t (quote :foreground) color) text)
  (put-text-property 0 (length text) (quote font-lock-face) (list (quote :underline) t (quote :foreground) color) text)
  (insert text))

(defun md-refresh-belt (window)
  (let ((inhibit-read-only t)
        (text "hello world")
        (color (window-parameter window (quote md-belt-color))))
    (erase-buffer)
    (md-insert-belt-text (concat text (make-string (- (window-total-width window) (length text) 2) 45)) color)
    (goto-char (point-max))))

(defun md-create-belt-window (frame name color)
  (unless (remove-if-not (lambda (w) (string= (buffer-name (window-buffer w)) name)) (window-list frame))
    (split-window (selected-window) 1 (quote above))
    (switch-to-buffer (get-buffer-create name))
    (let ((new-window (get-buffer-window)))
      (set-window-dedicated-p new-window t)
      (set-window-parameter new-window (quote no-other-window) t)
      (set-window-parameter new-window (quote delete-other-windows) (function (lambda (w) (message "delete-other-windows-executed") nil)))
      (set-window-parameter new-window (quote md-is-belt-window) t)
      (set-window-parameter new-window (quote md-refresh-belt-function) t)
      (set-window-parameter new-window (quote md-belt-color) color)
      (read-only-mode t)
      (md-refresh-belt new-window)
      (other-window 1))))

(defun md-window-configuration-hook ()
  (dolist (frame (frame-list))
    (md-create-belt-window frame "*nearest-belt*" "green")
    (md-create-belt-window frame "*recent-belt*" "yellow")
    (md-create-belt-window frame "*frequent-belt*" "red")
    (dolist (window (window-list frame))
      (when (window-parameter window (quote md-is-belt-window))
        (md-refresh-belt window)))))

;; (md-create-belt-window (selected-frame) "*nearest-belt*" "green")
;; (md-create-belt-window (selected-frame) "*recent-belt*" "yellow")
;; (md-create-belt-window (selected-frame) "*frequent-belt*" "red")

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
