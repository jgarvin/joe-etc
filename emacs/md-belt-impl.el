(require 'cl) ;; defstruct

(defvar md-belt-item-max 10)
(defvar md-current-message nil)
(defvar md-message-counter 0)
(defvar md-num-belts 3)
(defvar md-updating-belts nil)
(defvar md-belt-list nil "List of belts.")

;; TODO: font-lock-add-keywords
(cl-defstruct md-belt
  (construct nil :read-only t)
  (destruct nil :read-only t)
  (text nil :read-only t)
  (color nil :read-only t)
  (last-rendered-text ""))

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

(defun md-update-belts ()
  (unless md-updating-belts
    (let ((deactivate-mark nil)
          (inhibit-read-only t)
          (md-updating-belts t))
      (with-selected-window (minibuffer-window)
        (erase-buffer)
        (dolist (belt md-belt-list)
          (md-insert-belt-text (eval (md-belt-text belt)) (md-belt-color belt))
          (insert "\n"))
        (when md-current-message
          (insert md-current-message))
        (while (< (window-body-height) (+ (length md-belt-list) 1))
          (enlarge-window 1))
        (while (> (window-body-height) (+ (length md-belt-list) 1))
          (shrink-window 1))
        (goto-char (point-min))
        (message nil)))))

(defun md-save-message ()
  (let ((m (current-message)))
    (when m
      (setq md-current-message m)
      (md-update-belts))))

(defadvice message (around md-message-save-to-var disable)
  (if (not (ad-get-arg 0))
      ad-do-it
    (let ((formatted-string (apply 'format (ad-get-args 0))))
      (when (stringp formatted-string)
        (setq md-current-message formatted-string)
        (md-update-belts))
      ad-do-it)))

(defun md-setup-belt ()
  (dolist (belt md-belt-list)
    (funcall (md-belt-construct belt)))  
  (setq resize-mini-windows nil)
  (add-hook 'post-command-hook #'md-save-message t)
  (add-hook 'post-command-hook #'md-update-belts t)
  (add-hook 'window-configuration-change-hook #'md-update-belts t)
  (add-hook 'focus-in-hook #'md-update-belts t)
  (ad-enable-advice 'message 'around 'md-message-save-to-var))

(defun md-destroy-belt ()
  (setq resize-mini-windows 'grow-only)
  (remove-hook 'post-command-hook #'md-save-message)
  (remove-hook 'post-command-hook #'md-update-belts)
  (remove-hook 'window-configuration-change-hook #'md-update-belts)
  (remove-hook 'focus-in-hook #'md-update-belts)
  ;; TODO: disable doesn't work wtf?
  (ad-disable-advice 'message 'around 'md-message-save-to-var)
  (with-selected-window (minibuffer-window)
    (erase-buffer))
  (dolist (belt md-belt-list)
    (funcall (md-belt-destruct belt))))

;; (md-setup-belt)
;; (md-destroy-belt)

(provide 'md-belt-impl)
