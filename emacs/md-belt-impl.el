(require 'cl) ;; defstruct
(require 'dash) ;; -map-indexed

(defvar md-belt-item-max 8)
(defvar md-current-message nil)
(defvar md-message-counter 0)
(defvar md-num-belts 3)
(defvar md-updating-belts nil)
(defvar md-belt-list nil "List of belts.")

;; TODO: font-lock-add-keywords
(cl-defstruct md-belt
  (construct nil :read-only t)
  (destruct nil :read-only t)
  (contents nil :read-only t)
  (color nil :read-only t))

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

(defun md-truncate-string (x max-length)
  (let ((trailing ".."))
    (if (< (length x) max-length)
        x
      (concat (substring x 0 (- max-length (length trailing))) trailing))))
;; (md-truncate-string "this is really long" 10)

(defun md-preserve-position (old new)
  (if (null old)
      new
    (let* ((result
            (mapcar
             (lambda (x)
               (if (member x new) x nil)) old))
           (new-items (set-difference new old :test 'equal)))
      (loop for x on result do
            (when (null (car x))
              (setcar x (pop new-items))))
      result)))
;;(md-preserve-position '(2 4 5 1) '(3 4 1 2))

(defun md-build-belt-string (x)
  (let* ((width (window-body-width (minibuffer-window)))
         (items (subseq x 0 md-belt-item-max))
         (max-length width)
         ;; The form is:
         ;; | foo | bar | buzz
         ;; So 2 characters at start and end make 4, and then 3 characters
         ;; for each separator between items.
         (usable-length (- max-length 4 (* 3 (- md-belt-item-max 1))))
         (length-per-item (/ usable-length md-belt-item-max))
         (body-string (mapconcat (lambda (y)
                         (format
                          (format "%%-%ds" length-per-item)
                          (md-truncate-string y length-per-item))) items " | "))
         (space-left (- width (length body-string) 4)))
    ;;(message "%d %d %d %d %d" space-left width max-length usable-length length-per-item)
    (concat "| "
            body-string
            (make-string space-left ?\ )
            " |")))
;; (md-build-belt-string md-nearest-belt-symbols)
;; (length (md-build-belt-string md-nearest-belt-symbols))

(defun md-update-belts ()
  (unless (or md-updating-belts
              (window-minibuffer-p))
    (let ((deactivate-mark nil)
          (inhibit-read-only t)
          (md-updating-belts t)
          (w (minibuffer-window)))
      (with-current-buffer (window-buffer w)
        (erase-buffer)
        (dolist (belt md-belt-list)
          (md-insert-belt-text
           (md-build-belt-string (eval (md-belt-contents belt))) (md-belt-color belt))
          (insert "\n"))
        (when md-current-message
          (insert md-current-message))
        (while (< (window-body-height w) (+ (length md-belt-list) 1))
          (window-resize w 1))
        (while (> (window-body-height w) (+ (length md-belt-list) 1))
          (window-resize w -1))
        (goto-char (point-min))
        (message nil)))))

(defun md-save-message ()
  (let ((m (current-message)))
    (when m
      (setq md-current-message m)
      (md-update-belts))))

(defadvice message (around md-message-save-to-var disable)
  (if (or md-updating-belts (not (ad-get-arg 0)))
      ad-do-it
    (let ((formatted-string (apply 'format (ad-get-args 0))))
      (when (stringp formatted-string)
        (setq md-current-message formatted-string)
        (md-update-belts))
      ad-do-it)))

(defun md-setup-belt ()
  (let ((md-updating-belts t))
    (dolist (belt md-belt-list)
      (funcall (md-belt-construct belt)))  
    (setq resize-mini-windows nil)
    (add-hook 'post-command-hook #'md-save-message t)
    (add-hook 'post-command-hook #'md-update-belts t)
    (add-hook 'window-configuration-change-hook #'md-update-belts t)
    (add-hook 'focus-in-hook #'md-update-belts t)
    (ad-enable-advice 'message 'around 'md-message-save-to-var)))

(defun md-destroy-belt ()
  (let ((md-updating-belts t))
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
      (funcall (md-belt-destruct belt)))
    (message "")))

(progn
  (md-setup-nearest-belt)
  (md-setup-belt))
;; (md-destroy-belt)

(provide 'md-belt-impl)

