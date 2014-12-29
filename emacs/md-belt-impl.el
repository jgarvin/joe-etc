(require 'cl) ;; defstruct

;; TODO: overwrite existing characters, don't erase the
;; whole buffer everytime. And only update things that
;; are actually different.

(defvar md-belt-item-max 8)
(defvar md-current-message nil)
(defvar md-message-counter 0)
(defvar md-num-belts 3)
(defvar md-updating-belts nil)
(defvar md-belt-list nil "List of belts.")

;; TODO: font-lock-add-keywords
(cl-defstruct md-belt
  name
  (construct nil :read-only t)
  (destruct nil :read-only t)
  (contents nil :read-only t)
  (old-contents nil)
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
         (usable-length (- max-length 4 (* 3 md-belt-item-max)))
         (length-per-item (/ usable-length md-belt-item-max))
         (cur-char (- ?A 1))
         (body-string (mapconcat (lambda (y)
                         (format
                          (format "%c %%-%ds" (incf cur-char) length-per-item)
                          (md-truncate-string y length-per-item))) items " "))
         (space-left (- width (length body-string) 4)))
    ;;(message "%d %d %d %d %d" space-left width max-length usable-length length-per-item)
    (concat "| "
            body-string
            (make-string space-left ?\ )
            " |")))
;; (md-build-belt-string md-nearest-belt-symbols)
;; (length (md-build-belt-string md-nearest-belt-symbols))

(defun md-activate-belt-item (belt-name c)
  (interactive)
  (setq c (upcase c))
  (let ((belt (car
               (remove-if-not
                (lambda (x)
                  (string= belt-name (md-belt-name x))) md-belt-list))))
    (md-insert-text (format "%s" (nth (- c ?A) (md-belt-old-contents belt))) t nil)))

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
          (let* ((new (subseq (eval (md-belt-contents belt)) 0 md-belt-item-max))
                 (old (md-belt-old-contents belt))
                 (new-sorted (md-preserve-position old new)))
            (md-insert-belt-text
             (md-build-belt-string new-sorted) (md-belt-color belt))
            (insert "\n")
            (setf (md-belt-old-contents belt) new-sorted)))
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

(defun md-destroy-all-belts ()
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
    (setq md-belt-list nil)
    (message "")))

(progn
  (md-setup-nearest-belt)
  (md-setup-belt))
;; (md-destroy-all-belts)

(provide 'md-belt-impl)

