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
(defvar md-previous-belt-text nil)
(defvar md-belt-mode nil)
(defvar md-bt-timer nil)

;; TODO: font-lock-add-keywords
(cl-defstruct md-belt
  name
  (construct nil :read-only t)
  (destruct nil :read-only t)
  (contents nil :read-only t)
  (old-contents nil)
  (color nil :read-only t)
  (context t :read-only t))

;; (defun md-insert-belt-text (text color)
;;   (put-text-property 0 (length text) 'face `(:underline t :foreground ,color) text)
;;   (put-text-property 0 (length text) 'font-lock-face `(:underline t :foreground ,color) text)
;;   ;; (put-text-property 0 (length text) 'face `(:foreground ,color) text)
;;   ;; (put-text-property 0 (length text) 'font-lock-face `(:foreground ,color) text)
;;   (put-text-property 0 (length text) 'read-only t text)
;;   (put-text-property 0 (length text) 'intangible t text)
;;   (insert text))

(defun md-insert-belt-text (text color)
  (put-text-property 0 (length text) 'face `(:underline t :foreground ,color) text)
  (put-text-property 0 (length text) 'font-lock-face `(:underline t :foreground ,color) text)
  (let ((o (make-overlay (point) (point))))
    (overlay-put o 'after-string text)
    (overlay-put o 'window (minibuffer-window))))

(defun md-truncate-string (x max-length)
  (if (null x)
      "nil"
    ;; protect against arbitrarily long items
    ;; we could shorten to max length here but then we may make
    ;; some strings needlessly short that would have fit
    ;; once spaces were squeezed
    (setq x (substring x 0 (min (length x) 100)))
    (setq x (replace-regexp-in-string "[\\\n[:space:]]+" " " x))
    (let ((trailing ".."))
      (if (< (string-width x) max-length)
          x
        (concat (substring x 0 (- max-length (length trailing))) trailing)))))
;; (md-truncate-string "this is really long" 10)

(defun md-preserve-position (old new)
  ;; (setq old (delete-if #'null old))
  ;; (setq new (delete-if #'null new))
  (let ((len (max (length old) (length new))))
    (setq old (subseq old 0 len))
    (setq new (subseq new 0 len))
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
        ;; there may not be enough new items to fill the holes
        (setq result (delete-if #'null result))
        result))))
;;(md-preserve-position '(2 4 5 1) '(3 4 1 2))
;;(md-preserve-position '(nil nil nil 2 4 5 1) '(3 4 1 2))

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
         ;; the maximum call here shouldn't be necessary, some Unicode bug
         (space-left (max 0 (- width (string-width body-string) 4))))
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

(defun md-resize-minibuf (w target-height)
  (condition-case nil
      (while (< (window-body-height w) (+ target-height 1))
        (window-resize w 1))
    (args-out-of-range nil))
  (condition-case nil
      (while (> (window-body-height w) (+ target-height 1))
        (window-resize w -1))
    (args-out-of-range nil)))

(defun md-bt-inhibit-belt-update ()
  (or md-updating-belts
      (window-minibuffer-p)
      (minibuffer-prompt)
      (> (minibuffer-depth) 0)))

;; TODO: mysteriously minibuffer in other frames can't be
;; reduced to size 1, for no obvious reason
(defun md-update-belts ()
  (md-bt-cancel-timer)
  (unless (md-bt-inhibit-belt-update)
    ;;(message "md-updating-belts")
    (let ((deactivate-mark nil)
          (inhibit-read-only t)
          (md-updating-belts t)
          (w (minibuffer-window))
          (buffer (current-buffer))
          (active-belt-count 0))
      (md-server-log (format "prompt: %S" (minibuffer-prompt)))
      (with-current-buffer (window-buffer w)
        (erase-buffer)
        (dolist (o (overlays-in (point-min) (point-max)))
          (delete-overlay o))
        (dolist (belt md-belt-list)
          ;;(message (md-belt-context belt))
          (when (with-current-buffer buffer (eval (md-belt-context belt)))
            (incf active-belt-count)
            (let* ((contents (with-current-buffer buffer (eval (md-belt-contents belt))))
                   (new (subseq contents 0 (min (length contents) md-belt-item-max)))
                   (old (md-belt-old-contents belt))
                   (new-sorted (md-preserve-position old new)))
              (md-insert-belt-text
               (concat (md-build-belt-string new-sorted) "\n") (md-belt-color belt))
              (setf (md-belt-old-contents belt) new-sorted))))
        ;; (insert " ")
        ;; (when md-current-message
        ;;   (md-insert-belt-text md-current-message "white"))
        (when md-current-message
          ;; Apparently at least one character must be in buffer for overlays to show,
          ;; so may as well keep this as text in buffer
          (insert md-current-message))
        (dolist (frame (frame-list))
          (if (eq frame (selected-frame))
              (md-resize-minibuf w active-belt-count)
            (md-resize-minibuf (minibuffer-window frame) 1)))
        (goto-char (point-min))
        (message nil)))))

(defun md-save-message ()
  (let ((m (current-message)))
    (when m
      (setq md-current-message m)
      (md-bt-schedule-update))))

(defun md-belt-message-advice (original-function &rest args)
  (if (or md-updating-belts (not (car args)))
      (apply original-function args)
    (let ((formatted-string (apply #'format args)))
      (if (or (stringp formatted-string))
          (progn
            (setq md-current-message formatted-string)
            (md-bt-schedule-update))
        (apply original-function args)))))

(defun md-bt-update-post-command (&rest args)
  ;;(message "md-bt-update-post-command")
  (md-bt-schedule-update)
  ;;(message nil)
  )

(defun md-bt-update-focus (&rest args)
  ;;(message "md-bt-update-focus")
  (md-bt-schedule-update))

(defun md-bt-update-window (&rest args)
  ;;(message "md-bt-update-window")
  (md-bt-schedule-update))

(defun md-bt-schedule-update (&rest unused)
  (unless (md-bt-inhibit-belt-update)
    (md-run-when-idle-once 'md-bt-timer #'md-update-belts 0.25 nil)))

(defun md-bt-cancel-timer ()
  (when md-bt-timer
    (cancel-timer md-bt-timer)
    (setq md-bt-timer nil)))

(defun md-setup-belts ()
  (md-bt-cancel-timer)
  (let ((md-updating-belts t))
    (dolist (belt md-belt-list)
      (when (md-belt-construct belt)
        (funcall (md-belt-construct belt))))
    (setq resize-mini-windows nil)
    (add-hook 'post-command-hook #'md-save-message t)
    (add-hook 'post-command-hook #'md-bt-update-post-command t)
    (add-hook 'window-configuration-change-hook #'md-bt-update-window t)
    (add-hook 'focus-in-hook #'md-bt-update-focus t)
    (advice-add #'message :around #'md-belt-message-advice)
    (setq md-belt-mode t))
  (md-update-belts))

(defun md-hide-belts ()
  (md-bt-cancel-timer)
  (let ((md-updating-belts t))
    (setq resize-mini-windows 'grow-only)
    (advice-remove #'message #'md-belt-message-advice)
    (remove-hook 'post-command-hook #'md-save-message)
    (remove-hook 'post-command-hook #'md-bt-update-post-command)
    (remove-hook 'window-configuration-change-hook #'md-bt-update-window)
    (remove-hook 'focus-in-hook #'md-bt-update-focus)
    (advice-remove 'message #'md-belt-message-advice)
    (dolist (frame (frame-list))
      (with-selected-frame frame
        (with-selected-window (minibuffer-window frame)
          (with-current-buffer (window-buffer)
            (let ((inhibit-read-only t))
              (erase-buffer))))))
    (dolist (belt md-belt-list)
      (when (md-belt-destruct belt)
        (funcall (md-belt-destruct belt))))
    (message "")
    (setq md-belt-mode nil)))

;; TODO: use text properties to ensure we only delete text we inserted
(defun md-destroy-all-belts ()
  (md-hide-belts)
  (setq md-belt-list nil))
;;(md-destroy-all-belts)

(defun md-toggle-belt-mode (&optional arg)
  (interactive)
  (cond
   ((and (or (not arg) (= arg 0)) md-belt-mode) (md-hide-belts))
   ((and (or (not arg) (= arg 1)) (not md-belt-mode)) (md-setup-belts))))

;;(md-toggle-belt-mode 1)
;;(md-toggle-belt-mode 0)

;;(md-toggle-belt-mode)

(provide 'md-belt-impl)
