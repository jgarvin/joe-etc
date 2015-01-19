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
         ;; the minimum call here shouldn't be necessary, some Unicode bug
         (space-left (min 0 (- width (string-width body-string) 4))))
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
  (while (< (window-body-height w) (+ target-height 1))
    (window-resize w 1))
  (while (> (window-body-height w) (+ target-height 1))
    (window-resize w -1)))

;; TODO: mysteriously minibuffer in other frames can't be
;; reduced to size 1, for no obvious reason
(defun md-update-belts ()
  (unless (or md-updating-belts
              (window-minibuffer-p)
              (minibuffer-prompt)
              (> (minibuffer-depth) 1))
    (let ((deactivate-mark nil)
          (inhibit-read-only t)
          (md-updating-belts t)
          (w (minibuffer-window))
          (buffer (current-buffer))
          (active-belt-count 0))
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
      (md-update-belts))))

(defadvice message (around md-message-save-to-var disable)
  (if (or md-updating-belts (not (ad-get-arg 0)))
      ad-do-it
    (let ((formatted-string (apply 'format (ad-get-args 0))))
      (when (or (stringp formatted-string))
        (setq md-current-message formatted-string)
        (md-update-belts))
      ad-do-it)))

(defun md-setup-belts ()
  (let ((md-updating-belts t))
    (dolist (belt md-belt-list)
      (when (md-belt-construct belt)
        (funcall (md-belt-construct belt))))  
    (setq resize-mini-windows nil)
    (add-hook 'post-command-hook #'md-save-message t)
    (add-hook 'post-command-hook #'md-update-belts t)
    (add-hook 'window-configuration-change-hook #'md-update-belts t)
    (add-hook 'focus-in-hook #'md-update-belts t)
    (ad-enable-advice 'message 'around 'md-message-save-to-var)
    (setq md-belt-mode t))
  (md-update-belts))

(defun md-hide-belts ()
  (let ((md-updating-belts t))
    (setq resize-mini-windows 'grow-only)
    (remove-hook 'post-command-hook #'md-save-message)
    (remove-hook 'post-command-hook #'md-update-belts)
    (remove-hook 'window-configuration-change-hook #'md-update-belts)
    (remove-hook 'focus-in-hook #'md-update-belts)
    ;; TODO: disable doesn't work wtf?
    (ad-disable-advice 'message 'around 'md-message-save-to-var)
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
   ((and (not arg) md-belt-mode) (md-hide-belts))
   ((not md-belt-mode) (md-setup-belts))))

(md-toggle-belt-mode t)
;; (md-toggle-belt-mode nil)

(provide 'md-belt-impl)

