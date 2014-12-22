(require 'cl) ;; remove-if-not

(defvar md-belt-item-max 10)
(defvar-local md-nearest-symbols nil)

(defun md-get-next-symbol (dir)
  (let ((sym-start)
        (end (if dir (point-max) (point-min)))
        (search-function (if dir 're-search-forward 're-search-backward))
        (open-regex (if dir "\\_<" "\\_>"))
        (close-regex (if dir "\\_>" "\\_<")))
    (funcall search-function open-regex end)
    (setq sym-start (point))
    (funcall search-function close-regex end)
    (cons sym-start (point))))
    
(defun md-get-nearest-symbols ()
  (save-excursion
    (let ((symbol-list)
          (reverse-point (point))
          (forward-point (point))
          (next-symbol)
          (previous-symbol))
      (while (and (< (length symbol-list) md-belt-item-max)
                  (or (> reverse-point (point-min))
                      (< forward-point (point-max))))
        (goto-char forward-point)
        (setq next-symbol (md-get-next-symbol t))
        (goto-char reverse-point)
        (setq previous-symbol (md-get-next-symbol nil))
        (let* ((next-start (car next-symbol))
               (next-end (cdr next-symbol))
               (previous-start (car previous-symbol))
               (previous-end (cdr previous-symbol))
               (next-symbol-str (buffer-substring-no-properties next-start next-end))
               (previous-symbol-str (buffer-substring-no-properties previous-start previous-end)))
          (setq forward-point next-end)
          (setq reverse-point previous-end)
          (message "%s %s" next-symbol-str previous-symbol-str)
          (setq symbol-list (nconc symbol-list (list (unless (md-filter-symbol next-symbol-str next-start next-end) next-symbol-str)
                                                     (unless (md-filter-symbol previous-symbol-str previous-start previous-end) previous-symbol-str))))
          (setq symbol-list (delete-if-not 'identity symbol-list))
          (delete-dups symbol-list)))
      symbol-list)))

(defun md-update-nearest-symbols ()
  (when (not (window-parameter (get-buffer-window) 'md-is-belt-window))
    (setq md-nearest-symbols (md-get-nearest-symbols))))

;; TODO: post-command-hook

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

(defun md-create-belt-window (frame belt-type color)
  (let ((belt-name (format "*%S-%S*" belt-type (frame-parameter frame 'md-belt-id)))
        (original-frame (selected-frame)))
    (unless (remove-if-not (lambda (w) (string= (buffer-name (window-buffer w)) belt-name)) (window-list frame))
      (unwind-protect
          (progn
            (select-frame frame)
            (split-window (car (window-list frame 1)) 1 'above)
            (switch-to-buffer (get-buffer-create belt-name))
            (let ((new-window (get-buffer-window)))
              (set-window-dedicated-p new-window t)
              (set-window-parameter new-window 'no-other-window t)
              (set-window-parameter new-window 'md-is-belt-window t)
              (set-window-parameter new-window 'md-belt-type belt-type)
              (set-window-parameter new-window 'md-belt-color color)
              (read-only-mode t)
              (md-refresh-belt new-window)
              (other-window 1)))
        (select-frame original-frame)))))

(defun md-frame-setup (frame)
  (unless (frame-parameter frame 'md-belt-id)
    (set-frame-parameter frame 'md-belt-id (gensym)))
  (md-create-belt-window frame 'frequent "red")
  (md-create-belt-window frame 'recent "yellow")
  (md-create-belt-window frame 'nearest "green")
  (dolist (window (window-list frame))
    (when (window-parameter window 'md-is-belt-window)
      (md-refresh-belt window))))
    
;;(md-get-nearest-symbols)

(defun md-destroy-belts ()
  (let ((bad-windows))
    (dolist (frame (frame-list))
      (dolist (window (window-list frame 1))
        (when (window-parameter window 'md-is-belt-window)
          (setq bad-windows (cons window bad-windows)))))
    (dolist (w bad-windows)
      (delete-window w))))

;;(md-destroy-belts)

(defun md-window-configuration-hook ()
  (dolist (frame (frame-list))
    (md-frame-setup frame)))
;;(window-parameter (selected-window) 'md-is-belt-window)

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

(defun md-update-belts ()
  (with-selected-window (minibuffer-window)
    (when resize-mini-windows
      (setq resize-mini-windows nil))
    (erase-buffer)
    (insert "one\ntwo\nthree\n")
    (insert etc-last-message)
    (while (< (window-height) 4)
      (enlarge-window 1))
    (while (> (window-height) 4)
      (shrink-window 1))
    (goto-char (point-min))
    (message nil)))
;;(md-update-belts)
;;(message "tohetestetttttttehhehhehehhhehhehett")

(with-selected-window (minibuffer-window)
  (erase-buffer))
(setq resize-mini-windows nil)
(setq resize-mini-windows 'grow-only)


(let ((belt-start)
      (belt-end)
      (w (selected-window)))
  (setq belt-start
        (save-excursion
          (goto-char (window-end))
          (previous-line 3)
          (beginning-of-line)
          (point)))
  (setq belt-end (window-end))
  (setq belt-overlay (make-overlay belt-start belt-end))
  (overlay-put belt-overlay 'window w)
  (overlay-put belt-overlay 'intangible t)
  (overlay-put belt-overlay 'face (list :underline t :foreground "red" :background "blue"))
  )

(+ (line-number-at-pos (window-start)) (window-body-height))

(defvar md-debug-winend-overlay nil)

(defun md-debug-setup-winend-overlay (&optional start)
  (unless start
    (setq start (window-start)))
  (when md-debug-winend-overlay
    (delete-overlay md-debug-winend-overlay))
  (let* ((start (save-excursion
                  (goto-char (md-get-real-window-end start))
                  (point-at-bol)))
         (end (save-excursion
                (goto-char (md-get-real-window-end start))
                (point-at-eol))))
    (message "%s %s" (line-number-at-pos (window-start)) (line-number-at-pos (window-end)))
    (setq md-debug-winend-overlay (make-overlay start end))
    (overlay-put md-debug-winend-overlay 'window (selected-window))
    (overlay-put md-debug-winend-overlay 'face (list :background "red"))))

(defun md-get-real-window-end (&optional start)
  (unless start
    (setq start (window-start)))
  (save-excursion
    (goto-line (+ (line-number-at-pos start) (window-body-height)))
    (while (not (pos-visible-in-window-p (point)))
      (previous-line))
    (point-at-eol)))
(md-get-real-window-end)

(defun md-scroll-hook (w new-start)
  (md-debug-setup-winend-overlay new-start)
  )

;;(md-scroll-hook nil (window-start))

;;(add-hook 'window-scroll-functions 'md-scroll-hook)
;;(remove-hook 'window-scroll-functions 'md-scroll-hook)
;;(add-hook 'post-command-hook 'md-debug-setup-winend-overlay)
;;(remove-hook 'post-command-hook 'md-debug-setup-winend-overlay)
;;(md-debug-setup-winend-overlay)
;;(remove-overlays)

(let ((belt-start)
      (belt-end)
      (w (selected-window))
      (belt-text)
      (num-belts 3))
  (setq belt-start
        (save-excursion
          (if (>= (+ (line-number-at-pos (window-start)) (window-body-height)) (line-number-at-pos (window-end)))
              (progn
                (message "route1 %d %d %d" (line-number-at-pos (window-start)) (line-number-at-pos (window-end)) (+ (line-number-at-pos (window-start)) (window-body-height)))
                (goto-char (point-max))
                (beginning-of-line)
                (point))
            (progn
              (message "route2")
              (goto-char (window-end))
              (previous-line num-belts)
              (beginning-of-line)
              (point)))))
  ;; (setq belt-end (window-end))
  
  ;; (setq belt-barrier-overlay (make-overlay belt-start belt-start))
  ;; (overlay-put belt-barrier-overlay 'window w)
  ;; (overlay-put belt-barrier-overlay 'intangible t)
  ;; (setq belt-barrier-text (concat (make-string (- (window-body-width w) 2) ? ) "\n"))
  ;; ;;(setq belt-barrier-text (concat (make-string (- (window-body-width w) 2) ?-) ""))
  ;; (message "wft1")
  ;; (put-text-property 0 (length belt-barrier-text) 'face (list belt-text :foreground "black" :background "black" :underline "red" :overline "red") belt-barrier-text)
  ;; (message "wft2")
  ;; (setq belt-text (concat (make-string (- (window-body-width w) (length "") 2) ?-) "\n"))
  ;; (setq belt-text (concat belt-text (make-string (- (window-body-width w) (length "") 2) ?-) "\n"))
  ;; (setq belt-text (concat belt-text (make-string (- (window-body-width w) (length "") 2) ?-) "\n"))
  ;; (message "wft3")
  ;; (put-text-property 0 (length belt-text) 'face (list belt-text :foreground "red" :background "blue" :overline "red") belt-text)
  ;; (message "wft4")
  ;; ;;(setq belt-final-text (concat belt-barrier-text belt-text belt-barrier-text))
  ;; ;;(setq belt-final-text (concat belt-barrier-text belt-text))
  ;; (setq belt-final-text (concat belt-text))
  ;; (overlay-put belt-barrier-overlay 'after-string belt-final-text)

  ;; ;;(setq belt-start (save-excursion (goto-char belt-start) (next-line) (point)))
  ;; ;;(setq belt-start (save-excursion (goto-char belt-start) (next-line) (beginning-of-line) (point)))
  ;; (setq belt-start (+ 1 belt-start))
  ;; (setq belt-overlay (make-overlay belt-start belt-start))
  ;; (overlay-put belt-overlay 'window w)
  ;; (overlay-put belt-overlay 'intangible t)
  ;; (setq belt-text (concat "" (make-string (- (window-body-width w) (length "") 2) ?-)))
  ;; ;;(setq belt-text "one\n to\n 3\n\n\n\n")
  ;; (put-text-property 0 (length belt-text) 'face (list belt-text :underline t :foreground "red" :background "blue") belt-text)
  ;; (overlay-put belt-overlay 'after-string belt-text)
  ;;(overlay-put belt-overlay 'face )
  nil
  )


;;(remove-overlays)

;; adding as hook breaks mark?!
;;(remove-hook 'etc-message-hook 'md-update-belts)

