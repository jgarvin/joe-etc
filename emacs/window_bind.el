(defvar etc-buffer-window-binding nil
  "A list of cons cells where each cell contains a buffer name, its associated window, and frame.")

(defun etc-bind-buffer-to-window ()
  "Bind the current buffer to the current window."
  (interactive)
  (let ((buffer (current-buffer))
        (window (selected-window))
        (frame (selected-frame)))
    (setq etc-buffer-window-binding (cons (cons (buffer-name buffer) (cons window frame)) etc-buffer-window-binding))
    (message "Bound buffer '%s' to window in frame." (buffer-name buffer))))

(defun etc-release-buffer-binding ()
  "Release the binding of the current buffer."
  (interactive)
  (let ((buffer-name (buffer-name (current-buffer))))
    (setq etc-buffer-window-binding
          (assq-delete-all buffer-name etc-buffer-window-binding))
    (message "Released binding for buffer '%s'." buffer-name)))

(defun etc-get-smallest-window-in-frame (frame)
  "Return the smallest window in FRAME."
  (let ((smallest-window nil)
        (smallest-size most-positive-fixnum))
    (dolist (win (window-list frame))
      (let ((win-size (* (window-width win) (window-height win))))
        (when (< win-size smallest-size)
          (setq smallest-size win-size)
          (setq smallest-window win))))
    smallest-window))

(defvar etc-buffer-window-binding nil
  "A list of cons cells where each cell contains a buffer name, its associated window, and frame.")

(defun etc-switch-to-bound-window (orig-fun buffer-or-name &rest args)
  "Switch to the window bound to BUFFER-OR-NAME if it is bound."
  (let* ((buffer (get-buffer buffer-or-name))
         (buffer-name (buffer-name buffer))
         (binding (assoc buffer-name etc-buffer-window-binding)))
    (if (and binding buffer)
        (let* ((window (car (cdr binding)))
               (frame (cdr (cdr binding))))
          (if (and (window-live-p window) (eq (window-buffer window) buffer))
              (apply orig-fun buffer-or-name args)
            (if (window-live-p window)
                (progn
                  (select-window window)
                  (set-window-buffer window buffer))
              (when (frame-live-p frame)
                (let ((new-window (etc-get-smallest-window-in-frame frame)))
                  (when new-window
                    (setf (cdr binding) (cons new-window frame))
                    (select-window new-window)
                    (set-window-buffer new-window buffer))))
              (apply orig-fun buffer-or-name args))))
      (apply orig-fun buffer-or-name args))))

;; Remove old advice if it exists
(advice-remove 'switch-to-buffer #'etc-switch-to-bound-window)
(advice-remove 'pop-to-buffer #'etc-switch-to-bound-window)
(advice-remove 'display-buffer #'etc-switch-to-bound-window)

;; Add new advice
(advice-add 'switch-to-buffer :around #'etc-switch-to-bound-window)
(advice-add 'pop-to-buffer :around #'etc-switch-to-bound-window)
(advice-add 'display-buffer :around #'etc-switch-to-bound-window)

;; Bind the custom functions to key shortcuts
(global-set-key (kbd "C-c w b") 'etc-bind-buffer-to-window)
(global-set-key (kbd "C-c w u") 'etc-release-buffer-binding)
