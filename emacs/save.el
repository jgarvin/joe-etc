;disable backup
(setq backup-inhibited t)
;disable auto save
(setq auto-save-default nil)

;; builtin autosave randomly stops working for no reason,
;; so implement my own
(run-at-time "1 sec" 1
	     (lambda ()
	       (when (reduce (lambda (a b) (or a b))
		       (mapcar (lambda (b)
				 (and (buffer-modified-p b)
				      (not (null (buffer-file-name b)))))
			       (buffer-list)))
		 (save-some-buffers t nil))))

;; so I can't be tempted to do by hand
(global-unset-key "\C-x\C-s")

;; save when emacs loses focus
(when
   (and (featurep 'x) window-system)
 (defvar on-blur--saved-window-id 0 "Last known focused window.")
 (defvar on-blur--timer nil "Timer refreshing known focused window.")
 (defun on-blur--refresh ()
   "Runs on-blur-hook if emacs has lost focus."
   (let* ((active-window (x-window-property
                          "_NET_ACTIVE_WINDOW" nil "WINDOW" 0 nil t))
          (active-window-id (if (numberp active-window)
                                active-window
                              (string-to-number
                               (format "%x%04x"
                                       (car active-window)
                                       (cdr active-window)) 16)))
          (emacs-window-id (string-to-number
                            (frame-parameter nil 'outer-window-id))))
     (when (and
            (= emacs-window-id on-blur--saved-window-id)
            (not (= active-window-id on-blur--saved-window-id)))
       (run-hooks 'on-blur-hook))
     (setq on-blur--saved-window-id active-window-id)
     (run-with-timer 1 nil 'on-blur--refresh)))
 (add-hook 'on-blur-hook #'(lambda () (save-some-buffers t)))
 (on-blur--refresh))

;; make sure autosave calls after-save-hook, for some reason
;; doesn't by default
(defadvice do-auto-save (after after-auto-save activate)
  (run-hooks 'after-save-hook))
(defadvice save-some-buffers (after after-save-some-buffers activate)
  (run-hooks 'after-save-hook))

;; autosave under all these circumstances too, never want to save
;; manually
(defadvice switch-to-buffer (before save-buffer-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice other-window (before other-window-now activate)
  (when buffer-file-name (save-buffer)))
(defadvice other-frame (before other-frame-now activate)
  (when buffer-file-name (save-buffer)))

