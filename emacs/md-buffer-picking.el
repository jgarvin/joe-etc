(require 'dash)

(defun md-strip-text-properties (txt)
  (set-text-properties 0 (length txt) nil txt)
  txt)

(defun md-get-buffers-in-modes (modes)
  (unless (listp modes)
    (setq modes (list modes)))
  (-filter (lambda (x)
             (with-current-buffer x
               (apply #'derived-mode-p modes))) (buffer-list)))
(byte-compile #'md-get-buffers-in-modes)

(defun md-get-buffers-not-in-modes (modes)
  (unless (listp modes)
    (setq modes (list modes)))
  (-filter (lambda (x)
             (with-current-buffer x
               (not (apply #'derived-mode-p modes)))) (buffer-list)))
(byte-compile #'md-get-buffers-not-in-modes)

(defun md-get-special-buffers ()
  (-filter
   (lambda (x) (and (not (minibufferp x))
                    (string-match-p " ?\\*[^*]+\\*" (md-buffer-name x)))) (buffer-list)))

(defun md-buffer-name (&optional buffer)
  (md-strip-text-properties (buffer-name buffer)))

(defun md-get-buffer-names (lst)
  (mapcar #'buffer-name lst))
(byte-compile #'md-get-buffer-names)

(defun md-all-buffers-except (bufs)
  (-filter (lambda (x) (and (not (memq x bufs))
                            (not (minibufferp x)))) (buffer-list)))
(byte-compile #'md-all-buffers-except)

(defun md-invisible-buffers ()
  (-filter (lambda (x)
             (not (get-buffer-window x t))) (buffer-list (selected-frame))))

(defun md-switch-to-next-buffer-in-list (lst)
  (interactive)
  (let ((buffs (-intersection (md-invisible-buffers) lst)))
    (setq buffs (delq (current-buffer) buffs))
    (if buffs
        (switch-to-buffer (car buffs) t)
      (switch-to-buffer nil t))))

(defun md-switch-to-next-mode-buffer (modes)
  (interactive)
  (md-switch-to-next-buffer-in-list (md-get-buffers-in-modes modes)))

;; The md-swich-to-next-* functions don't change the buffer list
;; because otherwise they would just toggle between two buffers
;; instead of cycling through them.
;; .. or they SHOULD behave that way, but it appears buffers get
;; recorded regardless, and we still end up just cycling between
;; two? this could be useful later if we figure out why...
;; (defun md-push-buffer-into-list (beg end len)
;;   (unless (or buffer-read-only
;;               (eq (current-buffer) (car (buffer-list)))
;;               (not (eq (current-buffer) (window-buffer (selected-window))))
;;               (not (minibufferp))
;;               ;; by filtering insertions of read only text we avoid
;;               ;; mistaking log updates and others' irc chat messages
;;               ;; as being a user triggered modification
;;               (not (text-property-not-all beg end 'read-only t)))
;;     ;; this is borrowed from #'switch-to-buffer and is
;;     ;; how it moves the buffer to the head of the list,
;;     ;; roundabout but just following precedent...
;;     (select-window (selected-window))))
;; (byte-compile #'md-push-buffer-into-list)

;;(add-hook 'after-change-functions #'md-push-buffer-into-list t)

(defun md-folder-switch ()
  (interactive)
  (if (derived-mode-p 'dired-mode)
      (md-switch-to-next-mode-buffer 'dired-mode)
    (dired-jump)))

(global-set-key (kbd "C-c C-j") #'md-folder-switch)

;;(md-get-buffers-in-modes 'dired-mode)
