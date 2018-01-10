(require 'dash)

(defvar md-cached-buffer-list nil)
(defvar md-cached-special-buffer-list nil)
(defvar md-cached-buffer-time '(0 0 0 0))
(defvar md-cached-buffers-by-mode (make-hash-table :test 'equal))
(defvar md-cached-buffers-timer nil)

(defun md-update-buffer-lists-impl ()
    (let ((list-changes (-difference (buffer-list) md-cached-buffer-list)))
      (when list-changes
        ;; (message "testing thing %S %S" (length (buffer-list))
                 ;; (-difference (buffer-list) md-cached-buffer-list))
        (setq md-cached-special-buffer-list (md-get-special-buffers-impl))
        (setq md-cached-buffers-by-mode (md-build-mode-cache))
        )
      (setq md-cached-buffer-list (buffer-list))
      ))

(defun derived-mode-parents (mode)
  (and mode
       (cons mode (derived-mode-parents
                   (get mode 'derived-mode-parent)))))

(defun md-build-mode-cache ()
  (let ((cache (make-hash-table :test 'equal)))
    (dolist (buffer (buffer-list))
      (dolist (mode (derived-mode-parents (buffer-mode buffer)))
        (let ((buffers-in-mode (gethash mode cache)))
          (if buffers-in-mode
              (puthash mode (append buffers-in-mode (list buffer)) cache)
            (puthash mode (list buffer) cache))
          ))
      )
    cache))
;; (md-build-mode-cache)

(defun md-update-buffer-lists ()
  (md-run-when-idle-once 'md-cached-buffers-timer #'md-update-buffer-lists-impl 0.75 nil))

(add-hook 'buffer-list-update-hook #'md-update-buffer-lists)



(defun md-strip-text-properties (txt)
  (set-text-properties 0 (length txt) nil txt)
  txt)

(defun md-get-buffers-in-modes (modes)
  (unless (listp modes)
    (setq modes (list modes)))
  (let ((buffers))
    (dolist (mode modes)
      (setq buffers (append buffers (gethash mode md-cached-buffers-by-mode))))
    buffers))
(byte-compile #'md-get-buffers-in-modes)

(defun md-get-buffers-not-in-modes (modes)
  (unless (listp modes)
    (setq modes (list modes)))
  (-filter (lambda (x)
             (with-current-buffer x
               (not (apply #'derived-mode-p modes)))) (buffer-list)))
(byte-compile #'md-get-buffers-not-in-modes)

(defun md-buffer-name (&optional buffer)
  (md-strip-text-properties (buffer-name buffer)))

(defun md-special-buffer-p (x)
  (or (minibufferp x)
      (string-match-p " ?\\*[^*]+\\*" (md-buffer-name x))))
(byte-compile #'md-special-buffer-p)

;;(md-special-buffer-p (get-buffer "*Find*"))

(defun md-get-special-buffers ()
  md-cached-special-buffer-list)

(defun md-get-special-buffers-impl ()
  (-filter (lambda (a) (and (not (minibufferp a))
                            (buffer-live-p a)
                            (md-special-buffer-p a)))
           (buffer-list)))

(defun md-get-buffer-names (lst)
  (mapcar #'buffer-name lst))
(byte-compile #'md-get-buffer-names-impl)

(defun md-all-buffers-except (bufs)
  (-filter (lambda (x) (and (not (memq x bufs))
                            (buffer-live-p x)
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
