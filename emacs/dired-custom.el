(require 'dired)

(use-package async :ensure t)

(dired-async-mode 0)

(defun etc-dired-hook ()
  ;;(auto-revert-mode)
  (dired-hide-details-mode 1)
  )

(add-hook 'dired-mode-hook #'etc-dired-hook)

(defun dired-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 3))

(defun dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

(defun dired-xdg-open-file ()
  "Opens the current file in a Dired buffer."
  (interactive)
  (xdg-open-file (dired-get-file-for-visit)))

(defun xdg-open-file (filename)
  "xdg-opens the specified file."
  (interactive "fFile to open: ")
  (let ((process-connection-type nil))
    (start-process "" nil "/usr/bin/xdg-open" filename)))

(defun etc-open-dired ()
  (interactive)
  ;;(dired (file-name-directory (buffer-file-name)))
  (dired (file-name-directory default-directory))
  )

(global-set-key (kbd "C-x C-j") #'etc-open-dired)

(define-key dired-mode-map (kbd "e") 'dired-xdg-open-file)

(setq image-dired-external-viewer "/usr/bin/xdg-open")

;; going all the way to the real/top bottom of buffer in dired
;; is almost never what you want.
(define-key dired-mode-map
  (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)
(define-key dired-mode-map
  (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)

(load-file "~/etc/emacs/dired-copy-paste.el")
(require 'dired-copy-paste)
(define-key dired-mode-map "\C-c\M-w" 'dired-copy-paste-do-copy)
(define-key dired-mode-map "\C-c\C-c" nil)

