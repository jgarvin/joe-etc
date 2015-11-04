;; strangely setting eshell-mode-map doesn't work
;; unless you are in an eshell-mode buffer.
(defun etc-eshell-mode-hook ()
  (eshell/addpath "~/etc/bin")
  (eshell/addpath "~/opt/bin")
  ;; make sure first folder is in history
  (push (eshell/pwd) eshell-hist-dirs)
  ;; for helm
  (require 'helm-eshell)
  (eshell-cmpl-initialize)
  (define-key eshell-mode-map [remap pcomplete] 'helm-esh-pcomplete)
  (define-key eshell-mode-map (kbd "M-r") 'helm-eshell-history)
  ;; for me
  (define-key eshell-mode-map (kbd "<home>") #'eshell-bol)
  (define-key eshell-mode-map (kbd "<up>") #'previous-line)
  (define-key eshell-mode-map (kbd "<down>") #'next-line))

(add-hook 'eshell-mode-hook #'etc-eshell-mode-hook)

;;*--- track cd history ------------------------------------------------*/
(defvar-local eshell-hist-dirs nil)
(defvar-local eshell-hist-index 0)

(defun etc-eshell-update-hist-dir ()
  (when (not (equal (car (last eshell-hist-dirs)) (eshell/pwd)))
    (push (eshell/pwd) eshell-hist-dirs)))

(add-hook 'eshell-directory-change-hook #'etc-eshell-update-hist-dir)

(defun eshell-forward (n)
  (unless eshell-hist-dirs
    (user-error "eshell-hist-dirs is empty, cd a few times"))
  (let ((dirs eshell-hist-dirs))
    (prog1 (eshell/cd (nth (setq eshell-hist-index
                                 (if (> n 0)
                                     (min (- (length eshell-hist-dirs) 1) (+ eshell-hist-index n))
                                   (max 0 (+ eshell-hist-index n))))
                           dirs))
      (setq eshell-hist-dirs dirs))))

(defun eshell/b ()
  (eshell-forward 1))

(defun eshell/f ()
  (eshell-forward -1))

(setq eshell-history-size 10000)

(push "htop" eshell-visual-commands)
(push "nethack" eshell-visual-commands)
(push "perf report" eshell-visual-commands)
(push "iotop" eshell-visual-commands)

