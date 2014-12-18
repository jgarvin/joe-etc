(require 'dired+)


(defun dired-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 3))

(defun dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

;; going all the way to the real/top bottom of buffer in dired
;; is almost never what you want.
(define-key dired-mode-map
  (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)
(define-key dired-mode-map
  (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)

;; ;; make directory view auto-refresh
;; (add-hook 'dired-mode-hook
;; 	  (lambda ()
;; 	    (auto-revert-mode)))
