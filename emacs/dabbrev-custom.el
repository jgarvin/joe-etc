(defun etc-dabbrev-friend-buffer (other-buffer)
  (and 
   (< (buffer-size other-buffer) (* 1 1024 1024))
   (not (md-special-buffer-p other-buffer))))

(setq dabbrev-friend-buffer-function #'etc-dabbrev-friend-buffer)
