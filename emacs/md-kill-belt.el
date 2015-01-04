(require 'md-belt-impl)

(defun md-get-kill-ring ()
  (remove-if (lambda (x) (md-filter-symbol x nil nil)) kill-ring))

(defun md-setup-kill-belt ()
  (setq md-kill-belt
        (make-md-belt
         :name "kill"
         :construct #'md-setup-kill-belt
         :destruct #'md-destroy-kill-belt
         :contents '(md-get-kill-ring)
         :color "yellow"))
  (add-to-list 'md-belt-list md-kill-belt))

(defun md-destroy-kill-belt ()
  (setq md-belt-list (remove-if (lambda (x) (string= (md-belt-name x) "kill")) md-belt-list)))

(provide 'md-kill-belt)
