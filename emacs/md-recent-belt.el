(require 'md-belt-impl)

(defun md-get-recent-ring ()
  (remove-if (lambda (x) (or (not x) (md-filter-symbol x nil nil))) (ring-elements md-recent-ring)))

(defun md-setup-recent-belt ()
  (setq md-recent-belt
        (make-md-belt
         :name "recent"
         :construct #'md-setup-recent-belt
         :destruct #'md-destroy-recent-belt
         :contents '(md-get-recent-ring)
         :color "light blue"))
  (add-to-list 'md-belt-list md-recent-belt))

(defun md-destroy-recent-belt ()
  (setq md-belt-list (remove-if (lambda (x) (string= (md-belt-name x) "recent")) md-belt-list)))

(provide 'md-recent-belt)

