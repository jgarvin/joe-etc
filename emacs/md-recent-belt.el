(require 'md-belt-impl)

(defun md-get-recent-ring ()
  (when md-recent-ring
    (remove-if (lambda (x) (or (not x) (md-filter-symbol x nil nil))) (ring-elements md-recent-ring))))

(setq md-recent-belt
        (make-md-belt
         :name "recent"
         :construct nil
         :destruct nil
         :contents '(md-get-recent-ring)
         :color "light blue"))

(add-to-list 'md-belt-list md-recent-belt)

