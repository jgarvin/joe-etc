(require 'md-belt-impl)

(defun md-get-kill-ring ()
  (remove-if (lambda (x) (md-filter-symbol x nil nil nil)) kill-ring))

(setq md-kill-belt
      (make-md-belt
       :name "kill"
       :construct nil
       :destruct nil
       :contents '(md-get-kill-ring)
       :color "yellow"))

(add-to-list 'md-belt-list md-kill-belt)
;;(setq md-belt-list (delete md-kill-belt md-belt-list))
