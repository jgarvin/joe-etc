(require 'md-belt-impl)

(defun md-setup-nick-belt ()
  (setq md-nick-belt
        (make-md-belt
         :name "nick"
         :construct #'md-setup-nick-belt
         :destruct #'md-destroy-nick-belt
         :contents 'md-active-erc-nicknames
         :context '(equal major-mode 'erc-mode)
         :color "orange"))
  (add-to-list 'md-belt-list md-nick-belt))

(defun md-destroy-nick-belt ()
  (setq md-belt-list (remove-if (lambda (x) (string= (md-belt-name x) "nick")) md-belt-list)))

(provide 'md-nick-belt)

