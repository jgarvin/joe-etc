(require 'md-belt-impl)

(defun md-setup-frequency-belt ()
  (setq md-frequency-belt
        (make-md-belt
         :name "frequency"
         :construct #'md-setup-frequency-belt
         :destruct #'md-destroy-frequency-belt
         :contents 'md-symbols-cache
         :color "green"))
  (add-to-list 'md-belt-list md-frequency-belt)
  (add-hook 'md-symbols-cache-refresh-hook #'md-update-belts))

(defun md-destroy-frequency-belt ()
  (setq md-belt-list (remove-if (lambda (x) (string= (md-belt-name x) "frequency")) md-belt-list))
  (remove-hook 'md-symbols-cache-refresh-hook #'md-update-belts))

(provide 'md-frequency-belt)

