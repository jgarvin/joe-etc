(require 'md-belt-impl)

(setq md-frequency-belt
        (make-md-belt
         :name "frequency"
         :construct #'md-setup-frequency-belt
         :destruct #'md-destroy-frequency-belt
         :contents 'md-symbols-cache
         :color "green"))

(defun md-setup-frequency-belt ()
  (add-hook 'md-symbols-cache-refresh-hook #'md-bt-schedule-update))

(defun md-destroy-frequency-belt ()
  (remove-hook 'md-symbols-cache-refresh-hook #'md-bt-schedule-update))

(add-to-list 'md-belt-list md-frequency-belt)


