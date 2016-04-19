(global-company-mode 1)
(setq company-show-numbers t)
(define-key company-active-map (kbd "RET") nil)
(define-key company-active-map [return] nil)
(define-key company-active-map (kbd "C-RET") #'company-complete-selection)

(defun etc-disable-company ()
  (company-mode 0))

(add-hook 'sh-mode-hook #'etc-disable-company)