;; (global-company-mode 0)
;; (setq company-show-numbers t)
;; (define-key company-active-map (kbd "RET") nil)
;; (define-key company-active-map [return] nil)
;; (define-key company-active-map (kbd "C-RET") #'company-complete-selection)

;; (defun etc-disable-company ()
;;   (company-mode 0))

;; (add-hook 'sh-mode-hook #'etc-disable-company)

;; Disable arrow keys in LSP completion popups (via company-mode)
(with-eval-after-load 'lsp-mode
  (with-eval-after-load 'company
    ;; Remove arrow key bindings from company-active-map
    (define-key company-active-map [up] nil)
    (define-key company-active-map [down] nil)

    ;; Set alternative navigation keys
    (define-key company-active-map (kbd "M-p") #'company-select-previous)
    (define-key company-active-map (kbd "M-n") #'company-select-next)))
