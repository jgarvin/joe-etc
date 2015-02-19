;; automagically tail log files
(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-tail-mode))

(defun etc-log-tail-handler ()
  (end-of-buffer)
  (make-variable-buffer-local 'auto-revert-interval)
  (setq auto-revert-interval 1)
  (auto-revert-set-timer)
  (make-variable-buffer-local 'auto-revert-verbose)
  (setq auto-revert-verbose nil)
  (read-only-mode t)
  (when (fboundp 'show-smartparens-mode)
    (show-smartparens-mode 0)))

(add-hook 'auto-revert-tail-mode-hook 'etc-log-tail-handler)
