;; things like 'less' only work in real terminals
(setenv "PAGER" "cat")

(defun etc-shell-mode-hook ()
  ;; make it so I can hit enter on error messages from gcc
  ;; to open the file at that location
  (compilation-shell-minor-mode 1)
  (shell-dirtrack-mode -1)
  (dirtrack-mode 1)
  (setq dirtrack-list '("^[^@:\n]+@[^:\n]+:\\([^]]+\\)][$#]" 1))
  (ansi-color-for-comint-mode-on))

(add-hook 'shell-mode-hook #'etc-shell-mode-hook)

