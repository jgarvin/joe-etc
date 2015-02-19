;; things like 'less' only work in real terminals
(setenv "PAGER" "cat")

;; (require 'subr-x)

;; (let ((s "*special*")) (string-match "\\*\\([^*]+\\)\\*\\'" s) (match-string 1 s))

(defun etc-shell-name ()
  (let ((name (buffer-name (current-buffer))))
    (when (string-match "\\*\\([^*]+\\)\\*\\'" name)
      (setq name (match-string 1 name)))
    (concat "$" name)))

(defun etc-shell-mode-hook ()
  ;; (rename-buffer (generate-new-buffer-name (etc-shell-name)))
  ;;(rename-uniquely)
  ;; make it so I can hit enter on error messages from gcc
  ;; to open the file at that location
  (compilation-shell-minor-mode 1)
  (shell-dirtrack-mode -1)
  (dirtrack-mode 1)
  (setq dirtrack-list '("^[^@:\n]+@[^:\n]+:\\([^]]+\\)][$#]" 1))
  (ansi-color-for-comint-mode-on))

(defun etc-open-shell ()
  (interactive)
  (shell (generate-new-buffer-name "$shell")))

(add-hook 'shell-mode-hook #'etc-shell-mode-hook)


