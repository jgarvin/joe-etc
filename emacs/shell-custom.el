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
  ;;(setq dirtrack-list '("^[^@:\n]+@[^:\n]+:\\([^]]+\\)][$#]" 1))
  (setq dirtrack-list '("^\\[[^<\n]*<\\([^>\n]+\\)>][$#]" 1))
  (ansi-color-for-comint-mode-on))

;;(let ((s "[</ssh:prophet@panopticon:/home/prophet>]$ "))
;; (let ((s "[prophet@panopticon:<~/etc/shell>]$ "))  
;;   (string-match "^\\[[^<\n]*<\\([^>\n]+\\)>][$#]" s)
;;   (match-string-no-properties 1 s))

(defun etc-open-shell ()
  (interactive)
  ;; (let ((dir default-directory))
  ;;   (-filter (lambda (x))))
  (shell (generate-new-buffer-name "$shell")))

(global-set-key (kbd "C-z") #'etc-open-shell)

(add-hook 'shell-mode-hook #'etc-shell-mode-hook)


;; (defun etc-setup-delayed-truncate (&optional unused)
;;   (unless etc-next-truncate-allowed-timer
;;     (etc-comint-truncate)))

;; ;; If you truncate continously it causes too much performance trouble,
;; ;; the truncation starves I/O to all other processes, so mandimus gets
;; ;; disconnected.
;; (add-hook 'comint-output-filter-functions #'etc-setup-delayed-truncate)
