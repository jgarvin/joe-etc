(use-package
  sqlup-mode
  :ensure t
  )

;; don't ask to confirm user password etc.
(require 'sql)
(defalias 'sql-get-login 'ignore)

(make-directory "~/.emacs.d/sql/" t)

(defun my-sql-save-history-hook ()
  (let ((lval 'sql-input-ring-file-name))
    (if sql-server
        (let ((filename
               (concat "~/.emacs.d/sql/"
                       sql-server
                       "-history.sql")))
          (set (make-local-variable lval) filename))
      (error
       "SQL history will not be saved because sql-server is nil"
       ))))

(add-hook 'sql-interactive-mode-hook #'my-sql-save-history-hook)

(add-hook 'sql-interactive-mode-hook #'sqlup-mode)

(defun etc-sql-interactive-mode-hook ()
  (visual-line-mode 0))

(add-hook 'sql-interactive-mode-hook #'etc-sql-interactive-mode-hook)

(defun etc-delete-trailing-whitespace-bug-workaround (orig-fun &rest args)
  "Freezes when run in sql-interactive-mode!"
  (unless (derived-mode-p 'sql-interactive-mode 'shell-mode
                          'eshell-mode # also eshell strangely....
                          )
    (apply orig-fun args)))

(advice-add 'delete-trailing-whitespace :around #'etc-delete-trailing-whitespace-bug-workaround)

;; (defun example-test-copy-database ()
;;   (interactive)
;;   (let ((sql-server "test")
;;         (sql-database "examplebackoffice")
;;         (sql-user "test")
;;         (sql-postgres-program "psql"))
;;     (sql-postgres "test")))