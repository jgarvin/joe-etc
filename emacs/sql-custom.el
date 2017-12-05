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

(add-hook 'sql-interactive-mode-hook 'my-sql-save-history-hook)

;; (defun example-test-copy-database ()
;;   (interactive)
;;   (let ((sql-server "test")
;;         (sql-database "examplebackoffice")
;;         (sql-user "test")
;;         (sql-postgres-program "psql"))
;;     (sql-postgres "test")))