(require 'goto-chg)

(defun etc-goto-last-change ()
  (interactive)
  (condition-case err
      (call-interactively 'goto-last-change)
    (error
     (if (string= (error-message-string err) "No further change info")
         (message "No changes to navigate to")
       ;; Re-signal other errors
       (signal (car err) (cdr err))))))

(defun etc-goto-last-change-reverse ()
  (interactive)
  (condition-case err
      (call-interactively 'goto-last-change-reverse)
    (error
     (cond
      ((string= (error-message-string err) "No further change info")
       (message "No previous changes to navigate to"))
      ((string= (error-message-string err) "Negative arg: Cannot reverse as the first operation")
       (message "No previous changes to navigate to"))
      (t
       ;; Re-signal other errors
       (signal (car err) (cdr err)))))))

(global-set-key [(control ?.)] 'etc-goto-last-change)
(global-set-key [(control ?,)] 'etc-goto-last-change-reverse)
