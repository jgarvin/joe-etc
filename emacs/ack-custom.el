;; without this acks in separate frames will overwrite one another
(defun etc-rename-ack-buffer (compilation-buffer process-result)
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "bin/ack")
    (re-search-forward "-- \\(.*\\)$" (point-at-eol))
    (let ((new-name (concat "*ack " (buffer-substring (match-beginning 0) (point-at-eol)) "*")))
      (when (get-buffer new-name)
        (kill-buffer new-name))
      (rename-buffer new-name))))

(defun etc-ack-setup ()
  (set (make-local-variable 'compilation-finish-function) 'etc-rename-ack-buffer))

(add-hook 'ack-and-a-half-mode-hook 'etc-ack-setup)
