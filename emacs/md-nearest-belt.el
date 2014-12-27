(require 'md-belt-impl)

(defun md-get-next-symbol (dir)
  (let ((sym-start)
        (end (if dir (point-max) (point-min)))
        (search-function (if dir 're-search-forward 're-search-backward))
        (open-regex (if dir "\\_<" "\\_>"))
        (close-regex (if dir "\\_>" "\\_<")))
    (funcall search-function open-regex end t)
    (setq sym-start (point))
    (funcall search-function close-regex end t)
    (cons sym-start (point))))
    
(defun md-get-nearest-symbols ()
  (save-excursion
    (let ((symbol-list)
          (reverse-point (point))
          (forward-point (point))
          (next-symbol)
          (previous-symbol))
      (while (and (< (length symbol-list) md-belt-item-max)
                  (or (> reverse-point (point-min))
                      (< forward-point (point-max))))
        (goto-char forward-point)
        (setq next-symbol (md-get-next-symbol t))
        (goto-char reverse-point)
        (setq previous-symbol (md-get-next-symbol nil))
        (let* ((next-start (car next-symbol))
               (next-end (cdr next-symbol))
               (previous-start (car previous-symbol))
               (previous-end (cdr previous-symbol))
               (next-symbol-str (buffer-substring-no-properties next-start next-end))
               (previous-symbol-str (buffer-substring-no-properties previous-start previous-end)))
          (setq forward-point next-end)
          (setq reverse-point previous-end)
          ;; (message "%s %s" next-symbol-str previous-symbol-str)
          (setq symbol-list (nconc symbol-list (list (unless (md-filter-symbol next-symbol-str next-start next-end) next-symbol-str)
                                                     (unless (md-filter-symbol previous-symbol-str previous-start previous-end) previous-symbol-str))))
          (setq symbol-list (delete-if-not 'identity symbol-list))
          (delete-dups symbol-list)))
      symbol-list)))

(defun md-nearest-post-command-hook ()
  ;; (message "nearest hook")
  (setq md-nearest-belt-text (format "%S" (md-get-nearest-symbols))))
  
(defun md-setup-nearest-belt ()
  (add-hook 'post-command-hook #'md-nearest-post-command-hook))

(defun md-destroy-nearest-belt ()
  (setq md-nearest-belt-text nil)
  (remove-hook 'post-command-hook #'md-nearest-post-command-hook))

(setq nearest-belt
      (make-md-belt :construct #'md-setup-nearest-belt
                    :destruct #'md-destroy-nearest-belt
                    :text 'md-nearest-belt-text
                    :color "red"))

(add-to-list 'md-belt-list nearest-belt)
;; (setq md-belt-list nil)
