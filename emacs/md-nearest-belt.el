(require 'md-belt-impl)

(defvar md-nearest-belt-symbols nil)

(defun md-get-next-symbol (dir)
  (let ((sym-start)
        (end (if dir (point-max) (point-min)))
        (search-function (if dir 're-search-forward 're-search-backward))
        (open-regex (if dir "\\_<" "\\_>"))
        (close-regex (if dir "\\_>" "\\_<")))
    (funcall search-function open-regex end 1)
    (setq sym-start (point))
    (funcall search-function close-regex end 1)
    (cons sym-start (point))))
    
(defun md-get-nearest-symbols ()
  (save-excursion
    (let ((symbol-list)
          (reverse-point (point))
          (forward-point (point))
          (next-symbol)
          (previous-symbol))
      (condition-case nil
          (while (and (< (length symbol-list) md-belt-item-max)
                      (or (> reverse-point (point-min))
                          (< forward-point (point-max))))
            (goto-char forward-point)
            (setq next-symbol (md-get-next-symbol t))
            (goto-char reverse-point)
            (setq previous-symbol (md-get-next-symbol nil))
            (let* ((next-start (car next-symbol))
                   (next-end (cdr next-symbol))
                   (previous-start (cdr previous-symbol))
                   (previous-end (car previous-symbol))
                   (next-symbol-str (buffer-substring-no-properties next-start next-end))
                   (previous-symbol-str (buffer-substring-no-properties previous-start previous-end)))
              (setq forward-point next-end)
              (setq reverse-point previous-start)
              (unless (md-filter-symbol next-symbol-str next-start next-end)
                (setq symbol-list (cons next-symbol-str symbol-list)))
              (unless (md-filter-symbol previous-symbol-str previous-start previous-end)
                (setq symbol-list (cons previous-symbol-str symbol-list)))
              (setq symbol-list (delete-if #'null symbol-list))
              (delete-dups symbol-list)))
        (search-failed nil))
      symbol-list)))

(defun md-nearest-post-command-hook ()
  ;;(message "nearest hook")
  (unless (window-minibuffer-p)
    (setq md-nearest-belt-symbols (md-get-nearest-symbols))))
  
(defun md-setup-nearest-belt ()
  (add-hook 'post-command-hook #'md-nearest-post-command-hook)
  (setq md-nearest-belt
        (make-md-belt
         :name "nearest"
         :construct #'md-setup-nearest-belt
         :destruct #'md-destroy-nearest-belt
         :contents 'md-nearest-belt-symbols
         :color "red"))
  (add-to-list 'md-belt-list md-nearest-belt))

(defun md-destroy-nearest-belt ()
  (setq md-belt-list (remove-if (lambda (x) (string= (md-belt-name x) "nearest")) md-belt-list))
  (setq md-nearest-belt-symbols nil)
  (remove-hook 'post-command-hook #'md-nearest-post-command-hook))

(provide 'md-nearest-belt)

;; (md-setup-nearest-belt)
;; (md-destroy-nearest-belt)
