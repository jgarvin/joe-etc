(defun md-kill-symbol-or-sexp-or-region (beg end &optional region)
  (interactive (list (mark) (point) 'region))
  (cond
   ((region-active-p) (kill-region beg end))
   ((or (md-likely-followed-by-closer (point))
        (md-likely-preceded-by-opener (1+ (point))))
    (kill-region (plist-get (sp-get-thing) :beg)
                 (plist-get (sp-get-thing) :end)))
   (t (kill-region (car (bounds-of-thing-at-point 'symbol))
                   (cdr (bounds-of-thing-at-point 'symbol))))))

(defun md-copy-symbol-or-sexp-or-region (beg end &optional region)
  (interactive (list (mark) (point) 'region))
  (cond
   ((region-active-p) (kill-ring-save beg end))
   ((or (md-likely-followed-by-closer (point))
        (md-likely-preceded-by-opener (1+ (point))))
    (kill-ring-save (plist-get (sp-get-thing) :beg)
                    (plist-get (sp-get-thing) :end)))
   (t (kill-ring-save (car (bounds-of-thing-at-point 'symbol))
                      (cdr (bounds-of-thing-at-point 'symbol))))))

;; hello (listp '(3 . 4))