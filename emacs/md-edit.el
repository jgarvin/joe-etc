(require 'rect) ;; otherwise rectangle-mark-mode undefined

(defun md-kill-symbol-or-sexp-or-region (beg end &optional region)
  (interactive (list (mark) (point) 'region))
  (if rectangle-mark-mode
      (kill-region beg end region)
    (cond
     ((region-active-p)
      (kill-region beg end))
     ((or (md-likely-followed-by-closer (point))
          (md-likely-preceded-by-opener (1+ (point)))
          (equal (plist-get (sp-get-thing) :beg) (point))) ;; this works pretty well, do I need the other two conditions anymore?
      (kill-region (plist-get (sp-get-thing) :beg)
                   (plist-get (sp-get-thing) :end)))
     ((bounds-of-thing-at-point 'symbol)
      (kill-region (car (bounds-of-thing-at-point 'symbol))
                   (cdr (bounds-of-thing-at-point 'symbol))))
     (t (user-error "Nothing to kill at point!")))))

(defun md-copy-symbol-or-sexp-or-region (beg end &optional region)
  (interactive (list (mark) (point) 'region))
  (if rectangle-mark-mode
      (kill-ring-save beg end region)
    (cond
     ((region-active-p)
      (kill-ring-save beg end))
     ((or (md-likely-followed-by-closer (point))
          (md-likely-preceded-by-opener (1+ (point)))
          (equal (plist-get (sp-get-thing) :beg) (point))) ;; this works pretty well, do I need the other two conditions anymore?
      (kill-ring-save (plist-get (sp-get-thing) :beg)
                      (plist-get (sp-get-thing) :end)))
     ((bounds-of-thing-at-point 'symbol)
      (kill-ring-save (car (bounds-of-thing-at-point 'symbol))
                      (cdr (bounds-of-thing-at-point 'symbol))))
     (t (user-error "Nothing to copy at point!")))))

;; hello (listp '(3 . 4))