;; -*- lexical-binding: t -*-

(defun single-pair-sexp (p func)
  (lambda (&optional arg)
    (interactive "P")
    (sp-restrict-to-pairs p func)))

(defun pair-only-sexp (func)
  (lambda (&optional arg)
    (interactive "P")
    (sp-restrict-to-object 'sp-prefix-pair-object func)))

;; TODO: figure out why 'out' is deleting text of the sexp
;; TODO: figure out why select next doesn't work for quotes
;; TODO: figure out why colon can't go into align-regexp...
(defun single-pair-only-sexp (p func)
  (funcall
   (single-pair-sexp p (pair-only-sexp func))))

;; (setq func-list-sp
;;       '(sp-forward-sexp
;; 	sp-backward-sexp
;; 	sp-down-sexp
;; 	sp-backward-down-sexp
;; 	sp-up-sexp
;; 	sp-backward-up-sexp
;; 	sp-next-sexp
;; 	sp-previous-sexp
;; 	sp-beginning-of-sexp
;; 	sp-end-of-sexp
;; 	sp-beginning-of-next-sexp
;; 	sp-beginning-of-previous-sexp
;; 	sp-end-of-next-sexp
;; 	sp-end-of-previous-sexp))

;; (setq pair-list-sp
;;       '(("{" . "braces")
;; 	("(" . "parens")
;; 	("[" . "brackets")
;; 	("<" . "angles")
;; 	("\"" . "quotes")
;; 	("'" . "single-quotes")))

;; (dolist (func func-list-sp)
;;   (dolist (pair pair-list-sp)
;;     (let ((p (first pair))
;; 	  (name (cdr pair)))
;;       (print p)
;;       (print name)
;;       (print (symbol-name func))
;;       (print (concat (symbol-name func) "-" name))
;;       (fset (make-symbol (concat (symbol-name func) "-" name))
;; 	    (single-pair-only-sexp p func)))))
;; (funcall (single-pair-only-sexp "{" 'sp-forward-sexp)) 
;; (assoc "{" pair-list-sp)

;; (testingvar)

;; []{}()

;; (defun sp-pair-curly-down-sexp (&optional arg)

;; (defun sp-pair-forward-sexp (&optional arg)
;;   (interactive "P")
;;   (sp-restrict-to-object 'sp-prefix-pair-object 'sp-forward-sexp))

;; (sp-restrict-to-object 'sp-prefix-pair-object 'sp-forward-sexp)
