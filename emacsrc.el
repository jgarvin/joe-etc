(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/backup/emacs-backups"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)       ; use versioned backups

;; Prefer to code in Python 3.0, the future :D
(setq-default py-python-command "python3")

(setq auto-mode-alist
      (append
       ;; File name (within directory) starts with a dot.
       '(("/\\.[^/]*\\'" . fundamental-mode)
	 ;; File name has no dot.
	 ("/[^\\./]*\\'" . fundamental-mode)
	 ;; File name ends in `.C'.
	 ("\\.C\\'" . c++-mode))
       auto-mode-alist))
