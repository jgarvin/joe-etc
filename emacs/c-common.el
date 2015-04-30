;; -*- lexical-binding: t -*-

(defvar-local run-command nil)

(if (file-directory-p "~/opt/share/gtags")
    (add-to-list 'load-path "~/opt/share/gtags"))
(require 'gtags)
(gtags-mode t)

;; (defun gtags-select-tag-and-kill-buffer ()
;;   (interactive)
;;   (let ((buf (current-buffer)))
;;     (gtags-select-tag)
;;     (kill-buffer buf)))

;; (defun etc-setup-gtags ()
;;   (define-key gtags-select-mode-map (kbd "<return>") #'gtags-select-tag))

(add-hook 'gtags-select-mode-hook 'etc-setup-gtags)          

;; Run makefile, or if there isn't one
(defun smart-compile()
  (compile "bld"))

(defun ff/fast-compile ()
  "Compiles without asking anything."
  (interactive)
  (let ((compilation-read-command nil))
    (smart-compile)))

(define-key global-map [f9] 'ff/fast-compile)
(defun list-all-subfolders (folder)
  (let ((folder-list (list folder)))
    (dolist (subfolder (directory-files folder))
      (let ((name (concat folder "/" subfolder)))
	(when (and (file-directory-p name)
		   (not (equal subfolder ".."))
		   (not (equal subfolder ".")))
	  (set 'folder-list (append folder-list (list name))))))
    folder-list))

;;-------------
;; Switch between source and header
;;------------
;; Association list of extension -> inverse extension
(setq exts '(("c"   . ("hpp" "h" "H"))
             ("cc"  . ("hpp" "h" "H"))
             ("cpp" . ("hpp" "h" "H"))
             ("hpp" . ("cpp" "c" "C" "cc" "CC"))
             ("h"   . ("cpp" "c" "C" "cc" "CC"))
             ("H"   . ("cpp" "c" "C" "cc" "CC"))
             ("C"   . ("hpp" "h" "H"))))

;; Process the association list of extensions and find the last file
;; that exists
(defun find-other-file (fname fext)
  (dolist (value (cdr (assoc fext exts)) result)
    (let ((path (file-name-directory fname))
          (name (file-name-nondirectory fname)))
      (if (file-exists-p (concat path name "." value))
          (setq result (concat path name "." value))
        (if (file-exists-p (concat path "private/" name "." value))
            (setq result (concat path "private/" name "." value))
          (if (file-exists-p (concat path "../" name "." value))
              (setq result (concat path "../" name "." value))
            (if (file-exists-p (concat path name "INLINES." value))
                (setq result (concat path name "INLINES." value))
              (if (file-exists-p (concat path (replace-in-string name "INLINES" "") "." value))
                  (setq result (concat path (replace-in-string name "INLINES" "") "." value))))))))))

;; Toggle function that uses the current buffer name to open/find the
;; other file
(defun toggle-header-buffer()
  (interactive)
  (let ((ext (file-name-extension buffer-file-name))
        (fname (file-name-sans-extension buffer-file-name)))
    (find-file (find-other-file fname ext))))

(defun etc-compilation-finished (buffer finished-status)
  (message "Result: [%S] [%S]" buffer finished-status))

(defun etc-setup-c-common ()
  (local-set-key "\M-t" 'toggle-header-buffer)
  
  ;; Starting in emacs 23 there's some stupid default abbreviation
  ;; for trying to correct mispellings of 'else', problem is it doesn't
  ;; understand context, so a legit variable named elSE will always get
  ;; changed to Else.
  (abbrev-mode 0)
  
  (subword-mode 1)
  (require 'whitespace)
  (setq whitespace-style '(face lines))
  (whitespace-mode t)
  
  ;; Rebind the normal find tag functions to use the GNU global versions
  (local-set-key (kbd "M-.") 'gtags-find-tag)   ; find a tag, also M-.
  (local-set-key (kbd "M-,") 'gtags-find-rtag)  ; reverse tag

  ;; In later gtags versions these aren't set by default.
  (define-key gtags-select-mode-map "\e*" 'gtags-pop-stack)
  (define-key gtags-select-mode-map "\^?" 'scroll-down)
  (define-key gtags-select-mode-map " " 'scroll-up)
  (define-key gtags-select-mode-map "\C-b" 'scroll-down)
  (define-key gtags-select-mode-map "\C-f" 'scroll-up)
  (define-key gtags-select-mode-map "k" 'previous-line)
  (define-key gtags-select-mode-map "j" 'next-line)
  (define-key gtags-select-mode-map "p" 'previous-line)
  (define-key gtags-select-mode-map "n" 'next-line)
  (define-key gtags-select-mode-map "q" 'gtags-pop-stack)
  (define-key gtags-select-mode-map "u" 'gtags-pop-stack)
  (define-key gtags-select-mode-map "\C-t" 'gtags-pop-stack)
  (define-key gtags-select-mode-map "\C-m" 'gtags-select-tag)
  (define-key gtags-select-mode-map "\C-o" 'gtags-select-tag-other-window)

  ;;(setq require-final-newline t)
  
  (setq c-hungry-delete-key t)
  (local-set-key (kbd "C-d") 'c-hungry-delete-forward)
  (local-set-key (kbd "DEL") 'c-hungry-delete-forward)
  (local-set-key (kbd "<backspace>") 'c-hungry-delete-backwards)

  ;; Prefer 4-space tabs
  (setq my-indent-size 4)
  (c-set-offset 'innamespace 0) ;; don't indent top level namespace
  (c-set-offset 'case-label '+) ;; 'case' indented once after 'switch'

  (setq c-default-style "bsd")
  (setq c-basic-offset my-indent-size)
  (setq indent-tabs-mode nil)
  (setq default-tab-width my-indent-size)
  (setq tab-width my-indent-size))

(add-hook 'c-mode-common-hook 'etc-setup-c-common)

