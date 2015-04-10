;; -*- lexical-binding: t -*-

(defvar-local run-command nil)

(if (file-directory-p "~/opt/share/gtags")
    (add-to-list 'load-path "~/opt/share/gtags"))
(require 'gtags)
(gtags-mode t)

(defun gtags-select-tag-and-kill-buffer ()
  (interactive)
  (let ((buf (current-buffer)))
    (gtags-select-tag)
    (kill-buffer buf)))

(defun etc-setup-gtags ()
  (define-key gtags-select-mode-map (kbd "<return>") 'gtags-select-tag-and-kill-buffer))

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
  (setq tab-width my-indent-size)

  (require 'compile)
  (unless (file-exists-p "Makefile")
    (set (make-local-variable 'compile-command)
         ;; emulate make's .c.o implicit pattern rule, but with
         ;; different defaults for the CC, CPPFLAGS, and CFLAGS
         ;; variables:
         ;; $(CC) -c -o $@ $(CPPFLAGS) $(CFLAGS) $<
         (let ((file (file-name-nondirectory buffer-file-name)))
           (format "%s -c -o %s.o %s %s %s"
                   (or (getenv "CC") "g++")
                   (file-name-sans-extension file)
                   (or (getenv "CPPFLAGS") "-DDEBUG=9")
                   (or (getenv "CFLAGS") "-ansi -pedantic -Wall -g")
                   file))))

  ;; (add-to-list (make-local-variable 'compilation-finish-functions)
  ;;              #'etc-compilation-finished)
  )

(add-hook 'c-mode-common-hook 'etc-setup-c-common)

(defvar-local etc-compilation-run-command nil)
(defvar-local etc-compilation-project nil)
(defvar-local etc-compilation-compile-command nil)
(defvar-local etc-compilation-invoking-buffer nil)

(defun etc-quit-run (&optional kill-buffer)
  (interactive "P")
  (quit-window kill-buffer (selected-window)))

;; we only get the compilation buffer, so we either have to store data
;; on it ahead of time in a buffer-local, or we can store it in the
;; buffer name somehow
(defun etc-run (comp-buf finish-status)
  (setq finish-status (string-trim finish-status)) ;; trailing newline
  (if (string= "finished" finish-status)
      (when (with-current-buffer comp-buf (equal major-mode 'compilation-mode))
        ;; the variable will only be set if compile is invoked by our custom command
        ;; we don't want to do any of this if the user does M-x compile
        (when etc-compilation-invoking-buffer 
              (with-current-buffer etc-compilation-invoking-buffer
                (let* ((run (or run-command
                                (file-name-sans-extension (buffer-file-name))))
                       (buff-name (replace-regexp-in-string "compile|\\(.*?|.*\\)" "run|\\1" (buffer-name comp-buf)))
                       ;; We temporarily customize display-buffer-alist to not pop up
                       ;; a new window if the buffer is already displayed in one.
                       (display-buffer-alist
                        (if (get-buffer-window buff-name t)
                            (cons (cons (regexp-quote buff-name) (cons #'display-buffer-no-window '())) display-buffer-alist)
                          display-buffer-alist))
                       (buff (get-buffer-create buff-name)))
                  (async-shell-command run buff)
                  (with-current-buffer buff
                    (setq buffer-read-only t)
                    (local-set-key (kbd "q") #'etc-quit-run))))))))
  
(add-hook 'compilation-finish-functions #'etc-run)

(defun etc-get-project ()
  (if (projectile-project-p)
      (projectile-project-name)
    (if (buffer-file-name)
        (file-name-directory (buffer-file-name))
      (default-directory))))

(defun etc-compile-and-run (&optional arg)
  (interactive "P")
  (let* (;; make the compilaton buffer depend on the command name and the project,
         ;; this makes sure we can have multiple compiles going
         (buf-name (format "compile|%s|%s" (etc-get-project) compile-command))
         (compilation-buffer-name-function (lambda (mode) buf-name))
         ;; Pass info on for how to run things from the buffer we invoke in, which
         ;; in turn could be getting from project or elsewhere. Probably should
         ;; make compile-command and run-command functions...
         (runc run-command)
         (comc compile-command)
         (proj (etc-get-project))
         (invoking (current-buffer))
         (compilation-mode-hook (cons (lambda (&rest unused)
                                        (setq etc-compilation-compile-command comc)
                                        (setq etc-compilation-project proj)
                                        (setq etc-compilation-run-command runc)
                                        (setq etc-compilation-invoking-buffer invoking)) compilation-mode-hook)))
    (compile compile-command arg)))

(defun etc-c-init ()
  (define-key c-mode-base-map (kbd "C-c C-c") #'etc-compile-and-run))

(add-hook 'c-initialization-hook #'etc-c-init)


