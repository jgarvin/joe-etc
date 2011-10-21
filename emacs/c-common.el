(add-to-list 'load-path "/home/udesktop178/joeg/global-install/share/gtags/")

;; Function to generate tags with GNU Global
;; from here: http://emacs-fu.blogspot.com/2009/01/navigating-through-source-code-using.html
(defun djcb-gtags-create-or-update ()
  "create or update the gnu global tag file"
  (interactive)
  (if (not (= 0 (call-process "global" nil nil nil " -p"))) ; tagfile doesn't exist?
	  (let ((olddir default-directory)
			(topdir (read-directory-name
					 "gtags: top of source tree:" default-directory)))
		(when (not (string= topdir ""))
		  (progn
			(cd topdir)
			(start-process-shell-command "gtags create"
										 "gtags_buffer"
										 "~/etc/utils/remote_launch gtags -q && echo 'created tagfile'")
			(cd olddir)))) ; restore
    ;;  tagfile already exists; update it
    (start-process-shell-command "gtags update"
								 "gtags_buffer"
								 "~/etc/utils/remote_launch global -u 2> /dev/null && echo 'updated tagfile'")))

;; Rebind the normal find tag functions to use the GNU global versions
(add-hook 'gtags-mode-hook
		  (lambda()
			(local-set-key (kbd "M-.") 'gtags-find-tag)   ; find a tag, also M-.
			(local-set-key (kbd "M-,") 'gtags-find-rtag)))  ; reverse tag

(require 'gtags)
(gtags-mode t)
(djcb-gtags-create-or-update)

(add-to-list 'load-path "~/etc/autopair-read-only")

(require 'autopair)
(autopair-mode)
(local-set-key (kbd "C-y") 'yank-and-indent)

(setq require-final-newline t)

;; Don't indent whole files because they're in a namespace block
(c-set-offset 'innamespace 0)

(setq c-hungry-delete-key t)
(local-set-key (kbd "C-d") 'c-hungry-delete-forward)
(local-set-key (kbd "DEL") 'c-hungry-delete-forward)
(local-set-key (kbd "<backspace>") 'c-hungry-delete-backwards)

;; Prefer 4-space tabs
(setq c-default-style "bsd")
(setq c-basic-offset 4)
(setq indent-tabs-mode nil)
(setq default-tab-width 4)
(setq tab-width 4)
(c-set-offset 'case-label '+)     ;; 'case' indented once after 'switch'

;; TODO: Make this automatic for new .h files
(defun ff/headerize ()
  "Adds the #define HEADER_H, etc."
  (interactive)
  (let ((flag-name (replace-regexp-in-string
                    "[\. \(\)]" "_"
                    (upcase (file-name-nondirectory (buffer-name))))))
    (goto-char (point-max))
    (insert "\n#endif\n")
    (goto-char (point-min))
    (insert (concat "#ifndef " flag-name "\n"))
    (insert (concat "#define " flag-name "\n"))
    )
  )

;; Run makefile, or if there isn't one
(defun smart-compile()
  (if (or (file-exists-p "makefile")
		  (file-exists-p "Makefile")
		  (file-exists-p "../Makefile"))
	  (compile "make -k -j2")
	(if (file-expand-wildcards "*.tc")
		(compile "tlmake")
	  (compile (concat
				"make -k -j2 "
				(file-name-sans-extension
				 (file-name-nondirectory buffer-file-name)))))))


(defun ff/fast-compile ()
  "Compiles without asking anything."
  (interactive)
  (let ((compilation-read-command nil))
    (smart-compile)))

(defun tlmake-install ()
  (interactive)
  (compile "tlmake install"))

(define-key global-map [f9] 'ff/fast-compile)
(define-key global-map [f10] 'tlmake-install)
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
(setq exts '(("c"   . ("h" "H"))
			 ("cpp" . ("hpp" "h" "H"))
             ("hpp" . ("cpp" "c" "C"))
             ("h"   . ("cpp" "c" "C"))
			 ("H"   . ("cpp" "c" "C"))
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
			  (setq result (concat path "../" name "." value))))))))

;; Toggle function that uses the current buffer name to open/find the
;; other file
(defun toggle-header-buffer()
  (interactive)
  (let ((ext (file-name-extension buffer-file-name))
        (fname (file-name-sans-extension buffer-file-name)))
    (find-file (find-other-file fname ext))))

;; Bind the toggle function to a global key
(global-set-key "\M-t" 'toggle-header-buffer) ;; TODO: Think of better key

;; extra syntax highlighting
(defface font-lock-bracket-face
  '((t (:foreground "white")))
  "Font lock mode face for brackets, e.g. '(', ']', etc."
  :group 'font-lock-faces)
(defvar font-lock-bracket-face 'font-lock-bracket-face
  "Font lock mode face for backets.  Changing this directly
  affects only new buffers.")

(defvar operators-regexp
  (regexp-opt '("+" "-" "*" "/" "%" "!"
                "&" "^" "~" "|"
                "=" "<" ">"
                "." "," ";" ":" "?"))
  "Regexp matching symbols that are operators in most programming
  languages.")

(setq operators-font-lock-spec
      (cons operators-regexp
            (list
             0 ;; use whole match
             'font-lock-builtin-face
             'keep ;; OVERRIDE
             )))

(defvar brackets-regexp
  (regexp-opt '("(" ")" "[" "]" "{" "}"))
  "Regexp matching symbols that are grouping operators in most
  programming languages.")

(setq brackets-font-lock-spec
      (cons brackets-regexp
            (list
             0 ;; use whole match
             'font-lock-bracket-face
             'keep ;; OVERRIDE
             )))

(setq c-types-regexp
      (concat
       "\\<[_a-zA-Z][_a-zA-Z0-9]*_t\\>" "\\|"
       (regexp-opt '("unsigned" "int" "char" "float" "void") 'words)))

(font-lock-add-keywords
 'c-mode
 (list
  operators-font-lock-spec
  brackets-font-lock-spec
  (cons c-types-regexp 'font-lock-type-face)))

(font-lock-add-keywords
 'c++-mode
 (list
  operators-font-lock-spec
  brackets-font-lock-spec
  (cons c-types-regexp 'font-lock-type-face)))