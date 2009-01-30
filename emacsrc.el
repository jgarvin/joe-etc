(set-frame-font "Consolas-9")

;; Don't litter everywhere with file~ backups
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

;; Emacs won't load shell-script-mode for zsh automatically
(setq auto-mode-alist
      (append
       ;; File name (within directory) starts with a dot.
       '(("zshrc" . shell-script-mode))
       auto-mode-alist))

;; Make emacs use the normal clipboard
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; Function to generate tags with GNU Global
;; from here: http://emacs-fu.blogspot.com/2009/01/navigating-through-source-code-using.html
(defun djcb-gtags-create-or-update ()
  "create or update the gnu global tag file"
  (interactive)
  (if (not (= 0 (call-process "global" nil nil nil " -p"))) ; tagfile doesn't exist?
    (let ((olddir default-directory)
          (topdir (read-directory-name  
                    "gtags: top of source tree:" default-directory)))
      (cd topdir)
      (shell-command "gtags && echo 'created tagfile'")
      (cd olddir)) ; restore   
    ;;  tagfile already exists; update it
    (shell-command "global -u && echo 'updated tagfile'")))

;; Rebind the normal find tag functions to use the GNU global versions
(add-hook 'gtags-mode-hook 
  (lambda()
    (local-set-key (kbd "M-.") 'gtags-find-tag)   ; find a tag, also M-.
    (local-set-key (kbd "M-,") 'gtags-find-rtag)))  ; reverse tag

;; Generate tags whenever we open a C/C++ source file
(add-hook 'c-mode-common-hook
  (lambda ()
    (require 'gtags)
    (gtags-mode t)
    (djcb-gtags-create-or-update)))

;; Append a new line to files so GCC shuts up
(add-hook 'c-mode-common-hook
  (lambda ()
    (setq require-final-newline t)))

;; Turn off GUI parts
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t)
(setq visible-bell t)

;; Show column number in the mode line
(column-number-mode 1)

;; Prefer 4-space tabs
(setq c-default-style "bsd")
(setq-default c-basic-offset 4)
(setq-default indent-tabs-mode t)
(setq default-tab-width 4)
(setq tab-width 4)
(c-set-offset 'case-label '+)     ;; 'case' indented once after 'switch'

;; Show matching parentheses
(show-paren-mode 1)

;; Show current buffer name in titlebar (instead of emacs@whatever)
(setq frame-title-format "%b")

;;-------------
;; Switch between source and header
;;------------
;; Association list of extension -> inverse extension
(setq exts '(("c"   . ("h"))
			 ("cpp" . ("hpp" "h"))
             ("hpp" . ("cpp" "c"))
             ("h"   . ("cpp" "c"))))

;; Process the association list of extensions and find the last file
;; that exists
(defun find-other-file (fname fext)
  (dolist (value (cdr (assoc fext exts)) result)
    (if (file-exists-p (concat fname "." value))
        (setq result (concat fname "." value)))))

;; Toggle function that uses the current buffer name to open/find the 
;; other file
(defun toggle-header-buffer()
  (interactive)
  (let ((ext (file-name-extension buffer-file-name))
        (fname (file-name-sans-extension buffer-file-name)))
    (find-file (find-other-file fname ext))))

;; Bind the toggle function to a global key
;;(global-set-key "\M-t" 'toggle-header-buffer) ;; TODO: Think of better key
