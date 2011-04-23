;; TODO: Tab should OBEY. If the line is already indented, assume I want to insert a tab.

;; TODO: Make canceling telling global path just disable it

;; TODO: vc-annotate on CVS should use current working version when on a branch

;; TODO: If you undo, the mark should be reset.

;; Ideal tab behavior:
;; Indent line if not indented
;; If indented already, and are at beginning of line, insert a tab
;; If not indented already, and not at beginning of line, do code completion

;; Module: use indentation already used in file!
;; Nice to have: Next/prev buffer that is a file in same folder or subfolder, has the effect
;; of letting me browse project files. Nice for having dual emacs groups for different projects...

;; TODO: "automatic vertical indenting", know how much I like to space out my functions and classes
;; and automatically make sure that many lines are preserved when I copy/paste

;; TODO: When doing a find/replace, take the region from the last all whitespace line to the next all
;; whitespace line, and indent it.

;; TODO: hide-lines should put something in the status bar to indicate things are hidden
;; TODO: executing show-all-invisible should only reveal lines in the current buffer, not all

;; Enable debugging
(toggle-debug-on-error)

;; When remotely logging in, need to remap alt for emacs keybindings to work
(when (getenv "DISPLAY")
  (when (not (string= (nth 0 (split-string (nth 1 (split-string (getenv "DISPLAY") ":")) "\\.")) "0"))
	(setq x-alt-keysym 'meta)))

(setq backup-directory-alist
	  `((".*" . ,"~/backup")))
(setq auto-save-file-name-transforms
	  `((".*" ,"~/backup" t)))
(setq tramp-backup-directory-alist backup-directory-alist)

(setq load-path (cons "~/etc/color-theme-6.6.0" load-path))

(load-file "~/etc/color-theme-6.6.0/color-theme.el")
(load-file "~/etc/breadcrumb.el")

(add-to-list 'load-path "~/etc/drag-stuff")
(require 'drag-stuff)
(drag-stuff-global-mode t)

(load-file "~/etc/hide-lines.el")
(require 'hide-lines)
(global-set-key "\C-ch" 'hide-lines)
(global-set-key "\C-cu" 'show-all-invisible)

(defun yank-and-indent ()
  "Yank and then indent the newly formed region according to mode."
  (interactive)
  (yank)
  (call-interactively 'indent-region))

(load-file "~/etc/undo-tree.el")
(require 'undo-tree)
(global-undo-tree-mode)
(define-key undo-tree-map (kbd "C-/") nil)

(setq tramp-default-method "ssh")
(setq tramp-default-user "joeg")
(require 'tramp)

(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)
;; Without these two lines when I try to reopen a file in a new frame it jumps to the old one >_<
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)
(setq completion-ignored-extensions (append completion-ignored-extensions '(".fpo" ".ii")))

(require 'breadcrumb)
(setq bc-bookmark-limit 10000)
(global-set-key (kbd "C-S-SPC")         'bc-set) ;; Shift-SPACE for set bookmark
(global-set-key [(control meta j)]      'bc-previous) ;; M-j for jump to previous
(global-set-key [(control meta k)]      'bc-next) ;; Shift-M-j for jump to next
(global-set-key [(control meta l)]      'bc-goto-current) ;; C-c j for jump to current bookmark
(global-set-key [(control x)(control j)]        'bc-list) ;; C-x M-j for the bookmark menu list

(if (> (display-pixel-width) 1280)
	(add-to-list 'default-frame-alist '(font . "Consolas-12"))
  (add-to-list 'default-frame-alist '(font . "Consolas-11")))

;; Color theme
(require 'color-theme)
(setq color-theme-is-global t)
(color-theme-initialize)
(color-theme-euphoria)

;; Turn off GUI parts
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq visible-bell t)
(fset 'yes-or-no-p 'y-or-n-p) ;; Make all "yes or no" prompts be "y or n" instead

;; Get rid of the visual bell for some common 'errors'
(setq ring-bell-function
      (lambda ()
		(unless (memq this-command
                          '(isearch-abort abort-recursive-edit exit-minibuffer keyboard-quit))
		  (ding))))

;; Show me the region until I do something on it
(setq transient-mark-mode t)

;; Make killing the line also delete it
(setq kill-whole-line t)

;; Stop this crazy blinking cursor
(blink-cursor-mode 0)

;; when on a TAB, the cursor has the TAB length
(setq-default x-stretch-cursor t)

;; Make emacs use the normal clipboard
(setq x-select-enable-clipboard t)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

;; Show column number in the mode line
(column-number-mode 1)

;; Show current buffer name in titlebar (instead of emacs@whatever)
(setq frame-title-format "%b")

;; Scroll 1 line at a time
(setq scroll-step 1)

;; God, the emacs people do think of everything
;;(mouse-avoidance-mode 'jump)

;; A more useful C-a
(defun beginning-or-indentation (&optional n)
  "Move cursor to beginning of this line or to its indentation.
  If at indentation position of this line, move to beginning of line.
  If at beginning of line, move to beginning of previous line.
  Else, move to indentation position of this line.
  With arg N, move backward to the beginning of the Nth previous line.
  Interactively, N is the prefix arg."
  (interactive "P")
  (cond ((or (bolp) n)
	 (forward-line (- (prefix-numeric-value n))))
	((save-excursion (skip-chars-backward " \t") (bolp)) ; At indentation.
	 (forward-line 0))
	(t (back-to-indentation))))
(global-set-key [(control a)] 'beginning-or-indentation)

;; Make C-w consistent with shell usage
;; Rebinds cut to C-x C-k though
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)

;; Don't use alt-x, use C-x C-m, alt is a pain
(global-set-key "\C-x\C-m" 'execute-extended-command)

;; Emacs won't load shell-script-mode for zsh automatically
(setq auto-mode-alist
      (append
       ;; File name (within directory) starts with a dot.
       '(("zshrc" . shell-script-mode))
       auto-mode-alist))

;; Prefer 4-space tabs
(setq c-default-style "bsd")
(setq-default c-basic-offset 4)
(setq-default indent-tabs-mode nil)
(setq default-tab-width 4)
(setq tab-width 4)
(c-set-offset 'case-label '+)     ;; 'case' indented once after 'switch'

;; For most modes I'm coding, I don't want line wrap
(setq-default truncate-lines t)

;; Show matching parentheses
(show-paren-mode 1)

(setq auto-mode-alist (cons '("\\.incl$" . c++-mode) auto-mode-alist))

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

;; So I can delete it
(setq show-trailing-whitespace t)

;; Delete trailing whitespace automagically
(add-hook 'write-file-hooks
  (lambda ()
    (delete-trailing-whitespace)))

;; Most useful binding ever
(global-set-key (kbd "C-/") 'comment-or-uncomment-region) ;; C-S-_ does undo already

;; By default compilation frame is half the window. Yuck.
(setq compilation-window-height 8)

 ;; keep the window focused on the messages during compilation
(setq compilation-scroll-output t)

 ;; Keep the highlight on the compilation error
(setq next-error-highlight t)

(defun buffer-mode (buffer-or-string)
  "Returns the major mode associated with a buffer."
  (save-excursion
    (set-buffer buffer-or-string)
    major-mode))

;; When compiling, make the compile window go away when finished if there are no errors
(setq compilation-finish-function
      (lambda (buf str)
        (if (string-match "exited abnormally" str)

            ;;there were errors
            (message "compilation errors, press C-x ` to visit")

          ;;no errors, make the compilation window go away in 0.5 seconds
          (run-at-time 0.5 nil 'delete-windows-on buf)
          (message "NO COMPILATION ERRORS!"))))

(global-set-key "\M-j" 'previous-buffer)
(global-set-key "\M-k" 'next-buffer)

(defun indent-newline-indent ()
  (interactive)
  (progn
	(indent-according-to-mode)
	(newline-and-indent)))

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-m") 'newline-and-indent)

;; If I'm searching and I hit backspace, I mean backspace dammit.
(define-key isearch-mode-map '[backspace] 'isearch-delete-char)

(add-hook 'c-mode-common-hook
		  (lambda ()
			(setq c-hungry-delete-key t)
			(local-set-key (kbd "C-d") 'c-hungry-delete-forward)
			(local-set-key (kbd "DEL") 'c-hungry-delete-forward)
			(local-set-key (kbd "<backspace>") 'c-hungry-delete-backwards)))

(defun close-frame-or-exit ()
  "Tries to close the current frame, if it's the only one left just exits."
  (interactive)
  (if (= (length (frame-list)) 1)
	  (save-buffers-kill-emacs)
	(delete-frame)))

;; Close windows, not emacs.
(global-set-key "\C-x\C-c" 'close-frame-or-exit)

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

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

(global-set-key (kbd "C-x K") 'kill-other-buffers-of-this-file-name)

(defun kill-other-buffers-of-this-file-name (&optional buffer)
  "Kill all other buffers visiting files of the same base name."
  (interactive "bBuffer to make unique: ")
  (setq buffer (get-buffer buffer))
  (cond ((buffer-file-name buffer)
		 (let ((name (file-name-nondirectory (buffer-file-name buffer))))
		   (loop for ob in (buffer-list)
				 do (if (and (not (eq ob buffer))
							 (buffer-file-name ob)
							 (let ((ob-file-name (file-name-nondirectory (buffer-file-name ob))))
							   (or (equal ob-file-name name)
								   (string-match (concat name "\\.~.*~$") ob-file-name))) )
						(kill-buffer ob)))))
		(default (message "This buffer has no file name."))))

(defun unindent-region-with-tab ()
  (interactive)
  (save-excursion
	(if (< (point) (mark)) (exchange-point-and-mark))
	(let ((save-mark (mark)))
	  (if (= (point) (line-beginning-position)) (previous-line 1))
	  (goto-char (line-beginning-position))
	  (while (>= (point) save-mark)
		(goto-char (line-beginning-position))
		(if (= (string-to-char "\t") (char-after (point))) (delete-char 1))
		(previous-line 1)))))

(defun unindent-block()
  (interactive)
  (shift-region (- tab-width))
  (setq deactivate-mark nil))

(defun shift-region(numcols)
  (if (< (point)(mark))
    (if (not(bolp))    (progn (beginning-of-line)(exchange-point-and-mark) (end-of-line)))
    (progn (end-of-line)(exchange-point-and-mark)(beginning-of-line)))
  (setq region-start (region-beginning))
  (setq region-finish (region-end))
  (save-excursion
    (if (< (point) (mark)) (exchange-point-and-mark))
    (let ((save-mark (mark)))
      (indent-rigidly region-start region-finish numcols))))

(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\M-%" 'query-replace-regexp)

(add-to-list 'load-path "~/etc/")
(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(autoload 'ack-find-same-file "full-ack" nil t)
(autoload 'ack-find-file "full-ack" nil t)
(global-set-key "\M-k" 'ack)

;; Threshold after which we consider the file to be large
;; and don't want to do anything too expensive.
(setq uncomfortable-buffer-size (* 10 1024 1024))

;; Make more notepad like out of the box
(setq default-major-mode 'text-mode)
(setq text-mode-hook				; Enable auto-fill-mode
	  '(lambda ()
		 (let ((ext (file-name-extension (buffer-file-name))))
		   (when (and (not (or (string-equal ext "tc")
                               (string-equal ext "in")
                               (string-equal ext "tmp")
                               (string-equal ext "log")))
                      (< (buffer-size) uncomfortable-buffer-size))
             (longlines-mode 1)))))

;; Taken from Trey Jackson's answer on superuser.com
;; http://superuser.com/questions/205420/how-can-i-interrupt-emacs-opening-a-large-file
(defun my-find-file-check-make-large-file-read-only-hook ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) uncomfortable-buffer-size)
    (setq buffer-read-only t)
    (setq auto-save-default nil)
    (buffer-disable-undo)
    (fundamental-mode)
	(message "Large buffer: Undo disabled, made read only, autosave disabled.")))
(add-hook 'find-file-hooks 'my-find-file-check-make-large-file-read-only-hook)

;; AWESOMENESS
(require 'cc-mode)
(c-subword-mode 1) ;; lets you delete camelcase words one at a time

(defun my-delete-leading-whitespace (start end)
  "Delete whitespace at the beginning of each line in region."
  (interactive "*r")
  (save-excursion
    (if (not (bolp)) (forward-line 1))
    (delete-whitespace-rectangle (point) end nil)))
(global-set-key "\C-x\C-h" 'my-delete-leading-whitespace)

(add-hook 'c-mode-common-hook
		  (lambda ()
            (load-file "~/etc/emacs/c-common.el")))

(add-hook 'java-mode-hook
          (lambda ()
            (load-file "~/etc/emacs/java.el")))