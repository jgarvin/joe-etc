;; -*- lexical-binding: t -*-

(defvar-local run-command nil)

;; Enable helm-gtags-mode
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

;; Set key bindings
(eval-after-load "helm-gtags"
  '(progn
     (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-find-tag)
     (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-find-rtag)
     ;; (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
     ;; (define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
     (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
     (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
     ;; (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
     ))

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

(defun replace-in-string (s find-this replace-with-this)
  (replace-regexp-in-string s (regexp-quote find-this) replace-with-this))

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
  (setq whitespace-line-column 300)
  (whitespace-mode t)

  ;;(setq require-final-newline t)

  (setq c-hungry-delete-key t)
  (local-set-key (kbd "C-d") 'c-hungry-delete-forward)
  (local-set-key (kbd "<delete>") 'c-hungry-delete-forward)
  (local-set-key (kbd "<backspace>") 'c-hungry-delete-backwards)
  (local-set-key (kbd "C-M-y") #'sp-slurp-hybrid-sexp)
  (local-set-key (kbd "C-M-k") #'sp-kill-hybrid-sexp)
  ;; Prefer 4-space tabs

;;  (c-set-offset 'innamespace 0) ;; don't indent top level namespace
   ;; don't indent top level namespace
  (c-set-offset 'case-label '+) ;; 'case' indented once after 'switch'

  (setq c-default-style "bsd")
  (setq indent-tabs-mode nil)
  (etc-set-indent-preference 4)
  (setq c-indent-comments-syntactically-p t)

  ;; public void veryLongMethodNameHereWithArgs(
  ;;         String arg1,
  ;;         String arg2,
  ;;         int arg3)
  (c-set-offset 'arglist-intro '+)

  ;; none of these help the enum problem
  ;; (c-set-offset 'statement-cont 0)
  ;; (c-set-offset 'brace-list-open 0)
  ;; (c-set-offset 'brace-list-close 0)
  )

(defun etc-toggle-namespace-indent ()
  (interactive)
  (if (equal (assoc 'innamespace c-offsets-alist) 0)
      (c-set-offset 'innamespace my-indent-size)
    (c-set-offset 'innamespace 0)))

(defun etc-set-indent-preference (n)
  (interactive "nNumber of spaces: ")
  (setq my-indent-size n)
  (setq c-basic-offset my-indent-size)
  (setq default-tab-width my-indent-size)
  (setq tab-width my-indent-size)
  (when (not (equal (assoc 'innamespace c-offsets-alist) 0))
    (c-set-offset 'innamespace my-indent-size)))

(require 'font-lock)

(defun --copy-face (new-face face)
  "Define NEW-FACE from existing FACE."
  (copy-face face new-face)
  (eval `(defvar ,new-face nil))
  (set new-face new-face))

(--copy-face 'font-lock-label-face  ; labels, case, public, private, proteced, namespace-tags
             'font-lock-keyword-face)
(--copy-face 'font-lock-doc-markup-face ; comment markups such as Javadoc-tags
             'font-lock-doc-face)
(--copy-face 'font-lock-doc-string-face ; comment markups
             'font-lock-comment-face)

;;(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; (defun etc-c++-mode-hook ()
;;   (font-lock-add-keywords
;;    nil '(;; complete some fundamental keywords
;;          ("\\<\\(void\\|unsigned\\|signed\\|char\\|short\\|bool\\|int\\|long\\|float\\|double\\)\\>" . font-lock-keyword-face)
;;          ;; add the new C++11 keywords
;;          ("\\<\\(alignof\\|alignas\\|constexpr\\|decltype\\|noexcept\\|nullptr\\|static_assert\\|thread_local\\|override\\|final\\)\\>" . font-lock-keyword-face)
;;          ("\\<\\(char[0-9]+_t\\)\\>" . font-lock-keyword-face)
;;          ;; PREPROCESSOR_CONSTANT
;;          ("\\<[A-Z]+[A-Z_]+\\>" . font-lock-constant-face)
;;          ;; hexadecimal numbers
;;          ("\\<0[xX][0-9A-Fa-f]+\\>" . font-lock-constant-face)
;;          ;; integer/float/scientific numbers
;;          ("\\<[\\-+]*[0-9]*\\.?[0-9]+\\([ulUL]+\\|[eE][\\-+]?[0-9]+\\)?\\>" . font-lock-constant-face)
;;          ;; user-types (customize!)
;;          ("\\<[A-Za-z_]+[A-Za-z_0-9]*_\\(t\\|type\\|ptr\\)\\>" . font-lock-type-face)
;;          ("\\<\\(xstring\\|xchar\\)\\>" . font-lock-type-face)
;;          ("auto ?&? +\(\\w+\)" 1 'font-lock-variable-name-face)
;;          )))

;; (defadvice c-lineup-arglist (around my activate)
;;   "Improve indentation of continued C++11 lambda function opened as argument."
;;   (setq ad-return-value
;;         (if (and (equal major-mode 'c++-mode)
;;                  (ignore-errors
;;                    (save-excursion
;;                      (goto-char (c-langelem-pos langelem))
;;                      ;; Detect "[...](" or "[...]{". preceded by "," or "(",
;;                      ;;   and with unclosed brace.
;;                      (looking-at ".*[(,][ \t]*\\[[^]]*\\][ \t]*[({][^}]*$"))))
;;             0                           ; no additional indent
;;           ad-do-it)))                   ; default behavior

;; This hack fixes indentation for C++11's "enum class" in Emacs.
;; http://stackoverflow.com/questions/6497374/emacs-cc-mode-indentation-problem-with-c0x-enum-class/6550361#6550361
(defun inside-class-enum-p (pos)
  "Checks if POS is within the braces of a C++ \"enum class\"."
  (ignore-errors
    (save-excursion
      (goto-char pos)
      (up-list -1)
      (backward-sexp 1)
      (or (looking-back "enum\\s-+class\\s-+")
          (looking-back "enum\\s-+class\\s-+\\S-+\\s-*:\\s-*")))))

(defun inside-enum-p (pos)
  "Checks if POS is within the braces of a C++ \"enum\"."
  (ignore-errors
    (save-excursion
      (goto-char pos)
      (up-list -1)
      (backward-sexp 1)
      (or (looking-back "enum\\s-+")
          (looking-back "enum\\s-+\\S-+\\s-*:\\s-*")))))

(defun align-enum-class (langelem)
  (if (inside-class-enum-p (c-langelem-pos langelem))
      0
    (c-lineup-topmost-intro-cont langelem)))

(defun align-enum-class-closing-brace (langelem)
  (if (or (inside-class-enum-p (c-langelem-pos langelem))
          (inside-enum-p (c-langelem-pos langelem)))
      '-
    '+))

(defun fix-enum-class ()
  "Setup `c++-mode' to better handle \"class enum\"."
  (add-to-list 'c-offsets-alist '(topmost-intro-cont . align-enum-class))
  (add-to-list 'c-offsets-alist
               '(statement-cont . align-enum-class-closing-brace)))

(add-hook 'c-mode-common-hook 'etc-setup-c-common)
(add-hook 'c++-mode-hook 'fix-enum-class)
;; (add-hook 'c++-mode-hook #'etc-c++-mode-hook)
