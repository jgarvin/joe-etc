(use-package
  helm
  :ensure t
  :pin melpa-stable)


(use-package
  helm-projectile
  :ensure t
  :pin melpa-stable)


(use-package
  helm-swoop
  :ensure t
  :pin melpa-stable)

(use-package
  helm-gtags
  :ensure t
  :pin melpa-stable)


;; (helm-flx-mode +1)

(require 'helm-config)

(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-S-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c h o") 'helm-occur)
(setq projectile-completion-system 'helm)

(require 'helm-projectile)
(helm-projectile-on)

(global-set-key (kbd "C-x b") #'helm-mini)

;; try without for awhile, see how it goes
(setq helm-mode-fuzzy-match t) ;; doesn't work?
(setq helm-completion-in-region-fuzzy-match 1)
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)
(setq helm-semantic-fuzzy-match t
      helm-imenu-fuzzy-match    t)
(setq helm-locate-fuzzy-match t)
(setq helm-apropos-fuzzy-match t)
(setq helm-lisp-fuzzy-completion t)
(setq helm-M-x-fuzzy-match t)
(helm-autoresize-mode 1)

(require 'helm-swoop)

;; Change keybinds to whatever you like :)
(global-set-key (kbd "M-i") 'helm-swoop)
;; (global-set-key (kbd "C-s") 'helm-swoop)
;; (global-set-key (kbd "C-r") 'helm-swoop)
;; (global-set-key (kbd "C-s") 'isearch-forward)
;; (global-set-key (kbd "C-r") 'isearch-backward)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)
(global-set-key (kbd "C-S-y") 'helm-show-kill-ring)

;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)

;; Save buffer when helm-multi-swoop-edit complete
(setq helm-multi-swoop-edit-save t)

;; If this value is t, split window inside the current window
(setq helm-swoop-split-with-multiple-windows nil)

;; Split direction. 'split-window-vertically or 'split-window-horizontally
(setq helm-swoop-split-direction 'split-window-vertically)

;; If nil, you can slightly boost invoke speed in exchange for text color
(setq helm-swoop-speed-or-color nil)

;; Go to the opposite side of line from the end or beginning of line
(setq helm-swoop-move-to-line-cycle t)

;; Optional face for line numbers
;; Face name is `helm-swoop-line-number-face`
(setq helm-swoop-use-line-number-face t)

(setq helm-ff-file-name-history-use-recentf t)

;; disable pre-input
(setq helm-swoop-pre-input-function (lambda () ""))

(defun etc-no-helm-M-x ()
  (interactive)
  (let ((helm-completing-read-handlers-alist '((execute-extended-command . nil))))
    (call-interactively 'execute-extended-command)))
(global-set-key (kbd "C-c m x") #'etc-no-helm-M-x)

(setq helm-buffer-max-length 30)

;; for some reason in your home versions they changed the default,
;; restore the default here.
;; https://github.com/emacs-helm/helm/commit/1de1701c73b15a86e99ab1c5c53bd0e8659d8ede
(assq-delete-all 'find-file helm-completing-read-handlers-alist)
(assq-delete-all 'execute-extended-command helm-completing-read-handlers-alist)

;; I never want to toggle auto updating and this interferes with the normal binding for control backspace
(define-key helm-projectile-find-file-map (kbd "C-<backspace>") nil)

(setq helm-candidate-number-limit 500)

(advice-add 'helm-projectile-find-file :around #'etc-ignore-bug)

(setq helm-ag-base-command "ag --nocolor --nogroup")

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


;; Enable helm-gtags-mode
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)


(when (< emacs-major-version 26)

;;;; helm support
  (defvar helm-buffer)
  (defvar helm-candidate-separator)
  (defvar helm-alive-p)
  (declare-function with-helm-buffer "ext:helm-lib.el" (&rest body))
  (declare-function helm-candidate-number-at-point "ext:helm.el")
  (declare-function helm-pos-header-line-p "ext:helm.el")

  (defun linum-relative-for-helm ()
    (with-helm-buffer
      (make-local-variable 'linum-relative-last-pos))
    (linum-update helm-buffer))

  (add-hook 'helm-move-selection-after-hook 'linum-relative-for-helm)

;;;; Advices
  (defadvice linum-update (before relative-linum-update activate)
    "This advice get the last position of linum."
    (if (and (boundp 'helm-alive-p) helm-alive-p)
        (setq linum-relative-last-pos (helm-candidate-number-at-point))
      (setq linum-relative-last-pos (line-number-at-pos))))

;;;; Functions
  (defun linum-relative (line-number)
    (when (and (boundp 'helm-alive-p) helm-alive-p)
      (with-helm-buffer
        (if (looking-at helm-candidate-separator)
            (setq line-number (save-excursion
                                (forward-line 1) (helm-candidate-number-at-point)))
          (setq line-number (helm-candidate-number-at-point)))))
    (let* ((diff1 (abs (- line-number linum-relative-last-pos)))
           (diff (if (minusp diff1)
                     diff1
                   (+ diff1 linum-relative-plusp-offset)))
           (current-p (= diff linum-relative-plusp-offset))
           (current-symbol (if (and linum-relative-current-symbol current-p)
                               (if (string= "" linum-relative-current-symbol)
                                   (number-to-string line-number)
                                 linum-relative-current-symbol)
                             (number-to-string diff)))
           (face (if current-p 'linum-relative-current-face 'linum)))
      (if (and (boundp 'helm-alive-p)
               helm-alive-p
               (with-helm-buffer
                 (or (looking-at helm-candidate-separator)
                     (eq (point-at-bol) (point-at-eol))
                     (helm-pos-header-line-p))))
          (propertize (format linum-relative-format current-symbol) 'invisible t)
        (propertize (format linum-relative-format current-symbol) 'face face))))
  )