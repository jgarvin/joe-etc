;; (helm-flx-mode +1)

(require 'helm-config)

(helm-mode 1)
;; (global-set-key (kbd "M-x") 'helm-M-x)
;; (global-set-key (kbd "M-S-y") 'helm-show-kill-ring)
;; (global-set-key (kbd "C-x b") 'helm-mini)
;; (global-set-key (kbd "C-x C-f") 'helm-find-files)
;; (global-set-key (kbd "C-c h o") 'helm-occur)
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