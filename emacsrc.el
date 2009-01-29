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