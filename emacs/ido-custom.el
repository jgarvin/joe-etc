(ido-mode t)
(ido-everywhere 1)

(setq ido-enable-flex-matching t
      ido-use-filename-at-point 'guess)

;; Without this, when running emacs as sudo .ido.last will become
;; root owned. Super annoying.
(setq ido-save-directory-list-file (concat "~/.ido." (getenv "LOGNAME") ".last"))

;; Without these two lines when I try to reopen a file in a new frame it jumps to the old one >_<
(setq ido-default-file-method 'selected-window)
(setq ido-default-buffer-method 'selected-window)

(setq completion-ignored-extensions
      (append completion-ignored-extensions '(".fpo" ".ii" ".d" ".o")))

(setq ido-ignore-files
      (append ido-ignore-files '(".*-g" ".*-O2")))

;; Needed for ido-mode to work in large source trees
(setq ido-max-directory-size 3000000)

;; without this ido will randomly search in other dirs for files
(setq ido-auto-merge-work-directories-length -1)

