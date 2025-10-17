;; saves where you were in the buffer
(require 'saveplace)
(setq-default save-place t)

;; saves your minibuffer history
(require 'savehist)
;; make sure to also save the kill ring and the search ring
(setq savehist-additional-variables '(kill-ring search-ring regexp-search-ring))
(savehist-mode t)
(setq savehist-autosave-interval 300
      history-length 1000)
;; (savehist-uninstall)

(recentf-mode 1)
(setq recentf-max-saved-items 500
      recentf-auto-cleanup 'never)
(run-at-time nil 300 #'recentf-save-list)

(desktop-save-mode 1)
(setq desktop-auto-save-timeout 300
      desktop-load-locked-desktop t)

(require 'desktop)
(setq desktop-restore-eager t)

(defun etc-filter-saved-buffers (orig-fun &rest args)
  "Mode only lets you filter by regex for some reason."
  (cl-multiple-value-bind (filename bufname mode) args
    (with-current-buffer bufname
      (cond
       ((derived-mode-p 'special-mode) nil)
       ((get-buffer-process bufname) nil)
       ((not (buffer-file-name (get-buffer bufname))) nil)
       (t (apply orig-fun args))))))

(advice-add 'desktop-save-buffer-p :around #'etc-filter-saved-buffers)

