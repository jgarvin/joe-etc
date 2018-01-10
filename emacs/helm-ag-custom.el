(use-package
  helm-ag
  :ensure t
  :pin melpa-stable)

(setq helm-ag-base-command "pt -e --nocolor --nogroup")

(define-key projectile-mode-map (kbd "C-c p s g") nil)
(global-set-key (kbd "C-c p s g") #'helm-do-ag-project-root)

(defun etc-helm-do-ag ()
  (interactive)
  (helm-do-ag default-directory))

(define-key projectile-mode-map (kbd "C-c p s d") nil)
(global-set-key (kbd "C-c p s d") #'etc-helm-do-ag)

(setq helm-ag-insert-at-point 'symbol)

(defun etc-helm-ag-source-only ()
  (interactive)
  (let ((helm-ag-ignore-buffer-patterns '("\\.txt\\'" "\\.mkd\\'" "\\.json\\'"))
        (source-directory (file-name-as-directory (projectile-project-root))))
    (message "%s" (concat source-directory "source"))
    (helm-do-ag (concat source-directory "source"))
    ))

(define-key projectile-mode-map (kbd "C-c p s s") nil)
(global-set-key (kbd "C-c p s s") #'etc-helm-ag-source-only)
