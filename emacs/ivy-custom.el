;; Ivy and Counsel
(use-package ivy :ensure t
  :diminish (ivy-mode . "")
  :config
  (ivy-mode 1)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)
  ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (setq ivy-use-virtual-buffers t)
  ;; number of result lines to display
  (setq ivy-height 10)
  ;; no regexp by default
  (setq ivy-initial-inputs-alist nil)
  ;; configure regexp engine.
  (setq ivy-re-builders-alist
  ;; allow input not in order
        '((t   . ivy--regex-ignore-order))))

(use-package ivy-hydra :ensure t)

(use-package counsel :ensure t)
(use-package counsel-projectile :ensure t)
(use-package counsel-gtags :ensure t)
(use-package counsel-tramp :ensure t)

;;(setq tramp-default-method "ssh")
(define-key global-map (kbd "C-c s t") 'counsel-tramp)
;;(setq counsel-tramp-custom-connections '(/ssh:domain|sudo:user@localhost:/))

(let ((connections-file (format "/home/%s/.emacs.connections" (user-login-name))))
  (when (file-exists-p connections-file)
    (load-file connections-file)
  ))

;; needed to not get variable is void error...
(setq ivy-regex nil)

(setq projectile-completion-system 'ivy)

;; https://github.com/abo-abo/swiper/issues/2611#issuecomment-675424466
;; doesn't look quite usable out-of-the-box, need to just override down binding
;; (defvar swiper-isearch-map
;;   (let ((map (make-sparse-keymap)))
;;     (set-keymap-parent map swiper-map)
;;     (define-key map (kbd "M-n") 'swiper-isearch-thing-at-point)
;;     (define-key map (kbd "<down>") 'swiper-isearch-next-line)
;;     map)
;;   "Keymap for `swiper-isearch'.")


(defun swiper-isearch-next-line ()
  (interactive)
  (let ((shift 1))
    (with-ivy-window
      (let ((ln (line-number-at-pos (ivy-state-current ivy-last))))
        (while (and (< (+ ivy--index shift) ivy--length)
                    (= ln (line-number-at-pos (nth (+ ivy--index shift) ivy--all-candidates))))
          (cl-incf shift))))
    (ivy-next-line shift)))

(defun swiper-isearch-previous-line ()
  (interactive)
  (let ((shift 1))
    (with-ivy-window
      (let ((ln (line-number-at-pos (ivy-state-current ivy-last))))
        (while (and (< (+ ivy--index shift) ivy--length)
                    (= ln (line-number-at-pos (nth (+ ivy--index shift) ivy--all-candidates))))
          (cl-incf shift))))
    (ivy-previous-line shift)))

(define-key swiper-isearch-map (kbd "M-n") 'swiper-isearch-thing-at-point)
(define-key swiper-isearch-map (kbd "<down>") #'swiper-isearch-next-line)
(define-key swiper-isearch-map (kbd "<up>") #'swiper-isearch-previous-line)

(global-set-key (kbd "C-r") 'swiper-isearch-backward)
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "C-S-y") 'counsel-yank-pop)

(defun etc-set-eshell-keys ()
  (define-key eshell-mode-map (kbd "M-r") #'counsel-esh-history))

(add-hook 'eshell-mode-hook #'etc-set-eshell-keys)

(setq counsel-ag-base-command
      ;;"pt -e --nocolor --nogroup %s"
      "ag --nocolor --nogroup %s" ;; works better, counsel-ag support for pt has issues
      )

(defun etc-ivy-search-project-root ()
  (interactive)
  (counsel-ag (thing-at-point 'symbol) (etc-get-project-root)))

(define-key projectile-mode-map (kbd "C-c p s g") nil)
(global-set-key (kbd "C-c p s g") #'etc-ivy-search-project-root)

(defun etc-ivy-search-current-dir ()
  (interactive)
  (counsel-ag (thing-at-point 'symbol) default-directory))

(define-key projectile-mode-map (kbd "C-c p s d") nil)
(global-set-key (kbd "C-c p s d") #'etc-ivy-search-current-dir)

(defun etc-ivy-search-source-only ()
  (interactive)
  (let ((helm-ag-ignore-buffer-patterns '("\\.txt\\'" "\\.mkd\\'" "\\.json\\'"))
        (source-directory (file-name-as-directory (projectile-project-root))))
    (counsel-ag (thing-at-point 'symbol) (concat source-directory "source"))
    ))

(define-key projectile-mode-map (kbd "C-c p s s") nil)
(global-set-key (kbd "C-c p s s") #'etc-ivy-search-source-only)


(add-hook 'c-mode-hook 'counsel-gtags-mode)
(add-hook 'c++-mode-hook 'counsel-gtags-mode)

(with-eval-after-load 'counsel-gtags
  (define-key counsel-gtags-mode-map (kbd "M-.") 'counsel-gtags-find-definition)
  (define-key counsel-gtags-mode-map (kbd "M-,") 'counsel-gtags-find-reference)
  ;;(define-key counsel-gtags-mode-map (kbd "M-s") 'counsel-gtags-find-symbol)
  ;;(define-key counsel-gtags-mode-map (kbd "M-,") 'counsel-gtags-go-backward)
  )

;; This makes it possible to press up when you are on the top choice
;; and select what you have typed into the prompt instead of any of
;; the options. You need this to be able to make new files with names
;; that are substrings of names of existing files.
(setq ivy-use-selectable-prompt t)

;; without this color codes show up in gtags
(setq grep-command "grep --color=never")
;;(setq grep-find-command "grep --color=never")
;;(setq counsul-grep-command "grep --color=never")
