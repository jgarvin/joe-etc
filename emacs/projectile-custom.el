(use-package
  projectile
  :ensure t)

(projectile-global-mode 1)

;; (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(defvar-local etc-projectile-project-name-cached nil)
(defvar-local etc-projectile-project-root-cached nil)
(defvar-local etc-projectile-cached-default-directory nil
  "Stores the default-directory value when the cache was last updated.")

(defun etc-projectile-project-p ()
  (cond
   ((eq etc-projectile-project-name-cached 'none) nil)
   ((eq etc-projectile-project-root-cached 'none) nil)
   ;; without this experience for freezing navigating directories with
   ;; tramp
   ((and (buffer-file-name (current-buffer)) (file-remote-p (buffer-file-name (current-buffer)))) nil)
   ;; Check if default-directory has changed
   ((and etc-projectile-project-name-cached etc-projectile-cached-default-directory
         (string= default-directory etc-projectile-cached-default-directory)) t)
   (t (condition-case nil
          (let ((project-name (projectile-project-name))
                (project-root (projectile-project-root)))
            (setq etc-projectile-project-name-cached (if project-name project-name 'none))
            (setq etc-projectile-project-root-cached (if project-root project-root 'none))
            (setq etc-projectile-cached-default-directory default-directory)
            (or etc-projectile-project-name-cached etc-projectile-project-root-cached))
        (error nil))))
  )

(defun etc-get-project ()
  (if (etc-projectile-project-p) ;; updates cache
      etc-projectile-project-name-cached
    (with-current-buffer (etc-get-project-buffer)
      (etc-projectile-project-p) ;; updates cache
      etc-projectile-project-name-cached)))

(defun etc-get-project-root ()
  (if (etc-projectile-project-p) ;; updates cache
      etc-projectile-project-root-cached
    (with-current-buffer (etc-get-project-buffer)
      (etc-projectile-project-p) ;; updates cache
      etc-projectile-project-root-cached)))

(defun etc-get-project-buffer ()
  (or
   (and (etc-projectile-project-p) (current-buffer))
   (-any (lambda (x) (with-current-buffer x (and (etc-projectile-project-p) x))) (buffer-list))
   (current-buffer)))

(defmacro with-most-recent-project (&rest body)
  "Execute the forms in BODY with most recently visited project current.
The value returned is the value of the last form in BODY."
  (declare (indent 1) (debug t))
  `(if (etc-projectile-project-p)
       (progn
         ,@body)
     (when-let* ((filename (with-current-buffer (etc-get-project-buffer) (buffer-file-name)))
                 (default-directory (file-name-directory filename)))
       ,@body)))

;; without this projectile became incredibly slow when opening files
;; might make things slightly incorrect but it expires every two hours
(memoize 'projectile-project-dirs)

;; ;; allow projectile to use external utils to speed up indexing
;; (setq projectile-indexing-method 'alien)

;; ;; when switching to a project open the project root automatically rather than prompting for a folder
;; (setq projectile-switch-project-action 'projectile-dired)

;; ;; without this indexing some projects is seriously slow
(setq projectile-enable-caching t)

;; ;; without this accessing files via /sudo:: will freeze for some reason.
;; ;; see: https://github.com/bbatsov/prelude/issues/594
;; (setq projectile-mode-line " Projectile")

;; ;; disable projectile on remote hosts, too slow
;; (defadvice projectile-project-root (around ignore-remote first activate)
;;   (unless (file-remote-p default-directory) ad-do-it))

;; (setq projectile-use-git-grep t)