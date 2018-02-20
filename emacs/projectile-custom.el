(use-package
  projectile
  :ensure t)

(projectile-global-mode 1)

(defun etc-get-project ()
  (if (projectile-project-p)
      (projectile-project-name)
    (with-current-buffer (etc-get-project-buffer)
      (projectile-project-name))))

(defun etc-get-project-root ()
  (if (projectile-project-p)
      (projectile-project-root)
    (with-current-buffer (etc-get-project-buffer)
      (projectile-project-root))))

(defun etc-get-project-buffer ()
  (or
   (and (projectile-project-p) (current-buffer))
   (-any (lambda (x) (with-current-buffer x (and (projectile-project-p) x))) (buffer-list))
   (current-buffer)))

(defmacro with-most-recent-project (&rest body)
  "Execute the forms in BODY with most recently visited project current.
The value returned is the value of the last form in BODY."
  (declare (indent 1) (debug t))
  `(if (projectile-project-p)
       (progn
         ,@body)
     (let ((default-directory (with-current-buffer (etc-get-project-buffer) (file-name-directory (buffer-file-name)))))
       ,@body)))



;; ;; allow projectile to use external utils to speed up indexing
;; (setq projectile-indexing-method 'alien)

;; ;; when switching to a project open the project root automatically rather than prompting for a folder
;; (setq projectile-switch-project-action 'projectile-dired)

;; ;; without this indexing some projects is seriously slow
;; (setq projectile-enable-caching t)

;; ;; without this accessing files via /sudo:: will freeze for some reason.
;; ;; see: https://github.com/bbatsov/prelude/issues/594
;; (setq projectile-mode-line " Projectile")

;; ;; disable projectile on remote hosts, too slow
;; (defadvice projectile-project-root (around ignore-remote first activate)
;;   (unless (file-remote-p default-directory) ad-do-it))

;; (setq projectile-use-git-grep t)