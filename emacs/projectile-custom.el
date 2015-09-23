(projectile-global-mode)

;; allow projectile to use external utils to speed up indexing
(setq projectile-indexing-method 'alien)

;; when switching to a project open the project root automatically rather than prompting for a folder
(setq projectile-switch-project-action 'projectile-dired)

;; without this indexing some projects is seriously slow
(setq projectile-enable-caching t)

;; without this accessing files via /sudo:: will freeze for some reason.
;; see: https://github.com/bbatsov/prelude/issues/594
(setq projectile-mode-line " Projectile")

;; disable projectile on remote hosts, too slow
(defadvice projectile-project-root (around ignore-remote first activate)
  (unless (file-remote-p default-directory) ad-do-it))