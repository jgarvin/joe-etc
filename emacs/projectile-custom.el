(projectile-global-mode)

;; allow projectile to use external utils to speed up indexing
(setq projectile-indexing-method 'alien)

;; when switching to a project open the project root automatically rather than prompting for a folder
(setq projectile-switch-project-action 'projectile-dired)

;; without this indexing some projects is seriously slow
(setq projectile-enable-caching t)


