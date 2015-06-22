(projectile-global-mode)

(setq projectile-indexing-method 'native)

(setq projectile-enable-cathing t)

(setq projectile-switch-project-action 'projectile-dired)

(setq projectile-globally-ignored-file-suffixes '("o" "cache" "a" "dll" "exe" "rar" "doc"))

(add-to-list 'projectile-project-root-files-bottom-up ".dir-locals.el")

(setq projectile-project-root-files (append '(".dir-locals.el" ".projectile") projectile-project-root-files))


