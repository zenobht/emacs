
(use-package projectile
  :defer t
  :init (projectile-global-mode)
  :config
  (setq projectile-enable-caching t
        projectile-indexing-method 'alien
        projectile-globally-ignored-file-suffixes
        '("#" "~" ".swp" ".o" ".so" ".exe" ".dll" ".elc" ".pyc" ".jar" "*.class")
        projectile-globally-ignored-directories
        '(".git" "node_modules" "__pycache__" ".vs")
        projectile-globally-ignored-files '("TAGS" "tags" ".DS_Store")
        )
  )

(use-package counsel-projectile
  :defer t
  :after projectile evil evil-leader
  :init
  (setq counsel-projectile-remove-current-buffer t)
  )


