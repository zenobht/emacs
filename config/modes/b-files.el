(require 'projectile)

(use-package ranger
  :defer t
  :config
  (ranger-override-dired-mode t)
  (setq ranger-cleanup-on-disable t
        ranger-listing-dir-first t
        ranger-persistent-sort t
        )
  (if (not (display-graphic-p))
      (setq ranger-footer-delay nil)
    )
  (when (string= system-type "darwin")
    (setq dired-use-ls-dired nil))
  )

(use-package all-the-icons
  :defer t
  :after neotree
  :init
  (setq neo-theme (if (display-graphic-p) 'icons 'nerd))
  )

(use-package neotree
  :defer
  :after projectile
  :config
  (setq neo-window-width 35
        neo-smart-open t
        neo-show-hidden-files t
        neo-force-change-root t
        projectile-switch-project-action 'neotree-projectile-action
        neo-window-fixed-size nil)
  )

(provide 'b-files)
