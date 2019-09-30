(require 'projectile)

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
