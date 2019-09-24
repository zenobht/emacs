(use-package git-gutter
  :defer t
  :after magit
  :init
  (global-git-gutter-mode 1)
  )

(use-package evil-magit
  :defer t
  :after evil
  :init
  (evil-magit-init)
  :config
  (bind-key "q" #'my/magit-kill-buffers magit-status-mode-map)
  )

(provide 'b-git)
