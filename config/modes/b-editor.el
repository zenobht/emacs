(use-package whitespace
  :defer t
  )

(use-package dumb-jump
  :defer t
  :ensure ivy
  :config
  (setq dumb-jump-selector 'ivy))

(use-package yasnippet
  :defer t
  :after company
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode +1)
  :config
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
  )

(use-package yasnippet-snippets
  :defer
  :after yasnippet
  )

(use-package company
  :defer t
  :init
  (global-company-mode)
  :config
  (setq
   company-idle-delay 0 ; Delay to complete
   company-dabbrev-downcase 0
   company-minimum-prefix-length 0
   company-selection-wrap-around t ; Loops around suggestions
   company-show-numbers t
   company-frontends '(
                       company-tng-frontend
                       company-pseudo-tooltip-frontend
                       company-echo-metadata-frontend
                       )
   )
  )

(use-package display-line-numbers
  :defer t)

(use-package expand-region
  :defer t
  :after evil
  )

(use-package rainbow-mode
  :defer t)

(use-package hl-line
  :defer t
  :init
  (global-hl-line-mode 1)
  )

(use-package hippie-exp
  :defer t
  :config
  (setq-default hippie-expand-try-functions-list
                '(yas-hippie-try-expand emmet-expand-line)))

(use-package company-tabnine
  :defer t
  :after company
  :init
  (setq company-tabnine-auto-balance nil)
  (add-to-list 'company-backends #'company-tabnine)
  :config
  )

(use-package highlight-thing
  :config
  (setq highlight-thing-case-sensitive-p t
        highlight-thing-delay-seconds 0.5
        highlight-thing-limit-to-region-in-large-buffers-p nil
        highlight-thing-narrow-region-lines 15
        highlight-thing-large-buffer-limit 1000
        highlight-thing-prefer-active-region t)
  )

(use-package flycheck
  :defer t
  :config
  (setq flycheck-checker-error-threshold 2000)
  (advice-add 'flycheck-eslint-config-exists-p :override (lambda() t))
  )

(use-package git-gutter
  :defer t
  :init
  (global-git-gutter-mode 1)
  )

(use-package evil-magit
  :defer t
  :after evil
  :init
  (evil-magit-init)
  :config
  (add-to-list 'magit-no-confirm 'stage-all-changes)
  (bind-key "q" #'my/magit-kill-buffers magit-status-mode-map)
  )

(use-package editorconfig
  :defer t
  :config
  (editorconfig-mode 1)
  )

(provide 'b-editor)
