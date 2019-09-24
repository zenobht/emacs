(use-package whitespace
  :defer t
  :init
  (setq whitespace-style '(face trailing spaces tabs newline tab-mark newline-mark)
        show-trailing-whitespace t
        whitespace-display-mappings
        ;; all numbers are unicode codepoint in decimal. e.g. (insert-char 182 1)
        '(
          (space-mark 32 [183] [46]) ; SPACE 32 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
          (newline-mark 10 [172 10]) ; LINE FEED,
          (tab-mark 9 [9655 9] [92 9]) ; tab
          )
        )
  )

(use-package dumb-jump
  :defer t
  :ensure ivy
  :config
  (setq dumb-jump-selector 'ivy))

(use-package rainbow-delimiters
  :defer t)

(use-package yasnippet
  :defer t
  :after company
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode +1)
  :config
  ;; (add-to-list 'company-backends #'company-mode/backend-with-yas)
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
  (company-tng-configure-default)
  (setq
   company-idle-delay 0 ; Delay to complete
   company-dabbrev-downcase 0
   company-minimum-prefix-length 2
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

(use-package hideshow
  :defer t)

(use-package electric
  :defer t
  :init
  (electric-pair-mode 1)
  :config
  (push '(?\' . ?\') electric-pair-pairs)
  )

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
  (add-to-list 'company-backends #'company-tabnine)
  :config
  (setq company-tabnine-auto-balance nil)
  )

(use-package highlight-thing
  :defer t
  :config
  (setq highlight-thing-case-sensitive-p t
        highlight-thing-limit-to-region-in-large-buffers-p nil
        highlight-thing-narrow-region-lines 15
        highlight-thing-large-buffer-limit 1000
        highlight-thing-prefer-active-region t)
  )

(use-package flycheck
  :defer t
  :config
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
  :config
  (bind-key "q" #'my/magit-kill-buffers magit-status-mode-map)
  )

;;;###autoload
(defun my/editor-text-config ()
  (whitespace-mode +1)
  (display-line-numbers-mode +1)
  (visual-line-mode -1)
  )

;;;###autoload
(defun my/editor-prog-config ()
  (whitespace-mode +1)
  (rainbow-delimiters-mode +1)
  (display-line-numbers-mode +1)
  (hs-minor-mode +1)
  (rainbow-mode +1)
  (highlight-thing-mode +1)
  (flycheck-mode +1)
  )

(provide 'b-editor)
