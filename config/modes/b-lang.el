(use-package kotlin-mode
  :mode (("\\.kt$" . kotlin-mode))
  :defer t
  :config
  (setq-default kotlin-tab-width 4)
  )


(use-package yaml-mode
  :defer t
  :mode (("\\.yml$" . yaml-mode))
  )

(use-package typescript-mode
  :defer t
  :mode(("\\.tsx\\'" . typescript-mode)
        ("\\.ts\\'" . typescript-mode))
  :hook ((typescript-mode . add-node-modules-path)
         (typescript-mode . prettier-js-mode))
  :config
  (setq typescript-indent-level 2)
  )

(use-package tide
  :init
  :defer t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         ;; (typescript-mode . tide-hl-identifier-mode)
         ;; (before-save . tide-format-before-save)
         )
  :config
  (push '(?\< . ?\>) electric-pair-pairs)
  )

(use-package emmet-mode
  :defer t
  :after web-mode
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'emmet-mode)
  (setq-default emmet-move-cursor-between-quote t)
  )

(use-package web-mode
  :defer t
  :mode (("\\.html\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-block-padding 2
        web-mode-comment-style 2

        web-mode-enable-css-colorization t
        web-mode-enable-auto-pairing t
        web-mode-enable-comment-keywords t
        web-mode-enable-current-element-highlight t
        )
  )

(use-package add-node-modules-path
  :defer t)

(use-package rjsx-mode
  :defer t
  :mode(("\\.js\\'" . rjsx-mode)
        ("\\.jsx\\'" . rjsx-mode))
  :config
  (setq js2-mode-show-strict-warnings nil
        js2-mode-show-parse-errors nil)
  (add-hook 'rjsx-mode-hook (lambda ()
                              (add-node-modules-path)
                              (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
                              (prettier-js-mode)
                              ))
  )


(use-package flycheck-kotlin
  :defer t
  :after flycheck
  :init
  (flycheck-kotlin-setup)
  )

(use-package json-mode
  :defer t
  :mode (("\\.json$" . json-mode))
  )

(use-package elpy
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  )

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown")
  )

