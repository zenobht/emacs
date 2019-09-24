(require 'kotlin-mode)
(setq-default kotlin-tab-width 4)

(use-package flycheck-kotlin
  :defer t
  :after flycheck
  :init
  (flycheck-kotlin-setup)
  )
