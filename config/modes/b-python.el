(use-package elpy
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  )
