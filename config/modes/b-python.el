(use-package eglot
  :defer t
  :init
  (eglot-ensure)
  :config
  (add-to-list 'eglot-ignored-server-capabilites :documentHighlightProvider)
  (add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1)))
  )

(provide 'b-python)
