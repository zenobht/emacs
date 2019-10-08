(use-package add-node-modules-path
  :defer t)

(use-package typescript-mode
  :defer t
  :after add-node-modules-path
  )

(use-package tide
  :init
  :defer t
  :after (typescript-mode company flycheck)
  )

(use-package emmet-mode
  :defer t
  :after web-mode
  )

(use-package web-mode
  :defer t
  )

(use-package rjsx-mode
  :defer t
  )

(use-package json-mode
  :defer t
  )

(provide 'b-js)
