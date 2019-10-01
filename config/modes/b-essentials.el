(use-package which-key
  :defer t
  :init
  (which-key-mode)
  :config
  (setq which-key-popup-type 'minibuffer
        which-key-min-display-lines 2
        which-key-sort-order 'which-key-key-order-alpha
        which-key-idle-delay 0.5
        which-key-separator " → "
        which-key-prefix-prefix "+")
  )

(use-package paradox
  :defer t
  :after evil-leader
  )

(use-package esup
  :defer t
  )

 (use-package ace-window
   :init
   (setq
    aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
    aw-scope 'frame
    aw-background nil
    )
   )

(provide 'b-essentials)
