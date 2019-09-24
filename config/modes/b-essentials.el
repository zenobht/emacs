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

(use-package frog-jump-buffer
  :defer t
  :after evil-leader
  :init
  (setq frog-jump-buffer-default-filter 'frog-jump-buffer-filter-same-project)
  )

 (use-package ace-window
   :init
   (setq
    aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
    aw-scope 'frame
    aw-background nil
    )
   )

(use-package tabbar
  :hook ((special-mode) . tabbar-local-mode)
  :init
  (tabbar-mode)
  :config
  (setq
   tabbar-background-color nil
   tabbar-home-button-value nil
   tabbar-buffer-home-button (quote (("[+]") ""))
   tabbar-home-button (quote (("[+]") ""))
   tabbar-scroll-left-button (quote ((" <") ""))
   tabbar-scroll-right-button (quote ((" >") ""))
   tabbar-buffer-groups-function 'my/tabbar-buffer-groups
   tabbar-separator (quote ("  ")))
  )

(provide 'b-essentials)
