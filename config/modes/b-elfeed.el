(use-package elfeed
  :defer t
  :after evil-leader evil
  :init
  (setq elfeed-search-filter "@2-days-ago"
        elfeed-show-entry-switch #'my/show-elfeed)
  )

(use-package ov
  :defer t
  )

(use-package elfeed-goodies
  :defer t
  :after elfeed
  :init
  (elfeed-goodies/setup)
  (setq elfeed-goodies/feed-source-column-width 35
        elfeed-goodies/tag-column-width 25)
  )

(use-package elfeed-org
  :defer t
  :after elfeed
  :init
  (setq rmh-elfeed-org-files (list "~/gdrive/feed/elfeed.org"))
  (elfeed-org)
  )

(provide 'b-elfeed)
