(use-package elfeed
  :defer t
  :after evil-leader evil
  :init
  (add-hook 'elfeed-search-update-hook #'my/elfeed-search-add-separators)
  (setq elfeed-search-filter "@2-days-ago"
        elfeed-show-entry-switch #'my/show-elfeed)
  (add-hook 'elfeed-show-mode-hook
            (lambda ()
              (let ((inhibit-read-only t)
                    (inhibit-modification-hooks t))
                (setq-local truncate-lines nil
                            shr-width 85)
                (set-buffer-modified-p nil))
              (setq-local left-margin-width 15
                          right-margin-width 15)
              ))
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
