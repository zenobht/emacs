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

(provide 'b-tabbar)
