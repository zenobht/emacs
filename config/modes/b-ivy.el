(use-package ivy
  :defer t
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers nil
        enable-recursive-minibuffers t
        ivy-wrap t
        ivy-use-selectable-prompt t
        ivy-re-builders-alist
        '(
          (counsel-M-x . ivy--regex-plus)
          (swiper . ivy--regex-plus)
          (counsel-rg . ivy--regex-plus)
          (counsel-find-file . ivy--regex-plus)
          (t . ivy--regex-fuzzy)))
  (add-to-list 'ivy-highlight-functions-alist
               '(swiper--re-builder . ivy--highlight-ignore-order))

  (add-to-list 'ivy-ignore-buffers "\\*Messages\\*")
  )

(use-package wgrep
  :defer t
  :after ivy
  :init
  (ivy-set-occur 'counsel-rg 'counsel-rg-occur)
  :config
  (setq wgrep-auto-save-buffer t)
  )


(provide 'b-ivy)
