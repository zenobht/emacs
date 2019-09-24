(setq package-user-dir "~/.emacs.d/elpa")
(package-initialize)
(setq package-archives
      '(
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))

(package-refresh-contents)

(defconst my/packages
  '(
    ace-window
    add-node-modules-path
    all-the-icons
    company
    company-tabnine
    counsel-projectile
    display-line-numbers
    dumb-jump
    electric
    elfeed
    elfeed-goodies
    elfeed-org
    elpy
    emmet-mode
    esup
    evil
    evil-leader
    evil-magit
    evil-multiedit
    evil-nerd-commenter
    evil-surround
    evil-visualstar
    expand-region
    flycheck
    flycheck-kotlin
    frog-jump-buffer
    general
    git-gutter
    hideshow
    highlight-thing
    hippie-expand
    hl-line
    ivy
    json-mode
    kotlin-mode
    markdown-mode
    neotree
    org
    ov
    paradox
    projectile
    rainbow-delimiters
    rainbow-mode
    ranger
    rjsx-mode
    tabbar
    tide
    typescript-mode
    web-mode
    wgrep
    which-key
    whitespace
    yaml-mode
    yasnippet
    yasnippet-snippets
    )
  )

; install required
(dolist (package my/packages)
  (unless (package-installed-p package)
    (ignore-errors
      (package-install package))))

;; upgrade installed
(save-window-excursion
  (package-list-packages t)
  (package-menu-mark-upgrades)
  (condition-case nil
      (package-menu-execute t)
    (error
     (package-menu-execute))))

; (use-package which-key
;;   :defer t
;;   :init
;;   (which-key-mode)
;;   :config
;;   (setq which-key-popup-type 'minibuffer
;;         which-key-min-display-lines 2
;;         which-key-sort-order 'which-key-key-order-alpha
;;         which-key-idle-delay 0.5
;;         which-key-separator " → "
;;         which-key-prefix-prefix "+")
;;   )

;; (use-package whitespace
;;   :defer t
;;   :hook ((prog-mode text-mode) . whitespace-mode)
;;   :init
;;   (setq whitespace-style '(face trailing spaces tabs newline tab-mark newline-mark)
;;         show-trailing-whitespace t
;;         whitespace-display-mappings
;;         ;; all numbers are unicode codepoint in decimal. e.g. (insert-char 182 1)
;;         '(
;;           (space-mark 32 [183] [46]) ; SPACE 32 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
;;           (newline-mark 10 [172 10]) ; LINE FEED,
;;           (tab-mark 9 [9655 9] [92 9]) ; tab
;;           )
;;         )
;;   )

;; (use-package paradox
;;   :defer t
;;   :after evil-leader
;;   )


;; (use-package dumb-jump
;;   :defer t
;;   :config
;;   (setq dumb-jump-selector 'ivy))

;; (use-package rainbow-delimiters
;;   :defer t
;;   :hook ((prog-mode) . rainbow-delimiters-mode)
;;   )


;; (use-package yasnippet
;;   :defer t
;;   :after company
;;   :init
;;   (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
;;   (yas-global-mode +1)
;;   :config
;;   ;; (add-to-list 'company-backends #'company-mode/backend-with-yas)
;;   (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
;;   )

;; (use-package yasnippet-snippets
;;   :defer
;;   :after yasnippet
;;   )

;; (use-package company
;;   :defer t
;;   :init
;;   (global-company-mode)
;;   :config
;;   (company-tng-configure-default)
;;   (setq
;;    company-idle-delay 0 ; Delay to complete
;;    company-dabbrev-downcase 0
;;    company-minimum-prefix-length 2
;;    company-selection-wrap-around t ; Loops around suggestions
;;    company-show-numbers t
;;    company-frontends '(
;;                        company-tng-frontend
;;                        company-pseudo-tooltip-frontend
;;                        company-echo-metadata-frontend
;;                        )
;;    )
;;   )

;; (use-package git-gutter
;;   :defer t
;;   :after magit
;;   :init
;;   (global-git-gutter-mode 1)
;;   )

;; (use-package ranger
;;   :defer t
;;   :config
;;   (ranger-override-dired-mode t)
;;   (setq ranger-cleanup-on-disable t
;;         ranger-listing-dir-first t
;;         ranger-persistent-sort t
;;         )
;;   (if (not (display-graphic-p))
;;       (setq ranger-footer-delay nil)
;;     )
;;   (when (string= system-type "darwin")
;;     (setq dired-use-ls-dired nil))
;;   )

;; (use-package evil-magit
;;   :defer t
;;   :after evil
;;   :init
;;   (evil-magit-init)
;;   :config
;;   (bind-key "q" #'my/magit-kill-buffers magit-status-mode-map)
;;   )

;; (use-package display-line-numbers
;;   :defer t
;;   :hook ((prog-mode text-mode) . display-line-numbers-mode)
;;   )

;; (use-package hideshow
;;   :defer t
;;   :hook ((prog-mode) . hs-minor-mode)
;;   )

;; (use-package all-the-icons
;;   :defer t
;;   :after neotree
;;   :init
;;   (setq neo-theme (if (display-graphic-p) 'icons 'nerd))
;;   )

;; (use-package neotree
;;   :defer t
;;   :after projectile
;;   :config
;;   (setq neo-window-width 35
;;         neo-smart-open t
;;         neo-show-hidden-files t
;;         neo-force-change-root t
;;         projectile-switch-project-action 'neotree-projectile-action
;;         neo-window-fixed-size nil)
;;   )

;; (use-package kotlin-mode
;;   :mode (("\\.kt$" . kotlin-mode))
;;   :defer t
;;   :config
;;   (setq-default kotlin-tab-width 4)
;;   )

;; (use-package electric
;;   :defer t
;;   :init
;;   (electric-pair-mode 1)
;;   :config
;;   (push '(?\' . ?\') electric-pair-pairs)
;;   )

;; (use-package expand-region
;;   :defer t
;;   :after evil
;;   )

;; (use-package yaml-mode
;;   :defer t
;;   :mode (("\\.yml$" . yaml-mode))
;;   )

;; (use-package hl-line
;;   :defer t
;;   :init
;;   (global-hl-line-mode 1)
;;   )

;; (use-package rainbow-mode
;;   :defer t
;;   :hook ((prog-mode) . rainbow-mode))

;; (use-package org
;;   :defer t
;;   :config
;;   (use-package org-plus-contrib
;;     :defer t)
;;   )

;; (use-package esup
;;   :defer t
;;   )

;; (use-package frog-jump-buffer
;;   :defer t
;;   :after evil-leader
;;   :init
;;   (setq frog-jump-buffer-default-filter 'frog-jump-buffer-filter-same-project)
;;   )

;; (use-package typescript-mode
;;   :defer t
;;   :mode(("\\.tsx\\'" . typescript-mode)
;;         ("\\.ts\\'" . typescript-mode))
;;   :hook ((typescript-mode . add-node-modules-path)
;;          (typescript-mode . prettier-js-mode))
;;   :config
;;   (setq typescript-indent-level 2)
;;   )

;; (use-package tide
;;   :init
;;   :defer t
;;   :after (typescript-mode company flycheck)
;;   :hook ((typescript-mode . tide-setup)
;;          ;; (typescript-mode . tide-hl-identifier-mode)
;;          ;; (before-save . tide-format-before-save)
;;          )
;;   :config
;;   (push '(?\< . ?\>) electric-pair-pairs)
;;   )

;; (use-package emmet-mode
;;   :defer t
;;   :after web-mode
;;   :config
;;   (add-hook 'sgml-mode-hook 'emmet-mode)
;;   (add-hook 'css-mode-hook 'emmet-mode)
;;   (setq-default emmet-move-cursor-between-quote t)
;;   )

;; (use-package hippie-exp
;;   :defer t
;;   :config
;;   (setq-default hippie-expand-try-functions-list
;;                 '(yas-hippie-try-expand emmet-expand-line)))

;; (use-package web-mode
;;   :defer t
;;   :mode (("\\.html\\'" . web-mode))
;;   :config
;;   (setq web-mode-markup-indent-offset 2
;;         web-mode-css-indent-offset 2
;;         web-mode-code-indent-offset 2
;;         web-mode-block-padding 2
;;         web-mode-comment-style 2

;;         web-mode-enable-css-colorization t
;;         web-mode-enable-auto-pairing t
;;         web-mode-enable-comment-keywords t
;;         web-mode-enable-current-element-highlight t
;;         )
;;   )

;; (use-package add-node-modules-path
;;   :defer t)

;; (use-package rjsx-mode
;;   :defer t
;;   :mode(("\\.js\\'" . rjsx-mode)
;;         ("\\.jsx\\'" . rjsx-mode))
;;   :config
;;   (setq js2-mode-show-strict-warnings nil
;;         js2-mode-show-parse-errors nil)
;;   (add-hook 'rjsx-mode-hook (lambda ()
;;                               (add-node-modules-path)
;;                               (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
;;                               (prettier-js-mode)
;;                               ))
;;   )

;; (use-package flycheck
;;   :defer t
;;   :hook (prog-mode . flycheck-mode)
;;   :config
;;   (advice-add 'flycheck-eslint-config-exists-p :override (lambda() t))
;;   )

;; (use-package flycheck-kotlin
;;   :defer t
;;   :after flycheck
;;   :init
;;   (flycheck-kotlin-setup)
;;   )

;; (use-package json-mode
;;   :defer t
;;   :mode (("\\.json$" . json-mode))
;;   )

;; (use-package ace-window
;;   :init
;;   (setq
;;    aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
;;    aw-scope 'frame
;;    aw-background nil
;;    )
;;   )

;; (use-package wgrep
;;   :defer t
;;   :after ivy
;;   :init
;;   (ivy-set-occur 'counsel-rg 'counsel-rg-occur)
;;   :config
;;   (setq wgrep-auto-save-buffer t)
;;   )

;; (use-package elfeed
;;   :defer t
;;   :after evil-leader evil
;;   :init
;;   (add-hook 'elfeed-search-update-hook #'my/elfeed-search-add-separators)
;;   (setq elfeed-search-filter "@2-days-ago"
;;         elfeed-show-entry-switch #'my/show-elfeed)
;;   (add-hook 'elfeed-show-mode-hook
;;             (lambda ()
;;               (let ((inhibit-read-only t)
;;                     (inhibit-modification-hooks t))
;;                 (setq-local truncate-lines nil
;;                             shr-width 85)
;;                 (set-buffer-modified-p nil))
;;               (setq-local left-margin-width 15
;;                           right-margin-width 15)
;;               ))
;;   )

;; (use-package ov
;;   :defer t
;;   )

;; (use-package elfeed-goodies
;;   :defer t
;;   :after elfeed
;;   :init
;;   (elfeed-goodies/setup)
;;   (setq elfeed-goodies/feed-source-column-width 35
;;         elfeed-goodies/tag-column-width 25)
;;   )

;; (use-package elfeed-org
;;   :defer t
;;   :after elfeed
;;   :init
;;   (setq rmh-elfeed-org-files (list "~/gdrive/feed/elfeed.org"))
;;   (elfeed-org)
;;   )

;; (use-package tabbar
;;   :defer t
;;   :hook ((special-mode) . tabbar-local-mode)
;;   :init
;;   (tabbar-mode)
;;   :config
;;   (setq
;;    tabbar-background-color nil
;;    tabbar-home-button-value nil
;;    tabbar-buffer-home-button (quote (("[+]") ""))
;;    tabbar-home-button (quote (("[+]") ""))
;;    tabbar-scroll-left-button (quote ((" <") ""))
;;    tabbar-scroll-right-button (quote ((" >") ""))
;;    tabbar-buffer-groups-function 'my/tabbar-buffer-groups
;;    tabbar-separator (quote ("  ")))
;;   )

;; (use-package company-tabnine
;;   :defer t
;;   :after company
;;   :init
;;   (add-to-list 'company-backends #'company-tabnine)
;;   :config
;;   (setq company-tabnine-auto-balance nil)
;;   )

;; (use-package elpy
;;   :defer t
;;   :init
;;   (advice-add 'python-mode :before 'elpy-enable)
;;   )

;; (use-package markdown-mode
;;   :commands (markdown-mode gfm-mode)
;;   :mode (("README\\.md\\'" . gfm-mode)
;;          ("\\.md\\'" . markdown-mode)
;;          ("\\.markdown\\'" . markdown-mode))
;;   :init
;;   (setq markdown-command "multimarkdown")
;;   )

;; (use-package highlight-thing
;;   :defer t
;;   :hook (prog-mode . highlight-thing-mode)
;;   :config
;;   (setq highlight-thing-case-sensitive-p t
;;         highlight-thing-limit-to-region-in-large-buffers-p nil
;;         highlight-thing-narrow-region-lines 15
;;         highlight-thing-large-buffer-limit 5000
;;         highlight-thing-prefer-active-region t)
;;   )
