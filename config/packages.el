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
    anaconda-mode
    company
    company-tabnine
    counsel-projectile
    display-line-numbers
    dumb-jump
    eglot
    elfeed
    elfeed-goodies
    elfeed-org
    emmet-mode
    esup
    evil
    evil-leader
    evil-magit
    evil-multiedit
    evil-nerd-commenter
    evil-smartparens
    evil-surround
    evil-visualstar
    expand-region
    flycheck
    flycheck-kotlin
    general
    git-gutter
    hideshow
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
    py-autopep8
    rainbow-delimiters
    rainbow-mode
    ranger
    rjsx-mode
    smartparens
    smex
    tabbar
    tide
    typescript-mode
    use-package
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

