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
    exec-path-from-shell
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

