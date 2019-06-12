(setq gc-cons-threshold (* 100 1000 1000))

(fset 'yes-or-no-p 'y-or-n-p)

(setq
 package-quickstart t
 package-enable-at-startup nil
 mac-option-key-is-meta nil
 mac-command-key-is-meta t
 mac-command-modifier 'meta
 mac-option-modifier nil
 inhibit-startup-screen t
 indent-tabs-mode nil
 c-basic-offset 2
 coffee-tab-width 2 ; coffeescript
 javascript-indent-level 2 ; javascript-mode
 js-indent-level 2 ; js-mode
 js2-basic-offset 2 ; js2-mode, in latest js2-mode, it's alias of js-indent-level
 web-mode-markup-indent-offset 2 ; web-mode, html tag in html file
 web-mode-css-indent-offset 2 ; web-mode, css in html file
 web-mode-code-indent-offset 2 ; web-mode, js code in html file
 css-indent-offset 2 ; css-mode
 whitespace-style '(face trailing spaces tabs newline tab-mark newline-mark)
 show-trailing-whitespace t
 vc-follow-symlinks t
 create-lockfiles nil
 tab-width 2
 scroll-conservatively 101
 )

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))

(require 'package)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package drag-stuff
  :defer t
  :init
  (drag-stuff-mode 1)
  )

(use-package whitespace
  :defer t
  :custom-face
  (trailing-whitespace (( t ( :background "red" :foreground "black" ))))
  :init
  (progn
    (setq whitespace-display-mappings
	  ;; all numbers are unicode codepoint in decimal. e.g. (insert-char 182 1)
	  '(
	    (space-mark 32 [183] [46]) ; SPACE 32 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
	    (newline-mark 10 [172 10]) ; LINE FEED,
	    (tab-mark 9 [9655 9] [92 9]) ; tab
	    ))
    )
  (whitespace-mode)
  )

(use-package hl-line
  :defer t
  :init
  (global-hl-line-mode)
  )

(use-package evil
  :defer t
  :bind (
	 ( "C-H" . windmove-left)
	 ( "C-J" . windmove-down)
	 ( "C-K" . windmove-up)
	 ( "C-L" . windmove-right)
	 :map evil-normal-state-map
	 ( "gj" . next-buffer )
	 ( "gk" . previous-buffer )
	 ( "gd" . evil-delete-buffer )
	 ( "C-j" . drag-stuff-down )
	 ( "C-k" . drag-stuff-up )
	 ( "C-u" . evil-scroll-up )
	 :map evil-visual-state-map
	 ( "C-j" . drag-stuff-down )
	 ( "C-k" . drag-stuff-up )
	 )
  :custom-face
  (evil-ex-lazy-highlight  ((t (:background "blue" :foreground "black" ))))
  :init
  (setq evil-search-module 'evil-search
        evil-ex-search-case 'sensitive
	evil-insert-state-message nil
	)
  (evil-mode)
  :config
  (fset 'evil-visual-update-x-selection 'ignore)
  )

(use-package evil-surround
  :defer t
  :after evil
  :init
  (global-evil-surround-mode 1)
  )


(use-package evil-numbers
  :defer t
  :bind (
	 ( "C-c +" . evil-numbers/inc-at-pt )
	 ( "C-c -" . evil-numbers/dec-at-pt )
	 )
  )

(use-package evil-leader
  :after evil
  :defer t
  :init
  (global-evil-leader-mode)
  :config
    (evil-leader/set-leader ";")
    (evil-leader/set-key
    "f" 'counsel-rg
    "b" 'counsel-projectile-switch-to-buffer
    "B" 'ivy-switch-buffer
    "g" 'magit-status
    "d" 'deer
    "c" 'evil-ex-nohighlight
    "l" 'my/common-modes
    "n" 'treemacs
    )
  )

(use-package evil-commentary
  :after evil
  :init
  (evil-commentary-mode))

(use-package projectile
  :defer t
  :bind (
	 :map projectile-mode-map
	 ("s-p" . projectile-command-map)
	 )
  :init
  (projectile-mode)
  )

(use-package counsel-projectile
  :after projectile evil
  :bind (
	 :map evil-normal-state-map
	 ("C-c p" . counsel-projectile-switch-project)
	 ("C-p" . counsel-projectile-find-file)
	 )
  )

(use-package ivy
  :defer t
  :bind (
	 ("\C-s" . swiper)
	 ("M-x" . counsel-M-x)
	 :map minibuffer-local-map
	 ("C-r" . counsel-minibuffer-history)
	 :map ivy-minibuffer-map
	 ([escape] . minibuffer-keyboard-quit)
	 )
  :init
  (ivy-mode)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-re-builders-alist
	'((t . ivy--regex-plus)))
  )
(use-package ivy-hydra
  :defer t
  )

(use-package evil-escape
  :after evil
  :init
  (evil-escape-mode)
  )

(use-package evil-multiedit
  :defer t
  :bind (
	 :map evil-visual-state-map
	 ("R" . evil-multiedit-match-all)
	 ("C-b" . evil-multiedit-match-and-prev)
	 ("C-n" . evil-multiedit-match-and-next)
	 ("C-M-D" . evil-multiedit-restore)
	 :map evil-normal-state-map
	 ("C-b" . evil-multiedit-match-and-prev)
	 ("C-n" . evil-multiedit-match-and-next)
	 :map evil-multiedit-state-map
	 ("RET" . evil-multiedit-toggle-or-restrict-region)
	 ("C-j" . evil-multiedit-next)
	 ("C-k" . evil-multiedit-prev)
	 )
  :after evil
  :config
  (evil-ex-define-cmd "ie[dit]" 'evil-multiedit-ex-match)
  )

(use-package company
  :defer t
  :init
  (global-company-mode)
  )

(use-package highlight-indent-guides
  :defer t
  :after evil
  :hook ((prog-mode text-mode) . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-character ?\|)
  (setq highlight-indent-guides-auto-enabled nil)
  (set-face-foreground 'highlight-indent-guides-character-face "black")
  )

(use-package git-gutter
  :defer t
  :after magit
  :hook ((prog-mode text-mode) . git-gutter-mode)
  )

(modify-syntax-entry ?_ "w")

(use-package ranger
  :defer t
  )

(use-package evil-magit
  :defer t
  :init
  (evil-magit-init)
  )

(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if (executable-find "python3") 3 0)
	  treemacs-deferred-git-apply-delay      0.5
	  treemacs-display-in-side-window        t
	  treemacs-eldoc-display                 t
	  treemacs-file-event-delay              5000
	  treemacs-file-follow-delay             0.2
	  treemacs-follow-after-init             t
	  treemacs-git-command-pipe              ""
	  treemacs-goto-tag-strategy             'refetch-index
	  treemacs-indentation                   2
	  treemacs-indentation-string            " "
	  treemacs-is-never-other-window         nil
	  treemacs-max-git-entries               5000
	  treemacs-missing-project-action        'ask
	  treemacs-no-png-images                 nil
	  treemacs-no-delete-other-windows       t
	  treemacs-project-follow-cleanup        nil
	  treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
	  treemacs-recenter-distance             0.1
	  treemacs-recenter-after-file-follow    nil
	  treemacs-recenter-after-tag-follow     nil
	  treemacs-recenter-after-project-jump   'always
	  treemacs-recenter-after-project-expand 'on-distance
	  treemacs-show-cursor                   nil
	  treemacs-show-hidden-files             t
	  treemacs-silent-filewatch              nil
	  treemacs-silent-refresh                nil
	  treemacs-sorting                       'alphabetic-desc
	  treemacs-space-between-root-nodes      t
	  treemacs-tag-follow-cleanup            t
	  treemacs-tag-follow-delay              1.5
	  treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
		 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
	("M-0"       . treemacs-select-window)
	("C-x t 1"   . treemacs-delete-other-windows)
	("C-x t t"   . treemacs)
	("C-x t B"   . treemacs-bookmark)
	("C-x t C-t" . treemacs-find-file)
	("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after treemacs evil
  )

(use-package treemacs-projectile
  :after treemacs projectile
  )

(use-package treemacs-icons-dired
  :after treemacs dired
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  )

(use-package display-line-numbers
  :defer t
  :after evil
  :hook ((prog-mode text-mode) . display-line-numbers-mode)
  )

(defun my/common-modes ()
  (interactive)
  (git-gutter-mode 1)
  (display-line-numbers-mode 1)
  (highlight-indent-guides-mode 1)
  )


(defun kill-other-buffers ()
  "Kill all buffers but the current one.
Don't mess with special buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (or (eql buffer (current-buffer)) (not (buffer-file-name buffer)))
      (kill-buffer buffer))))

(use-package kotlin-mode
  :defer t
  )

(add-to-list 'auto-mode-alist '("\\.clj\\'" . prog-mode))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-hook 'after-init-hook (lambda () (load-theme 'nord t)))
(show-paren-mode 1)
(electric-pair-mode 1)
(menu-bar-mode -1)

(defun simple-mode-line-render (left right)
  "Return a string of `window-width' length containing LEFT, and RIGHT aligned respectively."
  (let* ((available-width (- (window-total-width) (+ (length (format-mode-line left)) (length (format-mode-line right))))))
    (append left (list (format (format "%%%ds" available-width) "")) right)
    )
  )

(defun my-shorten-vc-mode-line (string)
  (cond
   ((string-prefix-p "Git" string)
    (concat " " (substring string 4)))
   (t
    string)))

(advice-add 'vc-git-mode-line-string :filter-return 'my-shorten-vc-mode-line)

(set-face-attribute 'mode-line-inactive nil :background "brightblack")
(set-face-attribute 'mode-line nil :background "black")

(setq-default
  mode-line-format
  '
  (
    (:eval
      (simple-mode-line-render
        ;; left
        (quote (""evil-mode-line-tag mode-line-buffer-identification " %l : %c " "[%*]"))
        ;; right
        (quote ((vc-mode vc-mode)  " {"mode-name "} %p "))
        )
      )
    )
  )

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
