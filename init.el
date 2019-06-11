(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)
(setq package-enable-at-startup nil ; don't auto-initialize!
      ;; don't add that `custom-set-variables' block to my initl!
      package--init-file-ensured t)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'nord t)
(fset 'yes-or-no-p 'y-or-n-p)

(setq

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
 evil-search-module 'evil-search
 evil-ex-search-case 'sensitive
 create-lockfiles nil
 tab-width 2
 )

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(require 'use-package)

(menu-bar-mode -1)

(use-package drag-stuff
  :ensure t
  :defer t
  :config
  (drag-stuff-mode 1)
  )

(use-package evil
  :ensure t
  :defer t
  :bind (
         :map evil-normal-state-map
         ( "H" . next-buffer )
         ( "L" . previous-buffer )
         ( "C-j" . drag-stuff-down ) ( "C-k" . drag-stuff-up ) :map evil-visual-state-map ( "C-j" . drag-stuff-down )
         ( "C-k" . drag-stuff-up )
         )
  :config
  (evil-mode 1)
  )


(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))


(use-package evil-numbers
  :ensure t
  :defer t
  :config
  (global-set-key (kbd "C-c +") 'evil-numbers/inc-at-pt)
  (global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt))

(use-package evil-leader
  :ensure t
  :after evil
  :config
  (evil-leader/set-leader "<SPC>")
  (global-evil-leader-mode))

(use-package evil-commentary
  :ensure t
  :after evil
  :config
  (evil-commentary-mode))

(fset 'evil-visual-update-x-selection 'ignore)
(setq scroll-conservatively 101)

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  ;; (define-key projectile-m
  ;; ode-map (kbd "C-c p") 'projectile-command-map)
  )

(use-package counsel-projectile
  :after projectile evil
  :config
  (define-key evil-normal-state-map (kbd "C-c p") 'counsel-projectile-switch-project)
  (define-key evil-normal-state-map (kbd "C-p") 'counsel-projectile-find-file))

(use-package ivy
  :ensure t
  :defer t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  ;; enable this if you want `swiper' to use it
  ;; (setq search-default-mode #'char-fold-to-regexp)
  (global-set-key "\C-s" 'swiper)
  ;; (global-set-key (kbd "C-c C-r") 'ivy-resume)
  ;; (global-set-key (kbd "<f6>") 'ivy-resume)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  ;; (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  ;; (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  ;; (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  ;; (global-set-key (kbd "<f1> l") 'counsel-find-library)
  ;; (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  ;; (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  ;; (global-set-key (kbd "C-c g") 'counsel-git)
  ;; (global-set-key (kbd "C-c j") 'counsel-git-grep)
  ;; (global-set-key (kbd "C-c k") 'counsel-ag)
  ;; (global-set-key (kbd "C-x l") 'counsel-locate)
  ;; (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
  (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)
  (setq ivy-re-builders-alist
      '((t . ivy--regex-fuzzy)))
)

(use-package evil-escape
  :ensure t
  :after evil
  :config
  (evil-escape-mode 1))

(use-package evil-multiedit
  :ensure t
  :after evil
  )

(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  )

;; (global-display-line-numbers-mode 1)
(setq whitespace-display-mappings
      ;; all numbers are unicode codepoint in decimal. e.g. (insert-char 182 1)
      '(
        (space-mark 32 [183] [46]) ; SPACE 32 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
        (newline-mark 10 [172 10]) ; LINE FEED,
        (tab-mark 9 [9655 9] [92 9]) ; tab
        ))
(global-whitespace-mode 1)
(show-paren-mode 1)
(electric-pair-mode 1)

(use-package highlight-indent-guides
  :ensure t
  :config
    (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
    (add-hook 'text-mode-hook 'highlight-indent-guides-mode)
    (setq highlight-indent-guides-method 'character)
    (setq highlight-indent-guides-character ?\|)
    (setq highlight-indent-guides-auto-enabled nil)
    (set-face-foreground 'highlight-indent-guides-character-face "black")
  )

(use-package telephone-line
  :ensure t
  :config
  (progn
    (telephone-line-evil-config)
    (set-face-attribute 'telephone-line-evil-insert nil :background "white" :foreground "black")
    (set-face-attribute 'telephone-line-evil-normal nil :background "cyan" :foreground "black")
    (set-face-attribute 'telephone-line-evil-visual nil :background "turquoise" :foreground "black")
    (set-face-attribute 'telephone-line-evil-operator nil :background "turquoise" :foreground "black")
    (set-face-attribute 'telephone-line-evil-motion nil :background "light steel blue" :foreground "black")
    (set-face-attribute 'telephone-line-accent-active nil :background "blue" :foreground "black")
    (set-face-attribute 'telephone-line-accent-inactive nil :background "black" :foreground "white")
    (set-face-attribute 'mode-line nil :background "black" :foreground "white")
    (set-face-attribute 'mode-line-inactive nil :background "black" :foreground "white")
    (telephone-line-defsegment telephone-line-projectile-segment ()
      "Displays the current project name, according to projectile."
      (if (fboundp 'projectile-project-name)
          (propertize (projectile-project-name)
                      'face 'mode-line
                      'display '(raise 0.0)
                      'help-echo "Switch project"
                      'mouse-face '(:box 1)
                      'local-map (make-mode-line-mouse-map
                                  'mouse-1 (lambda ()
                                             (interactive)
                                             (projectile-switch-project))))))

    (setq telephone-line-faces
          '((evil . telephone-line-modal-face)
            (ryo . telephone-line-ryo-modal-face)
            (accent . (telephone-line-accent-active . telephone-line-accent-inactive))
            (nil . (mode-line . mode-line-inactive))
            )
          )
    (setq telephone-line-lhs
          '((evil   . (telephone-line-evil-tag-segment))
            (accent . (telephone-line-vc-segment
                       telephone-line-erc-modified-channels-segment
                       ))
            (nil    . (
                       telephone-line-projectile-buffer-segment
                       ))
            ))
    (setq telephone-line-rhs
          '(
            (nil . (telephone-line-misc-info-segment
                    telephone-line-major-mode-segment
                    ))
            (accent . (telephone-line-atom-encoding-segment))
            (evil   . (telephone-line-airline-position-segment))))
    (telephone-line-mode 1)
    )
  )

(use-package git-gutter
  :ensure t
  :defer t
  :after magit
  :config
  (git-gutter-mode 1)
  )

(modify-syntax-entry ?_ "w")

(use-package ranger
  :ensure t
  :defer t
  )

(use-package evil-magit
  :ensure t
  )

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if (executable-find "python3") 3 0) treemacs-deferred-git-apply-delay      0.5 treemacs-display-in-side-window        t treemacs-eldoc-display                 t
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
  :ensure t)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(defun my/common-modes ()
  (interactive)
  (git-gutter-mode 1)
  (display-line-numbers-mode 1)
  )

(evil-leader/set-key
  "f" 'counsel-rg
  "b" 'counsel-projectile-switch-to-buffer
  "B" 'ivy-switch-buffer
  "g" 'magit
  "d" 'deer
  "c" 'evil-ex-nohighlight
  "l" 'my/common-modes
  )

(add-hook 'prog-mode-hook #'my/common-modes)
(add-hook 'text-mode-hook #'my/common-modes)

(define-key evil-visual-state-map "R" 'evil-multiedit-match-all)
(define-key evil-normal-state-map (kbd "C-n") 'evil-multiedit-match-and-next)
(define-key evil-visual-state-map (kbd "C-n") 'evil-multiedit-match-and-next)
(define-key evil-normal-state-map (kbd "C-S-n") 'evil-multiedit-match-and-prev)
(define-key evil-visual-state-map (kbd "C-S-n") 'evil-multiedit-match-and-prev)
(define-key evil-visual-state-map (kbd "C-M-D") 'evil-multiedit-restore)
(define-key evil-multiedit-state-map (kbd "RET") 'evil-multiedit-toggle-or-restrict-region)
(define-key evil-multiedit-state-map (kbd "C-j") 'evil-multiedit-next)
(define-key evil-multiedit-state-map (kbd "C-k") 'evil-multiedit-prev)

;; Ex command that allows you to invoke evil-multiedit with a regular expression, e.g.
(evil-ex-define-cmd "ie[dit]" 'evil-multiedit-ex-match)
(set-face-attribute 'trailing-whitespace nil :background "red" :foreground "black")
(set-face-attribute 'evil-ex-lazy-highlight nil :background "blue" :foreground "black")

(defun kill-other-buffers ()
  "Kill all buffers but the current one.
Don't mess with special buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (or (eql buffer (current-buffer)) (not (buffer-file-name buffer)))
      (kill-buffer buffer))))

(add-hook 'emacs-startup-hook (lambda ()
                                (setq gc-cons-threshold 16777216
                                      gc-cons-percentage 0.1))
          )

(use-package kotlin-mode
  :ensure t
  :defer t
  )

(add-to-list 'auto-mode-alist '("\\.clj\\'" . prog-mode))
