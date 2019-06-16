(setq gc-cons-threshold (* 10 1000 1000))

(fset 'yes-or-no-p 'y-or-n-p)

(setq-default indent-tabs-mode nil)

(setq
 package-quickstart t
 package-enable-at-startup nil
 mac-option-key-is-meta nil
 mac-command-key-is-meta t
 mac-command-modifier 'meta
 mac-option-modifier nil
 inhibit-startup-screen t
 inhibit-startup-message t
 initial-scratch-message ""
 c-basic-offset 2
 coffee-tab-width 2 ; coffeescript
 javascript-indent-level 2 ; javascript-mode
 js-indent-level 2 ; js-mode
 js2-basic-offset 2 ; js2-mode, in latest js2-mode, it's alias of js-indent-level
 web-mode-markup-indent-offset 2 ; web-mode, html tag in html file
 web-mode-css-indent-offset 2 ; web-mode, css in html file
 web-mode-code-indent-offset 2 ; web-mode, js code in html file
 css-indent-offset 2 ; css-mode
 sh-basic-offset 2
 sh-indentation 2
 whitespace-style '(face trailing spaces tabs newline tab-mark newline-mark)
 show-trailing-whitespace t
 vc-follow-symlinks t
 create-lockfiles nil
 tab-width 2
 scroll-margin 3
 scroll-step 1
 scroll-conservatively 10000
 scroll-preserve-screen-position 1
 backup-directory-alist `(("." . "~/.emacs-saves"))
 backup-by-copying t
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t
 auto-save-file-name-transforms `((".*" "~/.emacs-saves/" t))
 auto-save-interval 20
 large-file-warning-threshold nil
 auto-revert-check-vc-info t
 )

(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-language-environment    'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system   'utf-8)
(prefer-coding-system        'utf-8)
(set-input-method nil)

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

(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize)
  )

(defun my/next-buffer ()
  "next-buffer, only skip *Messages*"
  (interactive)
  (next-buffer)
  (when (string= "*Messages*" (buffer-name))
      (next-buffer)))

(defun my/previous-buffer ()
  "previous-buffer, only skip *Messages*"
  (interactive)
  (previous-buffer)
  (when (string= "*Messages*" (buffer-name))
      (previous-buffer)))

(use-package drag-stuff
  :defer t
  :after evil
  :bind (
	 :map evil-normal-state-map
	 ( "C-j" . drag-stuff-down )
	 ( "C-k" . drag-stuff-up )
	 :map evil-visual-state-map
	 ( "C-j" . drag-stuff-down )
	 ( "C-k" . drag-stuff-up )
	 )
  :init
  (drag-stuff-mode 1)
  )

(use-package whitespace
  :defer t
  :hook ((prog-mode text-mode) . whitespace-mode)
  :custom-face
  (trailing-whitespace (( t ( :background "red" :foreground "black" ))))
  :init
  (setq whitespace-display-mappings
	;; all numbers are unicode codepoint in decimal. e.g. (insert-char 182 1)
	'(
	  (space-mark 32 [183] [46]) ; SPACE 32 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
	  (newline-mark 10 [172 10]) ; LINE FEED,
	  (tab-mark 9 [9655 9] [92 9]) ; tab
	  ))
  )

(use-package evil
  :defer t
  :bind (
         ( "M-{" . my/next-buffer)
         ( "M-}" . my/previous-buffer)
	 :map evil-normal-state-map
	 ( "gd" . evil-delete-buffer )
	 ( "C-u" . evil-scroll-up )
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

(use-package windmove
  :defer t
  :config
  (windmove-default-keybindings)
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
  :after evil ivy
  :defer t
  :init
  (global-evil-leader-mode)
  :config
  (evil-leader/set-leader ";")
  (evil-leader/set-key
    "b" 'counsel-projectile-switch-to-buffer
    "B" 'ivy-switch-buffer
    "c" 'evil-ex-nohighlight
    "d" 'ranger
    "f" 'swiper
    "F" 'counsel-rg
    "g" 'magit-status
    "l" 'my/common-modes
    "s" 'eshell
    )
  )

(use-package evil-commentary
  :after evil
  :init
  (evil-commentary-mode))

(use-package evil-visualstar
  :config
  (global-evil-visualstar-mode))

(use-package projectile
  :defer t
  :bind (
	 :map projectile-mode-map
	 ("s-p" . projectile-command-map)
	 )
  :init
  (projectile-mode)
  :config
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'alien)
  (setq projectile-globally-ignored-file-suffixes
        '("#" "~" ".swp" ".o" ".so" ".exe" ".dll" ".elc" ".pyc" ".jar" "*.class"))
  (setq projectile-globally-ignored-directories
        '(".git" "node_modules" "__pycache__" ".vs"))
  (setq projectile-globally-ignored-files '("TAGS" "tags" ".DS_Store"))
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
          '(
            (counsel-M-x . ivy--regex-plus)
            (swiper . ivy--regex-plus)
            (t . ivy--regex-fuzzy)))
    (add-to-list 'ivy-highlight-functions-alist
                 '(swiper--re-builder . ivy--highlight-ignore-order))
  )
(use-package ivy-hydra
  :defer t
  )

(use-package evil-escape
  :after evil
  :init
  (evil-escape-mode)
  )

(use-package dumb-jump
  :bind
  (:map evil-normal-state-map
        ("g D" . dumb-jump-go)
        ("g b" . dumb-jump-go-back)
        )
  :config
  (setq dumb-jump-selector 'ivy))

(use-package rainbow-delimiters
  :ensure t
  :init
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))

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
  :bind (:map ranger-normal-mode-map
	      ( "+" . dired-create-directory)
	      )
  :init
  (ranger-override-dired-mode t)
  (setq ranger-cleanup-on-disable t
       ranger-listing-dir-first nil)
  )

(use-package evil-magit
  :defer t
  :init
  (evil-magit-init)
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

(defun my/neotree-project-dir ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (projectile-project-root))
	(file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
	(if (neo-global--window-exists-p)
	    (progn
		(neotree-dir project-dir)
		(neotree-find file-name)))
	(message "Could not find git project root."))))

(use-package neotree
  :defer t
  :init
  (evil-leader/set-key
    "n" 'my/neotree-project-dir)
  :config
  (progn
    (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
    (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
    (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
    (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
    (evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
    (evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
    (evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
    (evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
    (evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)
    )
  )

(defun kill-other-buffers ()
  "Kill all buffers but the current one.
Don't mess with special buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (or (eql buffer (current-buffer)) (not (buffer-file-name buffer)))
      (kill-buffer buffer))))

(use-package kotlin-mode
  :mode "\\.kt\\'"
  :defer t
  )

(add-to-list 'auto-mode-alist '("\\.clj\\'" . prog-mode))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-hook 'after-init-hook (lambda () (load-theme 'nord t)))
(show-paren-mode 1)
(electric-pair-mode 1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(global-visual-line-mode t)

(defun simple-mode-line-render (left right)
  "Return a string of `window-width' length containing LEFT, and RIGHT aligned respectively."
  (let* ((available-width (- (window-total-width) (+ (length (format-mode-line left)) (length (format-mode-line right))))))
    (append left (list (format (format "%%%ds" available-width) "")) right)
    )
  )

(defun my-shorten-vc-mode-line (string)
  (cond
   ((string-prefix-p "Git" string)
    (concat "\ue0a0 " (magit-get-current-branch)))
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
    (quote (""
            (:eval (propertize evil-mode-line-tag
                               'face 'font-lock-preprocessor-face
                               'help-echo
                               "Evil mode"))

            (:eval (when (projectile-project-p)
                     (propertize (concat "[" (projectile-project-name) "]")
                                 'face 'font-lock-type-face
                                 'help-echo "Project Name")
                     ))

            " "
            mode-line-buffer-identification
            " "
            (:eval (propertize (if (buffer-modified-p)
                                   "[M]"
                                 "[-]")
                               'face 'font-lock-warning-face
                               'help-echo "Buffer modified"))
            " "
            (:eval (when buffer-read-only
                     (propertize "RO"
                                 'face 'font-lock-type-face
                                 'help-echo "Buffer is read-only")))
            "%3l:%c"
            ))
    ;; right
    (quote (
            (vc-mode (:eval
                      (propertize vc-mode
                                  'face 'font-lock-warning-face
                                  'help-echo "Buffer modified"
                                  )))
            " "
            mode-name
            " %p "
            "%I "
            ))
    )
   )
  )
 )

(use-package persp-mode
  :hook (after-init . (lambda () (persp-mode 1)))
  :config
  (defvar my/persp-default-workspace "main")
  (defvar my/persp-shared-buffers '("*scratch*" "*Messages*"))
  (defvar my/projectile-project-to-switch nil)

  (setq wg-morph-on nil ;; switch off animation
	persp-autokill-buffer-on-remove 'kill-weak
	persp-auto-save-opt 0
	persp-auto-resume-time -1
	persp-nil-hidden t
	persp-add-buffer-on-find-file t
	persp-add-buffer-on-after-change-major-mode t
	persp-hook-up-emacs-buffer-completion t)

  ;; Make ivy play nice
  (with-eval-after-load "ivy"
    (add-hook 'ivy-ignore-buffers
	      #'(lambda (b)
		  (when persp-mode
		    (let ((persp (get-current-persp)))
		      (if persp
			  (not (persp-contain-buffer-p b persp))
			nil)))))
    (setq ivy-sort-functions-alist
	  (append ivy-sort-functions-alist
		  '((persp-kill-buffer   . nil)
		    (persp-remove-buffer . nil)
		    (persp-add-buffer    . nil)
		    (persp-switch        . nil)
		    (persp-window-switch . nil)
		    (persp-frame-switch . nil)))))

  (defun my/projectile-switch-project-by-name (counsel-projectile-switch-project-by-name &rest args)
    (setq my/projectile-project-to-switch (car args))
    (apply counsel-projectile-switch-project-by-name args)
    (setq my/projectile-project-to-switch nil))
  (advice-add #'counsel-projectile-switch-project-by-name :around #'my/projectile-switch-project-by-name)

  (defun my/persp-create-project-persp ()
    (let ((frame (selected-frame))
	  (name (file-name-nondirectory
		 (directory-file-name
		  (file-name-directory
		   my/projectile-project-to-switch)))))
      (with-selected-frame frame
	(persp-add-new name)
	(persp-frame-switch name)
	(persp-add-buffer my/persp-shared-buffers (get-current-persp) nil))))

  (add-hook 'projectile-before-switch-project-hook 'my/persp-create-project-persp)

  (defun my/persp-concat-name (count)
    (if (eq count 0)
	my/persp-default-workspace
      (format "%s-%s" my/persp-default-workspace count)))

  (defun my/persp-next-main-name (&optional count)
    (let ((count (or count 0)))
      (if (persp-with-name-exists-p (my/persp-concat-name count))
	  (my/persp-next-main-name (+ count 1))
	(my/persp-concat-name count))))

  (add-hook
   'after-make-frame-functions
   (lambda (frame)
     (let ((name (my/persp-next-main-name)))
       (with-selected-frame frame
	 (set-frame-parameter frame 'my/persp-current-main name)
	 (persp-add-new name)
	 (persp-frame-switch name frame)
	 (persp-add-buffer my/persp-shared-buffers (get-current-persp) nil)))))

  (add-hook
   'delete-frame-functions
   (lambda (frame)
     (with-selected-frame frame
       (persp-kill (frame-parameter frame 'my/persp-current-main))))))

(use-package expand-region
  :after evil
  :bind (
	 :map evil-visual-state-map
	 ( "v" . er/expand-region)
	 )
  )

(use-package ein
  :defer t
  )

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
