(fset 'yes-or-no-p 'y-or-n-p)

(setq-default electric-indent-inhibit t
              evil-shift-width 2
              )

 (defun load-directory (directory)

    "Load recursively all `.el' files in DIRECTORY."
    (dolist (element (directory-files-and-attributes directory nil nil nil))
      (let* ((path (car element))
             (fullpath (concat directory "/" path))
             (isdir (car (cdr element)))
             (ignore-dir (or (string= path ".") (string= path ".."))))
        (cond
         ((and (eq isdir t) (not ignore-dir))
          (load-directory fullpath))
         ((and (eq isdir nil) (string= (substring path -3) ".el"))
          (load (file-name-sans-extension fullpath)))))))

(load-directory "~/.emacs.d/lib")

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
 scroll-margin 0
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
 vc-follow-symlinks t
 auto-revert-check-vc-info t
 backward-delete-char-untabify-method 'hungry
 initial-major-mode (quote text-mode)
 mouse-wheel-progressive-speed nil
 display-buffer-alist '(("\\`\\*e?shell" display-buffer-pop-up-window))
 indent-tabs-mode nil
 )

(add-hook 'eshell-exit-hook (lambda () (interactive) (delete-window)))

(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-language-environment    'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system   'utf-8)
(prefer-coding-system        'utf-8)
(set-input-method nil)

(setq package-archives
      '(
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize)
  (setup-project-paths)
  )

(defun my/next-buffer ()
  (interactive)
  (next-buffer)
  (while (string-match-p "^\*" (buffer-name))
    (next-buffer))
  )

(defun my/previous-buffer ()
  (interactive)
  (previous-buffer)
  (while (string-match-p "^\*" (buffer-name))
    (previous-buffer))
  )

(defun my/last-used-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer))
  )

(defun my/setup-indent (n)
  (setq c-basic-offset n)
  (setq tab-width n)
  (setq coffee-tab-width n)
  (setq javascript-indent-level n)
  (setq js-indent-level n)
  (setq js2-basic-offset n)
  (setq web-mode-markup-indent-offset n)
  (setq web-mode-css-indent-offset n)
  (setq web-mode-code-indent-offset n)
  (setq css-indent-offset n)
  )

(defun my/configure ()
  (my/setup-indent 2)
  )

(defun my/copy-to-clipboard ()
  "Copies selection to x-clipboard."
  (interactive)
  (if (display-graphic-p)
      (progn
        (message "Yanked region to x-clipboard!")
        (call-interactively 'clipboard-kill-ring-save)
        )
    (if (region-active-p)
        (progn
          (shell-command-on-region (region-beginning) (region-end) "pbcopy")
          (message "Yanked region to clipboard!")
          (deactivate-mark))
      (message "No region active; can't yank to clipboard!")))
  )

(defun my/paste-from-clipboard ()
  "Pastes from x-clipboard."
  (interactive)
  (if (display-graphic-p)
      (progn
        (clipboard-yank)
        (message "graphics active")
        )
    (insert (shell-command-to-string "pbpaste"))
    )
  )

(use-package whitespace
  :defer t
  :custom-face
  (trailing-whitespace (( t ( :background "red" :foreground "white" ))))
  :init
  (setq whitespace-style '(face trailing spaces tabs newline tab-mark newline-mark)
        show-trailing-whitespace t)
  (setq whitespace-display-mappings
        ;; all numbers are unicode codepoint in decimal. e.g. (insert-char 182 1)
        '(
          (space-mark 32 [183] [46]) ; SPACE 32 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
          (newline-mark 10 [172 10]) ; LINE FEED,
          (tab-mark 9 [9655 9] [92 9]) ; tab
          )
        )
  )

(defun my/visual-shift-left ()
  (interactive)
  ;; (evil-shift-left (region-beginning) (region-end))
  (call-interactively 'evil-shift-left)
  (evil-normal-state)
  (evil-visual-restore))

(defun my/visual-shift-right ()
  (interactive)
  ;; (evil-shift-right (region-beginning) (region-end))
  (call-interactively 'evil-shift-right)
  (evil-normal-state)
  (evil-visual-restore))

(defun my/substitute()
  (interactive)
  (if (eq evil-state 'visual)
      (evil-ex "`<,`>s!!!g")
    (evil-ex "%s!!!g")
    )
  )

(defun my/visual-macro ()
  (interactive)
  (evil-ex "`<,`>norm @")
  )

(use-package evil
  :defer t
  :bind (
         :map evil-insert-state-map
         ( "M-/" . yas-expand)
         :map evil-normal-state-map
         ( "C-u" . evil-scroll-up )
         ( "[e" . previous-error)
         ( "]e" . next-error)
         ( "gj" . evil-jump-backward)
         ( "gk" . evil-jump-forward)
         ( "gs" . my/substitute)
         :map evil-visual-state-map
         ( "M-c" . my/copy-to-clipboard)
         ( "M-v" . my/paste-from-clipboard)
         ( "gs" . my/substitute)
         ( "gM" . my/visual-macro)
         )
  :custom-face
  (evil-ex-lazy-highlight  ((t (:background "blue" :foreground "black" ))))
  :init
  (setq evil-search-module 'evil-search
        evil-insert-state-message nil
        evil-want-C-i-jump nil
        )
  (evil-mode)
  :config
  (fset 'evil-visual-update-x-selection 'ignore)
  (define-key evil-visual-state-map (kbd ">") 'my/visual-shift-right)
  (define-key evil-visual-state-map (kbd "<") 'my/visual-shift-left)
  (eval-after-load 'evil-ex
    '(evil-ex-define-cmd "jsf" (lambda ()
                                 (interactive)
                                 (json-mode)
                                 (json-pretty-print-buffer))))

  (evil-define-operator evil-move-up (beg end)
    "Move region up by one line."
    :motion evil-line
    (interactive "<r>")
    (if (not (eq evil-state 'normal))
        (evil-visual-line))
    (let ((beg-line (line-number-at-pos beg))
          (end-line (line-number-at-pos end))
          (dest (- (line-number-at-pos beg) 2)))
      (evil-move beg end dest)
      (goto-line (- beg-line 1))
      (exchange-point-and-mark)
      (goto-line (- end-line 2))
      (if (not (eq evil-state 'normal))
          (evil-visual-line))
      )
    )

  (evil-define-operator evil-move-down (beg end)
    "Move region down by one line."
    :motion evil-line
    (interactive "<r>")
    (if (not (eq evil-state 'normal))
        (evil-visual-line))
    (let ((beg-line (line-number-at-pos beg))
          (end-line (line-number-at-pos end))
          (dest (+ (line-number-at-pos end) 0)))
      (evil-move beg end dest)
      (goto-line (+ beg-line 1))
      (exchange-point-and-mark)
      (goto-line (+ end-line 0))
      (if (not (eq evil-state 'normal))
          (evil-visual-line))
      )
    )

  (define-key evil-normal-state-map (kbd "M-k") 'evil-move-up)
  (define-key evil-normal-state-map (kbd "M-j") 'evil-move-down)
  (define-key evil-visual-state-map (kbd "M-k") 'evil-move-up)
  (define-key evil-visual-state-map (kbd "M-j") 'evil-move-down)
  )

(defun my/new-empty-buffer ()
  "Create a new empty buffer.
New buffer will be named “untitled” or “untitled<2>”, “untitled<3>”, etc.

It returns the buffer (for elisp programing).

URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
Version 2017-11-01"
  (interactive)
  (let (($buf (generate-new-buffer "untitled")))
    (switch-to-buffer $buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)
    $buf
    ))

(bind-keys*
 ( "M-t" . tabbar-backward-group)
 ( "C-j" . tabbar-backward-tab)
 ( "C-k" . tabbar-forward-tab)
 ( "M-z" . kill-this-buffer)
 ( "M-Z" . delete-window)
 ( "M-f" . find-file)
 ( "M-r" . my/last-used-buffer)
 ( "M-v" . (lambda () (interactive) (message "Pasted from clipboard")))
 ( "M-c" . my/copy-to-clipboard)
 ( "M-e" . elfeed)
 ( "M-n" . my/new-empty-buffer)
 )

(unbind-key "C-l" global-map)

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

(use-package vimish-fold
  :defer t
  :after evil
  :bind (
         :map evil-normal-state-map
              ( "zc" . vimish-fold)
              ( "zo" . vimish-fold-unfold)
              ( "zO" . vimish-fold-unfold-all)
         )
  :init
  (vimish-fold-global-mode 1)
  )

(use-package evil-numbers
  :defer t
  :bind (
         ( "C-c +" . evil-numbers/inc-at-pt )
         ( "C-c -" . evil-numbers/dec-at-pt )
         )
  )

(defun my/rg-star-search ()
  (interactive)
  (if (evil-visual-state-p)
      (let ((selection (regexp-quote (substring-no-properties (buffer-substring (region-beginning) (region-end))))))
        (counsel-rg selection)
        )
    (let ((current-word (thing-at-point 'word 'no-properties)))
      (counsel-rg current-word)
      )
    )
  )

(use-package evil-leader
  :after evil ivy
  :defer t
  :init
  (global-evil-leader-mode)
  :config
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "<escape>" 'evil-ex-nohighlight
    "*" 'my/rg-star-search
    "b" 'ivy-switch-buffer
    "C" 'my/calendar
    "d" 'deer
    "f" 'counsel-rg
    "gg" 'magit-status
    "gb" 'magit-blame
    "s" 'swiper
    "t" 'eshell
    "u" 'package-upgrade-all
    )
  )

(use-package evil-visualstar
  :defer t
  :after evil
  :init
  (global-evil-visualstar-mode t))

(use-package projectile
  :defer t
  :init (projectile-global-mode)
  :config
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'alien)
  (setq projectile-globally-ignored-file-suffixes
        '("#" "~" ".swp" ".o" ".so" ".exe" ".dll" ".elc" ".pyc" ".jar" "*.class"))
  (setq projectile-globally-ignored-directories
        '(".git" "node_modules" "__pycache__" ".vs"))
  (setq projectile-globally-ignored-files '("TAGS" "tags" ".DS_Store"))
  (add-hook 'projectile-after-switch-project-hook
            (lambda () (setup-project-paths)))
  )

(use-package counsel-projectile
  :defer t
  :after projectile evil evil-leader
  :init
  (setq counsel-projectile-remove-current-buffer t)
  (evil-leader/set-key
    "pb" 'counsel-projectile-switch-to-buffer
    "pf" 'counsel-projectile-find-file
    "pp" 'counsel-projectile-switch-project
    "pi" 'projectile-invalidate-cache
    )
  )

(use-package ivy
  :defer t
  :bind (
         ("M-x" . counsel-M-x)
         :map ivy-minibuffer-map
         ([escape] . keyboard-escape-quit)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         )
  :init
  (use-package smex)
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers nil)
  (setq enable-recursive-minibuffers t)
  (setq ivy-re-builders-alist
          '(
            (counsel-M-x . ivy--regex-plus)
            (swiper . ivy--regex-plus)
            (counsel-rg . ivy--regex-plus)
            (t . ivy--regex-fuzzy)))
    (add-to-list 'ivy-highlight-functions-alist
                 '(swiper--re-builder . ivy--highlight-ignore-order))

   (add-to-list 'ivy-ignore-buffers "\\*Messages\\*")
   (setq ivy-wrap t)
  )
(use-package ivy-hydra
  :defer t
  )

(use-package dumb-jump
  :defer t
  :bind
  (:map evil-normal-state-map
        ("gd" . dumb-jump-go)
        )
  :config
  (setq dumb-jump-selector 'ivy))

(use-package rainbow-delimiters
  :defer t
  :hook ((prog-mode) . rainbow-delimiters-mode))

(use-package evil-multiedit
  :defer t
  :bind (
         :map evil-visual-state-map
         ("R" . evil-multiedit-match-all)
         ("M-D" . evil-multiedit-match-and-prev)
         ("M-d" . evil-multiedit-match-and-next)
         ("C-M-D" . evil-multiedit-restore)
         :map evil-normal-state-map
         ("M-D" . evil-multiedit-match-and-prev)
         ("M-d" . evil-multiedit-match-and-next)
         :map evil-multiedit-state-map
         ("RET" . evil-multiedit-toggle-or-restrict-region)
         ("C-j" . evil-multiedit-next)
         ("C-k" . evil-multiedit-prev)
         )
  :after evil
  :config
  (evil-ex-define-cmd "ie[dit]" 'evil-multiedit-ex-match)
  )

(defun company-mode/backend-with-yas (backend)
  (if (and (listp backend) (member 'company-yasnippet backend))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(use-package yasnippet
  :defer t
  :after company
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode +1)
  :config
  ;; (add-to-list 'company-backends #'company-mode/backend-with-yas)
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
  )

(use-package yasnippet-snippets
  :defer
  :after yasnippet
  )

(use-package company
  :defer t
  :bind (:map company-active-map
        ("C-j" . company-select-next)
        ("C-k" . company-select-previous)
        )
  :init
  (global-company-mode)
  :config
  (company-tng-configure-default)
  (setq
   company-idle-delay 0 ; Delay to complete
   company-minimum-prefix-length 2
   company-selection-wrap-around t ; Loops around suggestions
   company-show-numbers t
   company-frontends '(
                       company-tng-frontend
                       company-pseudo-tooltip-frontend
                       company-echo-metadata-frontend
                       )
   )
  )

(use-package git-gutter
  :defer t
  :after magit
  :init
  (global-git-gutter-mode 1)
  )

(modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table)

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

(defun mu-magit-kill-buffers ()
  "Restore window configuration and kill all Magit buffers."
  (interactive)
  (let ((buffers (magit-mode-get-buffers)))
    (magit-restore-window-configuration)
    (mapc #'kill-buffer buffers)))

(use-package evil-magit
  :defer t
  :custom-face
  (magit-branch-local ((t (:foreground "brightyellow"))))
  :init
  (evil-magit-init)
  :config
  (bind-key "q" #'mu-magit-kill-buffers magit-status-mode-map)
  )

(use-package display-line-numbers
  :defer t
  :after evil
  :init
  (global-display-line-numbers-mode 1)
  :config
  (setq display-line-numbers-width-start 100)
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
  (setq neo-window-width 35)
  (setq neo-window-fixed-size nil)
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
  :mode (("\\.kt$" . kotlin-mode))
  :defer t
  :config
  (setq-default kotlin-tab-width 4)
  )

(add-to-list 'auto-mode-alist '("\\.clj\\'" . prog-mode))
(show-paren-mode 1)
(delete-selection-mode 1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(use-package electric
  :defer t
  :init
  (electric-pair-mode 1)
  :config
  (push '(?\' . ?\') electric-pair-pairs)
  )

(defun my/disable-in-minibuffer ()
  (electric-pair-mode -1))
(defun my/enable-on-minibuffer-exit ()
  (electric-pair-mode +1))

(add-hook 'minibuffer-setup-hook #'my/disable-in-minibuffer)
(add-hook 'minibuffer-exit-hook #'my/enable-on-minibuffer-exit)

(add-hook 'text-mode-hook
          (lambda ()
            (interactive)
            (visual-line-mode -1)
            (setq indent-tabs-mode nil)
            (whitespace-mode +1)
            (modify-syntax-entry ?_ "w")))

(add-hook 'prog-mode-hook
          (lambda ()
            (interactive)
            (visual-line-mode +1)
            (setq indent-tabs-mode nil)
            (whitespace-mode +1)
            (modify-syntax-entry ?_ "w")))

(defun simple-mode-line-render (left right)
  "Return a string of `window-width' length containing LEFT, and RIGHT aligned respectively."
  (let* ((available-width (- (window-total-width) (+ (length (format-mode-line left)) (length (format-mode-line right))))))
    (append left (list (format (format "%%%ds" available-width) "")) right)
    )
  )

(defvar my/mode-line-coding-format
      '(:eval
        (let* ((code (symbol-name buffer-file-coding-system))
               (eol-type (coding-system-eol-type buffer-file-coding-system))
               (eol (if (eq 0 eol-type) "UNIX"
                      (if (eq 1 eol-type) "DOS"
                        (if (eq 2 eol-type) "MAC"
                          "???")))))
          ;; (concat code " " eol " "))))
          (concat code " "))))
(put 'my/mode-line-coding-format 'risky-local-variable t)

(defun my/shorten-vc-mode-line (string)
  (cond
   ((string-prefix-p "Git" string)
    (concat "\ue0a0 " (magit-get-current-branch)))
   (t
    string)))

(advice-add 'vc-git-mode-line-string :filter-return 'my/shorten-vc-mode-line)

(setq-default
 mode-line-format
 '
 (
  (:eval
   (simple-mode-line-render
    ;; left
    (quote (""
            (:eval (propertize evil-mode-line-tag
                               'help-echo
                               "Evil mode"))

            (vc-mode (:eval (propertize vc-mode
                                        'face 'bold
                                        'help-echo "Buffer modified")))
            " "
            (:eval (when (projectile-project-p)
                     (propertize (concat "[" (projectile-project-name) "]")
                                 'face 'italic
                                 'help-echo "Project Name")
                     ))

            " "
            mode-line-buffer-identification
            (:eval (propertize (if (buffer-modified-p)
                                   "[+]"
                                 "")
                               'help-echo "Buffer modified"))
            " "
            (:eval (when buffer-read-only
                     (propertize "RO"
                                 'help-echo "Buffer is read-only")))
            " "
            (flycheck-mode flycheck-mode-line)
            ))
    ;; right
    (quote (
            (:eval (propertize "[%2l:%2c] " 'face 'bold))
            "%p "
            mode-name
            "  %I "
            my/mode-line-coding-format
            ))
    )
   )
  )
 )

(use-package expand-region
  :defer t
  :after evil
  :bind (
         :map evil-visual-state-map
         ( "v" . er/expand-region)
         )
  )

(use-package yaml-mode
  :defer t
  :mode (("\\.yml$" . yaml-mode))
  )

(set-face-attribute 'show-paren-match nil :background "brightblue" :foreground "white")

(add-hook 'after-change-major-mode-hook #'my/configure)

(use-package hl-line
  :defer t
  :init
  (global-hl-line-mode 1)
  )

(use-package rainbow-mode
  :defer t
  :hook ((prog-mode) . rainbow-mode))


(defun my/org-move-up ()
  (interactive)
  (if (string-prefix-p "*" (thing-at-point 'line))
      (org-move-subtree-up)
    (org-move-item-up))
  )

(defun my/org-move-down ()
  (interactive)
  ;; in org-mode move subtree if its heading else move item
  (if (string-prefix-p "*" (thing-at-point 'line))
      (org-move-subtree-down)
    (org-move-item-down))
  )

(use-package evil-org
  :defer t
  :after evil evil-leader
  :custom-face
  (org-hide ((t (:foreground "black" :background: "black"))))
  :init
  (progn
    (evil-leader/set-key
      "oa" 'org-agenda
      "oc" 'org-capture
      )
    (setq org-agenda-files '("~/gdrive/gtd/inbox.org"
                             "~/gdrive/gtd/gtd.org"
                             "~/gdrive/gtd/tickler.org"
                             "~/gdrive/gtd/gcal.org"
                             ))

    (setq org-capture-templates '(("t" "Todo [inbox]" entry
                                   (file+headline "~/gdrive/gtd/inbox.org" "Tasks")
                                   "* TODO %i%?")
                                  ("T" "Tickler" entry
                                   (file+headline "~/gdrive/gtd/tickler.org" "Tickler")
                                   "* %i%? \n %U")))
    (setq org-refile-targets '(("~/gdrive/gtd/gtd.org" :maxlevel . 2)
                               ("~/gdrive/gtd/someday.org" :level . 1)
                               ("~/gdrive/gtd/tickler.org" :level . 2)))
    (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
    (setq org-hide-leading-stars t)
    )
  :config
  (progn
    (add-hook 'org-mode-hook 'evil-org-mode)
    (add-hook 'evil-org-mode-hook
              (lambda ()
                (evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading))))
    (require 'evil-org-agenda)
    (evil-org-agenda-set-keys)
    )
  )

(add-hook 'org-mode-hook
          (lambda ()
            (define-key org-mode-map (kbd "<C-up>") 'my/org-move-up)
            (define-key org-mode-map (kbd "<C-down>") 'my/org-move-down)
            )
          )

(use-package org-gcal
  :defer t
  :config
  (setq org-gcal-client-id (exec-path-from-shell-copy-env "G_CLIENT_ID")
      org-gcal-client-secret (exec-path-from-shell-copy-env "G_CLIENT_SECRET")
      org-gcal-file-alist '(((exec-path-from-shell-copy-env "G_CALENDAR_ID") .  "~/gdrive/gtd/gcal.org")))
  (add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync) ))
  (add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync)))
  )

(use-package calfw-gcal)

(use-package calfw-org)

(defun my/calendar ()
  (interactive)
  (cfw:open-calendar-buffer
    :contents-sources
    (list
    (cfw:org-create-source "Green")  ; orgmode source
    ))
  )

(use-package calfw
  :defer t
  :after calfw-ical calfw-org evil-leader
  :config
  (setq cfw:org-overwrite-default-keybinding t)
  )

(use-package esup
  :defer t
  )

(use-package frog-jump-buffer
  :defer t
  :after evil-leader
  :init
  (evil-leader/set-key
    ";" 'frog-jump-buffer)
  (setq
   frog-jump-buffer-default-filter 'frog-jump-buffer-filter-same-project
   )
  )

(defun package-upgrade-all ()
  "Upgrade all packages automatically without showing *Packages* buffer."
  (interactive)
  (package-refresh-contents)
  (let (upgrades)
    (cl-flet ((get-version (name where)
                           (let ((pkg (cadr (assq name where))))
                             (when pkg
                               (package-desc-version pkg)))))
      (dolist (package (mapcar #'car package-alist))
        (let ((in-archive (get-version package package-archive-contents)))
          (when (and in-archive
                     (version-list-< (get-version package package-alist)
                                     in-archive))
            (push (cadr (assq package package-archive-contents))
                  upgrades)))))
    (if upgrades
        (when (yes-or-no-p
               (message "Upgrade %d package%s (%s)? "
                        (length upgrades)
                        (if (= (length upgrades) 1) "" "s")
                        (mapconcat #'package-desc-full-name upgrades ", ")))
          (save-window-excursion
            (dolist (package-desc upgrades)
              (let ((old-package (cadr (assq (package-desc-name package-desc)
                                             package-alist))))
                (package-install package-desc)
                (package-delete  old-package)))))
      (load-file user-init-file)
      (message "All packages are up to date"))))

(defun my/prettier-setup ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (prettier (and root
                        (expand-file-name "node_modules/.bin/prettier"
                                          root))))
    ;; (if (not (and prettier (file-executable-p prettier)))
    ;;     ;; hack to remove formatting for js files if prettier is not installed locally
    ;;     (advice-remove #'format-all-buffer :override #'+format/buffer)
    ;;   )
    ))

(defun my/eslint-setup ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/.bin/eslint"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

(defun my/setup-tools-from-node ()
  (my/eslint-setup)
  (my/prettier-setup)
  )

(use-package web-mode
  :defer t
  :after flycheck
  :init
  (add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (add-hook 'web-mode-hook #'my/setup-tools-from-node)
  (setq web-mode-enable-auto-pairing nil)
  )

(add-hook 'emacs-lisp-mode-hook (lambda () (flycheck-mode -1)))

(use-package flycheck
  :defer t
  :custom-face
  (flycheck-error ((t (:foreground "red" :underline "red"))))
  :init (global-flycheck-mode))

(use-package flycheck-kotlin
  :defer t
  :after flycheck
  :init
  (flycheck-kotlin-setup)
  )

(use-package json-mode
  :defer t
  :mode (("\\.json$" . json-mode))
  )

(use-package ace-window
  :bind* ("M-o" . ace-window)
  :init
  (setq
   aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
   aw-scope 'frame
   aw-background nil
   )
  )

(defun my/mousewheel-scroll-up (event)
    "Scroll window under mouse up by five lines."
    (interactive "e")
    (let ((current-window (selected-window)))
      (unwind-protect
          (progn
            (select-window (posn-window (event-start event)))
            (scroll-up 5))
        (select-window current-window))))

(defun my/mousewheel-scroll-down (event)
    "Scroll window under mouse down by five lines."
    (interactive "e")
    (let ((current-window (selected-window)))
      (unwind-protect
          (progn
            (select-window (posn-window (event-start event)))
            (scroll-down 5))
        (select-window current-window))))

(xterm-mouse-mode 1)
(pixel-scroll-mode)
(unless window-system
    (global-set-key (kbd "<mouse-4>") 'my/mousewheel-scroll-down)
    (global-set-key (kbd "<mouse-5>") 'my/mousewheel-scroll-up))

(use-package wgrep
  :defer t
  :after ivy
  :init
  (ivy-set-occur 'counsel-rg 'counsel-rg-occur)
  :config
  (setq wgrep-auto-save-buffer t)
  )

(defun my/elfeed-mark-all-read ()
    (interactive)
    (elfeed-untag elfeed-search-entries 'unread)
    (elfeed-search-update :force)) ; redraw

(defun my/elfeed-star ()
  "Apply starred to all selected entries."
  (interactive )
  (let* ((entries (elfeed-search-selected))
         (tag 'starred))

    (cl-loop for entry in entries do (elfeed-tag entry tag))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))

;; remove a start
(defun my/elfeed-unstar ()
  "Remove starred tag from all selected entries."
  (interactive )
  (let* ((entries (elfeed-search-selected))
         (tag 'starred))

    (cl-loop for entry in entries do (elfeed-untag entry tag))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))

(defun my/show-elfeed (buffer)
  (with-current-buffer buffer
    (setq buffer-read-only nil)
    (setq fill-column 110)
    (goto-char (point-min))
    (re-search-forward "\n\n")
    (fill-individual-paragraphs (point) (point-max))
    (setq buffer-read-only t))
  ;; (setq fill-column 80)
  (switch-to-buffer buffer))

(defun my/elfeed-search-show-entry (entry)
  "Display the currently selected item in a buffer without going to next line."
  (interactive (list (elfeed-search-selected :ignore-region)))
  (require 'elfeed-show)
  (when (elfeed-entry-p entry)
    (elfeed-untag entry 'unread)
    (elfeed-search-update-entry entry)
    (elfeed-show-entry entry)))

(use-package elfeed
  :defer t
  :after evil-leader evil
  :custom-face
  (elfeed-search-date-face ((t (:foreground "brightcyan"))))
  (elfeed-search-feed-face ((t (:foreground "brightgreen"))))
  (elfeed-search-tag-face ((t (:foreground "brightmagenta"))))
  (elfeed-search-title-face ((t (:foreground "blue"))))
  :init
  (evil-define-key 'normal elfeed-search-mode-map
    (kbd "RET") 'my/elfeed-search-show-entry
    (kbd "u") 'elfeed-update
    (kbd "U") 'elfeed-search-update--force
    (kbd "q") 'quit-window
    (kbd "n") 'elfeed-unjam
    (kbd "w") 'elfeed-web-start
    (kbd "W") 'elfeed-web-stop
    (kbd "m") 'my/elfeed-star
    (kbd "M") 'my/elfeed-unstar
    (kbd "t") (lambda () (interactive) (elfeed-search-set-filter "+starred"))
    (kbd "d") (lambda () (interactive) (elfeed-search-set-filter "@1-weeks-ago"))
    (kbd "o") 'elfeed-search-browse-url
    )
  (evil-define-key 'normal elfeed-show-mode-map
    (kbd "q") 'quit-window
    )
  (add-hook 'elfeed-search-update-hook #'my/elfeed-search-add-separators)
  (setq elfeed-search-filter "@1-weeks-ago")
  (setq elfeed-show-entry-switch #'my/show-elfeed)
  )

(use-package ov
  :defer t
  )

(cl-defun my/elfeed-search-add-separators (&key (min-group-size 2))
    "Insert overlay spacers where the current date changes.
If no group has at least MIN-GROUP-SIZE items, no spacers will be
inserted. "
    ;; TODO: Use column-specific functions so that, e.g. date column could be grouped by month/year
    (cl-labels ((count-date-items (date)
                                  (cl-loop for entry in elfeed-search-entries
                                           when (equal date (elfeed-search-format-date (elfeed-entry-date entry)))
                                           count it))
                (insert-date (date &key count)
                             (ov (line-beginning-position) (line-beginning-position)
                                 'before-string (propertize (format "\n%s (%s)\n" date count)
                                                            'face 'elfeed-search-date-face)
                                 'type 'date-separator))
                (entry-date (offset)
                      (when-let ((entry (nth offset elfeed-search-entries)))
                        (elfeed-search-format-date (elfeed-entry-date entry)))))
      (ov-clear)
      (save-excursion
        (goto-char (point-min))
        (cl-loop with largest-group-size = 1
                 with offset = (- 1 elfeed-search--offset) ; 1 is first line
                 with prev-data = (entry-date offset)

                 initially do (insert-date prev-data
                                           :count (count-date-items prev-data))

                 while (not (eobp))
                 do (progn
                      (forward-line 1)
                      (incf offset))

                 for current-data = (entry-date offset)
                 if (not (equal current-data prev-data))
                 do (progn
                      (insert-date current-data
                                   :count (count-date-items current-data))
                      (setq prev-data current-data))
                 else do (incf largest-group-size)

                 finally do (when (< largest-group-size min-group-size)
                              (ov-clear))))))

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

(use-package tabbar
  :defer t
  :custom-face
  (tabbar-default ((t (:inherit variable-pitch :background "black" :foreground "white" :height 0.8))))
  (tabbar-selected ((t (:background "white" :foreground "black" :weight bold :box (:line-width 1 :color "white" :style pressed-button)))))
  :hook ((special-mode) . tabbar-local-mode)
  :init
  (tabbar-mode)
  :config
  (defun tabbar-buffer-groups ()
    (list
     (cond
      ((string-match "elfeed" (buffer-name))
       "Elfeed"
       )
      ((string-equal "*" (substring (buffer-name) 0 1))
       "Emacs Buffer"
       )
      ((eq major-mode 'dired-mode)
       "Dired"
       )
      (t
       "User Buffer"
       )
      )))
  (setq
   tabbar-home-button-value nil
   tabbar-buffer-home-button (quote (("[+]") ""))
   tabbar-home-button (quote (("[+]") ""))
   tabbar-scroll-left-button (quote ((" <") ""))
   tabbar-scroll-right-button (quote ((" >") ""))
   tabbar-buffer-groups-function 'tabbar-buffer-groups
   tabbar-separator (quote (" | ")))
  )

(use-package company-tabnine
  :defer t
  :after company
  :init
  (add-to-list 'company-backends #'company-tabnine)
  )

(use-package evil-nerd-commenter
  :defer t
  :after evil-leader
  :init
  (evil-leader/set-key
    "ci" 'evilnc-comment-or-uncomment-lines
    "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
    "cc" 'evilnc-copy-and-comment-lines
    "cp" 'evilnc-comment-or-uncomment-paragraphs
    "cr" 'comment-or-uncomment-region
    "cv" 'evilnc-toggle-invert-comment-line-by-line
    "."  'evilnc-copy-and-comment-operator
    "\\" 'evilnc-comment-operator ; if you prefer backslash key
    )
  )
