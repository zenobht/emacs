(fset 'yes-or-no-p 'y-or-n-p)

(if (display-graphic-p)
    (progn
      (set-frame-font "Hasklug Nerd Font-14")
      (menu-bar-mode t)
      )
  (progn
    (menu-bar-mode -1)
    )
  )

(setq-default electric-indent-inhibit t
              evil-shift-width 2
              )

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
 make-backup-files nil
 auto-save-file-name-transforms `((".*" "~/.emacs-saves/" t))
 auto-save-interval 50
 large-file-warning-threshold nil
 vc-follow-symlinks t
 auto-revert-check-vc-info t
 backward-delete-char-untabify-method 'hungry
 initial-major-mode (quote text-mode)
 mouse-wheel-progressive-speed nil
 display-buffer-alist '(("\\`\\*e?shell" display-buffer-pop-up-window))
 )

(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

(setq-default indent-tabs-mode nil)

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

(use-package evil
  :defer t
  :bind (
         :map evil-insert-state-map
         ( "M-/" . yas-expand)
         ( "C-a" . (lambda () (interactive) (evil-paste-from-register ?.)))
         ( "C-r" . evil-paste-from-register)
         :map evil-normal-state-map
         ( "C-u" . evil-scroll-up )
         ( "[e" . previous-error)
         ( "]e" . next-error)
         ( "gc" . evil-ex-nohighlight)
         ( "gs" . my/substitute)
         ( "C-i" . evil-jump-forward)
         ( "C-o" . evil-jump-backward)
         :map evil-visual-state-map
         ( "M-c" . my/copy-to-clipboard)
         ( "M-v" . my/paste-from-clipboard)
         ( "gs" . my/substitute)
         ( "gM" . my/visual-macro)
         )
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
 ( "M-l" . kill-this-buffer)
 ( "M-z" . delete-window)
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

(use-package evil-numbers
  :defer t
  :after evil
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

(use-package paradox
  :defer t
  :after evil-leader
  :init
  (evil-leader/set-key
    "pl" 'paradox-list-packages
    "pu" 'paradox-upgrade-packages
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
    "*" 'my/rg-star-search
    "b" 'ivy-switch-buffer
    "C" 'my/calendar
    "dd" 'deer
    "dr" 'ranger
    "f" 'counsel-rg
    "gb" 'magit-blame
    "gg" 'magit-status
    "gl" 'magit-log
    "j" 'avy-goto-char-2
    "l" 'avy-goto-line
    "s" 'swiper
    "t" 'eshell
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
    "pp" 'counsel-projectile-switch-project
    "pf" 'counsel-projectile-find-file
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
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g k" . dumb-jump-back)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config
  (setq dumb-jump-selector 'ivy))

(use-package rainbow-delimiters
  :defer t
  :hook ((prog-mode) . rainbow-delimiters-mode))

(use-package evil-multiedit
  :defer t
  :after evil
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
   company-dabbrev-downcase 0
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
  :config
  (ranger-override-dired-mode t)
  (setq ranger-cleanup-on-disable t
        ranger-listing-dir-first t
        ranger-persistent-sort t
        )
  (if (not (display-graphic-p))
      (setq ranger-footer-delay nil)
    )
  (when (string= system-type "darwin")
    (setq dired-use-ls-dired nil))
  )

(defun mu-magit-kill-buffers ()
  "Restore window configuration and kill all Magit buffers."
  (interactive)
  (let ((buffers (magit-mode-get-buffers)))
    (magit-restore-window-configuration)
    (mapc #'kill-buffer buffers)))

(use-package evil-magit
  :defer t
  :after evil
  :init
  (evil-magit-init)
  :config
  (bind-key "q" #'mu-magit-kill-buffers magit-status-mode-map)
  )

(use-package display-line-numbers
  :defer t
  :after evil
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
(tool-bar-mode -1)
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

(defun my/mode-hook ()
  (hs-minor-mode)
  (local-set-key (kbd "C-c K") 'hs-show-all) ;; ctrl+shift+=
  (local-set-key (kbd "C-c J") 'hs-hide-all)   ;; ctrl+shift+-
  (local-set-key (kbd "C-c k") 'hs-show-block)
  (local-set-key (kbd "C-c j") 'hs-hide-block)
  )

(add-hook 'text-mode-hook
          (lambda ()
            (interactive)
            (visual-line-mode -1)
            (display-line-numbers-mode +1)
            (whitespace-mode +1)
            (highlight-indent-guides-mode +1)
            (modify-syntax-entry ?_ "w")))

(add-hook 'prog-mode-hook
          (lambda ()
            (interactive)
            (visual-line-mode +1)
            (display-line-numbers-mode +1)
            (whitespace-mode +1)
            (my/mode-hook)
            (highlight-indent-guides-mode +1)
            (modify-syntax-entry ?_ "w")))

(defun simple-mode-line-render (left right)
  "Return a string of `window-width' length containing LEFT, and RIGHT aligned respectively."
  (let* ((available-width (- (window-total-width) (+ (length (format-mode-line left)) (length (format-mode-line right))))))
    (append left (list (format (format "%%%ds" available-width) "")) right)
    )
  )

(defvar my/mode-line-coding-format
  '(:eval
    (propertize
     (concat (pcase (coding-system-eol-type buffer-file-coding-system)
               (0 "  LF")
               (1 "  CRLF")
               (2 "  CR"))
             (let ((sys (coding-system-plist buffer-file-coding-system)))
               (cond ((memq (plist-get sys :category)
                            '(coding-category-undecided coding-category-utf-8))
                      " UTF-8 ")
                     (t (upcase (symbol-name (plist-get sys :name))))))))
    )
  )

(put 'my/mode-line-coding-format 'risky-local-variable t)

(defvar ml-selected-window (frame-selected-window))

(defun set-selected-window ()
  (when (not (minibuffer-window-active-p (frame-selected-window)))
    (setq ml-selected-window (frame-selected-window))))

(defun unset-selected-window ()
  (setq ml-selected-window nil))

(add-hook 'window-configuration-change-hook 'set-selected-window)

(add-hook 'focus-in-hook 'set-selected-window)

(add-hook 'focus-out-hook 'unset-selected-window)

(add-hook 'buffer-list-update-hook #'set-selected-window)

(defun selected-window-active ()
  (eq ml-selected-window (selected-window))
  )

(defun my/shorten-vc-mode-line (string)
  (cond
   ((string-prefix-p "Git" string)
    (concat "\ue0a0 " (magit-get-current-branch)))
   (t
    string)))

(advice-add 'vc-git-mode-line-string :filter-return 'my/shorten-vc-mode-line)

;; remove mode-line-buffer-identification default width
(setq-default mode-line-buffer-identification (propertized-buffer-identification "%b"))

(setq-default
 mode-line-format
 '
 (
  (:eval
   (simple-mode-line-render
    ;; left
    (quote (""
            (:eval (propertize evil-mode-line-tag
                               'face  (if (selected-window-active) 'font-lock-constant-face)
                               'help-echo
                               "Evil mode"))
            " %I"
            (:eval (when (projectile-project-p)
                     (propertize (concat " [" (projectile-project-name) "] ")
                                 'face (if (selected-window-active)
                                           '(:inherit font-lock-string-face :weight bold))
                                 'help-echo "Project Name")
                     ))
            mode-line-buffer-identification
            (:eval (propertize (if (buffer-modified-p)
                                   " [+]"
                                 " ")
                               'help-echo "Buffer modified"))
            (:eval (when buffer-read-only
                     (propertize " RO"
                                 'face (if (selected-window-active)
                                           '(:inherit error))
                                 'help-echo "Buffer is read-only")))
            " %l:%c"
            " %p "
            (flycheck-mode flycheck-mode-line)
            ))
    ;; right
    (quote (
            (vc-mode (:eval (propertize vc-mode
                                        'face (if (selected-window-active)
                                                  '(:inherit font-lock-regexp-grouping-backslash :weight bold))
                                        'help-echo "Buffer modified")))
            "  "
            (:eval (propertize mode-name
                               'face (if (selected-window-active)
                                         '(:inherit font-lock-function-name-face :slant normal))
                               ))

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
            (evil-define-key 'normal org-mode-map (kbd "TAB") #'org-cycle)
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

(defun my/setup-tools-from-node ()
  ;; (my/eslint-setup)
  ;; (my/prettier-setup)
  (prettier-js-mode +1)
  )


(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  ;; (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  )

(use-package typescript-mode
  :defer t
  :mode(("\\.tsx\\'" . typescript-mode)
        ("\\.ts\\'" . typescript-mode))
  :config
  (setq typescript-indent-level 2)
  (add-hook 'typescript-mode-hook 'flycheck-mode)
  (add-hook 'typescript-mode #'subword-mode))

(use-package tide
  :init
  :defer t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(use-package web-mode
  :defer t
  :after flycheck
  :mode (("\\.html?\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-block-padding 2
        web-mode-comment-style 2

        web-mode-enable-css-colorization t
        web-mode-enable-auto-pairing t
        web-mode-enable-comment-keywords t
        web-mode-enable-current-element-highlight t
        )
  )

(use-package rjsx-mode
  :defer t
  :mode(("\\.js\\'" . rjsx-mode)
        ("\\.jsx\\'" . rjsx-mode))
  :config
  (define-key rjsx-mode-map "<" nil)
  (define-key rjsx-mode-map (kbd "C-d") nil)
  (define-key rjsx-mode-map ">" nil)
  (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (setq js2-mode-show-strict-warnings nil
        js2-mode-show-parse-errors nil)
  (evil-define-key 'insert rjsx-mode-map
    (kbd ">") 'rjsx-electric-gt
    (kbd "<") 'rjsx-electric-lt
    )
  )

(add-hook 'emacs-lisp-mode-hook (lambda () (flycheck-mode -1)))

(use-package flycheck
  :defer t
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

(use-package elfeed
  :defer t
  :after evil-leader evil
  :init
  (evil-define-key 'normal elfeed-search-mode-map
    (kbd "RET") 'elfeed-search-show-entry
    (kbd "!") 'elfeed-update
    (kbd "q") 'quit-window
    (kbd "n") 'elfeed-unjam
    (kbd "w") 'elfeed-web-start
    (kbd "W") 'elfeed-web-stop
    (kbd "R") 'my/elfeed-mark-all-read
    (kbd "u") 'elfeed-search-tag-all-unread
    (kbd "r") 'elfeed-search-untag-all-unread
    (kbd "m") 'my/elfeed-star
    (kbd "M") 'my/elfeed-unstar
    (kbd "t") (lambda () (interactive) (elfeed-search-set-filter "+starred"))
    (kbd "d") (lambda () (interactive) (elfeed-search-set-filter "@1-weeks-ago"))
    (kbd "o") 'elfeed-search-browse-url
    )
  (evil-define-key 'visual elfeed-search-mode-map
    (kbd "r") 'elfeed-search-untag-all-unread
    (kbd "u") 'elfeed-search-tag-all-unread
    )
  (evil-define-key 'normal elfeed-show-mode-map
    (kbd "q") 'quit-window
    )
  (add-hook 'elfeed-search-update-hook #'my/elfeed-search-add-separators)
  (setq elfeed-search-filter "@2-days-ago")
  (setq elfeed-show-entry-switch #'my/show-elfeed)
  (add-hook 'elfeed-show-mode-hook
            (lambda ()
              (let ((inhibit-read-only t)
                    (inhibit-modification-hooks t))
                (setq-local truncate-lines nil)
                (setq-local shr-width 85)
                (set-buffer-modified-p nil))
              (setq-local left-margin-width 15)
              (setq-local right-margin-width 15)
              ))
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
   tabbar-background-color nil
   tabbar-home-button-value nil
   tabbar-buffer-home-button (quote (("[+]") ""))
   tabbar-home-button (quote (("[+]") ""))
   tabbar-scroll-left-button (quote ((" <") ""))
   tabbar-scroll-right-button (quote ((" >") ""))
   tabbar-buffer-groups-function 'tabbar-buffer-groups
   tabbar-separator (quote ("  ")))
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

(use-package elpy
  :defer t
  :bind (("M-g g" . elpy-goto-definition))
  :init
  (advice-add 'python-mode :before 'elpy-enable))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package ein
  :defer t
  :config
  (progn
    (setq ein:jupyter-default-notebook-directory "~/jupyter_notebooks/")
    (with-eval-after-load 'ein-notebooklist
      (evil-define-key 'normal ein:notebook-multilang-mode-map
        (kbd "j") 'ein:worksheet-goto-next-input
        (kbd "k") 'ein:worksheet-goto-prev-input
        (kbd "J") 'ein:worksheet-move-cell-down
        (kbd "K") 'ein:worksheet-move-cell-up
        (kbd "D") 'ein:worksheet-kill-cell
        (kbd "o") 'ein:worksheet-insert-cell-below
        (kbd "O") 'ein:worksheet-insert-cell-above
        (kbd "M-RET") 'ein:worksheet-execute-cell-and-goto-next
        (kbd "RET") 'ein:worksheet-execute-cell
        (kbd "C-l") 'ein:worksheet-clear-output
        (kbd "C-S-l") 'ein:worksheet-clear-all-output)
      (evil-define-key 'insert ein:notebook-multilang-mode-map
        (kbd "M-RET") 'ein:worksheet-execute-cell-and-goto-next)
      )
    )
  )

(use-package highlight-indent-guides
  :defer t
  :config
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-character ?\|)
  )
