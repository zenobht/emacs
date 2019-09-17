(fset 'yes-or-no-p 'y-or-n-p)

(if (display-graphic-p)
    (progn
      (set-frame-font "Hasklug Nerd Font-13")
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


(modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table)

(add-to-list 'auto-mode-alist '("\\.clj\\'" . prog-mode))
(show-paren-mode 1)
(delete-selection-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(visual-line-mode +1)
(modify-syntax-entry ?_ "w")

(add-hook 'minibuffer-setup-hook #'my/disable-in-minibuffer)
(add-hook 'minibuffer-exit-hook #'my/enable-on-minibuffer-exit)

(add-hook 'text-mode-hook
          (lambda ()
            (interactive)
            (visual-line-mode -1)
            ))

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

(defvar ml-selected-window nil)

(defun set-selected-window (windows)
  (when (not (minibuffer-window-active-p (frame-selected-window)))
    (setq ml-selected-window (selected-window))))

(add-function :before pre-redisplay-function #'set-selected-window)

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
            " %I "
            (:eval (when (projectile-project-p)
                     (propertize (concat " [" (projectile-project-name) "] ")
                                 'face (if (selected-window-active)
                                           '(:inherit font-lock-string-face :weight bold))
                                 'help-echo "Project Name")
                     ))
            " "
            mode-line-buffer-identification
            " "
            (:eval (propertize (if (buffer-modified-p)
                                   " [+] "
                                 " ")
                               'help-echo "Buffer modified"))
            (:eval (when buffer-read-only
                     (propertize " RO "
                                 'face (if (selected-window-active)
                                           '(:inherit error))
                                 'help-echo "Buffer is read-only")))
            (flycheck-mode flycheck-mode-line)
            "  %p  %l:%c  "
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

(add-hook 'after-change-major-mode-hook #'my/configure)
(add-hook 'emacs-lisp-mode-hook (lambda () (flycheck-mode -1)))

(xterm-mouse-mode 1)
(pixel-scroll-mode)
(unless window-system
  (global-set-key (kbd "<mouse-4>") 'my/mousewheel-scroll-down)
  (global-set-key (kbd "<mouse-5>") 'my/mousewheel-scroll-up))

(add-hook 'conf-mode-hook (lambda () (run-hooks 'prog-mode-hook)))
