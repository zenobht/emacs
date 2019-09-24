; (setq start-time (current-time))
;; (setq mode-line-format nil)

;; ;; assign high memory to reduce gc during load
;; ;; lifted from doom-emacs. Thanks
;; (defvar doom--file-name-handler-alist file-name-handler-alist)
;; (setq gc-cons-threshold 402653184
;;       gc-cons-percentage 0.6
;;       file-name-handler-alist nil
;;       )

;; (setq-default custom-file (expand-file-name ".custom.el" user-emacs-directory))

;; (defun load-directory (directory)

;;   "Load recursively all `.el' files in DIRECTORY."
;;   (dolist (element (directory-files-and-attributes directory nil nil nil))
;;     (let* ((path (car element))
;;            (fullpath (concat directory "/" path))
;;            (isdir (car (cdr element)))
;;            (ignore-dir (or (string= path ".") (string= path ".."))))
;;       (cond
;;        ((and (eq isdir t) (not ignore-dir))
;;         (load-directory fullpath))
;;        ((and (eq isdir nil) (string= (substring path -3) ".el"))
;;         (load (file-name-sans-extension fullpath)))))))

;; (add-hook 'after-init-hook (lambda ()
;;     (load-directory "~/.emacs.d/lib")
;;     (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;;     (load-file (expand-file-name "functions.el" user-emacs-directory))
;;     (load-file (expand-file-name "packages.el" user-emacs-directory))
;;     (load-theme 'nord t)
;;     (load-file (expand-file-name "setup.el" user-emacs-directory))
;;     (load-file (expand-file-name "keybindings.el" user-emacs-directory))
;;     (when (file-exists-p custom-file)
;;       (load custom-file))
;;   ))

;; (defun my/after-startup ()
;;   (message (concat (format-time-string "%Y-%m-%dT%H:%M:%S") " in emacs-startup-hook"))
;;   (message "Emacs ready in %s with %d garbage collections."
;;            (format "%.2f seconds"
;;                    (float-time
;;                     (time-subtract (current-time) start-time)))
;;            gcs-done)

;;   ;; set proper gc values after load
;;   (setq gc-cons-threshold 16777216
;;         gc-cons-percentage 0.1
;;         file-name-handler-alist doom--file-name-handler-alist)
;;  )

;; (add-hook 'emacs-startup-hook #'my/after-startup)

(defvar emacs-d "~/.emacs.d/config/")
(setq package-user-dir "~/.emacs.d/elpa/")

(package-initialize)
(add-to-list 'load-path emacs-d)
(add-to-list 'load-path (expand-file-name "modes/" emacs-d))

(if (display-graphic-p)
    (progn
      (set-frame-font "OperatorMono Nerd Font-15")
      (set-frame-parameter nil 'internal-border-width 3)
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
 create-lockfiles nil
 )

(setq-default indent-tabs-mode nil)

(add-hook 'eshell-exit-hook (lambda () (interactive) (delete-window)))

(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-language-environment    'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system   'utf-8)
(prefer-coding-system        'utf-8)
(set-input-method nil)

(fset 'yes-or-no-p 'y-or-n-p)

(setq nord-visual "#EBCB8B"
      nord-normal "#B48EAD"
      nord-insert "#A3BE8C")

(modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table)

(add-to-list 'auto-mode-alist '("\\.clj\\'" . prog-mode))
(show-paren-mode 1)
(delete-selection-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(visual-line-mode +1)
(modify-syntax-entry ?_ "w")
(load (concat emacs-d "loaddefs.el") nil t)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")))

(let ((file-name-handler-alist nil))
  (require 'exec-path-from-shell)
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize)
  (require 'nord-theme)
  (load-theme 'nord t)
  (require 'use-package)
  (require 'general)
  (require 'smex)
  )

(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)

(require 'b-evil)
(require 'b-ivy)

; (require 'hooks)
; (require 'keybindigs)
; (defvar my/mode-line-coding-format
;   '(:eval
;     (propertize
;      (concat (pcase (coding-system-eol-type buffer-file-coding-system)
;                (0 "  LF")
;                (1 "  CRLF")
;                (2 "  CR"))
;              (let ((sys (coding-system-plist buffer-file-coding-system)))
;                (cond ((memq (plist-get sys :category)
;                             '(coding-category-undecided coding-category-utf-8))
;                       " UTF-8 ")
;                      (t (upcase (symbol-name (plist-get sys :name))))))))
;     )
;   )

; (put 'my/mode-line-coding-format 'risky-local-variable t)

; (defvar ml-selected-window nil)

; (add-function :before pre-redisplay-function #'my/set-selected-window)

; (advice-add 'vc-git-mode-line-string :filter-return 'my/shorten-vc-mode-line)

; ;; remove mode-line-buffer-identification default width
; (setq-default mode-line-buffer-identification (propertized-buffer-identification "%b"))

; (setq-default
;  mode-line-format
;  '
;  (
;   (:eval
;    (simple-mode-line-render
;     ;; left
;     (quote (""
;             ;; (:eval (propertize evil-mode-line-tag
;             (:eval (propertize " "
;                                'face  (if (my/selected-window-active)
;                                           (cond
;                                            ((eq evil-state 'visual) `(:background ,nord-visual))
;                                            ((eq evil-state 'insert) `(:background ,nord-insert))
;                                            (t `(:background ,nord-normal))
;                                            )
;                                         )
;                                'help-echo
;                                "Evil mode"))
;             " %I "
;             (:eval (when (projectile-project-p)
;                      (propertize (concat " [" (projectile-project-name) "] ")
;                                  'face (if (my/selected-window-active)
;                                            '(:inherit font-lock-string-face :weight bold))
;                                  'help-echo "Project Name")
;                      ))
;             " "
;             mode-line-buffer-identification
;             " "
;             (:eval (propertize (if (buffer-modified-p)
;                                    " [+] "
;                                  " ")
;                                'help-echo "Buffer modified"))
;             (:eval (when buffer-read-only
;                      (propertize " RO "
;                                  'face (if (my/selected-window-active)
;                                            '(:inherit error))
;                                  'help-echo "Buffer is read-only")))
;             (flycheck-mode flycheck-mode-line)
;             "  %p  %l:%c  "
;             ))
;     ;; right
;     (quote (
;             (vc-mode (:eval (propertize vc-mode
;                                         'face (if (my/selected-window-active)
;                                                   '(:inherit font-lock-regexp-grouping-backslash :weight bold))
;                                         'help-echo "Buffer modified")))
;             "  "
;             (:eval (propertize mode-name
;                                'face (if (my/selected-window-active)
;                                          '(:inherit font-lock-function-name-face :slant normal))
;                                ))

;             my/mode-line-coding-format
;             ))
;     )
;    )
;   )
;  )

(xterm-mouse-mode 1)
(pixel-scroll-mode)
(unless window-system
  (global-set-key (kbd "<mouse-4>") 'my/mousewheel-scroll-down)
  (global-set-key (kbd "<mouse-5>") 'my/mousewheel-scroll-up))
