(setq start-time (current-time)
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      file-name-handler-alist nil
      TERM (getenv-internal "TERM" initial-environment)
      PATH (getenv-internal "PATH" initial-environment)
      )

(fset 'yes-or-no-p 'y-or-n-p)
(defvar emacs-d "~/.emacs.d/config/")
(setq package-user-dir "~/.emacs.d/elpa/")

(add-to-list 'load-path emacs-d)
(add-to-list 'load-path (expand-file-name "modes/" emacs-d))
(package-initialize)
(setq-default custom-file (expand-file-name ".custom.el" user-emacs-directory)
              indent-tabs-mode nil)

(if (display-graphic-p)
    (progn
      (set-frame-font "OperatorMono Nerd Font-15")
      (set-frame-parameter nil 'internal-border-width 3)
      )
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
 auto-save-default nil
 large-file-warning-threshold nil
 vc-follow-symlinks t
 auto-revert-check-vc-info t
 backward-delete-char-untabify-method 'hungry
 initial-major-mode (quote text-mode)
 mouse-wheel-progressive-speed nil
 display-buffer-alist '(("\\`\\*e?shell" display-buffer-pop-up-window))
 create-lockfiles nil
 column-number-mode t
 locale-coding-system 'utf-8
 nord-visual "#EBCB8B"
 nord-normal "#B48EAD"
 nord-insert "#A3BE8C"
 load-prefer-newer t
 )

(set-default-coding-systems 'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-language-environment    'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system        'utf-8)
(set-input-method nil)

(modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table)
(modify-syntax-entry ?_ "w")

(show-paren-mode +1)
(delete-selection-mode +1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(visual-line-mode +1)
(xterm-mouse-mode +1)
(pixel-scroll-mode +1)

(modify-syntax-entry ?_ "w")

(load (concat emacs-d "loaddefs.el") nil t)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")))

(require 'use-package)
(use-package ranger
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
(require 'hooks)

;; (global-auto-revert-mode 1)
;; (setq auto-revert-verbose nil)

(add-to-list 'auto-mode-alist '("\\.kt$" . kotlin-mode))
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.\\(tsx\\|ts\\)\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.\\(js\\|jsx\\)\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(add-to-list 'auto-mode-alist '("\\.clj\\'" . prog-mode))

