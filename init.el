(setq
 ;; key mappings
 mac-option-key-is-meta nil
 mac-command-key-is-meta t
 mac-command-modifier 'meta
 mac-option-modifier nil)

(setq-default indent-tabs-mode nil
              c-basic-offset 2
              ;; web development
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
              evil-ex-search-case 'sensitive)

(set-face-attribute 'trailing-whitespace nil :background "red" :foreground "brightblack")

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(require 'use-package)


(menu-bar-mode -1)
(use-package drag-stuff
  :defer t
  :config
  (drag-stuff-mode 1)
  )

(use-package evil
  :ensure t
  :bind (
         :map evil-normal-state-map
         ( "H" . next-buffer )
         ( "L" . previous-buffer )
         ( "C-j" . drag-stuff-down )
         ( "C-k" . drag-stuff-up )
         :map evil-visual-state-map
         ( "C-j" . drag-stuff-down )
         ( "C-k" . drag-stuff-up )
         )
  :config
  (evil-mode 1)
  )

(use-package nord-theme
  :ensure t
  :config
  (load-theme 'nord t))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))


(use-package evil-numbers
  :defer t
  :config
  (global-set-key (kbd "C-c +") 'evil-numbers/inc-at-pt)
  (global-set-key (kbd "C-c -") 'evil-numbers/dec-at-pt))

(use-package evil-leader
  :ensure t
  :config
  (evil-leader/set-leader "<SPC>")
  (global-evil-leader-mode))

(use-package evil-commentary
  :ensure t
  :config
  (evil-commentary-mode))

(fset 'evil-visual-update-x-selection 'ignore)
(setq scroll-conservatively 101)

(use-package projectile
  :defer t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  ;; (define-key projectile-m
  ;; ode-map (kbd "C-c p") 'projectile-command-map)
  )

(use-package counsel-projectile
  :ensure t
  :config
  (define-key evil-normal-state-map (kbd "C-p") 'counsel-projectile-find-file))

(use-package ivy
  :ensure t
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
  (evil-leader/set-key "f" 'counsel-rg)
  (evil-leader/set-key "b" 'ivy-switch-buffer)
  (setq ivy-re-builders-alist
      '((t . ivy--regex-fuzzy)))
)

(use-package evil-escape
  :ensure t
  :config
  (evil-escape-mode 1))

(use-package evil-multiedit
  :ensure t
  :config
  ;; Highlights all matches of the selection in the buffer.
  (define-key evil-visual-state-map "R" 'evil-multiedit-match-all)

  ;; Match the word under cursor (i.e. make it an edit region). Consecutive presses will ;; incrementally add the next unmatched match.
  (define-key evil-normal-state-map (kbd "C-n") 'evil-multiedit-match-and-next)
  ;; Match selected region.
  (define-key evil-visual-state-map (kbd "C-n") 'evil-multiedit-and-next)

  ;; Same as M-d but in reverse.
  (define-key evil-normal-state-map (kbd "C-S-n") 'evil-multiedit-match-and-prev)
  (define-key evil-visual-state-map (kbd "C-S-n") 'evil-multiedit-and-prev)

  ;; OPTIONAL: If you prefer to grab symbols rather than words, use
  ;; `evil-multiedit-match-symbol-and-next` (or prev).

  ;; Restore the last group of multiedit regions.
  (define-key evil-visual-state-map (kbd "C-M-D") 'evil-multiedit-restore)

  ;; RET will toggle the region under the cursor
  (define-key evil-multiedit-state-map (kbd "RET") 'evil-multiedit-toggle-or-restrict-region)

  ;; ...and in visual mode, RET will disable all fields outside the selected region
  ;; (define-key evil-motion-state-map (kbd "RET") 'evil-multiedit-toggle-or-restrict-region)

  ;; For moving between edit regions
  (define-key evil-multiedit-state-map (kbd "C-j") 'evil-multiedit-next)
  (define-key evil-multiedit-state-map (kbd "C-k") 'evil-multiedit-prev)

  ;; Ex command that allows you to invoke evil-multiedit with a regular expression, e.g.
  (evil-ex-define-cmd "ie[dit]" 'evil-multiedit-ex-match)
  )

(defun my/pre-popup-draw ()
  "Turn off whitespace mode before showing company complete tooltip"
  (if whitespace-mode
      (progn
        (setq my-prev-whitespace-mode t)
        (whitespace-mode -1)
        (setq my-prev-whitespace-mode t))))

(defun my/post-popup-draw ()
  "Restore previous whitespace mode after showing company tooltip"
  (if my-prev-whitespace-mode
      (progn
        (whitespace-mode 1)
        (setq my-prev-whitespace-mode nil))))

(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  )

(global-display-line-numbers-mode 1)
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
    (set-face-foreground 'highlight-indent-guides-character-face "brightblack")
  )

(use-package telephone-line
  :ensure t
  :config
  (progn


    (telephone-line-evil-config)
    (set-face-attribute 'telephone-line-evil-insert nil :background "white" :foreground "black")
    (set-face-attribute 'telephone-line-evil-normal nil :background "cyan" :foreground "black")
    (set-face-attribute 'telephone-line-evil-visual nil :background "brightcyan" :foreground "black")
    (set-face-attribute 'telephone-line-accent-active nil :background "blue" :foreground "black")
    (set-face-attribute 'mode-line nil :background "brightblack" :foreground "white")
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
            (nil . (mode-line . model-line-inactive))
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

