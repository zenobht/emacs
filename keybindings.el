
  ;; :bind (
  ;;        :map evil-insert-state-map
  ;;        ( "M-/" . yas-expand)
  ;;        ( "C-a" . (lambda () (interactive) (evil-paste-from-register ?.)))
  ;;        ( "C-r" . evil-paste-from-register)
  ;;        :map evil-normal-state-map
  ;;        ( "C-u" . evil-scroll-up )
  ;;        ( "gc" . evil-ex-nohighlight)
  ;;        ( "gs" . my/substitute)
  ;;        ( "C-i" . evil-jump-forward)
  ;;        ( "C-o" . evil-jump-backward)
  ;;        :map evil-visual-state-map
  ;;        ( "M-c" . my/copy-to-clipboard)
  ;;        ( "M-v" . my/paste-from-clipboard)
  ;;        ( "gs" . my/substitute)
  ;;        ( "gM" . my/visual-macro)
  ;;        )

;; (bind-keys*
;;  ( "M-t" . tabbar-backward-group)
;;  ( "C-j" . tabbar-backward-tab)
;;  ( "C-k" . tabbar-forward-tab)
;;  ( "M-l" . kill-this-buffer)
;;  ( "M-z" . delete-window)
;;  ( "M-f" . find-file)
;;  ( "M-r" . my/last-used-buffer)
;;  ( "M-v" . (lambda () (interactive) (message "Pasted from clipboard")))
;;  ( "M-c" . my/copy-to-clipboard)
;;  ( "M-e" . elfeed)
;;  ( "M-n" . my/new-empty-buffer)
;;  )

;; (unbind-key "C-l" global-map)

;; evil-leader
  ;; :config
  ;; (evil-leader/set-leader "<SPC>")
  ;; (evil-leader/set-key
  ;;   "s*" 'my/rg-star-search
  ;;   "bb" 'ivy-switch-buffer
  ;;   "C" 'my/calendar
  ;;   "dd" 'deer
  ;;   "dr" 'ranger
  ;;   "sf" 'counsel-rg
  ;;   "mb" 'magit-blame
  ;;   "mg" 'magit-status
  ;;   "ml" 'magit-log
  ;;   "jj" 'avy-goto-char-2
  ;;   "jl" 'avy-goto-line
  ;;   "ss" 'swiper
  ;;   "t" 'eshell
  ;;   )

;; projectile
  ;; (evil-leader/set-key
  ;;   "pb" 'counsel-projectile-switch-to-buffer
  ;;   "pp" 'counsel-projectile-switch-project
  ;;   "pf" 'counsel-projectile-find-file
  ;;   "pi" 'projectile-invalidate-cache
  ;;   )

 ;; ivy
  ;; :bind (
  ;;        ("M-x" . counsel-M-x)
  ;;        :map ivy-minibuffer-map
  ;;        ([escape] . keyboard-escape-quit)
  ;;        ("C-j" . ivy-next-line)
  ;;        ("C-k" . ivy-previous-line)
  ;;        )

;; dumb-jump
  ;; :bind (("M-g o" . dumb-jump-go-other-window)
  ;;        ("M-g j" . dumb-jump-go)
  ;;        ("M-g k" . dumb-jump-back)
  ;;        ("M-g i" . dumb-jump-go-prompt)
  ;;        ("M-g x" . dumb-jump-go-prefer-external)
  ;;        ("M-g z" . dumb-jump-go-prefer-external-other-window))

;; multiedit
  ;; :bind (
  ;;        :map evil-visual-state-map
  ;;        ("R" . evil-multiedit-match-all)
  ;;        ("M-D" . evil-multiedit-match-and-prev)
  ;;        ("M-d" . evil-multiedit-match-and-next)
  ;;        ("C-M-D" . evil-multiedit-restore)
  ;;        :map evil-normal-state-map
  ;;        ("M-D" . evil-multiedit-match-and-prev)
  ;;        ("M-d" . evil-multiedit-match-and-next)
  ;;        :map evil-multiedit-state-map
  ;;        ("RET" . evil-multiedit-toggle-or-restrict-region)
  ;;        ("C-j" . evil-multiedit-next)
  ;;        ("C-k" . evil-multiedit-prev)
  ;;        )

;; company
  ;; :bind (:map company-active-map
  ;;             ("C-j" . company-select-next)
  ;;             ("C-k" . company-select-previous)
  ;;             )

;; ranger
  ;; :bind (:map ranger-normal-mode-map
  ;;             ( "+" . dired-create-directory)
  ;;             )


;; neotree
;;   (progn
;;     (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
;;     (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
;;     (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
;;     (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
;;     (evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
;;     (evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
;;     (evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
;;     (evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
;;     (evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)
;;     )
  ;; (evil-leader/set-key
  ;;   "n" 'my/neotree-project-dir)

;; expand-region
;;   :bind (
;;          :map evil-visual-state-map
;;          ( "v" . er/expand-region)
;;          )



;; rjsx
;;   (define-key rjsx-mode-map "<" nil)
;;   (define-key rjsx-mode-map (kbd "C-d") nil)
;;   (define-key rjsx-mode-map ">" nil)
;;   (evil-define-key 'insert rjsx-mode-map
;;     (kbd ">") 'rjsx-electric-gt
;;     (kbd "<") 'rjsx-electric-lt
;;     )


;; ace-window
;;   :bind* ("M-o" . ace-window)




;; elfeed
;;   (evil-define-key 'normal elfeed-search-mode-map
;;     (kbd "RET") 'elfeed-search-show-entry
;;     (kbd "!") 'elfeed-update
;;     (kbd "q") 'quit-window
;;     (kbd "n") 'elfeed-unjam
;;     (kbd "w") 'elfeed-web-start
;;     (kbd "W") 'elfeed-web-stop
;;     (kbd "R") 'my/elfeed-mark-all-read
;;     (kbd "u") 'elfeed-search-tag-all-unread
;;     (kbd "r") 'elfeed-search-untag-all-unread
;;     (kbd "m") 'my/elfeed-star
;;     (kbd "M") 'my/elfeed-unstar
;;     (kbd "t") (lambda () (interactive) (elfeed-search-set-filter "+starred"))
;;     (kbd "d") (lambda () (interactive) (elfeed-search-set-filter "@1-weeks-ago"))
;;     (kbd "o") 'elfeed-search-browse-url
;;     )
;;   (evil-define-key 'visual elfeed-search-mode-map
;;     (kbd "r") 'elfeed-search-untag-all-unread
;;     (kbd "u") 'elfeed-search-tag-all-unread
;;     )
;;   (evil-define-key 'normal elfeed-show-mode-map
;;     (kbd "q") 'quit-window
;;     )

;; evil-nerd-commenter
;;   (evil-leader/set-key
;;     "ci" 'evilnc-comment-or-uncomment-lines
;;     "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
;;     "cc" 'evilnc-copy-and-comment-lines
;;     "cp" 'evilnc-comment-or-uncomment-paragraphs
;;     "cr" 'comment-or-uncomment-region
;;     "cv" 'evilnc-toggle-invert-comment-line-by-line
;;     "."  'evilnc-copy-and-comment-operator
;;     "\\" 'evilnc-comment-operator ; if you prefer backslash key
;;     )


;; elpy
;;   :bind (("M-g g" . elpy-goto-definition))
