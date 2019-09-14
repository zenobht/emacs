(general-define-key
 :states 'insert
 "C-a" (lambda () (interactive) (evil-paste-from-register ?.))
 "C-r" 'evil-paste-from-register
 "M-/" 'yas-expand
 )

(general-define-key
 :states 'normal
 "C-i" 'evil-jump-forward
 "C-o" 'evil-jump-backward
 "C-u" 'evil-scroll-up
 "M-D" 'evil-multiedit-match-and-prev
 "M-d" 'evil-multiedit-match-and-next
 "M-j" 'evil-move-down
 "M-k" 'evil-move-up
 "C-c +" 'evil-numbers/inc-at-pt
 "gs" 'my/substitute
 "C-c -" 'evil-numbers/dec-at-pt
 )


(general-define-key
 :states 'visual
 "<" 'my/visual-shift-left
 ">" 'my/visual-shift-right
 "C-M-D" 'evil-multiedit-restore
 "M-D" 'evil-multiedit-match-and-prev
 "M-c" 'my/copy-to-clipboard
 "M-d" 'evil-multiedit-match-and-next
 "M-j" 'evil-move-down
 "M-k" 'evil-move-up
 "M-v" 'my/paste-from-clipboard
 "R" 'evil-multiedit-match-all
 "gM" 'my/visual-macro
 "gs" 'my/substitute
 "v" 'er/expand-region
 )

(general-define-key
 "C-j" 'tabbar-backward-tab
 "C-k" 'tabbar-forward-tab
 "M-c" 'my/copy-to-clipboard
 "M-e" 'elfeed
 "M-f" 'find-file
 "M-g i" 'dumb-jump-go-prompt
 "M-g j" 'dumb-jump-go
 "M-g k" 'dumb-jump-back
 "M-g o" 'dumb-jump-go-other-window
 "M-g x" 'dumb-jump-go-prefer-external
 "M-g z" 'dumb-jump-go-prefer-external-other-window
 "M-l" 'kill-this-buffer
 "M-n" 'my/new-empty-buffer
 "M-o" 'ace-window
 "M-r" 'my/last-used-buffer
 "M-t" 'tabbar-backward-group
 "M-v" (lambda () (interactive) (message "Pasted from clipboard"))
 "M-x" 'counsel-M-x
 "M-z" 'delete-window
 )

(general-create-definer my-comment
   :prefix "g c"
   )

 (my-comment
   :keymaps '(normal visual)
   "c" 'evilnc-comment-or-uncomment-lines
   "h" 'evil-ex-nohighlight
   )

(unbind-key "C-l" global-map)

(general-create-definer my-leader-def
  :prefix "SPC"
  )

(my-leader-def
  :keymaps 'normal
  ";" '(frog-jump-buffer :which-key "Quick Jump Buffer")
  "." '(:ignore t :which-key "Paradox")
  ".l" 'paradox-list-packages
  ".u" 'paradox-upgrade-packages
  "C" '(my/calendar :which-key "Calendar")
  "b" '(:ignore t :which-key "Buffer")
  "bb" 'ivy-switch-buffer
  "d" '(:ignore t :which-key "Dired")
  "dd" 'deer
  "dd" 'deer
  "dr" 'ranger
  "j" '(:ignore t :which-key "Jump")
  "jj" 'avy-goto-char-2
  "jl" 'avy-goto-line
  "m" '(:ignore t :which-key "Magit")
  "mb" 'magit-blame
  "mg" 'magit-status
  "ml" 'magit-log
  "n" '(my/neotree-project-dir :which-key "Neotree")
  "p" '(:ignore t :which-key "Projectile")
  "pb" 'counsel-projectile-switch-to-buffer
  "pf" 'counsel-projectile-find-file
  "pi" 'projectile-invalidate-cache
  "pp" 'counsel-projectile-switch-project
  "s" '(:ignore t :which-key "Search")
  "s*" 'my/rg-star-search
  "sf" 'counsel-rg
  "ss" 'swiper
  "t" '(eshell :which-key "Terminal")
  )

(general-define-key
 :keymaps 'ivy-minibuffer-map
 "C-j" 'ivy-next-line
 "C-k" 'ivy-previous-line
 [escape] 'keyboard-escape-quit
 )

(general-define-key
 :keymaps 'evil-multiedit-state-map
 "RET" 'evil-multiedit-toggle-or-restrict-region
 "C-j" 'evil-multiedit-next
 "C-k" 'evil-multiedit-prev
 )

(general-define-key
 :keymaps 'company-active-map
 "C-j" 'company-select-next
 "C-k" 'company-select-previous
 )

(general-define-key
 :keymaps 'ranger-normal-mode-map
 "+" 'dired-create-directory
 )

(general-define-key
 :states 'normal
 :keymaps 'neotree-mode-map
 "TAB" 'neotree-enter
 "SPC" 'neotree-quick-look
 "q" 'neotree-hide
 "RET" 'neotree-enter
 "g" 'neotree-refresh
 "n" 'neotree-next-line
 "p" 'neotree-previous-line
 "A" 'neotree-stretch-toggle
 "H" 'neotree-hidden-file-toggle
 )

(general-define-key
 :keymap 'rjsx-mode-map
 "<" nil
 "C-d" nil
  ">" nil
 )

(general-define-key
 :states 'insert
 :keymaps 'rjsx-mode-map
 ">" 'rjsx-electric-gt
 "<" 'rjsx-electric-lt
 )

(general-define-key
 :states 'normal
 :keymaps 'elfeed-search-mode-map
 "RET" 'elfeed-search-show-entry
 "!" 'elfeed-update
 "q" 'quit-window
 "n" 'elfeed-unjam
 "w" 'elfeed-web-start
 "W" 'elfeed-web-stop
 "R" 'my/elfeed-mark-all-read
 "u" 'elfeed-search-tag-all-unread
 "r" 'elfeed-search-untag-all-unread
 "m" 'my/elfeed-star
 "M" 'my/elfeed-unstar
 "t" (lambda () (interactive) (elfeed-search-set-filter "+starred"))
 "d" (lambda () (interactive) (elfeed-search-set-filter "@1-weeks-ago"))
 "o" 'elfeed-search-browse-url
 )

(general-define-key
 :states 'visual
 :keymaps 'elfeed-search-mode-map
 "r" 'elfeed-search-untag-all-unread
 "u" 'elfeed-search-tag-all-unread
 )

(general-define-key
 :states 'normal
 :keymaps 'elfeed-show-mode-map
 "q" 'quit-window
 )

(general-define-key
 :states 'normal
 "q" 'quit-window
 )

;; elpy
;;   :bind (("M-g g" . elpy-goto-definition))

(general-define-key
 :keymap 'org-mode-map
 "<C-up>" 'my/org-move-up
 "<C-down>" 'my/org-move-down
 )

;; (general-define-key
;;  :states 'normal
;;  :keymap 'magit-status-mode-map
;;  "q" 'my/magit-kill-buffers
;;  )
