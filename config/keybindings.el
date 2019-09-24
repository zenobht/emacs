(require 'which-key)
(require 'general)
(which-key-mode)

(general-define-key
 :states 'insert
 "C-a" (lambda () (interactive) (evil-paste-from-register ?.))
 "C-r" 'evil-paste-from-register
 "C-l" 'my/trimn-next-char
 "M-/" 'hippie-expand
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
 "[B" '(hs-show-all :which-key "Open all")
 "[b" '(hs-show-block :which-key "Open block")
 "[e" '(my/previous-error :which-key "Previous error")
 "]B" '(hs-hide-all :which-key "Hide all")
 "]b" '(hs-hide-block :which-key "Hide block")
 "]e" '(my/next-error :which-key "Next error")
 "gh" 'evil-ex-nohighlight
 "gs" 'my/substitute
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
 "M-f" 'tabbar-forward-group
 "M-l" 'kill-this-buffer
 "M-n" 'my/new-empty-buffer
 "M-o" 'ace-window
 "M-r" 'my/last-used-buffer
 "M-t" 'delete-window
 "M-v" (lambda () (interactive) (message "Pasted from clipboard"))
 "M-x" 'counsel-M-x
 )

(general-create-definer my-comment
  :prefix "g c"
  )

(my-comment
  :keymaps '(normal visual)
  "c" 'evilnc-comment-or-uncomment-lines
  )

(unbind-key "C-l" global-map)

(general-create-definer my-leader-def
  :prefix "SPC"
  )

(my-leader-def
  :keymaps 'normal
  "*" '(my/rg-star-search :which-key "Find Current Word")
  "+" '(make-directory :which-key "Make Directory")
  "-" '(counsel-find-file :which-key "Find File")
  "." '(:ignore t :which-key "Paradox")
  ".l" '(paradox-list-packages :which-key "List Packages")
  ".u" '(paradox-upgrade-packages :which-key "Update Packages")
  ";" '(frog-jump-buffer :which-key "Quick Jump Buffer")
  "C" '(my/calendar :which-key "Calendar")
  "b" '(ivy-switch-buffer :which-key "Switch Buffer")
  "d" '(:ignore t :which-key "Dired")
  "dd" '(deer :which-key "Deer")
  "dr" '(ranger :which-key "Ranger")
  "f" '(:ignore t :which-key "Find")
  "ff" '(counsel-rg :which-key "Rg")
  "fs" '(swiper :which-key "Swiper")
  "g" '(:ignore t :which-key "Git")
  "gb" '(magit-blame :which-key "Blame")
  "gg" '(magit-status :which-key "Git Status")
  "gl" '(magit-log :which-key "Log")
  "j" '(:ignore t :which-key "Jump")
  "jb" 'dumb-jump-back
  "jg" 'dumb-jump-go
  "jj" '(avy-goto-char-2 :which-key "Jump to char")
  "jl" '(avy-goto-line :which-key "Jump to line")
  "jo" 'dumb-jump-go-other-window
  "n" '(my/neotree-project-dir :which-key "Neotree")
  "p" '(:ignore t :which-key "Projectile")
  "pb" '(counsel-projectile-switch-to-buffer :which-key "Switch buffer")
  "pf" '(counsel-projectile-find-file :which-key "Find file")
  "pi" '(projectile-invalidate-cache :which-key "Invalidate cache")
  "pp" '(counsel-projectile-switch-project :which-key "Switch project")
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
 :states 'insert
 :keymaps 'rjsx-mode-map
 ">" 'rjsx-electric-gt
 "<" 'rjsx-electric-lt
 "C-d" nil
 )

(general-define-key
 :states 'normal
 :keymaps 'elfeed-search-mode-map
 "RET" 'elfeed-search-show-entry
 "!" 'elfeed-update
 "q" 'kill-this-buffer
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
 :states 'normal
 :keymaps 'org-mode-map
 "TAB" 'org-cycle
 "C-j" 'tabbar-backward-tab
 "C-k" 'tabbar-forward-tab
 "M-g"  'hydra-org/body
 )

(defhydra hydra-org ()
  "Org"
  ("f" org-next-visible-heading "Next Heading")
  ("b" org-previous-visible-heading "Previous Heading")
  ("h" org-shiftleft "Org Shift Left")
  ("l" org-shiftright "Org Shift Right")
  ("J" my/org-move-up "Move up")
  ("K" my/org-move-down "Move down")
  ("H" org-shiftmetaleft "Org Shift Meta Left")
  ("L" org-shiftmetaright "Org Shift Meta Right")
  )

(general-define-key
 :states 'normal
 :keymaps 'symbol-overlay-map
 [escape] 'symbol-overlay-remove-all
 )

(unless window-system
  (global-set-key (kbd "<mouse-4>") 'my/mousewheel-scroll-down)
  (global-set-key (kbd "<mouse-5>") 'my/mousewheel-scroll-up))

(provide 'keybindings)
