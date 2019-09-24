(require 'magit)
(require 'evil)
(require 'projectile)

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

(add-function :before pre-redisplay-function #'my/set-selected-window)

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
            ;; (:eval (propertize evil-mode-line-tag
            (:eval (propertize " "
                               'face  (if (my/selected-window-active)
                                          (cond
                                           ((eq evil-state 'visual) `(:background ,nord-visual))
                                           ((eq evil-state 'insert) `(:background ,nord-insert))
                                           (t `(:background ,nord-normal))
                                           )
                                        )
                               'help-echo
                               "Evil mode"))
            " %I "
            (:eval (when (projectile-project-p)
                     (propertize (concat " [" (projectile-project-name) "] ")
                                 'face (if (my/selected-window-active)
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
                                 'face (if (my/selected-window-active)
                                           '(:inherit error))
                                 'help-echo "Buffer is read-only")))
            (flycheck-mode flycheck-mode-line)
            "  %p  %l:%c  "
            ))
    ;; right
    (quote (
            (vc-mode (:eval (propertize vc-mode
                                        'face (if (my/selected-window-active)
                                                  '(:inherit font-lock-regexp-grouping-backslash :weight bold))
                                        'help-echo "Buffer modified")))
            "  "
            (:eval (propertize mode-name
                               'face (if (my/selected-window-active)
                                         '(:inherit font-lock-function-name-face :slant normal))
                               ))

            my/mode-line-coding-format
            ))
    )
   )
  )
 )

(provide 'b-modeline)
