(add-hook 'text-mode-hook #'my/editor-text-config)
(add-hook 'prog-mode-hook #'my/editor-prog-config)

(add-hook 'after-change-major-mode-hook #'my/configure)
(add-hook 'emacs-lisp-mode-hook #'my/emacs-lisp-config)
(add-hook 'conf-mode-hook (lambda () (run-hooks 'prog-mode-hook)))

(add-hook 'emacs-startup-hook #'my/after-startup)

(add-hook 'rjsx-mode-hook #'my/rjsx-config)
(add-hook 'typescript-mode-hook #'my/typescript-config)
(add-hook 'web-mode-hook #'my/web-config)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook #'my/css-config)
(add-hook 'rjsx-mode-hook 'emmet-mode)
(add-hook 'emmet-mode-hook (lambda () (setq-default emmet-move-cursor-between-quote t)))

(add-hook 'elfeed-search-update-hook #'my/elfeed-search-add-separators)
(add-hook 'elfeed-show-mode-hook #'my/elfeed-show)

(add-hook 'smartparens-enabled-hook #'my/evil-smartparens)
(add-hook 'python-mode-hook #'my/python-config)
(add-hook 'kotlin-mode-hook #'my/kotlin-config)
(add-hook 'eshell-exit-hook 'delete-window)

(advice-add 'yas-hippie-try-expand :after-while #'disable-sp-hippie-advice)
(advice-add 'hippie-expand :after #'reenable-sp-hippie-advice
            ;; Set negative depth to make sure we go after
            ;; `sp-auto-complete-advice'.
            '((depth . -100)))

(add-hook 'evil-multiedit-state-entry-hook (lambda () (highlight-thing-mode -1)))
(add-hook 'evil-multiedit-state-exit-hook (lambda () (highlight-thing-mode +1)))

(provide 'hooks)
