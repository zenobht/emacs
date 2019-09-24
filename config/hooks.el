(add-hook 'minibuffer-setup-hook #'my/disable-in-minibuffer)
(add-hook 'minibuffer-exit-hook #'my/enable-on-minibuffer-exit)

(add-hook 'text-mode-hook #'my/editor-text-config)
(add-hook 'prog-mode-hook #'my/editor-prog-config)

(add-hook 'after-change-major-mode-hook #'my/configure)
(add-hook 'emacs-lisp-mode-hook (lambda () (flycheck-mode -1)))
(add-hook 'conf-mode-hook (lambda () (run-hooks 'prog-mode-hook)))

(add-hook 'emacs-startup-hook #'my/after-startup)

(add-hook 'rjsx-mode-hook #'my/rjsx-config)
(add-hook 'typscript-mode-hook #'my/typescript-config)
(add-hook 'web-mode-hook #'my/web-config)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)
(add-hook 'emmet-mode-hook (lambda () (setq-default emmet-move-cursor-between-quote t)))

(add-hook 'elfeed-search-update-hook #'my/elfeed-search-add-separators)
(add-hook 'elfeed-show-mode-hook #'my/elfeed-show)

(provide 'hooks)