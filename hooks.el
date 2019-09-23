(add-hook 'minibuffer-setup-hook #'my/disable-in-minibuffer)
(add-hook 'minibuffer-exit-hook #'my/enable-on-minibuffer-exit)

(add-hook 'text-mode-hook
          (lambda ()
            (interactive)
            (visual-line-mode -1)
            ))

(add-hook 'after-change-major-mode-hook #'my/configure)
(add-hook 'emacs-lisp-mode-hook (lambda () (flycheck-mode -1)))
(add-hook 'conf-mode-hook (lambda () (run-hooks 'prog-mode-hook)))
