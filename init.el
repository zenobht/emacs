(setq gc-cons-threshold (* 50 1000 1000))

(add-hook 'after-init-hook (lambda ()
    (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
    (load-file (expand-file-name "config.el" user-emacs-directory))
    (load-theme 'nord t)
    (setq gc-cons-threshold (* 2 1000 1000))
  ))

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

