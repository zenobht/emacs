(setq start-time (current-time))

;; assign high memory to reduce gc during load
(setq gc-cons-threshold (* 50 1000 1000))

(add-hook 'after-init-hook (lambda ()
    (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
    (load-file (expand-file-name "config.el" user-emacs-directory))
    (load-theme 'nord t)
  ))

(defun my/after-startup ()
  (message "Emacs ready in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract (current-time) start-time)))
           gcs-done)

  ;; set proper gc values after load
  (setq gc-cons-threshold (* 2 1000 1000))
  )

(add-hook 'emacs-startup-hook #'my/after-startup)

(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

(setq-default custom-file (expand-file-name ".custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
