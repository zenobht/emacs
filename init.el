(setq start-time (current-time))

;; assign high memory to reduce gc during load
(setq gc-cons-threshold (* 120 1000 1000))
(setq-default custom-file (expand-file-name ".custom.el" user-emacs-directory))

(defun load-directory (directory)

  "Load recursively all `.el' files in DIRECTORY."
  (dolist (element (directory-files-and-attributes directory nil nil nil))
    (let* ((path (car element))
           (fullpath (concat directory "/" path))
           (isdir (car (cdr element)))
           (ignore-dir (or (string= path ".") (string= path ".."))))
      (cond
       ((and (eq isdir t) (not ignore-dir))
        (load-directory fullpath))
       ((and (eq isdir nil) (string= (substring path -3) ".el"))
        (load (file-name-sans-extension fullpath)))))))

(add-hook 'after-init-hook (lambda ()
    (load-directory "~/.emacs.d/lib")
    (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
    (load-file (expand-file-name "functions.el" user-emacs-directory))
    (load-file (expand-file-name "packages.el" user-emacs-directory))
    (load-file (expand-file-name "setup.el" user-emacs-directory))
    (load-theme 'nord t)
    (when (file-exists-p custom-file)
      (load custom-file))
  ))

(defun my/start-server ()
  (require 'server)
  (unless (server-running-p)
    (server-start))
  )

(defun my/after-startup ()
  (message "Emacs ready in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract (current-time) start-time)))
           gcs-done)

  (my/start-server)

  ;; set proper gc values after load
  (setq gc-cons-threshold (* 20 1000 1000))
  )

(add-hook 'emacs-startup-hook #'my/after-startup)

(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

