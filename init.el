(setq start-time (current-time))
(setq mode-line-format nil)

;; assign high memory to reduce gc during load
;; lifted from doom-emacs. Thanks
(defvar doom--file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      file-name-handler-alist nil
      )

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
    (load-theme 'nord t)
    (load-file (expand-file-name "setup.el" user-emacs-directory))
    (load-file (expand-file-name "keybindings.el" user-emacs-directory))
    (when (file-exists-p custom-file)
      (load custom-file))
  ))

(defun my/after-startup ()
  (message (concat (format-time-string "%Y-%m-%dT%H:%M:%S") " in emacs-startup-hook"))
  (message "Emacs ready in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract (current-time) start-time)))
           gcs-done)

  ;; set proper gc values after load
  (setq gc-cons-threshold 16777216
        gc-cons-percentage 0.1
        file-name-handler-alist doom--file-name-handler-alist)
 )

(add-hook 'emacs-startup-hook #'my/after-startup)
