(setq gc-cons-threshold (* 50 1000 1000))

(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

(load-file (expand-file-name "config.el" user-emacs-directory))

(setq gc-cons-threshold (* 2 1000 1000))
