;;; add-node-modules-path.el --- Add node_modules to your exec-path

;; Copyright (C) 2016 Neri Marschik
;; This package uses the MIT License.
;; See the LICENSE file.

;; Author: Neri Marschik <marschik_neri@cyberagent.co.jp>
;; Version: 1.0
;; Package-Requires: ()
;; Keywords: javascript, node, node_modules, eslint
;; URL: https://github.com/codesuki/add-node-modules-path

;;; Commentary:
;;
;; This file provides `setup-project-paths', which searches
;; the current files parent directories for the `node_modules/.bin/' directory
;; and adds it to the buffer local `exec-path'.
;; This allows Emacs to find project based installs of e.g. eslint.
;;
;; Usage:
;;     M-x setup-project-paths
;;
;;     To automatically run it when opening a new buffer:
;;     (Choose depending on your favorite mode.)
;;
;;     (eval-after-load 'js-mode
;;       '(add-hook 'js-mode-hook #'setup-project-paths))
;;
;;     (eval-after-load 'js2-mode
;;       '(add-hook 'js2-mode-hook #'setup-project-paths))

;;; Code:

;;;###autoload
(defvar setup-project-paths-debug nil
  "Enable verbose output when non nil.")

;;;###autoload
(defun setup-project-paths ()
  "Search the current buffer's parent directories for `node_modules/.bin`.
If it's found, then add it to the `exec-path'."
  ;; creates a base-init-path variable which is used to set the
  ;; root path for each project clearing previous path
  (if (not (boundp 'base-path))
      (setq base-path exec-path)
      ;;(add-to-list 'base-path "~/")
    )
  ;; set exec-path for 
  (setq exec-path base-path)
  ;;(print base-path)
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (path (and root
                    (expand-file-name "node_modules/.bin/" root))))
    (if root
        (progn
          ;; find out why path is added twice
          (add-to-list 'exec-path path)
          (add-to-list 'exec-path root)
          (delete-dups exec-path) ;; temp fix
           ;;(print exec-path)
          (when setup-project-paths-debug
            (message (concat "added " path  " to exec-path"))))
      (when setup-project-paths-debug
        (message (concat "node_modules not found in " root))))))

(provide 'setup-project-paths)

;;; setup-project-paths.el ends here
