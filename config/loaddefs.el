;;; loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "functions" "functions.el" (0 0 0 0))
;;; Generated autoloads from functions.el

(autoload 'my/last-used-buffer "functions" "\


\(fn)" t nil)

(autoload 'my/setup-indent "functions" "\


\(fn N)" nil nil)

(autoload 'my/configure "functions" "\


\(fn)" nil nil)

(autoload 'my/copy-to-clipboard "functions" "\
Copies selection to x-clipboard.

\(fn)" t nil)

(autoload 'my/paste-from-clipboard "functions" "\
Pastes from x-clipboard.

\(fn)" t nil)

(autoload 'my/new-empty-buffer "functions" "\
Create a new empty buffer.
New buffer will be named “untitled” or “untitled<2>”, “untitled<3>”, etc.

It returns the buffer (for elisp programing).

URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
Version 2017-11-01

\(fn)" t nil)

(autoload 'my/rg-star-search "functions" "\


\(fn)" t nil)

(autoload 'company-mode/backend-with-yas "functions" "\


\(fn BACKEND)" nil nil)

(autoload 'my/magit-kill-buffers "functions" "\
Restore window configuration and kill all Magit buffers.

\(fn)" t nil)

(autoload 'my/neotree-project-dir "functions" "\
Open NeoTree using the git root.

\(fn)" t nil)

(autoload 'kill-other-buffers "functions" "\
Kill all buffers but the current one.
Don't mess with special buffers.

\(fn)" t nil)

(autoload 'my/disable-in-minibuffer "functions" "\


\(fn)" nil nil)

(autoload 'my/enable-on-minibuffer-exit "functions" "\


\(fn)" nil nil)

(autoload 'simple-mode-line-render "functions" "\
Return a string of `window-width' length containing LEFT, and RIGHT aligned respectively.

\(fn LEFT RIGHT)" nil nil)

(autoload 'set-selected-window "functions" "\


\(fn)" nil nil)

(autoload 'unset-selected-window "functions" "\


\(fn)" nil nil)

(autoload 'my/org-move-up "functions" "\


\(fn)" t nil)

(autoload 'my/org-move-down "functions" "\


\(fn)" t nil)

(autoload 'my/calendar "functions" "\


\(fn)" t nil)

(autoload 'package-upgrade-all "functions" "\
Upgrade all packages automatically without showing *Packages* buffer.

\(fn)" t nil)

(autoload 'my/mousewheel-scroll-up "functions" "\
Scroll window under mouse up by five lines.

\(fn EVENT)" t nil)

(autoload 'my/mousewheel-scroll-down "functions" "\
Scroll window under mouse down by five lines.

\(fn EVENT)" t nil)

(autoload 'my/elfeed-mark-all-read "functions" "\


\(fn)" t nil)

(autoload 'my/elfeed-star "functions" "\
Apply starred to all selected entries.

\(fn)" t nil)

(autoload 'my/elfeed-unstar "functions" "\
Remove starred tag from all selected entries.

\(fn)" t nil)

(autoload 'my/show-elfeed "functions" "\


\(fn BUFFER)" nil nil)

(autoload 'my/next-error "functions" "\


\(fn &optional N RESET)" t nil)

(autoload 'my/previous-error "functions" "\


\(fn &optional N RESET)" t nil)

(autoload 'my/trimn-next-char "functions" "\


\(fn)" t nil)

(autoload 'my/selected-window-active "functions" "\


\(fn)" nil nil)

(autoload 'my/shorten-vc-mode-line "functions" "\


\(fn STRING)" nil nil)

(autoload 'my/set-selected-window "functions" "\


\(fn WINDOWS)" nil nil)

(autoload 'my/visual-shift-left "functions" "\


\(fn)" t nil)

(autoload 'my/visual-shift-right "functions" "\


\(fn)" t nil)

(autoload 'my/substitute "functions" "\


\(fn)" t nil)

(autoload 'my/visual-macro "functions" "\


\(fn)" t nil)

(autoload 'my/tabbar-buffer-groups "functions" "\


\(fn)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "functions" '("measure-time")))

;;;***

;;;### (autoloads nil "keybindings" "keybindings.el" (0 0 0 0))
;;; Generated autoloads from keybindings.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "keybindings" '("hydra-org")))

;;;***

;;;### (autoloads nil "modes/prettier-js" "modes/prettier-js.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from modes/prettier-js.el

(autoload 'prettier-js-mode "modes/prettier-js" "\
Runs prettier on file save when this mode is turned on

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "modes/prettier-js" '("prettier-")))

;;;***

;;;### (autoloads nil "nord-theme" "nord-theme.el" (0 0 0 0))
;;; Generated autoloads from nord-theme.el

(when (and (boundp 'custom-theme-load-path) load-file-name) (add-to-list 'custom-theme-load-path (file-name-as-directory (file-name-directory load-file-name))))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "nord-theme" '("nord")))

;;;***

;;;### (autoloads nil "packages" "packages.el" (0 0 0 0))
;;; Generated autoloads from packages.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "packages" '("my/packages")))

;;;***

;;;### (autoloads nil nil ("hooks.el") (0 0 0 0))

;;;***

(provide 'loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; loaddefs.el ends here
