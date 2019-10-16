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

(autoload 'my/elfeed-search-add-separators "functions" "\
Insert overlay spacers where the current date changes.
If no group has at least MIN-GROUP-SIZE items, no spacers will be
inserted. 

\(fn &key (MIN-GROUP-SIZE 2))" nil nil)

(autoload 'my/next-error "functions" "\


\(fn &optional N RESET)" t nil)

(autoload 'my/previous-error "functions" "\


\(fn &optional N RESET)" t nil)

(autoload 'my/trimn-next-char "functions" "\


\(fn)" t nil)

(autoload 'measure-time "functions" "\
Measure the time it takes to evaluate BODY.

\(fn &rest BODY)" nil t)

(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate) "\
Create parent directory if not exists while visiting file." (unless (file-exists-p filename) (let ((dir (file-name-directory filename))) (unless (file-exists-p dir) (make-directory dir)))))

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

(autoload 'update-all-autoloads "functions" "\


\(fn)" t nil)

(autoload 'my/after-startup "functions" "\


\(fn)" nil nil)

(autoload 'my/rjsx-config "functions" "\


\(fn)" nil nil)

(autoload 'my/typescript-config "functions" "\


\(fn)" nil nil)

(autoload 'my/web-config "functions" "\


\(fn)" nil nil)

(autoload 'my/elfeed-show "functions" "\


\(fn)" nil nil)

(autoload 'my/emacs-lisp-config "functions" "\


\(fn)" nil nil)

(autoload 'my/python-config "functions" "\


\(fn)" nil nil)

(autoload 'my/json-format "functions" "\


\(fn)" t nil)

(autoload 'my/byte-compile-init-dir "functions" "\
Byte-compile all your dotfiles.

\(fn)" t nil)

(autoload 'my/clear-old-byte-compile-file "functions" "\


\(fn)" nil nil)

(autoload 'my/remove-elc-on-save "functions" "\
If you're saving an Emacs Lisp file, likely the .elc is no longer valid.

\(fn)" nil nil)

(autoload 'my/generate-uuid "functions" "\


\(fn)" t nil)

(autoload 'my/kotlin-config "functions" "\


\(fn)" nil nil)

(autoload 'my/magit-automated-commit "functions" "\


\(fn)" t nil)

(autoload 'my/newline-and-indent "functions" "\
inserts a newline between the brackets

\(fn)" t nil)

(autoload 'disable-sp-hippie-advice "functions" "\


\(fn &rest _)" nil nil)

(autoload 'reenable-sp-hippie-advice "functions" "\


\(fn &rest _)" nil nil)

;;;***

;;;### (autoloads nil "keybindings" "keybindings.el" (0 0 0 0))
;;; Generated autoloads from keybindings.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "keybindings" '("hydra-org")))

;;;***

;;;### (autoloads nil "modes/b-editor" "modes/b-editor.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from modes/b-editor.el

(autoload 'my/editor-text-config "modes/b-editor" "\


\(fn)" nil nil)

(autoload 'my/editor-prog-config "modes/b-editor" "\


\(fn)" nil nil)

;;;***

;;;### (autoloads nil "modes/b-evil" "modes/b-evil.el" (0 0 0 0))
;;; Generated autoloads from modes/b-evil.el

(autoload 'my/evil-smartparens "modes/b-evil" "\


\(fn)" nil nil)

;;;***

;;;### (autoloads nil "modes/b-modeline" "modes/b-modeline.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from modes/b-modeline.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "modes/b-modeline" '("my/mode-line-coding-format" "ml-selected-window")))

;;;***

;;;### (autoloads nil "modes/highlight-thing" "modes/highlight-thing.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from modes/highlight-thing.el

(autoload 'highlight-thing-mode "modes/highlight-thing" "\
Minor mode that highlights things at point

\(fn &optional ARG)" t nil)

(defvar global-highlight-thing-mode nil "\
Non-nil if Global highlight-thing mode is enabled.
See the `global-highlight-thing-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-highlight-thing-mode'.")

(custom-autoload 'global-highlight-thing-mode "modes/highlight-thing" nil)

(autoload 'global-highlight-thing-mode "modes/highlight-thing" "\
Toggle Highlight-thing mode in all buffers.
With prefix ARG, enable Global highlight-thing mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Highlight-thing mode is enabled in all buffers where
`highlight-thing-mode-maybe-activate' would do it.
See `highlight-thing-mode' for more information on Highlight-thing mode.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "modes/highlight-thing" '("highlight-thing-")))

;;;***

;;;### (autoloads nil "modes/nord-theme" "modes/nord-theme.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from modes/nord-theme.el

(when (and (boundp 'custom-theme-load-path) load-file-name) (add-to-list 'custom-theme-load-path (file-name-as-directory (file-name-directory load-file-name))))

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "modes/nord-theme" '("nord")))

;;;***

;;;### (autoloads nil "modes/prettier-js" "modes/prettier-js.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from modes/prettier-js.el

(autoload 'prettier-js-mode "modes/prettier-js" "\
Runs prettier on file save when this mode is turned on

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "modes/prettier-js" '("prettier-")))

;;;***

;;;### (autoloads nil "packages" "packages.el" (0 0 0 0))
;;; Generated autoloads from packages.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "packages" '("my/packages")))

;;;***

;;;### (autoloads nil nil ("hooks.el" "modes/b-elfeed.el" "modes/b-essentials.el"
;;;;;;  "modes/b-files.el" "modes/b-ivy.el" "modes/b-markdown.el"
;;;;;;  "modes/b-org.el") (0 0 0 0))

;;;***

(provide 'loaddefs)
;; Local Variables:
;; version-control: never
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; loaddefs.el ends here
