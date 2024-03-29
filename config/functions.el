(require 'cl)

;;;###autoload
(defun my/setup-indent (n)
  (setq c-basic-offset n)
  (setq tab-width n)
  (setq coffee-tab-width n)
  (setq javascript-indent-level n)
  (setq js-indent-level n)
  (setq js2-basic-offset n)
  (setq web-mode-markup-indent-offset n)
  (setq web-mode-css-indent-offset n)
  (setq web-mode-code-indent-offset n)
  (setq css-indent-offset n)
  )

;;;###autoload
(defun my/configure ()
  (my/setup-indent 2)
  )

;;;###autoload
(defun my/clipboard-yank ()
  "Copies selection to x-clipboard."
  (interactive)
  (if (region-active-p)
      (progn
        (shell-command-on-region (region-beginning) (region-end) "pbcopy")
        (call-interactively 'evil-yank)
        (message nil)
        )
    ))

;;;###autoload
(defun my/copy-to-clipboard ()
  "Copies selection to x-clipboard."
  (interactive)
  (if (display-graphic-p)
      (progn
        (message "Yanked region to x-clipboard!")
        (call-interactively 'clipboard-kill-ring-save)
        )
    (if (region-active-p)
        (progn
          (shell-command-on-region (region-beginning) (region-end) "pbcopy")
          (message "Yanked region to clipboard!")
          (deactivate-mark))
      (message "No region active; can't yank to clipboard!")))
  )

;;;###autoload
(defun my/paste-from-clipboard ()
  "Pastes from x-clipboard."
  (interactive)
  (if (display-graphic-p)
      (progn
        (clipboard-yank)
        (message "graphics active")
        )
    (insert (shell-command-to-string "pbpaste"))
    )
  )

;;;###autoload
(defun my/new-empty-buffer ()
  "Create a new empty buffer.
New buffer will be named “untitled” or “untitled<2>”, “untitled<3>”, etc.

It returns the buffer (for elisp programing).

URL `http://ergoemacs.org/emacs/emacs_new_empty_buffer.html'
Version 2017-11-01"
  (interactive)
  (let (($buf (generate-new-buffer "untitled")))
    (switch-to-buffer $buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)
    $buf
    ))

;;;###autoload
(defun my/rg-star-search ()
  (interactive)
  (if (evil-visual-state-p)
      (let ((selection (regexp-quote (substring-no-properties (buffer-substring (region-beginning) (region-end))))))
        (counsel-rg selection)
        )
    (let ((current-word (thing-at-point 'word 'no-properties)))
      (counsel-rg current-word)
      )
    )
  )

;;;###autoload
(defun company-mode/backend-with-yas (backend)
  (if (and (listp backend) (member 'company-yasnippet backend))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

;;;###autoload
(defun my/magit-kill-buffers ()
  "Restore window configuration and kill all Magit buffers."
  (interactive)
  (let ((buffers (magit-mode-get-buffers)))
    (magit-restore-window-configuration)
    (mapc #'kill-buffer buffers)))

;;;###autoload
(defun my/neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (require 'projectile)
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name)))
      (message "Could not find git project root."))))

;;;###autoload
(defun kill-other-buffers ()
  "Kill all buffers but the current one.
Don't mess with special buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (or (eql buffer (current-buffer)) (not (buffer-file-name buffer)))
      (kill-buffer buffer))))

;;;###autoload
(defun simple-mode-line-render (left right)
  "Return a string of `window-width' length containing LEFT, and RIGHT aligned respectively."
  (let* ((available-width (- (window-total-width) (+ (length (format-mode-line left)) (length (format-mode-line right))))))
    (append left (list (format (format "%%%ds" available-width) "")) right)
    )
  )

;;;###autoload
(defun set-selected-window ()
  (when (not (minibuffer-window-active-p (frame-selected-window)))
    (setq ml-selected-window (frame-selected-window))))

;;;###autoload
(defun unset-selected-window ()
  (setq ml-selected-window nil))

;;;###autoload
(defun my/org-move-up ()
  (interactive)
  (if (string-prefix-p "*" (thing-at-point 'line))
      (org-move-subtree-up)
    (org-move-item-up))
  )

;;;###autoload
(defun my/org-config ()
  (require 'b-org)
  )

;;;###autoload
(defun my/org-move-down ()
  (interactive)
  ;; in org-mode move subtree if its heading else move item
  (if (string-prefix-p "*" (thing-at-point 'line))
      (org-move-subtree-down)
    (org-move-item-down))
  )

;;;###autoload
(defun my/calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source "Green")  ; orgmode source
    ))
  )

;;;###autoload
(defun package-upgrade-all ()
  "Upgrade all packages automatically without showing *Packages* buffer."
  (interactive)
  (package-refresh-contents)
  (let (upgrades)
    (cl-flet ((get-version (name where)
                           (let ((pkg (cadr (assq name where))))
                             (when pkg
                               (package-desc-version pkg)))))
      (dolist (package (mapcar #'car package-alist))
        (let ((in-archive (get-version package package-archive-contents)))
          (when (and in-archive
                     (version-list-< (get-version package package-alist)
                                     in-archive))
            (push (cadr (assq package package-archive-contents))
                  upgrades)))))
    (if upgrades
        (when (yes-or-no-p
               (message "Upgrade %d package%s (%s)? "
                        (length upgrades)
                        (if (= (length upgrades) 1) "" "s")
                        (mapconcat #'package-desc-full-name upgrades ", ")))
          (save-window-excursion
            (dolist (package-desc upgrades)
              (let ((old-package (cadr (assq (package-desc-name package-desc)
                                             package-alist))))
                (package-install package-desc)
                (package-delete  old-package)))))
      (load-file user-init-file)
      (message "All packages are up to date"))))

;;;###autoload
(defun my/mousewheel-scroll-up (event)
  "Scroll window under mouse up by five lines."
  (interactive "e")
  (let ((current-window (selected-window)))
    (unwind-protect
        (progn
          (select-window (posn-window (event-start event)))
          (scroll-up 5))
      (select-window current-window))))

;;;###autoload
(defun my/mousewheel-scroll-down (event)
  "Scroll window under mouse down by five lines."
  (interactive "e")
  (let ((current-window (selected-window)))
    (unwind-protect
        (progn
          (select-window (posn-window (event-start event)))
          (scroll-down 5))
      (select-window current-window))))

;;;###autoload
(defun my/elfeed-mark-all-read ()
  (interactive)
  (elfeed-untag elfeed-search-entries 'unread)
  (elfeed-search-update :force)) ; redraw

;;;###autoload
(defun my/elfeed-star ()
  "Apply starred to all selected entries."
  (interactive )
  (let* ((entries (elfeed-search-selected))
         (tag 'starred))

    (cl-loop for entry in entries do (elfeed-tag entry tag))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))

;; remove a start
;;;###autoload
(defun my/elfeed-unstar ()
  "Remove starred tag from all selected entries."
  (interactive )
  (let* ((entries (elfeed-search-selected))
         (tag 'starred))

    (cl-loop for entry in entries do (elfeed-untag entry tag))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))

;;;###autoload
(defun my/show-elfeed (buffer)
  (with-current-buffer buffer
    (setq buffer-read-only nil)
    (setq fill-column 110)
    (goto-char (point-min))
    (re-search-forward "\n\n")
    (fill-individual-paragraphs (point) (point-max))
    (setq buffer-read-only t))
  ;; (setq fill-column 80)
  (switch-to-buffer buffer))

;;;###autoload
(cl-defun my/elfeed-search-add-separators (&key (min-group-size 2))
  "Insert overlay spacers where the current date changes.
If no group has at least MIN-GROUP-SIZE items, no spacers will be
inserted. "
  ;; TODO: Use column-specific functions so that, e.g. date column could be grouped by month/year
  (cl-labels ((count-date-items (date)
                                (cl-loop for entry in elfeed-search-entries
                                         when (equal date (elfeed-search-format-date (elfeed-entry-date entry)))
                                         count it))
              (insert-date (date &key count)
                           (ov (line-beginning-position) (line-beginning-position)
                               'before-string (propertize (format "\n%s (%s)\n" date count)
                                                          'face 'elfeed-search-date-face)
                               'type 'date-separator))
              (entry-date (offset)
                          (when-let ((entry (nth offset elfeed-search-entries)))
                            (elfeed-search-format-date (elfeed-entry-date entry)))))
    (ov-clear)
    (save-excursion
      (goto-char (point-min))
      (cl-loop with largest-group-size = 1
               with offset = (- 1 elfeed-search--offset) ; 1 is first line
               with prev-data = (entry-date offset)

               initially do (insert-date prev-data
                                         :count (count-date-items prev-data))

               while (not (eobp))
               do (progn
                    (forward-line 1)
                    (incf offset))

               for current-data = (entry-date offset)
               if (not (equal current-data prev-data))
               do (progn
                    (insert-date current-data
                                 :count (count-date-items current-data))
                    (setq prev-data current-data))
               else do (incf largest-group-size)

               finally do (when (< largest-group-size min-group-size)
                            (ov-clear))))))

;;;###autoload
(defun my/next-error (&optional n reset)
  (interactive "P")
  (next-line)
  (if flycheck-mode
      (flycheck-next-error n reset)
    (next-error n reset))
  )

;;;###autoload
(defun my/previous-error (&optional n reset)
  (interactive "P")
  (previous-line)
  (if flycheck-mode
      (flycheck-previous-error n)
    (previous-error n))
  )

;; delete single quote where required
;;;###autoload
(defun my/trim-next-char ()
  (interactive)
  (delete-char 1)
  )

;;;###autoload
(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

;;;###autoload
(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir)))))

;;;###autoload
(defun my/selected-window-active ()
  (eq ml-selected-window (selected-window))
  )

;;;###autoload
(defun my/shorten-vc-mode-line (string)
  (cond
   ((string-prefix-p "Git" string)
    (concat "\ue0a0 " (substring string 4)))
   (t
    string)))

;;;###autoload
(defun my/set-selected-window (windows)
  (when (not (minibuffer-window-active-p (frame-selected-window)))
    (setq ml-selected-window (selected-window))))

;;;###autoload
(defun my/visual-shift-left ()
  (interactive)
  ;; (evil-shift-left (region-beginning) (region-end))
  (call-interactively 'evil-shift-left)
  (evil-normal-state)
  (evil-visual-restore))

;;;###autoload
(defun my/visual-shift-right ()
  (interactive)
  ;; (evil-shift-right (region-beginning) (region-end))
  (call-interactively 'evil-shift-right)
  (evil-normal-state)
  (evil-visual-restore))

;;;###autoload
(defun my/substitute()
  (interactive)
  (if (eq evil-state 'visual)
      (evil-ex "`<,`>s!!!g")
    (evil-ex "%s!!!g")
    )
  )

;;;###autoload
(defun my/visual-macro ()
  (interactive)
  (evil-ex "`<,`>norm @")
  )

;;;###autoload
(defun my/tabbar-buffer-groups ()
  (list
    (cond
    ((string-match "elfeed" (buffer-name))
      "Elfeed"
      )
    ((string-match "magit" (buffer-name))
      "Magit"
      )
    ((string-equal "*" (substring (buffer-name) 0 1))
      "Emacs Buffer"
      )
    ((eq major-mode 'dired-mode)
      "Dired"
      )
    (t
      "User Buffer"
      )
    )))

;;;###autoload
(defun update-all-autoloads ()
  (interactive)
  (cd emacs-d)
  (let ((generated-autoload-file
         (expand-file-name "loaddefs.el")))
    (when (not (file-exists-p generated-autoload-file))
      (with-current-buffer (find-file-noselect generated-autoload-file)
        (insert ";;") ;; create the file with non-zero size to appease autoload
        (save-buffer)))
    (mapcar #'update-directory-autoloads
            '("" "modes"))
    (with-current-buffer (find-file-noselect generated-autoload-file)
      (while (search-forward ";; no-byte-compile: t\n" nil t)
        (replace-match "")
        (save-buffer))
      )
    )
  )

;;;###autoload
(defun my/after-startup ()
  (message (concat (format-time-string "%Y-%m-%dT%H:%M:%S") " in emacs-startup-hook"))
  (message "Emacs ready in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract (current-time) start-time)))
           gcs-done)

  (my/load-files)

  ;; set proper gc values after load
  (setq gc-cons-threshold 16777216
        gc-cons-percentage 0.1
        )

  (unless (server-running-p) (server-start))
  )

;;;###autoload
(defun my/rjsx-config ()
  (require 'b-js)
  (setq js2-mode-show-strict-warnings nil
        js2-mode-show-parse-errors nil)
  (add-node-modules-path)
  (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
  (prettier-js-mode)
  )

;;;###autoload
(defun my/typescript-config ()
  (require 'b-js)
  (setq typescript-indent-level 2)
  (add-node-modules-path)
  (prettier-js-mode)
  (tide-setup)
  (sp-local-pair 'typescript-mode "\<" "\>")
;; (typescript-mode . tide-hl-identifier-mode)
;; (before-save . tide-format-before-save)
  )

;;;###autoload
(defun my/web-config ()
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-block-padding 2
        web-mode-comment-style 2

        web-mode-enable-css-colorization t
        web-mode-enable-auto-pairing t
        web-mode-enable-comment-keywords t
        web-mode-enable-current-element-highlight t
        )
  )

;;;###autoload
(defun my/elfeed-show ()
  (let ((inhibit-read-only t)
        (inhibit-modification-hooks t))
    (setq truncate-lines nil
          shr-width 85)
    (set-buffer-modified-p nil))
  (setq left-margin-width 15
        right-margin-width 15)
  )

;;;###autoload
(defun my/emacs-lisp-config ()
  (flycheck-mode -1)
  (sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
  (my/remove-elc-on-save)
  )

;;;###autoload
(defun my/python-config ()
  (add-to-list 'exec-path "/usr/local/anaconda3/bin")
  (setq python-shell-interpreter "ipython"
        python-indent-offset 4
        python-shell-interpreter-args "-i"
        flycheck-python-flake8-executable "flake8"
        )
  (py-autopep8-enable-on-save)
  (anaconda-mode)
  (anaconda-eldoc-mode)
  (require 'b-eglot)
  )

;;;###autoload
(defun my/json-format ()
  (interactive)
  (json-mode)
  (json-pretty-print-buffer)
  )

;;;###autoload
(defun my/byte-compile-init-dir ()
  "Byte-compile all your dotfiles."
  (interactive)
  (byte-recompile-directory user-emacs-directory 0)
  )

;;;###autoload
(defun my/clear-old-byte-compile-file ()
  (if (file-exists-p (concat buffer-file-name "c"))
      (delete-file (concat buffer-file-name "c")))
  (byte-compile-file (buffer-file-name))
  )

;;;###autoload
(defun my/remove-elc-on-save ()
  "If you're saving an Emacs Lisp file, likely the .elc is no longer valid."
  (add-hook 'after-save-hook #'my/clear-old-byte-compile-file nil t)
  )

;;;###autoload
(defun my/generate-uuid ()
  (interactive)
  (insert (shell-command-to-string "uuidgen"))
  )

;;;###autoload
(defun my/kotlin-config ()
  (setq-default kotlin-tab-width 4)
  (flycheck-kotlin-setup)
  (require 'b-eglot)
  )

;;;###autoload
(defun my/magit-automated-commit ()
  (interactive)
  (magit-with-toplevel
    (magit-stage-modified 'all)
    (magit-run-git "commit" "-m" (concat "\"" (string-trim (shell-command-to-string "uuidgen")) "\""))
    (magit-push-current-to-upstream)
    )
  )

;;;###autoload
(defun my/newline-and-indent ()
  "inserts a newline between the brackets"
  (interactive)
  (newline)
  (save-excursion
    (newline)
    (indent-for-tab-command))
  (indent-for-tab-command)
  )

(defvar smartparens-mode-original-value)

;;;###autoload
(defun disable-sp-hippie-advice (&rest _)
  (setq smartparens-mode-original-value smartparens-mode)
  (setq smartparens-mode nil)
  t) ; We should still return t.
;; This advice could be added to other functions that usually insert
;; balanced parens, like `try-expand-list'.

;;;###autoload
(defun reenable-sp-hippie-advice (&rest _)
  (when (boundp 'smartparens-mode-original-value)
    (setq smartparens-mode smartparens-mode-original-value)
    (makunbound 'smartparens-mode-original-value)))

;;;###autoload
(defun my/css-config ()
  "inserts a newline between the brackets"
  (emmet-mode)
  (add-node-modules-path)
  (prettier-js-mode)
  )

;;;###autoload
(defun my/smart-open-line-above ()
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode)
  )

;;;###autoload
(defun my/smart-open-line-below ()
  (interactive)
  (move-beginning-of-line nil)
  (forward-line 1)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode)
  )

;;;###autoload
(defun my/load-files ()
  (require 'b-evil)
  (require 'b-ivy)
  (require 'b-projectile)
  (require 'b-tabbar)
  (require 'nord-theme)
  (require 'b-modeline)
  (require 'b-essentials)
  (require 'b-files)
  (require 'b-editor)
  (require 'keybindings)
)

;;;###autoload
(defun my/whitespace ()
  (setq whitespace-style '(face trailing spaces tabs newline tab-mark newline-mark)
        show-trailing-whitespace t
        whitespace-display-mappings
        ;; all numbers are unicode codepoint in decimal. e.g. (insert-char 182 1)
        '(
          (space-mark 32 [183] [46]) ; SPACE 32 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
          (newline-mark 10 [172 10]) ; LINE FEED,
          (tab-mark 9 [9655 9] [92 9]) ; tab
          )
        )
  )

;;;###autoload
(defun my/editor-text-config ()
  (my/whitespace)
  (whitespace-mode +1)
  (display-line-numbers-mode +1)
  (visual-line-mode -1)
  (smartparens-mode +1)
  )

;;;###autoload
(defun my/editor-prog-config ()
  (my/whitespace)
  (whitespace-mode +1)
  (display-line-numbers-mode +1)
  (rainbow-mode +1)
  (highlight-thing-mode +1)
  (flycheck-mode +1)
  (smartparens-mode +1)
  )
