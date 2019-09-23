(defun my/last-used-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer))
  )

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

(defun my/configure ()
  (my/setup-indent 2)
  )

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

(defun company-mode/backend-with-yas (backend)
  (if (and (listp backend) (member 'company-yasnippet backend))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(defun my/magit-kill-buffers ()
  "Restore window configuration and kill all Magit buffers."
  (interactive)
  (let ((buffers (magit-mode-get-buffers)))
    (magit-restore-window-configuration)
    (mapc #'kill-buffer buffers)))

(defun my/neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name)))
      (message "Could not find git project root."))))

(defun kill-other-buffers ()
  "Kill all buffers but the current one.
Don't mess with special buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (or (eql buffer (current-buffer)) (not (buffer-file-name buffer)))
      (kill-buffer buffer))))

(defun my/disable-in-minibuffer ()
  (electric-pair-mode -1))

(defun my/enable-on-minibuffer-exit ()
  (electric-pair-mode +1))

(defun simple-mode-line-render (left right)
  "Return a string of `window-width' length containing LEFT, and RIGHT aligned respectively."
  (let* ((available-width (- (window-total-width) (+ (length (format-mode-line left)) (length (format-mode-line right))))))
    (append left (list (format (format "%%%ds" available-width) "")) right)
    )
  )

(defun set-selected-window ()
  (when (not (minibuffer-window-active-p (frame-selected-window)))
    (setq ml-selected-window (frame-selected-window))))

(defun unset-selected-window ()
  (setq ml-selected-window nil))

(defun my/org-move-up ()
  (interactive)
  (if (string-prefix-p "*" (thing-at-point 'line))
      (org-move-subtree-up)
    (org-move-item-up))
  )

(defun my/org-move-down ()
  (interactive)
  ;; in org-mode move subtree if its heading else move item
  (if (string-prefix-p "*" (thing-at-point 'line))
      (org-move-subtree-down)
    (org-move-item-down))
  )

(defun my/calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source "Green")  ; orgmode source
    ))
  )

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

(defun my/mousewheel-scroll-up (event)
  "Scroll window under mouse up by five lines."
  (interactive "e")
  (let ((current-window (selected-window)))
    (unwind-protect
        (progn
          (select-window (posn-window (event-start event)))
          (scroll-up 5))
      (select-window current-window))))

(defun my/mousewheel-scroll-down (event)
  "Scroll window under mouse down by five lines."
  (interactive "e")
  (let ((current-window (selected-window)))
    (unwind-protect
        (progn
          (select-window (posn-window (event-start event)))
          (scroll-down 5))
      (select-window current-window))))

(defun my/elfeed-mark-all-read ()
  (interactive)
  (elfeed-untag elfeed-search-entries 'unread)
  (elfeed-search-update :force)) ; redraw

(defun my/elfeed-star ()
  "Apply starred to all selected entries."
  (interactive )
  (let* ((entries (elfeed-search-selected))
         (tag 'starred))

    (cl-loop for entry in entries do (elfeed-tag entry tag))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))

;; remove a start
(defun my/elfeed-unstar ()
  "Remove starred tag from all selected entries."
  (interactive )
  (let* ((entries (elfeed-search-selected))
         (tag 'starred))

    (cl-loop for entry in entries do (elfeed-untag entry tag))
    (mapc #'elfeed-search-update-entry entries)
    (unless (use-region-p) (forward-line))))

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

(defun my/next-error (&optional n reset)
  (interactive "P")
  (next-line)
  (if flycheck-mode
      (flycheck-next-error n reset)
    (next-error n reset))
  )

(defun my/previous-error (&optional n reset)
  (interactive "P")
  (previous-line)
  (if flycheck-mode
      (flycheck-previous-error n)
    (previous-error n))
  )

;; delete single quote where required
(defun my/trimn-next-char ()
  (interactive)
  (delete-char 1)
  )

(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir)))))

(defun my/selected-window-active ()
  (eq ml-selected-window (selected-window))
  )

(defun my/shorten-vc-mode-line (string)
  (cond
   ((string-prefix-p "Git" string)
    (concat "\ue0a0 " (magit-get-current-branch)))
   (t
    string)))

(defun my/set-selected-window (windows)
  (when (not (minibuffer-window-active-p (frame-selected-window)))
    (setq ml-selected-window (selected-window))))

(defun my/visual-shift-left ()
  (interactive)
  ;; (evil-shift-left (region-beginning) (region-end))
  (call-interactively 'evil-shift-left)
  (evil-normal-state)
  (evil-visual-restore))

(defun my/visual-shift-right ()
  (interactive)
  ;; (evil-shift-right (region-beginning) (region-end))
  (call-interactively 'evil-shift-right)
  (evil-normal-state)
  (evil-visual-restore))

(defun my/substitute()
  (interactive)
  (if (eq evil-state 'visual)
      (evil-ex "`<,`>s!!!g")
    (evil-ex "%s!!!g")
    )
  )

(defun my/visual-macro ()
  (interactive)
  (evil-ex "`<,`>norm @")
  )

(defun my/tabbar-buffer-groups ()
  (list
    (cond
    ((string-match "elfeed" (buffer-name))
      "Elfeed"
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
