;;; init.el --- Emacs init file.

;; Author: Ashlin Eldridge
;; Version: 0.0.2
;; Keywords: emacs, init, lisp

;;; Commentary:
;;
;; My bonsai.

;;; Code:

;; Theme-related variable definitions.
(defvar my/dark-theme 'doom-tomorrow-night)
(defvar my/light-theme 'spacemacs-light)
(defvar my/current-theme my/dark-theme)

;; Font-related variable definitions.
(defvar my/default-fixed-font "Jetbrains Mono") ;; Cantarell is another good one.
(defvar my/default-variable-font "ETBembo")
(defvar my/default-fixed-font-size 130)
(defvar my/default-variable-font-size 160)

;; Org-related variable definitions.
(defvar my/org-dir "~/Dropbox/org")

;; Emacs config directory-related variable definitions.
(defvar my/xdg-config-dir "~/.config")
(defvar my/xdg-cache-dir "~/.cache")
(defvar my/xdg-data-dir "~/.local/share")

;; Emacs performance settings. I'm following the general performance recommendations of lsp-mode
;; here https://emacs-lsp.github.io/lsp-mode/page/performance. Some people recommend against
;; modifying gc-cons-threshold but it does seem to speed things up for me.
(setq gc-cons-threshold (* 100 1024 1024))
(setq read-process-output-max (* 1 1024 1024))

;; Keep track of start up time.
(add-hook 'emacs-startup-hook
	  (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
		     (format "%.2f seconds" (float-time (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(defvar straight-repository-branch "develop")

;; Bootstrap straight.el.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Load straight helper package.
(require 'straight-x)

;; Install use-package.
(straight-use-package 'use-package)

;; Install org as early as possible after straight so that the built-in version of org doesn't get activated
;; between here and where org is actually configured below.
(straight-use-package 'org)

;; Make use-package use straight for package installation.
(setq straight-use-package-by-default t)

;; Do not show the startup screen.
(setq inhibit-startup-message t)

(tool-bar-mode -1)   ; Disable the tool bar
(tooltip-mode -1)    ; Disable tool tips
(menu-bar-mode -1)   ; Disable the menu bar
(scroll-bar-mode -1) ; Disable visible scrollbar
(set-fringe-mode 10) ; Increase left/right margins slightly

;; Why would this be true? https://blog.sumtypeofway.com/posts/emacs-config.html
(setq sentence-end-double-space nil)

;; Accept 'y' in lieu of 'yes'.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Make input characters overwrite the current region.
(delete-selection-mode t)

;; If I need to run Emacs in the terminal I'll use `-nw -q`. Better than
;; littering this file with unnecessary window system checks.
(unless window-system
  (error "This Emacs init file is for window systems only"))

;; Enlargen frame size if we're running within a window system.
(when window-system
  (set-frame-size (selected-frame) 110 50))

;; Prefer running Emacs in server mode and have Git and $EDITOR use emacsclient
;; to open files in a single Emacs instance.
(when window-system
  (server-mode))

;; Flash the mode line rather than use an audible bell.
(setq visible-bell nil)
(setq ring-bell-function (lambda ()
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil 'invert-face 'mode-line)))

;; Set the default face.
(set-face-attribute 'default nil :font my/default-fixed-font :height my/default-fixed-font-size)

;; Set the fixed pitch face.
(set-face-attribute 'fixed-pitch nil :font my/default-fixed-font :height my/default-fixed-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font my/default-variable-font :height my/default-variable-font-size :weight 'regular)

;; Show echoed keystrokes quicker.
(setq echo-keystrokes 0.01)

;; Global key bindings.
;; Disable arrow keys to "encourage" standard navigation.
(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<right>"))
(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "C-c e") 'eshell)
(global-set-key (kbd "C-;") 'comment-line)
(global-set-key (kbd "C-c o l") 'org-store-link)
(global-set-key (kbd "C-c o a") 'org-agenda)
(global-set-key (kbd "C-c o c") 'org-capture)
(global-set-key (kbd "C-c o i") (lambda () (interactive) (org-capture nil "i"))) ;; Capture to inbox.
(global-set-key (kbd "C-c m") 'mu4e)

;; Theme cycling.
(global-set-key (kbd "C-c t") 'my/cycle-theme)

;; Show column numbers in the mode line.
(column-number-mode t)

;; Enable line numbers for some modes.
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; Override some modes which derive from the above.
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Use CMD key for META.
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier nil)

;; Store all backup files in XDG_CACHE_HOME.
(setq backup-directory-alist '(("" . "~/.cache/emacs/backup/")))
(setq auto-save-file-name-transforms '((".*" "~/.cache/emacs/saves/" t)))

;; Save interrupted session records in XDG_DATA_HOME.
;(setq auto-save-list-file-prefix (concat emacs-data-home "/auto-save-list"))
(setq auto-save-list-file-prefix "~/.cache/emacs/auto-save-list/")

;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Revert buffers when the underlying file has changed
(global-auto-revert-mode 1)

;; Next, import environment variables from the shell (defined in "${XDG_BASE_CONFIG}/zsh/lib/env.zsh")
;; so that they are available to subsequent expressions.
(use-package exec-path-from-shell
  :init (when (memq window-system '(mac ns x))
	  (setq exec-path-from-shell-variables
		'("PATH" "MANPATH" "XDG_CONFIG_HOME" "XDG_CACHE_HOME" "XDG_DATA_HOME"
		  "GNUPGHOME" "PASSWORD_STORE_DIR" "DEVELOPER_DIR" "SDKROOT"))
	  (setq exec-path-from-shell-arguments nil)
	  (exec-path-from-shell-initialize)))

(use-package ivy
  :init (ivy-mode t) ; globally at startup
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 20)
  (setq ivy-count-format "%d/%d "))

(use-package ivy-rich
  :after 'counsel
  :init (ivy-rich-mode 1))

(use-package counsel
  :bind
  (("M-x"     . counsel-M-x)
   ("C-s"     . swiper)
   ("C-S-s"   . swiper-thing-at-point)
   ("C-x C-f" . counsel-find-file)
   ("C-x b"   . counsel-switch-buffer)
   ("M-y"     . counsel-yank-pop)
   :map minibuffer-local-map ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ; Don't start searches with ^

;; Credential management

(use-package pass)

(use-package auth-source-pass
  :config
  (auth-source-pass-enable))

;; Don't cache secrets.
(setq auth-source-do-cache nil)

;; Am finding prescient a bit confusing
;; (use-package prescient)

;; (use-package ivy-prescient
;;   ;; We can't hook off ivy-mode as it was already enabled above.
;;   :init (ivy-prescient-mode t))

;; (use-package company-prescient
;;   :hook (company-mode . company-prescient-mode))

;; (straight-use-package 'selectrum-prescient)
;; TODO: Investiage selectrum as a replacement for ivy?

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  ;; Remove M-n keybinding as I don't use it and it overrides nerdtree.
  :bind (:map markdown-mode-map ("M-n" . nil))
  :init (setq markdown-command "multimarkdown"))

(use-package rainbow-delimiters
  :init (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

;; Learn how to use first
;; (use-package paredit :hook (emacs-lisp-mode . paredit-mode))

(use-package which-key
  :init (progn
	  (setq which-key-show-early-on-C-h t)
          (setq which-key-idle-delay 2)
          (setq which-key-idle-secondary-delay 0.05)
	  (which-key-mode)))

(use-package yaml-mode
  :mode (("\\.yml\\'" . yaml-mode)
	 ("\\.yaml\\'" . yaml-mode))
  :config (add-hook 'yaml-mode-hook
		    '(lambda () (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package doom-themes)

;; Need :defer t to prevent requiring spacemacs-theme, which doesn't exist.
;; See https://github.com/nashamri/spacemacs-theme/issues/42.
(use-package spacemacs-theme :defer t)

(defun my/org-reset-buffers ()
  "Reset `org-mode` in all org buffers."
  (mapc (lambda (buf)
          (with-current-buffer buf
            (if (string-equal "org-mode" major-mode) (org-mode))))
        (buffer-list)))

(defun my/load-theme (theme)
  "Load the specified theme."
  (interactive)
  (load-theme theme t)
  (my/org-reset-buffers)
  )

(defun my/light-theme ()
  "Switch to light theme."
  (interactive)
  (disable-theme my/dark-theme)
  (my/load-theme my/light-theme))

(defun my/dark-theme ()
  "Switch to dark theme."
  (interactive)
  (disable-theme my/light-theme)
  (my/load-theme my/dark-theme))

(defun my/cycle-theme ()
  "Cycle between dark and light themes."
  (interactive)
  (if (eq my/current-theme my/dark-theme)
      (progn
        (my/light-theme)
        (setq my/current-theme my/light-theme))
    (progn
      (my/dark-theme)
      (setq my/current-theme my/dark-theme))))

(my/load-theme my/current-theme)

;; The package all-the-icons is required by doom-modeline.
(use-package all-the-icons
  :config
  ; Only install the fonts if they are not already installed. See:
  ; https://github.com/domtronn/all-the-icons.el/issues/120#issuecomment-480342779
  (when window-system
    (unless (member "all-the-icons" (font-family-list))
      (all-the-icons-install-fonts t))))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-height 15)
  (doom-modeline-bar-width 4)
  (doom-modeline-lsp t)
  (doom-modeline-github nil)
  (doom-modeline-mu4e nil)
  (doom-modeline-irc nil)
  (doom-modeline-minor-modes t)
  (doom-modeline-persp-name nil)
  (doom-modeline-buffer-file-name-style 'truncate-except-project)
  (doom-modeline-major-mode-icon nil))

(use-package minions
  :hook (doom-modeline-mode . minions-mode))

(use-package projectile
  :delight
  :init
  (projectile-add-known-project "~/.config")
  (setq projectile-project-search-path
	; Search path is displayed from right to left.
	'("~/Development/home" "~/Development/work" "~/Dropbox"))
  (setq projectile-switch-project-action #'projectile-dired)
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :custom ((projectile-completion-system 'ivy)))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit
  :config
  ;; Disable this as it takes over C-n and C-p which I'm used to using for navigation
  (setq magit-bind-magit-project-status nil))

(use-package forge
  :after magit
  :custom
  (forge-database-file (concat my/xdg-cache-dir "/emacs/forge-database.sqlite")))

(use-package ace-window
  :bind (("M-o" . ace-window))
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  ; Remove default key bindings which are crap.
  :init (setq yas-minor-mode-map (make-sparse-keymap))
  :bind (:map yas-minor-mode-map
	      ("C-c y s" . yas-insert-snippet)
	      ("C-c y v" . yas-visit-snippet-file)
  	      ("C-c y n" . yas-new-snippet))
  :config
  (setq
   yas-verbosity 1
   yas-wrap-around-region t)
  (yas-reload-all))

(use-package yasnippet-snippets
  :after yasnippet)

(defun my/org-mode-init ()
  "Personal `org-mode` configuration.

This function is intended to be attached to `org-mode-hook`.  While some of
these settings typically only need to be configured once when Emacs starts
\(e.g., font settings\), they are configured each time `org-mode` starts so
that they can be easily refreshed after global changes, such as switching the
color theme."

  (interactive)

  ;;; Indentation and page structure configuration.

  (org-indent-mode)
  (visual-line-mode)
  (setq left-margin-width 2)
  (setq right-margin-width 2)
  (variable-pitch-mode 1)

  ;;; Font configuration.

  ;; Replace list hyphen with dot.
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Vary heading sizes.
  (dolist (face '((org-level-1 . 1.3)
                  (org-level-2 . 1.25)
                  (org-level-3 . 1.2)
                  (org-level-4 . 1.15)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil
			:font my/default-variable-font
			:weight 'regular
			:height (cdr face)))

  ;; Since variable pitch width is set as the default above, override the face attributes
  ;; that we want to appear in fixed pitch width.
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-document-info-keyword nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-drawer nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-table nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-tag nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch)))

(use-package org
  :hook (org-mode . my/org-mode-init)
  :config
  ;; Save org buffers after refiling.
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  ;; Make it easier to create org-babel code blocks.
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  :custom
  (org-agenda-custom-commands
   `(("p" "Projects"
      ((todo
	"TODO"
        ((org-agenda-overriding-header "Personal Project Tasks")
	 (org-agenda-files '(,(concat my/org-dir "/personal.org")))
         (org-agenda-max-todos nil)))
       (todo
	"TODO"
        ((org-agenda-overriding-header "Work Project Tasks")
	 (org-agenda-files '(,(concat my/org-dir "/work.org")))
         (org-agenda-max-todos nil)))
       (todo
	"TODO"
        ((org-agenda-overriding-header "Unprocessed Inbox Tasks")
         (org-agenda-files '(,(concat my/org-dir "/inbox.org")))))))
     ;; TODO: Come up with these
     ;; ("1" "Next 7 Day Schedule"
     ;;  ((agenda "" ((org-deadline-warning-days 7)))))
     ;; ("2" "Next 14 Day Schedule"
     ;;  ((agenda "" ((org-deadline-warning-days 14)))))
     ;; ("3" "Next 30 Day Schedule"
     ;;  ((agenda "" ((org-deadline-warning-days )))))
     ))
  (org-agenda-files
   (list
    (concat my/org-dir "/inbox.org")
    (concat my/org-dir "/personal.org")
    (concat my/org-dir "/work.org")
    (concat my/org-dir "/tickler.org")))
  ;; Following variable allows customization of the agenda columns.
  (org-agenda-prefix-format
   '((agenda . " %i %-16:c%?-12t% s")
     (todo . " %i %-16:c")
     (tags . " %i %-16:c")
     (search . " %i %-16:c")))
  (org-agenda-span 'day)
  (org-agenda-start-with-log-mode t)
  (org-agenda-window-setup 'current-window)
  (org-capture-templates `(("i" "Inbox" entry
                            (file+headline ,(concat my/org-dir "/inbox.org") "Inbox")
			    "* TODO %i%?"
                            ;; "* TODO %i%?\nCREATED: %U\n"
			    :empty-lines-after 0)
			   ;; TODO: Come up with these
			   ;; ("p" "Project" entry
			   ;;  (file+headline ,(concat my/org-dir "/projects.org") "Projects")
			   ;;  "* %i%? %^g%^{CATEGORY}p\n** Tasks\n** Notes"
			   ;;  :empty-lines-before 1 :empty-lines-after 1 :jump-to-captured t)
                           ;; ("b" "Birthday" entry
                           ;;  (file+headline ,(concat my/org-dir "/tickler.org") "Birthdays")
                           ;;  "* %i%?\n<%<%Y-%m-%d +1y>>"
			   ;;  :empty-lines-after 1)
			   ))
  (org-confirm-babel-evaluate nil)
  (org-default-notes-file (concat my/org-dir "/inbox.org"))
  (org-directory my/org-dir)
  (org-ellipsis " 》")
  (org-fontify-done-headline t)
  (org-hide-emphasis-markers t)
  (org-log-done 'time)
  ;; Leaving drawer logging disabled for now as I don't like the format of the log items,
  ;; and I want to know when a task was created which doesn't happen without what apears
  ;; to be quite a bit of custom code.
  (org-log-into-drawer nil)
  (org-log-states-order-reversed nil) ; Make newest last
  (org-refile-targets
   `((,(concat my/org-dir "/archive.org") :level . 1)
     (,(concat my/org-dir "/inbox.org") :level . 1)
     (,(concat my/org-dir "/personal.org") :level . 3)
     (,(concat my/org-dir "/work.org") :level . 3)
     (,(concat my/org-dir "/someday.org") :level . 1)
     (,(concat my/org-dir "/tickler.org") :level . 2)))
  ;; The following two settings are required to make org-refile show the full heading path
  ;; to subheading refile candidates. Took a while to get this working properly.
  (org-refile-use-outline-path t)
  (org-outline-path-complete-in-steps nil)
  (org-tags-column 0)
  (org-todo-keywords
   `((sequence "TODO(t)" "HOLD(h)" "|" "DONE(d)")))
  ;; See colours here: https://alexschroeder.ch/geocities/kensanata/colors.html
  (org-todo-keyword-faces
   `(("TODO" . (:foreground "orange red" :weight bold))
     ("HOLD" . (:foreground "orange1" :weight bold))
     ("DONE" . (:foreground "DodgerBlue2" :weight bold)))))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun my/org-keywords-to-lower ()
  "Convert Org keywords to lower case.

Example: \"#+TITLE\" -> \"#+title\", etc."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil)
          (count 0))
      (while (re-search-forward "\\(?1:#\\+[A-Z_]+\\(?:_[[:alpha:]]+\\)*\\)\\(?:[ :=~’”]\\|$\\)" nil :noerror)
        (setq count (1+ count))
        (replace-match (downcase (match-string-no-properties 1)) :fixedcase nil nil 1))
      (message "Lower-cased %d matches" count))))

(use-package org-roam
  :hook
  ;; Should I just enable org-roam-mode when editing a .org file?
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory (concat my/org-dir "/notes"))
  (org-roam-db-location (concat my/xdg-cache-dir "/emacs/org-roam.db"))
  (org-roam-completion-everywhere t)
  (org-roam-completion-system 'ivy)
  (org-roam-capture-templates
    `(("d" "default" plain
       #'org-roam-capture--get-point
       ,(concat
	 "* Note\n"
	 "Replace or delete this section\n\n"
	 "* Links\n"
	 "** Roam Links\n"
	 "- Use org-roam-insert here\n\n"
	 "** Web Links\n"
	 "- Paste web URL here\n")
       :file-name "%<%Y%m%d%H%M%S>-${slug}"
       :head ,(concat
	       "#+title: ${title}\n"
	       "#+created: %<%Y-%m-%d>\n"
	       "#+roam_tags: tag1 tag2%?\n\n")
       :unnarrowed t)))
  :bind (:map org-roam-mode-map
              (("C-c n r" . org-roam)
               ("C-c n c" . org-roam-capture)
               ("C-c n f" . org-roam-find-file)
	       ("C-c n g" . org-roam-graph)
	       ;; Figure out how to bind these to simpler bindings that are only
	       ;; availble when editing an org-roam file.
	       ("C-c n t" . org-roam-tag-add)
	       ("C-c n a" . org-roam-alias-add))
              ;; ("C-c n d"   . org-roam-dailies-find-date)
              ;; ("C-c n c"   . org-roam-dailies-capture-today)
              ;; ("C-c n C r" . org-roam-dailies-capture-tomorrow)
              ;; ("C-c n t"   . org-roam-dailies-find-today)
              ;; ("C-c n y"   . org-roam-dailies-find-yesterday)
              ;; ("C-c n r"   . org-roam-dailies-find-tomorrow)
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate))))

;;
;; Languages
;;

  ;; Completion
  ;; (company-mode)
  ;; (setq company-idle-delay nil)

(defun my/company-mode ()
  "Run company-mode and configure autocompletion based on the major mode."
  (interactive)
  ;; Disable autocompletion for org files (it's annoying) but enable it for programming
  ;; modes. The variable company-idle-delay is global so we make a buffer local variable
  ;; out of it so that setting it in one buffer doesn't affect others.
  (set (make-local-variable 'company-idle-delay)
       (if (eq major-mode 'org-mode) nil 0.0))
  (company-mode))

(use-package company
  :hook
  (prog-mode . my/company-mode)
  (org-mode . my/company-mode)
  :bind
  (:map company-active-map
	("<tab>" . company-complete-selection)
	("C-n" . company-select-next-or-abort)
	("C-p" . company-select-previous-or-abort))
  ;; Can't use plain tab as org uses that for cycling visibility
  (:map org-mode-map ("C-c <tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(defun my/compile ()
  "Grabbed from https://github.com/rigtorp/dotemacs/blob/master/init.el."
  (interactive)
  (setq-local compilation-read-command nil)
  (call-interactively 'compile))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook
  (c-mode . lsp)
  (c++-mode . lsp)
  (go-mode . lsp)
  (typescript-mode . lsp)
  (rustic-mode . lsp)
  :custom
  (lsp-log-io nil)
  (lsp-keymap-prefix "C-c l")
  (lsp-disabled-clients '(ccls))
  (lsp-eldoc-render-all nil)

  ;; The following --query-driver args allow clangd to be used with the compile_commands.json
  ;; generated by bazel-compilation-database. This is due to the fact that, by default,
  ;; clangd expects the driver to be a standard compiler executable (e.g., clang++).
  ;; The log level can be changed to "debug" for additional information.
  (lsp-clients-clangd-args '("--query-driver=**/wrapped_clang"
			     "--background-index"
			     "--log=info"
			     ;; "--clang-tidy"
			     ;; "-j=1"
			     ))
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-rust-analyzer-server-display-inlay-hints t)
  :config
  (lsp-enable-which-key-integration t)
  :bind (:map lsp-mode-map
	      ("<tab>" . company-indent-or-complete-common)))

(use-package lsp-ui
  :custom
  (lsp-ui-sideline-enable nil)
  (lsp-ui-doc-position 'bottom))

(use-package dap-mode
  ;; :custom
  ;; (lsp-enable-dap-auto-configure nil)
  :hook
  (dap-stopped . (lambda (arg) (call-interactively #'dap-hydra)))
  :custom
  (dap-print-io t)
  ;; (dap-lldb-debug-program '("/Users/aeldridge/Development/home/llvm-project/build/bin/lldb-vscode"))
  :config
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  ;; (require 'dap-node)
  ;; (dap-node-setup)
  (require 'dap-lldb)
  ;; (require 'dap-gdb-lldb)
  ;; (dap-gdb-lldb-setup)
  ;; (require 'dap-cpptools)
  ;; (dap-cpptools-setup))
  ;; Do I need to call this?
  ;; (dap-auto-configure-mode)
  )

(use-package flycheck
  :defer t
  :hook (prog-mode . flycheck-mode))

(use-package lsp-treemacs
  :if window-system
  :after lsp-mode)

(use-package treemacs-all-the-icons
  :if window-system
  :config (treemacs-load-theme "all-the-icons"))

; TODO: configure what's shown/hidden by default
(use-package neotree
  :config
  (setq neo-theme (if window-system 'icons 'arrow)
	neo-smart-open t
	projectile-switch-project-action 'neotree-projectile-action)
  :bind
  (:map global-map
	("M-n" . my/neotree-project-dir)))

;; Copied from https://www.emacswiki.org/emacs/NeoTree
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

;; Similar to find symbol in project in Intellij
(use-package lsp-ivy
  :after lsp-mode)

(use-package typescript-mode
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2)
  (require 'dap-node)
  (dap-node-setup))

(use-package terraform-mode
  ; I prefer the // syntax to the default # comments.
  :hook (terraform-mode . (lambda () (setq comment-start "//"))))

;; Shell scripting indentation
(setq sh-basic-offset 2
      sh-indentation 2)

(use-package go-mode
  :mode "\\.go\\'"
  ;; Is this the best way to do this? Doesn't this meaen that the gofmt function
  ;; will get called on non-go files?
  :hook (before-save . gofmt-before-save)
  ;; :config
  ;; TODO: setup go-mode + integrations https://github.com/dominikh/go-mode.el
  )

(use-package rustic)

(use-package docker
  :bind ("C-c d" . docker))

(use-package dockerfile-mode)

(use-package bazel-mode
  :straight '(bazel-mode :host github
			 :repo "bazelbuild/emacs-bazel-mode")
  :mode "\\.BUILD\\'")

(defun my/compile ()
  "Grabbed from https://github.com/rigtorp/dotemacs/blob/master/init.el."
  (interactive)
  (setq-local compilation-read-command nil)
  (call-interactively 'compile))

(use-package cc-mode
  ;; :config
  ;; (require 'dap-lldb)
  ;; (require 'dap-cpptools)
  ;; (dap-cpptools-setup)
  ;; ()
  :bind (:map c++-mode-map
	      ("C-c f b" . clang-format-buffer)
	      ("C-c f r" . clang-format-region)
	      ;; ("M-p" . company-complete-common)
	      ("C-c c" . my/compile)))

;; TODO: consider switching to https://github.com/SavchenkoValeriy/emacs-clang-format-plus.
(use-package clang-format
  :commands clang-format clang-format-buffer clang-format-region
  :config
  (setq clang-format-fallback-style "llvm")) ;; Where is this defined?

(use-package flycheck-clang-tidy
  :after flycheck
  :hook
  (flycheck-mode . flycheck-clang-tidy-setup))

(add-hook 'before-save-hook 'my/delete-trailing-whitespace)

(defun my/delete-trailing-whitespace ()
  "Run 'delete-trailing-whitespace' if the current mode derives from 'prog-mode'."
  (when (derived-mode-p 'prog-mode)
    (delete-trailing-whitespace)))

;; Requires initial mu init command of:
;; mu init --maildir=~/Mail --my-address=aeldridge@fastmail.com --my-address=ashlin.eldridge@gmail.com
;;
;; Credentials are stored as app passwords using password-store (i.e., `pass`). For SMTP
;; credentials, the password should be named after the host (e.g., "smtp.gmail.com") and
;; the contents of the secret should specify the user and port as additional fields following
;; the password. E.g.:
;;
;; my-secret-password
;; user: aeldridge@fastmail.com
;; port: 465
;;
;; You can probably also include these details in the secret name itself using the pass notation,
;; but with the extra '@' in the username it starts getting ugly.

(use-package mu4e
  ;; Use the version of mu4e packaged with mu
  :straight nil
  :load-path "/usr/local/opt/mu/share/emacs/site-lisp/mu/mu4e/"
  :custom
  (mu4e-update-interval (* 10 60))
  (mu4e-get-mail-command (concat "mbsync -a -c " my/xdg-config-dir "/isync/mbsyncrc"))
  (mu4e-maildir "~/Mail")
  (mu4e-view-show-images t)
  (mu4e-view-show-addresses 't)

  ;; Use Ivy for mu4e completions (maildir folders, etc)
  (mu4e-completing-read-function #'ivy-completing-read)

  ;; Make sure that moving a message (like to Trash) causes the
  ;; message to get a new file name.  This helps to avoid the
  ;; dreaded "UID is N beyond highest assigned" error.
  ;; See this link for more info: https://stackoverflow.com/a/43461973
  (mu4e-change-filenames-when-moving t)

  ;; Don't keep message buffers around.
  (message-kill-buffer-on-exit t)

  ;; Context policies. Contexts are defined below in :config.
  (mu4e-context-policy 'pick-first)
  (mu4e-compose-context-policy 'always-ask)

  (mu4e-compose-dont-reply-to-self t)
  (mu4e-compose-format-flowed t)

  (mu4e-maildir-shortcuts
   '(("/Fastmail/Inbox" . ?f)
     ("/Gmail/Inbox"    . ?g)))

  (mu4e-bookmarks
   '((:name "Unread"
	    :query "flag:unread AND NOT \
                    flag:trashed AND NOT \
                    m:\"/Gmail/[Google Mail]/All Mail\" AND NOT \
                    m:\"/Gmail/[Google Mail]/Spam\" AND NOT \
                    m:\"/Gmail/[Google Mail]/Bin\" AND NOT \
                    m:/Fastmail/Spam AND NOT \
                    m:/Fastmail/Trash"
	    :key ?u)
     (:name "Today"
	    :query "date:today..now"
	    :key ?t)
     (:name "Last 7 days"
	    :query "date:7d..now"
	    :key ?w)))

  ;; Override view actions to remove unused actions and add an option to
  ;; to view the email in a browser - handy for complicated HTML emails.
  (mu4e-view-actions
   '(("capture message" . mu4e-action-capture-message)
     ("show this thread" . mu4e-action-show-thread)
     ("view in browser" . mu4e-action-view-in-browser)))

  :config
  ;; Use mu4e for sending e-mail.
  (setq mail-user-agent 'mu4e-user-agent)
  (setq message-send-mail-function 'smtpmail-send-it)

  ;; Configure email contexts. The variable mu4e-contexts is not defined as a custom variable
  ;; so cannot be configured above in the :custom section. When it is, an ordering problem
  ;; occurs where, when mu4e launches the context variables haven't been set and so mu4e wants
  ;; to create the sent, drafts, etc, directories.
  (setq mu4e-contexts
	(list
	 (make-mu4e-context
	  :name "Fastmail"
	  :match-func (lambda (msg)
			(when msg (string-prefix-p "/Fastmail" (mu4e-message-field msg :maildir))))
	  :vars '((user-full-name        . "Ashlin Eldridge")
		  (user-mail-address     . "aeldridge@fastmail.com")
		  (mu4e-sent-folder      . "/Fastmail/Sent")
		  (mu4e-trash-folder     . "/Fastmail/Trash")
		  (mu4e-drafts-folder    . "/Fastmail/Drafts")
		  (mu4e-refile-folder    . "/Fastmail/Archive")
		  (smtpmail-smtp-server  . "smtp.fastmail.com")
		  (smtpmail-smtp-service . 465)
		  (smtpmail-stream-type  . ssl)
		  ;; Does this work? If so, document.
		  (mu4e-sent-messages-behavior . sent)))

	 (make-mu4e-context
	  :name "Gmail"
	  :match-func (lambda (msg)
			(when msg (string-prefix-p "/Gmail" (mu4e-message-field msg :maildir))))
	  :vars '((user-full-name        . "Ashlin Eldridge")
		  (user-mail-address     . "ashlin.eldridge@gmail.com")
		  (mu4e-sent-folder      . "/Gmail/[Google Mail]/Sent Mail")
		  (mu4e-trash-folder     . "/Gmail/[Google Mail]/Bin")
		  (mu4e-drafts-folder    . "/Gmail/[Google Mail]/Drafts")
		  (mu4e-refile-folder    . "/Gmail/[Google Mail]/All Mail")
		  (smtpmail-smtp-server  . "smtp.gmail.com")
		  (smtpmail-smtp-service . 465)
		  (smtpmail-stream-type  . ssl)
		  ;; Trash sent messages because Gmail will already keep a copy in the sent folder.
		  (mu4e-sent-messages-behavior . trash))))))
