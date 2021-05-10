;;; init.el --- Emacs init file.

;; Author: Ashlin Eldridge
;; Version: 0.0.1
;; Keywords: emacs, init, lisp

;;; Commentary:
;;
;; My slowly growing Emacs init file.

;;; TO DO:
;; - Convert this into an org file
;; - Switch over to straight.el?
;; - Use :ensure t?
;; - Remove window-system checks - no longer needed
;; - Use :defer X where X is the number of secs to improve start up time
;;   See: https://blog.d46.us/advanced-emacs-startup/
;; - Use no-littering https://github.com/emacscollective/no-littering
;; - Get use-package / auto-package-update to install org from the org archive rather than built-in
;;   See: https://www.reddit.com/r/emacs/comments/5sx7j0/how_do_i_get_usepackage_to_ignore_the_bundled
;; - Get org mode completion working with company? counsel? Might need to use Karabiner Elements
;;   to allow Emacs to get CMD+Tab since org-mode needs tab.
;;   See: https://superuser.com/questions/548146/change-command-tab-to-option-tab-on-mac
;; - Make org-mode faces nicer (bold should be bolder now that org-hide-emphasis-markers is set to t.
;; - Get inspo from https://lepisma.xyz/2017/10/28/ricing-org-mode/
;; - Why do things go weird when I scroll past a .org file that hasn't been opened (i.e., is in light
;;   grey) when doing C-x b?
;; - Make my/org-reset-blah function restore the current state of org in terms of current visibility.
;; - How do I get rid of the shadow when an org section contains a code block and the buffer is in the
;;   `overview` state? (Following the ellipsis is a weird shadow block.) Workaround: tinyurl.com/r54xfc8n
;; - Switch as much stuff from `setq` inside `:config` to `:custom`.
;; - Use https://orgmode.org/worg/org-contrib/org-protocol.html to capture tasks from outside emacs -
;;   e.g., from pages open in browser (you can create a custom Firefox "button").
;; - Use org-ql for querying org files: https://github.com/alphapapa/org-ql.
;; - Install https://github.com/akermu/emacs-libvterm for a good Emacs terminal

;;; Links:
;; - https://sachachua.com/blog/wp-content/uploads/2014/01/2014-01-07-Map-for-learning-Org-Mode-for-Emacs.png
;; - https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html
;; - https://orgmode.org/manual/Capture-templates.html#Capture-templates
;; - https://orgmode.org/manual/Refile-and-Copy.html#Refile-and-copy
;; - https://orgmode.org/manual/Refile-and-Copy.html#Refile-and-copy

;;; Code:

;; Set this early so that newest versions of files are loaded. Apparently, this can negatively
;; impact performance but in my tests it didn't affect start-up time and I don't restart Emacs
;; that often anyway.
(setq load-prefer-newer t)

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
(defvar my/org-dir "~/Development/home/org")

;; Emacs config directory-related variable definitions.
(defvar my/xdg-config-dir "~/.config/")
(defvar my/xdg-cache-dir "~/.cache/")
(defvar my/xdg-data-dir "~/.local/share/")

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

;; Disable electric-indent-mode?
;; (add-hook 'after-change-major-mode-hook (lambda() (electric-indent-mode -1)))

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

;; Do not use `init.el` for `custom-*` code - use `custom.el`.
(setq custom-file "~/.config/emacs/custom.el")

;; Assuming that the code in custom.el is executed before the code
;; ahead of this line is not a safe assumption. So load this file
;; proactively.
(load-file custom-file)

;; Require and configure `package`.
(require 'package)
(setq package-user-dir "~/.cache/emacs/packages/")
(setq package-archives '(("elpa" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Bootstrap use-package.
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Install auto-package-update as the first package. When it is configured, it'll prompt
;; to run an update cycle (if the specified number of days have elapsed) early before the
;; other packages have loaded so that we can update them before they're loaded.
(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results nil)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

;; Next, import environment variables from the shell (defined in "${XDG_BASE_CONFIG}/zsh/lib/env.zsh")
;; so that they are available to subsequent expressions.
(use-package exec-path-from-shell
  :init (when (memq window-system '(mac ns x))
	  (setq exec-path-from-shell-variables
		'("PATH" "MANPATH" "XDG_CONFIG_HOME" "XDG_CACHE_HOME" "XDG_DATA_HOME"
		  "GNUPGHOME" "PASSWORD_STORE_DIR" "DEVELOPER_DIR" "SDKROOT"))
	  (setq exec-path-from-shell-arguments nil)
	  (exec-path-from-shell-initialize)))


;; Use the delight package for hiding major and minor modes.
;; Is this even necessary since we've got minions?
(use-package delight)
(require 'delight)

;; Show column rule indicator. This package provides a nicer looking
;; vertical bar than the in-built display-fill-column-indicator mode.
;; TODO: Put all column UX stuff together.
(use-package fill-column-indicator
  :disabled
  :hook
  ((prog-mode text-mode) . fci-mode)
  ;; :init (fci-mode t)
  :config
  (setq fci-rule-column 100))

(use-package ivy
  :delight
  :init (ivy-mode t) ; globally at startup
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 20)
  (setq ivy-count-format "%d/%d "))

(use-package ivy-rich :delight :init (ivy-rich-mode 1))

(use-package counsel
  :delight
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
  :init (setq markdown-command "multimarkdown"))

(use-package rainbow-delimiters
  :init (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

;; Learn how to use first
;; (use-package paredit :hook (emacs-lisp-mode . paredit-mode))

(use-package which-key
  :delight
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

;; TODO: Why aren't all minor modes shown by minions? E.g., ivy-prescient-mode?
(use-package minions
  :hook (doom-modeline-mode . minions-mode))

(use-package projectile
  :delight
  :init
  (projectile-add-known-project "~/.config")
  (setq projectile-project-search-path
	; Search path is displayed from right to left.
	'("~/Development/home" "~/Development/work"))
  (setq projectile-switch-project-action #'projectile-dired)
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :custom ((projectile-completion-system 'ivy)))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit)
  ;; :bind (("C-c g" . magit-status))) ;; But how to remove C-x g?

(use-package forge :after magit)

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
  :pin org ;; Why isn't this forcing pull latest?
  :hook (org-mode . my/org-mode-init)
  :config
  ;; Save org buffers after refiling.
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  :custom
  (org-agenda-custom-commands
	`(("d" "Dashboard"
           ((agenda "" ((org-deadline-warning-days 7)))
            ;; (tags-todo "+PRIORITY=\"A\""
            ;;            ((org-agenda-overriding-header "High Priority")))
            ;; (tags-todo "+followup" ((org-agenda-overriding-header "Needs Follow Up")))
            (todo "NEXT"
                  ((org-agenda-overriding-header "Next Actions")
                   (org-agenda-max-todos nil)))
            (tags-todo "+CATEGORY=\"Inbox\""
                  ((org-agenda-overriding-header "Unprocessed Inbox Tasks")
                   (org-agenda-files '(,(concat my/org-dir "/gtd.org")))))))
          ("n" "Next Tasks"
           ((agenda "" ((org-deadline-warning-days 7)))
            (todo "NEXT"
                  ((org-agenda-overriding-header "Next Tasks")))))))
  (org-agenda-files
   `(,(concat my/org-dir "/gtd.org")))
  (org-agenda-span 'day)
  (org-agenda-start-with-log-mode t)
  (org-agenda-window-setup 'current-window)
  (org-capture-templates `(("i" "Inbox" entry
                            (file+headline ,(concat my/org-dir "/gtd.org") "Inbox")
                            "* TODO %i%?"
			    :empty-lines-after 1)
			   ("p" "Project" entry
			    (file+headline ,(concat my/org-dir "/gtd.org") "Projects")
			    "* %i%? %^g%^{CATEGORY}p\n** Tasks\n** Notes"
			    :empty-lines-before 1 :empty-lines-after 1 :jump-to-captured t)
                           ("b" "Birthday" entry
                            (file+headline ,(concat my/org-dir "/gtd.org") "Birthdays")
                            "* %i%?\n<%<%Y-%m-%d +1y>>"
			    :empty-lines-after 1)))
  (org-default-notes-file (concat my/org-dir "/inbox.org"))
  (org-directory my/org-dir)
  (org-ellipsis " 》")
  (org-hide-emphasis-markers t)
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-refile-targets
   `((,(concat my/org-dir "/gtd.org") :maxlevel . 3)
     (,(concat my/org-dir "/archive.org") :level . 3)))
  (org-tags-column 0)
  (org-todo-keywords
   `((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
     (sequence "|" "WAIT(w)" "BACK(b)")))
  (org-todo-keyword-faces
   `(("NEXT" . (:foreground "orange red" :weight bold))
     ("WAIT" . (:foreground "HotPink2" :weight bold))
     ("BACK" . (:foreground "MediumPurple3" :weight bold)))))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org-roam
  ;; :pin org
  :ensure t
  :hook
  ;; Should I just enable org-roam-mode when editing a .org file?
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory (concat my/org-dir "/roam"))
  (org-roam-db-location (concat my/xdg-cache-dir "/emacs/org-roam.db"))
  (org-roam-completion-everywhere t)
  (org-roam-completion-system 'ivy)
  ;; (org-roam-capture-templates
  ;;   '(("d" "default" plain
  ;;      #'org-roam-capture--get-point
  ;;      "%?"
  ;;      :file-name "%<%Y%m%d%H%M%S>-${slug}"
  ;;      :head "#+title: ${title}\n"
  ;;      :unnarrowed t)
  ;;     ("ll" "link note" plain
  ;;      #'org-roam-capture--get-point
  ;;      "* %^{Link}"
  ;;      :file-name "Inbox"
  ;;      :olp ("Links")
  ;;      :unnarrowed t
  ;;      :immediate-finish)
  ;;     ("lt" "link task" entry
  ;;      #'org-roam-capture--get-point
  ;;      "* TODO %^{Link}"
  ;;      :file-name "Inbox"
  ;;      :olp ("Tasks")
  ;;      :unnarrowed t
  ;;      :immediate-finish)))
  :bind (:map org-roam-mode-map
              (("C-c r r" . org-roam)
               ("C-c r f" . org-roam-find-file)
	       ("C-c r g" . org-roam-graph)
	       ;; Figure out how to bind these to simpler bindings that are only
	       ;; availble when editing an org-roam file.
	       ("C-c r t" . org-roam-tag-add)
	       ("C-c r a" . org-roam-alias-add))
               ;; ("C-c r d"   . org-roam-dailies-find-date)
               ;; ("C-c r c"   . org-roam-dailies-capture-today)
               ;; ("C-c r C r" . org-roam-dailies-capture-tomorrow)
               ;; ("C-c r t"   . org-roam-dailies-find-today)
               ;; ("C-c r y"   . org-roam-dailies-find-yesterday)
               ;; ("C-c r r"   . org-roam-dailies-find-tomorrow)
              :map org-mode-map
              (("C-c r i" . org-roam-insert))
              (("C-c r I" . org-roam-insert-immediate))))

;;
;; Languages
;;

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
  ; The following --query-driver args allow clangd to be used with the compile_commands.json
  ; generated by bazel-compilation-database. This is due to the fact that, by default,
  ; clangd expects the driver to be a standard compiler executable (e.g., clang++).
  ; The log level can be changed to "debug" for additional information.
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
	      ("<tab>" . completion-at-point)))

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

;; daviwil's setup
;; (use-package dap-mode
;;   :straight t
;;   :custom
;;   (lsp-enable-dap-auto-configure nil)
;;   :config
;;   (dap-ui-mode 1)
;;   (dap-tooltip-mode 1)
;;   (require 'dap-node)
;;   (dap-node-setup))

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
	("C-c n" . neotree-project-dir)))

  (defun neotree-project-dir ()
    "Open NeoTree using the git root. \
     Copied from https://www.emacswiki.org/emacs/NeoTree"
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

(use-package company
  :after lsp-mode
  :hook
  (prog-mode . company-mode)
  (org-mode . company-mode)
  :bind
  (:map company-active-map
	("<tab>" . company-complete-selection)
	("C-n" . company-select-next-or-abort)
	("C-p" . company-select-previous-or-abort))
  (:map lsp-mode-map ("<tab>" . company-indent-or-complete-common))
  ;; (:map org-mode-map ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

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

;; Stick with clangd for now
;; (use-package ccls
;;   :hook ((c-mode c++-mode objc-mode cuda-mode) .
;;          (lambda () (require 'ccls) (lsp))))

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
  ;; :defer 20   ; Wait until 20 seconds after startup
  :ensure nil ; Use the version of mu4e packaged with mu
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

  ;; (add-to-list 'mu4e-bookmarks
  ;;              (make-mu4e-bookmark
  ;;               :name "All Inboxes"
  ;;               :query "maildir:/Fastmail/INBOX OR maildir:/Personal/Inbox"
  ;;               :key ?i))

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

;; TODO: Get email functional for Gmail and Fastmail with the intention of
;; being able to use Fastmail for subscribing to kernel mailing lists, etc.
;; Get email bookmarks working so that you can quickly hunt around for things.
;; Get email sending working efficiently/predictably.

;; ;; Load org-mode integration
;; (require 'org-mu4e)
