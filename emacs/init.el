;; TODO:
;; 1. Convert this into an org file
;; 2. Switch over to straight.el
;; 3. Use :ensure t?
;; 4. Remove window-system checks - no longer needed

;; Do not show the startup screen.
(setq inhibit-startup-message t)

(tool-bar-mode -1)   ; Disable the tool bar
(tooltip-mode -1)    ; Disable tool tips
(menu-bar-mode -1)   ; Disable the menu bar
(scroll-bar-mode -1) ; Disable visible scrollbar
(set-fringe-mode 10) ; Increase left/right margins slightly

;; If I need to run Emacs in the terminal I'll use `-nw -q`.
;; Better than littering this file with window system checks.
(unless window-system
  (error "This Emacs init file is for window systems only."))

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

;; Default font.
(set-face-attribute 'default nil :font "Jetbrains Mono" :height 130)

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

;; Show column numbers in the mode line.
(column-number-mode t)

;; Show line numbers on the left hand side.
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes.
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook
		shell-mode-hook
		help-mode-hook
		treemacs-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Use CMD key for META.
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier nil)

;; TODO: This could be better. Emacs XDG base directories.
(setq emacs-config-home "~/.config/emacs")
(setq emacs-cache-home "~/.cache/emacs")
(setq emacs-data-home "~/.local/share/emacs")

;; TODO: Use variables above and create subdirectories below if they don't exist.

;; Store all backup files in XDG_CACHE_HOME.
;(setq backup-directory-alist '(("" . (concat emacs-data-home "/backup"))))
(setq backup-directory-alist '(("" . "~/.cache/emacs/backup/")))
(setq auto-save-file-name-transforms '((".*" "~/.cache/emacs/saves/" t)))

;; Save interrupted session records in XDG_DATA_HOME.
;(setq auto-save-list-file-prefix (concat emacs-data-home "/auto-save-list"))
(setq auto-save-list-file-prefix "~/.cache/emacs/auto-save-list/")

;; Do not use `init.el` for `custom-*` code - use `custom.el`.
(setq custom-file (concat emacs-config-home "/custom.el"))

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

;; Use the delight package for hiding major and minor modes.
(use-package delight)
(require 'delight)

(use-package ivy
  :delight
  :init (ivy-mode 1) ; globally at startup
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
   ("C-x C-f" . counsel-find-file)
   ("C-x b"   . counsel-switch-buffer)
   ("M-y"     . counsel-yank-pop)
   :map minibuffer-local-map ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ; Don't start searches with ^

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package exec-path-from-shell
  :init (when (memq window-system '(mac ns x))
	  (setq exec-path-from-shell-variables
		'("PATH" "MANPATH" "XDG_CONFIG_HOME" "XDG_CACHE_HOME" "XDG_DATA_HOME" "GNUPGHOME"))
	  (setq exec-path-from-shell-arguments nil)
	  (exec-path-from-shell-initialize)))

(use-package rainbow-delimiters
  :init (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

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

(use-package doom-themes
  :init (load-theme 'doom-tomorrow-night t))

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
  (doom-modeline-height 25)
  (doom-modeline-bar-width 0)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-enable-word-count nil)
  (doom-modeline-buffer-encoding t)
  (doom-modeline-indent-info nil)
  (doom-modeline-checker-simple-format t)
  (doom-modeline-vcs-max-length 12)
  (doom-modeline-env-version t)
  (doom-modeline-irc-stylize 'identity)
  (doom-modeline-github-timer nil)
  (doom-modeline-gnus-timer nil))

(use-package minions
  :hook (doom-modeline-mode . minions-mode))
  ;; :custom
  ;; (minions-mode-line-lighter ""))

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
;  :custom
;  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge :after magit)

(use-package ace-window
  :bind (("M-o" . ace-window))
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; Turn on indentation and auto-fill mode for Org files
(defun ae/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (diminish org-indent-mode))

(use-package org
;  :hook (org-mode . ae/org-mode-setup)
  :config
  (setq org-ellipsis " 》"
	org-agenda-start-with-log-mode t
	org-log-done 'time
	org-log-into-drawer t
	org-agenda-files '("~/Development/home/hello-org/hello.org")
        org-src-fontify-natively t
        org-src-tab-acts-natively t))
        ;; org-src-preserve-indentation nil
        ;; org-edit-src-content-indentation 2
        ;; org-hide-block-startup nil
        ;; org-startup-folded 'content
        ;; org-cycle-separator-lines 2)

;; (use-package org-superstar
;;   :after org
;;   :hook (org-mode . org-superstar-mode)
;;   :custom
;;   (org-superstar-remove-leading-stars t)
;;   (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))

;;
;; Languages
;;

(defun ae/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segements '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))


(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook
  (lsp-mode . ae/lsp-mode-setup)
  (c-mode . lsp-deferred)
  (c++-mode . lsp-deferred)
  (go-mode . lsp-deferred)
  (typescript-mode . lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l") ;; C-l?
  (setq lsp-print-io t)
  :config
  (lsp-enable-which-key-integration t)
  :bind (:map lsp-mode-map
	      ("<tab>" . completion-at-point)))

;; TODO: Bind this to a key so you can optionally brig up the popup.
;; (use-package lsp-ui
;;   :hook (lsp-mode . lsp-ui-mode)
;;   :config
;;   (setq lsp-ui-sideline-enable t)
;;   (setq lsp-ui-sideline-show-hover nil)
;;   (setq lsp-ui-doc-position 'bottom)
;;   (lsp-ui-doc-show))

(use-package flycheck
  :defer t
  :hook (lsp-mode . flycheck-mode))

(use-package treemacs
  ; Disable treemacs in terminal for now as it's resulting in color errors.
  :if window-system
  :bind
  (:map global-map
	("C-c t 0"   . treemacs-select-window)
        ("C-c t 1"   . treemacs-delete-other-windows)
        ("C-c t t"   . treemacs)
        ("C-c t B"   . treemacs-bookmark)
        ("C-c t C-t" . treemacs-find-file)
        ("C-c t M-t" . treemacs-find-tag)))
  
(use-package lsp-treemacs
  :if window-system
  :after lsp-mode)

(use-package treemacs-all-the-icons
  :if window-system
  :config (treemacs-load-theme "all-the-icons"))

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
(use-package lsp-ivy)

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind
  (:map company-active-map ("<tab>" . company-complete-selection))
  (:map lsp-mode-map ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package go-mode)

(use-package typescript-mode
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2))

(use-package terraform-mode)

;; Shell scripting indentation
(setq sh-basic-offset 2
      sh-indentation 2)

(use-package ccls
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp))))
