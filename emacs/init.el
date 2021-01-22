;; Do not show the startup screen.
(setq inhibit-startup-message t)

(tool-bar-mode -1)   ; Disable the tool bar
(tooltip-mode -1)    ; Disable tool tips
(menu-bar-mode -1)   ; Disable the menu bar
(scroll-bar-mode -1) ; Disable visible scrollbar
(set-fringe-mode 10) ; Increase left/right margins slightly

;; Enlargen frame size if we're running within a window system.
(when window-system
  (set-frame-size (selected-frame) 110 50))

;; Flash the mode line rather than use an audible bell.
(setq visible-bell nil)
(setq ring-bell-function (lambda ()
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil 'invert-face 'mode-line)))

;; Default font.
(set-face-attribute 'default nil :font "JetBrains Mono" :height 130)

;; Temporary theme
(load-theme 'wombat)

;; Show echoed keystrokes quicker.
(setq echo-keystrokes 0.01)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Show column numbers in the mode line.
(column-number-mode t)

;; Show line numbers on the left hand side.
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes.
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook
		shell-mode-hook
		help-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Disable arrow keys to "encourage" standard navigation.
(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<right>"))
(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))

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
			 ("orga" . "https://orgmode.org/elpa")))
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

(use-package counsel
  :delight
  :bind* ; load when pressed
  (("M-x"     . counsel-M-x)
   ("C-s"     . swiper)
   ("C-x C-f" . counsel-find-file)
   ("C-x C-r" . counsel-recentf)  ; search for recently edited
   ("C-c g"   . counsel-git)      ; search for files in git repo
   ("C-c j"   . counsel-git-grep) ; search for regexp in git repo
   ("C-c /"   . counsel-ag)       ; Use ag for regexp
   ("C-x l"   . counsel-locate)
   ("C-x C-f" . counsel-find-file)
   ("<f1> f"  . counsel-describe-function)
   ("<f1> v"  . counsel-describe-variable)
   ("<f1> l"  . counsel-find-library)
   ("<f2> i"  . counsel-info-lookup-symbol)
   ("<f2> u"  . counsel-unicode-char)
   ("C-c C-r" . ivy-resume)) ; Resume last Ivy-based completion
  :config
  ;; Default to "" for initial M-x rather than "^".
  (setcdr (assoc 'counsel-M-x ivy-initial-inputs-alist) ""))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package exec-path-from-shell
  :init (when (memq window-system '(mac ns x))
	  (setq exec-path-from-shell-variables
		'("PATH" "MANPATH" "XDG_CONFIG_HOME" "XDG_CACHE_HOME" "XDG_DATA_HOME"))
	  (setq exec-path-from-shell-arguments nil)
	  (exec-path-from-shell-initialize)))
