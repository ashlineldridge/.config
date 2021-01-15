;; Do not show the startup screen.
(setq inhibit-startup-message t)

;; Disable tool bar, menu bar, scroll bar.
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Highlight current line.
(global-hl-line-mode t)

;; Show echoed keystrokes quicker.
(setq echo-keystrokes 0.01)

;; Emacs XDG base directories.
(setq emacs-config-home "~/.config/emacs")
(setq emacs-cache-home "~/.cache/emacs")
(setq emacs-data-home "~/.local/share/emacs")

;; Store all backup files in XDG_CACHE_HOME.
;(setq backup-directory-alist '(("" . (concat emacs-data-home "/backup"))))
(setq backup-directory-alist '(("" . "~/.cache/emacs/backup/")))
(setq auto-save-file-name-transforms '((".*" "~/.cache/emacs/saves/" t)))

;; Save interrupted session records in XDG_DATA_HOME.
;(setq auto-save-list-file-prefix (concat emacs-data-home "/auto-save-list"))
(setq auto-save-list-file-prefix "~/.cache/emacs/auto-save-list/")

;; Use `command` as `meta` in macOS.
;; (setq mac-command-modifier 'meta)

;; Do not use `init.el` for `custom-*` code - use `custom.el`.
(setq custom-file (concat emacs-config-home "/custom.el"))

;; Assuming that the code in custom.el is executed before the code
;; ahead of this line is not a safe assumption. So load this file
;; proactively.
(load-file custom-file)

;; Require and initialize `package`.
(require 'package)
(setq package-user-dir "~/.cache/emacs/packages/")
(package-initialize)

;; Add `melpa` to `package-archives`.
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; Bootstrap use-package.
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

;; Additional packages and their configurations

(use-package spacemacs-common
  :ensure spacemacs-theme
  :config
  ;; Comments should appear in italics.
  (setq spacemacs-theme-comment-italic t)

  ;; Use the `spacemacs-dark` theme.
  (load-theme 'spacemacs-dark t))

(use-package company
  ;; Navigate in completion minibuffer with `C-n` and `C-p`.
  :bind (:map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :config
  ;; Provide instant autocompletion.
  (setq company-idle-delay 0.3)

  ;; Use company mode everywhere.
  (global-company-mode t))

;; Recent buffers in a new Emacs session
;(use-package recentf
  ;:config
  ;(setq recentf-auto-cleanup 'never
        ;recentf-max-saved-items 1000
        ;recentf-save-file (concat user-emacs-directory ".recentf"))
  ;(recentf-mode t)
  ;:diminish nil)

;;; Display possible completions at all places
;(use-package ido-completing-read+
  ;:ensure t
  ;:config
  ;;; This enables ido in all contexts where it could be useful, not just
  ;;; for selecting buffer and file names
  ;(ido-mode t)
  ;(ido-everywhere t)
  ;;; This allows partial matches, e.g. "uzh" will match "Ustad Zakir Hussain"
  ;(setq ido-enable-flex-matching t)
  ;(setq ido-use-filename-at-point nil)
  ;;; Includes buffer names of recently opened files, even if they're not open now.
  ;(setq ido-use-virtual-buffers t)
  ;:diminish nil)

;;; Enhance M-x to allow easier execution of commands
;(use-package smex
  ;:ensure t
  ;;; Using counsel-M-x for now. Remove this permanently if counsel-M-x works better.
  ;:disabled t
  ;:config
  ;(setq smex-save-file (concat user-emacs-directory ".smex-items"))
  ;(smex-initialize)
  ;:bind ("M-x" . smex))

;;; Git integration for Emacs
;(use-package magit
  ;:ensure t
  ;:bind ("C-x g" . magit-status))

;;; Better handling of paranthesis when writing Lisps.
;(use-package paredit
  ;:ensure t
  ;:init
  ;(add-hook 'clojure-mode-hook #'enable-paredit-mode)
  ;(add-hook 'cider-repl-mode-hook #'enable-paredit-mode)
  ;(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  ;(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  ;(add-hook 'ielm-mode-hook #'enable-paredit-mode)
  ;(add-hook 'lisp-mode-hook #'enable-paredit-mode)
  ;(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  ;(add-hook 'scheme-mode-hook #'enable-paredit-mode)
  ;:config
  ;(show-paren-mode t)
  ;:bind (("M-[" . paredit-wrap-square)
         ;("M-{" . paredit-wrap-curly))
  ;:diminish nil)

