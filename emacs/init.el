;;; init.el --- Emacs Init -*- lexical-binding: t -*-

;; Author: Ashlin Eldridge <ashlin.eldridge@gmail.com>
;; URL: https://github.com/ashlineldridge/.config
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;;
;; My bonsai.

;;; Code:

;;;; Bootstrap

(load (expand-file-name "elpaca-bootstrap.el" user-emacs-directory))
(declare-function elpaca-wait "elpaca")

(use-package no-littering
  :demand t
  :commands no-littering-theme-backups
  :config
  (no-littering-theme-backups))

(use-package general
  :demand t
  :commands
  (general-auto-unbind-keys
   general-define-key)
  :config
  ;; Automatically unbind non-prefix keys when used.
  (general-auto-unbind-keys)
  (general-create-definer my/bind-search :prefix "M-s")
  (general-create-definer my/bind-goto :prefix "M-g")
  (general-create-definer my/bind-c-c :prefix "C-c")
  (general-create-definer my/bind-c-x :prefix "C-x"))

;; Packages that modify the syntax of `use-package' need to be waited on.
(elpaca-wait)

(use-package gcmh
  :hook (elpaca-after-init . gcmh-mode)
  :custom
  (gcmh-idle-delay 'auto)
  (gcmh-auto-idle-delay-factor 10)
  (gcmh-high-cons-threshold (* 16 1024 1024)))

(use-package elpaca
  :elpaca nil
  :general
  (my/bind-c-c
    "pp" #'elpaca-manager
    "pf" #'elpaca-fetch
    "pF" #'elpaca-fetch-all
    "pm" #'elpaca-merge
    "pM" #'elpaca-merge-all
    "pb" #'elpaca-browse
    "pd" #'elpaca-delete
    "pt" #'elpaca-try
    "pr" #'elpaca-rebuild
    "pv" #'elpaca-visit))

;;;; Base Settings

(use-package emacs
  :elpaca nil
  :hook
  ;; Display line numbers in certain modes.
  ((prog-mode config-mode text-mode) . display-line-numbers-mode)
  ;; Don't wrap long lines in programming modes.
  (prog-mode . my/truncate-lines)
  ;; Run Emacs in server mode.
  (elpaca-after-init . my/server-start)

  :general
  (general-def
    "<escape>" #'keyboard-escape-quit
    "C-;" #'comment-line
    "C-S-k" #'my/copy-to-eol
    "C-M-k" #'my/delete-to-eol
    "C-h C-h" nil
    "M-[" #'previous-buffer
    "M-]" #'next-buffer
    "M-<backspace>" #'my/delete-to-bol)

  (my/bind-search
    ;; Add search prefix descriptions.
    "h" '(:ignore t :which-key "highlight"))

  (my/bind-c-c
    "-" '(:ignore t :which-key "outline")
    "b" '(:ignore t :which-key "build")
    "b1" #'compile
    "b2" #'recompile
    "c" '(:ignore t :which-key "cape")
    "f" '(:ignore t :which-key "flymake")
    "g" '(:ignore t :which-key "magit")
    "n" '(:ignore t :which-key "notes")
    "o" '(:ignore t :which-key "org")
    "p" '(:ignore t :which-key "cape")
    "r" '(:ignore t :which-key "refactor")
    "t" '(:ignore t :which-key "test")
    "x" '(:ignore t :which-key "util")
    "x|" #'display-fill-column-indicator-mode
    "xh" #'hl-line-mode
    "xn" #'display-line-numbers-mode
    "xz" #'delete-trailing-whitespace
    "xZ" #'my/toggle-show-trailing-whitespace)

  (my/bind-c-x
    "rK" #'my/clear-registers)

  :custom
  (confirm-kill-emacs #'yes-or-no-p)
  (inhibit-startup-message t)
  (tool-bar-mode nil)
  (tooltip-mode nil)
  (menu-bar-mode nil)
  (scroll-bar-mode nil)
  ;; Flash the modeline rather than the frame.
  (visible-bell nil)
  (ring-bell-function #'my/flash-mode-line)
  ;; Play nice with window managers like Yabai.
  (frame-resize-pixelwise t)
  ;; Don't resize the frame when themes, fonts, etc, change.
  (frame-inhibit-implied-resize t)
  (sentence-end-double-space nil)
  (delete-selection-mode t)
  (echo-keystrokes 0.01)
  (mac-command-modifier 'meta)
  (mac-option-modifier nil)
  (tab-always-indent 'complete)
  (indent-tabs-mode nil)
  (set-mark-command-repeat-pop t)
  (mark-ring-max 10)
  (global-mark-ring-max 10)
  (vc-follow-symlinks t)
  (compilation-ask-about-save nil)
  (fill-column 80)
  (column-number-mode t)
  (async-shell-command-buffer 'new-buffer)
  (history-length 1000)
  ;; Save bookmarks immediately.
  (bookmark-save-flag 0)
  (savehist-mode t)
  ;; Variables to persist between sessions.
  (savehist-additional-variables
   '(kill-ring
     search-ring
     regexp-search-ring
     register-alist
     extended-command-history))
  (electric-pair-mode t)
  (electric-pair-inhibit-predicate #'my/electric-pair-inhibit)
  (repeat-mode t)
  (repeat-exit-timeout 10)
  (repeat-exit-key (kbd "RET"))
  (repeat-echo-function #'my/repeat-echo-mode-line)
  ;; Use GNU ls (used by dired and supports grouping directories first).
  (insert-directory-program "gls")
  ;; Increase margins slightly.
  (fringe-mode 5)
  ;; Ignore any changes made via the customization UI.
  (custom-file (make-temp-file "emacs-custom-"))
  ;; Enable recursive editing to allow multiple minibuffers to be opened on top
  ;; of each other. E.g., this allows starting a query-replace, then open a
  ;; file to look at something, then go back to the query-replace minibuffer.
  (enable-recursive-minibuffers t)
  ;; Display a small "[n]" that shows the minibuffer recursive depth. Another
  ;; option is to use https://github.com/minad/recursion-indicator.
  (minibuffer-depth-indicate-mode t)
  ;; Don't allow the cursor in the minibuffer prompt text.
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  ;; Don't show M-x commands which don't work in the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)

  :init
  ;; Use y/n rather than yes/no.
  (defalias 'yes-or-no-p #'y-or-n-p)

  ;; Enable Emacs functions that are disabled by default.
  (dolist (cmd '(narrow-to-region
                 upcase-region
                 downcase-region
                 set-goal-column))
    (put cmd 'disabled nil))

  (defun my/server-start ()
    "Start Emacs in server mode if it is not already."
    (require 'server)
    (unless (server-running-p)
      (server-start)))

  (defun my/truncate-lines ()
    "Truncate long lines rather than wrapping."
    (setq-local truncate-lines t))

  (defun my/toggle-show-trailing-whitespace ()
    "Toggle visibility of trailing whitespace."
    (interactive)
    (setq show-trailing-whitespace (if show-trailing-whitespace nil t))
    (message "%s trailing whitespace" (if show-trailing-whitespace
                                          "Showing" "Hiding")))

  (defun my/copy-to-eol ()
    "Copy the text from point to the end of the line."
    (interactive)
    (kill-ring-save (point) (line-end-position)))

  (defun my/delete-to-eol ()
    "Delete the text from point to the end of the line."
    (interactive)
    (let ((point (point))
          (end (line-end-position)))
      (if (eq point end)
          (delete-char 1 nil)
        (delete-region point end))))

  (defun my/delete-to-bol ()
    "Delete the text from point to the beginning of the line."
    (interactive)
    (let ((point (point))
          (begin (line-beginning-position)))
      (if (eq point begin)
          (delete-char -1)
        (delete-region (line-beginning-position) (point)))))

  (defun my/flash-mode-line ()
    "Flashes the mode line for a visible bell."
    (invert-face 'mode-line)
    (run-with-timer 0.1 nil 'invert-face 'mode-line))

  (defun my/electric-pair-inhibit (c)
    "Return whether C should be excluded from pairing."
    ;; Exclude '<' as I want that for triggering Tempel.
    (if (char-equal c ?<) t (electric-pair-default-inhibit c)))

  (defun my/clear-registers ()
    "Clear all registers."
    (interactive)
    (if register-alist
        (let ((len (length register-alist)))
          (setq register-alist nil)
          (message "Cleared %d registers" len))
      (message "No registers to clear")))

  (defun my/repeat-echo-mode-line (keymap)
    "Display a repeat mode-line indicator for KEYMAP."
    ;; The provided `repeat-echo-mode-line' doesn't work for me as it tries
    ;; to represent an in-progress repeat as an additional mode which I think
    ;; gets hidden by Minions. This looks cleaner anyway.
    (setq global-mode-string
          (when keymap (propertize "↻ " 'face 'mode-line-emphasis)))
    (force-mode-line-update t))

  ;; See: https://karthinks.com/software/it-bears-repeating.
  (defun my/repeatize (keymap)
    "Add `repeat-mode'-like support to KEYMAP (passed by symbol)."
    (map-keymap
     (lambda (_key cmd)
       (when (symbolp cmd)
         (put cmd 'repeat-map keymap)))
     (symbol-value keymap))))

(use-package time
  :elpaca nil
  :custom
  (display-time-mode t)
  (display-time-default-load-average nil)
  (display-time-format "%H:%M")
  ;; Timezones to be displayed by `world-clock'. Zones names can be found
  ;; here: https://www.timezoneconverter.com/cgi-bin/tzc.
  (world-clock-list
   '(("Australia/Melbourne" "Melbourne")
     ("America/Los_Angeles" "Seattle")
     ("America/New_York" "New York")
     ("Europe/London" "London")
     ("Europe/Paris" "Paris")
     ("Europe/Vilnius" "Lithuania")
     ("Asia/Tokyo" "Tokyo")
     ("Canada/Eastern" "Toronto"))))

;;;; Appearance

;;;;; Themes

(use-package custom
  :elpaca nil
  :init
  ;; Default theme loaded by `consult-theme' after init.
  (defvar my/default-theme 'modus-vivendi)

  ;; Variable pitch headings used by Modus and Ef themes.
  (defvar my/variable-pitch-headings
    '((1 . (variable-pitch semibold 1.2))
      (t . (variable-pitch semibold 1.1))))

  ;; Theme-agnostic hook that is run after a theme is enabled.
  (defvar my/after-enable-theme-hook nil
    "Hook run after enabling a theme.")

  ;; Call hooks after the theme is enabled. No need to call `disable-theme'
  ;; before enabling to avoid "blending" (Prot mentions this in his theme docs)
  ;; as I use `consult-theme' which does this anyway.
  (defun my/after-enable-theme (&rest _args)
    "Run after `enable-theme'."
    (run-hooks 'my/after-enable-theme-hook))
  (advice-add #'enable-theme :after #'my/after-enable-theme))

(use-package modus-themes
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-custom-auto-reload t)
  (modus-themes-prompts '(bold))
  (modus-themes-org-blocks 'gray-background)
  (modus-themes-headings my/variable-pitch-headings))

(use-package ef-themes
  :custom
  (ef-themes-mixed-fonts t)
  (ef-themes-variable-pitch-ui t)
  (ef-themes-headings my/variable-pitch-headings))

;;;;; Fonts

(use-package faces
  :elpaca nil
  :hook
  ;; Update fonts after theme is loaded so changes take effect.
  (my/after-enable-theme . my/apply-font-config)

  :config
  (defvar my/font-config-index 0)
  (defvar my/fixed-font "Iosevka Comfy")
  (defvar my/variable-font "Iosevka Comfy Duo")

  (defvar my/font-configs
    '((:fixed-font-height 140
       :variable-font-height 140
       :line-number-font-height 120
       :mode-line-font-height 130)
      (:fixed-font-height 146
       :variable-font-height 146
       :line-number-font-height 126
       :mode-line-font-height 136)))

  ;; Create faces used in `consult-imenu-config' as extension points from
  ;; the default faces used in programming language buffers. Use C-u C-x =
  ;; to discover the font face under point.
  (defface my/imenu-category-constant-face
    '((t :inherit font-lock-constant-face))
    "Face for displaying constant symbols in imenu."
    :group 'my/imenu-faces)

  (defface my/imenu-category-enum-face
    '((t :inherit font-lock-type-face))
    "Face for displaying enumeration symbols in imenu."
    :group 'my/imenu-faces)

  (defface my/imenu-category-function-face
    '((t :inherit font-lock-function-name-face))
    "Face for displaying function symbols in imenu."
    :group 'my/imenu-faces)

  (defface my/imenu-category-impl-face
    '((t :inherit font-lock-type-face))
    "Face for displaying implementation symbols in imenu."
    :group 'my/imenu-faces)

  (defface my/imenu-category-package-face
    '((t :inherit font-lock-constant-face))
    "Face for displaying package symbols in imenu."
    :group 'my/imenu-faces)

  (defface my/imenu-category-macro-face
    '((t :inherit font-lock-preprocessor-face))
    "Face for displaying macro symbols in imenu."
    :group 'my/imenu-faces)

  (defface my/imenu-category-method-face
    '((t :inherit font-lock-function-name-face))
    "Face for displaying method symbols in imenu."
    :group 'my/imenu-faces)

  (defface my/imenu-category-module-face
    '((t :inherit font-lock-constant-face))
    "Face for displaying module symbols in imenu."
    :group 'my/imenu-faces)

  (defface my/imenu-category-static-face
    '((t :inherit font-lock-constant-face))
    "Face for displaying static symbols in imenu."
    :group 'my/imenu-faces)

  (defface my/imenu-category-struct-face
    '((t :inherit font-lock-type-face))
    "Face for displaying struct symbols in imenu."
    :group 'my/imenu-faces)

  (defface my/imenu-category-trait-face
    '((t :inherit font-lock-type-face))
    "Face for displaying trait/interface symbols in imenu."
    :group 'my/imenu-faces)

  (defface my/imenu-category-type-face
    '((t :inherit font-lock-type-face))
    "Face for displaying type-centric symbols in imenu."
    :group 'my/imenu-faces)

  (defface my/imenu-category-variable-face
    '((t :inherit font-lock-variable-name-face))
    "Face for displaying variable symbols in imenu."
    :group 'my/imenu-faces)

  (defun my/apply-font-config (&optional index)
    "Apply the INDEX'th font configuration from `my/font-configs'."
    (let* ((index (or index my/font-config-index))
           (config (nth index my/font-configs))
           (fixed-font-height (plist-get config :fixed-font-height))
           (variable-font-height (plist-get config :variable-font-height))
           (line-number-font-height (plist-get config :line-number-font-height))
           (mode-line-font-height (plist-get config :mode-line-font-height)))

      (dolist (face '(default fixed-pitch))
        (set-face-attribute face nil
                            :font my/fixed-font
                            :height fixed-font-height))
      (dolist (face '(variable-pitch))
        (set-face-attribute face nil
                            :font my/variable-font
                            :height variable-font-height))
      (dolist (face '(mode-line mode-line-inactive))
        (set-face-attribute face nil
                            :font my/fixed-font
                            :height mode-line-font-height))
      (dolist (face '(line-number line-number-current-line))
        (set-face-attribute face nil
                            :font my/fixed-font
                            :height line-number-font-height
                            :slant 'italic))

      ;; Make the inlay face a bit bigger and italic.
      (with-eval-after-load 'eglot
        (set-face-attribute 'eglot-inlay-hint-face nil
                            :height 0.9
                            :slant 'italic))

      ;; Use fixed pitch for appropriate org elements (use C-u C-x = to
      ;; determine the font face of the character under point). Note that
      ;; `modus-themes-mixed-fonts' can also be used to achieve this.
      (require 'org-faces)
      (dolist (face '(org-block org-table))
        (set-face-attribute face nil :inherit 'fixed-pitch))))

  (defun my/cycle-font-config ()
    "Cycle to the next font configuration."
    (interactive)
    (setq my/font-config-index (mod (+ my/font-config-index 1)
                                    (length my/font-configs)))
    (my/apply-font-config my/font-config-index)))

;;;;; Icons

(use-package nerd-icons
  :commands nerd-icons-install-fonts
  :init
  ;; Install fonts if they are not already installed.
  (unless (member "Symbols Nerd Font Mono" (font-family-list))
    (nerd-icons-install-fonts t)))

;; Package-specific Nerd icon packages (e.g. `nerd-icons-dired') are grouped
;; with the related package.

;;;;; Mode Line

(use-package doom-modeline
  :custom
  ;; Mode line height is determined by the smaller of `doom-modeline-height'
  ;; and the mode line font. The function `doom-modeline--font-height' can be
  ;; called to determine the font height that will be used to calculate the
  ;; height of the mode line.
  (doom-modeline-mode t)
  (doom-modeline-buffer-file-name-style 'relative-from-project)
  (doom-modeline-lsp t)
  (doom-modeline-height 20)
  (doom-modeline-bar-width 4)
  (doom-modeline-column-zero-based t)
  (doom-modeline-total-line-number t)
  (doom-modeline-percent-position nil)
  (doom-modeline-window-width-limit nil)
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-buffer-state-icon nil)
  (doom-modeline-minor-modes nil)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-time-icon nil)
  :custom-face
  ;; Make the time text stand out a bit more.
  (doom-modeline-time ((t (:inherit doom-modeline-evil-insert-state)))))

;;;;; Point/Cursor

(use-package cursory
  :commands cursory-set-preset
  :hook
  (org-mode . (lambda () (cursory-set-preset 'bar :local)))
  :init
  (cursory-set-preset 'box))

(use-package pulsar
  :commands pulsar-pulse-line
  :custom
  (pulsar-global-mode t)
  (pulsar-delay 0.05)
  (pulsar-iterations 15)
  (pulsar-face 'pulsar-cyan)
  :config
  (add-to-list 'pulsar-pulse-functions #'avy-goto-char-timer)
  (add-to-list 'pulsar-pulse-functions #'avy-goto-line)
  (add-to-list 'pulsar-pulse-functions #'avy-goto-end-of-line))

;;;;; Margins

(use-package olivetti
  :general
  (general-def
    "<f7>" #'olivetti-mode)
  :custom
  (olivetti-body-width 90)
  (olivetti-style 'fancy))

;;;;; Scrolling

(use-package pixel-scroll
  :elpaca nil
  :general
  (general-def
    [remap scroll-up-command] #'pixel-scroll-interpolate-down
    [remap scroll-down-command] #' pixel-scroll-interpolate-up
    "M-<up>" #'my/pixel-scroll-partial-up
    "M-<down>" #'my/pixel-scroll-partial-down)
  :custom
  (pixel-scroll-precision-mode t)
  (pixel-scroll-precision-interpolate-page t)
  :config
  (defvar my/pixel-scroll-factor 0.25)

  (defun my/pixel-scroll-page (&optional factor)
    "Scroll up by the page's height multiplied by FACTOR."
    (let* ((factor (or factor 1.0))
           (window-height (window-text-height nil t))
           (delta (* window-height factor)))
      (pixel-scroll-precision-interpolate delta nil 1)))

  (defun my/pixel-scroll-partial-up ()
    "Scroll up by `my/pixel-scroll-factor' times the page height."
    (interactive)
    (my/pixel-scroll-page my/pixel-scroll-factor))

  (defun my/pixel-scroll-partial-down ()
    "Scroll down by `my/pixel-scroll-factor' times the page height."
    (interactive)
    (my/pixel-scroll-page (- my/pixel-scroll-factor))))

;;;; Window Management

;;;;; General Window Movement and Commands

(use-package window
  ;; Load after winner to override its default repeat map.
  :after winner
  :elpaca nil
  :functions my/repeatize
  :general
  (general-def
    "M-q" #'bury-buffer)
  (general-def "M-o"
    '(:keymap my/window-map))

  :custom
  ;; The following provides the default `display-buffer' behaviour for buffers
  ;; that are not managed by either Popper or Shackle.
  (display-buffer-base-action '((display-buffer-reuse-mode-window
                                 display-buffer-reuse-window
                                 display-buffer-same-window)))
  (even-window-sizes nil)

  :config
  (defvar-keymap my/window-map
    :doc "Keymap for window commands."
    "=" #'balance-windows
    ">" #'rotate-frame-clockwise
    "<" #'rotate-frame-anticlockwise
    "<left>" #'shrink-window-horizontally
    "<right>" #'enlarge-window-horizontally
    "<up>" #'enlarge-window
    "<down>" #'shrink-window
    "0" #'delete-window
    "1" #'delete-other-windows
    "2" #'split-window-below
    "3" #'split-window-right
    "4" #'flop-frame
    "5" #'flip-frame
    "o" #'other-window
    "u" #'winner-undo
    "r" #'winner-redo
    "f" #'windmove-right
    "b" #'windmove-left
    "p" #'windmove-up
    "n" #'windmove-down
    "F" #'windmove-swap-states-right
    "B" #'windmove-swap-states-left
    "P" #'windmove-swap-states-up
    "N" #'windmove-swap-states-down)

  ;; Make all window commands repeatable.
  (my/repeatize 'my/window-map))

;; Brings in useful functions such as `transpose-frame', `flip-frame', etc.
;; See: https://www.emacswiki.org/emacs/TransposeFrame.
(use-package transpose-frame)

;;;;; Window History

(use-package winner
  :elpaca nil
  :custom
  (winner-dont-bind-my-keys t)
  :init
  ;; Needs to be enabled via a function call rather than a customization
  ;; so that `winner-dont-bind-my-keys' takes effect.
  (winner-mode 1))

;;;;; Tab Bar

(use-package tab-bar
  :elpaca nil
  :custom
  (tab-bar-show nil))

;;;;; Window Orchestration

;; Use Shackle for managing windows that aren't considered to be "popups" by
;; Popper. Shackle basically replaces the manual configuration of
;; `display-buffer-base-action' and `display-buffer-alist' with a simpler
;; syntax. `shackle-mode' needs to be called before `popper-mode' so that the
;; latter gets priority in `display-buffer-alist'; that way, any buffer not
;; claimed by Popper will be subject to Shackle's rules.
(use-package shackle
  :custom
  (shackle-mode t)
  (shackle-default-rule nil)
  (shackle-rules
   '(("\\*eldoc" :select nil :other t :regexp t)
     ("\\*eshell-output\\*" :select t :other t :regexp t)
     ("\\*helpful" :select t :other t :regexp t)
     ("\\*rg\\*" :select t :other t :regexp t)
     ("\\*Occur\\*" :select t :other t :regexp t)
     ("\\*Pp" :select nil :other t :regexp t)
     ("^magit-diff:" :select nil :other t :regexp t))))

(use-package popper
  :after shackle
  :commands
  (popper-mode popper-echo-mode popper-kill-latest-popup popper-open-latest)
  :general
  (general-def
    "C-'" #'popper-toggle
    "M-'" #'popper-cycle
    "C-M-'" #'popper-toggle-type)
  (general-def 'popper-mode-map
    "M-k" #'my/popper-kill-popup-stay-open
    "M-K" #'popper-kill-latest-popup)

  :preface
  (defvar my/popper-ignore-modes '(grep-mode rg-mode))

  :custom
  (popper-mode t)
  (popper-echo-mode t)
  (popper-window-height 15)
  (popper-reference-buffers
   '("CAPTURE-.*\\.org"
     "\\*Messages\\*"
     "\\*Warnings\\*"
     "\\*Backtrace\\*"
     "\\*Breakpoints\\*"
     "\\*Flymake"
     "\\*Call Hierarchy\\*"
     "\\*Shell Command Output\\*"
     "\\*Async Shell Command\\*"
     "\\*apheleia-"

     ;; Match all modes that derive from compilation-mode but do not derive
     ;; from a member of `my/popper-ignore-modes'.
     (lambda (buf)
       (with-current-buffer buf
         (unless (apply #'derived-mode-p my/popper-ignore-modes)
           (derived-mode-p 'compilation-mode))))))

  (popper-echo-dispatch-keys '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))

  ;; Taken from: https://github.com/seagle0128/.emacs.d/blob/8cbec0c132cd6de06a8c293598a720d377f3f5b9/lisp/init-window.el#L148.
  (popper-mode-line
   '(:eval (let ((face (if (doom-modeline--active)
                           'mode-line-emphasis
                         'mode-line-inactive)))
             (format " %s " (nerd-icons-octicon "nf-oct-pin" :face face)))))

  :config
  (defun my/popper-kill-popup-stay-open ()
    "Kill the current popup but stay open if there are others."
    (interactive)
    (popper-kill-latest-popup)
    (popper-open-latest)))

;;;; Help System

(use-package help-fns
  :elpaca nil
  :general
  (general-def
    "C-h F" #'describe-face))

(use-package helpful
  :general
  (general-def
    "C-h c" #'helpful-callable
    ;; Replace `describe-*' bindings with Helpful where possible.
    [remap describe-function] #'helpful-function
    [remap describe-symbol] #'helpful-symbol
    [remap describe-variable] #'helpful-variable
    [remap describe-command] #'helpful-command
    [remap describe-key] #'helpful-key)
  (general-def '(emacs-lisp-mode-map lisp-interaction-mode-map)
    ;; Replace `display-local-help' with Helpful.
    "C-h ." #'helpful-at-point)

  :custom
  ;; Required so that I can tell Shackle NOT to select Helpful buffers.
  (helpful-switch-buffer-function #'display-buffer))

(use-package which-key
  :custom
  (which-key-mode t)
  ;; Use Embark for `prefix-help-command' as it is searchable.
  (which-key-show-early-on-C-h nil)
  (which-key-use-C-h-commands nil)
  (which-key-idle-delay 1.0)
  (which-key-idle-secondary-delay 0.05)
  (which-key-popup-type 'side-window)
  (which-key-side-window-location '(bottom right)))

;;;; General Editing

;;;;; Text Substitution

(use-package substitute
  :commands substitute-report-operation
  :general
  (general-def
    "M-# s" #'substitute-target-below-point
    "M-# r" #'substitute-target-above-point
    "M-# d" #'substitute-target-in-defun
    "M-# b" #'substitute-target-in-buffer)
  :config
  (add-to-list 'substitute-post-replace-functions #'substitute-report-operation))

;;;;; Undo/Redo

(use-package undo-tree
  :custom
  (global-undo-tree-mode t)
  (undo-tree-auto-save-history t)
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-diff t))

;;;;; Region Expansion

(use-package expreg
  :elpaca (:host github :repo "casouri/expreg")
  :general
  (general-def
    "C-=" #'expreg-expand
    "C-+" #'expreg-contract))

;;;;; Whitespace

(use-package ws-butler
  :hook
  ((text-mode prog-mode) . ws-butler-mode)
  :custom
  (ws-butler-keep-whitespace-before-point nil))

;;;;; Special Characters

(use-package iso-transl
  :elpaca nil
  :general
  ;; Bind C-x 8 0 to insert a zero-width space character which can be used to
  ;; escape org mode emphasis markers.
  ;; See: https://emacs.stackexchange.com/questions/16688/how-can-i-escape-the-in-org-mode-to-prevent-bold-fontification.
  (general-def 'iso-transl-ctl-x-8-map
    "0" [?​]))

;;;;; Point Jumping

(use-package avy
  :general
  (general-unbind "M-j")
  (general-def :prefix "M-j"
    "j" #'avy-goto-char-timer
    "l" #'avy-goto-line
    "L" #'avy-goto-end-of-line
    "y" #'avy-copy-line
    "Y" #'avy-copy-region
    "k" #'avy-kill-whole-line
    "K" #'avy-kill-region
    "w" #'avy-kill-ring-save-whole-line
    "W" #'avy-kill-ring-save-region
    "m" #'avy-move-line
    "M" #'avy-move-region)
  :custom
  (avy-single-candidate-jump t)
  (avy-timeout-seconds 0.4)
  (avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package isearch
  :elpaca nil
  :general
  (my/bind-search
    "." #'isearch-forward-thing-at-point)
  (general-def 'isearch-mode-map
    ;; The default `isearch-abort' requires multiple C-g if search not found.
    "C-g" #'isearch-cancel))

;;;;; Highlighting

(use-package hi-lock
  :elpaca nil
  :general
  (my/bind-search
    "h." #'highlight-symbol-at-point
    "hh" #'highlight-regexp
    "hl" #'highlight-lines-matching-regexp
    "hu" #'unhighlight-regexp))

;;;;; Templating

(use-package tempel
  :commands tempel-complete
  :general
  (general-def
    "M-+" #'tempel-insert)
  ;; Keymap used by navigating across template fields.
  (general-def 'tempel-map
    "<tab>" #'tempel-next
    "S-<tab>" #'tempel-previous
    [remap keyboard-quit] #'tempel-done)

  :custom
  ;; Tempel completions will only appear when prefixed with "<". The function
  ;; `tempel-complete' should be added to `completion-at-point-functions' of
  ;; relevant modes to facilitate this.
  (tempel-trigger-prefix "<")
  (tempel-path (no-littering-expand-etc-file-name "tempel/templates")))

;;;; Buffer Management

(use-package ibuffer
  :elpaca nil
  :general
  (my/bind-c-x
    ;; Replace `list-buffers' with `ibuffer'.
    "C-b" #'ibuffer)
  (general-def 'ibuffer-mode-map
    "M-o" nil
    ;; Group buffers by VC project.
    "/p" #'ibuffer-vc-set-filter-groups-by-vc-root))

(use-package ibuffer-vc)

(use-package nerd-icons-ibuffer
  :hook
  (ibuffer-mode . nerd-icons-ibuffer-mode))

;;;; File System

;;;;; File Browsing

(use-package dired
  :elpaca nil
  :general
  ;; Allow `dired-goto-file' (via j) straight after jumping with C-x C-j.
  ;; Without this, repeat mode takes over and j calls `dired-jump' again.
  (general-unbind 'dired-jump-map "j")
  (general-def 'dired-mode-map
    "N" #'dired-create-empty-file
    "?" #'which-key-show-major-mode
    "i" #'dired-subtree-insert
    ";" #'dired-subtree-remove
    "M-s" nil)
  :hook
  (dired-mode . auto-revert-mode)
  :custom
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (delete-by-moving-to-trash t)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-free-space nil)
  (dired-listing-switches "-al --group-directories-first"))

(use-package dired-subtree)

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(use-package dired-rainbow
  :config
  ;; Dired face highlighting by file extension.
  (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
  (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
  (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
  (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
  (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
  (dired-rainbow-define media "#de751f" ("mp3" "mp4" "mkv" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
  (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
  (dired-rainbow-define log "#c17d11" ("log"))
  (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
  (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js" "ts" "tf" "hcl" "bazel"))
  (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" "java"))
  (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
  (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
  (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
  (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
  (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
  (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
  (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))

  ;; Dired face highlighting by file permissions.
  (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
  (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*"))

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

;;;;; File History

(use-package recentf
  :elpaca nil
  :custom
  (recentf-mode t)
  (recentf-max-saved-items 300))

;;;;; Project Management

(use-package project
  :elpaca nil
  :general
  (my/bind-c-x
    "pj" #'project-dired
    "pu" #'my/project-update-list)

  :custom
  (project-prompt-project-dir)
  (project-switch-commands
   '((my/consult-project-multi "File" ?f)
     (project-dired "Dired" ?j)
     (consult-ripgrep "Ripgrep" ?s)
     (magit-project-status "Magit" ?m)
     (project-eshell "Eshell" ?e)
     (my/project-vterm "Vterm" ?v)
     (project-async-shell-command "Async shell" ?&)))

  :config
  (defun my/project-current-root ()
    "Return the root directory of the current or nil."
    (if-let* ((proj (project-current)))
        (expand-file-name (project-root proj))))

  (defun my/project-update-list ()
    "Update list of known projects."
    (interactive)
    (project-forget-zombie-projects)
    (my/project-remember-projects-under '("~/dev/home" "~/dev/work")))

  (defun my/project-remember-projects-under (dirs)
    "Remember all projects one level under each of DIRS."
    (let ((found 0)
          (files (mapcan (lambda (dir) (directory-files dir t)) dirs)))
      (dolist (file files)
        (when (and (file-directory-p file)
                   (not (member file '("." ".."))))
          (setq found (+ found
                         (project-remember-projects-under file)))))
      (message "Found %d new projects" found))))

;;;;; Auto-Save

(use-package super-save
  :commands super-save-mode
  :custom
  ;; Disable built-in `auto-save-mode' as this replaces it.
  (auto-save-default nil)
  (super-save-mode t)
  (super-save-auto-save-when-idle t)
  (super-save-idle-duration 30)
  (super-save-max-buffer-size 100000))

;;;; Completion System

(use-package corfu
  ;; Calling `global-corfu-mode' below invokes `eldoc-add-command' internally
  ;; and because I'm pulling a later version of Eldoc, Corfu needs to be
  ;; configured after Eldoc has been loaded.
  :after eldoc
  :elpaca (:files (:defaults "extensions/*.el"))
  :commands (corfu-mode global-corfu-mode)
  :functions consult-completion-in-region

  :general
  (general-def 'corfu-map
    ;; We S-SPC to keep completion going and include the space.
    "S-SPC" #'corfu-insert-separator
    ;; Move the completion session to the minibuffer.
    "M-m" #'my/corfu-move-to-minibuffer)

  :hook
  (minibuffer-setup . my/corfu-enable-in-minibuffer)

  :custom
  ;; Show the Corfu pop-up without requiring tab to be pressed (but after the
  ;; delay configured below).
  (corfu-auto t)
  ;; Number of typed characters before Corfu will display its pop-up.
  (corfu-auto-prefix 3)
  ;; Number of seconds of inactivity before the Corfu pop-up is displayed. This
  ;; setting only applies after the minimum number of prefix characters have
  ;; been entered. This is really useful to keep so that short words that you
  ;; don't want autocompleted don't trigger the Corfu pop-up (and subsequent
  ;; completion which inserts a space after the completed word).
  (corfu-auto-delay 0.2)
  ;; Maximum number of completion candidates.
  (corfu-count 16)
  ;; Automatically select the first candidate if it does not reference a
  ;; directory. This makes the eshell experience a little more natural.
  (corfu-preselect 'directory)
  ;; Modes which shouldn't use Corfu as I found the completions annoying.
  (corfu-excluded-modes
   '(bazel-build-mode
     bazel-workspace-mode
     bazel-starlark-mode))

  :init
  ;; Enable Corfu mode globally by default. Exclusions are captured
  ;; individually in `corfu-excluded-modes'.
  (global-corfu-mode)

  ;; From: https://github.com/minad/corfu#completing-in-the-minibuffer.
  (defun my/corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if appropriate."
    (when (local-variable-p 'completion-at-point-functions)
      (setq-local corfu-auto nil)
      (corfu-mode 1)))

  ;; From: https://github.com/minad/corfu#transfer-completion-to-the-minibuffer.
  (defun my/corfu-move-to-minibuffer ()
    "Transfer the Corfu completion session to the minibuffer."
    (interactive)
    (when completion-in-region--data
      (let ((completion-extra-properties corfu--extra)
            completion-cycle-threshold completion-cycling)
        (apply #'consult-completion-in-region completion-in-region--data)))))

(use-package corfu-popupinfo
  :after corfu
  :elpaca nil
  :general
  (general-def 'corfu-map
    "M-t" #'corfu-popupinfo-toggle)
  (general-def 'corfu-popupinfo-map
    "M-d" #'corfu-popupinfo-documentation
    ;; Unfortunately, Eglot doesn't seem to support :company-location which
    ;; is what `corfu-popupinfo' uses to determine the code location for the
    ;; candidate. It does work for Elisp, however.
    "M-l" #'corfu-popupinfo-location
    "M-p" #'corfu-popupinfo-scroll-down
    "M-n" #'corfu-popupinfo-scroll-up)
  :hook
  (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay 1.0)
  (corfu-popupinfo-max-height 16))

(use-package corfu-quick
  :after corfu
  :elpaca nil
  :general
  (general-def 'corfu-map
    "'" #'corfu-quick-complete)
  :custom
  (corfu-quick1 "asdfghjkl")
  (corfu-quick2 "asdfghjkl"))

(use-package corfu-history
  :after corfu
  :elpaca nil
  :custom
  (corfu-history-mode t)
  :init
  ;; Persist Corfu history between Emacs sessions.
  (add-to-list 'savehist-additional-variables 'corfu-history))

;; Icons used by Corfu's popup.
(use-package nerd-icons-corfu
  :after corfu
  :commands nerd-icons-corfu-formatter
  :defines corfu-margin-formatters
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; Vertico provides the vertical completion minibuffer and Orderless provides
;; the "completion style". Some commands that make use of Vertico's selection
;; list also allow a new non-matched value to be entered. For example,
;; `org-roam-node-insert' will create a new note when given a new note name.
;; However, if the new value matches part of an existing value in the selection
;; list (which is more likely when using Orderless) then you will need to press
;; M-RET which calls `vertico-exit-input' to cancel the completion and use the
;; new value.
(use-package vertico
  :elpaca (:files (:defaults "extensions/*.el"))
  :custom
  (vertico-mode t)
  (vertico-count 20))

(use-package vertico-directory
  :after vertico
  :elpaca nil
  :general
  (general-def 'vertico-map
    ;; More convenient directory navigation commands.
    "RET" #'vertico-directory-enter
    "DEL" #'vertico-directory-delete-char)
  ;; Tidy shadowed file names.
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-multiform
  :after vertico
  :elpaca nil
  :custom
  (vertico-multiform-mode t)
  (vertico-multiform-categories
   '((imenu buffer)
     (file (vertico-sort-function . my/vertico-sort-dirs-first))))

  ;; Sometimes commands are better when the category is too broad.
  (vertico-multiform-commands
   '((consult-outline buffer)))

  :config
  (defun my/vertico-sort-dirs-first (files)
    "Sorts FILES by directories then alphanumerically."
    (setq files (vertico-sort-history-length-alpha files))
    (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
           (seq-remove (lambda (x) (string-suffix-p "/" x)) files))))

;; I don't enable `vertico-buffer-mode' directly since this makes Vertico
;; always run in a buffer. Instead, `vertico-multiform' is used to toggle
;; buffer display on a per-category or per-command basis. The configuration
;; of `vertico-buffer-display-action' below changes the default way that
;; Vertico buffers are shown (otherwise they reuse the current window).
(use-package vertico-buffer
  :after vertico
  :elpaca nil
  :custom
  (vertico-buffer-display-action
   '(display-buffer-in-direction
     ;; Below results in the search buffer being displayed to the right of the
     ;; current window with both the windows being shown at 50% width. Trying
     ;; to control the % via (window-width . 0.3) seems to set the width based
     ;; on the frame width rather than the current window which makes the
     ;; search window too large when the frame is split vertically into
     ;; multiple windows. So 50% is fine for now.
     (direction . right))))

(use-package vertico-repeat
  :after vertico
  :elpaca nil
  :hook (minibuffer-setup . vertico-repeat-save)
  :general
  (my/bind-c-c
    "'" #'vertico-repeat-select)
  :init
  ;; Persist Vertico history between Emacs sessions.
  (add-to-list 'savehist-additional-variables 'vertico-repeat-history))

(use-package vertico-quick
  :after vertico
  :elpaca nil
  :general
  (general-def 'vertico-map
    "'" #'vertico-quick-exit)
  :custom
  (vertico-quick1 "asdfghjkl")
  (vertico-quick2 "asdfghjkl"))

;; Enables Vertico icons.
(use-package nerd-icons-completion
  ;; For some reason, this only seems to work when `nerd-icons-completion-mode.'
  ;; is called very late in the startup cycle.
  :hook (elpaca-after-init . nerd-icons-completion-mode))

;; Dedicated completion commands.
(use-package cape
  :general
  (my/bind-c-c
    "h" #'cape-history)

  (my/bind-c-c
    "cd" #'cape-dabbrev
    "ch" #'cape-history
    "cf" #'cape-file
    "ck" #'cape-keyword
    "co" #'cape-elisp-symbol
    "ca" #'cape-abbrev
    "cl" #'cape-line
    "cw" #'cape-dict)

  :custom
  ;; Only show dabbrev candidates of a minimum length to avoid being annoying.
  (cape-dabbrev-min-length 5)
  ;; Only show dabbrev completions for words in the current buffer.
  (cape-dabbrev-check-other-buffers nil))

;; Orderless configuration mostly taken from:
;; https://github.com/minad/corfu/wiki#basic-example-configuration-with-orderless.
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  ;; While it's possible to set `completion-category-overrides' to nil to go
  ;; all in on Orderless, it's recommended in the Vertico README to enable
  ;; `partial-completion' for files so that commands like `find-file' work
  ;; properly with wildcards (e.g. opening multiple files - need to use M-RET
  ;; to submit, however).
  (completion-category-overrides '((file (styles partial-completion))))
  ;; Allow backslash to escape the space to search for literal spaces.
  (orderless-component-separator #'orderless-escapable-split-on-space))

(use-package marginalia
  :commands marginalia-mode
  :custom
  (marginalia-mode t))

(use-package consult
  :defines
  (consult-imenu-config
   my/default-theme
   xref-show-xrefs-function
   xref-show-definitions-function)

  :preface
  ;; For some reason, declaring some of these functions under :functions
  ;; doesn't satisfy Flymake so they are declared here instead.
  (declare-function consult--buffer-query "consult")
  (declare-function consult--buffer-state "consult")
  (declare-function consult--customize-put "consult")
  (declare-function consult--file-action "consult")
  (declare-function consult--file-state "consult")
  (declare-function consult-register-format "consult")
  (declare-function consult-register-window "consult")
  (declare-function consult-theme "consult")
  (declare-function consult-xref "consult")
  (declare-function my/project-current-root nil)
  (declare-function project--find-in-directory "project")
  (declare-function project-files "project")

  :hook
  (elpaca-after-init . (lambda () (consult-theme my/default-theme)))

  :general
  (general-unbind 'minibuffer-local-map "M-s")

  (general-def
    "M-i" #'consult-imenu
    "M-y" #'consult-yank-pop)

  (general-def 'consult-narrow-map
    "C-<" #'consult-narrow-help
    ;; Remove if this becomes annoying due to needing a '?' character.
    "?" #'consult-narrow-help)

  (my/bind-search 'isearch-mode-map
    ;; Allow `consult-line' functions to "take over" from `isearch'.
    ;; TODO: The reverse would be better as I could search by line then within.
    "l" #'consult-line
    "L" #'consult-line-multi)

  (my/bind-search
    "a" #'consult-org-agenda
    "f" #'consult-fd
    "l" #'consult-line
    "L" #'consult-line-multi
    "p" #'my/consult-project-file
    "s" #'consult-ripgrep
    "=" #'consult-focus-lines)

  (my/bind-goto
    "-" #'consult-outline
    "f" #'consult-flymake
    "g" #'consult-goto-line
    "M-g" #'consult-goto-line
    "i" #'consult-imenu
    "I" #'consult-imenu-multi
    "m" #'consult-mark
    "M" #'consult-global-mark)

  (my/bind-c-c
    "xt" #'consult-theme)

  (my/bind-c-c 'flymake-mode-map
    "ff" #'consult-flymake)

  (my/bind-c-c 'isearch-mode-map
    ;; Use standard history keybinding for `consult-isearch-history' and
    ;; keep `M-e' as the default keybinding for `isearch-edit-string'.
    "h" #'consult-isearch-history)

  (my/bind-c-c 'minibuffer-local-map
    "h" #'consult-history)

  (my/bind-c-x
    "b" #'consult-buffer
    "4b" #'consult-buffer-other-window
    "5b" #'consult-buffer-other-frame
    "tb" #'consult-buffer-other-tab
    "pb" #'consult-project-buffer
    "pf" #'my/consult-project-file
    "rb" #'consult-bookmark
    "rr" #'consult-register
    "rs" #'consult-register-store
    "C-k k" #'consult-kmacro)

  :custom
  ;; Type < followed by a prefix key to narrow the available candidates.
  ;; Type C-< (defined above) to display prefix help. Alternatively, type
  ;; < followed by C-h (or ?) to make `embark-prefix-help-command' kick in
  ;; and display a completing prefix help.
  (consult-narrow-key "<")

  ;; Auto-preview by default.
  (consult-preview-key 'any)

  ;; Tell `consult-ripgrep' to search hidden dirs/files but ignore .git/.
  (consult-ripgrep-args
   '("rg"
     "--null"
     "--line-buffered"
     "--color=never"
     "--max-columns=1000"
     "--path-separator=/"
     "--smart-case"
     "--no-heading"
     "--with-filename"
     "--line-number"
     "--search-zip"
     "--hidden"
     "--glob=!.git/"))

  ;; Configure both `config-find' and `consult-fd' to follow, symlinks, include
  ;; hidden files, and ignore the .git directory. The fd command needs to be
  ;; specifically told to allow matching across the full path (e.g. so you
  ;; can search for "src/foo"). In general, I prefer `consult-fd' as it obeys
  ;; the .gitignore file if present.
  (consult-find-args "find -L . -not ( -name .git -type d -prune )")
  (consult-fd-args "fd -p -L -H -E .git/*")

  ;; Only show Modus and Ef themes.
  (consult-themes '("^modus-" "^ef-"))

  :init
  ;; Configure how registers are previewed and displayed.
  ;; See: https://github.com/minad/consult#use-package-example.
  (setq register-preview-delay 0)
  (setq register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use consult to select xref locations with preview.
  (with-eval-after-load 'xref
    (setq xref-show-xrefs-function #'consult-xref)
    (setq xref-show-definitions-function #'consult-xref))

  :config
  (defconst my/preview-key '(:debounce 0.3 any))

  (defvar my/consult-source-dired-buffer
    `(:name "Dired Buffer"
      :narrow ?d
      :hidden t
      :category buffer
      :face consult-buffer
      :history buffer-name-history
      :state ,#'consult--buffer-state
      :items
      ,(lambda ()
         (consult--buffer-query
          :sort 'visibility
          :as #'buffer-name
          :mode 'dired-mode))))

  (defvar my/consult-source-shell-buffer
    `(:name "Shell Buffer"
      :narrow ?s
      :hidden t
      :category buffer
      :face consult-buffer
      :history buffer-name-history
      :state ,#'consult--buffer-state
      :items
      ,(lambda ()
         (consult--buffer-query
          :sort 'visibility
          :as #'buffer-name
          :predicate
          (lambda (buf)
            (with-current-buffer buf
              (derived-mode-p 'eshell-mode 'vterm-mode)))))))

  ;; Consult source for all project files. This has largely been adapted from
  ;; the implementation of `consult--source-project-recent-file'.
  (defvar my/consult-source-project-file
    `(:name "Project File"
      :narrow ?f
      :preview-key ,my/preview-key
      :category file
      :face consult-file
      :history file-name-history
      :state ,#'consult--file-state
      :new
      ,(lambda (file)
         (consult--file-action
          (expand-file-name file (my/project-current-root))))
      :enabled
      ,(lambda () consult-project-function)
      :items
      ,(lambda ()
         (when-let (project-dir (my/project-current-root))
           (let* ((project (project--find-in-directory project-dir))
                  (project-files (seq-filter #'file-exists-p (project-files project)))
                  (len (length project-dir))
                  items)
             (dolist (file project-files (nreverse items))
               (setq file (expand-file-name file))
               (let ((part (substring file len)))
                 (when (equal part "") (setq part "./"))
                 (put-text-property 0 1 'multi-category `(file . ,file) part)
                 (push part items))))))))

  ;; Customize the list of sources shown by `consult-buffer'.
  (setq consult-buffer-sources
        '(consult--source-buffer                ;; Narrow: ?b (shown)
          consult--source-project-buffer-hidden ;; Narrow: ?p (hidden)
          my/consult-source-dired-buffer        ;; Narrow: ?d (hidden)
          my/consult-source-shell-buffer        ;; Narrow: ?s (hidden)
          consult--source-file-register         ;; Narrow: ?g (shown)
          consult--source-bookmark              ;; Narrow: ?m (shown)
          consult--source-recent-file))         ;; Narrow: ?r (hidden)

  ;; Designed to replace `project-find-file' (which doesn't provide preview),
  ;; this command only shows project files by default but can show project
  ;; buffers and project recent files if requested.
  (defun my/consult-project-file ()
    "Version of `project-find-file' that uses Consult sources."
    (interactive)
    (let ((consult-project-buffer-sources
           '(consult--source-project-buffer-hidden      ;; Narrow: ?p (hidden)
             consult--source-project-recent-file-hidden ;; Narrow: ?r (hidden)
             my/consult-source-project-file)))          ;; Narrow: ?f (shown)
      (consult-project-buffer)))

  ;; Designed to be used in `project-switch-commands', this command provides
  ;; quick insight into: what is open, what was open recently, and what is
  ;; available for a given project.
  (defun my/consult-project-multi ()
    "Multi-source project finder to be used in `project-switch-commands'."
    (interactive)
    (let ((consult-project-buffer-sources
           '(consult--source-project-buffer      ;; Narrow: ?p (shown)
             consult--source-project-recent-file ;; Narrow: ?r (shown)
             my/consult-source-project-file)))   ;; Narrow: ?f (shown)
      (consult-project-buffer)))

  ;; Customize individual Consult sources.
  (consult-customize
   consult--source-buffer
   :name "Open Buffer" :narrow ?b
   consult--source-project-buffer
   consult--source-project-buffer-hidden
   :name "Project Buffer" :narrow ?p
   consult--source-file-register
   :name "Register" :narrow ?g :preview-key my/preview-key
   ;; Due to the value of `consult-preview-key' configured above, the preview
   ;; will be displayed immediately for most commands. This is generally fine,
   ;; but for commands that access unopened files I prefer to delay the preview
   ;; so I can skip past candidates without incurring the preview.
   consult--source-recent-file
   :name "Recent File" :narrow ?r :hidden t :preview-key my/preview-key
   consult--source-project-recent-file
   consult--source-project-recent-file-hidden
   :name "Recent Project File" :narrow ?r :preview-key my/preview-key
   ;; Configure delayed preview for file finding (disabled by default).
   consult-find consult-fd
   :state (consult--file-preview) :preview-key my/preview-key
   ;; Configure delayed preview for commands/sources that relate to
   ;; unopened files and that preview automatically by default.
   consult-ripgrep consult-grep consult-git-grep consult--source-bookmark
   xref-find-references xref-find-references
   :preview-key my/preview-key))

(use-package consult-dir
  :autoload my/find-subdirs
  :commands consult-dir
  :general
  (my/bind-search
    "d" #'consult-dir)
  :custom
  (consult-dir-shadow-filenames nil)
  (consult-dir-default-command #'find-file)

  :config
  (defun my/find-subdirs (&optional dir)
    "Return a list of all subdirectories of DIR (else `default-directory')."
    (when-let ((default-directory (or dir default-directory)))
      (let ((command "fd -L -H -E .git/ -t=d -a -0 -c=never")
            res)
        (with-temp-buffer
          (let ((status (process-file-shell-command command nil t))
                (pt (point-min)))
            (unless (zerop status)
              (error "Error running command '%s' in directory '%s'" command default-directory))
            (goto-char pt)
            (while (search-forward "\0" nil t)
              (push (buffer-substring-no-properties pt (1- (point)))
                    res)
              (setq pt (point)))))
        (mapcar #'abbreviate-file-name (nreverse res)))))

  (defvar my/consult-dir-source-local-subdir
    `(:name "Local Subdir"
      :narrow ?l
      :hidden t
      :category file
      :face consult-file
      :history file-name-history
      :enabled ,#'my/project-current-root
      :items ,(lambda () (my/find-subdirs))))

  (defvar my/consult-dir-source-project-subdir
    `(:name "Project Subdir"
      :narrow ?p
      :hidden t
      :category file
      :face consult-file
      :history file-name-history
      :enabled ,#'my/project-current-root
      :items ,(lambda () (my/find-subdirs (my/project-current-root)))))

  (consult-customize
   consult-dir--source-bookmark :name "Bookmark" :narrow ?m
   consult-dir--source-default :name "Current" :narrow ?.
   consult-dir--source-project :name "Project" :narrow ?P
   consult-dir--source-recentf :name "Recent" :narrow ?r :hidden t)

  (setq consult-dir-sources
        '(consult-dir--source-default
          my/consult-dir-source-local-subdir
          my/consult-dir-source-project-subdir
          consult-dir--source-bookmark
          consult-dir--source-project
          consult-dir--source-recentf)))

(use-package embark
  :commands embark-prefix-help-command
  :general
  (general-def
    "C-." #'embark-act
    "C-M-." #'embark-dwim
    "C-h b" #'embark-bindings)

  :custom
  ;; Just show the minimal "Act" prompt (the default starts with minimal
  ;; and then `embark-mixed-indicator-delay' kicks in and the verbose screen
  ;; is shown). Shortcut keys can immediately be used. C-h can be pressed to
  ;; bring up the completing read prompter.
  (embark-indicators (list #'embark-minimal-indicator))
  ;; Use this key to switch Embark to the keymap prompter.
  (embark-keymap-prompter-key ",")

  :init
  ;; Use Embark to prompt for and run commands under a specified prefix
  ;; when C-h is pressed (e.g. C-x C-h) rather than `describe-prefix-bindings'.
  (with-eval-after-load 'help
    ;; It should be possible to just set `prefix-help-command' to
    ;; `embark-prefix-help-command' as specified (and commented out below)
    ;; in the Embark documentation but somehow it keeps getting reverted to
    ;; `describe-prefix-bindings'. Even when I use `with-eval-after-load' to
    ;; set it after the help package is loaded, it will still get reverted. I
    ;; haven't been able to figure out what is reverting it so this hacks
    ;; `describe-prefix-bindings' to proxy `embark-prefix-help-command'.
    ;; (setq prefix-help-command #'embark-prefix-help-command)
    (defun describe-prefix-bindings ()
      (interactive)
      (embark-prefix-help-command)))

  :config
  ;; Adapt the associated commands so that they are usable as Embark actions.
  ;; If commands don't behave properly with Embark, play with this. Look at
  ;; similar commands already in `embark-target-injection-hooks' and mimic.
  (add-to-list 'embark-target-injection-hooks '(eglot-code-actions embark--ignore-target))
  (add-to-list 'embark-target-injection-hooks '(eglot-rename embark--ignore-target))
  (add-to-list 'embark-target-injection-hooks '(eglot-find-implementation embark--ignore-target))
  (add-to-list 'embark-target-injection-hooks '(consult-eglot-symbols embark--allow-edit))
  (add-to-list 'embark-target-injection-hooks '(query-replace embark--allow-edit))
  (add-to-list 'embark-target-injection-hooks '(query-replace-regexp embark--allow-edit))
  (add-to-list 'embark-target-injection-hooks '(my/query-replace-wrap embark--allow-edit))
  (add-to-list 'embark-target-injection-hooks '(my/query-replace-regexp-wrap embark--allow-edit)))

(use-package embark-consult)

(use-package rg
  :commands rg-menu
  :functions popper--bury-all
  :general
  (my/bind-search
    "M-s" #'my/rg-menu)

  :config
  (defun my/rg-menu ()
    "Bury any popups before calling `rg-menu'."
    (interactive)
    (popper--bury-all)
    (call-interactively #'rg-menu)))

(use-package wgrep
  :general
  (my/bind-c-c
    "w" #'wgrep-change-to-wgrep-mode)
  :custom
  (wgrep-auto-save-buffer t))

;;;; Programming

;;;;; General Programming

;;;;;; Treesitter

(use-package treesit
  :elpaca nil
  :custom
  (treesit-font-lock-level 4)
  (treesit-extra-load-path
   (list (no-littering-expand-var-file-name "treesit-auto"))))

(use-package treesit-auto
  :commands (treesit-auto-install-all treesit-auto-add-to-auto-mode-alist)
  :custom
  (global-treesit-auto-mode t)
  (treesit-auto-install 'prompt)
  ;; Use TS-powered modes for a smaller set of languages for now.
  ;; See original value of `treesit-auto-langs' for the full set.
  (treesit-auto-langs '(bash dockerfile go gomod proto python rust))

  :init
  (treesit-auto-add-to-auto-mode-alist treesit-auto-langs)
  ;; For now, to upgrade grammars, delete ~/.config/emacs/var/treesit-auto,
  ;; re-open Emacs, and then run `my/treesit-auto-install-all'.
  (defun my/treesit-auto-install-all ()
    "Wrapper around `treesit-auto-install-all' that respects no-littering."
    (interactive)
    (treesit-auto-install-all)
    (let ((old-dir (expand-file-name "tree-sitter"  user-emacs-directory))
          (new-dir (car treesit-extra-load-path)))
      (delete-directory new-dir t)
      (rename-file old-dir new-dir))))

;;;;;; Eglot

(use-package eglot
  ;; Use latest. See: https://www.reddit.com/r/emacs/comments/16yny40/how_to_use_the_latest_version_of_eglot_with_elpaca.
  :elpaca (:inherit elpaca-menu-gnu-devel-elpa)
  :commands
  (eglot-completion-at-point
   eglot--current-server-or-lose
   eglot--request
   eglot--TextDocumentPositionParams)
  :functions my/flymake-eldoc-show-first

  :hook
  ((go-mode
    go-ts-mode
    rust-mode
    rust-ts-mode
    sh-mode
    bash-ts-mode
    haskell-mode) . eglot-ensure)
  (eglot-managed-mode . my/eglot-init)

  :general
  (my/bind-goto 'eglot-mode-map
    "^" #'eglot-find-implementation)

  (my/bind-c-c 'eglot-mode-map
    "ra" #'eglot-code-actions
    "ro" #'eglot-code-action-organize-imports
    "rr" #'eglot-rename)

  :custom
  (eglot-autoshutdown t)
  (eglot-confirm-server-edits nil)

  :config
  (defun my/eglot-init ()
    "Init function for `eglot--managed-mode'."
    (eglot-inlay-hints-mode 1)
    (setq-local completion-at-point-functions
                (list
                 #'tempel-complete
                 #'eglot-completion-at-point
                 #'cape-file))
    (my/flymake-eldoc-show-first))

  (defun my/eglot-open-external-docs ()
    "Open external documentation for the symbol at point."
    ;; This currently only works with rust-analyzer and is buggy.
    (interactive)
    (let ((url (eglot--request (eglot--current-server-or-lose)
                               :experimental/externalDocs
                               `(,@(eglot--TextDocumentPositionParams)))))
      (browse-url url)))

  ;; Tree-sitter produces a better imenu.
  (setq eglot-stay-out-of '(imenu))

  ;; See: https://github.com/minad/corfu/wiki#filter-list-of-all-possible-completions-with-completion-style-like-orderless.
  (add-to-list 'completion-category-overrides '(eglot (styles orderless)))

  ;; Customize configuration of LSP servers via `eglot-server-programs' below.
  ;; Note that `:json-false' is used to disable features rather than `nil'.
  ;; Run `eglot-stderr-buffer' when debugging LSP server errors.

  ;; See: https://rust-analyzer.github.io/manual.html#configuration.
  (add-to-list
   'eglot-server-programs
   '((rust-mode rust-ts-mode) .
     ("rust-analyzer" :initializationOptions
      (:procMacro (:enable t)
       :cargo (:buildScripts (:enable t) :features "all")
       ;; Use Clippy for extra lints.
       :check (:command "clippy")
       ;; WIP: Trying out this setting.
       :diagnostics (:experimental (:enable t))
       ;; Configure Rust inlay hints.
       :inlayHints (:parameterHints (:enable :json-false)
                    :closingBraceHints (:enable t
                                        :minLines 20))))))

  ;; See: https://github.com/golang/tools/blob/master/gopls/doc/inlayHints.md.
  (add-to-list
   'eglot-server-programs
   '((go-mode go-ts-mode) .
     ("gopls" :initializationOptions
      (:hints (:parameterNames :json-false
               :rangeVariableTypes t
               :functionTypeParameters t
               :assignVariableTypes t
               :compositeLiteralFields t
               :compositeLiteralTypes t
               :constantValues t))))))

(use-package consult-eglot
  :general
  (my/bind-goto 'eglot-mode-map
    "o" #'consult-eglot-symbols))

;;;;;; Eldoc

(use-package eldoc
  ;; Later versions of Eglot require a later version of Eldoc. The following
  ;; :preface is required to unload the built-in and allow the new version
  ;; to be loaded. See:
  ;; https://github.com/progfolio/elpaca/issues/236#issuecomment-1879838229.
  :preface
  (unload-feature 'eldoc t)
  (setq custom-delayed-init-variables '())
  (defvar global-eldoc-mode nil)

  :general
  (general-def
    "C-h t" #'eldoc-mode)
  :custom
  (global-eldoc-mode t)
  (eldoc-idle-delay 0.5)
  ;; Compose docs from multiple sources and display as soon as available.
  (eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  ;; If an `eldoc-doc-buffer' buffer is visible then prefer that, otherwise
  ;; fall back to the modeline. Also limit the number of lines shown in the
  ;; modeline and don't display the message about using `eldoc-doc-buffer'.
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-echo-area-use-multiline-p 3)
  (eldoc-echo-area-display-truncation-message nil)
  :config
  ;; Register commands and command prefixes that can move point to a symbol
  ;; where Eldoc documentation should be shown.
  (eldoc-add-command "embark-dwim")
  (eldoc-add-command-completions "avy-goto-")
  (eldoc-add-command-completions "paredit-")
  (eldoc-add-command-completions "xref-find-")
  (eldoc-add-command-completions "xref-go-")
  (eldoc-add-command-completions "xref-goto-"))

(use-package eldoc-box
  :commands
  (eldoc-box-help-at-point
   eldoc-box-quit-frame)
  :general
  (general-def
    "M-p" #'my/eldoc-box-toggle)

  :config
  ;; Quit Eldoc Box when C-g is received. Unfortunately, the variable
  ;; `eldoc-box-clear-with-C-g' only applies to Eldoc Box's hover mode.
  (advice-add #'keyboard-quit :before #'eldoc-box-quit-frame)

  (defun my/eldoc-box-toggle ()
    "Toggle the `eldoc-box-help-at-point' popup."
    (interactive)
    (if (and eldoc-box--frame (frame-visible-p eldoc-box--frame))
        (eldoc-box-quit-frame)
      (eldoc-box-help-at-point))))

;;;;;; Flymake

(use-package flymake
  :elpaca nil
  :functions my/repeatize
  :hook
  (prog-mode . flymake-mode)
  (elpaca-after-init . my/flymake-global-init)

  :general
  (my/bind-c-c 'flymake-mode-map
    "fd" #'flymake-show-buffer-diagnostics
    "fD" #'flymake-show-project-diagnostics
    "fn" #'flymake-goto-next-error
    "fp" #'flymake-goto-prev-error)

  :config
  (defun my/flymake-global-init ()
    "Global init function for `flymake-mode'."
    ;; Tell Flymake about the value of `load-path' late in the startup sequence.
    ;; See: https://emacs.stackexchange.com/a/72754.
    (setq elisp-flymake-byte-compile-load-path
          (append elisp-flymake-byte-compile-load-path load-path)))

  (defun my/flymake-eldoc-show-first ()
    "Prioritize Flymake messages when Eldoc composes documentation."
    ;; This ensures Flymake messages are always visible in the minibuffer.
    (remove-hook 'eldoc-documentation-functions #'flymake-eldoc-function t)
    (add-hook 'eldoc-documentation-functions #'flymake-eldoc-function nil t))

  (defvar-keymap my/flymake-repeat-map
    :doc "Keymap for repeatable Flymake commands."
    "n" #'flymake-goto-next-error
    "p" #'flymake-goto-prev-error)

  (my/repeatize 'my/flymake-repeat-map))

;;;;;; Xref

(use-package xref
  :elpaca nil
  :general
  (my/bind-goto
    "." #'xref-find-definitions
    "?" #'xref-find-references)
  :custom
  ;; Don't prompt by default (invoke with prefix arg to prompt).
  (xref-prompt-for-identifier nil))

;;;;;; Outline

(use-package outline
  :elpaca nil
  :custom
  (outline-minor-mode-prefix "\C-c-")
  (outline-minor-mode-cycle t))

;;;;;; Code Formatting

(use-package apheleia
  :custom
  (apheleia-global-mode t)
  :config
  ;; Use goimports rather than gofmt for Go files so imports get optimized.
  (setf (alist-get 'go-mode apheleia-mode-alist) 'goimports)
  (setf (alist-get 'go-ts-mode apheleia-mode-alist) 'goimports)
  ;; Use Ormolu for formatting Haskell files.
  (setf (alist-get 'haskell-mode apheleia-mode-alist) 'ormolu)
  (setf (alist-get 'ormolu apheleia-formatters) '("ormolu" "--stdin-input-file" ".")))

;;;;; Programming Languages

;;;;;; General

(use-package prog-mode
  :elpaca nil
  :general
  (my/bind-c-c
    ;; Build.
    "ba" #'my/build-system-add
    "bb" #'my/build-system-build
    "bc" #'my/build-system-clean
    "bf" #'my/build-system-fmt
    "bl" #'my/build-system-lint
    "bo" #'my/build-system-outdated
    "br" #'my/build-system-run
    "bu" #'my/build-system-upgrade
    ;; Test.
    "tp" #'my/build-system-test)

  :init
  (defun my/build-system-type ()
    "Return a symbol representing the current project's build type."
    (if-let* ((dir (my/project-current-root)))
        (cond ((file-exists-p (expand-file-name "Cargo.toml" dir)) 'cargo)
              ((file-exists-p (expand-file-name "go.mod" dir)) 'go))))

  (defvar my/build-system-command-alist
    '((cargo
       (add . rustic-cargo-add)
       (build . rustic-cargo-build)
       (clean . rustic-cargo-clean)
       (fmt . rustic-cargo-fmt)
       (lint . rustic-cargo-clippy)
       (outdated . rustic-cargo-outdated)
       (run . rustic-cargo-run)
       (upgrade . rustic-cargo-upgrade)
       (test . rustic-cargo-test))
      (go
       (test . go-test-current-project))))

  (defun my/build-system-run-action (action)
    "Execute ACTION using the project's build sytem.
If a prefix argument has been specified, the command is run from the root of
the current project, otherwise it is run from the current directory."
    (if-let* ((type (my/build-system-type))
              (commands (alist-get type my/build-system-command-alist))
              (command (alist-get action commands))
              (default-directory (if current-prefix-arg
                                     (my/project-current-root)
                                   default-directory)))
        (funcall command)
      (message "Action %s is not known for this build system" action)))

  (defun my/build-system-add ()
    "Execute the add dependency action."
    (interactive)
    (my/build-system-run-action 'add))

  (defun my/build-system-build ()
    "Execute the build/compile action."
    (interactive)
    (my/build-system-run-action 'build))

  (defun my/build-system-clean ()
    "Execute the clean action."
    (interactive)
    (my/build-system-run-action 'clean))

  (defun my/build-system-fmt ()
    "Execute the fmt action."
    (interactive)
    (my/build-system-run-action 'fmt))

  (defun my/build-system-lint ()
    "Execute the lint action."
    (interactive)
    (my/build-system-run-action 'lint))

  (defun my/build-system-outdated ()
    "Execute the outdated action."
    (interactive)
    (my/build-system-run-action 'outdated))

  (defun my/build-system-run ()
    "Execute the run action."
    (interactive)
    (my/build-system-run-action 'run))

  (defun my/build-system-upgrade ()
    "Execute the upgrade action."
    (interactive)
    (my/build-system-run-action 'upgrade))

  (defun my/build-system-test ()
    "Execute the test project action."
    (interactive)
    (my/build-system-run-action 'test)))

;;;;;; Rust

(use-package rust-ts-mode
  :elpaca nil
  :hook
  (rust-ts-mode . my/rust-ts-mode-init)
  :config
  ;; Custom function to be used for `treesit-defun-name-function' that
  ;; handles a wider range of node types.
  (defun my/treesit-rust-defun-name (node)
    "Return the defun name of NODE for Rust node types."
    (pcase (treesit-node-type node)
      ("const_item"
       (treesit-node-text (treesit-node-child-by-field-name node "name") t))
      ("macro_definition"
       (treesit-node-text (treesit-node-child-by-field-name node "name") t))
      ("trait_item"
       (treesit-node-text (treesit-node-child-by-field-name node "name") t))
      ;; Call the default value of `treesit-defun-name-function'.
      (_ (rust-ts-mode--defun-name node))))

  (defun my/rust-ts-mode-init ()
    "Init function for `rust-ts-mode'."
    ;; Override the default function used by tree-sitter to turn a node into
    ;; the name that is displayed by imenu.
    (setq-local treesit-defun-name-function #'my/treesit-rust-defun-name)

    ;; Override the imenu 'categories for Rust as the defaults leave out quite
    ;; a few node types. See possible node types here:
    ;; https://github.com/tree-sitter/tree-sitter-rust/blob/0a70e15da977489d954c219af9b50b8a722630ee/src/node-types.json.
    (setq-local treesit-simple-imenu-settings
                '(("Associated Type" "\\`type_item\\'" nil nil)
                  ("Constant" "\\`const_item\\'" nil nil)
                  ("Enumeration" "\\`enum_item\\'" nil nil)
                  ("Function" "\\`function_item\\'" nil nil)
                  ("Implementation" "\\`impl_item\\'" nil nil)
                  ("Macro" "\\`macro_definition\\'" nil nil)
                  ("Module" "\\`mod_item\\'" nil nil)
                  ("Static" "\\`static_item\\'" nil nil)
                  ("Struct" "\\`struct_item\\'" nil nil)
                  ("Trait" "\\`trait_item\\'" nil nil))))

  ;; Update `consult-imenu-config' with the symbol categories for Rust.
  (with-eval-after-load 'consult-imenu
    (add-to-list 'consult-imenu-config
                 '(rust-ts-mode
                   :types ((?a "Associated Type" my/imenu-category-type-face)
                           (?c "Constant" my/imenu-category-constant-face)
                           (?e "Enumeration" my/imenu-category-enum-face)
                           (?f "Function" my/imenu-category-function-face)
                           (?i "Implementation" my/imenu-category-impl-face)
                           (?M "Macro" my/imenu-category-macro-face)
                           (?m "Module" my/imenu-category-module-face)
                           (?S "Static" my/imenu-category-static-face)
                           (?s "Struct" my/imenu-category-struct-face)
                           (?t "Trait" my/imenu-category-trait-face))))))

;; Rustic is used for its useful commands (e.g. `rustic-cargo-build'), however
;; `rust-ts-mode' is used for the major mode for Rust files. The :demand
;; below forces Rustic to add its entry to `auto-mode-alist' which is then
;; removed in the :config block so that it doesn't override the entry added
;; by `treesit-auto' for `rust-ts-mode'. There must be a better way to do this.
(use-package rustic
  :demand t
  :general
  (my/bind-c-c '(rust-mode-map rust-ts-mode-map)
    "tt" #'rustic-cargo-current-test)
  :custom
  (rustic-lsp-client 'eglot)
  :config
  (setq auto-mode-alist (delete '("\\.rs\\'" . rustic-mode) auto-mode-alist)))

;;;;;; Go

(use-package go-ts-mode
  :elpaca nil
  :hook
  (go-ts-mode . my/go-ts-mode-init)

  :custom
  (go-ts-mode-indent-offset 4)

  :config
  (defun my/treesit-go-defun-name (node)
    "Return the defun name of NODE for Go node types."
    (pcase (treesit-node-type node)
      ("const_spec"
       (treesit-node-text (treesit-node-child-by-field-name node "name") t))
      ("var_spec"
       (treesit-node-text (treesit-node-child-by-field-name node "name") t))
      (_ (go-ts-mode--defun-name node))))

  ;; See possible node types here:
  ;; https://github.com/tree-sitter/tree-sitter-go/blob/bbaa67a180cfe0c943e50c55130918be8efb20bd/src/node-types.json.
  (defun my/go-ts-mode-init ()
    "Init function for `go-ts-mode'."
    (setq-local tab-width go-ts-mode-indent-offset)
    (setq-local go-test-args "-v")
    (setq-local treesit-defun-name-function #'my/treesit-go-defun-name)
    (setq-local treesit-simple-imenu-settings
                '(("Constant" "\\`const_spec\\'" nil nil)
                  ("Function" "\\`function_declaration\\'" nil nil)
                  ("Interface" "\\`type_declaration\\'" go-ts-mode--interface-node-p nil)
                  ("Method" "\\`method_declaration\\'" nil nil)
                  ("New Type" "\\`type_declaration\\'" go-ts-mode--other-type-node-p nil)
                  ("Struct" "\\`type_declaration\\'" go-ts-mode--struct-node-p nil)
                  ("Type Alias" "\\`type_declaration\\'" go-ts-mode--alias-node-p nil)
                  ;; Unfortunately, this also includes local variables.
                  ("Variable" "\\`var_spec\\'" nil nil))))

  (with-eval-after-load 'consult-imenu
    (add-to-list 'consult-imenu-config
                 '(go-ts-mode
                   :types ((?c "Constant" my/imenu-category-constant-face)
                           (?f "Function" my/imenu-category-function-face)
                           (?i "Interface" my/imenu-category-trait-face)
                           (?m "Method" my/imenu-category-method-face)
                           (?t "New Type" my/imenu-category-type-face)
                           (?s "Struct" my/imenu-category-struct-face)
                           (?a "Type Alias" my/imenu-category-type-face)
                           (?v "Variable" my/imenu-category-variable-face))))))

(use-package gotest
  :hook
  ((go-mode go-ts-mode) . (lambda () (setq-local go-test-args "-v")))
  :general
  (my/bind-c-c '(go-mode-map go-ts-mode-map)
    "tf" #'go-test-current-file
    "tt" #'go-test-current-test
    "tp" #'go-test-current-project
    "tb" #'go-test-current-benchmark
    "tc" #'go-test-current-coverage))

(use-package go-gen-test
  :general
  (my/bind-c-c '(go-mode-map go-ts-mode-map)
    "tg" #'go-gen-test-dwim))

;;;;;; Haskell

(use-package haskell-mode)

;;;;;; Terraform

(use-package terraform-mode)

;;;;;; Python

(use-package python
  :elpaca nil
  :init
  (setq python-shell-interpreter "python3"))

;;;;;; TypeScript

(use-package typescript-mode)

;;;;;; Rego

(use-package rego-mode)

;;;;;; Docker

(use-package dockerfile-mode)

;;;;;; Shell

(use-package sh-script
  :elpaca nil
  :custom
  (sh-basic-offset 2))

;;;;;; Protobuf

(use-package protobuf-ts-mode
  :elpaca (:host github :repo "ashlineldridge/protobuf-ts-mode")
  :config
  (with-eval-after-load 'consult-imenu
    (add-to-list 'consult-imenu-config
                 '(protobuf-ts-mode
                   :types ((?r "RPC" my/imenu-category-function-face)
                           (?e "Enum" my/imenu-category-enum-face)
                           (?m "Message" my/imenu-category-struct-face)
                           (?s "Service" my/imenu-category-impl-face))))))

;;;;;; Bazel

(use-package bazel-mode
  :elpaca (:host github :repo "bazelbuild/emacs-bazel-mode")
  :mode
  ("\\.BUILD\\'" . bazel-mode)
  ("\\.bazel\\'" . bazel-mode)
  ("\\.star\\'" . bazel-starlark-mode)
  :custom
  (bazel-buildifier-before-save t)
  :init
  ;; Remove the default Bazel keybindings.
  (setq bazel-mode-map (make-sparse-keymap)))

;;;;;; Just (Task Runner)

(use-package just-mode)

;;;;;; C/C++

(use-package cc-mode
  :elpaca nil)

(use-package clang-format
  :custom
  (clang-format-fallback-style "llvm"))

;;;;;; YAML

(use-package yaml-mode)

;;;;;; Lisp

(use-package elisp-mode
  :elpaca nil
  :hook
  (emacs-lisp-mode . my/elisp-init)

  :general
  (my/bind-c-x
    "C-r" #'eval-region)

  :config
  (defun my/elisp-init ()
    "Init function for `emacs-lisp-mode'."
    (setq-local outline-regexp ";;;+ [^\n]")
    (outline-minor-mode 1)
    (setq-local completion-at-point-functions
                (list
                 #'tempel-complete
                 #'elisp-completion-at-point
                 #'cape-file))
    (my/flymake-eldoc-show-first))

  ;; This configuration is from consult-imenu.el with the fonts changed.
  (with-eval-after-load 'consult-imenu
    (add-to-list 'consult-imenu-config
                 '(emacs-lisp-mode
                   :toplevel "Functions"
                   :types ((?f "Functions" my/imenu-category-function-face)
                           (?m "Macros" my/imenu-category-macro-face)
                           (?p "Packages" my/imenu-category-package-face)
                           (?t "Types" my/imenu-category-type-face)
                           (?v "Variables" my/imenu-category-variable-face))))))

(use-package paredit
  :hook
  ;; Note that I specifically don't enable Paredit in minibuffers as it causes
  ;; issues with RET keybindings.
  ((lisp-mode
    emacs-lisp-mode
    inferior-emacs-lisp-mode) . paredit-mode)

  :config
  ;; Unbind Paredit keybindings I don't use that can cause collisions. This
  ;; doesn't work unless I do it under :config rather than :general.
  (general-unbind 'paredit-mode-map
    "C-c C-M-l" "C-<left>" "C-<right>" "C-M-<left>" "C-M-<right>"
    "M-?" "M-<up>" "M-<down>" "M-S" "M-q" "M-r" "M-s"))

(use-package rainbow-delimiters
  :hook
  ((lisp-mode
    emacs-lisp-mode
    inferior-emacs-lisp-mode) . rainbow-delimiters-mode))

(use-package aggressive-indent
  :hook
  ((lisp-mode
    emacs-lisp-mode
    inferior-emacs-lisp-mode) . aggressive-indent-mode)
  :general
  (general-unbind 'aggressive-indent-mode-map "C-c C-q"))

;; Custom Elisp indentation function - see code comments in package.
(use-package emacs-lisp-indent
  :elpaca (:host github :repo "ashlineldridge/emacs-lisp-indent")
  :init
  (emacs-lisp-indent-install))

;;;;;; SGML/HTML

(use-package sgml-mode
  :elpaca nil
  :general
  (general-unbind 'html-mode-map "M-o"))

;;;;;; Markdown

(use-package markdown-mode
  :commands gfm-mode
  :mode ("\\.md\\'" . gfm-mode)
  :custom
  (markdown-command "multimarkdown"))

;;;; Git

(use-package magit
  :general
  (my/bind-c-c
    "gc" #'magit-clone
    "gs" #'magit-status
    "gd" #'magit-dispatch
    "gf" #'magit-file-dispatch)
  :custom
  ;; Show commit diff when writing commit message (Shackle will place the
  ;; diff buffer in the other window and NOT select it).
  (magit-commit-show-diff t)
  ;; Based on advice from: https://magit.vc/manual/magit/Performance.html.
  (magit-refresh-status-buffer nil)
  (auto-revert-buffer-list-filter #'magit-auto-revert-repository-buffer-p))

(use-package difftastic)

(use-package browse-at-remote
  :general
  (my/bind-c-c
    "go" #'browse-at-remote
    "gk" #'browse-at-remote-kill))

;;;; Shell/Terminal

(use-package eshell
  :elpaca nil
  :hook
  (eshell-mode . my/eshell-init)
  (eshell-pre-command . my/eshell-pre-command)
  (eshell-post-command . my/eshell-post-command)

  :general
  (my/bind-c-c
    "e" #'eshell)
  (my/bind-c-c 'eshell-mode-map
    "C-<backspace>" #'eshell-kill-output
    "C-SPC" #'eshell-mark-output)
  (general-unbind 'eshell-hist-mode-map
    "M-s")

  :custom
  (eshell-history-size 10000)
  (eshell-buffer-maximum-lines 10000)
  (eshell-hist-ignoredups t)
  (eshell-prompt-function #'my/eshell-prompt)
  (eshell-banner-message "Welcome to Eshell\n\n")
  ;; The following commands will be started in `term-mode'.
  (eshell-visual-commands '("vi" "vim" "htop" "btm" "watch"))

  :config
  ;; Needed so that `eshell-mode-map' is available above.
  (require 'esh-mode)

  (defun my/eshell-init ()
    "Hook function executed when `eshell-mode' is run."
    ;; Don't scroll the buffer around after it has been recentered (using C-l).
    ;; This seems to need to be done as a mode hook rather than in `:config' as
    ;; the latter results in `eshell-output-filter-functions' being set to nil.
    ;; See: https://emacs.stackexchange.com/a/45281
    (remove-hook 'eshell-output-filter-functions
                 'eshell-postoutput-scroll-to-bottom)
    ;; Configuration eshell completion.
    (setq-local corfu-auto nil)
    (setq-local cape-file-directory-must-exist nil)
    (setq-local completion-at-point-functions
                (list
                 #'tempel-complete
                 #'cape-file
                 #'pcomplete-completions-at-point))
    ;; Make outline work with eshell prompts.
    (setq-local outline-regexp eshell-prompt-regexp))

  (defun my/eshell-pre-command ()
    "Eshell pre-command hook function."
    ;; Interactive eshell commands should use colors but this gets reverted by
    ;; the post-command hook.
    ;; See: https://github.com/daviwil/dotfiles/blob/master/Emacs.org#configuration.
    (setenv "TERM" "xterm-256color")
    ;; Save history after command is entered but before it is invoked.
    ;; Otherwise, history is not saved until the eshell buffer is quit.
    (eshell-save-some-history))

  (defun my/eshell-post-command ()
    "Eshell post-command hook function."
    (setenv "TERM" "dumb"))

  (defun my/eshell-prompt ()
    "Custom eshell prompt function."
    ;; This is compatible with the default `eshell-prompt-regexp'.
    (concat (my/abbreviate-file-name (eshell/pwd) 30)
            (if (= (user-uid) 0) " # " " $ ")))

  ;; Adapted from https://github.com/zwild/eshell-prompt-extras/blob/c2078093323206b91a1b1f5786d79faa00b76be7/eshell-prompt-extras.el#L213.
  (defun my/abbreviate-file-name (path max-len)
    "Return an abbreviated version of PATH aiming for <= MAX-LEN characters."
    (let* ((components (split-string (abbreviate-file-name path) "/"))
           (len (+ (1- (length components))
                   (cl-reduce '+ components :key 'length)))
           (str ""))
      (while (and (> len max-len)
                  (cdr components))
        (setq str (concat str
                          (cond ((= 0 (length (car components))) "/")
                                ((= 1 (length (car components)))
                                 (concat (car components) "/"))
                                (t
                                 (if (string= "."
                                              (string (elt (car components) 0)))
                                     (concat (substring (car components) 0 2)
                                             "/")
                                   (string (elt (car components) 0) ?/)))))
              len (- len (1- (length (car components))))
              components (cdr components)))
      (concat str (cl-reduce (lambda (a b) (concat a "/" b)) components))))

  (defun my/eshell-refresh-aliases ()
    "Refresh eshell aliases."
    (interactive)
    (eshell-read-aliases-list))

  (defun my/eshell-sink (&optional name)
    "Return a reference to a buffer for sinking eshell command output.
If NAME is specified, a reference to that buffer will be returned, creating the
buffer if necessary. If NAME is not specified, a buffer name will be generated."
    (let* ((name (or name (generate-new-buffer-name "*eshell-output*")))
           (buf (get-buffer-create name)))
      (display-buffer buf)
      buf)))

(use-package vterm
  :commands (vterm-next-prompt vterm-previous-prompt vterm-send-key)
  :functions (my/repeatize project-prefixed-buffer-name)
  :hook (vterm-mode . my/vterm-init)
  :general
  (my/bind-c-c
    "v" #'vterm)
  (my/bind-c-c 'vterm-mode-map
    ;; Same keybinding for shell history as `consult-history' + `cape-history'.
    "h" (lambda () (interactive) (vterm-send-key (kbd "C-r"))))
  (general-unbind 'vterm-mode-map
    "C-o" "C-r" "C-s" "C-SPC"
    "M-s" "M-g" "M-:" "M-&")

  :custom
  (vterm-always-compile-module t)
  ;; I'd much prefer to NOT clear scrollback but I can't rely on it due to
  ;; https://github.com/akermu/emacs-libvterm/issues/546.
  (vterm-clear-scrollback-when-clearing t)
  (vterm-max-scrollback 10000)

  :config
  (defun my/vterm-init ()
    "Hook function executed when `vterm-mode' is run."
    ;; Make outline work with vterm prompts.
    (setq-local outline-regexp "^[^#$\n]* ❯ "))

  ;; Allow find-file-other-window to be called from Vterm.
  (add-to-list 'vterm-eval-cmds
               '("find-file-other-window" find-file-other-window))

  (defvar-keymap my/vterm-repeat-map
    :doc "Keymap for repeatable Vterm commands."
    "C-n" #'vterm-next-prompt
    "C-p" #'vterm-previous-prompt)

  (my/repeatize 'my/vterm-repeat-map)

  (defun my/vterm-project ()
    "Start a Vterm in the current project's root directory."
    (interactive)
    (let* ((default-directory (project-root (project-current t)))
           (buffer-name (project-prefixed-buffer-name "vterm"))
           (existing-buffer (get-buffer buffer-name)))
      (if (and existing-buffer (not current-prefix-arg))
          (pop-to-buffer existing-buffer)
        (vterm buffer-name)))))

(use-package sh-script
  :elpaca nil
  :general
  (my/bind-c-c 'sh-mode-map
    "C-o" nil))

;;;; Org Mode

(use-package org
  ;; Use built-in for now as latest has bugs. When Elpaca supports lockfiles
  ;; I'll just pin it. See: https://github.com/progfolio/elpaca/issues/151.
  :elpaca nil
  :preface
  ;; GTD (agenda) & PKM (notes) paths.
  (defvar my/gtd-dir (expand-file-name "~/dev/home/gtd/"))
  (defvar my/pkm-dir (expand-file-name "~/dev/home/pkm/"))
  (defvar my/gtd-inbox-file (expand-file-name "inbox.org" my/gtd-dir))
  (defvar my/gtd-personal-file (expand-file-name "personal.org" my/gtd-dir))
  (defvar my/gtd-work-file (expand-file-name "work.org" my/gtd-dir))
  (defvar my/gtd-recurring-file (expand-file-name "recurring.org" my/gtd-dir))
  (defvar my/gtd-someday-file (expand-file-name "someday.org" my/gtd-dir))
  (defvar my/gtd-archive-file (expand-file-name "archive.org" my/gtd-dir))
  (defvar my/gtd-bookmarks-file (expand-file-name "bookmarks.org" my/gtd-dir))
  (defvar my/gtd-coffee-file (expand-file-name "coffee.org" my/gtd-dir))
  (defvar my/gtd-music-file (expand-file-name "music.org" my/gtd-dir))

  ;; Order of Org agenda items.
  (defvar my/org-agenda-todo-sort-order '("PROG" "NEXT" "TODO" "HOLD" "DONE"))

  :defines org-agenda-mode-map
  :functions
  (org-agenda-quit
   org-agenda-refile
   org-indent-mode
   org-get-tags
   org-structure-template-alist
   org-refile-get-targets
   org-restart-font-lock
   my/org-agenda-refile
   my/org-agenda-refile-archive
   my/org-agenda-refile-personal
   my/org-agenda-refile-work
   my/org-agenda-refile-inbox)

  :general
  (my/bind-c-c
    "ol" #'org-store-link
    "oa" #'org-agenda
    "os" #'org-save-all-org-buffers
    "oi" '((lambda () (interactive) (org-capture nil "i"))
           :which-key "capture inbox")
    "ob" '((lambda () (interactive) (org-capture nil "b"))
           :which-key "capture bookmark")
    "oc" '((lambda () (interactive) (org-capture nil "c"))
           :which-key "capture coffee")
    "om" '((lambda () (interactive) (org-capture nil "m"))
           :which-key "capture music")
    ;; Use same keybinding given to `org-open-at-point' in Org mode.
    "C-o" #'org-open-at-point-global)
  (my/bind-c-c 'org-mode-map
    "'" nil
    "C-S-l" #'org-cliplink)
  (general-def 'org-agenda-mode-map
    "rr" '(org-agenda-refile :which-key "refile (select)")
    "rp" '(my/org-agenda-refile-personal :which-key "personal")
    "rw" '(my/org-agenda-refile-work :which-key "work")
    "ra" '(my/org-agenda-refile-archive :which-key "archive")
    "rs" '(my/org-agenda-refile-someday :which-key "someday")
    "ri" '(my/org-agenda-refile-inbox :which-key "inbox")
    "k" #'org-agenda-kill
    "?" #'which-key-show-major-mode)
  (general-unbind 'org-mode-map
    ;; Used for Popper.
    "C-'")

  :hook
  (org-mode . my/org-init)

  :custom
  (org-agenda-cmp-user-defined #'my/org-agenda-cmp-todo)
  (org-agenda-custom-commands
   `(("d" . "Dashboards")
     ("da" "All Tasks"
      ((alltodo
	""
	((org-agenda-overriding-header "Inbox")
	 (org-agenda-files '(,my/gtd-inbox-file))))
       (alltodo
	""
        ((org-agenda-overriding-header "Recurring")
	 (org-agenda-files '(,my/gtd-recurring-file))
	 (org-agenda-sorting-strategy '(priority-down))))
       (alltodo
        ""
        ((org-agenda-overriding-header "Work")
         (org-agenda-files '(,my/gtd-work-file))
         (org-agenda-sorting-strategy '(user-defined-up priority-down))))
       (alltodo
        ""
        ((org-agenda-overriding-header "Personal")
         (org-agenda-files '(,my/gtd-personal-file))
         (org-agenda-sorting-strategy '(user-defined-up priority-down))))))
     ("dp" "Personal Tasks"
      ((todo
	"PROG"
        ((org-agenda-overriding-header "Progress")
	 (org-agenda-files '(,my/gtd-personal-file))
	 (org-agenda-sorting-strategy '(priority-down))))
       (todo
	"NEXT"
        ((org-agenda-overriding-header "Next")
	 (org-agenda-files '(,my/gtd-personal-file))
	 (org-agenda-sorting-strategy '(priority-down))))
       (todo
        "HOLD"
        ((org-agenda-overriding-header "Hold")
	 (org-agenda-files '(,my/gtd-personal-file))
	 (org-agenda-sorting-strategy '(priority-down))))
       (todo
        "TODO"
        ((org-agenda-overriding-header "Backlog")
	 (org-agenda-files '(,my/gtd-personal-file))
	 (org-agenda-sorting-strategy '(priority-down))))
       (alltodo
	""
        ((org-agenda-overriding-header "Recurring")
	 (org-agenda-files '(,my/gtd-recurring-file))
	 (org-agenda-sorting-strategy '(priority-down))))
       (alltodo
	""
	((org-agenda-overriding-header "Inbox")
	 (org-agenda-files '(,my/gtd-inbox-file)))))
      ((org-agenda-tag-filter-preset '("-@work"))))
     ("dw" "Work Tasks"
      ((todo
	"PROG"
        ((org-agenda-overriding-header "Progress")
	 (org-agenda-files '(,my/gtd-work-file))
	 (org-agenda-sorting-strategy '(priority-down))))
       (todo
	"NEXT"
        ((org-agenda-overriding-header "Next")
	 (org-agenda-files '(,my/gtd-work-file))
	 (org-agenda-sorting-strategy '(priority-down))))
       (todo
	"HOLD"
        ((org-agenda-overriding-header "Hold")
	 (org-agenda-files '(,my/gtd-work-file))
	 (org-agenda-sorting-strategy '(priority-down))))
       (todo
	"TODO"
        ((org-agenda-overriding-header "Backlog")
	 (org-agenda-files '(,my/gtd-work-file))
	 (org-agenda-sorting-strategy '(priority-down))))
       (alltodo
	""
        ((org-agenda-overriding-header "Recurring")
	 (org-agenda-files '(,my/gtd-recurring-file))
	 (org-agenda-sorting-strategy '(priority-down))))
       (alltodo
	""
	((org-agenda-overriding-header "Inbox")
	 (org-agenda-files '(,my/gtd-inbox-file)))))
      ((org-agenda-tag-filter-preset '("-@personal"))))))
  (org-agenda-files
   (list
    my/gtd-inbox-file
    my/gtd-personal-file
    my/gtd-work-file
    my/gtd-recurring-file))
  ;; Following variable allows customization of the agenda columns.
  (org-agenda-prefix-format
   '((agenda . " %i %-16:c%?-12t% s")
     (todo . " %i %-16:c")
     (tags . " %i %-16:c")
     (search . " %i %-16:c")))
  (org-agenda-span 'week)
  (org-agenda-start-with-log-mode t)
  (org-agenda-tags-column 0)
  (org-agenda-window-setup 'current-window)
  (org-auto-align-tags nil)
  (org-blank-before-new-entry
   '((heading . nil)
     (plain-list-item . nil)))
  (org-capture-templates
   `(("i" "Inbox" entry
      (file+headline ,my/gtd-inbox-file "Inbox")
      "* TODO %i%?")
     ("b" "Bookmark" entry
      (file+olp+datetree ,my/gtd-bookmarks-file "Bookmarks")
      "* %(org-cliplink-capture)%?\n")
     ("c" "Coffee Journal" entry
      (file+olp+datetree ,my/gtd-coffee-file "Coffee Journal" "Log")
      ,(concat
	"* 6%?:00 AM\n"
        "- Beans: Use org-store-link (C-c o l) then org-insert-link (C-c C-l)\n"
        "- Grind: KM47C+PO @ 3.0.0\n"
        "- Water: Brisbane tap @ 95°C\n"
        "- Brew method: V60 4:6\n"
        "- Brew notes:\n"
        "  - Coffee / water: 20g coffee / 300g water\n"
        "  - Breakdown: 50g/70g/60g/60g/60g on 45s with no extra agitation\n"
        "  - Next time: Grind a bit finer\n"
        "- Taste notes:\n"
        "  - Yum yum\n") :jump-to-captured t)
     ("m" "Music" entry
      (file+olp+datetree ,my/gtd-music-file "Music" "Albums")
      ,(concat
	"* %?[[https://open.spotify.com/album/4VNqy9FUAFCvwE6XqrtlOn?si=r3jW-T5QSBqiygbwpzv5fQ][Tom Waits: Bone Machine]] :experimental:rock:jazz:halloffame:\n"
        "- Year: 2023\n"
        "- Rating: 5/5\n"
        "- Notes: Gud allbum.\n"))))
  (org-catch-invisible-edits 'show-and-error)
  (org-confirm-babel-evaluate nil)
  (org-default-notes-file my/gtd-inbox-file)
  (org-directory my/gtd-dir)
  (org-ellipsis " 》")
  (org-fontify-done-headline t)
  (org-fontify-quote-and-verse-blocks t)
  (org-hide-emphasis-markers t)
  (org-log-done 'time)
  ;; Leaving drawer logging disabled for now as I don't like the format of the
  ;; log items and I want to know when a task was created which doesn't happen
  ;; without what apears to be quite a bit of custom code.
  (org-log-into-drawer nil)
  (org-log-states-order-reversed nil)   ; Make newest last
  (org-outline-path-complete-in-steps nil)
  (org-pretty-entities t)
  (org-priority-default org-priority-lowest)
  (org-refile-targets
   `((,my/gtd-archive-file :level . 1)
     (,my/gtd-inbox-file :level . 1)
     (,my/gtd-personal-file :level . 1)
     (,my/gtd-work-file :level . 1)
     (,my/gtd-someday-file :level . 1)
     (,my/gtd-recurring-file :level . 1)))
  ;; Show refile headlines as nested paths.
  (org-refile-use-outline-path t)
  (org-special-ctrl-a/e t)
  (org-startup-indented t)
  (org-tags-column 0)
  (org-todo-keywords
   `((sequence "TODO(t)" "NEXT(n)" "PROG(p)" "HOLD(h)" "|" "DONE(d)")))
  ;; See colours here: https://alexschroeder.ch/geocities/kensanata/colors.html.
  (org-todo-keyword-faces
   `(("TODO" . (:foreground "DodgerBlue2" :weight bold))
     ("NEXT" . (:foreground "hot pink" :weight bold))
     ("PROG" . (:foreground "CadetBlue1" :weight bold))
     ("HOLD" . (:foreground "orange1" :weight bold))
     ("DONE" . (:foreground "orange red" :weight bold))))
  (org-use-fast-todo-selection 'expert)
  (org-use-sub-superscripts nil)

  :init
  ;; Require the `org-agenda' package so that its keymap can be customized.
  (require 'org-agenda)

  :config
  (defun my/org-init ()
    "Init function for `org-mode'."
    (interactive)
    (visual-line-mode 1)
    (variable-pitch-mode 1)
    (display-line-numbers-mode 0)
    (setq-local line-spacing 2)
    (setq-local completion-at-point-functions
                (list #'tempel-complete #'cape-file)))

  (defun my/org-agenda-cmp-todo (a b)
    "Custom compares agenda items A and B based on their todo keywords."
    (when-let ((state-a (get-text-property 14 'todo-state a))
               (state-b (get-text-property 14 'todo-state b))
               (cmp (--map (cl-position-if (lambda (x)
                                             (equal x it))
                                           my/org-agenda-todo-sort-order)
                           (list state-a state-b))))
      (cond ((apply '> cmp) 1)
            ((apply '< cmp) -1)
            (t nil))))

  ;; Function for refiling the current agenda item under point to the specified
  ;; file and heading. `org-agenda-refile' requires a destination refloc list
  ;; that is difficult to compose manually. This approach to pulling the refloc
  ;; out of the return value from `org-refile-get-targets' is adapted from:
  ;; https://emacs.stackexchange.com/questions/54580/org-refile-under-a-given-heading.
  (defun my/org-agenda-refile (file heading)
    "Refile the current agenda item to the refile target for FILE and HEADING."
    (org-agenda-refile nil
                       (seq-find
                        (lambda (refloc)
                          (and
                           (string= heading (nth 0 refloc))
                           (string= file (nth 1 refloc))))
                        (org-refile-get-targets)) nil))

  (defun my/org-agenda-refile-personal-or-work (file &optional category)
    "Refile the current org agenda item into FILE under Personal/ or Work/.
If CATEGORY is specified it must equal \\='personal or \\='work; if it is not
specified then a task category will be determined by the item's tags."
    (interactive)
    (let* ((hdm  (org-get-at-bol 'org-hd-marker))
	   (tags (with-current-buffer (marker-buffer hdm) (org-get-tags hdm))))
      (cond ((or (eq 'personal category) (member "@personal" tags))
             (my/org-agenda-refile file "Personal"))
            ((or (eq 'work category) (member "@work" tags))
             (my/org-agenda-refile file "Work"))
            (t (cl-case (read-char
                         (format "Refile into %s as [p]ersonal or [w]ork?"
                                 (file-name-nondirectory file)))
                 (?p (my/org-agenda-refile-personal-or-work file 'personal))
                 (?w (my/org-agenda-refile-personal-or-work file 'work))
                 (t (message "Bad selection")))))))

  (defun my/org-agenda-refile-personal ()
    "Refile the current org agenda item into the personal list."
    (interactive)
    (my/org-agenda-refile my/gtd-personal-file "Personal"))

  (defun my/org-agenda-refile-work ()
    "Refile the current org agenda item into the work list."
    (interactive)
    (my/org-agenda-refile my/gtd-work-file "Work"))

  (defun my/org-agenda-refile-inbox ()
    "Refile the current org agenda item into the inbox."
    (interactive)
    (my/org-agenda-refile my/gtd-inbox-file "Inbox"))

  (defun my/org-agenda-refile-archive ()
    "Refile the current org agenda item into the archive."
    (interactive)
    (my/org-agenda-refile-personal-or-work my/gtd-archive-file))

  (defun my/org-agenda-refile-someday ()
    "Refile the current org agenda item into the someday."
    (interactive)
    (my/org-agenda-refile-personal-or-work my/gtd-someday-file))

  (defun my/org-toggle-emphasis-markers ()
    "Toggle the display of org emphasis markers."
    (interactive)
    (org-restart-font-lock)
    (setq org-hide-emphasis-markers (not org-hide-emphasis-markers)))

  ;; Save all org buffers before quitting the agenda ('s' saves immediately).
  (advice-add #'org-agenda-quit :before #'org-save-all-org-buffers)

  ;; Make it easier to create `org-babel' code blocks.
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp")))

(use-package org-cliplink)

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org-roam
  :general
  (my/bind-c-c
    "nl" #'org-roam-buffer-toggle
    "nf" #'org-roam-node-find
    "ng" #'org-roam-graph
    "ni" #'org-roam-node-insert
    "nc" #'org-roam-capture
    "nt" #'org-roam-tag-add)

  :custom
  (org-roam-db-autosync-mode 1)
  (org-roam-directory (expand-file-name "notes/" my/pkm-dir))
  ;; Disable `org-roam' completion as it's a bit annoying.
  (org-roam-completion-everywhere nil)
  (org-roam-completion-functions nil)
  (org-roam-node-display-template
   (concat "${title:70} " (propertize "${tags:70}" 'face 'org-tag)))
  (org-roam-capture-templates
   `(("d" "default" plain
      ,(concat
        "#+title: ${title}\n"
        "#+created: %<%Y-%m-%d>\n"
        "#+filetags: :tag1:tag2:\n\n"
        "* Note\n"
        "Replace or delete this section\n\n"
        "* Links\n"
        "** Roam Links\n"
        "- Use org-roam-node-insert (C-c n i) here\n\n"
        "** Web Links\n"
        "- Use org-insert-link (C-c C-l), org-cliplink (C-c C-S-l), or paste URL here\n")
      :target (file "%<%Y%m%d%H%M%S>-${slug}.org")
      :empty-lines-before 1
      :unnarrowed t)))
  (org-roam-dailies-directory "../journal")
  (org-roam-dailies-capture-templates
   `(("d" "default" entry "* %?"
      :target (file+head+olp "%<%Y-%m-%d>.org"
                             "#+title: %<%Y-%m-%d>\n"
                             ("Today")))))

  :config
  (require 'org-roam-protocol)
  (require 'org-roam-dailies))

;;;; Spelling

(use-package jinx
  :general
  (general-def
    "M-$" #'jinx-correct
    "C-M-$" #'jinx-languages)
  (my/bind-c-c
    "xj" #'jinx-mode))

;;;; Process Management

(use-package proced
  :elpaca nil
  :general
  (my/bind-c-x
    "C-p" #'proced)
  :custom
  (proced-enable-color-flag t))

;;; End:
(provide 'init)

;;; init.el ends here
