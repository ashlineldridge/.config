;;; init.el --- Emacs Init -*- lexical-binding: t -*-

;; Author: Ashlin Eldridge <ashlin.eldridge@gmail.com>
;; URL: https://github.com/ashlineldridge/.config
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.0"))

;;; Commentary:
;;
;; My bonsai.

;;; Code:

;;;; Keybinding Management

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
  (general-create-definer my/bind-window :prefix "C-o")
  (general-create-definer my/bind-c-c :prefix "C-c")
  (general-create-definer my/bind-c-x :prefix "C-x"))

;; Packages that modify the syntax of `use-package' need to be waited on.
(declare-function elpaca-wait nil)
(elpaca-wait)

(use-package transient
  :commands transient-get-value)

(use-package repeat
  :elpaca nil
  :commands my/repeatize
  :config
  ;; See: https://karthinks.com/software/it-bears-repeating.
  (defun my/repeatize (keymap)
    "Add `repeat-mode'-like support to KEYMAP (passed by symbol)."
    (map-keymap
     (lambda (_key cmd)
       (when (symbolp cmd)
         (put cmd 'repeat-map keymap)))
     (symbol-value keymap))))

;;;; Base Settings

(use-package emacs
  :elpaca nil
  :hook
  ;; Display line numbers in certain modes.
  ((prog-mode config-mode text-mode) . display-line-numbers-mode)
  ;; Don't wrap long lines in programming modes.
  (prog-mode . my/truncate-lines)

  :general
  (general-def
    "<escape>" #'keyboard-escape-quit
    "C-;" #'comment-line
    "C-S-k" #'my/copy-to-eol
    "C-M-k" #'my/delete-to-eol
    "C-h C-h" nil
    "M-[" #'previous-buffer
    "M-]" #'next-buffer
    "M-<backspace>" #'my/delete-to-bol
    [remap quit-window] #'kill-this-buffer
    [remap query-replace] #'my/query-replace-wrap
    [remap query-replace-regexp] #'my/query-replace-regexp-wrap)

  (my/bind-c-x
    "C-k" #'kill-this-buffer)

  (my/bind-search
    ;; Add search prefix descriptions.
    "h" '(:ignore t :which-key "highlight"))

  (my/bind-c-c
    "-" '(:ignore t :which-key "outline")
    "b" '(:ignore t :which-key "build")
    "b1" #'compile
    "b2" #'recompile
    "f" '(:ignore t :which-key "flymake")
    "g" '(:ignore t :which-key "magit")
    "n" '(:ignore t :which-key "notes")
    "o" '(:ignore t :which-key "org")
    "p" '(:ignore t :which-key "cape")
    "r" '(:ignore t :which-key "refactor")
    "rw" #'delete-trailing-whitespace
    "t" '(:ignore t :which-key "test")
    "v" '(:ignore t :which-key "visual")
    "v|" #'display-fill-column-indicator-mode
    "vl" #'toggle-truncate-lines
    "vw" #'my/toggle-show-trailing-whitespace)

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
  (fill-column 100)
  (column-number-mode t)
  (global-auto-revert-mode t)
  (async-shell-command-buffer 'new-buffer)
  (shell-command-switch "-ic")
  (history-length 1000)
  (savehist-mode t)
  (savehist-additional-variables
   '(search-ring regexp-search-ring extended-command-history))
  (electric-pair-mode t)
  (electric-pair-inhibit-predicate #'my/electric-pair-inhibit)
  (repeat-mode t)
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
  (minibuffer-depth-indicate-mode 1)
  ;; Don't allow the cursor in the minibuffer prompt text.
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt))
  ;; Don't show M-x commands which don't work in the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)
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
     ("Canada/Eastern" "Toronto")))

  :init
  ;; Use y/n rather than yes/no.
  (defalias 'yes-or-no-p #'y-or-n-p)

  ;; Enable Emacs functions that are disabled by default.
  (put 'narrow-to-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)

  (defun my/truncate-lines ()
    "Truncate long lines rather than wrapping."
    (setq-local truncate-lines t))

  (defun my/toggle-show-trailing-whitespace ()
    "Toggle visibility of trailing whitespace."
    (interactive)
    (setq show-trailing-whitespace (if show-trailing-whitespace nil t)))

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
          (delete-backward-char 1 nil)
        (delete-region (line-beginning-position) (point)))))

  (defun my/flash-mode-line ()
    "Flashes the mode line for a visible bell."
    (invert-face 'mode-line)
    (run-with-timer 0.1 nil 'invert-face 'mode-line))

  (defun my/electric-pair-inhibit (c)
    "Return whether C should be excluded from pairing."
    ;; Exclude '<' as I want that for triggering Tempel.
    (if (char-equal c ?<) t (electric-pair-default-inhibit c)))

  (defun my/query-replace-advice (orig-fun &rest args)
    "Around advice to add wrapping behaviour to `query-replace' and friends."
    (save-excursion
      (let ((point-start (point))
            (point-min (point-min))
            (region-active (use-region-p)))
        (apply orig-fun args)
        (when (and
               (not region-active)
               (/= point-start point-min)
               (yes-or-no-p "Wrap to beginning of buffer? "))
          (beginning-of-buffer)
          (setf (nth 3 args) point-min)
          (setf (nth 4 args) point-start)
          (apply orig-fun args)))))

  ;; My versions of `query-replace' and `query-replace-regexp' that provide
  ;; an option to wrap the buffer and return point its starting position.
  (fset 'my/query-replace-wrap #'query-replace)
  (fset 'my/query-replace-regexp-wrap #'query-replace-regexp)
  (advice-add #'my/query-replace-wrap :around #'my/query-replace-advice)
  (advice-add #'my/query-replace-regexp-wrap :around #'my/query-replace-advice))

;;;; Appearance

;;;;; Themes

(use-package modus-themes
  :commands modus-themes-load-theme
  :hook
  (elpaca-after-init . (lambda ()
                         (modus-themes-load-theme (car modus-themes-to-toggle))))
  :custom
  (modus-themes-to-toggle '(modus-vivendi modus-operandi-tinted))
  (modus-themes-italic-constructs t)
  (modus-themes-custom-auto-reload t)
  (modus-themes-prompts '(bold))
  (modus-themes-org-blocks 'gray-background)
  (modus-themes-headings
   '((1 . (variable-pitch rainbow background 1.3))
     (2 . (variable-pitch rainbow background semibold 1.2))
     (3 . (variable-pitch rainbow background semibold 1.1))
     (t . (variable-pitch rainbow semilight 1.1)))))

(use-package standard-themes)

;;;;; Fonts

(use-package faces
  :elpaca nil
  :general
  (my/bind-c-c
    "vf" #'my/cycle-font-config)

  :hook
  ;; Update fonts after theme is loaded so changes take effect.
  ((modus-themes-post-load
    standard-themes-post-load) . my/apply-font-config)

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
    "Face for displaying constant symbols in imenu.")

  (defface my/imenu-category-enum-face
    '((t :inherit font-lock-type-face))
    "Face for displaying enumeration symbols in imenu.")

  (defface my/imenu-category-function-face
    '((t :inherit font-lock-function-name-face))
    "Face for displaying function symbols in imenu.")

  (defface my/imenu-category-impl-face
    '((t :inherit font-lock-type-face))
    "Face for displaying implementation symbols in imenu.")

  (defface my/imenu-category-package-face
    '((t :inherit font-lock-constant-face))
    "Face for displaying package symbols in imenu.")

  (defface my/imenu-category-macro-face
    '((t :inherit font-lock-preprocessor-face))
    "Face for displaying macro symbols in imenu.")

  (defface my/imenu-category-method-face
    '((t :inherit font-lock-function-name-face))
    "Face for displaying method symbols in imenu.")

  (defface my/imenu-category-module-face
    '((t :inherit font-lock-constant-face))
    "Face for displaying module symbols in imenu.")

  (defface my/imenu-category-static-face
    '((t :inherit font-lock-constant-face))
    "Face for displaying static symbols in imenu.")

  (defface my/imenu-category-struct-face
    '((t :inherit font-lock-type-face))
    "Face for displaying struct symbols in imenu.")

  (defface my/imenu-category-trait-face
    '((t :inherit font-lock-type-face))
    "Face for displaying trait/interface symbols in imenu.")

  (defface my/imenu-category-type-face
    '((t :inherit font-lock-type-face))
    "Face for displaying type-centric symbols in imenu.")

  (defface my/imenu-category-variable-face
    '((t :inherit font-lock-variable-name-face))
    "Face for displaying variable symbols in imenu.")

  (defun my/apply-font-config (&optional index)
    "Apply the INDEX'th font configuration from `my/font-configs'."
    (let* ((index (or index my/font-config-index))
           (config (nth index my/font-configs))
           (fixed-font-height (plist-get config :fixed-font-height))
           (variable-font-height (plist-get config :variable-font-height))
           (line-number-font-height (plist-get config :line-number-font-height))
           (mode-line-font-height (plist-get config :mode-line-font-height)))
      (set-face-attribute 'default nil
                          :font my/fixed-font
                          :height fixed-font-height)
      (set-face-attribute 'fixed-pitch nil
                          :font my/fixed-font
                          :height fixed-font-height)
      (set-face-attribute 'variable-pitch nil
                          :font my/variable-font
                          :height variable-font-height)
      (set-face-attribute 'mode-line nil
                          :font my/fixed-font
                          :height mode-line-font-height)
      (set-face-attribute 'mode-line-inactive nil
                          :font my/fixed-font
                          :height mode-line-font-height)
      (set-face-attribute 'line-number nil
                          :font my/fixed-font
                          :height line-number-font-height
                          :slant 'italic)

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

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-ibuffer
  :hook
  (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package nerd-icons-completion
  ;; For some reason, this only seems to work when `nerd-icons-completion-mode.'
  ;; is called very late in the startup cycle.
  :hook (elpaca-after-init . nerd-icons-completion-mode))

;;;;; Mode Line

(use-package doom-modeline
  :commands doom-modeline-mode
  :custom
  ;; Mode line height is determined by the smaller of `doom-modeline-height'
  ;; and the mode line font. The function `doom-modeline--font-height' can be
  ;; called to determine the font height that will be used to calculate the
  ;; height of the mode line.
  (doom-modeline-height 1)
  (doom-modeline-bar-width 4)
  (doom-modeline-lsp t)
  (doom-modeline-github nil)
  (doom-modeline-mu4e nil)
  (doom-modeline-irc nil)
  (doom-modeline-minor-modes t)
  (doom-modeline-persp-name nil)
  (doom-modeline-buffer-file-name-style 'relative-from-project)
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-column-zero-based nil)
  :init
  (doom-modeline-mode 1))

(use-package minions
  :commands minions-mode
  :init
  (minions-mode 1))

;;;;; Other Visuals

(use-package hl-line
  :elpaca nil
  :general
  (my/bind-c-c
    "vh" #'hl-line-mode))

;;;; Window Management

;;;;; Window Manipulation and Selection

(use-package ace-window
  :commands
  (aw-switch-to-window
   aw-delete-window
   aw-select)

  :functions
  (my/split-window-vertically
   my/split-window-horizontally)

  :general
  (my/bind-window
    ;; Add `ace-window' to the primary window prefix for completeness; though
    ;; I pretty much always use the shorter M-o defined below.
    "a" #'ace-window)

  (general-def
    "M-o" #'ace-window)

  :custom
  (aw-display-mode-overlay t)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?l ?m))
  (aw-dispatch-always t)
  (aw-background t)

  :config
  (defun my/ace-open-buffer (window)
    "Opens a buffer in WINDOW using `consult-buffer'."
    (aw-switch-to-window window)
    (consult-buffer))

  (defun my/ace-delete-frame (window)
    "Ace Window action that deletes WINDOW and kills its buffer."
    (aw-switch-to-window window)
    (delete-frame)
    (other-frame 1))

  (defun my/ace-split-window-vertically (window)
    "Ace Window action that splits WINDOW vertically and selects the new one."
    (select-window window)
    (my/split-window-vertically))

  (defun my/ace-split-window-horizontally (window)
    "Split WINDOW horizontally."
    (select-window window)
    (my/split-window-horizontally))

  ;; Personalize Ace Window's dispatch menu (doesn't work when customized via
  ;; the :custom block so need to do via `setq' here).
  (setq aw-dispatch-alist
        '((?k aw-delete-window "Delete Window")
          (?K my/ace-delete-frame "Delete Frame")
          (?1 delete-other-windows "Delete Other Windows")
          (?w aw-swap-window "Swap Windows")
          (?m aw-move-window "Move Window")
          (?c aw-copy-window "Copy Window")
          (?b my/ace-open-buffer "Open Buffer")
          (?x my/ace-split-window-vertically "Split Vertical")
          (?y my/ace-split-window-horizontally "Split Horizontal")
          (?? aw-show-dispatch-help))))

;; Brings in useful functions such as `transpose-frame', `flip-frame', etc.
;; See: https://www.emacswiki.org/emacs/TransposeFrame.
(use-package transpose-frame)

;;;;; Window History

(use-package winner
  :elpaca nil
  :custom
  (winner-dont-bind-my-keys t)
  :init
  (winner-mode 1))

;;;;; General Window Commands

(use-package window
  :elpaca nil
  :general
  (my/bind-window
    "=" #'balance-windows
    "]" #'rotate-frame-clockwise
    "[" #'rotate-frame-anticlockwise
    "1" #'delete-other-windows
    "o" #'other-window
    "C-o" #'other-frame
    "b" #'shrink-window-horizontally
    "f" #'enlarge-window-horizontally
    "n" #'enlarge-window
    "N" #'make-frame
    "k" #'delete-window
    "K" #'delete-frame
    "p" #'shrink-window
    "u" #'winner-undo
    "r" #'winner-redo
    "x" #'my/split-window-vertically
    "y" #'my/split-window-horizontally
    "X" #'flip-frame
    "Y" #'flop-frame)

  (my/bind-c-x
    ;; Replace existing splitting bindings.
    "2" #'my/split-window-vertically
    "3" #'my/split-window-horizontally)

  :custom
  ;; The following provides the default `display-buffer' behaviour for buffers
  ;; that are not managed by either Popper or Shackle.
  (display-buffer-base-action '((display-buffer-reuse-mode-window
                                 display-buffer-reuse-window
                                 display-buffer-same-window)))
  (even-window-sizes nil)

  :config
  (defvar-keymap my/window-repeat-map
    :doc "Keymap for repeatable window commands."
    "o" #'other-window
    "C-o" #'other-frame
    "b" #'shrink-window-horizontally
    "f" #'enlarge-window-horizontally
    "n" #'enlarge-window
    "p" #'shrink-window
    "]" #'rotate-frame-clockwise
    "[" #'rotate-frame-anticlockwise
    "u" #'winner-undo
    "r" #'winner-redo)

  (my/repeatize 'my/window-repeat-map)

  (defun my/split-window-vertically ()
    "Split window vertically and select the other window."
    (interactive)
    (split-window-vertically)
    (other-window 1))

  (defun my/split-window-horizontally ()
    "Split window horizontally and select the other window."
    (interactive)
    (split-window-horizontally)
    (other-window 1)))

;;;;; Window Orchestration

;; Use Shackle for managing windows that aren't considered to be "popups" by
;; Popper. Shackle basically replaces the manual configuration of
;; `display-buffer-base-action' and `display-buffer-alist' with a simpler
;; syntax. `shackle-mode' needs to be called before `popper-mode' so that the
;; latter gets priority in `display-buffer-alist'; that way, any buffer not
;; claimed by Popper will be subject to Shackle's rules.
(use-package shackle
  :commands shackle-mode
  :custom
  (shackle-default-rule nil)
  (shackle-rules
   '(("\\*eldoc" :select nil :other t :regexp t)
     ("\\*eshell-output\\*" :select t :other t :regexp t)
     ("\\*helpful" :select t :other t :regexp t)
     ("\\*rg\\*" :select t :other t :regexp t)
     ("\\*Occur\\*" :select t :other t :regexp t)
     ("\\*Pp" :select nil :other t :regexp t)))
  :init
  (shackle-mode 1))

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

  :init
  (popper-mode 1)
  (popper-echo-mode 1)

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
  :commands (which-key-mode which-key-add-key-based-replacements)
  :custom
  ;; Use Embark for `prefix-help-command' as it is searchable.
  (which-key-show-early-on-C-h nil)
  (which-key-use-C-h-commands nil)
  (which-key-idle-delay 1.0)
  (which-key-idle-secondary-delay 0.05)
  (which-key-popup-type 'side-window)
  (which-key-side-window-location '(bottom right))
  :init
  (which-key-mode 1))

;;;; Completion System

(use-package corfu
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
  (corfu-auto-delay 0.5)
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
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      ;; Disable automatic documentation echo and popup (if enabled).
      (setq-local corfu-echo-delay nil
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))

  ;; From: https://github.com/minad/corfu#transfer-completion-to-the-minibuffer.
  (defun my/corfu-move-to-minibuffer ()
    "Transfer the Corfu completion session to the minibuffer."
    (interactive)
    (when completion-in-region--data
      (let ((completion-extra-properties corfu--extra)
            completion-cycle-threshold completion-cycling)
        (apply #'consult-completion-in-region completion-in-region--data)))))

;; Documentation shown alongside Corfu completion popups.
(use-package corfu-popupinfo
  :after corfu
  :elpaca
  (:host github :repo "minad/corfu" :files ("extensions/corfu-popupinfo.el"))
  :general
  (general-def 'corfu-map
    "M-d" #'corfu-popupinfo-toggle
    "<up>" #'corfu-popupinfo-scroll-down
    "<down>" #'corfu-popupinfo-scroll-up)
  :hook
  (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay 1.0))

;; Corfu icons.
(use-package kind-icon
  :after corfu
  :commands kind-icon-margin-formatter
  :defines corfu-margin-formatters
  :hook
  ;; After toggling between themes the icon cache needs to be reset.
  ((modus-themes-post-load
    standard-themes-post-load) . kind-icon-reset-cache)
  :custom
  (kind-icon-default-face 'corfu-default)
  :init
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

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
  :commands vertico-mode
  :custom
  (vertico-count 20)
  :init
  (vertico-mode 1))

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
  :commands vertico-multiform-mode
  :custom
  (vertico-multiform-categories
   '((imenu buffer)
     (file (vertico-sort-function . my/vertico-sort-dirs-first))))

  ;; Sometimes commands are better when the category is too broad.
  (vertico-multiform-commands
   '((consult-outline buffer)))

  :init
  (vertico-multiform-mode 1)

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
  (general-def
    ;; Use prefix arg to select from list.
    "M-R" #'vertico-repeat)
  :init
  ;; Persist Vertico history between Emacs sessions.
  (add-to-list 'savehist-additional-variables 'vertico-repeat-history))

;; Dedicated completion commands.
(use-package cape
  :general
  (my/bind-c-c
    "pd" #'cape-dabbrev
    "ph" #'cape-history
    "pf" #'cape-file
    "pk" #'cape-keyword
    "po" #'cape-elisp-symbol
    "pa" #'cape-abbrev
    "pl" #'cape-line
    "pw" #'cape-dict)
  (general-def 'eshell-mode-map
    "C-r" #'cape-history)

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
  (completion-category-overrides nil)
  ;; Allow backslash to escape the space to search for literal spaces.
  (orderless-component-separator #'orderless-escapable-split-on-space))

(use-package marginalia
  :commands marginalia-mode
  :custom
  (marginalia-mode t))

(use-package consult
  :commands
  (consult--buffer-state
   consult--buffer-action
   consult--buffer-query
   consult--customize-put)
  :defines consult-imenu-config

  :general
  (general-def
    "M-y" #'consult-yank-pop)

  (general-def 'minibuffer-local-map
    "M-s" nil
    "C-r" #'consult-history)

  (general-def 'isearch-mode-map
    ;; Replace `isearch-edit-string' to get history completion.
    "M-e" #'consult-isearch-history
    ;; Allow `consult-line' functions to "take over" from `isearch'.
    "M-s l" #'consult-line
    "M-s L" #'consult-line-multi)

  (general-def 'consult-narrow-map
    "C-<" #'consult-narrow-help
    ;; Remove if this becomes annoying due to needing a '?' character.
    "?" #'consult-narrow-help)

  (my/bind-search
    "a" #'consult-org-agenda
    "f" #'consult-find
    "l" #'consult-line
    "L" #'consult-line-multi
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

  (my/bind-c-c 'flymake-mode-map
    "ff" #'consult-flymake)

  (my/bind-c-c
    "vv" #'consult-theme)

  (my/bind-c-x
    "b" #'consult-buffer
    "rr" #'consult-register
    "rl" #'consult-register-load
    "rs" #'consult-register-store)

  :custom
  (register-preview-delay 0.5)
  (register-preview-function #'consult-register-format)

  ;; Type < followed by a prefix key to narrow the available candidates.
  ;; Type C-< (defined above) to display prefix help. Alternatively, type
  ;; < followed by C-h (or ?) to make `embark-prefix-help-command' kick in
  ;; and display a completing prefix help.
  (consult-narrow-key "<")

  ;; By default, only show previews when M-. is pressed.
  (consult-preview-key 'any)

  ;; Customise the list of sources shown by consult-buffer.
  (consult-buffer-sources
   '(consult--source-buffer          ;; Narrow: ?b
     consult--source-modified-buffer ;; Narrow: ?*
     my/consult-source-dired-buffer  ;; Narrow: ?d
     my/consult-source-eshell-buffer ;; Narrow: ?e
     consult--source-project-buffer  ;; Narrow: ?p
     consult--source-recent-file     ;; Narrow: ?r
     consult--source-bookmark))      ;; Narrow: ?m

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

  ;; Tell `consult-find' to search hidden dirs/files but ignore .git/.
  (consult-find-args "find . -not ( -name .git -type d -prune )")

  :config
  (defun my/consult-line-strict (&optional initial start)
    "Version of `consult-line' that uses a strict substring completion style."
    (interactive (list nil (not (not current-prefix-arg))))
    (let ((completion-styles '(substring)))
      (consult-line initial start)))

  (defvar my/consult-source-dired-buffer
    `(:name "Dired Buffer"
      :narrow ?d
      :category buffer
      :face consult-buffer
      :history buffer-name-history
      :state ,#'consult--buffer-state
      :items ,(lambda ()
                (consult--buffer-query
                 :sort 'visibility
                 :as #'buffer-name
                 :mode 'dired-mode))))

  (defvar my/consult-source-eshell-buffer
    `(:name "Eshell Buffer"
      :narrow ?e
      :category buffer
      :face consult-buffer
      :history buffer-name-history
      :state ,#'consult--buffer-state
      :items ,(lambda ()
                (consult--buffer-query
                 :sort 'visibility
                 :as #'buffer-name
                 :mode 'eshell-mode))))

  (consult-customize
   ;; Source name and narrow key customization.
   consult--source-buffer :name "Open Buffer" :narrow ?b
   consult--source-project-buffer :name "Project Buffer" :narrow ?p
   consult--source-recent-file :name "Recent File" :narrow ?r))

(use-package consult-dir
  :commands consult-dir
  :general
  (my/bind-search
    "d" #'consult-dir)
  :custom
  (consult-dir-shadow-filenames nil)
  (consult-dir-default-command #'consult-dir-dired))

(use-package embark
  :commands
  (embark-prefix-help-command
   embark-target-identifier-at-point
   my/goto-consult-xref)

  :general
  (general-def
    "C-." #'embark-act
    "C-M-." #'embark-dwim
    "C-h b" #'embark-bindings)

  (general-def 'minibuffer-local-map
    "M-b" #'embark-become)

  (general-def 'embark-file-map "o" #'my/ace-find-file)
  (general-def 'embark-buffer-map "o" #'my/ace-switch-to-buffer)
  (general-def 'embark-bookmark-map "o" #'my/ace-bookmark-jump)
  (general-def 'my/embark-org-roam-node-map "o" #'my/ace-org-roam-node-find)
  (general-def 'my/embark-consult-xref-map "o" #'my/ace-goto-consult-xref)

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
  ;; TODO: What is the more idiomatic way to do this? This way is flawed as it
  ;; only navigates to the line rather than where the symbol is on the line.
  (defun my/goto-consult-xref ()
    "Embark action function for opening a `consult-xref' candidate."
    (interactive)
    (let ((location (read-from-minibuffer "")))
      (message "The location is: %s" location)
      (let* ((parts (string-split location ":"))
             (file (nth 0 parts))
             (line (nth 1 parts)))
        (find-file file)
        (goto-char (point-min))
        (forward-line (1- (string-to-number line))))))

  (defun my/ace-find-file ()
    "Switch window and run `find-file'."
    (interactive)
    (aw-switch-to-window (aw-select nil))
    (call-interactively #'find-file))

  (defun my/ace-switch-to-buffer ()
    "Switch window and run `switch-to-buffer'."
    (interactive)
    (aw-switch-to-window (aw-select nil))
    (call-interactively #'switch-to-buffer))

  (defun my/ace-bookmark-jump ()
    "Switch window and run `bookmark-jump'."
    (interactive)
    (aw-switch-to-window (aw-select nil))
    (call-interactively #'bookmark-jump))

  (defun my/ace-org-roam-node-find ()
    "Switch window and run `org-roam-node-find'."
    (interactive)
    (aw-switch-to-window (aw-select nil))
    (call-interactively #'org-roam-node-find))

  (defun my/ace-goto-consult-xref ()
    "Switch window and run `my/goto-consult-xref'."
    (interactive)
    (aw-switch-to-window (aw-select nil))
    (call-interactively #'my/goto-consult-xref))

  (defvar-keymap my/embark-org-roam-node-map
    :doc "Keymap for Embark `org-roam-node' actions."
    :parent embark-general-map)

  (defvar-keymap my/embark-consult-xref-map
    :doc "Keymap for Embark `consult-xref' actions."
    :parent embark-general-map)

  (add-to-list 'embark-keymap-alist '(org-roam-node . my/embark-org-roam-node-map))
  (add-to-list 'embark-keymap-alist '(consult-xref . my/embark-consult-xref-map))
  (add-to-list 'embark-keymap-alist '(xref . my/embark-consult-xref-map))

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

;;;; General Editing

;;;;; Undo/Redo

(use-package undo-tree
  :commands global-undo-tree-mode
  :general
  (general-unbind 'undo-tree-mode-map "C-_")
  :custom
  ;; Disable history saving for now.
  (undo-tree-auto-save-history nil)
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-diff t)
  :init
  (global-undo-tree-mode 1))

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

;; A lot of my Avy inspiration came from the following:
;; https://karthinks.com/software/avy-can-do-anything and
;; https://github.com/karthink/.emacs.d/blob/master/lisp/setup-avy.el.
(use-package avy
  :functions ring-ref
  :general
  (general-unbind "M-j")
  (general-def :prefix "M-j"
    "j" #'avy-goto-char-timer
    "l" #'avy-goto-line
    "M-l" #'avy-goto-end-of-line
    "y" #'avy-copy-line
    "M-y" #'avy-copy-region
    "k" #'avy-kill-whole-line
    "M-k" #'avy-kill-region
    "w" #'avy-kill-ring-save-whole-line
    "M-w" #'avy-kill-ring-save-region
    "m" #'avy-move-line
    "M-m" #'avy-move-region)

  :init
  (defun my/avy-action-embark-act (pt)
    "Avy action for running `embark-act' on the selected candidate."
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0)))) t)

  (defun my/avy-action-kill-line (pt)
    "Avy action for running `kill-line' on the line of the selected candidate."
    (save-excursion
      (goto-char pt)
      (kill-line))
    (select-window
     (cdr (ring-ref avy-ring 0))) t)

  (defun my/avy-action-kill-whole-line (pt)
    "Avy action for running `kill-whole-line' on the line of the candidate."
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window
     (cdr
      (ring-ref avy-ring 0))) t)

  (defun my/avy-action-copy-whole-line (pt)
    "Avy action for copying the whole line of the candidate to the kill ring."
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'line)
        (copy-region-as-kill start end)))
    (select-window
     (cdr
      (ring-ref avy-ring 0))) t)

  :custom
  (avy-single-candidate-jump t)
  (avy-timeout-seconds 0.4)
  (avy-keys '(?a ?s ?d ?f ?g ?h ?j ?l ?v ?b))
  (avy-dispatch-alist
   '((?k . avy-action-kill-stay)
     (?K . my/avy-action-kill-whole-line)
     (?t . avy-action-teleport)
     (?m . avy-action-mark)
     (?w . avy-action-copy)
     (?W . my/avy-action-copy-whole-line)
     (?y . avy-action-yank)
     (?Y . avy-action-yank-line)
     (?z . avy-action-zap-to-char)
     (?. . my/avy-action-embark-act))))

(use-package isearch
  :elpaca nil
  :general
  (my/bind-search
    "." #'isearch-forward-thing-at-point)
  (general-def 'isearch-mode-map
    "C-n" #'isearch-repeat-forward
    "C-p" #'isearch-repeat-backward))

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
    ;; Keep M-o binding for ace-window.
    "M-o" nil
    ;; Group buffers by VC project.
    "/p" #'ibuffer-vc-set-filter-groups-by-vc-root))

(use-package ibuffer-vc)

;;;; File System

;;;;; File Browsing

(use-package dired
  :elpaca nil
  :general
  (general-def 'dired-mode-map
    "C-o" nil
    "N" #'dired-create-empty-file
    "?" #'which-key-show-major-mode
    "i" #'dired-subtree-insert
    ";" #'dired-subtree-remove
    "M-s" nil)
  :hook
  (dired-mode . auto-revert-mode)
  :custom
  ;; See more settings here:
  ;; https://github.com/protesilaos/dotfiles/blob/master/emacs/.emacs.d/prot-emacs-modules/prot-emacs-dired.el
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (delete-by-moving-to-trash t)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-use-ls-dired nil))

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

;; I generally prefer to not use a tree viewer and rely more on dired and the
;; various file jumping commands, but we've got some huge repos at work where
;; I find it useful to have a tree viewer so that I can mentally situate myself
;; within the directory tree. I use treemacs for visualization only and stick
;; to dired and eshell for manipulation. This treemacs config was inspired by:
;; https://github.com/seagle0128/.emacs.d/blob/8cbec0c132cd6de06a8c293598a720d377f3f5b9/lisp/init-treemacs.el.
(use-package treemacs
  :commands
  (treemacs
   treemacs-select-window
   ;; Used by the inline function `treemacs-current-visibility' below.
   treemacs-get-local-buffer
   treemacs-get-local-window)
  :general
  (general-def
    "M-t" #'my/treemacs-stay)
  (general-def 'treemacs-mode-map
    ;; Otherwise it takes two clicks to open a directory.
    [mouse-1] #'treemacs-single-click-expand-action)

  :hook
  ;; Don't wrap long lines in Treemacs.
  (treemacs-mode . my/truncate-lines)

  :custom
  (treemacs-follow-mode t)
  (treemacs-project-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-width 40)
  (treemacs-missing-project-action 'remove)
  (treemacs-follow-after-init t)

  :init
  (defun my/treemacs-stay ()
    "Open the Treemacs viewer but keep the current window selected."
    (interactive)
    (treemacs)
    ;; If the Treemacs buffer is visible then call `treemacs-select-window'
    ;; to shift focus back to the previously selected window.
    (when (eq 'visible (treemacs-current-visibility))
      (treemacs-select-window)))

  :config
  ;; By default, treemacs-mode will add itself to `aw-ignored-buffers' which
  ;; prevents jumping to its window using ace-window. Personally, I prefer
  ;; being able to treat it just like any other window.
  (require 'ace-window)
  (setq aw-ignored-buffers (delq 'treemacs-mode aw-ignored-buffers)))

;; Style the treemacs viewer with nerd icons.
(use-package treemacs-nerd-icons
  :after treemacs
  :functions treemacs-load-theme
  :custom-face
  (treemacs-nerd-icons-root-face ((t (:inherit nerd-icons-green :height 1.3))))
  (treemacs-nerd-icons-file-face ((t (:inherit nerd-icons-dsilver))))
  :config
  (treemacs-load-theme "nerd-icons"))

;;;;; File History

(use-package recentf
  :elpaca nil
  :custom
  (recentf-max-saved-items 300)
  :config
  (recentf-mode 1))

;;;;; Project Management

(use-package project
  :elpaca nil
  :general
  (general-def 'project-prefix-map
    "u" #'my/project-update-list)

  :custom
  (project-switch-commands #'project-dired)

  :config
  (defun my/project-current-root ()
    "Return the root directory of the current or nil."
    (if-let* ((proj (project-current)))
        (project-root proj)))

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
  (super-save-auto-save-when-idle t)
  (super-save-idle-duration 15)
  (super-save-max-buffer-size 100000)
  :init
  (super-save-mode 1))

;;;; Programming

;;;;; General Programming

;;;;;; Tree-Sitter

(use-package treesit
  :elpaca nil
  :custom
  (treesit-font-lock-level 4) ;; Be extra.
  (treesit-extra-load-path
   (list (no-littering-expand-var-file-name "tree-sitter")))
  :config
  (defvar my/treesit-language-source-alist
    '((bash "https://github.com/tree-sitter/tree-sitter-bash")
      (go "https://github.com/tree-sitter/tree-sitter-go")
      (gomod "https://github.com/camdencheek/tree-sitter-gomod")
      (json "https://github.com/tree-sitter/tree-sitter-json")
      (proto "https://github.com/mitchellh/tree-sitter-proto")
      (rust "https://github.com/tree-sitter/tree-sitter-rust")
      (toml "https://github.com/tree-sitter/tree-sitter-toml")
      (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
  (setq treesit-language-source-alist my/treesit-language-source-alist)
  (setq major-mode-remap-alist
        '((conf-toml-mode . toml-ts-mode)
          (bash-mode . bash-ts-mode)
          (go-mode . go-ts-mode)
          (js-json-mode . json-ts-mode)
          (rust-mode . rust-ts-mode)
          (rustic-mode . rust-ts-mode)
          (sh-mode . bash-ts-mode)
          (conf-toml-mode . toml-ts-mode)))

  (defun my/treesit-update-language-grammars ()
    "Update all Tree Sitter language grammars."
    (interactive)
    ;; Calling `treesit-install-language-grammar' modifies the source alist so
    ;; so it needs to be reset if called multiple times in the same session.
    (setq treesit-language-source-alist my/treesit-language-source-alist)
    ;; Download the latest grammars to the default directory and then move them
    ;; into the directory managed by no-littering.
    (let ((old-dir (expand-file-name "tree-sitter"  user-emacs-directory))
          (new-dir (car treesit-extra-load-path)))
      (delete-directory old-dir t)
      (mapc #'treesit-install-language-grammar
            (mapcar #'car treesit-language-source-alist))
      (delete-directory new-dir t)
      (rename-file old-dir new-dir))))

(elpaca-wait)

;;;;;; Eglot

(use-package eglot
  :commands
  (eglot-completion-at-point
   eglot--current-server-or-lose
   eglot--request
   eglot--TextDocumentPositionParams)
  :functions my/flymake-eldoc-show-first

  :hook
  ((bash-ts-mode
    go-ts-mode
    rust-ts-mode) . eglot-ensure)
  (eglot-managed-mode . my/eglot-init)

  :general
  (my/bind-goto :keymaps 'eglot-mode-map
    "^" #'eglot-find-implementation)

  (my/bind-c-c :keymaps 'eglot-mode-map
    "ra" #'eglot-code-actions
    "ro" #'eglot-code-action-organize-imports
    "rr" #'eglot-rename)

  (my/bind-c-c :keymaps 'eglot-mode-map
    "vi" #'eglot-inlay-hints-mode)

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
  :after eglot
  :general
  (my/bind-goto :keymap 'eglot-mode-map
    "o" #'consult-eglot-symbols))

;;;;;; Eldoc

(use-package eldoc
  :elpaca nil
  :general
  (general-def
    "C-h t" #'eldoc-mode)
  :custom
  (global-eldoc-mode 1)
  (eldoc-idle-delay 0)
  ;; Compose docs from multiple sources and display as soon as available.
  (eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  ;; If an `eldoc-doc-buffer' buffer is visible then prefer that, otherwise
  ;; fall back to the modeline. Also limit the number of lines shown in the
  ;; modeline and don't display the message about using `eldoc-doc-buffer'.
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-echo-area-use-multiline-p 3)
  (eldoc-echo-area-display-truncation-message nil)
  :init
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
  :general
  (my/bind-c-c 'flymake-mode-map
    "fd" #'flymake-show-buffer-diagnostics
    "fD" #'flymake-show-project-diagnostics
    "fn" #'flymake-goto-next-error
    "fp" #'flymake-goto-prev-error)

  :hook
  (prog-mode . flymake-mode)
  (elpaca-after-init . my/flymake-global-init)

  :config
  (defun my/flymake-global-init ()
    "Global init function for `flymake-mode'."
    ;; Tell Flymake about the value of `load-path' late in the startup sequence.
    ;; See: https://emacs.stackexchange.com/a/72754.
    (setq elisp-flymake-byte-compile-load-path load-path))

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
  (xref-prompt-for-identifier nil)
  ;; Use consult to select xref locations with preview.
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref))

;;;;;; Outline

(use-package outline
  :elpaca nil
  :custom
  (outline-minor-mode-prefix "\C-c-")
  (outline-minor-mode-cycle t))

;;;;;; Code Formatting

(use-package apheleia
  :commands apheleia-global-mode
  :init
  (apheleia-global-mode 1)
  :config
  ;; Use goimports rather than gofmt so that imports get optimized.
  (setf (alist-get 'go-ts-mode apheleia-mode-alist) 'goimports))

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
    "Execute ACTION using the project's build sytem."
    (if-let* ((type (my/build-system-type))
              (commands (alist-get type my/build-system-command-alist))
              (command (alist-get action commands)))
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
  ;; Demand to register .rs files in `auto-mode-alist'.
  :demand t
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

(use-package rustic
  ;; TODO: Look at reintegrating Rustic when it provides tree-sitter support.
  ;; This package is not disabled as I want to use the `rustic-*' functions.
  ;; However, `rustic-mode' should not be run as it is mapped to `rust-ts-mode'
  ;; in `major-mode-remap-alist'. See: https://github.com/brotzeit/rustic/issues/475.
  :general
  (my/bind-c-c :keymaps 'rust-ts-mode-map
    "tt" #'rustic-cargo-current-test)
  :custom
  (rustic-lsp-client 'eglot))

;;;;;; Go

(use-package go-ts-mode
  :elpaca nil
  ;; Demand to register .go files in `auto-mode-alist'.
  :demand t
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
  :general
  (my/bind-c-c :keymaps 'go-ts-mode-map
    "tf" #'go-test-current-file
    "tt" #'go-test-current-test))

(use-package go-gen-test
  :general
  (my/bind-c-c :keymaps 'go-ts-mode-map
    "rg" #'go-gen-test-dwim))

;;;;;; Terraform

(use-package terraform-mode)

;;;;;; Python

(use-package python
  :elpaca nil
  :init
  (setq python-shell-interpreter "python3"))

;;;;;; JavaScript

(use-package js2-mode
  :mode "\\.js\\'")

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

(use-package yaml-ts-mode
  :elpaca nil)

;;;;;; JSON

(use-package json-ts-mode
  :elpaca nil)

;;;;;; Jsonnet

(use-package jsonnet-mode)

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
    (setq-local fill-column 80)
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
    "C-c C-M-l"
    "C-<left>"
    "C-<right>"
    "C-M-<left>"
    "C-M-<right>"
    "M-S"
    "M-s"
    "M-?"))

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
  (general-def 'html-mode-map
    ;; Unbind M-o as I want that for ace-window.
    "M-o" nil))

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
    "gs" #'magit-status
    "gd" #'magit-dispatch
    "gf" #'magit-file-dispatch)
  :custom
  ;; Otherwise Magit shows a read-only diff screen when you press C-c C-c and
  ;; you've already had a chance to look at the diff when you stage the files.
  (magit-commit-show-diff nil))

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
  (my/bind-c-c :keymaps 'eshell-mode-map
    ;; Needed for `org-open-at-point-global'.
    "C-o" nil)
  (eshell-hist-mode-map
   ;; Needed for search key prefix.
   "M-s" nil)

  :custom
  (eshell-history-size 10000)
  (eshell-buffer-maximum-lines 10000)
  (eshell-hist-ignoredups t)
  (eshell-prompt-function #'my/eshell-prompt)
  ;; The following commands will be started in `term-mode'.
  (eshell-visual-commands '("vi" "vim" "htop" "watch"))

  :config
  ;; Needed so that `eshell-mode-map' is available above.
  (require 'esh-mode)

  (defun my/eshell-init ()
    "Hook function executed when `eshell-mode' is run."
    ;; Default to not wrapping long eshell lines.
    (my/truncate-lines)
    ;; Don't scroll the buffer around after it has been recentered (using C-l).
    ;; This seems to need to be done as a mode hook rather than in `:config' as
    ;; the latter results in `eshell-output-filter-functions' being set to nil.
    ;; See: https://emacs.stackexchange.com/a/45281
    (remove-hook 'eshell-output-filter-functions
                 'eshell-postoutput-scroll-to-bottom)
    ;; Configuration eshell completion.
    (setq-local completion-at-point-functions
                (list
                 #'tempel-complete
                 #'pcomplete-completions-at-point
                 #'cape-file
                 #'cape-dabbrev)))

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

(use-package mistty
  :elpaca (:host github :repo "szermatt/mistty")
  :general
  (my/bind-c-c
    "s" #'mistty)
  (general-def 'mistty-prompt-map
    "C-r" #'mistty-send-C-r))

(use-package sh-script
  :elpaca nil
  :general
  (my/bind-c-c :keymaps 'sh-mode-map
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
   my/org-agenda-refile-personal-ongoing
   my/org-agenda-refile-work-ongoing
   my/org-agenda-refile-someday-ongoing
   my/org-agenda-refile-inbox)

  :general
  (my/bind-c-c
    "ol" #'org-store-link
    "oa" #'org-agenda
    "om" #'org-capture
    "os" #'org-save-all-org-buffers
    "oi" #'my/org-capture-inbox
    "ob" #'my/org-capture-bookmark
    "oc" #'my/org-capture-coffee
    ;; Use same keybinding given to `org-open-at-point' in Org mode.
    "C-o" #'org-open-at-point-global)
  (my/bind-c-c :keymaps 'org-mode-map
    "C-S-l" #'org-cliplink)
  (general-def 'org-agenda-mode-map
    "ra" #'my/org-agenda-refile-archive
    "rp" #'my/org-agenda-refile-personal-ongoing
    "rw" #'my/org-agenda-refile-work-ongoing
    "rs" #'my/org-agenda-refile-someday-ongoing
    "ri" #'my/org-agenda-refile-inbox
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
        "  - Yum yum\n") :jump-to-captured t)))
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
   `((,my/gtd-archive-file :tag . "refile")
     (,my/gtd-inbox-file :level . 1)
     (,my/gtd-personal-file :regexp . "Tasks")
     (,my/gtd-work-file :regexp . "Tasks")
     (,my/gtd-someday-file :tag . "refile")
     (,my/gtd-recurring-file :level . 2)))
  ;; Show refile headlines as nested paths.
  (org-refile-use-outline-path t)
  (org-special-ctrl-a/e t)
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
    (org-indent-mode 1)
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

  (defun my/org-capture-inbox ()
    "Capture an inbox item."
    (interactive)
    (org-capture nil "i"))

  (defun my/org-capture-bookmark ()
    "Capture a bookmark."
    (interactive)
    (org-capture nil "b"))

  (defun my/org-capture-coffee ()
    "Capture a coffee log entry."
    (interactive)
    (org-capture nil "c"))

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

  (defun my/org-agenda-refile-archive (&optional category)
    "Refile the current org agenda item into the appropriate archive.
If CATEGORY is specified it must equal \\='personal or \\='work; if it is not
specified then a task category will be determined by the item's tags."
    (interactive)
    (let* ((hdm  (org-get-at-bol 'org-hd-marker))
	   (tags (with-current-buffer (marker-buffer hdm) (org-get-tags hdm))))
      (cond ((or (eq 'personal category) (member "@personal" tags))
             (my/org-agenda-refile my/gtd-archive-file
                                   "Personal/Projects/Ongoing/Tasks"))
            ((or (eq 'work category) (member "@work" tags))
             (my/org-agenda-refile my/gtd-archive-file
                                   "Work/Projects/Ongoing/Tasks"))
            (t (cl-case (read-char "Archive as [p]ersonal or [w]ork?")
                 (?p (my/org-agenda-refile-archive 'personal))
                 (?w (my/org-agenda-refile-archive 'work))
                 (t (message "Bad selection")))))))

  (defun my/org-agenda-refile-personal-ongoing ()
    "Refile the current org agenda item into the personal/ongoing list."
    (interactive)
    (my/org-agenda-refile my/gtd-personal-file "Projects/Ongoing/Tasks"))

  (defun my/org-agenda-refile-work-ongoing ()
    "Refile the current org agenda item into the work/ongoing list."
    (interactive)
    (my/org-agenda-refile my/gtd-work-file "Projects/Ongoing/Tasks"))

  (defun my/org-agenda-refile-someday-ongoing ()
    "Refile the current org agenda item into the someday/ongoing list."
    (interactive)
    (my/org-agenda-refile my/gtd-someday-file "Projects/Ongoing/Tasks"))

  (defun my/org-agenda-refile-inbox ()
    "Refile the current org agenda item into the inbox list."
    (interactive)
    (my/org-agenda-refile my/gtd-inbox-file "Inbox"))

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
  :commands org-roam-db-autosync-mode
  :general
  (my/bind-c-c
    "nl" #'org-roam-buffer-toggle
    "nf" #'org-roam-node-find
    "ng" #'org-roam-graph
    "ni" #'org-roam-node-insert
    "nc" #'org-roam-capture
    "nt" #'org-roam-tag-add)

  :custom
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
  (require 'org-roam-dailies)
  (org-roam-db-autosync-mode 1))

;;;; Process Management

(use-package proced
  :elpaca nil
  :general
  (my/bind-c-x
    "C-p" #'proced)
  :custom
  (proced-enable-color-flag t))

;;;; Credential Management

(use-package pass)

(use-package auth-source-pass
  :elpaca nil
  :custom
  (auth-source-do-cache nil)
  :init
  (auth-source-pass-enable))

;;; End:
(provide 'init)

;;; init.el ends here
