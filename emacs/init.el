;;; init.el --- Emacs Init -*- lexical-binding: t -*-

;; Author: Ashlin Eldridge <ashlin.eldridge@gmail.com>
;; URL: https://github.com/ashlineldridge/.config
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.2"))

;;; Commentary:
;;
;; My bonsai.

;;; Code:

;;;; Bootstrap

(load (expand-file-name "bootstrap.el" user-emacs-directory))

;;;; Base Settings

(use-package emacs
  :ensure nil
  :preface
  (defalias 'yes-or-no-p #'y-or-n-p)

  (declare-function server-running-p "server")
  (defun my/server-start ()
    "Start Emacs in server mode if it is not already."
    (require 'server)
    (unless (server-running-p)
      (server-start)))

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

  (defun my/toggle-show-trailing-whitespace ()
    "Toggle visibility of trailing whitespace."
    (interactive)
    (setq show-trailing-whitespace (not show-trailing-whitespace))
    (message "%s trailing whitespace in current buffer"
             (if show-trailing-whitespace "Showing" "Hiding")))

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

  (defun my/truncate-lines ()
    "Show long lines as truncated in the current buffer."
    (setq-local truncate-lines t))

  :hook
  ;; Display line numbers in certain modes.
  ((prog-mode config-mode text-mode) . display-line-numbers-mode)
  ;; Don't wrap long lines in programming modes.
  (prog-mode . my/truncate-lines)
  ;; Run Emacs in server mode.
  (elpaca-after-init . my/server-start)

  :bind
  ("<escape>" . keyboard-escape-quit)
  ("C-;" . comment-line)
  ("C-S-k" . my/copy-to-eol)
  ("C-M-k" . my/delete-to-eol)
  ("C-h C-h" . nil)
  ("M-[" . previous-buffer)
  ("M-]" . next-buffer)
  ("M-<backspace>" . my/delete-to-bol)
  ("M-k" . kill-current-buffer)
  ("C-c b 1" . compile)
  ("C-c b 2" . recompile)
  ("C-c x |" . display-fill-column-indicator-mode)
  ("C-c x n" . display-line-numbers-mode)
  ("C-c x w" . my/toggle-show-trailing-whitespace)
  ("C-c x W" . delete-trailing-whitespace)
  ("C-x r K" . my/clear-registers)

  :custom
  (confirm-kill-emacs #'yes-or-no-p)
  (inhibit-startup-screen t)
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
  ;; Save the left pinky: tab for tabbing and M-/ for completion.
  (tab-always-indent t)
  (indent-tabs-mode nil)
  (set-mark-command-repeat-pop t)
  (mark-ring-max 10)
  (global-mark-ring-max 10)
  (vc-follow-symlinks t)
  (compilation-ask-about-save nil)
  (fill-column 80)
  (column-number-mode t)
  (shell-command-prompt-show-cwd t)
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
  ;; Enable Emacs functions that are disabled by default.
  (dolist (cmd '(narrow-to-region
                 upcase-region
                 downcase-region
                 set-goal-column))
    (put cmd 'disabled nil))

  ;; Bind some special characters under 'C-x 8'. The [?] character is a zero-
  ;; width space character that can be used to escape org mode emphasis markers.
  ;; See: https://emacs.stackexchange.com/questions/16688/how-can-i-escape-the-in-org-mode-to-prevent-bold-fontification.
  (bind-keys
   :map iso-transl-ctl-x-8-map
   ("0" . [?​])
   ("a" . [?α])
   ("b" . [?β])
   ("l" . [?λ])
   (">" . [?⟶])
   ("<" . [?⟵])
   ("s" . [?😊])))

;;;; Appearance

;;;;; Fonts

(use-package faces
  :ensure nil
  :preface
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
    :group 'my/imenu-faces))

(use-package fontaine
  :preface
  ;; Variable pitch headings used by Modus and Ef themes.
  (defvar my/variable-pitch-headings
    '((1 . (variable-pitch semibold 1.2))
      (t . (variable-pitch semibold 1.1))))

  ;; Can't seem to get `fontaine-mode' to work so doing this for now.
  (defun my/fontaine-reset (&rest _)
    "Apply the current font configuration."
    (fontaine-set-preset (or fontaine-current-preset 'small)))

  :bind
  ("C-c x f" . fontaine-set-preset)
  :custom
  (fontaine-presets
   '((small
      :default-height 134
      :mode-line-active-height 120
      :mode-line-inactive-height 120
      :line-number-height 112)
     (regular)
     (t
      :default-family "Iosevka Comfy"
      :default-weight regular
      :default-height 140
      :fixed-pitch-height 1.0
      :fixed-pitch-serif-height 1.0
      :variable-pitch-family "Iosevka Comfy Duo"
      :variable-pitch-weight regular
      :variable-pitch-height 1.0
      :mode-line-active-family "Iosevka Comfy"
      :mode-line-active-height 130
      :mode-line-inactive-family "Iosevka Comfy"
      :mode-line-inactive-height 130
      :line-number-family "Iosevka Comfy"
      :line-number-slant italic
      :line-number-height 120
      :bold-weight bold
      :italic-slant italic)))
  :init
  (add-hook 'enable-theme-functions #'my/fontaine-reset))

;;;;; Themes

(use-package modus-themes
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-custom-auto-reload t)
  (modus-themes-mixed-fonts t)
  (modus-themes-prompts '(bold))
  (modus-themes-org-blocks 'gray-background)
  (modus-themes-headings my/variable-pitch-headings))

(use-package ef-themes
  :custom
  (ef-themes-mixed-fonts t)
  (ef-themes-variable-pitch-ui t)
  (ef-themes-headings my/variable-pitch-headings))

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
  (doom-modeline-mode t)
  (doom-modeline-bar-width 4)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-buffer-file-name-style 'relative-from-project)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-column-zero-based t)
  (doom-modeline-height 20)
  (doom-modeline-lsp t)
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-minor-modes nil)
  (doom-modeline-percent-position nil)
  (doom-modeline-time-icon nil)
  (doom-modeline-total-line-number t)
  (doom-modeline-window-width-limit nil))

;;;;; Point/Cursor

(use-package cursory
  :commands cursory-set-preset
  :hook
  (org-mode . (lambda () (cursory-set-preset 'bar :local)))
  :init
  (cursory-set-preset 'box))

;;;;; Margins

(use-package olivetti
  :bind
  ("<f7>" . olivetti-mode)
  :custom
  (olivetti-body-width 90)
  (olivetti-style 'fancy))

;;;;; Scrolling

(use-package pixel-scroll
  :ensure nil
  :preface
  (declare-function pixel-scroll-precision-interpolate "pixel-scroll")
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
    (my/pixel-scroll-page (- my/pixel-scroll-factor)))

  :bind
  ([remap scroll-up-command] . pixel-scroll-interpolate-down)
  ([remap scroll-down-command] . pixel-scroll-interpolate-up)
  ("M-S-<up>" . my/pixel-scroll-partial-up)
  ("M-S-<down>" . my/pixel-scroll-partial-down)
  :custom
  (pixel-scroll-precision-mode t)
  (pixel-scroll-precision-interpolate-page t))

;;;; Window Management

;;;;; General Window Movement and Commands

(use-package window
  :ensure nil
  :bind
  (("M-q" . bury-buffer)
   ("M-o =" . balance-windows)
   ("M-o <left>" . shrink-window-horizontally)
   ("M-o <right>" . enlarge-window-horizontally)
   ("M-o <up>" . enlarge-window)
   ("M-o <down>" . shrink-window)
   ("M-o 0" . delete-window)
   ("M-o 1" . delete-other-windows)
   ("M-o 2" . split-window-below)
   ("M-o 3" . split-window-right)
   :repeat-map my/window-repeat-map
   ("=" . balance-windows)
   ("<left>" . shrink-window-horizontally)
   ("<right>" . enlarge-window-horizontally)
   ("<up>" . enlarge-window)
   ("<down>" . shrink-window)
   ("0" . delete-window)
   ("1" . delete-other-windows)
   ("2" . split-window-below)
   ("3" . split-window-right))
  :custom
  ;; The following provides the default `display-buffer' behaviour for buffers
  ;; that are not managed by either Popper or Shackle.
  (display-buffer-base-action '((display-buffer-reuse-mode-window
                                 display-buffer-reuse-window
                                 display-buffer-same-window)))
  (even-window-sizes nil))

(use-package windmove
  :ensure nil
  :preface
  (declare-function windmove-find-other-window "windmove")
  (declare-function windmove-do-window-select "windmove")

  (defun my/windmove-throw-left ()
    "Move the current buffer to the window to the left."
    (interactive)
    (my/windmove-throw 'left))

  (defun my/windmove-throw-right ()
    "Move the current buffer to the window to the right."
    (interactive)
    (my/windmove-throw 'right))

  (defun my/windmove-throw-up ()
    "Move the current buffer to the window above."
    (interactive)
    (my/windmove-throw 'up))

  (defun my/windmove-throw-down ()
    "Move the current buffer to the window below."
    (interactive)
    (my/windmove-throw 'down))

  (defun my/windmove-throw (dir)
    "Move the current buffer to the window in direction DIR."
    (require 'windmove)
    (let ((window (selected-window))
          (buffer (current-buffer))
          (target (windmove-find-other-window dir)))
      (if (null target)
          (user-error "No window %s from selected window" dir)
        (windmove-do-window-select dir)
        (switch-to-buffer buffer nil t)
        (select-window window)
        (switch-to-prev-buffer))))
  :bind
  (("M-o b" . windmove-left)
   ("M-o f" . windmove-right)
   ("M-o p" . windmove-up)
   ("M-o n" . windmove-down)
   ("M-o B" . my/windmove-throw-left)
   ("M-o F" . my/windmove-throw-right)
   ("M-o P" . my/windmove-throw-up)
   ("M-o N" . my/windmove-throw-down)
   ("M-o M-b" . windmove-delete-left)
   ("M-o M-f" . windmove-delete-right)
   ("M-o M-p" . windmove-delete-up)
   ("M-o M-n" . windmove-delete-down)
   :repeat-map my/window-repeat-map
   ("b" . windmove-left)
   ("f" . windmove-right)
   ("p" . windmove-up)
   ("n" . windmove-down)
   ("B" . my/windmove-throw-left)
   ("F" . my/windmove-throw-right)
   ("P" . my/windmove-throw-up)
   ("N" . my/windmove-throw-down)
   ("M-b" . windmove-delete-left)
   ("M-f" . windmove-delete-right)
   ("M-p" . windmove-delete-up)
   ("M-n" . windmove-delete-down)))

(use-package framemove
  :ensure (:host github :repo "emacsmirror/framemove")
  :config
  (setq framemove-hook-into-windmove t))

(use-package transpose-frame
  :bind
  (("M-o >" . rotate-frame-clockwise)
   ("M-o <" . rotate-frame-anticlockwise)
   ("M-o 4" . flop-frame)
   ("M-o 5" . flip-frame)
   :repeat-map my/window-repeat-map
   (">" . rotate-frame-clockwise)
   ("<" . rotate-frame-anticlockwise)
   ("4" . flop-frame)
   ("5" . flip-frame)))

;;;;; Window History

(use-package winner
  :ensure nil
  :preface
  :bind
  (("M-o u" . winner-undo)
   ("M-o r" . winner-redo)
   :repeat-map my/window-repeat-map
   ("u" . winner-undo)
   ("r" . winner-redo))
  :custom
  (winner-dont-bind-my-keys t)
  :init
  ;; Needs to be enabled via a function call rather than a customization
  ;; so that `winner-dont-bind-my-keys' takes effect.
  (winner-mode 1))

;;;;; Tab Bar

(use-package tab-bar
  :ensure nil
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
   ;; More reliable to match by regex than by major mode as some commands
   ;; don't set the major mode until after they've called `display-buffer'.
   '(("\\*eldoc" :select nil :other t :regexp t)
     ("\\*eshell-output\\*" :select t :other t :regexp t)
     ("\\*helpful" :select nil :other t :regexp t)
     ("\\*rg\\*" :select t :other t :regexp t)
     ("\\*Occur\\*" :select t :other t :regexp t)
     ("\\*Pp" :select nil :other t :regexp t)
     ("^magit-diff:" :select nil :other t :regexp t))))

(use-package popper
  :after shackle
  :preface
  (declare-function popper-open-latest "popper")
  (declare-function popper-kill-latest-popup "popper")
  (defvar my/popper-ignore-modes '(grep-mode rg-mode))

  (defun my/popper-kill-popup-stay-open ()
    "Kill the current popup but stay open if there are others."
    (interactive)
    (popper-kill-latest-popup)
    (popper-open-latest))

  :bind
  ("C-'" . popper-toggle)
  ("M-'" . popper-cycle)
  ("C-M-'" . popper-toggle-type)
  ("C-M-\"" . my/popper-kill-popup-stay-open)

  :custom
  (popper-mode t)
  (popper-echo-mode t)
  (popper-window-height 20)
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
             (format " %s " (nerd-icons-octicon "nf-oct-pin" :face face))))))

;;;; Help System

(use-package help-fns
  :ensure nil
  :bind
  ("C-h F" . describe-face))

(use-package helpful
  :bind
  (("C-h c" . helpful-callable)
   ;; Replace `describe-*' bindings with Helpful where possible.
   ([remap describe-function] . helpful-function)
   ([remap describe-symbol] . helpful-symbol)
   ([remap describe-variable] . helpful-variable)
   ([remap describe-command] . helpful-command)
   ([remap describe-key] . helpful-key)
   ;; Replace `display-local-help' with Helpful for Elisp.
   :map emacs-lisp-mode-map
   ([remap display-local-help] . helpful-at-point)
   :map lisp-interaction-mode-map
   ([remap display-local-help] . helpful-at-point))

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

;;;;; Undo/Redo

(use-package undo-tree
  :custom
  (global-undo-tree-mode t)
  (undo-tree-auto-save-history t)
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-diff t))

;;;;; Region Expansion

(use-package expreg
  :ensure (:host github :repo "casouri/expreg")
  :bind
  ("C-=" . expreg-expand)
  ("C-+" . expreg-contract))

;;;;; Whitespace

(use-package ws-butler
  :hook ((text-mode prog-mode) . ws-butler-mode)
  :custom
  (ws-butler-keep-whitespace-before-point nil))


;;;;; Jumping Around

(use-package avy
  :bind
  ("M-j" . nil)
  ("M-j j" . avy-goto-char-timer)
  ("M-j l" . avy-goto-line)
  ("M-j L" . avy-goto-end-of-line)
  ("M-j y" . avy-copy-line)
  ("M-j Y" . avy-copy-region)
  ("M-j k" . avy-kill-whole-line)
  ("M-j K" . avy-kill-region)
  ("M-j w" . avy-kill-ring-save-whole-line)
  ("M-j W" . avy-kill-ring-save-region)
  ("M-j m" . avy-move-line)
  ("M-j M" . avy-move-region)
  :custom
  (avy-all-windows 'all-frames)
  (avy-single-candidate-jump t)
  (avy-timeout-seconds 0.4)
  (avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package link-hint
  :bind
  ("M-j o " . link-hint-open-link)
  ("M-j O" . link-hint-copy-link))

;;;;; Highlighting

(use-package hl-line
  :ensure nil
  :bind
  ("C-c x h" . hl-line-mode)
  :custom
  (hl-line-sticky-flag nil))

(use-package hl-todo
  :custom
  (global-hl-todo-mode t))

(use-package hi-lock
  :ensure nil
  :bind
  (("M-s h ." . highlight-symbol-at-point)
   ("M-s h h" . highlight-regexp)
   ("M-s h l" . highlight-lines-matching-regexp)
   ("M-s h u" . unhighlight-regexp)))

(use-package pulsar
  :defines pulsar-pulse-functions
  :custom
  (pulsar-global-mode t)
  (pulsar-delay 0.06)
  (pulsar-iterations 10)
  (pulsar-face 'pulsar-generic)
  :config
  ;; Extra functions (and hooks below) that should trigger Pulsar. I'm not
  ;; using #'fn syntax here to avoid needing all the forward declarations.
  (add-to-list 'pulsar-pulse-functions 'avy-goto-char-timer)
  (add-to-list 'pulsar-pulse-functions 'avy-goto-end-of-line)
  (add-to-list 'pulsar-pulse-functions 'avy-goto-line)
  (add-to-list 'pulsar-pulse-functions 'beginning-of-buffer)
  (add-to-list 'pulsar-pulse-functions 'end-of-buffer)
  (add-to-list 'pulsar-pulse-functions 'beginning-of-defun)
  (add-to-list 'pulsar-pulse-functions 'end-of-defun)
  (add-to-list 'pulsar-pulse-functions 'flymake-goto-next-error)
  (add-to-list 'pulsar-pulse-functions 'flymake-goto-prev-error)
  (add-to-list 'pulsar-pulse-functions 'pop-global-mark)
  (add-to-list 'pulsar-pulse-functions 'set-mark-command)
  (add-to-list 'pulsar-pulse-functions 'popper-toggle)
  (add-to-list 'pulsar-pulse-functions 'eshell-next-prompt)
  (add-to-list 'pulsar-pulse-functions 'eshell-previous-prompt)
  (add-to-list 'pulsar-pulse-functions 'vterm-next-prompt)
  (add-to-list 'pulsar-pulse-functions 'vterm-previous-prompt)
  (add-to-list 'pulsar-pulse-functions 'xref-find-definitions)
  (add-to-list 'pulsar-pulse-functions 'xref-find-definitions-other-window)

  ;; Some functionality is better accessed via hooks than by registering
  ;; functions in `pulsar-pulse-functions'. See Pulsar docs and Prot's config.
  (add-hook 'minibuffer-setup-hook 'pulsar-pulse-line)
  (add-hook 'next-error-hook 'pulsar-pulse-line-red)
  (add-hook 'next-error-hook 'pulsar-recenter-top)
  (add-hook 'next-error-hook 'pulsar-reveal-entry))

;;;;; Templating

(use-package tempel
  :after nerd-icons-corfu
  :defines nerd-icons-corfu-mapping
  :bind
  (("C-M-/" . tempel-complete)
   (:map tempel-map
    ;; Use tab to select the next/previous field and M-/ to complete within.
    ("<tab>" . tempel-next)
    ("<backtab>" . tempel-previous)
    ([remap keyboard-quit] . tempel-done)))
  :config
  (add-to-list 'nerd-icons-corfu-mapping
               ;; Run `nerd-icons-insert' to see icons.
               '(snippet :style "oct" :icon "heart" :face font-lock-string-face)))

;;;; Buffer Management

;; Use `ibuffer' as a replacement for `list-buffers'.
(use-package ibuffer
  :ensure nil
  :bind
  (("C-x C-b" . ibuffer)
   :map ibuffer-mode-map
   ("M-o" . nil)))

(use-package ibuffer-vc
  :bind
  (:map ibuffer-mode-map
   ("/p" . ibuffer-vc-set-filter-groups-by-vc-root))
  :hook (ibuffer . ibuffer-vc-mode))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

;;;; File System

;;;;; File Browsing

(use-package dired
  :ensure nil
  :bind
  (:map dired-mode-map
   ("M-s" . nil)
   ("N" . dired-create-empty-file)
   ("?" . which-key-show-major-mode)
   ("i" . dired-subtree-insert)
   (";" . dired-subtree-remove)
   :map dired-jump-map
   ;; Allow `dired-goto-file' (via j) straight after jumping with C-x C-j.
   ;; Without this, repeat mode takes over and j calls `dired-jump' again.
   ("j" . nil))
  :hook
  (dired-mode . auto-revert-mode)
  (dired-mode . dired-hide-details-mode)
  :custom
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (delete-by-moving-to-trash t)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-free-space nil)
  (dired-listing-switches "-al --group-directories-first")
  (dired-hide-details-hide-symlink-targets t))

(use-package dired-aux
  :ensure nil
  :custom
  (dired-create-destination-dirs 'ask)
  (dired-create-destination-dirs-on-trailing-dirsep t))

(use-package dired-subtree
  :bind
  (:map dired-mode-map
   ("<tab>" . dired-subtree-toggle)
   ("<backtab>" . dired-subtree-remove))
  :custom
  (dired-subtree-use-backgrounds nil))

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(use-package nerd-icons-dired
  :preface
  (declare-function nerd-icons-dired--refresh-advice "nerd-icons-dired")
  :hook (dired-mode . nerd-icons-dired-mode)
  :config
  ;; Add support for additional subtree commands.
  ;; TODO: Remove after https://github.com/rainstormstudio/nerd-icons-dired/pull/18 is merged.
  (with-eval-after-load 'dired-subtree
    (advice-add 'dired-subtree-insert :around #'nerd-icons-dired--refresh-advice)
    (advice-add 'dired-subtree-remove :around #'nerd-icons-dired--refresh-advice)))

;; Provides the super useful `wdired-change-to-wdired-mode' command.
(use-package wdired
  :ensure nil
  :custom
  (wdired-allow-to-change-permissions t)
  (wdired-create-parent-directories t))

;;;;; File History

(use-package recentf
  :ensure nil
  :custom
  (recentf-mode t)
  (recentf-max-saved-items 300))

;;;;; Project Management

(use-package project
  :ensure nil
  :preface
  (declare-function project-remember-projects-under "project")
  (declare-function project-forget-zombie-projects "project")

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
      (message "Found %d new projects" found)))

  :bind
  ("C-M-&" . project-async-shell-command)
  ("C-x p j" . project-dired)
  ("C-x p u" . my/project-update-list)
  :custom
  (project-prompt-project-dir)
  (project-switch-commands
   '((my/consult-project-multi "File" ?f)
     (project-dired "Dired" ?j)
     (consult-ripgrep "Ripgrep" ?s)
     (magit-project-status "Magit" ?m)
     (project-eshell "Eshell" ?e)
     (my/vterm-project "Vterm" ?v)
     (project-async-shell-command "Async shell" ?&))))

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
  :ensure (:files (:defaults "extensions/*.el"))
  :preface
  ;; From: https://github.com/minad/corfu#completing-in-the-minibuffer.
  (defun my/corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if appropriate."
    (when (local-variable-p 'completion-at-point-functions)
      (setq-local corfu-auto nil)
      (corfu-mode 1)))

  ;; From: https://github.com/minad/corfu#transfer-completion-to-the-minibuffer.
  (defun my/corfu-move-to-minibuffer ()
    (interactive)
    (pcase completion-in-region--data
      (`(,beg ,end ,table ,pred ,extras)
       (let ((completion-extra-properties extras)
             completion-cycle-threshold completion-cycling)
         (consult-completion-in-region beg end table pred)))))

  :bind
  (("M-/" . completion-at-point)
   :map corfu-map
   ("S-SPC" . corfu-insert-separator)
   ("M-m" . my/corfu-move-to-minibuffer)
   ("M-/" . corfu-complete)
   ("<tab>" . corfu-complete))
  :hook (minibuffer-setup . my/corfu-enable-in-minibuffer)
  :custom
  (global-corfu-mode t)
  (corfu-auto nil)
  (corfu-preselect 'first)
  (corfu-quit-at-boundary nil)
  (corfu-on-exact-match 'insert)
  (corfu-count 16)
  (corfu-preview-current nil)
  (corfu-min-width 60)
  (corfu-max-width 80)
  :config
  (add-to-list 'corfu-continue-commands #'my/corfu-move-to-minibuffer))

(use-package corfu-popupinfo
  :after corfu
  :ensure nil
  :bind
  (:map corfu-popupinfo-map
   ("M-d" . corfu-popupinfo-documentation)
   ;; Unfortunately, Eglot doesn't seem to support :company-location which
   ;; is what `corfu-popupinfo' uses to determine the code location for the
   ;; candidate. It does work for Elisp, however.
   ("M-l" . corfu-popupinfo-location)
   ("M-p" . corfu-popupinfo-scroll-down)
   ("M-n" . corfu-popupinfo-scroll-up))
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay nil)
  (corfu-popupinfo-max-height 16))


(use-package corfu-quick
  :after corfu
  :ensure nil
  :bind
  (:map corfu-map
   ("'" . corfu-quick-complete))
  :custom
  (corfu-quick1 "asdfghjkl")
  (corfu-quick2 "asdfghjkl"))

(use-package corfu-history
  :after corfu
  :ensure nil
  :defines savehist-additional-variables
  :custom
  (corfu-history-mode t)
  :init
  ;; Persist Corfu history between Emacs sessions.
  (add-to-list 'savehist-additional-variables 'corfu-history))

(use-package nerd-icons-corfu
  :after corfu
  :preface
  (declare-function nerd-icons-corfu-formatter "nerd-icons-corfu")
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package vertico
  :ensure (:files (:defaults "extensions/*.el"))
  :custom
  (vertico-mode t)
  (vertico-count 8)
  (vertico-count-format nil)
  (vertico-resize nil))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :preface
  (declare-function vertico-sort-history-length-alpha "vertico")
  (defun my/vertico-sort-dirs-first (files)
    "Sorts FILES by directories then alphanumerically."
    (setq files (vertico-sort-history-length-alpha files))
    (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
           (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))
  :bind
  (:map vertico-map
   ;; More convenient directory navigation commands.
   ("RET" . vertico-directory-enter)
   ("DEL" . vertico-directory-delete-char))
  :hook
  ;; Tidy shadowed file names.
  (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-multiform
  :after vertico
  :ensure nil
  :custom
  (vertico-multiform-mode t)
  (vertico-multiform-categories
   '((imenu buffer)
     (file (vertico-sort-function . my/vertico-sort-dirs-first))))

  ;; Sometimes commands are better when the category is too broad.
  (vertico-multiform-commands
   '((consult-outline buffer))))

;; I don't enable `vertico-buffer-mode' directly since this makes Vertico
;; always run in a buffer. Instead, `vertico-multiform' is used to toggle
;; buffer display on a per-category or per-command basis. The configuration
;; of `vertico-buffer-display-action' below changes the default way that
;; Vertico buffers are shown (otherwise they reuse the current window).
(use-package vertico-buffer
  :after vertico
  :ensure nil
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
  :ensure nil
  :hook (minibuffer-setup . vertico-repeat-save)
  :bind
  ("C-c '" . vertico-repeat-select)
  :init
  ;; Persist Vertico history between Emacs sessions.
  (add-to-list 'savehist-additional-variables 'vertico-repeat-history))

(use-package vertico-quick
  :after vertico
  :ensure nil
  :bind
  (:map vertico-map
   ("'" . vertico-quick-exit))
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
  :bind
  ;; For history, default to `cape-history' which is displayed as a Corfu
  ;; popup and use `consult-history' with local keymaps where minibuffer
  ;; history seems more appropriate.
  ("C-c h" . cape-history)
  ("C-c c d" . cape-dabbrev)
  ("C-c c h" . cape-history)
  ("C-c c f" . cape-file)
  ("C-c c k" . cape-keyword)
  ("C-c c o" . cape-elisp-symbol)
  ("C-c c a" . cape-abbrev)
  ("C-c c l" . cape-line)
  ("C-c c w" . cape-dict))

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
  :after flymake
  :defines
  (flymake-mode-map
   xref-show-xrefs-function
   xref-show-definitions-function)
  :preface
  (declare-function consult-xref "consult-xref")
  (declare-function consult-register-format "consult-register")
  (declare-function consult-register-window "consult-register")
  (declare-function consult--customize-put "consult")
  (declare-function consult--file-preview "consult")
  (declare-function consult--read "consult")
  (declare-function consult--file-action "consult")
  (declare-function consult--file-state "consult")
  (declare-function consult--buffer-query "consult")
  (declare-function consult--buffer-state "consult")
  (declare-function project--find-in-directory "project")
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

  (defun my/consult-read-file-name (prompt &optional dir _default mustmatch _initial pred)
    "Function to assign to `read-file-name-function' to enable previewing."
    (interactive)
    (let ((default-directory (or dir default-directory))
          (minibuffer-completing-file-name t))
      (consult--read #'read-file-name-internal :state (consult--file-preview)
                     :prompt prompt
                     :initial (abbreviate-file-name default-directory)
                     ;; Less disruptive for a commonly used command.
                     :preview-key "M-."
                     :require-match mustmatch
                     :predicate pred)))

  (defun my/consult-find-file ()
    "Version of `find-file' that supports preview."
    (interactive)
    (let ((read-file-name-function #'my/consult-read-file-name))
      (call-interactively #'find-file)))

  (defun my/consult-find-file-other-window ()
    "Version of `find-file-other-window' that supports preview."
    (interactive)
    (let ((read-file-name-function #'my/consult-read-file-name))
      (call-interactively #'find-file-other-window)))

  :bind
  (("M-i" . consult-imenu)
   ("M-y" . consult-yank-pop)
   ("M-s a" . consult-org-agenda)
   ("M-s f" . consult-fd)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi)
   ("M-s p" . my/consult-project-file)
   ("M-s s" . consult-ripgrep)
   ("M-s =" . consult-focus-lines)
   ("M-g -" . consult-outline)
   ("M-g f" . consult-flymake)
   ("M-g g" . consult-goto-line)
   ("M-g M-g" . consult-goto-line)
   ("M-g i" . consult-imenu)
   ("M-g I" . consult-imenu-multi)
   ("M-g m" . consult-mark)
   ("M-g M" . consult-global-mark)
   ("C-c x t" . consult-theme)
   ("C-x b" . consult-buffer)
   ("C-x 4 b" . consult-buffer-other-window)
   ("C-x 4 f" . my/consult-find-file-other-window)
   ("C-x 5 b" . consult-buffer-other-frame)
   ("C-x t b" . consult-buffer-other-tab)
   ("C-x p b" . consult-project-buffer)
   ("C-x p f" . my/consult-project-file)
   ("C-x r b" . consult-bookmark)
   ("C-x r r" . consult-register)
   ("C-x r l" . consult-register-load)
   ("C-x r s" . consult-register-store)
   ("C-x C-f" . my/consult-find-file)
   ("C-x C-k k" . consult-kmacro)
   :map minibuffer-local-map
   ("M-s" . nil)
   :map consult-narrow-map
   ("C-," . consult-narrow-help)
   ("?" . consult-narrow-help)
   :map flymake-mode-map
   ("C-c f f" . consult-flymake)
   :map isearch-mode-map
   ("C-c h" . consult-isearch-history)
   :map minibuffer-local-map
   ("C-c h" . consult-history))

  :hook
  ;; Use the previously selected theme else default to Modus Vivendi.
  (elpaca-after-init . (lambda ()
                         (consult-theme (if consult--theme-history
                                            (intern (car consult--theme-history))
                                          'modus-vivendi))))

  :custom
  ;; Type ',' followed by a prefix key to narrow the available candidates.
  ;; Type C-, (defined above) to display prefix help. Alternatively, type
  ;; ',' followed by C-h (or ?) to call `embark-prefix-help-command'. If
  ;; ',' is problematic I'll search for another key.
  (consult-narrow-key ",")

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

  ;; Configure both `config-find' and `consult-fd' to follow symlinks, include
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
  ;; Customize the list of sources shown by `consult-buffer'.
  (setq consult-buffer-sources
        '(consult--source-buffer                ;; Narrow: ?b (shown)
          consult--source-project-buffer-hidden ;; Narrow: ?p (hidden)
          my/consult-source-dired-buffer        ;; Narrow: ?d (hidden)
          my/consult-source-shell-buffer        ;; Narrow: ?s (hidden)
          consult--source-file-register         ;; Narrow: ?g (shown)
          consult--source-bookmark              ;; Narrow: ?m (shown)
          consult--source-recent-file))         ;; Narrow: ?r (hidden)

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
  :preface
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

  :bind
  ("M-s d" . consult-dir)
  :custom
  (consult-dir-shadow-filenames nil)
  (consult-dir-default-command #'find-file)
  :config
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
  :preface
  ;; This command has the downside that leaves all previewed files open.
  (defun my/embark-force-preview ()
    "Force a preview of the current candidate for non-Consult commands."
    (interactive)
    (unless (bound-and-true-p consult--preview-function)
      (save-selected-window
        (let ((embark-quit-after-action nil))
          (embark-dwim)))))

  :commands embark-prefix-help-command
  :bind
  (("C-." . embark-act)
   ("C-M-." . embark-dwim)
   ("C-h b" . embark-bindings)
   :map minibuffer-local-map
   ("M-." . my/embark-force-preview))

  :custom
  ;; Just show the minimal "Act" prompt (the default starts with minimal
  ;; and then `embark-mixed-indicator-delay' kicks in and the verbose screen
  ;; is shown). Shortcut keys can immediately be used. C-h can be pressed to
  ;; bring up the completing read prompter.
  (embark-indicators (list #'embark-minimal-indicator))
  ;; Use this key to switch Embark to the keymap prompter.
  (embark-keymap-prompter-key ",")

  :init
  ;; Use Embark to show keybindings under a prefix rather than the default
  ;; `describe-prefix-bindings'. It should be possible to just use set
  ;; `prefix-help-command' but it keeps getting reverted so doing it this way.
  (with-eval-after-load 'help
    (fset #'describe-prefix-bindings #'embark-prefix-help-command))

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

;;;; Search and Replace

(use-package isearch
  :ensure nil
  :bind
  (("M-s ." . isearch-forward-thing-at-point)
   :map isearch-mode-map
   ;; The default `isearch-abort' requires multiple C-g if search not found.
   ("C-g" . isearch-cancel)))

(use-package occur
  :ensure nil
  :hook
  (occur-mode . hl-line-mode)
  (occur-mode . my/truncate-lines))

(use-package rg
  :bind
  ("M-s M-s" . rg)
  ("M-s M-l" . rg-literal))

(use-package wgrep
  :bind
  ("C-c w" . wgrep-change-to-wgrep-mode)
  :custom
  (wgrep-auto-save-buffer t))

(use-package substitute
  :commands substitute-report-operation
  :bind
  ("M-# s" . substitute-target-below-point)
  ("M-# r" . substitute-target-above-point)
  ("M-# d" . substitute-target-in-defun)
  ("M-# b" . substitute-target-in-buffer)
  :config
  (add-to-list 'substitute-post-replace-functions #'substitute-report-operation))

;;;; Programming

;;;;; General Programming

;;;;;; Tree-sitter

(use-package treesit
  :ensure nil
  :custom
  (treesit-font-lock-level 4)
  (treesit-extra-load-path
   (list (no-littering-expand-var-file-name "treesit-auto"))))

(use-package treesit-auto
  :preface
  (declare-function treesit-auto-install-all "treesit-auto")

  ;; For now, to upgrade grammars, delete ~/.config/emacs/var/treesit-auto,
  ;; re-open Emacs, and then run `my/treesit-auto-install-all'.
  (defun my/treesit-auto-install-all ()
    "Wrapper around `treesit-auto-install-all' that respects no-littering."
    (interactive)
    (treesit-auto-install-all)
    (let ((old-dir (expand-file-name "tree-sitter"  user-emacs-directory))
          (new-dir (car treesit-extra-load-path)))
      (delete-directory new-dir t)
      (rename-file old-dir new-dir)))

  :commands treesit-auto-add-to-auto-mode-alist
  :custom
  (global-treesit-auto-mode t)
  (treesit-auto-install 'prompt)
  ;; Use TS-powered modes for a smaller set of languages for now.
  ;; See original value of `treesit-auto-langs' for the full set.
  (treesit-auto-langs '(bash dockerfile go gomod proto python rust))
  :init
  (treesit-auto-add-to-auto-mode-alist treesit-auto-langs))

;;;;;; Eglot

(use-package eglot
  ;; Use latest. See: https://www.reddit.com/r/emacs/comments/16yny40/how_to_use_the_latest_version_of_eglot_with_elpaca.
  :ensure (:inherit elpaca-menu-gnu-devel-elpa)
  :preface
  (defun my/eglot-init ()
    "Init function for `eglot--managed-mode'."
    (eglot-inlay-hints-mode 1)
    (setq-local completion-at-point-functions
                (list
                 #'eglot-completion-at-point
                 #'cape-file)))

  :bind
  (:map eglot-mode-map
   ("M-g ^" . eglot-find-implementation)
   ("C-c r a" . eglot-code-actions)
   ("C-c r o" . eglot-code-action-organize-imports)
   ("C-c r r" . eglot-rename))

  :hook
  ((go-mode
    go-ts-mode
    rust-mode
    rust-ts-mode
    sh-mode
    bash-ts-mode
    haskell-mode) . eglot-ensure)
  (eglot-managed-mode . my/eglot-init)

  :custom
  (eglot-autoshutdown t)
  (eglot-connect-timeout 60)
  (eglot-confirm-server-edits nil)
  (eglot-extend-to-xref t)
  (eglot-ignored-server-capabilities
   '(:documentFormattingProvider :documentRangeFormattingProvider
     :documentOnTypeFormattingProvider :documentHighlightProvider))

  :config
  ;; Tree-sitter produces a better imenu.
  (setq eglot-stay-out-of '(imenu))

  ;; Potential performance improving settings.
  ;; See: https://www.reddit.com/r/emacs/comments/1b25904/is_there_anything_i_can_do_to_make_eglots.
  (fset #'jsonrpc--log-event #'ignore)
  (setf (plist-get eglot-events-buffer-config :size) 0)

  ;; Remove completion styles installed by Eglot and to fall back to Orderless.
  (setq completion-category-defaults nil)

  ;; Style the inlay type hinting face.
  (set-face-attribute 'eglot-inlay-hint-face nil :height 0.9 :slant 'italic)

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

(use-package eglot-x
  :ensure (:host github :repo "nemethf/eglot-x")
  :hook  (eglot-managed-mode . eglot-x-setup))

;; Speed up Eglot.
(use-package eglot-booster
  :ensure (:host github :repo "jdtsmith/eglot-booster")
  :hook (eglot-managed-mode . eglot-booster-mode))

;; Adapt Eglot to use Tempel rather than Yasnippet for placeholder completion.
(use-package eglot-tempel
  :commands eglot-tempel-mode
  ;; :after (eglot tempel)
  :init
  (eglot-tempel-mode))

(use-package consult-eglot
  :bind
  (:map eglot-mode-map
   ("M-g o" . consult-eglot-symbols)))

;;;;;; Eldoc

(use-package eldoc
  :ensure nil
  :bind
  ("C-h t" . eldoc-mode)
  :custom
  (global-eldoc-mode t)
  (eldoc-idle-delay 0.5)
  (eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-echo-area-display-truncation-message t)
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
  :preface
  (declare-function eldoc-box-help-at-point "eldoc-box")
  (declare-function eldoc-box-quit-frame "eldoc-box")

  (defun my/eldoc-box-visible-p ()
    "Return whether the `eldoc-box' popup is visible."
    (and eldoc-box--frame (frame-visible-p eldoc-box--frame)))

  (defun my/eldoc-box-toggle ()
    "Toggle the `eldoc-box-help-at-point' popup."
    (interactive)
    (require 'eldoc-box)
    (if (my/eldoc-box-visible-p)
        (eldoc-box-quit-frame)
      (eldoc-box-help-at-point)))

  ;; Workaround to ensure the correct documentation is shown by the `eldoc-box'
  ;; popup if Eldoc is updated. See: https://github.com/casouri/eldoc-box/issues/96.
  (defun my/eldoc-display-in-eldoc-box (&rest _)
    "Display latest Eldoc buffer in `eldoc-box' if visible."
    (when (my/eldoc-box-visible-p)
      (eldoc-box-help-at-point)))

  :bind
  ("M-p" . my/eldoc-box-toggle)
  :custom
  (eldoc-box-clear-with-C-g t)
  :config
  (add-to-list 'eldoc-display-functions #'my/eldoc-display-in-eldoc-box))

;;;;;; Flymake

(use-package flymake
  :ensure nil
  :preface
  (declare-function flymake-proc-legacy-flymake "flymake-proc")
  (defun my/elisp-flymake-byte-compile (fn &rest args)
    "Wrapper for `elisp-flymake-byte-compile' that considers `load-path'."
    ;; Don't let Vterm onto Flymake's load path as it oddly blocks it.
    (let* ((alt-load-path (seq-filter (lambda (d) (not (string-match-p "elpaca/builds/vterm" d))) load-path))
           (elisp-flymake-byte-compile-load-path (append elisp-flymake-byte-compile-load-path alt-load-path)))
      (apply fn args)))

  :bind
  (:map flymake-mode-map
   ("C-c f d" . flymake-show-buffer-diagnostics)
   ("C-c f D" . flymake-show-project-diagnostics)
   ("C-c f n" . flymake-goto-next-error)
   ("C-c f p" . flymake-goto-prev-error)
   :repeat-map my/flymake-repeat-map
   ("n" . flymake-goto-next-error)
   ("p" . flymake-goto-prev-error))

  :hook (prog-mode . flymake-mode)
  :custom
  (flymake-no-changes-timeout 0.5)
  (flymake-start-on-save-buffer)
  :config
  (advice-add #'elisp-flymake-byte-compile :around #'my/elisp-flymake-byte-compile)
  (remove-hook 'flymake-diagnostic-functions #'flymake-proc-legacy-flymake))

(use-package sideline-flymake
  :hook (flymake-mode . sideline-mode)
  :custom
  ;; Using the left side always displays even when using smaller windows.
  (sideline-backends-left '(sideline-flymake))
  (sideline-flymake-display-mode 'point))

;;;;;; Xref

(use-package xref
  :ensure nil
  :custom
  ;; Don't prompt by default (invoke with prefix arg to prompt).
  (xref-prompt-for-identifier nil))

;;;;;; Outline

(use-package outline
  :ensure nil
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

;;;;;; Rust

(use-package rust-ts-mode
  :ensure nil
  :defines consult-imenu-config
  :preface
  (declare-function treesit-node-text "treesit")
  (declare-function rust-ts-mode--defun-name "rust-ts-mode")

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

  :hook (rust-ts-mode . my/rust-ts-mode-init)
  :config
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

;; Rustic doesn't yet integrate with Tree-sitter and so I just use Rustic for
;; it's useful commands while keeping `rust-ts-mode' as the major mode for
;; Rust files. See: https://github.com/brotzeit/rustic/issues/475.
(use-package rustic
  :after rust-ts-mode
  :preface
  ;; Every once in a while, Rustic will get used as the major mode instead of
  ;; `rust-ts-mode'. This is an attempt to prevent that from happening.
  (add-to-list 'major-mode-remap-alist '(rustic-mode . rust-ts-mode))
  :bind
  (:map rustic-compilation-mode-map
   ("p" . previous-error-no-select)
   :map rust-ts-mode-map
   ;; TODO: Look at latest Rustic commands.
   ("C-c b a" . rustic-cargo-add)
   ("C-c b b" . rustic-cargo-build)
   ("C-c b c" . rustic-cargo-clean)
   ("C-c b f" . rustic-cargo-fmt)
   ("C-c b l" . rustic-cargo-clippy)
   ("C-c b o" . rustic-cargo-outdated)
   ("C-c b r" . rustic-cargo-run)
   ("C-c b u" . rustic-cargo-update)
   ("C-c b U" . rustic-cargo-upgrade)
   ("C-c t ." . rustic-cargo-test-dwim)
   ("C-c t t" . rustic-cargo-current-test)))

;;;;;; Go

(use-package go-ts-mode
  :ensure nil
  :preface
  (declare-function treesit-node-text "treesit")
  (declare-function go-ts-mode--defun-name "go-ts-mode")

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

  (defun my/treesit-go-defun-name (node)
    "Return the defun name of NODE for Go node types."
    ;; See possible node types here:
    ;; https://github.com/tree-sitter/tree-sitter-go/blob/bbaa67a180cfe0c943e50c55130918be8efb20bd/src/node-types.json.
    (pcase (treesit-node-type node)
      ("const_spec"
       (treesit-node-text (treesit-node-child-by-field-name node "name") t))
      ("var_spec"
       (treesit-node-text (treesit-node-child-by-field-name node "name") t))
      (_ (go-ts-mode--defun-name node))))

  :hook (go-ts-mode . my/go-ts-mode-init)
  :custom
  (go-ts-mode-indent-offset 4)
  :config
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
  :after go-ts-mode
  :preface
  (defun my/go-test-verbose (fn)
    "Run the Go test function FN with the verbose flag enabled."
    (require 'gotest)
    (let ((go-test-verbose t))
      (funcall fn)))

  :bind
  (:map go-ts-mode-map
   ("C-c b r" . go-run)
   ("C-c t f" . go-test-current-file)
   ("C-c t t" . go-test-current-test)
   ("C-c t p" . go-test-current-project)
   ("C-c t b" . go-test-current-benchmark)
   ("C-c t c" . go-test-current-coverage)
   ("C-c t T" . (lambda () (interactive) (my/go-test-verbose #'go-test-current-test)))
   ("C-c t F" . (lambda () (interactive) (my/go-test-verbose #'go-test-current-file)))
   ("C-c t P" . (lambda () (interactive) (my/go-test-verbose #'go-test-current-project)))))

(use-package go-gen-test
  :after go-ts-mode
  :bind
  (:map go-ts-mode-map
   ("C-c t g" . go-gen-test-dwim)))

(use-package go-playground
  :preface
  (declare-function go-playground "go-playground")
  (declare-function go-playground-insert-template-head "go-playground")
  :custom
  (go-playground-init-command "go mod init playground")
  :config
  ;; Don't insert all the preamble which contains special comments to set the
  ;; mode to `go-mode' rather than `go-ts-mode'.
  (fset #'go-playground-insert-template-head #'ignore)
  ;; Switch `go-mode' for `go-ts-mode'. I'd prefer using `major-mode-remap-alist'
  ;; to perform this mapping but it doesn't seem to work when the major mode
  ;; is called directly like it is by `go-playground'.
  (advice-add #'go-playground :after #'go-ts-mode))

;;;;;; Haskell

(use-package haskell-mode)

;;;;;; Terraform

(use-package terraform-mode)

;;;;;; Python

(use-package python
  :ensure nil
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
  :ensure nil
  :custom
  (sh-basic-offset 2))

;;;;;; Protobuf

(use-package protobuf-ts-mode
  :ensure (:host github :repo "ashlineldridge/protobuf-ts-mode")
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
  :ensure (:host github :repo "bazelbuild/emacs-bazel-mode")
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
  :ensure nil)

(use-package clang-format
  :custom
  (clang-format-fallback-style "llvm"))

;;;;;; YAML

(use-package yaml-mode)

;;;;;; Lisp

(use-package elisp-mode
  :ensure nil
  :preface
  (defun my/elisp-init ()
    "Init function for `emacs-lisp-mode'."
    (setq-local outline-regexp ";;;+ [^\n]")
    (outline-minor-mode 1)
    (setq-local completion-at-point-functions
                (list
                 #'elisp-completion-at-point
                 #'cape-file)))
  :bind
  (:map emacs-lisp-mode-map
   ("C-x C-r" . eval-region))
  :hook
  (emacs-lisp-mode . my/elisp-init)
  :config
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
  ;; Unbind keybindings that collide with things I find more useful.
  (dolist (key '("C-c C-M-l" "C-<left>" "C-<right>" "C-M-<left>" "C-M-<right>"
                 "M-?" "M-<up>" "M-<down>" "M-S" "M-J" "M-q" "M-r" "M-s"))
    (define-key paredit-mode-map (kbd key) nil)))

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
  :bind
  (:map aggressive-indent-mode-map
   ("C-c C-q" . nil)))

;; Custom Elisp indentation function - see code comments in package.
(use-package emacs-lisp-indent
  :ensure (:host github :repo "ashlineldridge/emacs-lisp-indent")
  :init
  (emacs-lisp-indent-install))

;;;;;; SGML/HTML

(use-package sgml-mode
  :ensure nil
  :bind
  (:map html-mode-map
   ("M-o" . nil)))

;;;;;; Markdown

(use-package markdown-mode
  :commands gfm-mode
  :mode ("\\.md\\'" . gfm-mode)
  :custom
  (markdown-command "multimarkdown"))

;;;; Git

(use-package magit
  :bind
  ("C-c g c" . magit-clone)
  ("C-c g s" . magit-status)
  ("C-c g d" . magit-dispatch)
  ("C-c g f" . magit-file-dispatch)
  :config
  ;; Speed up Magit by removing a bunch of the slower status hook functions
  ;; that add details to the status buffer. It makes it look more bare-bones
  ;; but the speed increase is worth it. Run M-x `magit-toggle-verbose-refresh'
  ;; to see how long each function takes.
  (remove-hook 'magit-status-sections-hook 'magit-insert-status-headers)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
  (remove-hook 'magit-status-headers-hook 'magit-insert-tags-header))

(use-package difftastic)

(use-package browse-at-remote
  :bind
  (("C-c g o" . browse-at-remote)
   ("C-c g k" . browse-at-remote-kill)))

;;;; Shell/Terminal

(use-package eshell
  :ensure nil
  :defines eshell-prompt-regexp
  :preface
  (declare-function eshell-bol "esh-mode")
  (declare-function eshell-read-aliases-list "esh-alias")
  (declare-function eshell/pwd "em-dirs")
  (declare-function eshell-save-some-history "em-hist")
  (declare-function pcomplete-completions-at-point "pcomplete")

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
      buf))

  (defun my/eshell-kill-whole-line ()
    "Eshell version of `kill-whole-line' that respects the prompt, if any."
    (interactive)
    (eshell-bol)
    (kill-line))

  :bind
  ("C-c e" . eshell)
  :hook
  (eshell-mode . my/eshell-init)
  (eshell-pre-command . my/eshell-pre-command)
  (eshell-post-command . my/eshell-post-command)
  :custom
  (eshell-history-size 10000)
  (eshell-buffer-maximum-lines 10000)
  (eshell-hist-ignoredups t)
  (eshell-prompt-function #'my/eshell-prompt)
  (eshell-banner-message "Welcome to Eshell\n\n")
  ;; The following commands will be started in `term-mode'.
  (eshell-visual-commands '("vi" "vim" "htop" "btm" "watch")))

(use-package esh-mode
  :ensure nil
  :bind
  (:map eshell-mode-map
   ("C-c C-<backspace>" . eshell-kill-output)
   ("C-c C-SPC" . eshell-mark-output)
   ("C-S-<backspace>" . my/eshell-kill-whole-line)))

(use-package em-hist
  :ensure nil
  :bind
  (:map eshell-hist-mode-map
   ("M-s" . nil)
   ("M-r" . nil)))

(use-package vterm
  :defines (vterm-mode-map vterm-eval-cmds)
  :preface
  (declare-function vterm-send-key "vterm")
  (declare-function vterm-previous-prompt "vterm")
  (declare-function vterm-next-prompt "vterm")
  (declare-function project-prefixed-buffer-name "project")

  (defun my/vterm-init ()
    "Hook function executed when `vterm-mode' is run."
    ;; Make outline work with vterm prompts.
    (setq-local outline-regexp "^[^#$\n]* ❯ "))

  (defun my/vterm-project ()
    "Start a Vterm in the current project's root directory."
    (interactive)
    (let* ((default-directory (project-root (project-current t)))
           (buffer-name (project-prefixed-buffer-name "vterm"))
           (existing-buffer (get-buffer buffer-name)))
      (if (and existing-buffer (not current-prefix-arg))
          (pop-to-buffer existing-buffer)
        (vterm buffer-name))))

  :bind
  (("C-c v" . vterm)
   :map vterm-mode-map
   ;; Same keybinding for shell history as `consult-history' and `cape-history'.
   ("C-c h" . (lambda () (interactive) (vterm-send-key (kbd "C-r"))))
   :repeat-map my/vterm-repeat-map
   ("C-n" . vterm-next-prompt)
   ("C-p" . vterm-previous-prompt))

  :hook (vterm-mode . my/vterm-init)
  :custom
  (vterm-always-compile-module t)
  ;; I'd much prefer to NOT clear scrollback but I can't rely on it due to
  ;; https://github.com/akermu/emacs-libvterm/issues/546.
  (vterm-clear-scrollback-when-clearing t)
  (vterm-max-scrollback 10000)

  :config
  ;; Allow find-file-other-window to be called from Vterm.
  (add-to-list 'vterm-eval-cmds
               '("find-file-other-window" find-file-other-window))

  ;; Unset a bunch of keybindings that I want to keep.
  (dolist (key '("C-o" "C-r" "C-s" "C-SPC"
                 "M-s" "M-g" "M-:" "M-&" "M-'"
                 "M-H" "M-J" "M-K" "M-L"
                 "C-M-H" "C-M-J" "C-M-K" "C-M-L"))
    (define-key vterm-mode-map (kbd key) nil)))

(use-package sh-script
  :ensure nil
  :bind
  (:map sh-mode-map
   ("C-c C-o" . nil)))

;;;; Org Mode

(use-package org
  :ensure nil
  :preface
  (declare-function org-get-tags "org")
  (declare-function org-refile-get-targets "org-refile")

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

  ;; Extra electric pairs to use in org mode.
  (defvar my/org-extra-electric-pairs '((?/ . ?/) (?= . ?=)))

  (defun my/org-init ()
    "Init function for `org-mode'."
    (interactive)
    (visual-line-mode 1)
    (variable-pitch-mode 1)
    (display-line-numbers-mode 0)
    (setq-local line-spacing 2)
    (setq-local completion-at-point-functions (list #'cape-file))
    (setq-local electric-pair-pairs
                (append electric-pair-pairs my/org-extra-electric-pairs))
    (setq-local electric-pair-text-pairs
                (append electric-pair-text-pairs my/org-extra-electric-pairs)))

  :bind
  (("C-c C-o" . org-open-at-point-global)
   ("C-c o l" . org-store-link)
   ("C-c o s" . org-save-all-org-buffers)
   ("C-c o i" . (lambda () (interactive) (org-capture nil "i")))
   ("C-c o b" . (lambda () (interactive) (org-capture nil "b")))
   ("C-c o c" . (lambda () (interactive) (org-capture nil "c")))
   ("C-c o m" . (lambda () (interactive) (org-capture nil "m")))
   :map org-mode-map
   ("C-'" . nil)
   ("M-S-<up>" . nil)
   ("M-S-<down>" . nil)
   ("C-c C-S-l" . org-cliplink))
  :hook (org-mode . my/org-init)
  :custom
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
     (,my/gtd-someday-file :level . 1)))
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

  :config
  ;; Make it easier to create `org-babel' code blocks.
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp")))

(use-package org-agenda
  :ensure nil
  :preface
  (declare-function org-agenda-quit "org-agenda")

  ;; Order of Org agenda items.
  (defvar my/org-agenda-todo-sort-order '("PROG" "NEXT" "TODO" "HOLD" "DONE"))
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

  :bind
  (("C-c o a" . org-agenda)
   :map org-agenda-mode-map
   ("r" . nil) ;; Allows 'r' to be bound as a prefix key.
   ("r r" . org-agenda-refile)
   ("r p" . my/org-agenda-refile-personal)
   ("r w" . my/org-agenda-refile-work)
   ("r a" . my/org-agenda-refile-archive)
   ("r s" . my/org-agenda-refile-someday)
   ("r i" . my/org-agenda-refile-inbox)
   ("k" . org-agenda-kill)
   ("?" . which-key-show-major-mode))
  :hook
  (org-agenda-mode . hl-line-mode)
  (org-agenda-mode . (lambda () (setq-local default-directory org-directory)))
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

  :config
  ;; Save all org buffers before quitting the agenda ('s' saves immediately).
  (advice-add #'org-agenda-quit :before #'org-save-all-org-buffers))

(use-package org-cliplink)

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org-appear
  :ensure (:host github :repo "awth13/org-appear")
  :hook (org-mode . org-appear-mode)
  :custom
  (org-appear-autolinks t)
  (org-appear-delay 1.0))

(use-package org-roam
  :preface
  (declare-function org-roam-db-autosync-mode "org-roam")
  :bind
  ("C-c n l" . org-roam-buffer-toggle)
  ("C-c n f" . org-roam-node-find)
  ("C-c n g" . org-roam-graph)
  ("C-c n i" . org-roam-node-insert)
  ("C-c n c" . org-roam-capture)
  ("C-c n t" . org-roam-tag-add)
  :custom
  (org-roam-directory (expand-file-name "notes/" my/pkm-dir))
  ;; Disable `org-roam' completion as it's a bit annoying.
  (org-roam-completion-everywhere nil)
  (org-roam-completion-functions nil)
  (org-roam-node-display-template
   (concat "${title:60} " (propertize "${tags:*}" 'face 'org-tag)))
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

;;;; Time

(use-package time
  :ensure nil
  :custom
  (display-time-mode nil)
  (display-time-default-load-average nil)
  (display-time-format "%H:%M")
  ;; Timezones to be displayed by `world-clock'. Zones names can be found
  ;; here: https://www.timezoneconverter.com/cgi-bin/tzc.
  (world-clock-list
   '(("UTC" "UTC")
     ("Australia/Melbourne" "Melbourne")
     ("America/Los_Angeles" "Seattle")
     ("America/New_York" "NY")
     ("Europe/London" "London")
     ("Europe/Paris" "Paris")
     ("Europe/Vilnius" "Lithuania")
     ("Asia/Tokyo" "Tokyo")
     ("Canada/Eastern" "Toronto"))))

;; See `tzc-world-clock' and `tzc-convert-time'.
(use-package tzc
  :custom
  (tzc-favourite-time-zones-alist world-clock-list))

;;;; Spelling

(use-package jinx
  :bind
  ("M-$" . jinx-correct)
  ("C-M-$" . jinx-languages)
  ("C-c x j" . jinx-mode))

;;;; Emacs Package Management

(use-package elpaca
  :ensure nil
  :bind
  ("C-c p p" . elpaca-manager)
  ("C-c p f" . elpaca-fetch)
  ("C-c p F" . elpaca-fetch-all)
  ("C-c p m" . elpaca-merge)
  ("C-c p M" . elpaca-merge-all)
  ("C-c p b" . elpaca-browse)
  ("C-c p d" . elpaca-delete)
  ("C-c p t" . elpaca-try)
  ("C-c p r" . elpaca-rebuild)
  ("C-c p v" . elpaca-visit))

;;;; Process Management

(use-package proced
  :ensure nil
  :bind
  ("C-x C-p" . proced)
  :custom
  (proced-enable-color-flag t))

;;; End:
(provide 'init)

;;; init.el ends here
