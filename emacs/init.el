;;; init.el --- Emacs Init -*- lexical-binding: t -*-

;; Author: Ashlin Eldridge <ashlin.eldridge@gmail.com>
;; URL: https://github.com/ashlineldridge/.config
;; Version: 1.0.0
;; Package-Requires: ((emacs "30.0"))

;;; Commentary:
;;
;; My bonsai.

;;; Code:

;;;; Bootstrap

(load (expand-file-name "bootstrap.el" user-emacs-directory))

;;;; Appearance

;;;;; Themes

(use-package theme
  :ensure nil
  :preface
  (defvar my/default-theme 'ef-dream)
  (defvar my/variable-pitch-headings
    '((1 . (variable-pitch semibold 1.2))
      (t . (variable-pitch semibold 1.1))))
  :no-require
  :hook
  (elpaca-after-init . (lambda () (load-theme my/default-theme :no-confirm))))

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

;;;;; Fonts

(use-package fontaine
  :preface
  (defconst my/fixed-family "Iosevka Comfy")
  (defconst my/variable-family "Iosevka Comfy Motion Duo")
  (defun my/fontaine-apply-preset ()
    "Apply the current (or default) Fontaine preset."
    (fontaine-set-preset (or fontaine-current-preset 'regular)))
  :bind
  ("C-c x f" . fontaine-set-preset)
  :hook
  (elpaca-after-init . my/fontaine-apply-preset)
  :custom
  (fontaine-presets
   `((regular)
     (large
      :default-family ,my/fixed-family
      :default-height 140
      :default-weight semibold
      :fixed-pitch-weight semibold
      :variable-pitch-weight semibold
      :mode-line-active-height 140
      :mode-line-inactive-height 140
      :line-number-height 130)
     (t
      :default-family ,my/fixed-family
      :default-height 130
      :default-weight regular
      :fixed-pitch-family ,my/fixed-family
      :fixed-pitch-height 1.0
      :fixed-pitch-weight regular
      :variable-pitch-family ,my/variable-family
      :variable-pitch-height 1.0
      :variable-pitch-weight regular
      :mode-line-active-family ,my/fixed-family
      :mode-line-inactive-family ,my/fixed-family
      :mode-line-active-height 130
      :mode-line-inactive-height 130
      :line-number-family ,my/fixed-family
      :line-number-slant italic
      :line-number-height 120
      :bold-weight bold
      :italic-slant italic))))

(use-package font-lock
  :ensure nil
  :preface
  ;; Create face aliases to be used in `consult-imenu-config'.
  (put 'my/imenu-constant-face 'face-alias 'font-lock-constant-face)
  (put 'my/imenu-enum-face 'face-alias 'font-lock-type-face)
  (put 'my/imenu-function-face 'face-alias 'font-lock-function-name-face)
  (put 'my/imenu-impl-face 'face-alias 'font-lock-type-face)
  (put 'my/imenu-package-face 'face-alias 'font-lock-constant-face)
  (put 'my/imenu-macro-face 'face-alias 'font-lock-preprocessor-face)
  (put 'my/imenu-method-face 'face-alias 'font-lock-function-name-face)
  (put 'my/imenu-module-face 'face-alias 'font-lock-constant-face)
  (put 'my/imenu-static-face 'face-alias 'font-lock-constant-face)
  (put 'my/imenu-struct-face 'face-alias 'font-lock-type-face)
  (put 'my/imenu-trait-face 'face-alias 'font-lock-type-face)
  (put 'my/imenu-type-face 'face-alias 'font-lock-type-face)
  (put 'my/imenu-variable-face 'face-alias 'font-lock-variable-name-face))

(use-package face-remap
  :ensure nil
  :config
  ;; Makes things like breadcrumb look better when scaling text.
  (setq-default text-scale-remap-header-line t))

;;;;; Icons

;; Run `nerd-icons-install-fonts' manually to install fonts for the first time.
(use-package nerd-icons)

(use-package nerd-icons-ibuffer
  :after ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode)
  :custom
  (nerd-icons-ibuffer-formats ibuffer-formats))

;; Enables Vertico icons.
(use-package nerd-icons-completion
  :hook (vertico-mode . nerd-icons-completion-mode))

(use-package nerd-icons-corfu
  :after corfu
  :defines corfu-margin-formatters
  :init
  (add-to-list 'corfu-margin-formatters 'nerd-icons-corfu-formatter)
  :config
  ;; Make `tempel-complete' look nice (use `nerd-icons-insert' to see icons).
  (add-to-list 'nerd-icons-corfu-mapping
               '(snippet :style "oct" :icon "heart" :face font-lock-string-face)))

;;;;; Mode Line

(use-package smol
  :if (file-directory-p "~/dev/home/smol")
  :load-path "~/dev/home/smol"
  :hook
  (elpaca-after-init . smol-init)
  :custom
  (smol-string-truncate-length 80))

;;;;; Point/Cursor

(use-package cursory
  :commands cursory-set-preset
  :hook
  (org-mode . (lambda () (cursory-set-preset 'bar :local)))
  :init
  (cursory-set-preset 'box))

;;;;; Margins/Padding

(use-package spacious-padding
  :preface
  (defun my/spacious-padding-toggle-subtle-mode-line ()
    "Toggle whether the mode line is displayed in the subtle style."
    (interactive)
    (setq spacious-padding-subtle-mode-line
          (not spacious-padding-subtle-mode-line))
    (when spacious-padding-mode
      (spacious-padding-mode -1)
      (spacious-padding-mode 1)))
  :hook
  (elpaca-after-init . spacious-padding-mode)
  :bind
  ("C-c x _" . my/spacious-padding-toggle-subtle-mode-line)
  :custom
  (spacious-padding-subtle-mode-line nil)
  (spacious-padding-widths
   '(:internal-border-width 25
     :right-divider-width 25
     :fringe-width 8
     :mode-line-width 5)))

;;;;; Lines and Columns

(use-package display-line-numbers
  :ensure nil
  :bind
  ("<f9>" . display-line-numbers-mode))

(use-package display-fill-column-indicator
  :ensure nil
  :bind
  ("C-c x |" . display-fill-column-indicator-mode)
  :custom
  (fill-column 80))

;;;; Repeating

(use-package repeat
  :ensure nil
  :hook (elpaca-after-init . repeat-mode)
  :custom
  (repeat-keep-prefix t)
  (repeat-exit-timeout 5)
  (repeat-exit-key "RET"))

;;;; Bookmarks

(use-package bookmark
  :ensure nil
  :custom
  ;; Save bookmarks immediately.
  (bookmark-save-flag 0))

;;;; Registers

(use-package register
  :ensure nil
  :preface
  (defun my/clear-registers ()
    "Clear all registers."
    (interactive)
    (let ((len (length register-alist)))
      (setq register-alist nil)
      (message "Cleared %d registers" len)))
  :bind
  ("C-x r K" . my/clear-registers)
  :custom
  (register-preview-delay 1)
  :config
  ;; Make `insert-register' obey `delete-selection-mode'.
  (advice-add #'insert-register :before #'my/maybe-delete-selection))

;;;; General History

(use-package savehist
  :ensure nil
  :hook (elpaca-after-init . savehist-mode)
  :custom
  (history-length 1000)
  (history-delete-duplicates t)
  (savehist-save-minibuffer-history t)
  ;; Variables to persist between sessions.
  (savehist-additional-variables
   '(kill-ring
     search-ring
     regexp-search-ring
     extended-command-history
     ;; Not perfect as doesn't persist if contains certain register types.
     register-alist)))

;;;; Windows and Frames

(use-package ns-win
  :ensure nil
  :custom
  (mac-command-modifier 'meta)
  (mac-option-modifier nil))

;;;;; Window Basics

(use-package window
  :ensure nil
  :bind
  (("M-[" . previous-buffer)
   ("M-]" . next-buffer)
   ("M-q" . bury-buffer)
   ("M-O =" . balance-windows)
   ("M-O }" . enlarge-window-horizontally)
   ("M-O {" . shrink-window-horizontally)
   ("M-O ]" . enlarge-window)
   ("M-O [" . shrink-window)
   :repeat-map my/window-repeat-map
   ("}" . enlarge-window-horizontally)
   ("{" . shrink-window-horizontally)
   ("]" . enlarge-window)
   ("[" . shrink-window))
  :custom
  (even-window-sizes nil)
  ;; Prefer splitting by width and only when the window is quite wide.
  (split-height-threshold nil)
  (split-width-threshold 200)
  ;; WORK-IN-PROGRESS: Display buffer configuration.
  ;; See: https://protesilaos.com/codelog/2024-02-08-emacs-window-rules-display-buffer-alist
  ;; See: https://github.com/ashlineldridge/.config/blob/b92a568ec77f69170bc06523b163020f73962464/emacs/init.el#L534C6-L534C69
  ;; See: https://github.com/protesilaos/dotfiles/blob/d16ad4d8d54e1d06ec1cbe7a62568d408651847e/emacs/.emacs.d/prot-emacs-modules/prot-emacs-window.el#L66
  (display-buffer-alist
   `(;; No window.
     ("\\*Placeholder\\*"
      ;; Note that some buffers additionally require the `allow-no-window'
      ;; option to be specified. The downside of this is that it breaks other-
      ;; window commands such as `consult-buffer-other-window'.
      (display-buffer-no-window))
     ;; Below current window in a regular window.
     ("CAPTURE-.*\\.org"
      (display-buffer-below-selected)
      (window-height . 16))
     ;; Below current window in dedicated side window with no mode line.
     ("\\*\\(Org \\(Select\\|Note\\)\\|Agenda Commands\\)\\*"
      (display-buffer-in-side-window)
      (dedicated . t)
      (side . bottom)
      (slot . 0)
      (window-parameters . ((mode-line-format . none))))
     ;; Same window.
     ("\\(\\*vc-dir\\*\\|magit: \\)"
      (display-buffer-same-window)))))

;;;;; Window Movement

(use-package ace-window
  :defines (embark-buffer-map embark-file-map embark-bookmark-map)
  :preface
  (declare-function aw-switch-to-window "ace-window")
  (declare-function aw-flip-window "ace-window")

  (defun my/ace-switch-buffer (window)
    "Ace window action to select buffer in WINDOW."
    (let ((origin-window (selected-window)))
      ;; This approach also works across frames.
      (aw-switch-to-window window)
      (unwind-protect
          (consult-buffer)
        (when (not (equal origin-window window))
          (aw-flip-window)))))

  (defun my/ace-bury-buffer (window)
    "Ace window action to bury buffer in WINDOW."
    ;; Window needs to be selected to bury its buffer.
    (with-selected-window window
      (bury-buffer)))

  (defun my/ace-kill-buffer (window)
    "Ace window action to kill buffer in WINDOW."
    (kill-buffer (window-buffer window)))

  (defun my/ace-delete-window (window)
    "Ace window action to delete WINDOW."
    (delete-window window))

  (defun my/ace-kill-buffer-delete-window (window)
    "Ace window action to delete WINDOW and kill its buffer."
    (my/ace-kill-buffer window)
    (my/ace-delete-window window))

  (defun my/ace-split-window-horz (window)
    "Ace window action to split WINDOW horizontally."
    (split-window-horizontally nil window))

  (defun my/ace-split-window-vert (window)
    "Ace window action to split WINDOW vertically."
    (split-window-vertically nil window))

  (defun my/ace-copy-window (window)
    "Ace window action to copy current buffer to WINDOW."
    (my/ace-update-window (current-buffer) window (point) (window-start)))

  (defun my/ace-move-window (window)
    "Ace window action to move current buffer to WINDOW."
    (when (not (equal window (selected-window)))
      (my/ace-copy-window window)
      (switch-to-prev-buffer)))

  (defun my/ace-swap-window (window)
    "Ace window action to swap current buffer with WINDOW."
    (let ((origin-window (selected-window))
          (buffer (current-buffer))
          (point (point))
          (start (window-start)))
      (with-selected-window window
        (my/ace-copy-window origin-window))
      (my/ace-update-window buffer window point start)))

  (defun my/ace-update-window (buffer window point window-start)
    "Helper function to set BUFFER in WINDOW with POINT and WINDOW-START."
    (with-selected-window window
      (switch-to-buffer buffer nil t)
      (set-window-point window point)
      (set-window-start window window-start t)))

  (defmacro my/define-ace-embark-action (name map fn)
    "Define an Embark action to use Ace Window to select a window and run FN."
    `(progn
       (defun ,name ()
         ,(format "Use Ace Window to select a window and then run `%s'." fn)
         (interactive)
         (ace-window t)
         (call-interactively #',fn))
       (with-eval-after-load 'embark
         (bind-key "M-o" #',name ,map))))

  ;; Create additional Embark actions that support Ace Window.
  (my/define-ace-embark-action my/ace-embark-buffer embark-buffer-map switch-to-buffer)
  (my/define-ace-embark-action my/ace-embark-file embark-file-map find-file)
  (my/define-ace-embark-action my/ace-embark-bookmark embark-bookmark-map bookmark-jump)
  :bind
  ("M-o" . ace-window)
  :custom
  (aw-scope 'global)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-background t)
  (aw-display-mode-overlay t)
  (aw-dispatch-always t)
  :custom-face
  (aw-leading-char-face ((t (:bold t :height 1.0))))
  :config
  ;; Dispatch actions should maintain point in current window (where possible)
  ;; to provide an alternative to moving to the window and running a command.
  (setq aw-dispatch-alist
        '((?0 my/ace-delete-window "Delete Window")
          (?1 delete-other-windows "Delete Other Windows")
          (?2 my/ace-split-window-vert "Split Vertically")
          (?3 my/ace-split-window-horz "Split Horizontally")
          (?- my/ace-kill-buffer "Kill Buffer")
          (?_ my/ace-kill-buffer-delete-window "Kill Buffer and Delete Window")
          (?b my/ace-switch-buffer "Switch Buffer")
          (?q my/ace-bury-buffer "Bury Buffer")
          (?c my/ace-copy-window "Copy Window")
          (?m my/ace-move-window "Move Window")
          (?w my/ace-swap-window "Swap Window")
          (?? aw-show-dispatch-help))))

;;;;; Window History

(use-package winner
  :ensure nil
  :bind
  ("M-O u" . winner-undo)
  ("M-O r" . winner-redo)
  :hook (elpaca-after-init . winner-mode)
  :custom
  (winner-dont-bind-my-keys t)
  :config
  ;; Need to bind the repeat map after the package is loaded as Winner defines
  ;; its own repeat map that we need to override.
  (bind-keys :package winner
             :repeat-map my/window-repeat-map
             ("u" . winner-undo)
             ("r" . winner-redo)))

;;;;; Window Scrolling

(use-package pixel-scroll
  :ensure nil
  :preface
  (declare-function pixel-line-height "pixel-scroll")
  (declare-function pixel-scroll-precision-interpolate "pixel-scroll")

  (defun my/scroll-delta (delta &optional factor)
    "Scroll up by DELTA pixels with scale FACTOR (defaults to 1.0)."
    (pixel-scroll-precision-interpolate delta nil (or factor 1.0)))

  (defun my/scroll-up-page ()
    "Scroll up by the page height."
    (interactive)
    (my/scroll-delta (window-text-height nil t)))

  (defun my/scroll-down-page ()
    "Scroll down by the page height."
    (interactive)
    (my/scroll-delta (- (window-text-height nil t))))

  (defun my/scroll-up-lines ()
    "Scroll up by a line factor."
    (interactive)
    (my/scroll-delta (pixel-line-height) 2.0))

  (defun my/scroll-down-lines ()
    "Scroll down by a line factor."
    (interactive)
    (my/scroll-delta (- (pixel-line-height)) 2.0))

  (defun my/scroll-up-page-other-window ()
    "Scroll the other window up by the page height."
    (interactive)
    (with-selected-window (other-window-for-scrolling)
      (my/scroll-up-page)))

  (defun my/scroll-down-page-other-window ()
    "Scroll down by the page height."
    (interactive)
    (with-selected-window (other-window-for-scrolling)
      (my/scroll-down-page)))

  (defun my/scroll-up-lines-other-window ()
    "Scroll the other window up by a line factor."
    (interactive)
    (with-selected-window (other-window-for-scrolling)
      (my/scroll-up-lines)))

  (defun my/scroll-down-lines-other-window ()
    "Scroll the other window down by a line factor."
    (interactive)
    (with-selected-window (other-window-for-scrolling)
      (my/scroll-down-lines)))

  (defun my/other-window-for-scrolling ()
    "Custom window selection for scrolling other window."
    ;; Prioritize right, then left, then next.
    (or (window-in-direction 'right)
        (window-in-direction 'left)
        (next-window)))

  :bind
  ("M-v" . my/scroll-up-page)
  ("C-v" . my/scroll-down-page)
  ("<prior>" . my/scroll-up-page)
  ("<next>" . my/scroll-down-page)
  ("M-S-<up>" . my/scroll-up-lines)
  ("M-S-<down>" . my/scroll-down-lines)
  ;; Use the `C-M' chorded keybinds for scrolling the other window. This
  ;; chord is also used for other window keybindings in other packages.
  ("C-M-<prior>" . my/scroll-up-page-other-window)
  ("C-M-<next>" . my/scroll-down-page-other-window)
  ("C-M-<up>" . my/scroll-up-lines-other-window)
  ("C-M-<down>" . my/scroll-down-lines-other-window)
  ;; Also replace existing keybinds for scrolling the other window.
  ("M-<prior>" . my/scroll-up-page-other-window)
  ("M-<next>" . my/scroll-down-page-other-window)
  ("C-M-S-v" . my/scroll-up-page-other-window)
  ("C-M-v" . my/scroll-down-page-other-window)
  :hook (elpaca-after-init . pixel-scroll-precision-mode)
  :init
  ;; The default value of 0 causes point to be recentered when it scrolls off
  ;; screen. A value of 101 (or above) will mean that point is never recentered,
  ;; though for some commands (e.g. xref navigation) it can cause point to be
  ;; shown in an awkward position when you want recentering to occur. A value of
  ;; 1 provides a good balance such that point is prevented from scrolling off
  ;; screen but it is recentered when moving by one line doesn't bring point on
  ;; screen again (e.g. during xref navigation).
  (setq scroll-conservatively 1))

;;;;; Frame Management

(use-package frame
  :ensure nil
  :bind
  ;; Remove silly `suspend-frame' bindings.
  ("C-z" . nil)
  ("C-x C-z" . nil)
  ("M-O M-O" . other-frame)
  ("M-O M-N" . make-frame-command)
  ("M-O M-K" . delete-frame)
  ("M-O M-U" . undelete-frame)
  :hook (elpaca-after-init . undelete-frame-mode))

;; I would like to use the posframe for displaying the Ace Window indicator
;; with `ace-window-posframe-mode' but it looks wacky with multiple frames.
(use-package posframe :disabled)

;;;;; Transient

;; Use external transient as some packages require a later version.
(use-package transient)

;;;;; Tab Bar

(use-package tab-bar
  :ensure nil
  :custom
  (tab-bar-show nil))

;;;;; Breadcrumb

(use-package breadcrumb
  :preface
  (declare-function breadcrumb-local-mode "breadcrumb")
  (defun my/breadcrumb-maybe-show ()
    "Enable `breadcrumb-local-mode' for non-minibuffer buffers."
    (when (not (minibufferp))
      (breadcrumb-local-mode 1)))

  ;; Some package like `ibuffer' insist on modifying the header line.
  ;; This function provides around advice for restoring it.
  (defun my/breadcrumb-restore (fn &rest args)
    "Around advice to restore the breadcrumb header line."
    (let ((old header-line-format))
      (apply fn args)
      (setq header-line-format old)))
  :hook
  (after-change-major-mode . my/breadcrumb-maybe-show)
  :custom
  (breadcrumb-project-max-length 0.4)
  (breadcrumb-imenu-max-length 0.4))

;;;; Help System

(use-package help-fns
  :ensure nil
  :bind
  ("C-h F" . describe-face))

(use-package helpful
  :bind
  ("C-h c" . helpful-callable)
  ("C-h ," . helpful-at-point)
  ;; Replace `describe-*' bindings with Helpful where possible.
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key)
  :custom
  ;; Don't select the helpful buffer.
  (helpful-switch-buffer-function #'display-buffer))

(use-package which-key
  :ensure nil
  :hook (elpaca-after-init . which-key-mode)
  :custom
  (echo-keystrokes 0.01)
  ;; Use Embark for `prefix-help-command' as it is searchable.
  (which-key-show-early-on-C-h nil)
  (which-key-use-C-h-commands nil)
  (which-key-idle-delay 1.0)
  (which-key-idle-secondary-delay 0.05)
  (which-key-popup-type 'side-window)
  (which-key-side-window-location '(bottom right)))

;;;; General Editing

(use-package simple
  :ensure nil
  :preface
  (declare-function called-interactively-p "subr")

  (defun my/keyboard-quit-dwim ()
    "DWIM version of `keyboard-quit'."
    (interactive)
    (cond
     ((region-active-p)
      (deactivate-mark))
     ((> (minibuffer-depth) 0)
      (abort-recursive-edit))
     ((> (recursion-depth) 0)
      (exit-recursive-edit))
     (t
      (keyboard-quit))))

  (defun my/kill-ring-save ()
    "Copy the current region or line."
    (interactive)
    (if (region-active-p)
        (copy-region-as-kill nil nil t)
      (copy-region-as-kill (line-beginning-position) (line-end-position))))

  (defun my/kill-line-backward ()
    "Kill from point to the beginning of the line."
    (interactive)
    (kill-line 0))

  (defun my/zap-to-char-backward (arg char)
    "Backward `zap-to-char' for the ARGth occurrance of CHAR."
    (interactive
     (list
      (prefix-numeric-value current-prefix-arg)
      (read-char-from-minibuffer "Zap back to char: " nil 'read-char-history)))
    (zap-to-char (- arg) char t))

  (defun my/open-line-below ()
    "Open line below and indent point without breaking the current line."
    (interactive)
    (move-end-of-line 1)
    (newline-and-indent))

  (defun my/open-line-above ()
    "Open line below and indent point without breaking the current line."
    (interactive)
    (move-beginning-of-line 1)
    (newline)
    (move-beginning-of-line 0)
    (indent-according-to-mode))

  (defun my/expand-line ()
    "Expand/mark the current line from the margin."
    (interactive)
    (back-to-indentation)
    (set-mark (point))
    (end-of-line))

  (defun my/truncate-lines ()
    "Show long lines as truncated in the current buffer."
    (setq-local truncate-lines t))

  (defun my/async-shell-command (arg)
    "Version of `async-shell-command' that always creates new buffers.
When ARG is non-nil, the working directory can be selected."
    (interactive "P")
    (let ((default-directory (if (or arg (not default-directory))
                                 (read-directory-name "Working directory: ")
                               default-directory)))
      (async-shell-command
       (read-shell-command
        (concat (my/abbreviate-file-name
                 (string-remove-suffix "/" default-directory) 30) " $ "))
       (generate-new-buffer shell-command-buffer-name-async))))

  :custom
  (indent-tabs-mode nil)
  ;; Allow shift-selection to continue any active region.
  (shift-select-mode 'permanent)
  ;; Kill more, remember more.
  (kill-ring-max 1000)
  (mark-ring-max 16)
  (global-mark-ring-max 16)
  (set-mark-command-repeat-pop t)
  (shell-command-prompt-show-cwd nil)
  (async-shell-command-buffer 'confirm-new-buffer)
  ;; Don't show M-x commands that don't work in the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)
  :bind
  ([remap kill-ring-save] . my/kill-ring-save)
  ([remap keyboard-quit] . my/keyboard-quit-dwim)
  ("ESC ESC" . keyboard-escape-quit) ;; 3rd ESC is unnecessary.
  ("M-&" . my/async-shell-command)
  ("M-*" . my/expand-line)
  ("M-c" . capitalize-dwim)
  ("M-l" . downcase-dwim)
  ("M-u" . upcase-dwim)
  ("M-k" . my/kill-line-backward)
  ("M-S-SPC" . cycle-spacing)
  ;; I prefer `zap-up-to-char' when zapping forward and `zap-to-char' when
  ;; zapping backward. This also mimics the zapping behavior of Avy. Some of
  ;; the following commands are from misc.el but bound here for simplicity.
  ("M-z" . zap-up-to-char)
  ("M-Z" . my/zap-to-char-backward)
  ("C-M-<return>" . duplicate-dwim)
  ("C-<return>" . my/open-line-below)
  ("C-S-<return>" . my/open-line-above)
  ("M-S-<left>" . beginning-of-buffer)
  ("M-S-<right>" . end-of-buffer)
  ("C-M-<left>" . beginning-of-buffer-other-window)
  ("C-M-<right>" . end-of-buffer-other-window)
  ("C-c x SPC" . delete-trailing-whitespace)
  ("C-c x l" . toggle-truncate-lines)
  :hook
  (elpaca-after-init . column-number-mode)
  (prog-mode . my/truncate-lines)
  :config
  ;; Don't yank face properties (e.g. prevents pasting colors).
  (add-to-list 'yank-excluded-properties 'face))

;;;;; Text

(use-package text-mode
  :ensure nil
  :custom
  ;; Otherwise I get spelling auto-completion in Magit commit buffers
  ;; which I find annoying. Call Jinx manually instead.
  (text-mode-ispell-word-completion nil))

(use-package paragraphs
  :ensure nil
  :no-require
  :custom
  (sentence-end-double-space nil))

(use-package indent
  :ensure nil
  :no-require
  :custom
  (tab-always-indent 'complete))

(use-package delsel
  :ensure nil
  :preface
  (defun my/maybe-delete-selection (&rest _)
    "Delete region if `delete-selection-mode' is enabled."
    (when (and delete-selection-mode mark-active)
      (delete-active-region)))
  :hook (elpaca-after-init . delete-selection-mode))

(use-package iso-transl
  :ensure nil
  :bind
  ;; Bind some special characters under 'C-x 8'. The [?] character is a zero-
  ;; width space character that can be used to escape Org mode emphasis markers.
  ;; See: https://emacs.stackexchange.com/questions/16688/how-can-i-escape-the-in-org-mode-to-prevent-bold-fontification.
  (:map iso-transl-ctl-x-8-map
   ("0" . [?​])
   ("a" . [?α])
   ("b" . [?β])
   ("l" . [?λ])
   (">" . [?⟶])
   ("<" . [?⟵])
   ("s" . [?😊])))

;;;;; Undo/Redo

(use-package vundo
  :bind
  ("C-x u" . vundo)
  :custom
  (vundo-glyph-alist vundo-unicode-symbols)
  :config
  (set-face-attribute 'vundo-default nil :family "Iosevka Comfy Fixed"))

;;;;; Region Expansion

(use-package expreg
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
  :preface
  (declare-function avy-action-copy "avy")
  (declare-function avy-goto-end-of-line "avy")

  (defun my/avy-goto-end-of-line ()
    "Same as `avy-goto-end-of-line' but show the overlay as a postfix."
    ;; Can't use `avy-styles-alist' as `avy-goto-end-of-line' wraps `avy-goto-line'.
    (interactive)
    (require 'avy)
    (let ((avy-style 'post))
      (call-interactively #'avy-goto-end-of-line)))

  (defun my/avy-action-yank (pt)
    "Same as `avy-action-yank' but respects `delete-selection-mode'."
    (avy-action-copy pt)
    (my/maybe-delete-selection)
    (yank))

  :bind
  (("M-j" . avy-goto-word-1)
   ("M-J" . avy-goto-char-in-line)
   ("M-g c" . avy-goto-char-timer)
   ("M-g C" . avy-goto-char-in-line)
   ("M-g l" . avy-goto-line)
   ("M-g L" . my/avy-goto-end-of-line)
   ("M-g w" . avy-goto-word-1)
   :map isearch-mode-map
   ("M-j" . avy-isearch)
   ("M-g" . avy-isearch))
  :custom
  (avy-all-windows 'all-frames)
  (avy-single-candidate-jump nil)
  (avy-timeout-seconds 0.3)
  (avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (avy-styles-alist '((avy-isearch . post)))
  (avy-dispatch-alist
   '((?m . avy-action-mark)
     (?t . avy-action-teleport)
     (?w . avy-action-copy)
     (?y . my/avy-action-yank)
     (?z . avy-action-zap-to-char)
     (?x . avy-action-kill-stay)))
  :config
  (eldoc-add-command-completions "avy-goto-"))

(use-package link-hint
  :bind
  ("M-g ." . link-hint-open-link)
  ("M-g M-." . link-hint-copy-link))

;;;;; Highlighting

(use-package hl-line
  :ensure nil
  :custom
  (hl-line-sticky-flag nil))

(use-package hi-lock
  :ensure nil
  :bind
  (("M-s h ." . highlight-symbol-at-point)
   ("M-s h h" . highlight-regexp)
   ("M-s h l" . highlight-lines-matching-regexp)
   ("M-s h u" . unhighlight-regexp)))

(use-package lin
  :hook (elpaca-after-init . lin-global-mode)
  :bind
  ("C-c x h" . lin-mode)
  :custom
  (lin-face 'lin-green))

(use-package pulsar
  :preface
  (defun my/pulsar-pulse-line (&optional _)
    "Line pulsing function intended to be used as after advice."
    (pulsar-pulse-line))
  :hook (elpaca-after-init . pulsar-global-mode)
  ;; Some functionality is better accessed via hooks than by registering
  ;; functions in `pulsar-pulse-functions'.
  (minibuffer-setup . pulsar-pulse-line)
  (next-error . pulsar-pulse-line-red)
  (next-error . pulsar-reveal-entry)
  (next-error . pulsar-recenter-center)
  :custom
  (pulsar-delay 0.05)
  (pulsar-iterations 13)
  (pulsar-face 'pulsar-cyan)
  :config
  ;; Add extra functions that should trigger Pulsar. I'm not using
  ;; #'fn syntax here to avoid needing all the forward declarations.
  (add-to-list 'pulsar-pulse-functions 'ace-window)
  (add-to-list 'pulsar-pulse-functions 'avy-goto-line)
  (add-to-list 'pulsar-pulse-functions 'beginning-of-buffer)
  (add-to-list 'pulsar-pulse-functions 'beginning-of-defun)
  (add-to-list 'pulsar-pulse-functions 'end-of-buffer)
  (add-to-list 'pulsar-pulse-functions 'end-of-defun)
  (add-to-list 'pulsar-pulse-functions 'eshell-next-prompt)
  (add-to-list 'pulsar-pulse-functions 'eshell-previous-prompt)
  (add-to-list 'pulsar-pulse-functions 'flymake-goto-next-error)
  (add-to-list 'pulsar-pulse-functions 'flymake-goto-prev-error)
  (add-to-list 'pulsar-pulse-functions 'isearch-repeat-backward)
  (add-to-list 'pulsar-pulse-functions 'isearch-repeat-forward)
  (add-to-list 'pulsar-pulse-functions 'magit-section-backward)
  (add-to-list 'pulsar-pulse-functions 'magit-section-forward)
  (add-to-list 'pulsar-pulse-functions 'my/avy-goto-end-of-line)
  (add-to-list 'pulsar-pulse-functions 'other-frame)
  (add-to-list 'pulsar-pulse-functions 'pop-global-mark)
  (add-to-list 'pulsar-pulse-functions 'treesit-beginning-of-defun)
  (add-to-list 'pulsar-pulse-functions 'treesit-end-of-defun)
  (add-to-list 'pulsar-pulse-functions 'vterm-next-prompt)
  (add-to-list 'pulsar-pulse-functions 'vterm-previous-prompt)
  (add-to-list 'pulsar-pulse-functions 'xref-find-definitions)
  (add-to-list 'pulsar-pulse-functions 'xref-go-back)
  (add-to-list 'pulsar-pulse-functions 'xref-go-forward)
  ;; Functions called by Embark need to be advised.
  (with-eval-after-load 'embark
    (advice-add 'embark-next-symbol :after 'my/pulsar-pulse-line)
    (advice-add 'embark-previous-symbol :after 'my/pulsar-pulse-line)))

(use-package rainbow-mode)

;;;;; Templating

(use-package tempel
  :bind
  (("M-/" . tempel-complete)
   (:map tempel-map
    ("M-/" . tempel-next)
    ("M-?" . tempel-previous)
    ([remap keyboard-quit] . tempel-done))))

;;;; Buffer Management

(use-package ibuffer
  :ensure nil
  :preface
  (declare-function ibuffer-update "ibuffer")
  :bind
  ("C-x C-b" . ibuffer)
  :custom
  (ibuffer-formats
   '(;; Default format.
     (mark vc-status-mini
           " " (icon 2 2)
           " " (name 40 40 :left :elide)
           " " filename-and-process+)
     ;; More detailed format.
     (mark modified read-only vc-status-mini
           " " (icon 2 2)
           " " (name 30 30 :left :elide)
           " " (size-h 10 10 :right)
           " " (mode+ 16 16 :left :elide)
           " " filename-and-process+)))
  (ibuffer-saved-filter-groups
   '(("default"
      ("Git" (or (name . "^magit") (name . "^*vc-")))
      ("Shell" (or (mode . eshell-mode) (mode . vterm-mode)))
      ("Command" (mode . shell-command-mode))
      ("Dired" (mode . dired-mode))
      ("Org" (or (mode . org-mode) (mode . org-agenda-mode))))))
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-use-header-line nil)
  :config
  ;; Restore breadcrumb after ibuffer after messes with it.
  (advice-add #'ibuffer-update :around #'my/breadcrumb-restore)
  ;; Unbind clashing keybindings.
  (dolist (key '("M-g" "M-j" "M-o" "M-s"))
    (define-key ibuffer-mode-map (kbd key) nil)))

(use-package ibuffer-vc
  :after ibuffer
  :bind
  (:map ibuffer-mode-map
   ;; Take /p for grouping by "project". By default, /p and /P are bound to
   ;; `ibuffer-pop-filter' and `ibuffer-pop-filter-group' but these commands
   ;; are also bound to /<up> and /S-<up>.
   ("/P" . nil)
   ("/p" . ibuffer-vc-set-filter-groups-by-vc-root)))

;;;; File System

(use-package files
  :ensure nil
  :preface
  (defun my/kill-buffer-maybe-save ()
    "Kill the current buffer after saving if required."
    (interactive)
    (when (and buffer-file-name (buffer-modified-p))
      (save-buffer))
    (kill-current-buffer))

  (defun my/abbreviate-file-name (path max-len)
    "Return an abbreviated version of PATH aiming for <= MAX-LEN characters."
    (let* ((parts (split-string (abbreviate-file-name path) "/"))
           (len (+ (1- (length parts)) (cl-reduce '+ parts :key 'length)))
           (str ""))
      (while (and (> len max-len)
                  (cdr parts))
        (setq len (- len (1- (length (car parts)))))
        (setq parts (cdr parts))
        (setq str (concat str (cond ((= 0 (length (car parts))) "/")
                                    ((= 1 (length (car parts)))
                                     (concat (car parts) "/"))
                                    (t
                                     (if (string= "." (string (elt (car parts) 0)))
                                         (concat (substring (car parts) 0 2) "/")
                                       (string (elt (car parts) 0) ?/)))))))
      (concat str (cl-reduce (lambda (a b) (concat a "/" b)) parts))))

  :bind
  ;; Shorter save/quit buffer bindings.
  ("M-'" . save-buffer)
  ("M-\"" . my/kill-buffer-maybe-save)
  ("C-x C-r" . restart-emacs)
  :hook (elpaca-after-init . auto-save-visited-mode)
  :custom
  (trusted-content
   (list (expand-file-name "early-init.el" user-emacs-directory)
         (expand-file-name "bootstrap.el" user-emacs-directory)
         "~/dev/home/smol/"
         "~/dev/home/gptel-extras/"
         "~/dev/home/chronosphere/"))
  ;; Disable `auto-save-mode' which saves buffers to separate files in favor of
  ;; `auto-save-visited-mode' which saves file-visiting buffers to their files.
  (auto-save-default nil)
  (auto-save-visited-interval 30)
  (confirm-kill-emacs #'yes-or-no-p)
  (delete-by-moving-to-trash t)
  ;; Use GNU ls (used by dired and supports grouping directories first).
  (insert-directory-program "gls"))

;;;;; File Browsing

(use-package dired
  :ensure nil
  :bind
  (:map dired-mode-map
   ("M-s" . nil)
   ("N" . dired-create-empty-file)
   ("?" . which-key-show-major-mode)
   :map dired-jump-map
   ;; Allow `dired-goto-file' (via j) straight after jumping with C-x C-j.
   ;; Without this, repeat mode takes over and j calls `dired-jump' again.
   ("j" . nil))
  :hook
  (dired-mode . my/truncate-lines)
  :custom
  (delete-by-moving-to-trash t)
  (dired-free-space nil)
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-kill-when-opening-new-dired-buffer t)
  (dired-listing-switches "-AGFhlv --group-directories-first --time-style=long-iso"))

(use-package dired-aux
  :ensure nil
  :custom
  (dired-create-destination-dirs 'ask)
  (dired-create-destination-dirs-on-trailing-dirsep t))

(use-package dired-subtree
  :after dired
  :bind
  (:map dired-mode-map
   ("i" . dired-subtree-insert)
   (";" . dired-subtree-remove)
   ("<tab>" . dired-subtree-toggle))
  :custom
  (dired-subtree-use-backgrounds nil))

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

;; Provides the super useful `wdired-change-to-wdired-mode' command.
(use-package wdired
  :after dired
  :ensure nil
  :bind
  (:map dired-mode-map
   ;; Use same keybinding as `wgrep-change-to-wgrep-mode'.
   ("C-c C-w" . wdired-change-to-wdired-mode))
  :custom
  (wdired-allow-to-change-permissions t)
  (wdired-create-parent-directories t))

;;;;; File History

(use-package recentf
  :ensure nil
  :hook (elpaca-after-init . recentf-mode)
  :custom
  (recentf-max-saved-items 300))

;;;;; Reverting

(use-package autorevert
  :ensure nil
  :hook (elpaca-after-init . global-auto-revert-mode)
  :custom
  (auto-revert-interval 2)
  (auto-revert-verbose t)
  (global-auto-revert-non-file-buffers t))

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
    (project-remember-projects-under "~/dev/home")
    (project-remember-projects-under "~/dev/work"))

  (defun my/project-async-shell-command (arg)
    "Version of `project-async-shell-command' that always creates new buffers.
When ARG is non-nil, the working directory can be selected."
    (interactive "P")
    (let ((default-directory (my/project-current-root)))
      (my/async-shell-command arg)))

  :bind
  ("C-M-&" . my/project-async-shell-command)
  ("C-x p j" . project-dired)
  ("C-x p u" . my/project-update-list)
  :custom
  (project-prompt-project-dir)
  (project-switch-commands
   '((my/consult-project-file "File" ?f)
     (project-dired "Dired" ?j)
     (consult-ripgrep "Ripgrep" ?s)
     (magit-project-status "Magit" ?g)
     (project-vc-dir "VC" ?v)
     (project-eshell "Eshell" ?e)
     (my/project-async-shell-command "Command" ?&))))

;;;; Minibuffer

(use-package minibuffer
  :ensure nil
  :hook (elpaca-after-init . minibuffer-depth-indicate-mode)
  :custom
  (completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (enable-recursive-minibuffers t)
  ;; Don't allow the cursor in the minibuffer prompt text.
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

(use-package vertico
  :ensure (:files (:defaults "extensions/*.el"))
  :bind
  (:map vertico-map
   ;; Support pagination with usual keys.
   ("M-v" . vertico-scroll-down)
   ("C-v" . vertico-scroll-up)
   ("<prior>" . vertico-scroll-down)
   ("<next>" . vertico-scroll-up)
   ;; Override pixel scrolling keybindings.
   ("M-S-<up>" . vertico-next)
   ("M-S-<down>" . vertico-next))
  :hook (elpaca-after-init . vertico-mode)
  :custom
  (vertico-count 16)
  (vertico-count-format '("%-6s " . "%s/%s"))
  (vertico-resize nil))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :preface
  (declare-function vertico-sort-alpha "vertico")
  (defun my/vertico-sort-dirs-first (files)
    "Sorts FILES alphanumerically with directories listed first."
    (setq files (vertico-sort-alpha files))
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
  :ensure nil
  :hook (elpaca-after-init . vertico-multiform-mode)
  :bind
  (:map vertico-multiform-map
   ;; Free up M-B for `embark-become'.
   ("M-B" . nil)
   ;; Toggle between vertical buffer and horizontal minibuffer.
   ("M-T" . vertico-multiform-buffer))
  :custom
  (vertico-multiform-categories
   '((file (vertico-sort-function . my/vertico-sort-dirs-first))
     (jinx buffer)))
  ;; Commands work better when the category is too broad. To customize display
  ;; of completions, reference the `consult-completion-in-region' command.
  ;; Use `vertico-multiform-buffer' (M-B) to toggle between multiform displays.
  (vertico-multiform-commands
   '((consult-imenu buffer)
     (consult-outline buffer))))

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
     (direction . right)
     (window-width . 0.25))))

(use-package vertico-repeat
  :ensure nil
  :hook (minibuffer-setup . vertico-repeat-save)
  :bind
  ("C-c '" . vertico-repeat-select)
  :init
  ;; Persist Vertico history between Emacs sessions.
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'vertico-repeat-history)))

(use-package vertico-quick
  :after vertico
  :ensure nil
  :preface
  (defun my/insert-apostrophe ()
    "Insert an apostrophe character (as it is often bound to quick commands)."
    (interactive)
    (insert "'"))
  :bind
  (:map vertico-map
   ("'" . vertico-quick-exit))
  :custom
  (vertico-quick1 "asdfghjkl")
  (vertico-quick2 "asdfghjkl"))

;;;; Completion System

(use-package corfu
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
  (:map corfu-map
   ("S-SPC" . corfu-insert-separator)
   ("M-m" . my/corfu-move-to-minibuffer))
  :hook
  (elpaca-after-init . global-corfu-mode)
  (minibuffer-setup . my/corfu-enable-in-minibuffer)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.3)
  (corfu-preselect 'directory)
  (corfu-quit-at-boundary 'separator)
  (corfu-on-exact-match nil)
  (corfu-count 16)
  (corfu-preview-current nil)
  (corfu-min-width 80)
  (corfu-max-width 120)
  (corfu-scroll-margin 4)
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
  :hook (global-corfu-mode . corfu-popupinfo-mode)
  :custom
  (corfu-popupinfo-delay '(1.0 . 0.5))
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
  :hook (global-corfu-mode . corfu-history-mode)
  :init
  ;; Persist Corfu history between Emacs sessions.
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'corfu-history)))

;; Dedicated completion commands.
(use-package cape
  :bind
  ("C-' a" . cape-abbrev)
  ("C-' d" . cape-dabbrev)
  ("C-' f" . cape-file)
  ("C-' h" . cape-history)
  ("C-' k" . cape-keyword)
  ("C-' l" . cape-line)
  ("C-' o" . cape-elisp-symbol)
  ("C-' r" . cape-rfc1345)
  ("C-' w" . cape-dict)
  ("C-' :" . cape-emoji)
  :custom
  (cape-dabbrev-min-length 1)
  (cape-dabbrev-check-other-buffers t)
  (cape-line-buffer-function #'buffer-list))

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
  :hook (elpaca-after-init . marginalia-mode))

(use-package consult
  :defines (xref-show-xrefs-function xref-show-definitions-function)
  :preface
  (declare-function consult-completion-in-region "consult")
  (declare-function consult-org-agenda "consult-org")
  (declare-function consult-register-format "consult-register")
  (declare-function consult-register-window "consult-register")
  (declare-function consult-xref "consult-xref")
  (declare-function consult--buffer-query "consult")
  (declare-function consult--buffer-state "consult")
  (declare-function consult--customize-put "consult")
  (declare-function consult--file-action "consult")
  (declare-function consult--file-preview "consult")
  (declare-function consult--file-state "consult")
  (declare-function consult--read "consult")
  (declare-function project--find-in-directory "project")

  (defun my/consult-source-buffer (name narrow mode)
    "Return a Consult buffer source with NAME, NARROW key and MODE target."
    `(:name ,name
      :narrow ,narrow
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
          :mode mode))))

  (defvar my/consult-source-agenda-buffer
    (my/consult-source-buffer "Agenda Buffer" ?a 'org-agenda-mode))
  (defvar my/consult-source-dired-buffer
    (my/consult-source-buffer "Dired Buffer" ?d 'dired-mode))
  (defvar my/consult-source-eshell-buffer
    (my/consult-source-buffer "Eshell Buffer" ?e 'eshell-mode))
  (defvar my/consult-source-magit-buffer
    (my/consult-source-buffer "Magit Buffer" ?g 'magit-status-mode))
  (defvar my/consult-source-vc-buffer
    (my/consult-source-buffer "VC Buffer" ?v 'vc-dir-mode))

  ;; Consult source for all project files. This has largely been adapted from
  ;; the implementation of `consult--source-project-recent-file'.
  (defvar my/consult-source-project-file
    `(:name "Project File"
      :narrow ?f
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

  (defun my/consult-project-file ()
    "Replacement for `project-find-file' that uses Consult sources."
    (interactive)
    (require 'consult)
    (let* ((consult-project-buffer-sources
            `((:narrow ?b ,@consult--source-project-buffer) ;; Narrow: ?b (shown)
              my/consult-source-project-file                ;; Narrow: ?f (shown)
              consult--source-project-recent-file-hidden))) ;; Narrow: ?r (hidden)
      (consult-project-buffer)))

  (defun my/consult-read-file-name (prompt &optional dir _default mustmatch _initial pred)
    "Function to assign to `read-file-name-function' to enable previewing."
    (interactive)
    (require 'consult)
    (let ((default-directory (or dir default-directory))
          (minibuffer-completing-file-name t))
      (consult--read #'read-file-name-internal :state (consult--file-preview)
                     :prompt prompt
                     :initial (abbreviate-file-name default-directory)
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
   ("M-I" . consult-imenu-multi)
   ("M-_" . consult-focus-lines)
   ("C-M-_" . consult-keep-lines)
   ("M-s a" . consult-org-agenda)
   ("M-s f" . my/consult-project-file)
   ("M-s F" . consult-fd)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi)
   ("M-s s" . consult-ripgrep)
   ("M-g -" . consult-outline)
   ("M-g e" . consult-compile-error)
   ("M-g f" . consult-flymake)
   ("M-g g" . consult-goto-line)
   ("M-g M-g" . consult-goto-line)
   ("M-g i" . consult-imenu)
   ("M-g I" . consult-imenu-multi)
   ("M-g m" . consult-mark)
   ("M-g M" . consult-global-mark)
   ("C-c h" . consult-history)
   ("C-c f f" . consult-flymake)
   ("C-c x t" . consult-theme)
   ("C-x b" . consult-buffer)
   ("C-x 4 b" . consult-buffer-other-window)
   ("C-x 4 f" . my/consult-find-file-other-window)
   ("C-x 5 b" . consult-buffer-other-frame)
   ("C-x t b" . consult-buffer-other-tab)
   ("C-x p b" . consult-project-buffer)
   ("C-x p f" . my/consult-project-file)
   ("C-x r b" . consult-bookmark)
   ("C-x r i" . consult-register-load)
   ("C-x r r" . consult-register)
   ("C-x r s" . consult-register-store)
   ("C-x C-f" . my/consult-find-file)
   ("C-x C-k k" . consult-kmacro)
   :map minibuffer-local-map
   ("M-s" . nil)
   :map consult-narrow-map
   ("C-," . consult-narrow-help)
   ("?" . consult-narrow-help)
   :map isearch-mode-map
   ("M-e" . consult-isearch-history)
   ("M-s l" . consult-line)
   ("M-s L" . consult-line-multi))

  :custom
  ;; Type ',' followed by a prefix key to narrow the available candidates.
  ;; Type C-, (defined above) to display prefix help. Alternatively, type
  ;; ',' followed by C-h (or ?) to call `embark-prefix-help-command'.
  (consult-narrow-key ",")
  ;; Default to manual preview as it prevents the screen from jumping resulting
  ;; in a cleaner experience which I prefer.
  (consult-preview-key "M-.")
  ;; Tell `consult-ripgrep' to search hidden dirs/files but ignore .git/.
  (consult-ripgrep-args "rg --null --line-buffered --color=never \
    --max-columns=1000 --path-separator=/ --smart-case --no-heading \
    --with-filename --line-number --hidden --glob=!.git/")
  ;; Configure both `config-find' and `consult-fd' to follow symlinks, include
  ;; hidden files, and ignore the .git directory. The fd command needs to be
  ;; specifically told to allow matching across the full path (e.g. so you
  ;; can search for "src/foo"). In general, I prefer `consult-fd' as it obeys
  ;; the .gitignore file if present.
  (consult-find-args "find -L . -not ( -name .git -type d -prune )")
  (consult-fd-args "fd -p -L -H -E .git/*")
  ;; Only show Modus and Ef themes.
  (consult-themes '("^modus-" "^ef-"))
  ;; Following will be overridden if/when `global-corfu-mode' is run.
  (completion-in-region-function #'consult-completion-in-region)
  ;; Hide narrow groupings for bookmarks as unnecessary.
  (consult-bookmark-narrow nil)

  :init
  ;; Configure how registers are previewed and displayed.
  ;; See: https://github.com/minad/consult#use-package-example.
  (setq register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  ;; Use Consult to select xref locations with preview.
  (with-eval-after-load 'xref
    (setq xref-show-xrefs-function #'consult-xref)
    (setq xref-show-definitions-function #'consult-xref))

  :config
  ;; Customize the list of sources shown by `consult-buffer'.
  (setq consult-buffer-sources
        '(consult--source-buffer                ;; Narrow: ?b (shown)
          consult--source-project-buffer-hidden ;; Narrow: ?p (hidden)
          my/consult-source-agenda-buffer       ;; Narrow: ?a (hidden)
          my/consult-source-dired-buffer        ;; Narrow: ?d (hidden)
          my/consult-source-eshell-buffer       ;; Narrow: ?e (hidden)
          my/consult-source-magit-buffer        ;; Narrow: ?g (hidden)
          my/consult-source-vc-buffer           ;; Narrow: ?v (hidden)
          consult--source-bookmark              ;; Narrow: ?m (shown)
          consult--source-recent-file))         ;; Narrow: ?r (hidden)

  ;; Customize individual Consult sources.
  (consult-customize
   consult--source-buffer
   :name "Open Buffer" :narrow ?b
   consult--source-project-buffer
   consult--source-project-buffer-hidden
   :name "Project Buffer" :narrow ?p
   consult--source-bookmark
   :name "Bookmark" :narrow ?m
   consult--source-recent-file
   :name "Recent File" :narrow ?r :hidden t
   consult--source-project-recent-file
   consult--source-project-recent-file-hidden
   :name "Recent Project File" :narrow ?r
   ;; Allow preview for commands that don't support it by default.
   consult-find consult-fd
   :state (consult--file-preview)
   ;; Configure automatic preview for the following commands. This should be
   ;; limited to commands where immediate preview is the natural UX. I'd
   ;; prefer to avoid delayed preview (via :debounce) and force a cleaner split
   ;; between manual and immediate previewing.
   consult-compile-error consult-flymake consult-focus-lines
   consult-goto-line consult-history consult-imenu consult-imenu-multi
   consult-keep-lines consult-line consult-line-multi consult-mark
   consult-global-mark consult-outline consult-xref
   :preview-key 'any))

(use-package consult-dir
  :preface
  (declare-function consult-dir--pick "consult-dir")
  (declare-function consult-dir--project-list-make "consult-dir")

  (defvar my/consult-dir-source-local-subdir
    `(:name "Local Subdir"
      :narrow ?.
      :hidden t
      :category file
      :face consult-file
      :history file-name-history
      :enabled ,#'my/project-current-root
      :items ,#'my/find-subdirs))

  (defvar my/consult-dir-source-project-subdir
    `(:name "Project Subdir"
      :narrow ?p
      :hidden t
      :category file
      :face consult-file
      :history file-name-history
      :enabled ,#'my/project-current-root
      :items ,(lambda () (my/find-subdirs (my/project-current-root)))))

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

  (defun my/consult-dir-cd ()
    "Use `consult-dir' to change directory in Eshell."
    (interactive)
    (require 'consult-dir)
    (eshell/cd (consult-dir--pick))
    (eshell-send-input))

  :bind
  ("M-s d" . consult-dir)
  :custom
  (consult-dir-shadow-filenames nil)
  (consult-dir-default-command #'consult-dir-dired)
  :config
  (consult-customize
   consult-dir--source-bookmark :name "Bookmark" :narrow ?k
   consult-dir--source-recentf :name "Recent" :narrow ?r
   ;; Override `:items' to show all projects including the current one.
   consult-dir--source-project :name "Project" :narrow ?P :items #'consult-dir--project-dirs)

  ;; Customize the set of sources used by `consult-dir'.
  (setq consult-dir-sources
        '(consult-dir--source-bookmark
          consult-dir--source-project
          my/consult-dir-source-project-subdir
          consult-dir--source-recentf
          my/consult-dir-source-local-subdir))

  ;; Refresh projects maintained by `consult-dir' when the main list is updated.
  (advice-add #'my/project-update-list
              :after (lambda () (consult-dir--project-list-make t))))

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
   ;; Allow Embark to show keybindings under C-h as configured below.
   ("C-h C-h" . nil)
   :map embark-general-map
   ("&" . my/async-shell-command)
   :map minibuffer-local-map
   ("M-." . my/embark-force-preview)
   ("M-B" . embark-become))
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
  (eldoc-add-command "embark-dwim")
  ;; Adapt the associated commands so that they are usable as Embark actions.
  ;; If commands don't behave properly with Embark, play with this. Look at
  ;; similar commands already in `embark-target-injection-hooks' and mimic.
  (add-to-list 'embark-target-injection-hooks '(eglot-code-actions embark--ignore-target))
  (add-to-list 'embark-target-injection-hooks '(eglot-rename embark--ignore-target))
  (add-to-list 'embark-target-injection-hooks '(eglot-find-implementation embark--ignore-target))
  (add-to-list 'embark-target-injection-hooks '(consult-eglot-symbols embark--allow-edit))
  (add-to-list 'embark-target-injection-hooks '(query-replace embark--allow-edit))
  (add-to-list 'embark-target-injection-hooks '(query-replace-regexp embark--allow-edit))
  (add-to-list 'embark-target-injection-hooks '(my/async-shell-command embark--ignore-target))
  ;; Configure `my/async-shell-command' to run from directory associated with the candidate.
  (setf (alist-get #'my/async-shell-command embark-around-action-hooks) '(embark--cd)))

(use-package embark-consult)

;;;; Search and Replace

(use-package isearch
  :ensure nil
  :bind
  (("M-s ." . isearch-forward-thing-at-point)
   :map isearch-mode-map
   ;; The default `isearch-abort' requires multiple C-g if search not found.
   ("C-g" . isearch-cancel)
   ("C-n" . isearch-repeat-forward)
   ("C-p" . isearch-repeat-backward)))

(use-package occur
  :ensure nil
  :hook
  (occur-mode . my/truncate-lines))

(use-package rg
  :bind
  ("M-s M-s r" . rg)
  ("M-s M-s m" . rg-menu)
  ("M-s M-s l" . rg-literal))

(use-package wgrep
  :bind
  (:map grep-mode-map
   ("C-c C-w" . wgrep-change-to-wgrep-mode)
   ("C-c C-c" . wgrep-finish-edit))
  :custom
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

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

(use-package prog-mode
  :ensure nil
  :bind
  (:map prog-mode-map
   ("M-q" . nil)))

(use-package compile
  :ensure nil
  :custom
  (compilation-ask-about-save nil)
  (compilation-max-output-line-length nil)
  :bind
  ("C-c b 1" . compile)
  ("C-c b 2" . recompile))

(use-package newcomment
  :ensure nil
  :bind
  ("C-;" . comment-line))

;;;;;; Tree-sitter

(use-package treesit
  :ensure nil
  :custom
  (treesit-font-lock-level 4)
  (treesit-extra-load-path
   (list (no-littering-expand-var-file-name "tree-sitter"))))

(use-package treesit-auto
  :preface
  (declare-function treesit-auto-add-to-auto-mode-alist "treesit-auto")
  (declare-function treesit-auto--build-treesit-source-alist "treesit-auto")
  (defun my/treesit-auto-maybe-install ()
    "Install Tree-sitter grammars if necessary."
    (interactive)
    (let ((old-dir (locate-user-emacs-file "tree-sitter"))
          (new-dir (car treesit-extra-load-path)))
      (when (not (file-directory-p new-dir))
        (delete-directory old-dir t)
        (let ((treesit-language-source-alist (treesit-auto--build-treesit-source-alist)))
          (mapc #'treesit-install-language-grammar treesit-auto-langs))
        (rename-file old-dir new-dir)
        (message "Tree-sitter grammars have been installed"))))
  :custom
  (treesit-auto-install 'prompt)
  (treesit-auto-langs '(bash dockerfile go gomod javascript json proto python rust))
  :hook
  (elpaca-after-init . global-treesit-auto-mode)
  ;; Perform an idle wait before installing grammars (when necessary) to prevent
  ;; locking up on a blank screen while waiting for installation.
  :defer 1
  :config
  ;; Add all languages in `treesit-auto-langs' except Rust which uses Rustic.
  (treesit-auto-add-to-auto-mode-alist '(bash dockerfile go gomod javascript json proto python))
  (my/treesit-auto-maybe-install))

(use-package treesit-fold
  :bind
  (:map treesit-fold-mode-map
   ("C-<tab>" . treesit-fold-toggle))
  :hook
  (elpaca-after-init . global-treesit-fold-mode))

;;;;;; Eglot

(use-package eglot
  :ensure nil
  :preface
  (declare-function eglot-inlay-hints-mode "eglot")
  (declare-function jsonrpc--log-event "subr")

  (defun my/eglot-init ()
    "Init function for `eglot--managed-mode'."
    (eglot-inlay-hints-mode 1)
    (setq-local eglot-cache-session-completions nil)
    (setq-local completion-at-point-functions
                '(eglot-completion-at-point
                  cape-file))
    (setq-local eldoc-documentation-functions
                '(flymake-eldoc-function
                  eglot-signature-eldoc-function
                  eglot-hover-eldoc-function)))
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
    haskell-mode
    zig-mode) . eglot-ensure)
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
  (set-face-attribute 'eglot-inlay-hint-face nil :height 0.9 :slant 'italic))

(use-package eglot-x
  :after eglot
  :ensure (:host github :repo "nemethf/eglot-x")
  :bind
  ;; Most of the LSP extension commands below only apply to rust-analyzer.
  (:map eglot-mode-map
   ("C-c / c" . eglot-x-find-crate)
   ("C-c / m" . eglot-x-view-recursive-memory-layout)
   ("C-c / o" . eglot-x-open-external-documentation)
   ("C-c / r" . eglot-x-ask-runnables)
   ("C-c / s" . eglot-x-structural-search-replace)
   ("C-c / t" . eglot-x-ask-related-tests)
   ("C-c / w" . eglot-x-reload-workspace)
   ("C-c / x" . eglot-x-expand-macro))
  :hook (eglot-managed-mode . eglot-x-setup))

;; Speed up Eglot.
(use-package eglot-booster
  :ensure (:host github :repo "jdtsmith/eglot-booster")
  :hook (eglot-managed-mode . eglot-booster-mode))

(use-package eglot-tempel
  :hook (elpaca-after-init . eglot-tempel-mode))

(use-package consult-eglot
  :after eglot
  :bind
  (:map eglot-mode-map
   ("M-g o" . consult-eglot-symbols)))

(use-package consult-eglot-embark
  :commands consult-eglot-embark-mode
  :init
  (with-eval-after-load 'consult-eglot
    (consult-eglot-embark-mode)))

(use-package dape
  :bind-keymap
  ("C-c C-d" . dape-global-map)
  :custom
  (dape-key-prefix nil)
  (dape-repl-use-shorthand t)
  (dape-buffer-window-arrangement 'right)
  (dape-info-hide-mode-line t)
  :config
  ;; Don't display REPL initially and create on demand with `dape-repl'.
  (remove-hook 'dape-start-hook 'dape-repl)
  ;; Dape config for debugging the Go test under point. Adapted from example
  ;; at https://github.com/svaante/dape/wiki.
  (add-to-list 'dape-configs
               `(dlv-test
                 modes (go-mode go-ts-mode)
                 ensure dape-ensure-command
                 command "dlv"
                 command-args ("dap" "--listen" "127.0.0.1::autoport")
                 command-cwd dape-command-cwd
                 port :autoport
                 :request "launch"
                 :type "debug"
                 :mode "test"
                 :cwd "."
                 :program "."
                 :args (lambda ()
                         (require 'which-func)
                         (if-let* ((test-name (which-function))
                                   (test-regexp (concat "^" test-name "$")))
                             `["-test.run" ,test-regexp]
                           (error "No test selected"))))))

;;;;;; Eldoc

(use-package eldoc
  :ensure nil
  :bind
  ("C-h ." . eldoc-doc-buffer)
  ("C-h t" . eldoc-mode)
  ("C-h T" . global-eldoc-mode)
  :hook (elpaca-after-init . global-eldoc-mode)
  :custom
  (eldoc-idle-delay 0.1)
  (eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-echo-area-display-truncation-message nil))

(use-package eldoc-box
  :bind
  ("C-h C-." . eldoc-box-help-at-point)
  :hook
  ;; Just use for Eglot-managed languages for now.
  (eglot-managed-mode . eldoc-box-hover-mode)
  ;; Reset the eldoc-box frame so that its padding isn't affected.
  (spacious-padding-mode . eldoc-box-reset-frame)
  :custom
  (eldoc-box-clear-with-C-g t))

;;;;;; Flymake

(use-package flymake
  :ensure nil
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
  (flymake-start-on-save-buffer nil)
  (flymake-no-changes-timeout 0.5)
  (flymake-wrap-around nil)
  (flymake-show-diagnostics-at-end-of-line nil))

(use-package flymake-proc
  :ensure nil
  :custom
  (flymake-proc-compilation-prevents-syntax-check t))

;;;;;; Xref

(use-package xref
  :ensure nil
  :custom
  ;; Don't prompt by default (invoke with prefix arg to prompt).
  (xref-prompt-for-identifier nil)
  :config
  (eldoc-add-command-completions "xref-find-" "xref-go-" "xref-goto-"))

;;;;;; Outline

(use-package outline
  :ensure nil
  :custom
  (outline-minor-mode-prefix "\C-c-")
  (outline-minor-mode-cycle t))

;;;;;; Code Formatting

(use-package apheleia
  :hook (elpaca-after-init . apheleia-global-mode)
  :config
  ;; Use goimports rather than gofmt for Go files so imports get optimized.
  (setf (alist-get 'go-mode apheleia-mode-alist) 'goimports)
  (setf (alist-get 'go-ts-mode apheleia-mode-alist) 'goimports)
  ;; Use Ormolu for formatting Haskell files.
  (setf (alist-get 'haskell-mode apheleia-mode-alist) 'ormolu)
  (setf (alist-get 'ormolu apheleia-formatters) '("ormolu" "--stdin-input-file" "."))
  ;; Use OpenTofu instead of Terraform.
  (setf (alist-get 'terraform-mode apheleia-mode-alist) 'tofu)
  (setf (alist-get 'tofu apheleia-formatters) '("tofu" "fmt" "-")))

;;;;; Programming Languages

;;;;;; Rust

(use-package rust-mode
  :ensure nil
  :bind
  (:map rust-mode-map
   ("C-c C-d" . nil))
  :custom
  (rust-mode-treesitter-derive t))

(use-package rust-ts-mode
  :ensure nil
  :defines consult-imenu-config
  :preface
  (declare-function treesit-node-text "treesit")
  (declare-function rust-ts-mode--defun-name "rust-ts-mode")

  (defun my/rust-ts-mode-init ()
    "Init function for `rust-ts-mode'."
    ;; Custom Rust node types. See possible node types here:
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
                  ("Trait" "\\`trait_item\\'" nil nil)))
    ;; Install custom function for Rust node names.
    (setq-local treesit-defun-name-function #'my/treesit-rust-defun-name))

  (defun my/treesit-rust-defun-name (node)
    "Return the defun name of NODE for Rust node types."
    (pcase (treesit-node-type node)
      ((or "const_item" "macro_definition" "trait_item")
       (treesit-node-text (treesit-node-child-by-field-name node "name") t))
      (_ (rust-ts-mode--defun-name node))))

  :hook (rust-ts-mode . my/rust-ts-mode-init)
  :config
  ;; Update `consult-imenu-config' with the symbol categories for Rust.
  (with-eval-after-load 'consult-imenu
    (add-to-list 'consult-imenu-config
                 '(rust-ts-mode
                   :types ((?a "Associated Type" my/imenu-type-face)
                           (?c "Constant" my/imenu-constant-face)
                           (?e "Enumeration" my/imenu-enum-face)
                           (?f "Function" my/imenu-function-face)
                           (?i "Implementation" my/imenu-impl-face)
                           (?M "Macro" my/imenu-macro-face)
                           (?m "Module" my/imenu-module-face)
                           (?S "Static" my/imenu-static-face)
                           (?s "Struct" my/imenu-struct-face)
                           (?t "Trait" my/imenu-trait-face))))))

(use-package rustic
  ;; TODO: Loading rustic after rust-mode is meant to ensure that the entry
  ;; that rustic adds to `auto-mode-alist' takes precedence over rust-mode and
  ;; rust-ts-mode but this doesn't always work. Further investigation needed.
  :after rust-mode
  :ensure (:host github :repo "emacs-rustic/rustic")
  :bind
  (:map rustic-mode-map
   ("C-c" . nil)
   ("C-c b a" . rustic-cargo-add)
   ("C-c b b" . rustic-cargo-build)
   ("C-c b B" . rustic-cargo-bench)
   ("C-c b c" . rustic-cargo-clean)
   ("C-c b d" . rustic-cargo-doc)
   ("C-c b f" . rustic-cargo-fmt)
   ("C-c b h" . rustic-cargo-check)
   ("C-c b l" . rustic-cargo-clippy)
   ("C-c b L" . rustic-cargo-clippy-fix)
   ("C-c b o" . rustic-cargo-outdated)
   ("C-c b r" . rustic-cargo-run)
   ("C-c b R" . rustic-cargo-rm)
   ("C-c b u" . rustic-cargo-update)
   ("C-c b U" . rustic-cargo-upgrade)
   ("C-c t ." . rustic-cargo-test-dwim)
   ("C-c t p" . rustic-cargo-test)
   ("C-c t t" . rustic-cargo-current-test))
  :custom
  (rustic-lsp-client 'eglot)
  :config
  ;; See: https://rust-analyzer.github.io/manual.html#configuration.
  ;; Note that `eglot-stderr-buffer' can be used to debug LSP server errors.
  (with-eval-after-load 'eglot
    (add-to-list
     'eglot-server-programs
     '((rustic-mode :language-id "rust") .
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
                                          :minLines 20))))))))

;;;;;; Go

(use-package go-ts-mode
  :ensure nil
  :preface
  (declare-function treesit-node-text "treesit")
  (declare-function go-ts-mode--defun-name "go-ts-mode")

  (defun my/go-ts-mode-init ()
    "Init function for `go-ts-mode'."
    (setq-local tab-width go-ts-mode-indent-offset)
    ;; Don't let tests use cached results (buffer local var used by `gotest').
    (setq-local go-test-args "-count 1")
    ;; Custom Go node types. See possible node types here:
    ;; https://github.com/tree-sitter/tree-sitter-go/blob/bbaa67a180cfe0c943e50c55130918be8efb20bd/src/node-types.json.
    (setq-local treesit-simple-imenu-settings
                '(("Constant" "\\`const_spec\\'" nil nil)
                  ("Function" "\\`function_declaration\\'" nil nil)
                  ("Interface" "\\`type_declaration\\'" go-ts-mode--interface-node-p nil)
                  ("Method" "\\`method_declaration\\'" nil nil)
                  ("New Type" "\\`type_declaration\\'" go-ts-mode--other-type-node-p nil)
                  ("Struct" "\\`type_declaration\\'" go-ts-mode--struct-node-p nil)
                  ("Type Alias" "\\`type_declaration\\'" go-ts-mode--alias-node-p nil)
                  ;; Unfortunately, this also includes local variables.
                  ("Variable" "\\`var_spec\\'" nil nil)))
    ;; Install custom function for Go node names.
    (setq-local treesit-defun-name-function #'my/treesit-go-defun-name))

  (defun my/treesit-go-defun-name (node)
    "Return the defun name of NODE for Go node types."
    (pcase (treesit-node-type node)
      ((or "const_spec" "var_spec")
       (treesit-node-text (treesit-node-child-by-field-name node "name") t))
      (_ (go-ts-mode--defun-name node))))

  :hook (go-ts-mode . my/go-ts-mode-init)
  :bind
  (:map go-ts-mode-map
   ("C-c C-d" . nil))
  :custom
  (go-ts-mode-indent-offset 4)
  :config
  (with-eval-after-load 'consult-imenu
    (add-to-list 'consult-imenu-config
                 '(go-ts-mode
                   :types ((?c "Constant" my/imenu-constant-face)
                           (?f "Function" my/imenu-function-face)
                           (?i "Interface" my/imenu-trait-face)
                           (?m "Method" my/imenu-method-face)
                           (?t "New Type" my/imenu-type-face)
                           (?s "Struct" my/imenu-struct-face)
                           (?a "Type Alias" my/imenu-type-face)
                           (?v "Variable" my/imenu-variable-face)))))

  ;; See: https://github.com/golang/tools/blob/master/gopls/doc/inlayHints.md.
  ;; Note that `eglot-stderr-buffer' can be used to debug LSP server errors.
  (with-eval-after-load 'eglot
    (add-to-list
     'eglot-server-programs
     '((go-ts-mode :language-id "go") .
       ("gopls" :initializationOptions
        (:hints (:parameterNames :json-false
                 :rangeVariableTypes t
                 :functionTypeParameters t
                 :assignVariableTypes t
                 :compositeLiteralFields t
                 :compositeLiteralTypes t
                 :constantValues t)))))))

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

(use-package go-add-tags
  :after go-ts-mode
  :bind
  (:map go-ts-mode-map
   ("C-c r t" . go-add-tags))
  :custom
  (go-add-tags-style 'camel-case)
  (go-add-tags-fields-tags '("json" "yaml" "validate")))

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

;;;;;; Zig

(use-package zig-mode)

;;;;;; Haskell

(use-package haskell-mode)

;;;;;; Terraform

(use-package terraform-mode)

;;;;;; Python

(use-package python
  :ensure nil
  :mode
  ;; Also use Python mode for Bazel and Starlark files.
  ("\\.\\(bazel\\|star\\)\\'" . python-ts-mode)
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
  :config
  (with-eval-after-load 'consult-imenu
    (add-to-list 'consult-imenu-config
                 '(protobuf-ts-mode
                   :types ((?r "RPC" my/imenu-function-face)
                           (?e "Enum" my/imenu-enum-face)
                           (?m "Message" my/imenu-struct-face)
                           (?s "Service" my/imenu-impl-face))))))

;;;;;; Just (Task Runner)

(use-package just-mode)

;;;;;; C/C++

(use-package cc-mode
  :ensure nil)

(use-package clang-format
  :custom
  (clang-format-fallback-style "llvm"))

;;;;;; YAML

(use-package yaml-mode
  :hook
  ;; Disable electric indent as it's quite annoying for this mode.
  (yaml-mode . (lambda () (electric-indent-local-mode -1))))

;;;;;; Lisp

(use-package elisp-mode
  :ensure nil
  :preface
  (defun my/elisp-init ()
    "Init function for `emacs-lisp-mode'."
    (setq-local outline-regexp ";;;+ [^\n]")
    (outline-minor-mode 1)
    (setq-local completion-at-point-functions
                '(elisp-completion-at-point cape-file))
    (setq-local eldoc-documentation-functions
                '(flymake-eldoc-function
                  elisp-eldoc-var-docstring-with-value
                  elisp-eldoc-funcall)))

  (defun my/elisp-flymake-setup-load-path (fn &rest args)
    "Advises `elisp-flymake-byte-compile' to setup the correct load path."
    ;; Exclude Vterm from the load path as it causes Flymake to hang.
    (let* ((load-path (seq-remove (lambda (d)
                                    (string-match-p "elpaca/builds/vterm" d))
                                  load-path))
           (elisp-flymake-byte-compile-load-path
            (append elisp-flymake-byte-compile-load-path load-path)))
      (apply fn args)))

  :hook
  (emacs-lisp-mode . my/elisp-init)
  :config
  (advice-add #'elisp-flymake-byte-compile :around #'my/elisp-flymake-setup-load-path)
  ;; This configuration is from consult-imenu.el with the fonts changed.
  (with-eval-after-load 'consult-imenu
    (add-to-list 'consult-imenu-config
                 '(emacs-lisp-mode
                   :toplevel "Functions"
                   :types ((?f "Functions" my/imenu-function-face)
                           (?m "Macros" my/imenu-macro-face)
                           (?p "Packages" my/imenu-package-face)
                           (?t "Types" my/imenu-type-face)
                           (?v "Variables" my/imenu-variable-face))))))

(use-package paredit
  :hook
  ;; Note that I specifically don't enable Paredit in minibuffers as it causes
  ;; issues with RET keybindings.
  ((lisp-mode
    emacs-lisp-mode
    inferior-emacs-lisp-mode) . paredit-mode)
  :config
  (eldoc-add-command "paredit-backward" "paredit-forward"
                     "paredit-backward-delete" "paredit-close-round")
  ;; Unbind keybindings that collide with things I find more useful.
  (dolist (key '("M-?" "M-\"" "M-q" "M-r" "M-s" "M-J"
                 "C-c C-M-l" "C-M-<left>" "C-M-<right>"))
    (define-key paredit-mode-map (kbd key) nil)))

(use-package elec-pair
  :ensure nil
  :hook (elpaca-after-init . electric-pair-mode))

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

;;;; Version Control

(use-package vc
  :ensure nil
  :defines vc-dir-mode-map
  :custom
  (vc-follow-symlinks t)
  :bind
  ;; Consolidate keybindings into a single `use-package' form as VC commands
  ;; are spread over many sub-packages. Requires need to be correct below.
  (:map vc-prefix-map
   ("RET" . vc-dir-root)
   ("B" . vc-annotate)
   ("e" . vc-ediff)
   ("F" . vc-update)
   ("k" . vc-revert)
   ("K" . vc-delete-file)
   :map vc-dir-mode-map
   ("M-s a" . nil)
   ("B" . vc-annotate)
   ("e" . vc-ediff)
   ("F" . vc-update)
   ("k" . vc-revert)
   ("K" . vc-dir-delete-file))
  :config
  (require 'vc-dir))

;; The `log-edit' package is used by VC for entering commit messages. Based on
;; todos in the code, it looks like it will be moved into VC at some point.
(use-package log-edit
  :ensure nil
  :custom
  ;; Remove unnecessary/unwanted hook functions like `log-edit-show-files'.
  (log-edit-hook '(log-edit-insert-message-template
                   log-edit-insert-changelog)))

(use-package magit
  :bind
  ("C-c g RET" . magit-status)
  ("C-c g ." . magit-file-dispatch)
  ("C-c g f" . magit-find-file)
  ("C-c g g" . magit-dispatch)

  :custom
  (magit-auto-revert-mode nil) ;; Use `auto-revert-mode' instead.
  (magit-verbose-messages t)
  (magit-refresh-verbose t)
  (magit-refresh-status-buffer nil)
  (magit-delete-by-moving-to-trash t)
  (magit-diff-refine-hunk 'all))

(use-package browse-at-remote
  :preface
  (declare-function browse-at-remote "browse-at-remote")
  (declare-function browse-at-remote-kill "browse-at-remote")
  (declare-function browse-at-remote--get-local-branch "browse-at-remote")
  (declare-function vc-git-branches "vc-git")

  (defvar my/browse-at-remote-branch nil)
  (defun my/browse-at-remote-local-branch (fn &rest args)
    "Advises `browse-at-remote--get-local-branch' to override the branch name."
    (or my/browse-at-remote-branch (apply fn args)))

  (defun my/browse-at-remote (use-trunk &optional kill-only)
    "Browse to the remote file (or KILL-ONLY) honoring USE-TRUNK."
    (interactive "P")
    (require 'vc-git)
    (let ((my/browse-at-remote-branch
           (when use-trunk (if (member "main" (vc-git-branches)) "main" "master"))))
      (if kill-only (browse-at-remote-kill) (browse-at-remote))))

  (defun my/browse-at-remote-kill (use-trunk)
    "Add the remote file to the kill-ring honoring USE-TRUNK."
    (interactive "P")
    (my/browse-at-remote use-trunk t))
  :bind
  (("C-c g o" . my/browse-at-remote)
   ("C-c g l" . my/browse-at-remote-kill))
  :config
  (advice-add #'browse-at-remote--get-local-branch :around #'my/browse-at-remote-local-branch))

;;;; Diff

(use-package diff-mode
  :ensure nil
  :bind
  (:map diff-mode-map
   ("M-k" . nil)
   ("M-o" . nil)))

(use-package difftastic)

;;;; Shell/Terminal

(use-package eshell
  :ensure nil
  :defines
  (eshell-mode-map eshell-hist-mode-map eshell-prompt-regexp)
  :preface
  (defalias 'eshell/v 'eshell-exec-visual)
  ;; Don't want the prompt repeat keys as I use defun-style navigation.
  (defvar eshell-prompt-repeat-map (make-sparse-keymap))
  (declare-function eshell-bol "esh-mode")
  (declare-function eshell-send-input "esh-mode")
  (declare-function eshell-read-aliases-list "esh-alias")
  (declare-function eshell/cd "em-dirs")
  (declare-function eshell/pwd "em-dirs")
  (declare-function eshell-save-some-history "em-hist")

  (defun my/eshell-init ()
    "Hook function executed when `eshell-mode' is run."
    ;; Don't scroll the buffer around after it has been recentered (using C-l).
    ;; This seems to need to be done as a mode hook rather than in `:config' as
    ;; the latter results in `eshell-output-filter-functions' being set to nil.
    ;; See: https://emacs.stackexchange.com/a/45281
    (remove-hook 'eshell-output-filter-functions
                 'eshell-postoutput-scroll-to-bottom)
    ;; Preferred eshell completion settings.
    (setq-local corfu-auto nil)
    (setq-local corfu-popupinfo-mode nil)
    ;; Make outline work with eshell prompts.
    (setq-local outline-regexp (concat eshell-prompt-regexp ".+")))

  (defun my/eshell-pre-command ()
    "Eshell pre-command hook function."
    ;; Temporarily set TERM as certain shell commands use this to decide whether
    ;; to output in color. This setting is reverted in `my/eshell-post-command'.
    (setenv "TERM" "xterm-256color")
    ;; Save history after command is entered but before it is invoked.
    ;; Otherwise, history is not saved until the eshell buffer is quit.
    (eshell-save-some-history))

  (defun my/eshell-post-command ()
    "Eshell post-command hook function."
    (setenv "TERM" "dumb"))

  (defun my/eshell-prompt ()
    "Custom Eshell prompt function."
    ;; This is compatible with the default `eshell-prompt-regexp'.
    (concat (my/abbreviate-file-name (eshell/pwd) 30)
            (if (= (user-uid) 0) " # " " $ ")))

  (defun my/eshell-sink (&optional id)
    "Return a reference to a buffer for sinking Eshell command output.
The name of the returned buffer will have the name *eshell-output* if ID is
nil, otherwise it will have the name *eshell-output-<ID>*. In either case,
if a buffer with that name already exists, then a new buffer will be created
with a numbered suffix."
    (let* ((name (if id (format "*eshell-output-%s" id) "*eshell-output*"))
           (name (generate-new-buffer-name name))
           (buf (get-buffer-create name)))
      (display-buffer buf)
      buf))

  (defun my/eshell-insert-arg (&optional n)
    "Insert the Nth argument (from the end) of the previous command."
    (interactive (list (prefix-numeric-value (or current-prefix-arg 0))))
    (when-let* ((args (flatten-list eshell-last-arguments))
                (num-args (length args))
                (arg (nth (max 0 (- num-args n 1)) args)))
      (insert (substring-no-properties (format "%s" arg)))))

  (defun my/eshell-refresh-aliases ()
    "Refresh Eshell aliases."
    (interactive)
    (eshell-read-aliases-list))

  :bind
  ;; For convenience, consolidate Eshell keybindings here rather than in
  ;; separate `use-package' forms (requires below are necessary).
  (("C-c e" . eshell)
   :map eshell-mode-map
   ("C-c C-o" . nil)
   ("C-c i" . my/eshell-insert-arg)
   ;; Defun-style prompt navigation.
   ("C-M-a" . eshell-previous-prompt)
   ("C-M-e" . eshell-next-prompt)
   ;; Jump between Eshell directories using `consult-dir'.
   ("M-s M-d" . my/consult-dir-cd)
   :map eshell-hist-mode-map
   ("M-s" . nil)
   ("M-r" . nil))

  :hook
  (eshell-mode . my/eshell-init)
  (eshell-pre-command . my/eshell-pre-command)
  (eshell-post-command . my/eshell-post-command)
  :custom
  (eshell-history-size 10000)
  (eshell-buffer-maximum-lines 10000)
  (eshell-hist-ignoredups t)
  (eshell-prompt-function #'my/eshell-prompt)
  (eshell-banner-message "")
  (eshell-visual-commands '("top" "vi" "vim" "htop" "watch"))
  :config
  (require 'esh-mode)
  (require 'em-hist))

;; I much prefer Eshell but have Vterm as an escape hatch when I need a proper
;; terminal emulator. Just call 'M-x vterm' rather than wasting a keybinding.
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

  :bind
  (:map vterm-mode-map
   ;; Use same keybinding for shell history as `consult-history'.
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
  (dolist (key '("C-r" "C-s" "C-SPC" "M-g" "M-k" "M-s" "M-:" "M-&" "M-'" "M-\"" "M-]"))
    (define-key vterm-mode-map (kbd key) nil)))

(use-package sh-script
  :ensure nil
  :bind
  (:map sh-mode-map
   ("C-c C-o" . nil)))

(use-package comint
  :ensure nil
  :bind
  (:map comint-mode-map
   ("C-c C-o" . nil)))

;;;; Org Mode

(use-package org
  :ensure nil
  :preface
  (declare-function org-get-tags "org")
  (declare-function org-refile-get-targets "org-refile")
  (declare-function org-refile-goto-last-stored "org-refile")
  (declare-function org-capture-goto-last-stored "org-capture")
  (declare-function bookmark-get-bookmark-record "bookmark")

  ;; Org paths.
  (defvar my/org-notes-dir (expand-file-name "~/dev/home/org/notes/"))
  (defvar my/org-agenda-dir (expand-file-name "~/dev/home/org/agenda/"))
  (defvar my/org-other-dir (expand-file-name "~/dev/home/org/other/"))
  (defvar my/org-inbox-file (expand-file-name "inbox.org" my/org-agenda-dir))
  (defvar my/org-personal-file (expand-file-name "personal.org" my/org-agenda-dir))
  (defvar my/org-work-file (expand-file-name "work.org" my/org-agenda-dir))
  (defvar my/org-recurring-file (expand-file-name "recurring.org" my/org-agenda-dir))
  (defvar my/org-someday-file (expand-file-name "someday.org" my/org-agenda-dir))
  (defvar my/org-archive-file (expand-file-name "archive.org" my/org-agenda-dir))
  (defvar my/org-journal-file (expand-file-name "journal.org" my/org-other-dir))
  (defvar my/org-bookmarks-file (expand-file-name "bookmarks.org" my/org-other-dir))
  (defvar my/org-coffee-file (expand-file-name "coffee.org" my/org-other-dir))

  ;; Extra electric pairs to use in Org mode.
  (defvar my/org-extra-electric-pairs '((?/ . ?/) (?= . ?=) (?~ . ?~)))

  (defun my/org-init ()
    "Init function for `org-mode'."
    (visual-line-mode 1)
    (variable-pitch-mode 1)
    (display-line-numbers-mode 0)
    (setq-local line-spacing 2)
    (setq-local completion-at-point-functions (list #'cape-file))
    (setq-local electric-pair-pairs
                (append electric-pair-pairs my/org-extra-electric-pairs))
    (setq-local electric-pair-text-pairs
                (append electric-pair-text-pairs my/org-extra-electric-pairs)))

  (defun my/org-goto-last-dwim ()
    "Navigate to the last org \"thing\"."
    (interactive)
    (let* ((refile-bm (bookmark-get-bookmark-record "org-refile-last-stored"))
           (capture-bm (bookmark-get-bookmark-record "org-capture-last-stored"))
           (refile-time (alist-get 'last-modified refile-bm))
           (capture-time (alist-get 'last-modified capture-bm)))
      (if (time-less-p capture-time refile-time)
          (org-refile-goto-last-stored)
        (org-capture-goto-last-stored))))

  :bind
  (("C-c C-o" . org-open-at-point-global)
   ("C-c o s" . org-save-all-org-buffers)
   ("C-c o l" . my/org-goto-last-dwim)
   :map org-mode-map
   ;; Rebind conflicting <return>-based keybindings under C-c.
   ("C-<return>" . nil)
   ("C-S-<return>" . nil)
   ("C-c C-<return>" . org-insert-heading-respect-content)
   ("C-c C-S-<return>" . org-insert-todo-heading-respect-content)
   ("C-c o M-l" . org-id-copy))
  :hook (org-mode . my/org-init)
  :custom
  ;; The `org-agenda-files' variable is from org.el rather than org-agenda.el.
  (org-agenda-files
   `(,my/org-inbox-file
     ,my/org-work-file
     ,my/org-personal-file
     ,my/org-recurring-file
     ,my/org-someday-file
     ,my/org-archive-file))
  (org-auto-align-tags nil)
  (org-blank-before-new-entry
   '((heading . nil)
     (plain-list-item . nil)))
  (org-catch-invisible-edits 'show-and-error)
  (org-confirm-babel-evaluate nil)
  (org-deadline-warning-days 0)
  (org-default-notes-file my/org-inbox-file)
  (org-directory my/org-agenda-dir)
  (org-ellipsis " 》")
  (org-fontify-done-headline t)
  (org-fontify-quote-and-verse-blocks t)
  (org-hide-emphasis-markers t)
  (org-image-actual-width nil)
  ;; Log state changes into the LOGBOOK drawer.
  (org-log-into-drawer t)
  ;; Special properties aren't and shouldn't be put into the LOGBOOK drawer. As
  ;; SCHEDULED and DEADLINE will be created, I also log CLOSED even though this
  ;; information can be derived from the state changes in the drawer.
  ;; See: https://orgmode.org/manual/Special-Properties.html.
  (org-log-done t)
  (org-M-RET-may-split-line nil)
  (org-outline-path-complete-in-steps nil)
  (org-pretty-entities t)
  (org-priority-lowest 10)
  (org-priority-highest 1)
  (org-priority-default org-priority-lowest)
  (org-refile-targets
   `((,my/org-archive-file :level . 1)
     (,my/org-inbox-file :level . 1)
     (,my/org-personal-file :level . 1)
     (,my/org-work-file :level . 1)
     (,my/org-someday-file :level . 1)))
  ;; Show refile headlines as nested paths.
  (org-refile-use-outline-path t)
  (org-special-ctrl-a/e t)
  (org-startup-indented t)
  (org-tags-column 0)
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "PROG(p!)" "HOLD(h@)" "|" "DONE(d!)")))
  ;; See colors here: https://alexschroeder.ch/geocities/kensanata/colors.html.
  (org-todo-keyword-faces
   '(("TODO" . (:foreground "DodgerBlue2" :weight bold))
     ("NEXT" . (:foreground "hot pink" :weight bold))
     ("PROG" . (:foreground "CadetBlue1" :weight bold))
     ("HOLD" . (:foreground "orange1" :weight bold))
     ("DONE" . (:foreground "orange red" :weight bold))))
  (org-use-fast-todo-selection 'expert)
  (org-use-sub-superscripts nil)

  :config
  ;; Make it easier to create `org-babel' code blocks.
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))

  ;; Unset conflicting org keybindings.
  (dolist (key '("C-'" "M-S-<up>" "M-S-<down>" "M-S-<left>" "M-S-<right>"
                 "C-M-S-<left>" "C-M-S-<right>"))
    (define-key org-mode-map (kbd key) nil))

  ;; Bit of feedback.
  (advice-add #'org-id-copy :after (lambda (&optional _)
                                     (message "Copied: %s" (car kill-ring)))))

(use-package org-agenda
  :ensure nil
  :preface
  (declare-function org-agenda-redo-all "org-agenda")
  (declare-function org-verify-change-for-undo "org-agenda")
  (defvar my/org-agenda-todo-sort-order '("PROG" "NEXT" "TODO" "HOLD" "DONE"))

  (defun my/org-agenda-init ()
    "Init function for `org-agenda-mode'."
    (hl-line-mode 1)
    (setq-local default-directory org-directory))

  (defun my/org-agenda-cmp-todo (a b)
    "Custom compares agenda items A and B based on their todo keywords."
    (when-let ((state-a (get-text-property 14 'todo-state a))
               (state-b (get-text-property 14 'todo-state b))
               (cmp (--map (cl-position-if
                            (lambda (x) (equal x it))
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
    (org-agenda-refile nil (seq-find(lambda (refloc)
                                      (and (string= heading (nth 0 refloc))
                                           (string= file (nth 1 refloc))))
                                    (org-refile-get-targets)) t))

  (defun my/org-agenda-refile-personal-or-work (file &optional category)
    "Refile the current agenda item into FILE under Personal/ or Work/.
If CATEGORY is specified it must equal \\='personal or \\='work; if it is not
specified then a task category will be determined by the item's tags."
    (let* ((hdm (org-get-at-bol 'org-hd-marker))
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
    "Refile the current agenda item into the personal backlog."
    (interactive)
    (my/org-agenda-refile my/org-personal-file "Personal"))

  (defun my/org-agenda-refile-work ()
    "Refile the current agenda item into the work backlog."
    (interactive)
    (my/org-agenda-refile my/org-work-file "Work"))

  (defun my/org-agenda-refile-inbox ()
    "Refile the current agenda item into the inbox."
    (interactive)
    (my/org-agenda-refile my/org-inbox-file "Inbox"))

  (defun my/org-agenda-refile-archive ()
    "Refile the current agenda item into the archive."
    (interactive)
    (my/org-agenda-refile-personal-or-work my/org-archive-file))

  (defun my/org-agenda-refile-someday ()
    "Refile the current agenda item into the someday list."
    (interactive)
    (my/org-agenda-refile-personal-or-work my/org-someday-file))

  (defun my/org-agenda-id-copy ()
    "Copy the ID of the agenda item at point."
    (interactive)
    ;; See: https://emacs.stackexchange.com/questions/47605/save-excursion-in-an-org-agenda-buffer.
    (let* ((marker (org-get-at-bol 'org-marker))
           (buffer (marker-buffer marker))
           (pos (marker-position marker)))
      (org-with-remote-undo buffer
        (with-current-buffer buffer
          (goto-char pos)
          (org-id-copy)))))

  (defun my/org-agenda-redo-all ()
    "Rebuild all open agenda buffers."
    (interactive)
    (org-agenda-redo-all t))

  :bind
  (("C-c o a" . org-agenda)
   :map org-agenda-mode-map
   ("r" . nil) ;; Rebind 'r' as a prefix key.
   ("r r" . org-agenda-refile)
   ("r p" . my/org-agenda-refile-personal)
   ("r w" . my/org-agenda-refile-work)
   ("r a" . my/org-agenda-refile-archive)
   ("r s" . my/org-agenda-refile-someday)
   ("r i" . my/org-agenda-refile-inbox)
   ("g" . my/org-agenda-redo-all)
   ("k" . org-agenda-kill)
   ("M-l" . my/org-agenda-id-copy)
   ("?" . which-key-show-major-mode))
  :hook
  (org-agenda-mode . my/org-agenda-init)
  :custom
  (org-agenda-cmp-user-defined #'my/org-agenda-cmp-todo)
  (org-agenda-custom-commands
   `(("A" "Agenda for all tasks"
      ((todo
	""
	((org-agenda-overriding-header "Inbox")
	 (org-agenda-files '(,my/org-inbox-file))))
       (todo
        ""
        ((org-agenda-overriding-header "Work")
         (org-agenda-files '(,my/org-work-file))
         (org-agenda-sorting-strategy '(user-defined-up priority-down))))
       (todo
        ""
        ((org-agenda-overriding-header "Personal")
         (org-agenda-files '(,my/org-personal-file))
         (org-agenda-sorting-strategy '(user-defined-up priority-down))))))))
  (org-agenda-sorting-strategy
   '((agenda habit-down time-up category-keep user-defined-up priority-down)
     (todo category-keep user-defined-up priority-down)
     (tags category-keep user-defined-up priority-down)
     (search category-keep)))
  (org-agenda-span 'week)
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-skip-scheduled-repeats-after-deadline t)
  (org-agenda-sticky t)
  (org-agenda-start-with-log-mode t)
  (org-agenda-tags-column 0)
  (org-agenda-use-time-grid nil) ;; Toggle with 'G'.
  (org-agenda-window-setup 'current-window))

(use-package org-capture
  :ensure nil
  :bind
  ("C-c o c" . org-capture)
  ("C-c o i" . (lambda () (interactive) (org-capture nil "i")))
  ("C-c o j" . (lambda () (interactive) (org-capture nil "j")))
  :custom
  (org-capture-templates
   `(("i" "Inbox" entry
      (file+headline ,my/org-inbox-file "Inbox")
      "* TODO %i%?")
     ("b" "Bookmark" entry
      (file+olp+datetree ,my/org-bookmarks-file "Bookmarks")
      "* %(org-cliplink-capture)%?")
     ("j" "Journal" item
      (file+olp+datetree ,my/org-journal-file "Journal"))
     ("c" "Coffee Log" entry
      (file+olp+datetree ,my/org-coffee-file "Coffee Log" "Log")
      ,(concat
	"* 6%?:00 AM\n"
        "- Beans: Use org-id-copy then org-insert-link\n"
        "- Grind: KM47C+PO @ 3.0.0\n"
        "- Water: Brisbane tap @ 95°C\n"
        "- Brew method: V60 4:6\n"
        "- Brew notes:\n"
        "  - Coffee / water: 20g coffee / 300g water\n"
        "  - Breakdown: 50g/70g/60g/60g/60g on 45s with no extra agitation\n"
        "  - Next time: Grind a bit finer\n"
        "- Taste notes:\n"
        "  - Yum yum\n")
      :jump-to-captured t))))

(use-package org-clock
  :ensure nil
  :after org
  :preface
  (declare-function org-clock-get-clock-string "org-clock")
  :bind
  (("C-c o X" . org-clock-cancel)
   ("C-c o O" . org-clock-out)
   ("C-c o J" . org-clock-goto)
   :map org-mode-map
   ("C-c o I" . org-clock-in))
  :config
  ;; Use custom mode line indicator rather than `mode-line-misc-info'.
  (fset #'org-clock-get-clock-string (lambda () "")))

(use-package org-timer
  :ensure nil
  :bind
  ("C-c o t" . org-timer-set-timer)
  ("C-c o T" . org-timer-stop)
  ("C-c o SPC" . org-timer-pause-or-continue))

(use-package org-cliplink
  :after org
  :bind
  (:map org-mode-map
   ;; Use `org-insert-link' (C-c C-l) to insert links with a user-provided
   ;; description as well as for editing links/descriptions. Use `org-cliplink'
   ;; below to insert a link with the page title as the description.
   ("C-c C-S-l" . org-cliplink)))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :custom
  ;; Don't auto-show links as it's too distracting.
  (org-appear-autolinks nil)
  (org-appear-delay 0.5))

(use-package org-roam
  :bind
  ("C-c n n" . org-roam-node-find)
  ("C-c n i" . org-roam-node-insert)
  ("C-c n t" . org-roam-tag-add)
  ("C-c n u" . org-roam-db-sync)
  :custom
  (org-roam-directory my/org-notes-dir)
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
      :unnarrowed t))))

(use-package consult-org-roam
  :bind
  ("C-c n s" . consult-org-roam-search)
  :hook (elpaca-after-init . consult-org-roam-mode)
  :custom
  (consult-org-roam-buffer-enabled nil)
  (consult-org-roam-grep-func #'consult-ripgrep))

;;;; Emacs Server

(use-package server
  :ensure nil
  :preface
  (declare-function server-running-p "server")
  :custom
  (server-client-instructions 1)
  :defer 1
  :config
  (unless (server-running-p)
    (server-start)))

;;;; Time

(use-package time
  :ensure nil
  :hook
  (elpaca-after-init . display-time-mode)
  :custom
  ;; See time in mode line when `display-time-mode' is enabled.
  (display-time-format "%a %d %b, %H:%M")
  (display-time-default-load-average nil)
  ;; Timezones to be displayed by `world-clock'. Zones names can be found
  ;; here: https://www.timezoneconverter.com/cgi-bin/tzc.
  (world-clock-list
   '(("UTC" "UTC")
     ("Australia/Melbourne" "Melbourne")
     ("Australia/Sydney" "Sydney")
     ("Australia/Brisbane" "Brisbane")
     ("America/New_York" "NY")
     ("America/Los_Angeles" "LA")
     ("America/Denver" "Denver")
     ("Asia/Tokyo" "Tokyo")
     ("Canada/Eastern" "Toronto")
     ("Europe/London" "London")
     ("Europe/Paris" "Paris")
     ("Europe/Vilnius" "Vilnius"))))

;; See `tzc-world-clock' and `tzc-convert-time'.
(use-package tzc
  :custom
  (tzc-favourite-time-zones-alist world-clock-list))

;;;; Calculator

(use-package calc
  :ensure nil
  :bind
  ;; Use also 'C-x *' to launch `calc-dispatch'.
  ("C-x C-*" . full-calc))

;;;; Spelling

(use-package jinx
  :bind
  ("<f8>" . jinx-mode)
  ("S-<f8>" . global-jinx-mode)
  ("M-$" . jinx-correct)
  ("C-M-$" . jinx-correct-all)
  :hook (elpaca-after-init . global-jinx-mode)
  :custom
  ;; Default to US English. Run 'M-x jinx-languages' to switch.
  (jinx-languages "en_US")
  :config
  ;; Don't spellcheck strings in programming modes.
  (add-to-list 'jinx-exclude-faces '(prog-mode font-lock-string-face)))

;;;; Package Management

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

;;;; AI/LLM

(use-package gptel
  :preface
  (declare-function gptel-make-anthropic "gptel")
  (declare-function gptel-make-gemini "gptel")
  :custom
  (gptel-default-mode 'org-mode)
  (gptel-prompt-prefix-alist '((org-mode . "*Prompt ❯❯❯* ")))
  (gptel-response-prefix-alist '((org-mode . "*Response ❯❯❯* ")))
  (gptel-display-buffer-action '(pop-to-buffer-same-window))
  :bind
  (("C-z RET" . gptel-send)
   ("C-z m" . gptel-menu)
   ("C-z a" . gptel-add)
   ("C-z f" . gptel-add-file)
   ("C-z k" . gptel-abort)
   ("C-z r" . gptel-rewrite)
   :map gptel-mode-map
   ("C-z o" . gptel-org-set-topic)
   ("C-z O" . gptel-org-set-properties))
  :config
  ;; Load extras package to configure directives.
  (require 'gptel-extras)
  ;; Register additional backends.
  (gptel-make-anthropic "Claude-Sonnet"
    :stream t
    :key gptel-api-key
    :models '(claude-3-7-sonnet-20250219))
  (gptel-make-anthropic "Claude-Opus"
    :stream t
    :key gptel-api-key
    :models '(claude-3-opus-20240229))
  (gptel-make-gemini "Gemini"
    :key gptel-api-key
    :stream t))

;; Hide cruft in separate package.
(use-package gptel-extras
  :if (file-directory-p "~/dev/home/gptel-extras")
  :load-path "~/dev/home/gptel-extras"
  :bind
  ("C-z C-z" . gptel-extras-chat)
  ("C-z SPC" . gptel-extras-select-model))

(use-package gptel-quick
  :ensure (:host github :repo "karthink/gptel-quick")
  :bind
  ("C-z h" . gptel-quick)
  :init
  (with-eval-after-load 'embark
    (bind-key "?" #'gptel-quick 'embark-general-map)))

;;;; Work Configuration

(use-package chronosphere
  :if (file-directory-p "~/dev/home/chronosphere")
  :load-path "~/dev/home/chronosphere"
  :hook (elpaca-after-init . chronosphere-init))

;;; End:
(provide 'init)

;;; init.el ends here
