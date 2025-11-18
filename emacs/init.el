;;; init.el --- Emacs Init -*- lexical-binding: t -*-

;; Author: Ashlin Eldridge <ashlin.eldridge@gmail.com>
;; URL: https://github.com/ashlineldridge/.config
;; Version: 1.0.0
;; Package-Requires: ((emacs "31.0"))

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
  :no-require
  :preface
  (defvar my/theme 'ef-dark)
  (defvar my/theme-headings
    '((1 . (variable-pitch semibold 1.2))
      (t . (variable-pitch semibold 1.1))))
  :hook
  (elpaca-after-init . (lambda () (load-theme my/theme :no-confirm))))

(use-package modus-themes
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-custom-auto-reload t)
  (modus-themes-mixed-fonts t)
  (modus-themes-prompts '(bold))
  (modus-themes-org-blocks 'gray-background)
  (modus-themes-headings my/theme-headings))

(use-package ef-themes
  :custom
  (ef-themes-mixed-fonts t)
  (ef-themes-variable-pitch-ui t)
  (ef-themes-headings my/theme-headings))

;;;;; Fonts

(use-package fontaine
  :preface
  (defconst my/fixed-family "Iosevka Comfy")
  (defconst my/variable-family "Iosevka Comfy Motion Duo")
  (defun my/fontaine-apply-preset ()
    "Apply the current (or default) Fontaine preset."
    (fontaine-set-preset (or fontaine-current-preset 'desktop)))
  :bind
  ("C-c x f" . fontaine-set-preset)
  :hook
  (elpaca-after-init . my/fontaine-apply-preset)
  :custom
  (fontaine-presets
   `((laptop1)
     (laptop2
      :default-family ,my/fixed-family
      :default-height 140
      :default-weight regular
      :fixed-pitch-weight regular
      :variable-pitch-weight regular
      :mode-line-active-height 140
      :mode-line-inactive-height 140
      :line-number-height 130)
     (desktop
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
  ;; TODO: Either make this look good or get rid of it.
  :disabled)

;;;;; Mode Line

(use-package smol
  :if (file-directory-p "~/dev/home/smol")
  :load-path "~/dev/home/smol"
  :hook
  (elpaca-after-init . smol-init)
  :custom
  (smol-string-truncate-length 80))

;;;;; Margins/Padding

(use-package spacious-padding
  :preface
  (defun my/spacious-padding-toggle-subtle ()
    "Toggle whether the mode and header line are displayed in the subtle style."
    (interactive)
    (let ((show-subtle (not spacious-padding-subtle-frame-lines)))
      (setq spacious-padding-subtle-frame-lines show-subtle))
    (when spacious-padding-mode
      (spacious-padding-mode -1)
      (spacious-padding-mode 1)))
  :hook
  (elpaca-after-init . spacious-padding-mode)
  :bind
  ("C-c x _" . my/spacious-padding-toggle-subtle)
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
  ("C-c x n" . display-line-numbers-mode)
  ("C-c x N" . global-display-line-numbers-mode))

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
  :preface
  (defun my/scroll-up-line-other-window (&optional arg)
    "Scroll up by ARG (or one) lines in the other window."
    (interactive)
    (with-selected-window (other-window-for-scrolling)
      (scroll-up-line arg)))

  (defun my/scroll-down-line-other-window (&optional arg)
    "Scroll down by ARG (or one) lines in the other window."
    (interactive)
    (with-selected-window (other-window-for-scrolling)
      (scroll-down-line arg)))

  (defun my/other-window-for-scrolling ()
    "Custom window selection for scrolling other window."
    ;; Prioritize right, then left, then next.
    (or (window-in-direction 'right)
        (window-in-direction 'left)
        (next-window)))

  :bind
  (("M-[" . previous-buffer)
   ("M-]" . next-buffer)
   ("M-q" . bury-buffer)
   ("M-O =" . balance-windows)
   ("M-O }" . enlarge-window-horizontally)
   ("M-O {" . shrink-window-horizontally)
   ("M-O ]" . enlarge-window)
   ("M-O [" . shrink-window)
   ("M-S-<down>" . scroll-up-line)
   ("M-S-<up>" . scroll-down-line)
   ("C-M-<next>" . scroll-other-window)
   ("C-M-<prior>" . scroll-other-window-down)
   ("C-M-<down>" . my/scroll-up-line-other-window)
   ("C-M-<up>" . my/scroll-down-line-other-window)
   :repeat-map my/window-repeat-map
   ("}" . enlarge-window-horizontally)
   ("{" . shrink-window-horizontally)
   ("]" . enlarge-window)
   ("[" . shrink-window))
  :custom
  ;; WORK-IN-PROGRESS: Experimenting with all window display settings.
  ;; See: https://www.gnu.org/software/emacs/manual/html_node/elisp/The-Zen-of-Buffer-Display.html.
  (even-window-sizes 'height-only)
  (split-height-threshold 100)
  (split-width-threshold 120)
  (window-min-height 6)
  (window-min-width 30)
  (window-combination-resize t)
  ;; Display buffer configuration.
  (display-buffer-alist
   `(;; Hide by default. The `allow-no-window' setting isn't always required
     ;; and it the has side effect of breaking other-window style commands.
     ("\\*Warnings\\*"
      (display-buffer-no-window)
      (allow-no-window . t))
     ;; Display below current window in a regular window.
     ("\\(CAPTURE-.*\\.org\\|\\*vc-log\\*\\)"
      (display-buffer-below-selected)
      (window-height . 16))
     ;; Display below current window in a side window with no mode line.
     ("\\*\\(Org \\(Select\\|Note\\)\\|Agenda Commands\\)\\*"
      (display-buffer-in-side-window)
      (dedicated . t)
      (side . bottom)
      (slot . 0)
      (window-parameters . ((mode-line-format . none))))
     ;; Display in other window.
     ("\\(\\*eldoc\\|\\*helpful\\)"
      (display-buffer-reuse-window)
      (inhibit-same-window . t))
     ;; Display in same window.
     ("\\(\\*Async Shell Command\\*\\|\\*Proced\\*\\|\\*vc-dir\\*\\|magit:\\)"
      (display-buffer-same-window))))

  :config
  ;; The default value of 0 causes point to be recentered when it scrolls off
  ;; screen. A value of 101 (or above) will mean that point is never recentered,
  ;; though for some commands (e.g. xref navigation) it can cause point to be
  ;; shown in an awkward position when you want recentering to occur. A value of
  ;; 1 provides a good balance such that point is prevented from scrolling off
  ;; screen but it is recentered when moving by one line doesn't bring point on
  ;; screen again (e.g. during xref navigation).
  (setq scroll-conservatively 1)
  (setq other-window-scroll-default #'my/other-window-for-scrolling))

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
         (define-key ,map (kbd "M-o") #',name))))

  ;; Create additional Embark actions that support Ace Window.
  (my/define-ace-embark-action my/ace-embark-buffer embark-buffer-map switch-to-buffer)
  (my/define-ace-embark-action my/ace-embark-file embark-file-map find-file)
  (my/define-ace-embark-action my/ace-embark-bookmark embark-bookmark-map bookmark-jump)
  :bind
  ("M-o" . ace-window)
  :custom
  (aw-scope 'global)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j))
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
          (?b my/ace-switch-buffer "Switch Buffer")
          (?k my/ace-kill-buffer "Kill Buffer")
          (?K my/ace-kill-buffer-delete-window "Kill Buffer and Window")
          (?q my/ace-bury-buffer "Bury Buffer")
          (?c my/ace-copy-window "Copy Window")
          (?m my/ace-move-window "Move Window")
          (?w my/ace-swap-window "Swap Window")
          (?? aw-show-dispatch-help))))

;;;;; Window History

(use-package winner
  :ensure nil
  :bind
  ("M-U" . winner-undo)
  ("M-R" . winner-redo)
  :hook (elpaca-after-init . winner-mode)
  :custom
  (winner-dont-bind-my-keys t))

;;;;; Frame Management

(use-package frame
  :ensure nil
  :preface
  (defun my/toggle-frame-jump-scope ()
    "Toggle the scope of Ace and Avy between frame local and global."
    (interactive)
    (let ((scope aw-scope))
      (setq aw-scope (if (eq scope 'global) 'frame 'global))
      (setq avy-all-windows (if (eq scope 'global) t 'all-frames))
      (message "Set frame jump scope: %s" (symbol-name aw-scope))))

  :bind
  ;; Remove silly `suspend-frame' bindings.
  ("C-z" . nil)
  ("C-x C-z" . nil)
  ("M-O M-O" . other-frame)
  ("M-O M-N" . make-frame-command)
  ("M-O M-K" . delete-frame)
  ("M-O M-U" . undelete-frame)
  ("M-O M-T" . my/toggle-frame-jump-scope)
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
  :demand t
  :preface
  (declare-function breadcrumb-local-mode "breadcrumb")
  (defun my/breadcrumb-show ()
    "Show breadcrumb if not a minibuffer and there is no special header line."
    (when (and
           (not (minibufferp))
           (not (derived-mode-p 'proced-mode))
           (listp header-line-format))
      (breadcrumb-local-mode 1)))

  ;; Some package like `ibuffer' insist on modifying the header line.
  ;; This function provides around advice for restoring it.
  (defun my/breadcrumb-restore (fn &rest args)
    "Around advice to restore the breadcrumb header line."
    (let ((old header-line-format))
      (apply fn args)
      (setq header-line-format old)))
  :custom
  (breadcrumb-project-max-length 0.4)
  (breadcrumb-imenu-max-length 0.4)
  :defer 0.25
  :config
  ;; Add the breadcrumb to all existing buffers (e.g. *Messages*) and install
  ;; a hook for new ones. This could be done as a global minor mode.
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (my/breadcrumb-show)))
  (add-hook 'after-change-major-mode-hook #'my/breadcrumb-show))

;;;; Help System

(use-package help-buffer
  :ensure nil
  :no-require
  :preface
  (defvar my/help-buffer-kill-to-close nil)

  (defun my/help-buffer (&optional arg)
    "Show an appropriate help buffer for the current symbol at point.
With prefix ARG, all help buffers will be killed or buried depending on
the value of `my/help-buffer-kill-to-close'."
    (interactive "P")
    (if arg
        (dolist (buffer (buffer-list))
          (when (string-match-p "\\(\\*helpful\\|\\*eldoc\\)" (buffer-name buffer))
            (if my/help-buffer-kill-to-close
                (kill-buffer buffer)
              (dolist (window (get-buffer-window-list buffer nil t))
                (with-selected-window window
                  (bury-buffer))))))
      (if (bound-and-true-p eglot--managed-mode)
          (my/eldoc-buffer)
        (helpful-at-point))))
  :bind
  ("M-h" . my/help-buffer))

(use-package eldoc
  :ensure nil
  :preface
  (defvar my/eldoc-buffer-frozen nil)

  (defun my/eldoc-buffer ()
    "Show Eldoc buffer for the symbol at point and freeze it."
    (setq my/eldoc-buffer-frozen nil)
    (eldoc-print-current-symbol-info t))

  (defun my/eldoc-display-in-buffer (docs interactive)
    "Custom function for `eldoc-display-functions' to freeze the Eldoc buffer."
    (unless my/eldoc-buffer-frozen
      (eldoc-display-in-buffer docs interactive)
      (setq my/eldoc-buffer-frozen t)))
  :custom
  (eldoc-idle-delay 0.1)
  (eldoc-documentation-strategy #'eldoc-documentation-compose)
  (eldoc-echo-area-prefer-doc-buffer nil)
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-echo-area-display-truncation-message nil)
  :commands my/eldoc-doc-buffer
  :config
  (setq eldoc-display-functions '(eldoc-display-in-echo-area
                                  my/eldoc-display-in-buffer)))

(use-package helpful
  :preface
  :bind
  ("C-h c" . helpful-callable)
  ;; Replace `describe-*' bindings with Helpful where possible.
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key)
  :custom
  ;; Don't select the Helpful buffer.
  (helpful-switch-buffer-function #'display-buffer)
  (helpful-max-buffers 5)
  :commands helpful-at-point)

(use-package help-fns
  :ensure nil
  :bind
  ("C-h F" . describe-face))

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
With prefix ARG, the working directory can be selected."
    (interactive "P")
    (let ((default-directory (if (or arg (not default-directory))
                                 (read-directory-name "Working directory: ")
                               default-directory)))
      (async-shell-command
       (read-shell-command
        (concat (my/abbreviate-file-name
                 (string-remove-suffix "/" default-directory) 30) " $ "))
       (generate-new-buffer shell-command-buffer-name-async))))

  (defun my/unbind-common-keys (map)
    "Unbind (often stolen) common keybindings from MAP."
    (dolist (key '("C-z" "C-SPC" "C-M-<left>" "C-M-<right>" "C-M-a" "C-M-e"
                   "M-g" "M-j" "M-J" "M-k" "M-o" "M-s" "M-q" "M-r" "M-R" "M-U"
                   "M-." "M-?" "M-:" "M-&" "M-'" "M-\"" "M-]" "M->" "M-<"))
      (define-key map (kbd key) nil t)))

  :custom
  (indent-tabs-mode nil)
  ;; Allow shift-selection to continue any active region.
  (shift-select-mode 'permanent)
  ;; Kill more, remember more.
  (kill-ring-max 1000)
  (mark-ring-max 16)
  (global-mark-ring-max 16)
  (set-mark-command-repeat-pop t)
  (cycle-spacing-actions '((just-one-space -1) (delete-all-space -1) restore))
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
  ("M-=" . count-words)
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
  :bind
  ;; Reserve M-h for showing help buffer.
  ("M-H" . mark-paragraph)
  :custom
  (sentence-end-double-space nil))

(use-package indent
  :ensure nil
  :no-require
  :custom
  (tab-always-indent 'complete))

(use-package subword
  :ensure nil
  :hook
  (elpaca-after-init . global-subword-mode))

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

(use-package stripspace
  :hook
  ((prog-mode text-mode conf-mode) . stripspace-local-mode))

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

  (defun my/avy-action-xref (pt)
    "Show the Xref definition of the symbol at PT without moving point."
    (save-excursion
      (goto-char pt)
      (with-selected-window (selected-window)
        (call-interactively #'xref-find-definitions-other-window))))

  (defun my/avy-action-help-buffer (pt)
    "Show a help buffer for the symbol at PT without moving point."
    (save-excursion
      (goto-char pt)
      (my/help-buffer)))

  :bind
  (("M-j" . avy-goto-subword-1)
   ("M-J" . avy-goto-char-in-line)
   ("M-g c" . avy-goto-char-timer)
   ("M-g C" . avy-goto-char-in-line)
   ("M-g l" . avy-goto-line)
   ("M-g L" . my/avy-goto-end-of-line)
   ("M-g w" . avy-goto-subword-1)
   :map isearch-mode-map
   ("M-j" . avy-isearch))
  :custom
  (avy-all-windows 'all-frames)
  (avy-single-candidate-jump nil)
  (avy-timeout-seconds 0.3)
  (avy-keys '(?a ?s ?d ?f ?g ?j ?k ?l))
  (avy-styles-alist '((avy-isearch . post)))
  (avy-dispatch-alist
   '((?m . avy-action-mark)
     (?t . avy-action-teleport)
     (?w . avy-action-copy)
     (?y . my/avy-action-yank)
     (?z . avy-action-zap-to-char)
     (?x . avy-action-kill-stay)
     (?. . my/avy-action-xref)
     (?h . my/avy-action-help-buffer)))
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
  :hook
  (elpaca-after-init . pulsar-global-mode)
  (minibuffer-setup . pulsar-pulse-line)
  (next-error . pulsar-pulse-line-red)
  (next-error . pulsar-reveal-entry)
  (next-error . pulsar-recenter-center)
  :custom
  (pulsar-delay 0.05)
  (pulsar-iterations 13)
  (pulsar-face 'pulsar-green)
  :config
  ;; Add extra functions that should trigger Pulsar.
  (setq pulsar-pulse-functions
        (append pulsar-pulse-functions
                '(ace-window
                  avy-goto-line
                  avy-goto-subword-1
                  avy-goto-char-timer
                  avy-isearch
                  beginning-of-buffer
                  beginning-of-defun
                  ;; TODO: Reveals/centers but doesn't pulse?
                  consult-preview-at-point
                  diff-hunk-next
                  diff-hunk-prev
                  end-of-buffer
                  end-of-defun
                  eshell-next-prompt
                  eshell-previous-prompt
                  flymake-goto-next-error
                  flymake-goto-prev-error
                  isearch-repeat-backward
                  isearch-repeat-forward
                  magit-section-backward
                  magit-section-forward
                  my/avy-goto-end-of-line
                  other-frame
                  pop-global-mark
                  treesit-beginning-of-defun
                  treesit-end-of-defun
                  vterm-next-prompt
                  vterm-previous-prompt
                  xref-find-definitions
                  xref-go-back
                  xref-go-forward)))

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
      ("AI" (predicate my/ai-buffer-p (current-buffer)))
      ("Shell" (predicate my/shell-buffer-p (current-buffer)))
      ("Command" (mode . shell-command-mode))
      ("Dired" (mode . dired-mode))
      ("Org" (or (mode . org-mode) (mode . org-agenda-mode))))))
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-use-header-line nil)
  :config
  (my/unbind-common-keys ibuffer-mode-map)
  ;; Restore breadcrumb after ibuffer after messes with it.
  (advice-add #'ibuffer-update :around #'my/breadcrumb-restore))

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
  (defun my/save-kill-buffer ()
    "Save the current buffer (if applicable) and then kill it."
    (interactive)
    (when (and buffer-file-name (buffer-modified-p))
      (save-buffer))
    (kill-current-buffer))

  (defun my/save-kill-buffer-other-window ()
    "Save the other window's buffer (if applicable) and then kill it."
    (interactive)
    (if (= (length (window-list)) 2)
        (with-selected-window (next-window (selected-window))
          (my/save-kill-buffer))
      (message "Command requires two windows")))

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
  ("M-\"" . my/save-kill-buffer)
  ("C-M-'" . my/save-kill-buffer-other-window)
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
With prefix ARG, the working directory can be selected."
    (interactive "P")
    (let ((default-directory (my/project-current-root)))
      (my/async-shell-command arg)))

  :bind
  ("C-M-&" . my/project-async-shell-command)
  ("C-x p j" . project-dired)
  ("C-x p u" . my/project-update-list)
  :custom
  (project-switch-commands
   '((magit-project-status "Magit" ?g)
     (project-dired "Dired" ?j)
     (my/consult-project-file "File" ?f)
     (consult-ripgrep "Ripgrep" ?s)
     (my/project-async-shell-command "Command" ?&)
     (gptel-agent "GPTel" ?z)
     (agent-shell "Agent Shell" ?Z))))

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
  :custom
  ;; Serenity trial in progress...
  (corfu-auto nil)
  (corfu-auto-delay 0.3)
  (corfu-auto-prefix 5)
  (corfu-preselect 'directory)
  (corfu-quit-at-boundary 'separator)
  (corfu-on-exact-match nil)
  (corfu-count 16)
  (corfu-preview-current nil)
  (corfu-min-width 80)
  (corfu-max-width 120)
  (corfu-scroll-margin 4)
  (global-corfu-minibuffer t)
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
  ("C-' b" . cape-elisp-block)
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
  (cape-line-buffer-function #'buffer-list)
  :init
  ;; EXPERIMENTAL: Can also play with hook depth.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file))

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

  (defun my/consult-source-buffer (name narrow filter-type filter-value)
    "Return a Consult buffer source with NAME, NARROW key and filter.
The filter is defined by FILTER-TYPE which must be :mode or :predicate and
FILTER-VALUE which should be a mode symbol or predicate function, respectively."
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
          :predicate
          (lambda (buf)
            (pcase filter-type
              (:predicate (funcall filter-value buf))
              (:mode (provided-mode-derived-p
                      (buffer-local-value 'major-mode buf) filter-value))))))))

  (defvar my/consult-source-magit-buffer
    (my/consult-source-buffer "Magit Buffer" ?g :mode 'magit-status-mode))
  (defvar my/consult-source-shell-buffer
    (my/consult-source-buffer "Shell Buffer" ?s :predicate 'my/shell-buffer-p))
  (defvar my/consult-source-dired-buffer
    (my/consult-source-buffer "Dired Buffer" ?d :mode 'dired-mode))
  (defvar my/consult-source-agenda-buffer
    (my/consult-source-buffer "Agenda Buffer" ?a :mode 'org-agenda-mode))
  (defvar my/consult-source-ai-buffer
    (my/consult-source-buffer "AI Buffer" ?z :predicate 'my/ai-buffer-p))

  ;; Consult source for all project files. This has largely been adapted from
  ;; the implementation of `consult-source-project-recent-file'.
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
         (when-let* ((project-dir (my/project-current-root)))
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
            `((:narrow ?b ,@consult-source-project-buffer) ;; Narrow: ?b (shown)
              my/consult-source-project-file               ;; Narrow: ?f (shown)
              consult-source-project-recent-file-hidden))) ;; Narrow: ?r (hidden)
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
   ("M-y" . consult-yank-pop)
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
        '(consult-source-buffer                ;; Narrow: ?b (shown)
          consult-source-project-buffer-hidden ;; Narrow: ?p (hidden)
          my/consult-source-agenda-buffer      ;; Narrow: ?a (hidden)
          my/consult-source-dired-buffer       ;; Narrow: ?d (hidden)
          my/consult-source-shell-buffer       ;; Narrow: ?s (hidden)
          my/consult-source-magit-buffer       ;; Narrow: ?g (hidden)
          my/consult-source-ai-buffer          ;; Narrow: ?z (hidden)
          consult-source-bookmark              ;; Narrow: ?m (shown)
          consult-source-recent-file))         ;; Narrow: ?r (hidden)

  ;; Customize individual Consult sources.
  (consult-customize
   consult-source-buffer
   :name "Open Buffer" :narrow ?b
   consult-source-project-buffer
   consult-source-project-buffer-hidden
   :name "Project Buffer" :narrow ?p
   consult-source-bookmark
   :name "Bookmark" :narrow ?m
   consult-source-recent-file
   :name "Recent File" :narrow ?r :hidden t
   consult-source-project-recent-file
   consult-source-project-recent-file-hidden
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
    (when-let* ((default-directory (or dir default-directory)))
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
  (declare-function embark-prefix-help-command "embark")
  :bind
  (("C-." . embark-act)
   ("C-h b" . embark-bindings)
   ;; Allow Embark to show keybindings under C-h as configured below.
   ("C-h C-h" . nil)
   :map embark-general-map
   ("&" . my/async-shell-command)
   :map minibuffer-local-map
   ("M-B" . embark-become))
  :custom
  ;; Show minimal "Act" prompt and highlight the target under point.
  (embark-indicators '(embark-minimal-indicator embark-highlight-indicator))
  ;; Use this key to switch Embark to the keymap prompter.
  (embark-keymap-prompter-key ",")
  :init
  ;; Use Embark to show prefix keybindings with C-h.
  (setq prefix-help-command #'embark-prefix-help-command)
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
   ("C-p" . isearch-repeat-backward))
  :custom
  (isearch-allow-scroll t)
  (isearch-allow-motion t)
  (isearch-lazy-count t)
  (isearch-repeat-on-direction-change t))

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
  (treesit-auto-langs '(bash dockerfile go gomod javascript json proto python rust yaml))
  :hook
  (elpaca-after-init . global-treesit-auto-mode)
  ;; Perform an idle wait before installing grammars (when necessary) to prevent
  ;; locking up on a blank screen while waiting for installation.
  :defer 1
  :config
  ;; Add all languages in `treesit-auto-langs' except Rust which uses Rustic.
  (treesit-auto-add-to-auto-mode-alist (remove 'rust treesit-auto-langs))
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
  ("C-c d" . dape-global-map)
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
    (setq-local eldoc-documentation-functions
                ;; Put at the top so I can see values in single line echo area.
                '(elisp-eldoc-var-docstring-with-value
                  elisp-eldoc-funcall
                  flymake-eldoc-function)))

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
  (my/unbind-common-keys paredit-mode-map)
  (eldoc-add-command "paredit-backward" "paredit-forward"
                     "paredit-backward-delete" "paredit-close-round"))

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
  :defines
  (vc-dir-mode-map
   vc-dir-git-mode-map)
  :custom
  (vc-follow-symlinks t)
  (vc-git-show-stash 10)
  :bind
  ;; Consolidate keybindings into a single `use-package' form as VC commands
  ;; are spread over many sub-packages. Requires need to be correct below.
  (:map vc-prefix-map
   ("RET" . vc-dir-root)
   ("e" . vc-ediff)
   ("F" . vc-update)
   ("k" . vc-revert)
   :map vc-dir-mode-map
   ("M-s" . nil)
   ("e" . vc-ediff)
   ("F" . vc-update)
   ("k" . vc-revert)
   :map vc-dir-git-mode-map
   ("z a" . vc-git-stash-apply)
   ("z k" . vc-git-stash-delete))
  :config
  (require 'vc-dir))

;; The `log-edit' package is used by VC for entering commit messages. Based on
;; todos in the code, it looks like it will be moved into VC at some point.
(use-package log-edit
  :ensure nil
  :custom
  ;; Remove unnecessary/unwanted hook functions. Use C-c C-d from the log
  ;; buffer to run `log-edit-show-diff' and C-c C-f for `log-edit-show-files'.
  (log-edit-hook '(log-edit-insert-message-template
                   log-edit-insert-changelog)))

(use-package magit
  :preface
  (declare-function magit-git-string "magit-git")
  (defun my/magit-copy-hash (arg)
    "Copy the short 12 character Git commit hash to the kill ring.
If ARG is non-nil, the full 40 character commit hash will be copied."
    (interactive "P")
    (require 'magit-git)
    (let ((sha (magit-git-string "rev-parse" "HEAD")))
      (kill-new (if arg sha (substring sha 0 12)))
      (message "Copied: %s" (car kill-ring))))

  (defun my/magit-copy-hash-full ()
    "Copy the full 40 character Git commit hash to the kill ring."
    (interactive)
    (my/magit-copy-hash t))

  :bind
  ("C-c g" . magit-dispatch)
  :custom
  ;; Use `auto-revert-mode' instead.
  (magit-auto-revert-mode nil)
  (magit-diff-refine-hunk t)
  (magit-refresh-status-buffer t)
  (magit-refresh-verbose t)
  (magit-verbose-messages t)
  :config
  ;; Add custom commands to the `magit-dispatch' transient menu as a new
  ;; subgroup. Existing suffixes that already use these bindings are removed
  ;; first. Suffix removal shifts subsequent suffixes in the subgroup up.
  ;; See: https://magit.vc/manual/transient/Modifying-Existing-Transients.html.
  (dolist (loc '((0 1 9) (0 1 9) (0 2 4) (0 2 4)))
    (transient-remove-suffix 'magit-dispatch loc))
  (transient-append-suffix 'magit-dispatch '(0 -1)
    [("s" "Status" magit-status)
     ("w" "Copy short hash" my/magit-copy-hash)
     ("W" "Copy long hash" my/magit-copy-hash-full)
     ("o" "Browse remote branch" my/browse-at-remote)
     ("O" "Browse remote trunk" my/browse-at-remote-trunk)]))

(use-package browse-at-remote
  :preface
  (declare-function browse-at-remote "browse-at-remote")
  (declare-function vc-git-branches "vc-git")

  (defun my/browse-at-remote (arg)
    "Browse to the remote file, using the trunk branch if ARG is non-nil."
    (interactive "P")
    (require 'vc-git)
    (if arg
        (cl-letf (((symbol-function 'browse-at-remote--get-local-branch)
                   (lambda ()
                     (if (member "main" (vc-git-branches)) "main" "master"))))
          (browse-at-remote))
      (browse-at-remote)))

  (defun my/browse-at-remote-trunk ()
    "Browse to the remote file using the trunk branch."
    (interactive)
    (my/browse-at-remote t)))

;;;; Diff

(use-package diff-mode
  :ensure nil
  :bind
  (:map diff-mode-map
   ("M-k" . nil)
   ("M-o" . nil)
   ("u" . diff-undo)
   ("v" . vc-next-action)
   ("l" . vc-print-log)
   ("L" . vc-print-root-log))
  :custom
  (diff-default-read-only t))

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
    "Init function for `eshell-mode'."
    ;; Don't scroll the buffer around after it has been recentered (using C-l).
    ;; This seems to need to be done as a mode hook rather than in `:config' as
    ;; the latter results in `eshell-output-filter-functions' being set to nil.
    ;; See: https://emacs.stackexchange.com/a/45281
    (remove-hook 'eshell-output-filter-functions
                 'eshell-postoutput-scroll-to-bottom)
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

  (defun my/shell-buffer-p (buf)
    "Return whether BUF is considered a generalized shell buffer."
    (let ((mm (buffer-local-value 'major-mode buf)))
      (provided-mode-derived-p mm '(eshell-mode eat-mode vterm-mode))))
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
  (eshell-visual-commands
   '("claude" "cursor-agent" "gemini" "goose" "vim"))
  :config
  (require 'esh-mode)
  (require 'em-hist))

(use-package eat
  :ensure
  ;; Include file list recommended in docs.
  (:files ("*.el" "*.texi" "*.ti"
           ("term" "term/*.el")
           ("terminfo/e" "terminfo/e/*")
           ("terminfo/65" "terminfo/65/*")
           ("integration" "integration/*")
           (:exclude ".dir-locals.el" "*-tests.el")))
  :preface
  (defun my/eat-new-line ()
    "Send a new line to the current `eat' terminal."
    (interactive)
    (eat-term-send-string eat-terminal "\C-j"))
  :bind
  (:map eat-mode-map
   ("C-M-a" . eat-previous-shell-prompt)
   ("C-M-e" . eat-next-shell-prompt)
   ("S-<return>" . my/eat-new-line))
  :hook
  ;; I prefer the experience of opening terminal apps in a dedicated Eat buffer
  ;; rather than enabling `eat-eshell-mode' and running them directly within
  ;; Eshell. Terminal commands are registered in `eshell-visual-commands'.
  (eshell-load . eat-eshell-visual-command-mode)
  :custom
  (eat-term-name "xterm-256color")
  (eat-term-scrollback-size 500000)
  :config
  (my/unbind-common-keys eat-char-mode-map)
  (my/unbind-common-keys eat-semi-char-mode-map)
  (my/unbind-common-keys eat-eshell-char-mode-map))

(use-package vterm
  :defines (vterm-mode-map vterm-eval-cmds)
  :preface
  (declare-function vterm-send-key "vterm")
  (declare-function vterm-previous-prompt "vterm")
  (declare-function vterm-next-prompt "vterm")
  (declare-function project-prefixed-buffer-name "project")

  (defun my/vterm-init ()
    "Init function for `vterm-mode'."
    ;; Make outline work with vterm prompts.
    (setq-local outline-regexp "^[^#$\n]* ❯ "))

  :bind
  (:map vterm-mode-map
   ;; These only work reliably when copy mode is disabled.
   ("C-M-a" . vterm-previous-prompt)
   ("C-M-e" . vterm-next-prompt)
   ;; Because I always press C-g to cancel things.
   ("C-g" . (lambda () (interactive) (vterm-send-key (kbd "C-c"))))
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
  (my/unbind-common-keys vterm-mode-map)
  (add-to-list 'vterm-eval-cmds
               '("find-file-other-window" find-file-other-window)))

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
    (display-line-numbers-mode -1)
    (setq-local line-spacing 2)
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
   ;; Rebind conflicting keybindings.
   ("M-h" . nil)
   ("M-H" . org-mark-element)
   ("C-'" . nil)
   ("C-<return>" . nil)
   ("C-S-<return>" . nil)
   ("C-c C-<return>" . org-insert-heading-respect-content)
   ("C-c C-S-<return>" . org-insert-todo-heading-respect-content)
   ("C-c o M-l" . org-id-copy))
  :hook
  (org-mode . my/org-init)
  (org-mode . org-indent-mode)
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
  (org-fontify-emphasized-text t)
  (org-hide-emphasis-markers nil)
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
  (my/unbind-common-keys org-mode-map)
  ;; Make it easier to create `org-babel' code blocks.
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  ;; Add some user feedback.
  (advice-add #'org-id-copy
              :after (lambda (&optional _) (message "Copied: %s" (car kill-ring)))))

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
    (when-let* ((state-a (get-text-property 14 'todo-state a))
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
  ("C-c o b" . (lambda () (interactive) (org-capture nil "b")))
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
  ;; Prefer custom mode line indicator rather than `mode-line-misc-info'.
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
        "Replace or delete this section\n"
        "* Links\n"
        "** Roam Links\n"
        "- Use org-roam-node-insert (C-c n i) here\n"
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

;; TODO: Raise issues to allow cities file to be customized and remove
;; unnecessary header/footer whitespace.
(use-package time-zones
  :preface
  (declare-function no-littering-expand-var-file-name "no-littering")
  :custom
  (time-zones-show-details nil)
  (time-zones-show-help nil)
  :config
  (setq time-zones--city-list-file
        (no-littering-expand-var-file-name "time-zones.el")))

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

;;;; Work Configuration

(use-package chronosphere
  :if (file-directory-p "~/dev/home/chronosphere")
  :load-path "~/dev/home/chronosphere"
  :hook (elpaca-after-init . chronosphere-init))

;;;; AI

(use-package ai
  :ensure nil
  :no-require
  :preface
  (defun my/ai-buffer-p (buf)
    "Return whether BUF is considered to be an AI/LLM/agent buffer."
    (with-current-buffer buf
      (cl-some (lambda (m)
                 (or (derived-mode-p m) (and (boundp m) (symbol-value m))))
               '(gptel-mode agent-shell-mode)))))

(use-package gptel
  :preface
  (defvar my/gptel-prompt-prefix "=@ash=\n")
  (defvar my/gptel-response-prefix "=@bot=\n")
  (defun my/gptel-init ()
    "Init function for `gptel-mode'."
    (org-indent-mode -1)
    (electric-pair-local-mode -1))
  :custom
  (gptel-default-mode 'org-mode)
  (gptel-track-media t)
  (gptel-include-reasoning t)
  (gptel-org-branching-context t)
  (gptel-display-buffer-action '(pop-to-buffer-same-window))
  (gptel-prompt-prefix-alist `((org-mode . ,my/gptel-prompt-prefix)))
  (gptel-response-prefix-alist `((org-mode . ,my/gptel-response-prefix)))
  :bind
  (("C-z m" . gptel-menu)
   ("C-z M" . gptel-mode)
   ("C-z h" . gptel-highlight-mode)
   ("C-z k" . gptel-abort)
   ("C-z r" . gptel-rewrite)
   ("C-z RET" . gptel-send)
   :map gptel-mode-map
   ("C-z o" . gptel-org-set-topic)
   ("C-z O" . gptel-org-set-properties))
  :hook
  (gptel-mode . my/gptel-init)
  :config
  (setq gptel-expert-commands t))

(use-package gptel-vertex
  :if (file-directory-p "~/dev/home/gptel-vertex")
  :load-path "~/dev/home/gptel-vertex"
  :after (gptel chronosphere)
  :demand t
  :commands gptel-make-vertex
  :config
  (setq gptel-model chronosphere-default-anthropic-model
        gptel-backend (gptel-make-vertex
                       "VertexAI"
                       :project-id chronosphere-vertex-project
                       :location chronosphere-vertex-location)))

(use-package gptel-agent
  ;; TODO: Using my own fork for now so that I can make tweaks and also
  ;; using the default set of agents that come with the package. Eventually,
  ;; I think I'd prefer to define my own agents/presets in a custom directory.
  :ensure (:host github :repo "ashlineldridge/gptel-agent" :files (:defaults "agents"))
  :bind
  ("C-z C-z" . gptel-agent)
  :config
  (gptel-agent-update))

(use-package gptel-quick
  :ensure (:host github :repo "karthink/gptel-quick")
  :bind
  ("C-z ?" . gptel-quick)
  :init
  (with-eval-after-load 'embark
    (bind-key "?" #'gptel-quick 'embark-general-map)))

(use-package agent-shell
  :ensure (:host github :repo "xenodium/agent-shell")
  :preface
  (defun my/agent-shell-init ()
    "Init function for `agent-shell-mode'."
    (setq-local indent-line-function (lambda () 'noindent)))
  :hook
  (agent-shell-mode . my/agent-shell-init)
  :bind
  (("C-z > >" . agent-shell)
   ("C-z > s" . agent-shell-set-session-mode)
   :map agent-shell-mode-map
   ;; Mirror `gptel' newline and abort behavior.
   ("RET" . newline)
   ("C-c RET" . agent-shell-submit)
   ("C-z k" . agent-shell-interrupt)
   ;; Defun-style prompt navigation.
   ("C-M-a" . comint-previous-prompt)
   ("C-M-e" . comint-next-prompt)
   ("C-M-f" . agent-shell-next-item)
   ("C-M-b" . agent-shell-previous-item))
  :custom
  (agent-shell-header-style 'text)
  ;; We'll use `cape-file' for consistency with `gptel'.
  (agent-shell-file-completion-enabled nil)
  :config
  ;; Use GCP authentication.
  (setq agent-shell-goose-authentication
        (agent-shell-make-goose-authentication :none t))
  ;; Clear existing bindings as contains tabs, etc.
  (setq agent-shell-mode-map (make-sparse-keymap)))

(use-package acp
  :ensure (:host github :repo "xenodium/acp.el"))

(use-package shell-maker
  :custom
  (shell-maker-transcript-default-path
   (expand-file-name "~/.local/share/agent-shell/transcripts/"))
  (shell-maker-transcript-default-filename
   (lambda () (format-time-string "%F-%T-transcript.txt"))))

;;; End:
(provide 'init)

;;; init.el ends here
