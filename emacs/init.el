;;; init.el --- Emacs Init -*- lexical-binding: t -*-

;; Author: Ashlin Eldridge <ashlin.eldridge@gmail.com>
;; URL: https://github.com/ashlineldridge/.config
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.0"))

;;; Commentary:
;;
;; My bonsai.

;;; Code:

;;;; System Check

;; For now, this config only supports window systems.
(unless window-system
  (error "This Emacs configuration is for window systems only"))

;;;; Base Settings

(use-package emacs
  :straight nil
  :hook
  ;; Display line numbers in certain modes.
  ((prog-mode config-mode text-mode) . display-line-numbers-mode)
  ;; Don't wrap long lines in programming modes.
  (prog-mode . (lambda () (setq-local truncate-lines t)))

  :bind
  (:map global-map
        ("C-x m" . nil)   ;; Remove `compose-mail' binding.
        ("C-h C-h" . nil) ;; Remove `help-for-help' binding.
        ("<escape>". keyboard-escape-quit)
        ("C-;" . comment-line)
        ("M-[" . previous-buffer)
        ("M-]" . next-buffer)
        ("C-x 2" . my/split-window-vertically)
        ("C-x 3" . my/split-window-horizontally)
        ("C-x C-k" . kill-this-buffer)
        ("C-x w w" . my/toggle-show-trailing-whitespace)
        ("C-x w k" . delete-trailing-whitespace)
        ("C-c C-c |" . display-fill-column-indicator-mode)
        ("C-S-k" . my/copy-to-eol)
        ("C-M-k" . my/delete-to-eol)
        ("M-<backspace>" . my/delete-to-bol))

  :custom
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
  (savehist-mode t)
  ;; Increase margins slightly.
  (fringe-mode 5)
  ;; Ignore any changes made via the customization UI.
  (custom-file (make-temp-file "emacs-custom-"))
  ;; Recommended no-littering settings for backup files.
  ;; See https://github.com/emacscollective/no-littering#backup-files.
  (backup-directory-alist
   `(("\\`/tmp/" . nil)
     ("\\`/dev/shm/" . nil)
     ("." . ,(no-littering-expand-var-file-name "backup/"))))
  ;; Recommended no-littering settings for auto-save files.
  ;; See https://github.com/emacscollective/no-littering#auto-save-files.
  (auto-save-file-name-transforms
   `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
      ,(concat temporary-file-directory "\\2") t)
     ("\\`\\(/tmp\\|/dev/shm\\)\\([^/]*/\\)*\\(.*\\)\\'" "\\3")
     (".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  ;; Enable recursive editing to allow multiple minibuffers to be opened on top
  ;; of each other. E.g., this allows starting a query-replace, then open a
  ;; file to look at something, then go back to the query-replace minibuffer.
  (enable-recursive-minibuffers t)
  ;; Display a small "[n]" that shows the minibuffer recursive depth. Another
  ;; option is to use https://github.com/minad/recursion-indicator.
  (minibuffer-depth-indicate-mode 1)
  ;; Timezones to be displayed by `world-clock'.
  (world-clock-list
   '(("Australia/Melbourne" "Melbourne")
     ("America/Los_Angeles" "Seattle")
     ("America/New_York" "New York")
     ("Europe/London" "London")
     ("Europe/Paris" "Paris")
     ("Europe/Vilnius" "Lithuania")
     ("Asia/Tokyo" "Tokyo")))

  :init
  ;; Use y/n rather than yes/no.
  (defalias 'yes-or-no-p #'y-or-n-p)

  ;; Enable Emacs functions that are disabled by default.
  (put 'narrow-to-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)

  (defun my/split-window-vertically ()
    "Split window vertically and select the other window."
    (interactive)
    (split-window-vertically)
    (other-window 1))

  (defun my/split-window-horizontally ()
    "Split window horizontally and select the other window."
    (interactive)
    (split-window-horizontally)
    (other-window 1))

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

  (defun my/query-replace-case-sensitive ()
    "Calls `query-replace' in a case-sensitive way."
    (interactive)
    (let (case-fold-search)
      (call-interactively #'query-replace)))

  (defun my/query-replace-regexp-case-sensitive ()
    "Calls `query-replace-regexp' in a case-sensitive way."
    (interactive)
    (let (case-fold-search)
      (call-interactively #'query-replace-regexp)))

  (defun my/flash-mode-line ()
    "Flashes the mode line for a visible bell."
    (invert-face 'mode-line)
    (run-with-timer 0.1 nil 'invert-face 'mode-line))

  ;; Prefer running a single instance of Emacs in server mode.
  (require 'server)
  (unless (server-running-p)
    (server-start)))

;;;; Keybindings

(use-package hydra)

;;;; Appearance

;;;;; Theme

;; The Modus themes are pre-installed into Emacs 28+ but I pull latest.
(use-package modus-themes
  :functions my/apply-font-config
  :bind
  (:map global-map
        ("<f12>" . my/cycle-font-config))
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-region '(no-extend accented))
  (modus-themes-mode-line '(borderless))
  (modus-themes-paren-match '(intense underline))
  (modus-themes-prompts '(bold intense))
  (modus-themes-org-blocks 'gray-background)
  (modus-themes-fringes nil)
  (modus-themes-headings
   '((1 . (variable-pitch rainbow background 1.3))
     (2 . (variable-pitch rainbow background semibold 1.2))
     (3 . (variable-pitch rainbow background semibold 1.1))
     (t . (variable-pitch rainbow semilight 1.1))))

  :init
  (load-theme 'modus-vivendi t)

  (defvar my/font-configs
        '((:name "Laptop"
                 :fixed-font "Iosevka Fixed SS14"
                 :fixed-font-size 140
                 :fixed-font-weight normal
                 :variable-font "Iosevka Aile"
                 :variable-font-size 140
                 :line-number-font-size 120
                 :mode-line-font-size 130)
          (:name "Desktop"
                 :fixed-font "Iosevka Fixed SS14"
                 :fixed-font-size 148
                 :fixed-font-weight normal
                 :variable-font "Iosevka Aile"
                 :variable-font-size 148
                 :line-number-font-size 124
                 :mode-line-font-size 136)))

  (defun my/apply-font-config (index)
    "Apply the INDEX'th font configuration from `my/font-configs'."
    (let* ((config (nth index my/font-configs))
           (name (plist-get config :name))
           (fixed-font (plist-get config :fixed-font))
           (fixed-font-size (plist-get config :fixed-font-size))
           (fixed-font-weight (plist-get config :fixed-font-weight))
           (variable-font (plist-get config :variable-font))
           (variable-font-size (plist-get config :variable-font-size))
           (line-number-font-size (plist-get config :line-number-font-size))
           (mode-line-font-size (plist-get config :mode-line-font-size)))
      (set-face-attribute 'default nil
                          :font fixed-font
                          :height fixed-font-size
                          :weight fixed-font-weight)
      (set-face-attribute 'variable-pitch nil
                          :font variable-font
                          :height variable-font-size)
      (set-face-attribute 'line-number nil
                          :font fixed-font
                          :height line-number-font-size
                          :weight 'ultra-light)
      (set-face-attribute 'mode-line nil
                          :height mode-line-font-size)
      (set-face-attribute 'mode-line-inactive nil
                          :height mode-line-font-size)
      (message "Applied font configuration: %s" name)))

  (defvar my/font-config-index 0)

  (defun my/cycle-font-config ()
    "Cycle to the next font configuration."
    (interactive)
    (setq my/font-config-index (mod (+ my/font-config-index 1)
                                    (length my/font-configs)))
    (my/apply-font-config my/font-config-index))

  ;; Apply the initial font configuration.
  (my/apply-font-config my/font-config-index))

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
  :hook (emacs-startup . nerd-icons-completion-mode))

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

;;;; Window Management

;;;;; Window Manipulation and Selection

(use-package ace-window
  :after consult
  :commands (aw-switch-to-window aw-delete-window)
  :bind
  (:map global-map
        ("M-o" . ace-window))
  :custom
  (aw-display-mode-overlay t)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?r ?t ?y ?u ?i ?o))
  (aw-dispatch-always t)
  (aw-background t)
  :config
  (defun my/ace-open-buffer (window)
    "Opens a buffer in WINDOW using `consult-buffer'."
    (aw-switch-to-window window)
    (consult-buffer))

  (defun my/ace-delete-window-buffer (window)
    "Ace Window dispatch action for deleting WINDOW and killing its buffer."
    (aw-delete-window window t))

  ;; Personalize Ace Window's dispatch menu (doesn't work when customized via
  ;; the :custom block so need to do via `setq' here).
  (setq aw-dispatch-alist
        '((?0 aw-delete-window "Delete Window")
          (?9 my/ace-delete-window-buffer "Delete Window and Kill Buffer")
          (?1 delete-other-windows "Delete Other Windows")
          (?w aw-swap-window "Swap Windows")
          (?m aw-move-window "Move Window")
          (?c aw-copy-window "Copy Window")
          (?b my/ace-open-buffer "Open Buffer")
          (?2 aw-split-window-vert "Split Vertical")
          (?3 aw-split-window-horz "Split Horizontal")
          (?? aw-show-dispatch-help))))

;; Brings in useful functions such as `transpose-frame', `flip-frame', etc.
;; See: https://www.emacswiki.org/emacs/TransposeFrame.
(use-package transpose-frame)

;;;;; Window History

(use-package winner
  :straight nil
  :init
  (winner-mode 1))

;;;;; General Window Keybindings

(use-package window
  :straight nil
  :custom
  ;; The following customizations result in Emacs not trying to be clever with
  ;; my windows. Generally, when I open a buffer I want it to occupy the same
  ;; window unless a more appropriate window for that buffer already exists.
  (display-buffer-base-action '((display-buffer-reuse-mode-window
                                 display-buffer-reuse-window
                                 display-buffer-same-window)))
  (even-window-sizes nil)
  :init
  (defhydra my/hydra-manage-windows (global-map "C-o")
    ("a" ace-window :exit t)
    ("p" previous-window-any-frame :exit t)
    ("<left>" shrink-window-horizontally)
    ("<right>" enlarge-window-horizontally)
    ("<down>" shrink-window)
    ("<up>" enlarge-window)
    ("=" balance-windows :exit t)
    ("]" rotate-frame-clockwise)
    ("[" rotate-frame-anticlockwise)
    ("x" flip-frame :exit t) ;; Flip about x-axis.
    ("y" flop-frame :exit t) ;; Flip about y-axis.
    ("u" winner-undo :exit t)
    ("r" winner-redo :exit t)
    ("q" nil)))

;;;;; Window Placement

;; For more advanced configuration, see:
;; https://github.com/seagle0128/.emacs.d/blob/8cbec0c132cd6de06a8c293598a720d377f3f5b9/lisp/init-window.el#L148.
(use-package popper
  :commands
  (popper-mode popper-echo-mode popper-kill-latest-popup popper-open-latest)
  :bind
  (:map global-map
        ("C-'" . popper-toggle-latest)
        ("M-'" . popper-cycle)
        ("C-M-'" . popper-toggle-type))
  (:map popper-mode-map
        ("M-k" . my/popper-kill-popup-stay-open)
        ("M-K" . popper-kill-latest-popup))

  :preface
  (defvar my/popper-ignore-modes '(grep-mode))

  :custom
  (popper-window-height 15)
  (popper-reference-buffers
   '("\\*Messages\\*"
     "\\*Warnings\\*"
     "\\*Backtrace\\*"
     "\\*Breakpoints\\*"
     "\\*Pp Macroexpand Output\\*"
     "\\*Flycheck "
     "\\*dap-ui-"
     "\\*Help\\*"
     "\\*helpful "
     "\\*eldoc for "
     "\\*eshell-output\\*"
     "CAPTURE-.*\\.org"
     "\\*Shell Command Output\\*"
     "\\*Async Shell Command\\*"
     "\\*Call Hierarchy\\*"

     ;; Match all modes that derive from compilation-mode but do not derive
     ;; from a member of `my/popper-ignore-modes'.
     (lambda (buf)
       (with-current-buffer buf
         (unless (derived-mode-p
                  (car (member major-mode my/popper-ignore-modes)))
           (derived-mode-p 'compilation-mode))))))

  (popper-echo-dispatch-keys '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))
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

(use-package helpful
  :bind
  ("C-h c" . helpful-callable)
  ("C-h ." . helpful-at-point)
  ;; Replace `describe-*' bindings with Helpful.
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

(use-package which-key
  :commands which-key-mode
  :custom
  ;; Use Embark for `prefix-help-command' as it is searchable.
  (which-key-show-early-on-C-h nil)
  (which-key-use-C-h-commands nil)
  (which-key-idle-delay 2)
  (which-key-idle-secondary-delay 0.05)
  (which-key-popup-type 'side-window)
  (which-key-side-window-location '(bottom right))
  :init
  (which-key-mode 1))

;;;; Completion System

(use-package corfu
  :straight (corfu-mode :host github :repo "minad/corfu")
  :commands (corfu-mode global-corfu-mode)
  :functions consult-completion-in-region

  :bind
  (:map corfu-map
        ;; By default, `corfu-insert-separator' is bound to M-SPC which on
        ;; macOS is already taken by Spotlight. Instead, bind it to S-SPC -
        ;; this allows us to enter a space character using S-SPC to completing.
        ("S-SPC" . corfu-insert-separator)
        ;; Move the completion session to the minibuffer.
        ("M-m" . my/corfu-move-to-minibuffer))

  :hook
  (minibuffer-setup . my/corfu-enable-in-minibuffer)

  :custom
  ;; Show the Corfu pop-up without requiring tab to be pressed (but after the
  ;; delay configured below).
  (corfu-auto t)
  ;; Number of typed characters before Corfu will display its pop-up.
  (corfu-auto-prefix 1)
  ;; Number of seconds of inactivity before the Corfu pop-up is displayed. This
  ;; setting only applies after the minimum number of prefix characters have
  ;; been entered. This is really useful to keep so that short words that you
  ;; don't want autocompleted don't trigger the Corfu pop-up (and subsequent
  ;; completion which inserts a space after the completed word).
  (corfu-auto-delay 0.3)
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
        (apply #'consult-completion-in-region completion-in-region--data))))

  :config
  ;; Documentation shown alongside Corfu completion popups.
  (use-package corfu-popupinfo
    :after corfu
    :straight
    (:host github :repo "minad/corfu" :files ("extensions/corfu-popupinfo.el"))
    :bind
    (:map corfu-map
          ("M-d" . corfu-popupinfo-toggle)
          ("M-p" . corfu-popupinfo-scroll-down)
          ("M-n" . corfu-popupinfo-scroll-up))
    :hook
    (corfu-mode . corfu-popupinfo-mode)
    :custom
    (corfu-popupinfo-delay 1.0))

  ;; Corfu icons.
  (use-package kind-icon
    :after corfu
    :commands kind-icon-margin-formatter
    :custom
    (kind-icon-default-face 'corfu-default)
    :init
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)))

;; Vertico provides the vertical completion minibuffer and Orderless provides
;; the "completion style". Some commands that make use of Vertico's selection
;; list also allow a new non-matched value to be entered. For example,
;; `org-roam-node-insert' will create a new note when given a new note name.
;; However, if the new value matches part of an existing value in the selection
;; list (which is more likely when using Orderless) then you will need to press
;; M-RET which calls `vertico-exit-input' to cancel the completion and use the
;; new value.
(use-package vertico
  :commands vertico-mode
  :init
  (vertico-mode 1))

;; Dedicated completion commands.
(use-package cape
  :commands cape-super-capf
  :functions
  (pcomplete-completions-at-point lsp-completion-at-point)
  :bind
  (:map global-map
        ("C-r"     . my/cape-history)
        ("C-c p p" . completion-at-point)
        ("C-c p t" . complete-tag)
        ("C-c p d" . cape-dabbrev)
        ("C-c p h" . cape-history)
        ("C-c p f" . cape-file)
        ("C-c p k" . cape-keyword)
        ("C-c p s" . cape-symbol)
        ("C-c p a" . cape-abbrev)
        ("C-c p l" . cape-line)
        ("C-c p y" . cape-yasnippet)
        ("C-c p w" . cape-dict))
  :hook
  (emacs-lisp-mode . my/init-elisp-capfs)
  (eshell-mode . my/init-eshell-capfs)
  (minibuffer-mode . my/init-eshell-capfs)
  (org-mode . my/init-org-capfs)
  (lsp-completion-mode . my/init-lsp-capfs)

  :custom
  ;; Only show dabbrev candidates of a minimum length to avoid being annoying.
  (cape-dabbrev-min-length 5)
  ;; Only show dabbrev completions for words in the current buffer.
  (cape-dabbrev-check-other-buffers nil)

  :init
  (use-package cape-yasnippet
    :after yasnippet
    :straight
    (:host github :repo "elken/cape-yasnippet")
    :custom
    (cape-yasnippet-lookup-by 'key))

  (defun my/cape-history ()
    "Version of `cape-history' that runs as an in-buffer completion."
    (interactive)
    (let ((completion-at-point-functions (list #'cape-history)))
      (completion-at-point)))

  ;; These separate completion setup functions are a work-in-progress.
  ;; See: https://github.com/minad/corfu/wiki#using-cape-to-tweak-and-combine-capfs.
  (defun my/init-elisp-capfs ()
    "Configure CAPFs to be used for `emacs-lisp-mode'."
    (setq-local completion-at-point-functions
                (list #'elisp-completion-at-point #'cape-file)))

  (defun my/init-eshell-capfs ()
    "Configure CAPFs to be used for `eshell-mode'."
    (setq-local completion-at-point-functions (list
                                               #'pcomplete-completions-at-point
                                               #'cape-file
                                               #'cape-dabbrev)))

  (defun my/init-org-capfs ()
    "Configure CAPFs to be used for `org-mode'."
    (setq-local completion-at-point-functions
                (list (cape-super-capf #'cape-dabbrev #'cape-yasnippet))))

  (defun my/init-lsp-capfs ()
    "Configure CAPFs to be used for `lsp-mode'."
    ;; See: https://github.com/minad/corfu/wiki#basic-example-configuration-with-orderless.
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))
    (setq-local completion-at-point-functions
                (list
                 (cape-super-capf #'lsp-completion-at-point #'cape-yasnippet)))))

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
  :bind
  (:map minibuffer-local-map
        ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode 1))

(use-package consult
  :commands
  (consult--buffer-state
   consult--buffer-action
   consult--buffer-query)

  :bind
  (:map global-map
        ("C-s" . consult-line)
        ("C-x b" . consult-buffer)
        ("M-g M-g" . consult-goto-line)
        ("M-y" . consult-yank-pop)
        ("C-x r r" . consult-register)
        ("C-x r l" . consult-register-load)
        ("C-x r s" . consult-register-store)
        ("C-c o s" . consult-org-agenda)

        ;; Put all search/goto commands under M-s for simplicity.
        ("M-s d" . consult-dir)
        ("M-s f" . consult-find)
        ("M-s l" . consult-line)
        ("M-s L" . my/consult-line-strict)
        ("M-s r" . consult-ripgrep)
        ("M-s i" . consult-imenu)
        ("M-s I" . consult-imenu-multi)
        ("M-s m" . consult-mark)
        ("M-s M" . consult-global-mark)
        ("M-s g" . consult-goto-line)
        ("M-s o" . consult-outline))
  (:map minibuffer-local-map
        ("M-s" . nil)
        ("C-r" . consult-history))
  (:map consult-narrow-map
        ("C-<" . consult-narrow-help))

  :custom
  ;; Type < followed by a prefix key to narrow the available candidates.
  ;; Type C-< (defined above) to display prefix help. Alternatively, type
  ;; < followed by C-h (or ?) to make `embark-prefix-help-command' kick in
  ;; and display a completing prefix help.
  (consult-narrow-key "<")

  ;; By default, only show previews when M-. is pressed.
  (consult-preview-key "M-.")

  ;; Customise the list of sources shown by consult-buffer.
  (consult-buffer-sources
   '(consult--source-buffer          ;; Narrow: ?b
     consult--source-project-buffer  ;; Narrow: ?p
     my/consult-source-eshell-buffer ;; Narrow: ?e
     consult--source-recent-file     ;; Narrow: ?r
     consult--source-bookmark))      ;; Narrow: ?m

  ;; Tell `consult-ripgrep' to search hidden dirs/files but ignore .git/.
  (consult-ripgrep-args
   '("rg"
     "--null"
     "--hidden"
     "--glob=!.git/"
     "--line-buffered"
     "--color=never"
     "--max-columns=1000"
     "--path-separator=/"
     "--smart-case"
     "--no-heading"
     "--line-number"
     "."))

  ;; Tell `consult-find' to search hidden dirs/files but ignore .git/.
  (consult-find-args "find . -not ( -name .git -type d -prune )")

  ;; Customize `consult-imenu' groupings for languages I regularly use. The
  ;; imenu backend for each language will return the symbol category strings
  ;; used below. For Rust and Go, the imenu backend is implemented by their
  ;; respective LSP servers. The font faces are chosen for aesthetics only.
  ;; If a symbol category is missing from the alists below the symbols for
  ;; that category will be displayed in an ungrouped way and the line will be
  ;; prefixed with the category name - it can then; be added below. See the
  ;; `lsp-imenu-symbol-kinds' variable and also:
  ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#symbolKind.
  (consult-imenu-config
   '((emacs-lisp-mode
      ;; This configuration is copied directly from consult-imenu.el as I'd
      ;; prefer to set `consult-imenu-config' idempotently within :custom.
      :toplevel "Functions"
      :types ((?f "Functions" font-lock-function-name-face)
              (?m "Macros" font-lock-function-name-face)
              (?p "Packages" font-lock-constant-face)
              (?t "Types" font-lock-type-face)
              (?v "Variables" font-lock-variable-name-face)))
     (rustic-mode
      :types ((?f "Functions" font-lock-function-name-face)
              (?F "Fields" font-lock-variable-name-face)
              (?s "Structs" font-lock-type-face)
              (?i "Interfaces" font-lock-type-face)
              (?c "Constants" font-lock-constant-face)
              (?m "Modules" font-lock-type-face)
              (?o "Objects" font-lock-type-face)
              (?e "Enums" font-lock-constant-face)
              (?E "Enum Members" font-lock-variable-name-face)
              (?t "Type Parameters" font-lock-type-face)))
     (go-mode
      :types ((?f "Functions" font-lock-function-name-face)
              (?m "Methods" font-lock-function-name-face)
              (?F "Fields" font-lock-variable-name-face)
              (?s "Structs" font-lock-type-face)
              (?i "Interfaces" font-lock-type-face)
              (?v "Variables" font-lock-variable-name-face)
              (?c "Constants" font-lock-constant-face)))
     (protobuf-mode
      ;; The imenu symbols in `protobuf-mode' aren't pluralized.
      :types ((?s "Service" font-lock-function-name-face)
              (?m "Message" font-lock-variable-name-face)
              (?e "Enum" font-lock-constant-face)))))

  :config
  (defun my/consult-line-strict (&optional initial start)
    "Version of `consult-line' that uses a strict substring completion style."
    (interactive (list nil (not (not current-prefix-arg))))
    (let ((completion-styles '(substring)))
      (consult-line initial start)))

  (defvar my/consult-source-eshell-buffer
    `(:name "Eshell Buffer"
            :narrow ?e
            :category eshell-buffer
            :face consult-buffer
            :history buffer-name-history
            ;; https://github.com/jwiegley/use-package/issues/795#issuecomment-673077162
            :state ,#'consult--buffer-state
            :action ,#'consult--buffer-action
            :items ,(lambda ()
                      (consult--buffer-query
                       :sort 'visibility
                       :as #'buffer-name
                       :mode 'eshell-mode))))


  (consult-customize
   ;; Source name and narrow key customization.
   consult--source-buffer :name "Open Buffer" :narrow ?b
   consult--source-project-buffer :name "Project Buffer" :narrow ?p
   consult--source-recent-file :name "Recent File" :narrow ?r

   ;; Show preview immediately for the following commands.
   consult-goto-line
   consult-imenu
   consult-imenu-multi
   consult-line
   :preview-key 'any))

(use-package consult-dir
  :commands consult-dir
  :custom
  (consult-dir-shadow-filenames nil)
  (consult-dir-default-command #'consult-dir-dired))

(use-package consult-lsp
  :custom
  (consult-lsp-marginalia-mode t))

(use-package consult-yasnippet)

(use-package embark
  :commands embark-prefix-help-command

  :bind
  (:map global-map
        ("C-." . embark-act)
        ("M-." . embark-dwim)
        ("C-h b" . embark-bindings))
  (:map minibuffer-local-map
        ("C-S-b" . embark-become))

  :custom
  ;; The following two settings tell Embark to just use the minibuffer and
  ;; completing-read for displaying the Embark popup (rather than a window).
  (embark-prompter #'embark-completing-read-prompter)
  (embark-indicators (list #'embark-minimal-indicator))
  ;; Use this key to switch Embark to the keymap prompter.
  (embark-keymap-prompter-key ",")

  :init
  ;; Use Embark to prompt for and run commands under a specified prefix
  ;; when C-h is pressed (e.g. C-x C-h) rather than `describe-prefix-bindings'.
  ;; Needs to be set in :init rather than :custom otherwise it gets overridden.
  (require 'help)
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Org-roam nodes have their own Embark category and hence need their own
  ;; keymap so that I can act on them. Here, I create a new Embark keymap and
  ;; add it to `embark-keymap-alist'.
  (defvar-keymap my/embark-org-roam-node-map
    :doc "Keymap for Embark `org-roam-node' actions"
    :parent embark-general-map)

  (defvar-keymap my/embark-consult-xref-map
    :doc "Keymap for Embark `consult-xref' actions"
    :parent embark-general-map)

  (add-to-list 'embark-keymap-alist '(org-roam-node . my/embark-org-roam-node-map))
  (add-to-list 'embark-keymap-alist '(consult-xref . my/embark-consult-xref-map))
  (add-to-list 'embark-keymap-alist '(xref . my/embark-consult-xref-map))

  ;; This function is used as the argument to `my/embark-ace-window-action' for
  ;; opening a `consult-xref' candidate in an `ace-window' selected window.
  ;; TODO: There must be a more idiomatic way to do this (may need to lean
  ;; on `consult-xref' private functions) rather than string splitting. This
  ;; way is flawed as it only navigates to the line rather than where the
  ;; symbol is on the line.
  (defun my/goto-consult-xref ()
    "Embark action function for opening a `consult-xref' candidate."
    (interactive)
    (let ((location (read-from-minibuffer "")))
      (let* ((parts (string-split location ":"))
             (file (nth 0 parts))
             (line (nth 1 parts)))
        (find-file file)
        (goto-char (point-min))
        (forward-line (1- (string-to-number line))))))

  ;; Macro for defining an Embark action that executes FN using an `ace-window'
  ;; selected window. Taken from:
  ;; https://github.com/karthink/.emacs.d/blob/f0514340039502b79a306a77624a604de8a1b546/lisp/setup-embark.el#L93.
  (eval-when-compile
    (defmacro my/embark-ace-window-action (fn)
      `(defun ,(intern (concat
                        "my/ace-window-"
                        (string-remove-prefix "my/" (symbol-name fn)))) ()
         "Execute Embark action after jumping with `ace-window'."
         (interactive)
         (with-demoted-errors "%s"
           (require 'ace-window)
           (let ((aw-dispatch-always t))
             (aw-switch-to-window (aw-select nil))
             (call-interactively (symbol-function ',fn)))))))

  ;; Create ace-window actions against relevant keymaps.
  (define-key embark-file-map (kbd "o") (my/embark-ace-window-action find-file))
  (define-key embark-buffer-map (kbd "o") (my/embark-ace-window-action switch-to-buffer))
  (define-key embark-bookmark-map (kbd "o") (my/embark-ace-window-action bookmark-jump))
  (define-key my/embark-org-roam-node-map (kbd "o") (my/embark-ace-window-action org-roam-node-find))
  (define-key my/embark-consult-xref-map (kbd "o") (my/embark-ace-window-action my/goto-consult-xref)))

(use-package embark-consult
  :hook
  ;; TODO: Understand how this actually works.
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package rg
  :commands rg-enable-default-bindings
  :custom
  (rg-keymap-prefix (kbd "C-c r"))
  :init
  ;; TODO: Need a way to hide popper when the side window is shown.
  ;; See also: https://github.com/seagle0128/.emacs.d/blob/00f25c86d2efc9067364e2173d102a4bf1b460ad/lisp/init-utils.el#L163.
  (rg-enable-default-bindings))

(use-package wgrep
  :bind
  (:map global-map
        ("C-c C-w" . wgrep-change-to-wgrep-mode))
  :custom
  (wgrep-auto-save-buffer t))

;;;; General Editing

;;;;; Undo/Redo

(use-package undo-tree
  :commands global-undo-tree-mode
  :custom
  ;; Disable history saving for now.
  (undo-tree-auto-save-history nil)
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-diff t)
  :init
  (global-undo-tree-mode 1))

;;;;; Region Expansion

(use-package expand-region
  :bind
  (:map global-map
        ("C-=" . er/expand-region)
        ("C-+" . er/mark-outside-pairs))
  :custom
  (expand-region-fast-keys-enabled nil)
  (expand-region-autocopy-register "e"))

;;;;; Whitespace

(use-package ws-butler
  :hook
  (text-mode . ws-butler-mode)
  (prog-mode . ws-butler-mode))


;;;;; Point Jumping

;; A lot of my Avy inspiration came from the following:
;; https://karthinks.com/software/avy-can-do-anything and
;; https://github.com/karthink/.emacs.d/blob/master/lisp/setup-avy.el.
(use-package avy
  :functions ring-ref
  :bind
  (:map global-map
        ("M-j" . nil)
        ("M-j j" . avy-goto-char-timer)
        ("M-j l" . avy-goto-line)
        ("M-j M-l" . avy-goto-end-of-line)
        ("M-j y" . avy-copy-line)
        ("M-j M-y" . avy-copy-region)
        ("M-j k" . avy-kill-whole-line)
        ("M-j M-k" . avy-kill-region)
        ("M-j w" . avy-kill-ring-save-whole-line)
        ("M-j M-w" . avy-kill-ring-save-region)
        ("M-j m" . avy-move-line)
        ("M-j M-m" . avy-move-region))

  :init
  (defun my/avy-action-embark (pt)
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
     (?. . my/avy-action-embark))))

(use-package isearch
  :straight nil
  :bind
  ;; C-s is already taken by `consult-line' so create different bindings for
  ;; isearch. `isearch-forward-regexp' and `isearch-backward-regexp' are more
  ;; generally useful
  (:map global-map
        ("C-S-s" . isearch-forward)
        ("C-S-r" . isearch-backward))
  (:map isearch-mode-map
        ("C-S-s" . isearch-repeat-forward)
        ("C-S-r" . isearch-repeat-backward)))

;;;; Buffer Management

(use-package ibuffer
  :straight nil
  :bind
  (:map global-map
        ;; Replace `list-buffers' with `ibuffer'.
        ("C-x C-b" . ibuffer))
  (:map ibuffer-mode-map
        ;; Keep M-o binding for ace-window.
        ("M-o" . nil)
        ;; Group buffers by VC project.
        ("/ p" . ibuffer-vc-set-filter-groups-by-vc-root))
  :init
  (use-package ibuffer-vc))

;;;; File System

;;;;; File Browsing

(use-package dired
  :straight nil
  :bind
  (:map dired-mode-map
        ("C-o" . nil)
        ("N" . dired-create-empty-file)
        ("?" . which-key-show-major-mode)
        ("i" . dired-subtree-insert)
        (";" . dired-subtree-remove))
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
  :bind
  (:map global-map
        ("M-t" . treemacs)
        ("M-T" . my/treemacs-stay)
        ("M-0" . treemacs-select-window))
  (:map treemacs-mode-map
        ;; Otherwise it takes two clicks to open a directory.
        ([mouse-1] . treemacs-single-click-expand-action))

  :hook
  ;; Don't wrap long lines in Treemacs.
  (treemacs-mode . (lambda () (setq-local truncate-lines t)))

  :custom
  (treemacs-follow-mode t)
  (treemacs-project-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-width 40)
  (treemacs-missing-project-action 'remove)
  (treemacs-follow-after-init t)

  :init
  (defun my/treemacs-stay ()
    "Open the Treemacs viewer but don't shift focus to it."
    (interactive)
    (treemacs)
    (treemacs-select-window))

  :config
  ;; Override `treemacs--propagate-new-icons' so that janky image icons from
  ;; the default treemacs package don't get mixed in with the nerd icons.
  ;; See: https://github.com/Alexander-Miller/treemacs/issues/1018#issuecomment-1599599392.
  (defun treemacs--propagate-new-icons (_theme))

  ;; Style the treemacs viewer with nerd icons.
  (use-package treemacs-nerd-icons
    :functions treemacs-load-theme
    :custom-face
    (treemacs-nerd-icons-root-face ((t (:inherit nerd-icons-green :height 1.3))))
    (treemacs-nerd-icons-file-face ((t (:inherit nerd-icons-dsilver))))
    :config
    (treemacs-load-theme "nerd-icons"))

  ;; By default, treemacs-mode will add itself to `aw-ignored-buffers' which
  ;; prevents jumping to its window using ace-window. Personally, I prefer
  ;; being able to treat it just like any other window.
  (require 'ace-window)
  (setq aw-ignored-buffers (delq 'treemacs-mode aw-ignored-buffers)))

;;;;; File History

(use-package recentf
  :straight nil
  :custom
  (recentf-max-saved-items 300)
  :config
  (recentf-mode 1))

;;;;; Project Management

(use-package project
  :straight nil
  :bind
  (:map project-prefix-map
        ("F" . my/project-find-file-relative)
        ("u" . my/project-refresh-list))

  :custom
  (project-switch-commands
   '((project-find-file "Find file" ?f)
     (project-find-dir "Find directory" ?d)
     (project-dired "Dired" ?D)
     (consult-ripgrep "Ripgrep" ?r)
     (magit-project-status "Magit" ?m)
     (project-eshell "Eshell" ?e)
     (project-shell-command "Shell" ?!)
     (project-async-shell-command "Async shell" ?&)))

  :config
  (defun my/project-current-root ()
    "Return the root directory of the current or nil."
    (if-let* ((proj (project-current)))
        (project-root proj)))

  (defun my/project-find-file-relative ()
    "Complete a file name relative to the current buffer and project."
    (interactive)
    (if-let* ((proj (project-current))
              (dir default-directory))
        (project-find-file-in (buffer-file-name) (list dir) proj nil)
      (project-find-file)))

  (defun my/project-refresh-list ()
    "Refresh list of known projects."
    (interactive)
    (project-forget-zombie-projects)
    (my/project-index-under "~/dev/home")
    (my/project-index-under "~/dev/work"))

  (defun my/project-index-under (dir)
    "Index all projects below directory DIR.
This is an adaptation of a previous version of `project-remember-projects-under'
as there appears to be a bug in the current version."
    (interactive "DDirectory: \nP")
    (project--ensure-read-project-list)
    (let ((queue (directory-files dir t nil t)) (count 0)
          (known (make-hash-table
                  :size (* 2 (length project--list))
                  :test #'equal )))
      (dolist (project (mapcar #'car project--list))
        (puthash project t known))
      (while queue
        (when-let ((subdir (pop queue))
                   ((file-directory-p subdir))
                   ((not (gethash subdir known))))
          (when-let (pr (project--find-in-directory subdir))
            (project-remember-project pr t)
            (message "Found %s..." (project-root pr))
            (setq count (1+ count)))))
      (if (zerop count)
          (message "No projects were found")
        (project--write-project-list)
        (message "%d project%s were found"
                 count (if (= count 1) "" "s"))))))

;;;;; Auto-Save

(use-package super-save
  :commands super-save-mode
  :custom
  ;; Disable built-in `auto-save-mode' as this replaces it.
  (auto-save-default nil)
  (super-save-auto-save-when-idle t)
  ;; Large idle duration to avoid programming modes (e.g., Rustic) that perform
  ;; actions on save from running to eagerly.
  (super-save-idle-duration 40)
  (super-save-max-buffer-size 100000)
  :init
  (super-save-mode 1))

;;;; Programming

;;;;; Outline

(use-package outline
  :straight nil
  :hook
  (emacs-lisp-mode . (lambda ()
                       (setq-local outline-regexp ";;;+ [^\n]")
                       (outline-minor-mode)))
  :bind
  (:map outline-minor-mode-map
        ("C-c C-n" . outline-next-visible-heading)
        ("C-c C-p" . outline-previous-visible-heading)
        ("C-c C-f" . outline-forward-same-level)
        ("C-c C-b" . outline-backward-same-level)
        ("C-c C-a" . outline-show-all)
        ("C-c C-h" . outline-hide-other)
        ("C-c C-u" . outline-up-heading))
  ;; TODO: Find better keybindings as these are taken by Paredit.
  ;; ("M-<down>" . outline-move-subtree-down)
  ;; ("M-<up>" . outline-move-subtree-up))
  :custom
  (outline-minor-mode-cycle t))

;;;;; Code Templating

(use-package yasnippet
  :after consult-yasnippet
  :hook ((text-mode prog-mode eshell-mode) . yas-minor-mode)
  :bind
  (:map global-map
  	("C-c y n" . yas-new-snippet)
        ("C-c y u" . yas-reload-all)
        ("C-c y d" . yas-describe-tables))
  (:map yas-minor-mode-map
        ("C-c &" . nil)
        ("C-c y y" . yas-expand)
        ("C-c y i" . consult-yasnippet)
        ("C-c y f" . yas-visit-snippet-file))

  :custom
  (yas-verbosity 1)
  (yas-wrap-around-region t))

(use-package yasnippet-snippets)

;;;;; Code Formatting and Linting

(use-package apheleia
  :commands apheleia-global-mode
  :init
  (apheleia-global-mode 1)
  :config
  ;; Use goimports rather than the gofmt so that imports get optimized.
  (setf (alist-get 'go-mode apheleia-mode-alist) 'goimports))

;;;;;; LSP (Language Server Protocol)

(use-package lsp-mode
  ;; Shouldn't be necessary but gets ride of Flycheck warnings.
  :functions my/if-essential-advice
  :commands
  (lsp
   lsp-deferred
   lsp-register-custom-settings)
  :hook
  ((c-mode
    c++-mode
    go-mode
    rustic-mode
    sh-mode
    terraform-mode
    python-mode) . lsp-deferred)
  :bind-keymap ("C-c l" . lsp-command-map)
  :bind
  (:map lsp-mode-map
        ("M-P" . lsp-describe-thing-at-point)
        ("C-c C-c s f" . consult-lsp-file-symbols)
        ("C-c C-c s w" . consult-lsp-symbols)
        ("C-c C-c h" . lsp-inlay-hints-mode))

  :custom
  (lsp-log-io nil)

  ;; No need to also show the count.
  (lsp-modeline-code-actions-segments '(icon))

  ;; Haven't found a great use for these.
  (lsp-lens-enable nil)

  ;; The Flycheck modeline segment already displays this.
  (lsp-modeline-diagnostics-enable nil)

  ;; Display type hints by default (not supported by all language servers).
  (lsp-inlay-hint-enable t)

  ;; Recommended setting as I'm using Corfu instead of Company for completion.
  ;; See: https://github.com/minad/corfu/issues/71#issuecomment-977693717
  (lsp-completion-provider :none)

  ;; Categorise `lsp-ui-imenu' entries by their type (variable, function, etc).
  (lsp-imenu-index-function #'lsp-imenu-create-categorized-index)

  ;; Close the language server buffer when the last file in the workspace/
  ;; project is closed.
  (lsp-keep-workspace-alive nil)

  ;; When `lsp-eldoc-render-all' is set to nil, moving point to a function call
  ;; should result in a one line function signature being displayed in the
  ;; minibuffer. There is an issue with lsp-mode and rust-analyzer, however,
  ;; where what gets displayed is not the function signature but (usually) the
  ;; type of struct to which the function belongs. However, setting
  ;; `lsp-eldoc-render-all' to non-nil is even worse as it often results in a
  ;; huge block of text being displayed that contains too much information.
  ;; I'm going to disable for now, and use lsp-ui mouse over or
  ;; `lsp-ui-doc-glance' to view type information.
  ;; See: https://github.com/emacs-lsp/lsp-mode/issues/2613
  ;; Also: https://github.com/rust-analyzer/rust-analyzer/issues/3415
  (lsp-eldoc-render-all nil)
  (lsp-eldoc-enable-hover nil)

  ;; Disable the displaying of signature help in the minibuffer as it makes the
  ;; size of the minibuffer expand to show all the additional info which is
  ;; distracting. Signature help can be obtained from lsp-ui via mouse hover or
  ;; calling `lsp-ui-doc-glance' explicitly.
  (lsp-signature-auto-activate nil)
  (lsp-signature-render-documentation nil)

  ;; The following --query-driver args allow clangd to be used with the
  ;; compile_commands.json generated by bazel-compilation-database. This is
  ;; due to the fact that, by default, clangd expects the driver to be a
  ;; standard compiler executable (e.g. clang++). The log level can be changed
  ;; to "debug" for additional information.
  (lsp-clients-clangd-args '("--query-driver=**/wrapped_clang"
			     "--background-index"
			     "--log=info"))

  ;; Use "clippy" rather than "check" for additional lints.
  (lsp-rust-analyzer-cargo-watch-command "clippy")

  ;; Configure lsp-mode to use the official terraform-ls LSP server rather than
  ;; terraform-lsp which it uses by default and is more experimental.
  (lsp-terraform-server '("terraform-ls" "serve"))

  :init
  (defun my/if-essential-advice (f &rest args)
    "Around advice that invokes F with ARGS if `non-essential' is non-nil."
    (unless non-essential
      (apply f args)))

  ;; Advise `lsp-deferred' and `lsp' so that they only run if non-essential is
  ;; non-nil. This prevents lsp-mode from starting during Consult previews.
  (advice-add 'lsp :around #'my/if-essential-advice)
  (advice-add 'lsp-deferred :around #'my/if-essential-advice)

  :config
  ;; Configure custom LSP server settings.
  ;;
  ;; For gpls:
  ;; See: https://github.com/golang/tools/blob/9b5e55b1a7e215a54c9784492d801104a8381a91/gopls/doc/emacs.md#configuring-gopls-via-lsp-mode
  ;; And available settings: https://github.com/golang/tools/blob/9b5e55b1a7e215a54c9784492d801104a8381a91/gopls/doc/settings.md
  ;; Not all experimental settings appear to be documented. For the full list,
  ;; see: https://github.com/golang/tools/blob/9b5e55b1a7e215a54c9784492d801104a8381a91/gopls/internal/lsp/source/options.go
  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ("gopls.staticcheck" t t))))

(use-package lsp-ui
  :bind
  (:map lsp-ui-mode-map
        ("M-p" . lsp-ui-doc-toggle)
        ("C-c C-c i" . lsp-ui-imenu))
  :custom
  (lsp-ui-sideline-delay 0)
  (lsp-ui-doc-delay 0)
  (lsp-ui-imenu-auto-refresh t)
  (lsp-ui-doc-show-with-mouse nil)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-max-width 150)
  (lsp-ui-doc-max-height 30))

;;;;;; DAP (Debug Adapter Protocol)

;; The following DAP configuration is optimised for debugging Rust and Go.
;; The same keybindings and UI is used across all languages. The Go debugging
;; workflow follows the recommendations for Go here:
;; https://emacs-lsp.github.io/dap-mode/page/configuration.
;;
;; The Rust debugging workflow does not follow the recommendations for Rust in
;; the docs linked above as they recommend using rust-gdb which cannot be used
;; on Apple silicon as gdb doesn't support it yet.
;;
;; To work around this and other issues, I've defined my own custom debug
;; configuration (a DAP provider and a template) for debugging Rust. When
;; `dap-debug' is run, a configuration named "Rust LLDB Default" will be shown.
;; When selected, this configuration will prompt for the executable file to be
;; debugged. Unlike with Go/Delve, the debuggable executable must be recompiled
;; between code changes. This default configuration does not supply arguments
;; or environment variables.
;;
;; For more advanced Rust debugging scenarios (e.g. args, vars, multiple
;; binaries, etc), drop a launch.json file at the root of the Rust project
;; with the following format:
;;
;; {
;;   "version": "0.2.0",
;;   "configurations": [{
;;     "name": "Rust LLDB Custom",
;;     "type": "rust-lldb",
;;     "request": "launch",
;;     "program": "${workspaceFolder}/target/debug/axum-full-example",
;;     "args": [],
;;     "env": {},
;;     "cwd": "${workspaceFolder}",
;;     "stopOnEntry": false,
;;     "debuggerRoot": "${workspaceFolder}"
;;   }]
;; }
;;
;; In the example above, replace "axum-full-example" with the name of the
;; executable. If there are multiple executables, multiple configuration
;; blocks will be needed and each should have a unique name (e.g.
;; "Rust LLDB Custom (axum-full-example)", etc).
;;
;; When a launch.json file is present, `dap-debug' will show the configurations
;; that it defines in addition to the ones that are registered below. The
;; launch.json configurations will be mixed in with the properties set by the
;; `my/dap-rust-lldb-debug-provider' provider below as long as a type of
;; "rust-lldb" is specified.
;;
;; Note: dap-mode uses `lsp-workspace-root' to determine the root project
;; directory that should contain the launch.json (or .vscode/launch.json) file.
;; In projects with submodules where you want to debug a submodule
;; independently, this can cause problems if `lsp-workspace-root' says that the
;; project root is higher up the directory tree than you want it to be. If you
;; get stuck in this cycle you will probably want to delete or edit
;; ~/.config/emacs/.lsp-session-v1 or find a way to hook into dap-mode/lsp-mode
;; so the process is a bit smarter.
(use-package dap-mode
  :functions (my/project-current-root my/dap-rust-lldb-debug-provider)
  :commands
  (dap-mode
   dap-ui-mode
   dap-ui-controls-mode
   dap-register-debug-provider
   dap-register-debug-template)

  :hook
  (go-mode . my/dap-mode)
  (rustic-mode . my/dap-mode)

  :bind
  (:map dap-mode-map
        ("C-c C-d d" . dap-debug)
        ("C-c C-d l" . dap-debug-last)
        ("C-c C-d m" . dap-hydra)
        ("C-c C-d b" . dap-breakpoint-toggle)
        ("C-c C-d c" . dap-breakpoint-condition)
        ("C-c C-d k" . dap-breakpoint-delete-all)
        ("C-c C-d q" . dap-disconnect)

        ;; DAP UI (windows are managed by popper).
        ("C-c C-d w a" . dap-ui-expressions-add)
        ("C-c C-d w e" . dap-ui-expressions)
        ("C-c C-d w l" . dap-ui-breakpoints-list)
        ("C-c C-d w v" . dap-ui-locals)
        ("C-c C-d w r" . dap-ui-repl)
        ("C-c C-d w s" . dap-ui-sessions)

        ;; Keys to be used during a debug session.
        ("C-c C-d C-n" . dap-next)
        ("C-c C-d C-i" . dap-step-in)
        ("C-c C-d C-o" . dap-step-out)
        ("C-c C-d C-c" . dap-continue))

  :custom
  (dap-auto-configure-features nil)
  (dap-print-io nil)
  (dap-auto-show-output nil)
  (dap-ui-repl-history-dir no-littering-var-directory)

  :config
  (require 'dap-dlv-go)
  (require 'dap-ui)

  ;; Override `dap-ui--show-buffer' to show buffers in a standard window rather
  ;; than a side window allowing popper to control them.
  (defun dap-ui--show-buffer (buf)
    "Overidden version of `dap-ui--show-buffer' that doesn't use side windows."
    (display-buffer buf))

  (defun my/dap-mode ()
    "Start `dap-mode' in an opinionated way."
    (interactive)
    (dap-mode 1)
    (dap-ui-mode 1)
    (dap-ui-controls-mode -1))

  ;; Location of lldb-vscode on the host.
  (defvar my/dap-rust-lldb-debug-program
    '("/opt/homebrew/opt/llvm/bin/lldb-vscode"))

  ;; Custom DAP debug provider for Rust using LLDB. This allows the Rust binary
  ;; that is to be debugged to be selected at the start of the debug session.
  (defun my/dap-rust-lldb-debug-provider (conf)
    "Populate CONF with the required arguments."
    (-> conf
        (dap--put-if-absent :dap-server-path my/dap-rust-lldb-debug-program)
        (dap--put-if-absent :cwd (expand-file-name (my/project-current-root)))
        (dap--put-if-absent :program
                            (read-file-name
                             "Select binary file to debug: "
                             (my/project-current-root)
                             nil t nil #'file-executable-p))))

  ;; Register the custom DAP debug provider for Rust using LLDB.
  (dap-register-debug-provider "rust-lldb" #'my/dap-rust-lldb-debug-provider)

  ;; Register the custom DAP template for the custom provider.
  (dap-register-debug-template "Rust LLDB Default"
                               (list :type "rust-lldb"
                                     :request "launch"
                                     :name "rust-lldb-template"
                                     :mode "auto"
                                     :buildFlags nil
                                     :args nil
                                     :env nil)))

;;;;;; Xref

(use-package xref
  :straight nil
  :custom
  ;; Don't prompt by default (invoke with prefix arg to prompt).
  (xref-prompt-for-identifier nil)
  ;; Use consult to select xref locations with preview.
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref))

;;;;;; Eldoc

(use-package eldoc
  :straight nil
  :hook (emacs-lisp-mode . eldoc-mode)
  :custom
  (eldoc-idle-delay 0)
  :init
  ;; Print Eldoc documentation in echo area when Paredit commands are run.
  (eldoc-add-command-completions "paredit-"))

;;;;;; Flycheck

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :bind
  (:map flycheck-mode-map
        ("C-c C-c l" . flycheck-list-errors))
  :custom
  ;; Tell Flycheck to use the load-path of the current Emacs session. Without
  ;; this, Flycheck tends towards both false negatives and false positives.
  (flycheck-emacs-lisp-load-path 'inherit)
  ;; For performance, only perform checking when a file is saved or opened.
  (flycheck-check-syntax-automatically '(save mode-enabled)))

;;;;; Programming Languages

;;;;;; Rust

(use-package rustic
  :functions my/build-rust-keymap
  :commands
  (rustic-cargo-add
   rustic-cargo-build
   rustic-cargo-clean
   rustic-format-buffer
   rustic-cargo-fmt
   rustic-cargo-clippy
   rustic-cargo-outdated
   rustic-cargo-run
   rustic-cargo-test
   rustic-cargo-current-test
   rustic-cargo-upgrade
   lsp-rust-analyzer-open-external-docs)
  :config
  ;; The following function is used to ensure Rust keybindings are placed in
  ;; both `rustic-mode-map' and `rustic-compilation-mode-map' (so they can also
  ;; be used from within the compilation popup window). I'm sure there is a
  ;; more idiomatic way to do this but I haven't found it yet.
  (defun my/build-rust-keymap (base-map)
    "Return custom Rust keymap built on top of BASE-MAP."
    (define-key base-map (kbd "C-c C-c") nil)
    (define-key base-map (kbd "C-c C-c b") #'rustic-cargo-build)
    (define-key base-map (kbd "C-c C-c c a") #'rustic-cargo-add)
    (define-key base-map (kbd "C-c C-c c b") #'rustic-cargo-build)
    (define-key base-map (kbd "C-c C-c c c") #'rustic-cargo-clean)
    (define-key base-map (kbd "C-c C-c c f") #'rustic-cargo-fmt)
    (define-key base-map (kbd "C-c C-c c l") #'rustic-cargo-clippy)
    (define-key base-map (kbd "C-c C-c c o") #'rustic-cargo-outdated)
    (define-key base-map (kbd "C-c C-c c r") #'rustic-cargo-run)
    (define-key base-map (kbd "C-c C-c c t") #'rustic-cargo-test)
    (define-key base-map (kbd "C-c C-c c T") #'rustic-cargo-current-test)
    (define-key base-map (kbd "C-c C-c c u") #'rustic-cargo-upgrade)
    (define-key base-map (kbd "C-c C-c C-o") #'lsp-rust-analyzer-open-external-docs)
    base-map)

  (setq rustic-mode-map (my/build-rust-keymap (make-sparse-keymap)))
  (setq rustic-compilation-mode-map
        (my/build-rust-keymap rustic-compilation-mode-map)))

;;;;;; Terraform

(use-package terraform-mode)

;;;;;; Python

(use-package python
  :straight nil
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
  :straight nil
  :custom
  (sh-basic-offset 2))

;;;;;; Go

(use-package go-mode
  :bind
  (:map go-mode-map
        ("C-c C-c p b" . go-play-buffer)
        ("C-c C-c p r" . go-play-region)
        ("C-c C-c b" . compile)
        ("C-c C-c r" . recompile))
  :hook
  (go-mode . (lambda () (setq-local tab-width 4)))
  :init
  ;; Remove the default `go-mode' keybindings.
  (setq go-mode-map (make-sparse-keymap)))

;;;;;; Protobuf

(use-package protobuf-mode
  :mode "\\.proto\\'")

;;;;;; Bazel

(use-package bazel-mode
  :straight (bazel-mode :host github :repo "bazelbuild/emacs-bazel-mode")
  :mode
  ("\\.BUILD\\'" . bazel-mode)
  ("\\.bazel\\'" . bazel-mode)
  ("\\.star\\'" . bazel-starlark-mode)
  :bind
  (:map bazel-mode-map
        ("C-c C-c b" . bazel-build)
        ("C-c C-c f" . bazel-buildifier)
        ("C-c C-c r" . bazel-run)
        ("C-c C-c t" . bazel-test)
        ("C-c C-c T" . bazel-coverage))
  :custom
  (bazel-buildifier-before-save t)
  :init
  ;; Remove the default Bazel keybindings.
  (setq bazel-mode-map (make-sparse-keymap)))

;;;;;; Just (Task Runner)

(use-package just-mode)

;;;;;; C/C++

(use-package cc-mode
  :bind (:map c++-mode-map
	      ("C-c C-c f" . clang-format-buffer)
	      ("C-c C-c F" . clang-format-region)
	      ("C-c C-c c" . compile)))

(use-package clang-format
  :commands
  (clang-format clang-format-buffer clang-format-region)
  :config
  (setq clang-format-fallback-style "llvm")) ;; Where is this defined?

;;;;;; YAML

(use-package yaml-mode
  :mode (("\\.yml\\'"  . yaml-mode)
	 ("\\.yaml\\'" . yaml-mode)))

;;;;;; Jsonnet

(use-package jsonnet-mode)

;;;;;; Lisp

(use-package elisp-mode
  :straight nil
  :hook
  (emacs-lisp-mode . my/init-emacs-lisp-mode)
  :bind
  (:map global-map
        ("C-x C-r" . eval-region))
  :init
  (defun my/init-emacs-lisp-mode ()
    "Initializes `emacs-lisp-mode'."
    (setq-local fill-column 80)))

(use-package paredit
  :bind
  (:map paredit-mode-map
        ;; Unbind Paredit keybindings I don't use that can cause collisions.
        ("C-<left>" . nil)
        ("C-<right>" . nil)
        ("C-M-<left>" . nil)
        ("C-M-<right>" . nil)
        ("M-s" . nil)
        ("M-?" . nil))
  :hook
  ;; Note that I specifically don't enable Paredit in minibuffers as it causes
  ;; issues with RET keybindings.
  (emacs-lisp-mode . paredit-mode)       ;; Elisp buffers.
  (lisp-mode . paredit-mode)             ;; Common Lisp buffers.
  (lisp-interaction-mode . paredit-mode) ;; Scratch buffers.
  (ielm-mode-hook . paredit-mode))       ;; IELM buffers.

(use-package rainbow-delimiters
  :hook
  (emacs-lisp-mode . rainbow-delimiters-mode))

;;;;;; SGML/HTML

(use-package sgml-mode
  :straight nil
  :bind
  (:map html-mode-map
        ;; Unbind M-o as I want that for ace-window.
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
  (:map global-map
        ("C-c g s" . magit-status)
        ("C-c g d" . magit-dispatch)
        ("C-c g f" . magit-file-dispatch))
  :custom
  ;; Tell Magit not to add the C-x bindings as we'll use the ones above.
  (magit-define-global-key-bindings nil)
  ;; Otherwise Magit shows a read-only diff screen when you press C-c C-c and
  ;; you've already had a chance to look at the diff when you stage the files.
  (magit-commit-show-diff nil))

(use-package browse-at-remote
  :bind
  (:map global-map
        ("C-c g o" . browse-at-remote)
        ("C-c g k" . browse-at-remote-kill)))

;;;; Shell/Terminal

(use-package eshell
  :straight nil
  :hook
  (eshell-mode . my/init-eshell-mode)
  (eshell-pre-command . my/eshell-pre-command)
  (eshell-post-command . my/eshell-post-command)

  :bind
  (:map global-map
	("C-c e e" . eshell)
        ("C-c e n" . my/eshell-new)
        ("C-c e p" . project-eshell))
  (:map eshell-mode-map
        ("C-c C-o" . nil)) ;; Needed for `org-open-at-point-global'.

  :custom
  (eshell-history-size 10000)
  (eshell-buffer-maximum-lines 10000)
  (eshell-hist-ignoredups t)
  (eshell-prompt-function #'my/eshell-prompt)
  ;; The following commands will be started in `term-mode'.
  (eshell-visual-commands '("vi" "vim" "htop" "ktop" "watch" "gcloud"))

  :init
  ;; Needed so that `eshell-mode-map' is available above.
  (require 'esh-mode)

  (defun my/init-eshell-mode ()
    "Hook function executed when `eshell-mode' is run."
    ;; Don't wrap long lines in eshell.
    (setq-local truncate-lines t)
    ;; Don't scroll the buffer around after it has been recentered (using C-l).
    ;; This seems to need to be done as a mode hook rather than in `:config' as
    ;; the latter results in `eshell-output-filter-functions' being set to nil.
    ;; See: https://emacs.stackexchange.com/a/45281
    (remove-hook 'eshell-output-filter-functions
                 'eshell-postoutput-scroll-to-bottom)
    ;; Unbind M-s as that's used for consult commands. This needs to happen
    ;; here as `eshell-hist-mode-map' isn't loaded in time for `:bind' above.
    (define-key eshell-hist-mode-map (kbd "M-s") nil))

  (defun my/eshell-pre-command ()
    "Eshell pre-command hook function."
    ;; Interactive eshell commands should use colors but this gets reverted by
    ;; the post-command hook.
    ;; See: https://github.com/daviwil/dotfiles/blob/master/Emacs.org#configuration.
    (setenv "TERM" "xterm-256color"))

  (defun my/eshell-post-command ()
    "Eshell post-command hook function."
    (setenv "TERM" "dumb"))

  (defun my/eshell-new ()
    "Create a new eshell buffer."
    (interactive)
    (eshell t))

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

  (defun my/sink (&optional name)
    "Return a reference to a buffer for sinking eshell command output.
If NAME is specified, a reference to that buffer will be returned, creating the
buffer if necessary. If NAME is not specified, a buffer name will be generated."
    (let* ((name (or name (generate-new-buffer-name "*eshell-output*")))
           (buf (get-buffer-create name)))
      (display-buffer buf)
      buf)))

;;;; Org Mode

(use-package org
  :preface
  ;; GTD (Getting Things Done) paths.
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
   my/org-agenda-refile
   my/org-agenda-refile-archive
   my/org-agenda-refile-personal-ongoing
   my/org-agenda-refile-work-ongoing
   my/org-agenda-refile-someday-ongoing
   my/org-agenda-refile-inbox)

  :bind
  (:map global-map
        ("C-c o l" . org-store-link)
        ("C-c o a" . org-agenda)
        ("C-c o m" . org-capture)
        ("C-c o S" . org-save-all-org-buffers)
        ("C-c o i" . my/org-capture-inbox)
        ("C-c o b" . my/org-capture-bookmark)
        ("C-c o c" . my/org-capture-coffee)
        ("C-c C-o" . org-open-at-point-global)) ;; Open links everywhere.
  (:map org-mode-map
        ("C-'" . nil) ;; Used for Popper.
        ("C-c C-S-l" . org-cliplink))
  (:map org-agenda-mode-map
        ("r" . my/hydra-org-agenda-refile/body)
        ("k" . org-agenda-kill)
        ("?" . which-key-show-major-mode))

  :hook
  (org-mode . my/init-org-mode)

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
  ;; Require the `org-agenda' package so that its keymap can be customized
  ;; above and `org-tempo' to enable <s + TAB shortcuts in org docs.
  (require 'org-agenda)
  (require 'org-tempo)

  :config
  (defun my/init-org-mode ()
    "Org-mode init function that should be attached to `org-mode-hook`."
    (interactive)
    (org-indent-mode 1)
    (visual-line-mode 1)
    (display-line-numbers-mode 0))

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

  (defhydra my/hydra-org-agenda-refile ()
    "
Refile this agenda item to:

_a_: Archive
_p_: Personal/ongoing
_w_: Work/ongoing
_s_: Someday/ongoing
_i_: Inbox
_q_: Quit this menu
"
    ("a" my/org-agenda-refile-archive :exit t)
    ("p" my/org-agenda-refile-personal-ongoing :exit t)
    ("w" my/org-agenda-refile-work-ongoing :exit t)
    ("s" my/org-agenda-refile-someday-ongoing :exit t)
    ("i" my/org-agenda-refile-inbox :exit t)
    ("q" nil))

  ;; Function for refiling the current agenda item under point to the specified
  ;; file and heading. `org-agenda-refile' requires a destination refloc list
  ;; that is difficult to compose manually. This approach to pulling the refloc
  ;; out of the return value from `org-refile-get-targets' is adapted from:
  ;; https://emacs.stackexchange.com/questions/54580/org-refile-under-a-given-heading.
  (defun my/org-agenda-refile (file heading)
    "Refiles the current agenda item to the refile target for FILE and HEADING."
    (org-agenda-refile nil
                       (seq-find
                        (lambda (refloc)
                          (and
                           (string= heading (nth 0 refloc))
                           (string= file (nth 1 refloc))))
                        (org-refile-get-targets)) nil))

  (defun my/org-agenda-refile-archive (&optional category)
    "Refiles the current org agenda item into the appropriate archive.
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
    "Refiles the current org agenda item into the personal/ongoing list."
    (interactive)
    (my/org-agenda-refile my/gtd-personal-file "Projects/Ongoing/Tasks"))

  (defun my/org-agenda-refile-work-ongoing ()
    "Refiles the current org agenda item into the work/ongoing list."
    (interactive)
    (my/org-agenda-refile my/gtd-work-file "Projects/Ongoing/Tasks"))

  (defun my/org-agenda-refile-someday-ongoing ()
    "Refiles the current org agenda item into the someday/ongoing list."
    (interactive)
    (my/org-agenda-refile my/gtd-someday-file "Projects/Ongoing/Tasks"))

  (defun my/org-agenda-refile-inbox ()
    "Refiles the current org agenda item into the inbox list."
    (interactive)
    (my/org-agenda-refile my/gtd-inbox-file "Inbox"))

  ;; Save all org buffers before quitting the agenda ('s' saves immediately).
  (advice-add #'org-agenda-quit :before #'org-save-all-org-buffers)

  ;; Make it easier to create `org-babel' code blocks.
  (add-to-list #'org-structure-template-alist '("el" . "src emacs-lisp")))

(use-package org-cliplink)

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org-roam
  :commands org-roam-db-autosync-mode
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n g" . org-roam-graph)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n c" . org-roam-capture)
   ("C-c n t" . org-roam-tag-add)
   ("C-c j ." . org-roam-dailies-find-directory)
   ("C-c j j" . org-roam-dailies-capture-today)
   ("C-c j J" . org-roam-dailies-goto-today)
   ("C-c j y" . org-roam-dailies-capture-yesterday)
   ("C-c j Y" . org-roam-dailies-goto-yesterday)
   ("C-c j d" . org-roam-dailies-capture-date)
   ("C-c j D" . org-roam-dailies-goto-date))

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
  :straight nil
  :bind
  (:map global-map
        ("C-x C-p" . proced))
  :custom
  (proced-enable-color-flag t))

;;;; Credential Management

(use-package pass)

(use-package auth-source-pass
  :straight nil
  :custom
  (auth-source-do-cache nil)
  :init
  (auth-source-pass-enable))

;;; End:
(provide 'init)

;;; init.el ends here
