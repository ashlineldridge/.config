;;; init.el --- Emacs Init -*- lexical-binding: t -*-

;; Author: Ashlin Eldridge <aeldridge@fastmail.com>
;; URL: https://github.com/ashlineldridge/.config
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;;
;; My bonsai.

;;; Code:

;;;; System Settings

;; XDG directories.
(defvar my/xdg-config-dir (expand-file-name "~/.config"))
(defvar my/xdg-cache-dir  (expand-file-name "~/.cache"))
(defvar my/xdg-data-dir   (expand-file-name "~/.local/share"))

;; Emacs directories.
(defvar my/emacs-config-dir (expand-file-name "emacs" my/xdg-config-dir))
(defvar my/emacs-cache-dir  (expand-file-name "emacs" my/xdg-cache-dir))
(defvar my/emacs-data-dir   (expand-file-name "emacs" my/xdg-data-dir))

;; Why would this be true? https://blog.sumtypeofway.com/posts/emacs-config.html
(setq sentence-end-double-space nil)

;; Accept 'y' in lieu of 'yes'.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Make input characters overwrite the current region.
(delete-selection-mode 1)

;; Show echoed keystrokes quicker.
(setq echo-keystrokes 0.01)

;; Use CMD key for META.
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

;; Store all backup files under XDG_CACHE_HOME.
(setq backup-directory-alist `(("" . ,(expand-file-name "backup/" my/emacs-cache-dir))))
(setq auto-save-file-name-transforms `((".*" ,(expand-file-name "saves/" my/emacs-cache-dir) t)))

;; Save interrupted session records under XDG_DATA_HOME.
(setq auto-save-list-file-prefix (expand-file-name "auto-save-list/" my/emacs-cache-dir))

;; I don't want `custom-set-variables'-managed configuration as I want to do it properly
;; within the structure of this file. So here we tell Emacs to save custom settings into
;; a separate file which we then never load.
(setq custom-file (expand-file-name "custom.el" my/emacs-config-dir))

;; Don't ask when following symlinks to source code.
(setq vc-follow-symlinks t)

;; Automatically save unsaved files before compilation.
(setq compilation-ask-about-save nil)

;; Make query replaces case-sensitive.
(setq case-fold-search nil)

;; Revert buffers when the underlying file has changed.
(global-auto-revert-mode 1)

;; Enable recentf-mode so that we can reopen recently opened files.
(recentf-mode 1)

;; Enable Emacs functionality which is disabled by default.
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Prefer running Emacs in server mode and have Git and $EDITOR use emacsclient
;; to open files in a single Emacs instance.
(server-start)

;;;; Boot Performance

;; If I need to run Emacs in the terminal I'll use `-nw -q`. Better than littering this file with
;; unnecessary window system checks.
(unless window-system
  (error "Emacs init file is for window systems only"))

;; Emacs performance settings. I'm following the general performance recommendations of lsp-mode
;; here https://emacs-lsp.github.io/lsp-mode/page/performance. Some people recommend against
;; modifying gc-cons-threshold but it does seem to speed things up for me.
(setq gc-cons-threshold (* 100 1024 1024))
(setq read-process-output-max (* 1 1024 1024))

;; Keep track of start up time.
(add-hook 'emacs-startup-hook
	  (lambda ()
            (message "Emacs ready in %s seconds with %d garbage collections."
		     (emacs-init-time "%.2f")
                     gcs-done)))

;;;; Native Compilation

;; Quieten Emacs 28+ down as otherwise it prints many warnings when compiling packages.
;; See https://www.reddit.com/r/emacs/comments/l42oep/suppress_nativecomp_warnings_buffer.
(setq native-comp-async-report-warnings-errors nil)

;; Set the right directory to store the native comp cache
(add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" my/xdg-cache-dir))

;;;; Package Management

;; Bootstrap straight.el.
;; See: https://github.com/raxod502/straight.el#bootstrapping-straightel
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Load straight helper package.
(require 'straight-x)

;; Install use-package.
(straight-use-package 'use-package)

;; Make use-package use straight for package installation.
(setq straight-use-package-by-default t)

;; Install org as early as possible after straight so that the built-in version of org doesn't
;; get activated between here and where org is actually configured below.
(straight-use-package 'org)

;;;; Keyboard Bindings

;;;;; Stateful Keymaps

(use-package hydra)

;;;;; High-Level Keyboard Bindings

;; ESC cancels all.
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; Use C-x C-k to kill the current buffer as C-x k prompts for which buffer to kill.
(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
;; TODO: Use a use-package prog-mode to set this up and move it Programming section
(global-set-key (kbd "C-;") 'comment-line)
;; Replace list-buffers with the more advanced ibuffer.
(global-set-key (kbd "C-x C-b") 'ibuffer)
;; Remove compose-mail binding.
(global-set-key (kbd "C-x m") nil)
;; Trim trailing whitespace.
(global-set-key (kbd "C-x C-w") 'delete-trailing-whitespace)

;;;; Appearance

;;;;; General Appearance

;; Do not show the startup screen.
(setq inhibit-startup-message t)

(tool-bar-mode 0)   ;; Disable the tool bar.
(tooltip-mode 0)    ;; Disable tool tips.
(menu-bar-mode 0)   ;; Disable the menu bar.
(scroll-bar-mode 0) ;; Disable visible scrollbar.
(set-fringe-mode 5) ;; Increase left/right margins slightly.

;; Make frame size bigger.
(set-frame-size (selected-frame) 110 50)

;; Flash the mode line rather than use an audible bell.
(setq visible-bell nil)
(setq ring-bell-function (lambda ()
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil 'invert-face 'mode-line)))

;; This setting makes the Emacs window play nicer with window managers like yabai.
(setq frame-resize-pixelwise t)

;; Truncate long lines in programming modes.
(add-hook 'prog-mode-hook (lambda () (setq truncate-lines t)))

;; Programming-ish mode hooks.
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda ()
                   (display-line-numbers-mode 1)        ;; Enable line numbers.
                   (setq show-trailing-whitespace t)))) ;; Show trailing whitespace.

;;;;; Fonts

;; Font-related variable definitions.
(defvar my/fixed-font "Iosevka SS14") ;; "Jetbrains Mono" and "Cantarell"" are also good ones.
(defvar my/variable-font "Iosevka Aile") ;; ETBembo is another good one.
(defvar my/fixed-font-size 140)
(defvar my/variable-font-size 140)
(defvar my/line-number-font-size 120)
(defvar my/mode-line-font-size 130)

(defun my/init-faces ()
  "Intialize font face attributes."
  (interactive)
  (set-face-attribute 'default nil
                      :font my/fixed-font
                      :height my/fixed-font-size
                      :width 'normal
                      :weight 'normal)
  (set-face-attribute 'variable-pitch nil
                      :font my/variable-font
                      :height my/variable-font-size
                      :width 'normal
                      :weight 'normal)
  (set-face-attribute 'line-number nil
                      :font my/fixed-font
                      :height my/line-number-font-size
                      :width 'normal
                      :weight 'ultra-light)
  (set-face-attribute 'mode-line nil :height my/mode-line-font-size)
  (set-face-attribute 'mode-line-inactive nil :height my/mode-line-font-size))

;;;;; Icons

(use-package all-the-icons
  :config
  ;; Only install the fonts if they are not already installed. See:
  ;; https://github.com/domtronn/all-the-icons.el/issues/120#issuecomment-480342779
  (unless (member "all-the-icons" (font-family-list))
      (all-the-icons-install-fonts t)))

;;;;; Themes

;; Note: I'm going to push forward with the excellenet and highly-customisable
;; Modus themes and disable all the others for now. The Modus themes are already
;; really good out-of-the-box (and the latest versions are better than the pre-
;; packaged versions, IMO). There should be a setting for anything I'd want to
;; configure. See https://systemcrafters.net/emacs-from-scratch/the-modus-themes
;; for common customisations.

(use-package modus-themes
  ;; This package is pre-installed into Emacs 28+ but we pull latest to get
  ;; the updates that have been made.
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-region '(no-extend accented)) ; Play with bg-only as well.
  (modus-themes-mode-line '(borderless))
  (modus-themes-paren-match '(intense underline))
  (modus-themes-prompts '(bold intense))
  (modus-themes-org-blocks 'gray-background)
  (modus-themes-fringes nil)
  (modus-themes-headings
   '((1 . (variable-pitch rainbow background 1.3))
     (2 . (variable-pitch rainbow background semibold 1.2))
     (3 . (variable-pitch rainbow background semibold 1.1))
     (t . (variable-pitch rainbow semilight 1.1)))))

(defvar my/themes
  '(modus-vivendi
    modus-operandi
    ;; doom-opera-light
    ;; doom-tomorrow-night
    ;; doom-one
    ;; doom-nord-light
    ;; zenburn
    ))

(defun my/disable-all-themes ()
  "Disable all active themes."
  (interactive)
  (dolist (theme custom-enabled-themes)
    (disable-theme theme)))

(defun my/load-theme (theme)
  "Load the THEME."
  (interactive)
  (my/disable-all-themes)
  (load-theme theme t)
  (my/init-faces)
  (message "Loaded theme: %s" theme))

(defun my/load-next-theme ()
  "Cycle through and load the next theme in the list."
  (interactive)
  (setq my/themes (append (cdr my/themes) `(,(car my/themes))))
  (my/load-theme (car my/themes)))

(defun my/load-prev-theme ()
  "Cycle back and load the previous theme in the list."
  (interactive)
  (setq my/themes (append (last my/themes) (butlast my/themes)))
  (my/load-theme (car my/themes)))

(defhydra hydra-manage-themes (global-map "C-c h")
  "manage-themes"
  ("n" my/load-next-theme "load-next-theme")
  ("p" my/load-prev-theme "load-prev-theme")
  ("d" my/disable-all-themes "disable-all-themes")
  ("q" nil "quit"))

;; Load the first theme in the list.
(my/load-theme (car my/themes))

;;;;; Mode Line

(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  (setq column-number-indicator-zero-based nil)
  (column-number-mode 1)
  :custom
  ;; Mode line height is determined by the smaller of doom-modeline-height and the mode line
  ;; The function doom-modeline--font-height can be called to determine the actual font height
  ;; that will be used when determining the minimum height of the mode line.
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
  (doom-modeline-column-zero-based t))

(use-package minions
  :init (minions-mode 1))

;; Enable recursive editing to allow multiple minibuffers to be opened on top of
;; each other. E.g., this allows starting a query-replace, then open a file to look
;; at something, then go back to the query-replace minibuffer.
(setq enable-recursive-minibuffers t)
;; Display a small "[n]" that shows the minibuffer recursive depth. Another option is to
;; use the minad/recursion-indicator but I don't really use the feature enough.
(minibuffer-depth-indicate-mode)

;;;; Window Management

;;;;; Window Selection with Ace Window

(use-package ace-window
  :bind
  (:map global-map
        ("M-o" . ace-window))
  :custom
  (aw-display-mode-overlay t)
  ;; (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-dispatch-always t)
  (aw-background nil)
  :config
  (setq aw-dispatch-alist
   '((?k aw-delete-window "Delete Window")
     (?K delete-other-windows "Delete Other Windows")
     (?s aw-swap-window "Swap Windows")
     (?m aw-move-window "Move Window")
     (?c aw-copy-window "Copy Window")
     (?o aw-flip-window) ; Flip to previous window (adding description text doesn't work for some reason).
     (?v aw-split-window-vert "Split Vert Window")
     (?h aw-split-window-horz "Split Horz Window")
     (?? aw-show-dispatch-help))))

;;;;; Jump Between Point Locations

(use-package avy
  :bind
  (("C-: c" . avy-goto-char)
   ("C-: l" . avy-goto-line)
   ("C-: w" . avy-goto-word-1)))

;;;;; Window History

(use-package winner
  :straight nil ;; Comes pre-installed.
  :config (winner-mode 1))

;;;;; Window Management Keybindings

;; See roided out version here: https://github.com/jmercouris/configuration/blob/master/.emacs.d/hydra.el#L86
;; See simpler version here: https://www.reddit.com/r/emacs/comments/b13n39/how_do_you_manage_window_sizes_in_emacs/eik6xzb
(defhydra hydra-manage-windows (global-map "C-c w")
  "manage-windows"
  ("<left>"  shrink-window-horizontally  "shrink-horizontal")
  ("<right>" enlarge-window-horizontally "grow-horizontal")
  ("<down>"  shrink-window               "shrink-vertical")
  ("<up>"    enlarge-window              "grow-vertical")
  ("="       balance-windows             "balance")
  ("u"       winner-undo                 "winner-undo" :exit t)
  ("U"       winner-redo                 "winner-redo" :exit t)
  ("w"       window-toggle-side-windows  "toggle-side-windows" :exit t)
  ;; ("k l"     (my/delete-window 'left)    "delete-window-left" :exit t)
  ;; ("k r"     (my/delete-window 'right)   "delete-window-right" :exit t)
  ;; ("k a"     (my/delete-window 'above)   "delete-window-above" :exit t)
  ;; ("k b"     (my/delete-window 'below)   "delete-window-below" :exit t)
  ("q" nil "quit"))

(defun my/delete-window (direction)
  "Deletes the window in the specified DIRECTION."
  (let ((window (window-in-direction direction)))
    (if window
        (delete-window window)
      (message "No window in direction: %s." direction))))

;;;;; Display Buffer Settings

;; Take over Emacs' default buffer placement algorithm. Taken from here:
;; https://github.com/daviwil/dotfiles/blob/master/Emacs.org#control-buffer-placement.
;; See also: https://www.youtube.com/watch?v=-H2nU0rsUMY.
(setq display-buffer-base-action
      '((display-buffer-reuse-mode-window
        display-buffer-reuse-window
        display-buffer-same-window)))

;; If a popup does happen, don't resize windows to be equal-sized.
(setq even-window-sizes nil)

;; One day I will learn how this bloody monster actually works.
;; Just read: https://www.gnu.org/software/emacs/manual/html_node/elisp/Displaying-Buffers.html
;; For examples see:
;; https://github.com/daviwil/emacs-from-scratch/blob/master/show-notes/Emacs-Tips-DisplayBuffer-1.org
;; https://www.reddit.com/r/emacs/comments/dqz5ux/thanks_for_the_displaybuffer_knowledge/ and
;; https://www.reddit.com/r/emacs/comments/cpdr6m/any_additional_docstutorials_on_displaybuffer_and/
(setq display-buffer-alist
      '(
        ;; Rust operation windows:
        ("\\(cargo-\\|rustic-\\|rustfmt\\)"
         ;; It is not really necessary for me to use display-buffer-reuse-mode-window below since I'm
         ;; always displaying Rust operation windows in the bottom side window. The below mode-based
         ;; configuration only takes effect if I manually open a Rustic mode window and then expect
         ;; subsequent Rustic buffers to reuse that window. But I'm going to leave the below in place
         ;; for the moment to remind myself of how this works.
         (display-buffer-reuse-mode-window display-buffer-in-side-window)
         ;; This alist argument caters for a list of modes as well as a single mode. Even though all of
         ;; the modes below derive (or are) rustic-compilation-mode, they all need to be specified to have
         ;; the desired effect because if, for example, a window displays a buffer in rustic-cargo-test-mode,
         ;; rustic-cargo-run-mode does not derive from that (it is a sibling) so that window will not be used.
         (mode . (rustic-compilation-mode
                  rustic-format-mode
                  rustic-cargo-test-mode
                  rustic-cargo-run-mode
                  rustic-cargo-clippy-mode
                  rustic-cargo-outdated-mode))
         (side . bottom))
        ("\\*compilation\\*"
         (display-buffer-in-side-window)
         (side . bottom))
        ("\\*Flycheck "
         (display-buffer-in-side-window)
         (side . bottom))
        ("\\*xref\\*"
         (display-buffer-in-side-window)
         (side . bottom))
        ("\\*lsp-help\\*"
         (display-buffer-in-side-window)
         (side . bottom))
        ("\\*org-roam\\*"
         (display-buffer-in-side-window)
         (side . bottom))))

;;;; Environment Variables

;;;;; Import Shell Variables

(use-package exec-path-from-shell
  :init (when (memq window-system '(mac ns x))
	  (setq exec-path-from-shell-variables
		'("PATH" "MANPATH" "XDG_CONFIG_HOME" "XDG_CACHE_HOME" "XDG_DATA_HOME"
		  "SHELL" "GNUPGHOME" "PASSWORD_STORE_DIR" "DEVELOPER_DIR" "SDKROOT"
                  "GOPATH" "GOROOT"))
	  (setq exec-path-from-shell-arguments nil)
	  (exec-path-from-shell-initialize)))

;;;; Help System

(use-package helpful
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

(use-package which-key
  :init (which-key-mode)
  :custom
  ;; Use Embark which is searchable instead.
  (which-key-show-early-on-C-h nil)
  (which-key-idle-delay 2)
  (which-key-idle-secondary-delay 0.05)
  (which-key-popup-type 'side-window)
  (which-key-side-window-location '(bottom right)))

;;;; Completion System

(use-package corfu
  :straight '(corfu-mode :host github :repo "minad/corfu")
  :bind
  (:map corfu-map
        ;; By default corfu-insert-separator is bound to M-SPC which on macOS is
        ;; already taken by Spotlight. Instead, bind it to S-SPC - this allows us
        ;; to enter a space character using S-SPC to keep the completion going.
        ("S-SPC"   . corfu-insert-separator))
  :init
  ;; Enable globally; exclusions are captured individually by `corfu-excluded-modes'.
  (global-corfu-mode)
  :custom
  ;; Needs to be set so that the Corfu post-command-hook is installed and Corfu is
  ;; called on every key press (so that it knows whether to display a pop-up).
  (corfu-auto t)
  ;; Number of typed characters before Corfu displays its pop-up.
  (corfu-auto-prefix 1)
  ;; Number of seconds of inactivity before the Corfu pop-up is displayed. This setting
  ;; only applies after the minimum number of prefix characters have been entered.
  (corfu-auto-delay 0)
  ;; Modes which shouldn't use corfu. The following modes have been added as the
  ;; completions are kinda useless. Seems like corfu requires the concrete mode -
  ;; you can't use the derived-from mode.
  (corfu-excluded-modes
   '(org-mode
     bazel-build-mode
     bazel-workspace-mode
     bazel-starlark-mode)))

;; Enable indentation + completion using the TAB key. This is required for Corfu integration.
(setq tab-always-indent 'complete)

;; Documentation shown alongside Corfu completion popups.
(use-package corfu-doc
  :straight '(corfu-doc-mode :host github :repo "galeo/corfu-doc")
  :after corfu
  :bind
  (:map corfu-map
        ("M-S-d"   . corfu-doc-mode)
        ("M-d"     . corfu-doc-toggle)
        ("M-p"     . corfu-doc-scroll-down)
        ("M-n"     . corfu-doc-scroll-up))
  ;; Don't enable corfu-doc-mode by default as it can be a bit much and with my smaller
  ;; screen the popup frame sometimes shows in odd places. For now, toggle the doc pop-up
  ;; using M-d or enable `corfu-doc-mode' using M-S-d (configured above).
  ;; :hook (corfu-mode . corfu-doc-mode)
  :custom
  ;; Show doc immediately.
  (corfu-doc-delay 0))

;; Nice icons in the margin of corfu completion popups.
(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; To compute blended backgrounds correctly.
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;; Vertico provides the vertical completion minibuffer and Orderless provides the
;; "completion style". Some commands that make use of Vertico's selection list also
;; allow a new distinct value to be entered. E.g., `org-roam-node-insert' will create
;; a new note when given a new note name. However, if the new value matches part of
;; an existing value in the selection list (which is more likely when using Orderless)
;; then you will need to press M-RET which calls `vertico-exit-input' to cancel the
;; completion and use the new value.
(use-package vertico
  :straight '(vertico-mode :host github :repo "minad/vertico")
  :init
  (vertico-mode 1))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :config
  (setq history-length 25)
  (savehist-mode 1))

;; Orderless configuration taken from https://github.com/minad/corfu/wiki#basic-example-configuration-with-orderless.
(use-package orderless
  :init
  (setq completion-styles '(orderless partial-completion basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package marginalia
  :init
  (marginalia-mode 1))

(defvar my/consult-source-vterm-buffer
  `(:name     "Vterm Buffer"
    :narrow   ?v
    :category vterm-buffer
    :face     consult-buffer
    :history  buffer-name-history
    :state    ,#'consult--buffer-state
    :action   ,#'consult--buffer-action
    :items
    ,(lambda () (consult--buffer-query :sort 'visibility
                                       :as #'buffer-name
                                       :mode 'vterm-mode)))
  "Vterm buffer source for `consult-buffer'.")

(use-package consult
  :straight '(consult-mode :host github :repo "minad/consult")
  :bind
  (("C-s"     . consult-line)
   ("C-x i"   . consult-imenu)       ; Local buffer imenu
   ("C-x I"   . consult-imenu-multi) ; Open buffers imenu
   ("C-x b"   . consult-buffer)
   ("M-g M-g" . consult-goto-line)
   ("M-y"     . consult-yank-pop)
   ("C-x r r" . consult-register)
   ("C-x r l" . consult-register-load)
   ("C-x r s" . consult-register-store))

  :init
  ;; Use consult to select xref locations with preview.
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  ;; Show narrowing help in the minibuffer when ? is pressed.
  ;; Narrowing by group requires pressing the group key (e.g., p for project) and then <spc>.
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  (consult-customize
   ;; Source name and narrow key customization. The consult-project-extra sources
   ;; need to be patched with :state otherwise previewing will not work on them.
   consult--source-buffer
   :name "Open Buffer" :narrow ?b
   consult--source-project-buffer
   :name "Project Buffer" :narrow ?p
   consult--source-recent-file
   :name "Recent File" :narrow ?r

   ;; By default, consult will automatically preview all commands and sources. Below
   ;; we customize certain commands/sources so that the preview is shown on request.
   consult-ripgrep
   consult-git-grep
   consult-grep
   consult-bookmark
   consult-recent-file
   consult-xref
   consult--source-buffer
   consult--source-project-buffer
   consult--source-bookmark
   consult--source-recent-file
   my/consult-source-vterm-buffer
   ;; So that the consult-ripgrep project shortcut doesn't show previews (you need to customize
   ;; the interactive function: https://github.com/minad/consult/issues/676#issuecomment-1286196998).
   project-switch-project
   :preview-key (kbd "M-."))

  ;; Customise the list of sources shown by consult-buffer.
  (setq consult-buffer-sources
        '(consult--source-buffer                ; Narrow: ?b
          consult--source-project-buffer        ; Narrow: ?p
          my/consult-source-vterm-buffer        ; Narrow: ?v
          consult--source-recent-file           ; Narrow: ?r
          consult--source-bookmark              ; Narrow: ?m
          ))

  ;; Add `consult-imenu' groupings for Rust. These groupings originate from the LSP specification:
  ;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#symbolKind.
  ;; The lsp-types crate defines the SymbolKind struct; rust-analyzer uses this crate to expose the set
  ;; of symbols that it supports. lsp-mode, in turn, defines the `lsp-imenu-symbol-kinds' variable which
  ;; maps each symbol's integer value to the group names (e.g., "Type Parameters") used below.
  (require 'consult-imenu)
  (add-to-list 'consult-imenu-config
               '(rustic-mode
                 :types ((?f "Functions")
                         (?o "Objects")
                         (?e "Enums")
                         (?E "Enum Members")
                         (?s "Structs")
                         (?S "Fields")
                         (?m "Modules")
                         (?t "Type Parameters")
                         (?c "Constants"))))

  ;; Tell ripgrep to search hidden directories/files but ignore .git/.
  (setq consult-ripgrep-args
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
          ".")))

(use-package consult-dir
  :bind
  (:map global-map
        ("C-x C-d" . consult-dir))
  (:map vertico-map
        ("C-x C-d" . consult-dir)
        ("C-x C-j" . consult-dir-jump-file)))

;; See more advanced configuration here:
;; https://github.com/karthink/.emacs.d/blob/42875586daa23d69c581be01bdc1e12718aef083/lisp/setup-embark.el.
(use-package embark
  ;; Load after xref so that the overidden keybinding below takes effect.
  :after xref
  :bind
  (:map global-map
        ("C-."   . embark-act)
        ("M-."   . embark-dwim)
        ("C-h b" . embark-bindings)) ;; Replace `describe-bindings'.
  :custom
  (embark-mixed-indicator-delay 0.5)
  :config
  ;; Run Embark after a prefix (e.g. C-x) is pressed and then C-h.
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult))

;; Taken from here: https://karthinks.com/software/fifteen-ways-to-use-embark/
;; TODO: Work in progress... What's a better way of organising this? Some of the
;; embark maps seem to be wrong - e.g. I can't use C-c n f and then act on a note.
;; (eval-when-compile
;;   (defmacro my/embark-ace-action (fn)
;;     `(defun ,(intern (concat "my/embark-ace-" (symbol-name fn))) ()
;;        (interactive)
;;        (with-demoted-errors "%s"
;;          (require 'ace-window)
;;          (let ((aw-dispatch-always t))
;;            (aw-switch-to-window (aw-select nil))
;;            (call-interactively (symbol-function ',fn)))))))

;; (define-key embark-file-map     (kbd "o") (my/embark-ace-action find-file))
;; (define-key embark-buffer-map   (kbd "o") (my/embark-ace-action switch-to-buffer))
;; (define-key embark-bookmark-map (kbd "o") (my/embark-ace-action bookmark-jump))

(use-package wgrep
  :bind
  (("C-c C-w" . wgrep-change-to-wgrep-mode))
  :custom
  (wgrep-auto-save-buffer t))

;;;; General Editing

;;;;; Yanking and Deleting

(defun my/copy-to-end-of-line ()
  "Copy the text from point to the end of the line to the kill ring."
  (interactive)
  (let* ((num-chars (- (line-end-position) (point)))
         (suffix (if (eq 1 num-chars) "" "s")))
    (kill-ring-save (point) (line-end-position))
    (message "Copied %d character%s." num-chars suffix)))

(defun my/delete-to-end-of-line ()
  "Delete the text from point to the end of the line without copying to the kill ring."
  (interactive)
  (let* ((num-chars (- (line-end-position) (point)))
         (suffix (if (eq 1 num-chars) "" "s")))
    (delete-region (point) (line-end-position))
    (message "Deleted %d character%s." num-chars suffix)))

(global-set-key (kbd "C-S-k") 'my/copy-to-end-of-line)
(global-set-key (kbd "C-M-k") 'my/delete-to-end-of-line)

;;;;; Mark Ring

;; When popping marks off the mark ring (C-u C-SPC for local or C-x C-SPC for global),
;; allow repeated invocations of C-SPC to keeping popping.
(setq set-mark-command-repeat-pop t)
;; Make the rings a bit shorter (default 16) to make rotating around the ring quicker.
(setq mark-ring-max 8)
(setq global-mark-ring-max 8)

;;;;; Region Expansion

(use-package expand-region
  :bind (("C-=" . er/expand-region)
         ("C-+" . er/mark-outside-pairs))
  :custom
  (expand-region-fast-keys-enabled nil)
  (expand-region-autocopy-register "e"))

;;;; File System

;;;;; File Browsing

(use-package dired
  :straight nil ;; Built-in.
  :bind
  (:map dired-mode-map
        ("N" . dired-create-empty-file)
        ("?" . which-key-show-major-mode))
  :config
  ;; Revert Dired (and other non-file) buffers.
  (setq global-auto-revert-non-file-buffers t)
  ;; Kill current dired buffer when opening a new one (e.g. when navigating into a directory).
  (setq dired-kill-when-opening-new-dired-buffer t))

;;;;; Project Management

(use-package project
  :straight nil ;; Built-in.
  :bind
  (:map global-map
        ("C-x p"   . nil) ; Remove previous bindings
        ("C-x p d" . project-dired)
        ("C-x p c" . project-compile)
        ("C-x p f" . project-find-file)
        ("C-x p F" . my/project-find-file-relative)
        ("C-x p k" . project-kill-buffers)
        ("C-x p p" . project-switch-project)
        ("C-x p r" . consult-ripgrep)
        ("C-x p R" . project-query-replace-regexp)
        ("C-x p u" . my/project-refresh-list))
  :custom
  (project-list-file (expand-file-name "projects" my/emacs-cache-dir))
  (project-switch-commands
   '((project-find-file   "Find file"    ?f)
     (magit-status        "Magit Status" ?g)
     (consult-ripgrep     "Ripgrep"      ?r)
     (multi-vterm-project "Vterm"        ?v)
     (project-dired       "Dired"        ?d)))
  :config
  ;; Override the way that project.el determines the project root.
  (setq project-find-functions '(my/project-find-root)))

;; Customise project root identification (the default only searches for version control markers).
;; See: https://andreyorst.gitlab.io/posts/2022-07-16-project-el-enhancements
;; And: https://github.com/golang/tools/blob/9b5e55b1a7e215a54c9784492d801104a8381a91/gopls/doc/emacs.md#configuring-project-for-go-modules-in-emacs
;; Note: Although it's sometimes handy to put files like go.mod and Cargo.toml in the list below, this
;; is problematic when dealing with monorepos as you can get "stuck" in a subdirectory of the monorepo.
(defvar my/project-root-markers
  '(".git" ".project" ".projectile"))

(defun my/project-root-p (dir)
  "Check whether DIR is a project root."
  (catch 'found
    (dolist (marker my/project-root-markers)
      (when (file-exists-p (expand-file-name marker dir))
        (throw 'found marker)))))

(defun my/project-find-root (dir)
  "Search up the directory tree from DIR to find the project root."
  (when-let ((root (locate-dominating-file dir #'my/project-root-p)))
    `(transient . ,(expand-file-name root))))

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
  (my/project-index-under "~/dev/home/00rg")
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
               count (if (= count 1) "" "s")))))

;;;;; Auto-Save

(use-package super-save
  :custom
  (super-save-auto-save-when-idle t)
  ;; Large idle duration to avoid programming modes (e.g., Rustic) that
  ;; perform actions on save from running to eagerly.
  (super-save-idle-duration 40)
  (super-save-max-buffer-size 100000)
  :config
  (setq auto-save-default nil) ; Disable built-in auto-save-mode.
  (super-save-mode +1))

;;;; Programming

;;;;; Outline

(defvar my/outline-headings-per-mode
  '((emacs-lisp-mode . ";\\{3,\\}+ [^\n]")))

(defvar my/outline-major-modes-blocklist
  '(org-mode outline-mode markdown-mode edebug-eval-mode))

;; Taken from https://protesilaos.com/emacs/dotemacs
(defun my/outline-minor-mode-safe ()
  "Enables and configures `outline-minor-mode' if it is safe to do so."
  (interactive)
  (let* ((blocklist my/outline-major-modes-blocklist)
         (mode major-mode)
         (headings (alist-get mode my/outline-headings-per-mode)))
    (unless (derived-mode-p (car (member mode blocklist)))
      (if (null outline-minor-mode)
          (progn
            (when (derived-mode-p mode)
              (setq-local outline-regexp headings))
            (outline-minor-mode 1))
        (outline-minor-mode -1)))))

(use-package outline
  :straight nil ;; Built-in.
  ;; :hook
  ;; TODO: Seems broken since last upgrade to Emacs 29
  ;; (emacs-lisp-mode . my/outline-minor-mode-safe)
  :bind
  (:map outline-minor-mode-map
        ("C-c C-n"  . outline-next-visible-heading)
        ("C-c C-p"  . outline-previous-visible-heading)
        ("C-c C-f"  . outline-forward-same-level)
        ("C-c C-b"  . outline-backward-same-level)
        ("C-c C-a"  . outline-show-all)
        ("C-c C-h"  . outline-hide-other)
        ("C-c C-u"  . outline-up-heading)
        ("M-<down>" . outline-move-subtree-down)
        ("M-<up>"   . outline-move-subtree-up))
  :custom
  (outline-minor-mode-cycle t))

;;;;; Code Templating

(use-package yasnippet
  :hook ((text-mode . yas-minor-mode)
         (prog-mode . yas-minor-mode))
  :init
  ;; Remove default key bindings which are crap.
  (setq yas-minor-mode-map (make-sparse-keymap))
  :bind
  (:map global-map
        ("C-c y f" . yas-visit-snippet-file)
  	("C-c y n" . yas-new-snippet)
        ("C-c y u" . yas-reload-all))
  (:map yas-minor-mode-map
        ("C-c y y" . yas-expand)
        ("C-c y i" . consult-yasnippet))
  :config
  (use-package consult-yasnippet)
  (setq yas-verbosity 1)
  (setq yas-wrap-around-region t)
  (yas-reload-all))

;;;;; Language Support

;;;;;; Language Server Support

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook
  (c-mode . lsp-deferred)
  (c++-mode . lsp-deferred)
  (go-mode . lsp-deferred)
  (rustic-mode . lsp-deferred)
  (sh-mode . lsp-deferred)
  (terraform-mode . lsp-deferred)
  (python-mode . lsp-deferred)
  :bind
  (:map lsp-mode-map
        ;; M-? is normally bound to xref-find-references but the way that this function exposes
        ;; references is not particularly helpful so we override it with lsp-find-references.
        ;; M-. (xref-find-definitions) and M-, (xref-pop-marker-stack) we'll leave in place since
        ;; they work well.
        ("M-?" . lsp-find-references)
        ("C-c l m" . lsp-ui-imenu)
        ("C-c l c d" . consult-lsp-diagnostics)
        ("C-c l c s" . (lambda () (interactive) (consult-lsp-file-symbols t)))
        ;; Still trying to figure out the benefit of consult-lsp-symbols.
        ("C-c l c S" . consult-lsp-symbols))
  :init
  (defun my/if-essential-advice (f &rest args)
    "Around advice that invokes F with ARGS if `non-essential' is non-nil."
    (unless non-essential
      (apply f args)))

  ;; Advise lsp-deferred and lsp so that they only run if non-essential is non-nil.
  ;; This prevents lsp-mode from starting during consult previews.
  (advice-add 'lsp :around #'my/if-essential-advice)
  (advice-add 'lsp-deferred :around #'my/if-essential-advice)

  ;; See https://github.com/minad/corfu/wiki#basic-example-configuration-with-orderless.
  (defun my/lsp-mode-init-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  :hook
  (lsp-completion-mode . my/lsp-mode-init-completion)
  :custom
  (lsp-log-io nil)
  (lsp-keymap-prefix "C-c l")
  (lsp-modeline-code-actions-segments '(icon)) ;; No need to also show the count.
  (lsp-lens-enable nil)                        ;; Haven't found a great use for these.
  (lsp-modeline-diagnostics-enable nil)        ;; The Flycheck modeline segment already displays this.

  ;; Recommended setting as I'm using corfu instead of company for completion.
  ;; See: https://github.com/minad/corfu/issues/71#issuecomment-977693717
  (lsp-completion-provider :none)

  ;; Categorise lsp-ui-imenu entries by their type (variable, function, etc).
  (lsp-imenu-index-function 'lsp-imenu-create-categorized-index)

  ;; Close the language server buffer when the last file in the workspace/project is closed.
  (lsp-keep-workspace-alive nil)

  ;; When lsp-eldoc-render-all is set to nil, moving point to a function call should result
  ;; in a one line function signature being displayed in the minibuffer. There is an issue
  ;; with lsp-mode and rust-analyzer however where what gets displayed is not the function
  ;; signature but (usually) the type of struct to which the function belongs. However,
  ;; setting lsp-eldoc-render-all to non-nil is even worse as it often results in a huge block
  ;; of text being displayed that contains too much information. Going to disable for now,
  ;; and use lsp-ui mouse over or lsp-ui-doc-glance to view type information.
  ;; See: https://github.com/emacs-lsp/lsp-mode/issues/2613
  ;; Also: https://github.com/rust-analyzer/rust-analyzer/issues/3415
  (lsp-eldoc-render-all nil)
  (lsp-eldoc-enable-hover nil)

  ;; Disable the displaying of signature help in the minibuffer as it makes the size of the minibuffer
  ;; expand to show all the additional info which is distracting. Signature help can be obtained
  ;; from lsp-ui via mouse hover or calling lsp-ui-doc-glance explicitly.
  (lsp-signature-auto-activate nil)
  (lsp-signature-render-documentation nil)

  ;; The following --query-driver args allow clangd to be used with the compile_commands.json
  ;; generated by bazel-compilation-database. This is due to the fact that, by default,
  ;; clangd expects the driver to be a standard compiler executable (e.g., clang++).
  ;; The log level can be changed to "debug" for additional information.
  (lsp-clients-clangd-args '("--query-driver=**/wrapped_clang"
			     "--background-index"
			     "--log=info"
			     ;; "--clang-tidy"
			     ;; "-j=1"
			     ))

  (lsp-rust-analyzer-cargo-watch-command "clippy")
  ;; I'd prefer to have this off by default but there seem to be some hooks configured where if
  ;; this is going to be set it needs to be set when lsp-mode starts.
  (lsp-rust-analyzer-server-display-inlay-hints t)

  ;; Enable procedural macro support (apparently assists with auto-completion when procedural
  ;; macros are involved; see https://news.ycombinator.com/item?id=28802428).
  ;; (lsp-rust-analyzer-proc-macro-enable t)
  ;; Configure lsp-mode to use the official terraform-ls LSP server rather than terraform-lsp
  ;; which it uses by default and is more experimental (crashes constantly for me).
  (lsp-terraform-server '("terraform-ls" "serve"))

  :config
  (lsp-enable-which-key-integration t)

  ;; Configure custom LSP server settings.
  ;;
  ;; For gpls:
  ;; See: https://github.com/golang/tools/blob/9b5e55b1a7e215a54c9784492d801104a8381a91/gopls/doc/emacs.md#configuring-gopls-via-lsp-mode
  ;; And available settings: https://github.com/golang/tools/blob/9b5e55b1a7e215a54c9784492d801104a8381a91/gopls/doc/settings.md
  ;; Not all experimental settings appear to be documented. For the full list, see the code here:
  ;; https://github.com/golang/tools/blob/9b5e55b1a7e215a54c9784492d801104a8381a91/gopls/internal/lsp/source/options.go
  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ("gopls.staticcheck" t t))))

(use-package lsp-ui
  :bind
  (:map lsp-ui-mode-map
        ("M-p" . lsp-ui-doc-show)
        ("M-P" . lsp-ui-doc-hide))
  :custom
  (lsp-ui-sideline-delay 0.2)
  (lsp-ui-doc-delay 0)
  (lsp-ui-imenu-auto-refresh t)
  (lsp-ui-doc-show-with-mouse nil)
  (lsp-ui-doc-position 'at-point)
  (lsp-ui-doc-max-width 150)
  (lsp-ui-doc-max-height 30))

(use-package consult-lsp
  :custom
  (consult-lsp-marginalia-mode t))

;; The following DAP configuration is optimised for debugging Rust using the LLVM
;; lldb-vscode binary. The easiest way to get the latest version of this binary on MacOS
;; is to install llvm as a brew package. The following configuration expects a launch.json
;; file to exist at the root of the project. The launch.json file should have the format below.
;; Each time a code change is made you'll need to rebuild the debug binary.
;;
;; {
;;   "version": "0.2.0",
;;   "configurations": [{
;;     "name": "lldb-vscode-launch",
;;     "type": "lldb-vscode",
;;     "request": "launch",
;;     "program": "${workspaceFolder}/target/debug/name_of_binary",
;;     "args": [],
;;     "env": {},
;;     "cwd": "${workspaceFolder}",
;;     "stopOnEntry": false,
;;     "debuggerRoot": "${workspaceFolder}"
;;   }]
;; }
;;
;; Note: dap-mode uses `lsp-workspace-root' to determine the root project directory that should
;; contain the launch.json (or .vscode/launch.json) file. In projects with submodules where you
;; want to debug a submodule independently, this can cause problems if `lsp-workspace-root' says
;; that the project root is higher up the directory tree than you want it to be. If you get stuck
;; in this cycle you will probably want to delete or edit ~/.config/emacs/.lsp-session-v1 or
;; find a way to hook into dap-mode/lsp-mode so the process is a bit smarter.
(use-package dap-mode
  :hook
  (dap-stopped . (lambda (arg) (call-interactively #'dap-hydra)))
  :config
  (require 'dap-lldb)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  :custom
  (dap-print-io t)
  (dap-lldb-debug-program '("/opt/homebrew/opt/llvm/bin/lldb-vscode")))

;;;;;; Flycheck

(use-package flycheck
  :defer t
  :hook (prog-mode . flycheck-mode)
  :bind
  (:map flycheck-mode-map
        ("C-c C-c l" . flycheck-list-errors)))

(use-package flycheck-clang-tidy
  :after flycheck
  :hook
  (flycheck-mode . flycheck-clang-tidy-setup))

;;;;;; Rust

(defun my/toggle-rust-inlay-hints ()
  "Toggle whether Rust inlay type hints are shown."
  (interactive)
  (if lsp-rust-analyzer-server-display-inlay-hints
    (progn
      (setq lsp-rust-analyzer-server-display-inlay-hints nil)
      (lsp-rust-analyzer-inlay-hints-mode 0))
    (progn
      (setq lsp-rust-analyzer-server-display-inlay-hints t)
      (lsp-rust-analyzer-inlay-hints-mode 1))))

(use-package rustic
  :init
  ;; Remove the default rustic keybindings.
  (setq rustic-mode-map (make-sparse-keymap))
  :bind
  (:map rustic-mode-map
        ("C-c C-c b" . rustic-cargo-build)
        ;; TODO: Fix Cargo C-c C-c c x prefixes below and then enable `compile'.
        ;; ("C-c C-c c" . compile)
        ("C-c C-c f" . rustic-format-buffer)
        ("C-c C-c F" . rustic-cargo-fmt)
        ("C-c C-c l" . rustic-cargo-clippy)
        ("C-c C-c r" . rustic-cargo-run)
        ("C-c C-c t" . rustic-cargo-test)
        ("C-c C-c T" . rustic-cargo-current-test)
        ("C-c C-c i" . my/toggle-rust-inlay-hints) ;; Toggle inlay type hints.
        ("C-c C-c o" . lsp-rust-analyzer-open-external-docs)
        ("C-c C-c c a" . rustic-cargo-add)
        ("C-c C-c c c" . rustic-cargo-clean)
        ("C-c C-c c o" . rustic-cargo-outdated)
        ("C-c C-c c u" . rustic-cargo-upgrade)
        ("C-c C-c d d" . dap-debug)
        ("C-c C-c d l" . dap-debug-last)
        ("C-c C-c d m" . dap-hydra)))

;;;;;; Terraform

(use-package terraform-mode
  :hook
  (hcl-mode . terraform-format-on-save-mode)
  (hcl-mode . (lambda () (setq comment-start "//"))) ; I prefer the // syntax to the default # comments.
  (hcl-mode . (lambda () (setq-local indent-line-function 'my/hcl-indent-line))))

;; This function is a modified version of hcl-indent-line in an attempt
;; to fix https://github.com/purcell/emacs-hcl-mode/issues/7.
(defun my/hcl-indent-line ()
  "Indent current line as HCL configuration."
  (interactive)
  (let* ((curpoint (point))
         (pos (- (point-max) curpoint)))
    (back-to-indentation)
    (if (hcl--in-string-or-comment-p)
        (goto-char curpoint)
      (let ((block-indentation (hcl--block-indentation)))
        (if block-indentation
            (if (looking-at "[]}]")
                (my/hcl--maybe-indent-line block-indentation)
              (my/hcl--maybe-indent-line (+ block-indentation hcl-indent-level)))
          (my/hcl--maybe-indent-line (hcl--previous-indentation)))
        (when (> (- (point-max) pos) (point))
          (goto-char (- (point-max) pos)))))))

(defun my/hcl--maybe-indent-line (column)
  "Indent current line to COLUMN if required."
  (let ((curcol (- (point) (line-beginning-position))))
    (unless (= curcol column)
      (delete-region (line-beginning-position) (point))
      (indent-to column))))

;;;;;; Python

(use-package python
  :straight (:type built-in)
  :init
  (setq python-shell-interpreter "python3"))

;;;;;; JavaScript

(use-package js2-mode
  :mode "\\.js\\'")

;;;;;; TypeScript

;; Just basic syntax highlighting for now. God forbid I ever have to write TS.
(use-package typescript-mode)

;;;;;; Rego

(use-package rego-mode)

;;;;;; Docker

(use-package dockerfile-mode)

;;;;;; Shell

;; Shell scripting indentation
(setq sh-basic-offset 2
      sh-indentation 2)
(setq-default indent-tabs-mode nil)

;;;;;; Go

(use-package go-mode
  :init
  ;; Remove the default go-mode keybindings.
  (setq go-mode-map (make-sparse-keymap))
  :hook
  ((go-mode . (lambda () (setq tab-width 4)))
   (before-save . gofmt-before-save))
  :bind
  (:map go-mode-map
        ("C-c C-c p" . go-play-buffer)
        ("C-c C-c P" . go-play-region)
        ("C-c C-c c" . compile))
  :custom
  ;; Replace gofmt with goimports which also removes/adds imports as
  ;; needed. See: https://pkg.go.dev/golang.org/x/tools/cmd/goimports.
  (gofmt-command "goimports"))

;; TODO: Implement inlay hints for gopls when possible. See:
;; https://github.com/emacs-lsp/lsp-mode/issues/3618.

;;;;;; Bazel

(use-package bazel-mode
  :straight '(bazel-mode :host github
			 :repo "bazelbuild/emacs-bazel-mode")
  :mode "\\.BUILD\\'"
  :init
  ;; Remove the default Bazel keybindings.
  (setq bazel-mode-map (make-sparse-keymap))
  :bind
  (:map bazel-mode-map
        ("C-c C-c b" . bazel-build)
        ("C-c C-c f" . bazel-buildifier)
        ("C-c C-c r" . bazel-run)
        ("C-c C-c t" . bazel-test)
        ("C-c C-c T" . bazel-coverage))
  :custom
  (bazel-buildifier-before-save t))

;;;;;; C/C++

(defun my/compile-no-ask ()
  "Run `compile' without prompting for confirmation of the command."
  (interactive)
  (setq-local compilation-read-command nil)
  (call-interactively 'compile))

(use-package cc-mode
  :bind (:map c++-mode-map
	      ("C-c C-c f" . clang-format-buffer)
	      ("C-c C-c F" . clang-format-region)
	      ("C-c C-c c" . my/compile-no-ask)))

(use-package clang-format
  :commands clang-format clang-format-buffer clang-format-region
  :config
  (setq clang-format-fallback-style "llvm")) ;; Where is this defined?

;;;;;; YAML

(use-package yaml-mode
  :mode (("\\.yml\\'" . yaml-mode)
	 ("\\.yaml\\'" . yaml-mode))
  :hook ((yaml-mode . (lambda () (define-key yaml-mode-map "\C-m" 'newline-and-indent)))))

;;;;;; Jsonnet

(use-package jsonnet-mode)

;;;;;; Lisp

(use-package rainbow-delimiters
  :init (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

(global-set-key (kbd "C-x C-r") 'eval-region)

;;;;;; SGML/HTML

;; The following configuration needs to be done on the built-in sgml-mode rather than
;; html-mode as the latter is not actually a package and use-package will complain.
(use-package sgml-mode
  :straight nil ;; Built-in.
  :bind
  (:map html-mode-map
        ;; Unbind M-o as I want that for ace-window.
        (("M-o" . nil))))

;;;;;; Markdown

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;;;; Git

(use-package magit
  :bind
  (("C-c g s" . magit-status)
   ("C-c g d" . magit-dispatch)
   ("C-c g f" . magit-file-dispatch))
  :custom
  ;; Tell Magit not to add the C-x bindings as I like to keep C-x reserved
  ;; for Emacs native commands.
  (magit-define-global-key-bindings nil)
  ;; Otherwise Magit shows a read-only diff screen when you press C-c C-c and
  ;; you've already had a chance to look at the diff when you stage the files.
  (magit-commit-show-diff nil)
  :config
  ;; Disable as I get errors about `project-switch-commands' being nil.
  (setq magit-bind-magit-project-status nil))

;;;; Terminal

(use-package vterm
  :bind
  (:map vterm-mode-map
        ;; Unbind C-s which sends a stop signal to the terminal which freezes
        ;; output (and requires a C-q to unfreeze) as it's annoying.
        ("C-s" . nil)
        ;; Unbind C-SPC as otherwise it prevents pop-global-mark from working across
        ;; vterm buffers.
        ("C-SPC" . nil)
        ;; Unbind M-s so we can use ripgrep, etc.
        ("M-s" . nil)
        ;; Unbind F11 as this is used to fullscreen the window.
        ("<f11>" . nil)
        ;; Unbind M-: so we can have eval-expression back.
        ("M-:" . nil)
        ;; Bind S-ESC to keyboard-escape-quit when in vterm-mode as ESC is used for
        ;; shell signals (e.g., ESC + underscore for last word of the previous command).
        ("S-<escape>" . keyboard-escape-quit)
        ;; Vterm yasnippet integration.
        ("C-c y i" . my/vterm-consult-yasnippet))
  :custom
  ;; Don't prompt for permission to compile on first install.
  (vterm-always-compile-module t)
  (vterm-max-scrollback 10000)
  ;; I can't seem to get a reliable way of clearing without losing buffer history.
  ;; The below setting (which is already the default) should work but doesn't.
  (vterm-clear-scrollback-when-clearing nil)
  (vterm-buffer-name-string "vterm: %s"))

(use-package multi-vterm
  :bind
  (:map global-map
	("C-c v v" . multi-vterm)
	("C-c v n" . multi-vterm-next)
	("C-c v p" . multi-vterm-project)
	("C-c v b" . multi-vterm-rename-buffer)))

(define-derived-mode vterm-phony-mode text-mode "VTerm-Phony"
  "Major mode used to run yasnippet expansion in terminals.")

;; This function allows `consult-yasnippet' to be called from a vterm buffer.
;; Oridinarly, these yasnippet functions can't be called from vterm as Emacs
;; considers it a read-only buffer. This function creates a new temporary
;; `vterm-phony-mode' buffer, executes `consult-yasnippet' there, and then
;; copies the result into the vterm buffer.
;; See also: https://mullikine.github.io/posts/use-yasnippets-in-term/
(defun my/vterm-consult-yasnippet ()
  "Run `consult-yasnippet' for the current vterm buffer."
  (interactive)
  (if (eq major-mode 'vterm-mode)
      (let ((expansion))
        (let ((b (generate-new-buffer (generate-new-buffer-name "yas-temp"))))
          (with-current-buffer b
            (vterm-phony-mode)
            (consult-yasnippet nil)
            (setq expansion (buffer-string)))
          (kill-buffer b))
        ;; Insert the expanded snippet.
        (vterm-insert expansion)
        ;; Send a key to unselect the region (must be a better way to do this).
        (vterm-send-key "<right>"))))

;;;; Org Mode

(defvar my/gtd-dir "~/dev/home/gtd")
(defvar my/pkm-dir "~/dev/home/pkm")

(defun my/org-mode-init ()
  "Org-mode init function that should be attached to `org-mode-hook`."
  (interactive)
  ;;; Org-mode buffer local settings.
  (org-indent-mode 1)
  (visual-line-mode 1)
  (display-line-numbers-mode 0))

(defvar my/org-todo-sort-order '("PROG" "NEXT" "TODO" "HOLD" "DONE"))

(defun my/org-agenda-cmp-todo (a b)
  "Custom compares agenda items A and B based on their todo keywords."
  (when-let ((state-a (get-text-property 14 'todo-state a))
             (state-b (get-text-property 14 'todo-state b))
             (cmp (--map (cl-position-if (lambda (x)
                                           (equal x it))
                                         my/org-todo-sort-order)
                         (list state-a state-b))))
    (cond ((apply '> cmp) 1)
          ((apply '< cmp) -1)
          (t nil))))

(use-package org
  :hook
  (org-mode . my/org-mode-init)
  ;; Override '?' key to show helpful which-key display.
  (org-agenda-mode . (lambda () (define-key org-agenda-mode-map "?" 'which-key-show-major-mode)))

  :bind
  ;; Below produces errors that look like org-agenda-mode-map isn't in scope sometimes.
  ;; Unsure why this doesn't work when the mapping for org-mode-map does work. The above lambda
  ;; is a workaround. Will try to clean up at some point.
  ;; (:map org-agenda-mode-map
  ;; (("?" . which-key-show-full-major-mode)
  (:map org-mode-map
        ("C-c C-S-l" . org-cliplink))

  :config
  ;; Always save all org buffers before quitting the agenda (press 's' to save immediately).
  (advice-add 'org-agenda-quit :before 'org-save-all-org-buffers)

  ;; Make it easier to create org-babel code blocks.
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))

  :custom
  (org-agenda-cmp-user-defined 'my/org-agenda-cmp-todo)
  (org-agenda-custom-commands
   `(("d" . "Dashboards") ;; Creates command prefix "d".
     ("da" "All Tasks"
      ((alltodo
	""
	((org-agenda-overriding-header "Inbox")
	 (org-agenda-files '(,(concat my/gtd-dir "/inbox.org")))))
       (alltodo
        ""
        ((org-agenda-overriding-header "Work")
         (org-agenda-files '(,(concat my/gtd-dir "/work.org")))
         (org-agenda-sorting-strategy '(user-defined-up priority-down))))
       (alltodo
        ""
        ((org-agenda-overriding-header "Personal")
         (org-agenda-files '(,(concat my/gtd-dir "/personal.org")))
         (org-agenda-sorting-strategy '(user-defined-up priority-down))))))
     ("dp" "Personal Tasks"
      ((todo
	"PROG"
        ((org-agenda-overriding-header "Progress")
	 (org-agenda-files '(,(concat my/gtd-dir "/personal.org")))
	 (org-agenda-sorting-strategy '(priority-down))))
       (todo
	"NEXT"
        ((org-agenda-overriding-header "Next")
	 (org-agenda-files '(,(concat my/gtd-dir "/personal.org")))
	 (org-agenda-sorting-strategy '(priority-down))))
       (todo
        "HOLD"
        ((org-agenda-overriding-header "Hold")
	 (org-agenda-files '(,(concat my/gtd-dir "/personal.org")))
	 (org-agenda-sorting-strategy '(priority-down))))
       (todo
        "TODO"
        ((org-agenda-overriding-header "Backlog")
	 (org-agenda-files '(,(concat my/gtd-dir "/personal.org")))
	 (org-agenda-sorting-strategy '(priority-down))))
       (alltodo
	""
	((org-agenda-overriding-header "Inbox")
	 (org-agenda-files '(,(concat my/gtd-dir "/inbox.org"))))))
      ((org-agenda-tag-filter-preset '("-@work"))))
     ("dw" "Work Tasks"
      ((todo
	"PROG"
        ((org-agenda-overriding-header "Progress")
	 (org-agenda-files '(,(concat my/gtd-dir "/work.org")))
	 (org-agenda-sorting-strategy '(priority-down))))
       (todo
	"NEXT"
        ((org-agenda-overriding-header "Next")
	 (org-agenda-files '(,(concat my/gtd-dir "/work.org")))
	 (org-agenda-sorting-strategy '(priority-down))))
       (todo
	"HOLD"
        ((org-agenda-overriding-header "Hold")
	 (org-agenda-files '(,(concat my/gtd-dir "/work.org")))
	 (org-agenda-sorting-strategy '(priority-down))))
       (todo
	"TODO"
        ((org-agenda-overriding-header "Backlog")
	 (org-agenda-files '(,(concat my/gtd-dir "/work.org")))
	 (org-agenda-sorting-strategy '(priority-down))))
       (alltodo
	""
	((org-agenda-overriding-header "Inbox")
	 (org-agenda-files '(,(concat my/gtd-dir "/inbox.org"))))))
      ((org-agenda-tag-filter-preset '("-@personal"))))))
  (org-agenda-files
   (list
    (concat my/gtd-dir "/inbox.org")
    (concat my/gtd-dir "/personal.org")
    (concat my/gtd-dir "/work.org")
    (concat my/gtd-dir "/recurring.org")))
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
  (org-blank-before-new-entry '((heading . nil)
                                (plain-list-item . nil)))
  (org-capture-templates `(("i" "Inbox" entry
                            (file+headline ,(concat my/gtd-dir "/inbox.org") "Inbox")
			    "* TODO %i%?")
			   ("b" "Bookmark" entry
			    (file+olp+datetree ,(concat my/gtd-dir "/bookmarks.org") "Bookmarks")
			    "* %(org-cliplink-capture)%?\n")
                           ("c" "Coffee Journal" entry
			    (file+olp+datetree ,(concat my/gtd-dir "/coffee.org") "Coffee Journal" "Log")
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
  (org-default-notes-file (concat my/gtd-dir "/inbox.org"))
  (org-directory my/gtd-dir)
  (org-ellipsis " 》")
  (org-fontify-done-headline t)
  (org-fontify-quote-and-verse-blocks t)
  (org-hide-emphasis-markers t)
  (org-log-done 'time)
  ;; Leaving drawer logging disabled for now as I don't like the format of the log items,
  ;; and I want to know when a task was created which doesn't happen without what apears
  ;; to be quite a bit of custom code.
  (org-log-into-drawer nil)
  (org-log-states-order-reversed nil) ; Make newest last
  (org-outline-path-complete-in-steps nil)
  (org-pretty-entities t)
  (org-priority-default org-priority-lowest)
  (org-refile-targets
   `((,(concat my/gtd-dir "/archive.org") :regexp . "Ongoing")
     (,(concat my/gtd-dir "/inbox.org") :level . 1)
     (,(concat my/gtd-dir "/personal.org") :regexp . "Tasks") ;; This seems to work nicer.
     (,(concat my/gtd-dir "/work.org") :regexp . "Tasks")
     (,(concat my/gtd-dir "/someday.org") :regexp . "Tasks") ;; TODO: Make it possible to refile into Projects or Tasks.
     (,(concat my/gtd-dir "/recurring.org") :level . 2)))
  ;; The following two settings are required to make org-refile show the full heading path
  ;; to subheading refile candidates. Took a while to get this working properly.
  (org-refile-use-outline-path t)
  (org-special-ctrl-a/e t)
  (org-tags-column 0)
  (org-todo-keywords
   `((sequence "TODO(t)" "NEXT(n)" "PROG(p)" "HOLD(h)" "|" "DONE(d)")))
  ;; See colours here: https://alexschroeder.ch/geocities/kensanata/colors.html
  (org-todo-keyword-faces
   `(("TODO" . (:foreground "DodgerBlue2" :weight bold))
     ("NEXT" . (:foreground "hot pink" :weight bold))
     ("PROG" . (:foreground "CadetBlue1" :weight bold))
     ("HOLD" . (:foreground "orange1" :weight bold))
     ("DONE" . (:foreground "orange red" :weight bold))))
  (org-use-fast-todo-selection 'expert)
  (org-use-sub-superscripts nil))

(use-package org-cliplink)

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org-roam
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n g" . org-roam-graph)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n c" . org-roam-capture)
   ("C-c n t" . org-roam-tag-add)
   ;; Org-roam daily jounal keybindings.
   ("C-c j ." . org-roam-dailies-find-directory)
   ("C-c j j" . org-roam-dailies-capture-today)
   ("C-c j J" . org-roam-dailies-goto-today)
   ("C-c j y" . org-roam-dailies-capture-yesterday)
   ("C-c j Y" . org-roam-dailies-goto-yesterday)
   ("C-c j d" . org-roam-dailies-capture-date)
   ("C-c j D" . org-roam-dailies-goto-date))
  :config
  (require 'org-roam-protocol)
  (require 'org-roam-dailies)
  (org-roam-db-autosync-mode 1)
  :custom
  (org-roam-directory (concat my/pkm-dir "/notes"))
  (org-roam-db-location (concat my/xdg-cache-dir "/emacs/org-roam.db"))
  (org-roam-completion-everywhere t)
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
                             ("Today"))))))

(global-set-key (kbd "C-c o l") 'org-store-link)
(global-set-key (kbd "C-c o a") 'org-agenda)
(global-set-key (kbd "C-c o m") 'org-capture)
(global-set-key (kbd "C-c o i") (lambda () (interactive) (org-capture nil "i"))) ;; Capture inbox item.
(global-set-key (kbd "C-c o b") (lambda () (interactive) (org-capture nil "b"))) ;; Capture bookmark.
(global-set-key (kbd "C-c o c") (lambda () (interactive) (org-capture nil "c"))) ;; Capture coffee log.
(global-set-key (kbd "C-c C-o") 'org-open-at-point-global) ;; Open links everywhere just like in org-mode.

;;;; Credential Management

(use-package pass)

(use-package auth-source-pass
  :custom
  (auth-source-do-cache nil)
  :config
  (auth-source-pass-enable))

;;;; Kubernetes
(use-package kubernetes
  :bind
  (:map global-map
	("C-c k" . kubernetes-overview))
  ;; By default, '?' will call `kubernetes-dispatch' which shows most of the
  ;; available shortcuts but not all of them. Bind `which-key-show-major-mode'
  ;; so that we can easily see all available shortcuts.
  (:map kubernetes-mode-map
        ("M-?" . which-key-show-major-mode))
  :custom
  ;; The following frequency settings effectively disables auto-refresh which
  ;; can improve performance. Just press 'g' to refresh instead.
  (kubernetes-poll-frequency 3600)
  (kubernetes-redraw-frequency 3600))

;;;; Miscellaneous

;; Function for starting "An Introduction to Programming in Emacs Lisp" which is no longer
;; shown on the main info page (C-h i) and I can never remember how else to get to it.
(defun my/emacs-lisp-intro ()
  "Opens \"An Introduction to Programming in Emacs Lisp\" by Robert J. Chassell."
  (interactive)
  (info "(eintr) Top"))

;;; End:
(provide 'init)
;;; init.el ends here
