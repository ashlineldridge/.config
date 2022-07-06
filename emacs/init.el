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

;; Revert Dired and other buffers.
(setq global-auto-revert-non-file-buffers t)

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

;;;; Appearance

;;;;; General Appearance

;; Do not show the startup screen.
(setq inhibit-startup-message t)

(tool-bar-mode 0)    ;; Disable the tool bar.
(tooltip-mode 0)     ;; Disable tool tips.
(menu-bar-mode 0)    ;; Disable the menu bar.
(scroll-bar-mode 0)  ;; Disable visible scrollbar.
(set-fringe-mode 10) ;; Increase left/right margins slightly.

;; Make frame size bigger.
(set-frame-size (selected-frame) 110 50)

;; Flash the mode line rather than use an audible bell.
(setq visible-bell nil)
(setq ring-bell-function (lambda ()
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil 'invert-face 'mode-line)))

;; This setting makes the Emacs window play nicer with window managers like yabai.
(setq frame-resize-pixelwise t)

;;;;; Line and Column Numbers

;; Show column numbers in the mode line.
(column-number-mode 1)

;; Enable line numbers for some modes.
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

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
  (modus-themes-headings
   '((1 . (variable-pitch rainbow background 1.3))
     (2 . (variable-pitch rainbow background semibold 1.2))
     (3 . (variable-pitch rainbow background semibold 1.1))
     (t . (variable-pitch rainbow semilight 1.1)))))

(use-package doom-themes
  :disabled
  :config
  (doom-themes-visual-bell-config))

(use-package zenburn-theme
  :disabled)

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

;;;;; Fonts

;; Font-related variable definitions.
(defvar my/default-fixed-font "Iosevka SS14") ;; "Jetbrains Mono" and "Cantarell"" are also good ones.
(defvar my/default-variable-font "Iosevka Aile") ;; ETBembo is another good one.
(defvar my/default-fixed-font-size 144)
(defvar my/default-variable-font-size 144)

;; Set the default face.
(set-face-attribute 'default nil
                    :font my/default-fixed-font
                    :height my/default-fixed-font-size
                    :width 'normal
                    :weight 'normal)

;; Set the fixed pitch face.
(set-face-attribute 'fixed-pitch nil
                    :font my/default-fixed-font
                    :height my/default-fixed-font-size
                    :width 'normal
                    :weight 'normal)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil
                    :font my/default-variable-font
                    :height my/default-variable-font-size
                    :width 'normal
                    :weight 'normal)

;;;;; Icons

(use-package all-the-icons
  :config
  ;; Only install the fonts if they are not already installed. See:
  ;; https://github.com/domtronn/all-the-icons.el/issues/120#issuecomment-480342779
  (unless (member "all-the-icons" (font-family-list))
      (all-the-icons-install-fonts t)))

;;;;; Mode-Line

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-height 15)
  (doom-modeline-bar-width 4)
  (doom-modeline-lsp t)
  (doom-modeline-github nil)
  (doom-modeline-mu4e nil)
  (doom-modeline-irc nil)
  (doom-modeline-minor-modes t)
  (doom-modeline-persp-name nil)
  (doom-modeline-buffer-file-name-style 'truncate-except-project)
  (doom-modeline-major-mode-icon nil))

(use-package minions
  :hook (doom-modeline-mode . minions-mode))

;;;; Window Management

;;;;; Window Selection with Ace Window

(use-package ace-window
  :bind
  (("M-o" . ace-window)
   ("M-O" . ace-delete-window))
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;;;;; Jump Between Windows

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
  ("k l"     (my/delete-window 'left)    "delete-window-left" :exit t)
  ("k r"     (my/delete-window 'right)   "delete-window-right" :exit t)
  ("k a"     (my/delete-window 'above)   "delete-window-above" :exit t)
  ("k b"     (my/delete-window 'below)   "delete-window-below" :exit t)
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
		  "SHELL" "GNUPGHOME" "PASSWORD_STORE_DIR" "DEVELOPER_DIR" "SDKROOT"))
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
  (which-key-show-early-on-C-h t)
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
  ;; Modes which shouldn't use cofu. Org-mode is excluded as the only completions that
  ;; get shown are org-roam note names which is pretty useless.
  (corfu-excluded-modes '(org-mode)))

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

(defun my/consult-project-file (selected-root)
  "Create a view for selecting project files for the project at SELECTED-ROOT.
This function is adapted from consult-project-extra--file so that we can patch
in the :preview-key; otherwise, selection of the project file to open will
automatically display the preview which we don't want. Unfortunately, it
doesn't appear possible to achieve this behaviour using consult-customize."
  (find-file (consult--read
              (consult-project-extra--project-files selected-root)
              :prompt "Project File: "
              :sort t
              :require-match t
              :preview-key (kbd "M-.")
              :category 'file
              :state (consult--file-preview)
              :history 'file-name-history)))

(use-package consult
  :straight '(consult-mode :host github :repo "minad/consult")
  :bind
  (("C-s"     . consult-line)
   ("M-s r"   . consult-ripgrep)
   ("M-s i i" . consult-imenu)       ; Local buffer imenu
   ("M-s i o" . consult-imenu-multi) ; Open buffers imenu
   ("C-x b"   . consult-buffer)
   ("M-g M-g" . consult-goto-line)
   ("M-y"     . consult-yank-pop)
   ("C-x r r" . consult-register)
   ("C-x r l" . consult-register-load)
   ("C-x r s" . consult-register-store))

  :init
  ;; TODO: Review example consult config https://github.com/minad/consult

  ;; Use consult to select xref locations with preview.
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  ;; Load consult-project-extra as we need some of its sources below.
  (use-package consult-project-extra
    :straight '(consult-project-extra :host github :repo "Qkessler/consult-project-extra"))

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
   consult-project-extra--source-file
   :name "Project File" :narrow ?f :state 'consult--file-state
   consult--source-recent-file
   :name "Recent File" :narrow ?r
   consult-project-extra--source-project
   :name "Switch Project" :narrow ?j :action 'my/consult-project-file

   ;; By default, consult will automatically preview all commands and sources. Below
   ;; we customize certain commands/sources so that the preview is shown on request.
   consult-ripgrep
   consult-git-grep
   consult-grep
   consult-bookmark
   consult-recent-file
   consult-xref
   consult--source-bookmark
   consult--source-recent-file
   consult-project-extra--source-file
   consult-project-extra--source-project
   :preview-key (kbd "M-."))

  ;; Customise the list of sources shown by consult-buffer.
  (setq consult-buffer-sources
        '(consult--source-buffer                ; Narrow: ?b
          consult--source-project-buffer        ; Narrow: ?p
          my/consult-source-vterm-buffer        ; Narrow: ?v
          consult-project-extra--source-file    ; Narrow: ?f
          consult--source-recent-file           ; Narrow: ?r
          consult--source-bookmark              ; Narrow: ?m
          consult-project-extra--source-project ; Narrow: ?j
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
                         (?c "Constants")))))

(use-package embark
  ;; Load after xref so that the overidden keybinding below takes effect.
  :after xref
  :bind
  (("C-." . embark-act)))

(use-package embark-consult
  :after (embark consult))

(use-package wgrep
  :bind
  (("C-c C-w" . wgrep-change-to-wgrep-mode))
  :custom
  (wgrep-auto-save-buffer t))

;;;; General Editing

;;;;; Whitespace

(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

;; Is there a better way/place to do this?
(setq js-indent-level 2)

;; Mostly useful for highlighting whitespace characters.
(use-package highlight-chars)

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

;;;;; Project Management

(use-package project
  :straight nil ;; Built-in.
  :bind (("C-x p u" . my/project-refresh-list))
  :custom
  (project-list-file (expand-file-name "projects" my/emacs-cache-dir))
  :config
  (project-remember-projects-under "~/dev/home")
  (project-remember-projects-under "~/dev/work"))

(use-package project-x
  :straight '(project-x-mode-mode :host github :repo "karthink/project-x")
  :after project
  :custom
  (project-x-window-list-file (expand-file-name "project-window-list" my/emacs-cache-dir))
  :config
  (project-x-mode 1))

(defun my/project-root ()
  "Return the root directory of the current or nil."
  (if-let* ((proj (project-current)))
      (project-root proj)))

(defun my/project-refresh-list ()
  "Refresh list of known projects."
  (interactive)
  (project-forget-zombie-projects)
  (project-remember-projects-under "~/dev/home")
  (project-remember-projects-under "~/dev/work"))

;;;;; File Browsing

(use-package neotree
  :custom
  (neo-theme 'nerd)
  (neo-smart-open t)
  (neo-window-fixed-size nil)
  (neo-toggle-window-keep-p t)
  :bind
  (:map global-map
	("C-c t t" . my/neotree-toggle)
	("C-c t r" . my/neotree-refresh)))

(defun my/neotree-refresh ()
  "Refresh the Neotree window (if open) to navigate to the current file/project."
  (interactive)
  (if (neo-global--window-exists-p)
      (let ((project-dir (my/project-root))
            (file-name (buffer-file-name))
	    (cw (selected-window)))
	(if project-dir
	    (progn
	      (neotree-dir project-dir)
	      (neotree-find file-name)
	      (select-window cw))))))

(defun my/neotree-toggle ()
  "Toggle Neotree and navigates to the current file/project."
  (interactive)
  (neotree-toggle)
  (my/neotree-refresh))

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
  :hook
  (emacs-lisp-mode . my/outline-minor-mode-safe)
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

(global-set-key (kbd "<f10>") 'my/outline-minor-mode-safe)

;;;;; Code Templating

(use-package yasnippet
  :hook
  (prog-mode . yas-minor-mode)
  :init
  ;; Remove default key bindings which are crap.
  (setq yas-minor-mode-map (make-sparse-keymap))
  :bind
  (:map yas-minor-mode-map
        ("C-c y y" . yas-expand)
	("C-c y i" . yas-insert-snippet)
	("C-c y f" . yas-visit-snippet-file)
  	("C-c y c" . yas-new-snippet))
  :config
  (setq
   yas-verbosity 1
   yas-wrap-around-region t)
  (yas-reload-all))

(use-package yasnippet-snippets
  :after yasnippet)

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

  (lsp-imenu-index-function 'lsp-imenu-create-categorized-index)

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
  (lsp-enable-which-key-integration t))

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
  ;; Remove the default rustic keybindings (C-c C-c x) in favour of C-c r x.
  (setq rustic-mode-map (make-sparse-keymap))
  :bind
  (:map rustic-mode-map
        ("C-c r b" . rustic-cargo-build)
        ("C-c r f" . rustic-format-buffer)
        ("C-c r F" . rustic-cargo-fmt)
        ("C-c r l" . rustic-cargo-clippy)
        ("C-c r r" . rustic-cargo-run)
        ("C-c r t" . rustic-cargo-test)
        ("C-c r T" . rustic-cargo-current-test)
        ("C-c r i" . my/toggle-rust-inlay-hints) ;; Toggle inlay type hints.
        ("C-c r o" . lsp-rust-analyzer-open-external-docs)
        ("C-c r c a" . rustic-cargo-add)
        ("C-c r c c" . rustic-cargo-clean)
        ("C-c r c o" . rustic-cargo-outdated)
        ("C-c r c u" . rustic-cargo-upgrade)
        ("C-c r d d" . dap-debug)
        ("C-c r d l" . dap-debug-last)
        ("C-c r d m" . dap-hydra)))

;;;;;; Terraform

(use-package terraform-mode
  ; I prefer the // syntax to the default # comments.
  :hook
  (terraform-mode . terraform-format-on-save-mode)
  (terraform-mode . (lambda () (setq comment-start "//")))
  (terraform-mode . (lambda () (setq-local indent-line-function 'my/hcl-indent-line))))

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

;;;;;; Docker

(use-package dockerfile-mode)

;;;;;; Shell

;; Shell scripting indentation
(setq sh-basic-offset 2
      sh-indentation 2)
(setq-default indent-tabs-mode nil)

;;;;;; Go

(use-package go-mode
  :mode "\\.go\\'"
  ;; Is this the best way to do this?
  :hook
  ((before-save . gofmt-before-save)
   (go-mode . (lambda () (setq tab-width 4))))

  ;; :config
  ;; TODO: setup go-mode + integrations https://github.com/dominikh/go-mode.el
  ;; TODO: Consider gopls settings here: https://github.com/golang/tools/blob/master/gopls/doc/emacs.md#configuring-gopls-via-lsp-mode
  )

;;;;;; Bazel

(use-package bazel-mode
  :straight '(bazel-mode :host github
			 :repo "bazelbuild/emacs-bazel-mode")
  :mode "\\.BUILD\\'")

;;;;;; C/C++

(defun my/compile ()
  "Grabbed from https://github.com/rigtorp/dotemacs/blob/master/init.el."
  (interactive)
  (setq-local compilation-read-command nil)
  (call-interactively 'compile))

(use-package cc-mode
  :bind (:map c++-mode-map
	      ("C-c f b" . clang-format-buffer)
	      ("C-c f r" . clang-format-region)
	      ;; ("M-p" . company-complete-common)
	      ("C-c c" . my/compile)))

(use-package clang-format
  :commands clang-format clang-format-buffer clang-format-region
  :config
  (setq clang-format-fallback-style "llvm")) ;; Where is this defined?

;;;;;; YAML

(use-package yaml-mode
  :mode (("\\.yml\\'" . yaml-mode)
	 ("\\.yaml\\'" . yaml-mode))
  :hook ((yaml-mode . (lambda () (define-key yaml-mode-map "\C-m" 'newline-and-indent)))))

;;;;;; Lisp

(use-package rainbow-delimiters
  :init (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

;; See example configurations here:
;; - https://www.reddit.com/r/emacs/comments/adkj95/smartparens_configuration_for_elisp_and_other/
;; - https://gist.github.com/oantolin/5751fbaa7b8ab4f9570893f2adfe1862
;; - https://alexpeits.github.io/emacs.d/#org687ce2c
(use-package smartparens
  ;; Giving global mode a try.
  ;; :hook (prog-mode . smartparens-mode)
  :init
  (smartparens-global-mode)
  :config
  (require 'smartparens-config))

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

(use-package forge
  :after magit
  :custom
  (forge-database-file (concat my/xdg-cache-dir "/emacs/forge-database.sqlite")))

;;;; Email

;; Requires initial mu init command of:
;; mu init --maildir=~/Mail --my-address=aeldridge@fastmail.com --my-address=ashlin.eldridge@gmail.com
;;
;; Credentials are stored as app passwords using password-store (i.e., `pass`). For SMTP
;; credentials, the password should be named after the host (e.g., "smtp.gmail.com") and
;; the contents of the secret should specify the user and port as additional fields following
;; the password. E.g.:
;;
;; my-secret-password
;; user: aeldridge@fastmail.com
;; port: 465
;;
;; You can probably also include these details in the secret name itself using the pass notation,
;; but with the extra '@' in the username it starts getting ugly.

(use-package mu4e
  ;; Use the version of mu4e packaged with mu
  :straight
  ;; The mu installation comes prepackaged with its Lisp files and it seems better to use these
  ;; rather than creating the possibility for a version mismatch between an mu4e package pulled
  ;; from GitHub or Melpa and the mu binary. See https://github.com/raxod502/straight.el/issues/491.
  (:local-repo "/opt/homebrew/opt/mu/share/emacs/site-lisp/mu/mu4e" :pre-build ())
  :bind
  ;; Take back '?' for which-key and use '@' for marking unread.
  (:map mu4e-headers-mode-map
	("@" . mu4e-headers-mark-for-unread)
        ("?" . which-key-show-major-mode))
  (:map mu4e-view-mode-map
	("@" . mu4e-view-mark-for-unread)
        ("?" . which-key-show-major-mode))
  :custom
  (mu4e-update-interval (* 10 60))
  (mu4e-get-mail-command (concat "mbsync -a -c " my/xdg-config-dir "/isync/mbsyncrc"))
  (mu4e-maildir "~/Mail")
  (mu4e-view-show-images t)
  (mu4e-view-show-addresses 't)

  ;; Make sure that moving a message (like to Trash) causes the
  ;; message to get a new file name.  This helps to avoid the
  ;; dreaded "UID is N beyond highest assigned" error.
  ;; See this link for more info: https://stackoverflow.com/a/43461973
  (mu4e-change-filenames-when-moving t)

  ;; Don't keep message buffers around.
  (message-kill-buffer-on-exit t)

  ;; Context policies. Contexts are defined below in :config.
  (mu4e-context-policy 'pick-first)
  (mu4e-compose-context-policy 'always-ask)

  (mu4e-compose-dont-reply-to-self t)
  (mu4e-compose-format-flowed t)

  (mu4e-maildir-shortcuts
   '(("/Fastmail/Inbox" . ?f)
     ("/Gmail/Inbox"    . ?g)))

  (mu4e-bookmarks
   '((:name "Unread"
	    :query "flag:unread AND NOT \
                    flag:trashed AND NOT \
                    m:\"/Gmail/[Google Mail]/All Mail\" AND NOT \
                    m:\"/Gmail/[Google Mail]/Spam\" AND NOT \
                    m:\"/Gmail/[Google Mail]/Bin\" AND NOT \
                    m:/Fastmail/Spam AND NOT \
                    m:/Fastmail/Trash"
	    :key ?u)
     (:name "Today"
	    :query "date:today..now"
	    :key ?t)
     (:name "Last 7 days"
	    :query "date:7d..now"
	    :key ?w)))

  ;; Override view actions to remove unused actions and add an option to
  ;; to view the email in a browser - handy for complicated HTML emails.
  (mu4e-view-actions
   '(("capture message" . mu4e-action-capture-message)
     ("show this thread" . mu4e-action-show-thread)
     ("view in browser" . mu4e-action-view-in-browser)))

  :config
  ;; Use mu4e for sending e-mail.
  (setq mail-user-agent 'mu4e-user-agent)
  (setq message-send-mail-function 'smtpmail-send-it)

  ;; Configure email contexts. The variable mu4e-contexts is not defined as a custom variable
  ;; so cannot be configured above in the :custom section. When it is, an ordering problem
  ;; occurs where, when mu4e launches the context variables haven't been set and so mu4e wants
  ;; to create the sent, drafts, etc, directories.
  (setq mu4e-contexts
	(list
	 (make-mu4e-context
	  :name "Fastmail"
	  :match-func (lambda (msg)
			(when msg (string-prefix-p "/Fastmail" (mu4e-message-field msg :maildir))))
	  :vars '((user-full-name        . "Ashlin Eldridge")
		  (user-mail-address     . "aeldridge@fastmail.com")
		  (mu4e-sent-folder      . "/Fastmail/Sent")
		  (mu4e-trash-folder     . "/Fastmail/Trash")
		  (mu4e-drafts-folder    . "/Fastmail/Drafts")
		  (mu4e-refile-folder    . "/Fastmail/Archive")
		  (smtpmail-smtp-server  . "smtp.fastmail.com")
		  (smtpmail-smtp-service . 465)
		  (smtpmail-stream-type  . ssl)
		  ;; Does this work? If so, document.
		  (mu4e-sent-messages-behavior . sent)))

	 (make-mu4e-context
	  :name "Gmail"
	  :match-func (lambda (msg)
			(when msg (string-prefix-p "/Gmail" (mu4e-message-field msg :maildir))))
	  :vars '((user-full-name        . "Ashlin Eldridge")
		  (user-mail-address     . "ashlin.eldridge@gmail.com")
		  (mu4e-sent-folder      . "/Gmail/[Google Mail]/Sent Mail")
		  (mu4e-trash-folder     . "/Gmail/[Google Mail]/Bin")
		  (mu4e-drafts-folder    . "/Gmail/[Google Mail]/Drafts")
		  (mu4e-refile-folder    . "/Gmail/[Google Mail]/All Mail")
		  (smtpmail-smtp-server  . "smtp.gmail.com")
		  (smtpmail-smtp-service . 465)
		  (smtpmail-stream-type  . ssl)
		  ;; Trash sent messages because Gmail will already keep a copy in the sent folder.
		  (mu4e-sent-messages-behavior . trash))))))

(global-set-key (kbd "C-c m") 'mu4e)

;;;; Web Feeds

(use-package elfeed
  :bind
  (:map elfeed-show-mode-map ("?" . which-key-show-major-mode))
  (:map elfeed-search-mode-map ("?" . which-key-show-major-mode))
  :custom
  (elfeed-db-directory (concat my/xdg-cache-dir "/elfeed"))
  :config
  (use-package elfeed-org
    :custom
    (rmh-elfeed-org-files `(,(concat my/gtd-dir "/feeds.org")))
    :config
    (elfeed-org)))

(global-set-key (kbd "C-c e") 'elfeed)

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
        ;; Bind S-ESC to keyboard-escape-quit when in vterm-mode as ESC is used for
        ;; shell signals (e.g., ESC + underscore for last word of the previous command).
        ("S-<escape>" . keyboard-escape-quit))
  :custom
  ;; Don't prompt for permission to compile on first install.
  (vterm-always-compile-module t)
  (vterm-max-scrollback 10000)
  ;; I can't seem to get a reliable way of clearing without losing buffer history.
  ;; The below setting (which is already the default) should work but doesn't.
  (vterm-clear-scrollback-when-clearing nil)
  (vterm-buffer-name-string "vterm: %s"))

(use-package multi-vterm
  :bind (:map global-map
	      ("C-c v v" . multi-vterm)
	      ("C-c v n" . multi-vterm-next)
	      ("C-c v p" . multi-vterm-prev)
	      ("C-c v b" . multi-vterm-rename-buffer)))

;;;; Org Mode

(defvar my/gtd-dir "~/dev/home/gtd")
(defvar my/pkm-dir "~/dev/home/pkm")

;; TODO: Investigate replacing this with https://github.com/minad/org-modern
;; See also: https://protesilaos.com/emacs/dotemacs#h:a6b1bb67-b62b-4018-999a-90cbd0bdceb5
;; See also: https://github.com/daviwil/dotfiles/blob/master/Emacs.org#org-mode
(defun my/org-mode-init ()
  "Org-mode init function that should be attached to `org-mode-hook`."
  (interactive)

  ;;; Org-mode buffer local settings.
  (org-indent-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (variable-pitch-mode 1)
  (display-line-numbers-mode 0)
  (setq-local left-margin-width 2
              right-margin-width 2)

  ;;; Font configuration.

  ;; Replace list hyphen with dot.
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Vary heading sizes.
  ;; This is commented out at the moment as I'm using the modus-themes package
  ;; which has its own way of customising Org-mode headings.
  ;; (dolist (face '((org-level-1 . 1.3)
  ;;                 (org-level-2 . 1.25)
  ;;                 (org-level-3 . 1.2)
  ;;                 (org-level-4 . 1.15)
  ;;                 (org-level-5 . 1.1)
  ;;                 (org-level-6 . 1.1)
  ;;                 (org-level-7 . 1.1)
  ;;                 (org-level-8 . 1.1)))
  ;;   (set-face-attribute (car face) nil
  ;;       		:font my/default-variable-font
  ;;       		:weight 'light
  ;;       		:height (cdr face)))

  ;; Without this, some priorities are bolded/italicized while others are not which looks
  ;; ugly. Strangely, I can't seem to make use of all the face attributes properly - but
  ;; this at least makes them uniform.
  (setq org-priority-faces '((?A . (:foreground "DeepSkyBlue1" :weight 'normal))
			     (?B . (:foreground "DeepSkyBlue2" :weight 'normal))
			     (?C . (:foreground "DeepSkyBlue3" :weight 'normal))))

  ;; Since variable pitch width is set as the default above, override the face attributes
  ;; that we want to appear in fixed pitch width.
  (set-face-attribute 'line-number nil :inherit 'fixed-pitch)
  (set-face-attribute 'line-number-current-line nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-document-info-keyword nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-drawer nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-table nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-tag nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch)))

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
  (org-agenda-mode . (lambda ()
		       ;; Override '?' key to show helpful which-key display.
		       (define-key org-agenda-mode-map "?" 'which-key-show-major-mode)))

  :bind
  ;; Below produces errors that look like org-agenda-mode-map isn't in scope sometimes.
  ;; Unsure why this doesn't work when the mapping for org-mode-map does work. The above lambda
  ;; is a workaround. Will try to clean up at some point.
  ;; (:map org-agenda-mode-map
  ;; (("?" . which-key-show-full-major-mode)
  (:map org-mode-map
        ("C-c C-S-l" . org-cliplink))

  :config
  ;; Add org (typically agenda-related) actions here that require org buffers be saved.
  (advice-add 'org-refile          :after 'org-save-all-org-buffers)
  (advice-add 'org-todo            :after 'org-save-all-org-buffers)
  (advice-add 'org-agenda-set-tags :after 'org-save-all-org-buffers)
  (advice-add 'org-agenda-quit     :before 'org-save-all-org-buffers)

  ;; Make it easier to create org-babel code blocks.
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))

  :custom
  (org-priority-default org-priority-lowest)
  (org-use-fast-todo-selection 'expert)
  (org-agenda-cmp-user-defined 'my/org-agenda-cmp-todo)
  (org-agenda-custom-commands
   `(("d" . "Dashboards") ;; Creates command prefix "d".
     ("da" "All Tasks"
      ((agenda
        ""
        ((org-agenda-overriding-header "Weekly Agenda")
         (org-agenda-span 'week)))
       (alltodo
	""
	((org-agenda-overriding-header "Inbox")
	 (org-agenda-files '(,(concat my/gtd-dir "/inbox.org")))))
       (alltodo
        ""
        ((org-agenda-overriding-header "Personal")
         (org-agenda-files '(,(concat my/gtd-dir "/personal.org")))
         (org-agenda-sorting-strategy '(user-defined-up priority-down))))
       (alltodo
        ""
        ((org-agenda-overriding-header "Work")
         (org-agenda-files '(,(concat my/gtd-dir "/work.org")))
         (org-agenda-sorting-strategy '(user-defined-up priority-down))))))
     ("dp" "Personal Tasks"
      ((agenda
        ""
        ((org-agenda-overriding-header "Weekly Agenda")
         (org-agenda-span 'week)))
       (todo
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
	 (org-agenda-sorting-strategy '(priority-down)))))
      ((org-agenda-tag-filter-preset '("-@work"))))
     ("dw" "Work Tasks"
      ((agenda
        ""
        ((org-agenda-overriding-header "Weekly Agenda")
         (org-agenda-span 'week)))
       (todo
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
	 (org-agenda-sorting-strategy '(priority-down)))))
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
  (org-agenda-window-setup 'current-window)
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
  (org-refile-targets
   `((,(concat my/gtd-dir "/archive.org") :level . 2)
     (,(concat my/gtd-dir "/inbox.org") :level . 1)
     (,(concat my/gtd-dir "/personal.org") :regexp . "Tasks") ;; This seems to work nicer.
     (,(concat my/gtd-dir "/work.org") :regexp . "Tasks")
     (,(concat my/gtd-dir "/someday.org") :regexp . "Tasks") ;; TODO: Make it possible to refile into Projects or Tasks.
     (,(concat my/gtd-dir "/recurring.org") :level . 2)))
  ;; The following two settings are required to make org-refile show the full heading path
  ;; to subheading refile candidates. Took a while to get this working properly.
  (org-refile-use-outline-path t)
  (org-outline-path-complete-in-steps nil)
  (org-tags-column 0)
  (org-todo-keywords
   `((sequence "TODO(t)" "NEXT(n)" "PROG(p)" "HOLD(h)" "|" "DONE(d)")))
  ;; See colours here: https://alexschroeder.ch/geocities/kensanata/colors.html
  (org-todo-keyword-faces
   `(("TODO" . (:foreground "DodgerBlue2" :weight bold))
     ("NEXT" . (:foreground "hot pink" :weight bold))
     ("PROG" . (:foreground "CadetBlue1" :weight bold))
     ("HOLD" . (:foreground "orange1" :weight bold))
     ("DONE" . (:foreground "orange red" :weight bold)))))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n g" . org-roam-graph)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n c" . org-roam-capture)
   ("C-c n t" . org-roam-tag-add))
   ;; Dailies
   ;; ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-setup)
  (require 'org-roam-protocol)
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
      :if-new (file "%<%Y%m%d%H%M%S>-${slug}.org")
      :empty-lines-before 1
      :unnarrowed t))))

(use-package org-cliplink)

(use-package ox-hugo :after ox)

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
  :config
  (auth-source-pass-enable))

;; Don't cache secrets.
(setq auth-source-do-cache nil)

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
