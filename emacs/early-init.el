;;; early-init.el --- Emacs Early Init -*- lexical-binding: t -*-

;; Author: Ashlin Eldridge <ashlin.eldridge@gmail.com>
;; URL: https://github.com/ashlineldridge/.config
;; Version: 1.0.0
;; Package-Requires: ((emacs "30.0"))

;;; Commentary:
;;
;; Early init configuration.

;;; Code:

;; Init color should be set to the background color of the theme. E.g. for an
;; an `ef-themes' theme, use the value of 'bg-main from the relevant palette
;; variable (e.g. `ef-dark-palette').
(defvar my/init-color "#000000")

;; Increase GC threshold during init and then drop it after. See:
;; - https://github.com/jamescherti/minimal-emacs.d
;; - https://emacsconf.org/2023/talks/gc
(defvar my/gc-cons-threshold (* 64 1024 1024))
(setq gc-cons-threshold most-positive-fixnum)

(defun my/emacs-init-gc ()
  "Init GC settings after startup."
  (setq gc-cons-threshold my/gc-cons-threshold))

(add-hook 'emacs-startup-hook #'my/emacs-init-gc 101)

;; Change the default native compilation cache directory.
;; See https://github.com/emacscollective/no-littering#native-compilation-cache.
(startup-redirect-eln-cache
 (expand-file-name "var/eln-cache/" user-emacs-directory))

;; Disable JIT compilation.
(setq native-comp-jit-compilation nil)

;; Base settings to be configured early.
(custom-set-variables
 '(use-short-answers t)
 `(ring-bell-function #'ignore)
 ;; Disable package.el as Elpaca is used for package management.
 '(package-enable-at-startup nil)
 '(native-comp-jit-compilation nil)
 '(frame-resize-pixelwise t)
 '(frame-inhibit-implied-resize t)
 '(inhibit-splash-screen t)
 '(inhibit-startup-screen t)
 '(inhibit-startup-buffer-menu t)
 '(tool-bar-mode nil)
 '(tooltip-mode nil)
 '(menu-bar-mode nil)
 '(scroll-bar-mode nil)
 ;; Ignore any changes made via the customization UI.
 `(custom-file ,(make-temp-file "emacs-custom-")))

;; Enable functions that are disabled by default.
(dolist (cmd '(narrow-to-region
               upcase-region
               downcase-region
               set-goal-column))
  (put cmd 'disabled nil))

;; Configure frame parameters.
(setq default-frame-alist
      (append '((undecorated-round . t)
                (width . (text-pixels . 700))
                (height . (text-pixels . 600)))
              default-frame-alist))

;; Blank out the screen to prevent the initial flash of white light.
(fringe-mode 0)
(setq mode-line-format nil)
(set-face-attribute 'default nil :background my/init-color :foreground "#ffffff")
(set-face-attribute 'mode-line nil :background my/init-color :foreground "#ffffff" :box 'unspecified)
;; Without the following hook, new frames will retain the blanked out features set above.
(add-hook 'after-make-frame-functions
          (lambda (&rest _)
            (when-let ((theme (car custom-enabled-themes)))
              (enable-theme theme))))

;; Configure environment variables here.
(setenv "XDG_CONFIG_HOME" (expand-file-name "~/.config"))
(setenv "XDG_CACHE_HOME" (expand-file-name "~/.cache"))
(setenv "XDG_DATA_HOME" (expand-file-name "~/.local/share"))
(setenv "XDG_STATE_HOME" (expand-file-name "~/.local/state"))
(setenv "ZDOTDIR" (expand-file-name "zsh" (getenv "XDG_CONFIG_HOME")))
(setenv "GNUPGHOME" (expand-file-name "gnupg" (getenv "XDG_DATA_HOME")))
(setenv "PASSWORD_STORE_DIR" (expand-file-name "pass" (getenv "XDG_DATA_HOME")))
(setenv "GOPATH" (expand-file-name "~/dev/go"))
(setenv "GOROOT" "/opt/homebrew/opt/go/libexec")
(setenv "DEVELOPER_DIR" "/Applications/Xcode.app/Contents/Developer")
(setenv "SDKROOT" "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk")
(setenv "JAVA_HOME" "/Library/Java/JavaVirtualMachines/current/Contents/Home")
(setenv "PAGER" "cat")
(setenv "KUBECTX_IGNORE_FZF" "true")
(setenv "BUILDKIT_PROGRESS" "plain")
(setenv "EDITOR" "emacsclient")

;; Set `exec-path' and PATH explicitly so that they mirror each other.
(setq exec-path
      (mapcar #'expand-file-name
              `("~/bin"
                "~/.local/bin" ;; Used by tools like Haskell's Stack.
                "~/.cargo/bin"
                "~/.ghcup/bin"
                "/opt/homebrew/bin"
                "/opt/homebrew/opt/llvm/bin"
                ,(expand-file-name "bin" (getenv "GOROOT"))
                ,(expand-file-name "bin" (getenv "GOPATH"))
                ,(expand-file-name "bin" (getenv "JAVA_HOME"))
                "/usr/local/bin"
                "/usr/bin"
                "/bin"
                "/opt/homebrew/sbin"
                "/usr/sbin"
                "/sbin"
                ;; Emacs expects the last value of `exec-path' to be `exec-directory'.
                ,exec-directory)))
(setenv "PATH" (mapconcat #'identity exec-path ":"))

;;; early-init.el ends here
