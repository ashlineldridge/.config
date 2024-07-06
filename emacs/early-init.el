;;; early-init.el --- Emacs Early Init -*- lexical-binding: t -*-

;; Author: Ashlin Eldridge <ashlin.eldridge@gmail.com>
;; URL: https://github.com/ashlineldridge/.config
;; Version: 1.0.0
;; Package-Requires: ((emacs "30.0"))

;;; Commentary:
;;
;; Early init configuration.

;;; Code:

;; Defer garbage collection until after init and restore normality below.
;; See also: https://emacsconf.org/2023/talks/gc.
(setq gc-cons-threshold most-positive-fixnum)

;; Keep track of start up time and restore sensible GC settings.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f") gcs-done)
            (setq gc-cons-threshold (* 16 1024 1024))))

;; Change the default native compilation cache directory.
;; See https://github.com/emacscollective/no-littering#native-compilation-cache.
(startup-redirect-eln-cache
 (expand-file-name "var/eln-cache/" user-emacs-directory))

;; Disable package.el as Elpaca is used for package management.
(setq package-enable-at-startup nil)

;; Disable JIT compilation.
(setq native-comp-jit-compilation nil)

;; Configure `use-package' options that need to before load.
(setq-default use-package-always-defer t
              use-package-always-ensure t
              use-package-expand-minimally t
              use-package-enable-imenu-support t)

;; Hide the title bar and use rounded corners.
;; See https://github.com/d12frosted/homebrew-emacs-plus#emacs-29-and-emacs-30.
(add-to-list 'default-frame-alist '(undecorated-round . t))

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

;; This is a workaround for a bunch of warnings that get displayed by the
;; d12frosted/homebrew-emacs-plus distribution of Emacs on MacOS.
;; See https://github.com/d12frosted/homebrew-emacs-plus/issues/323 for details.
(setenv "LIBRARY_PATH"
	(mapconcat #'identity
                   '("/opt/homebrew/opt/gcc/lib/gcc/14"
                     "/opt/homebrew/opt/libgccjit/lib/gcc/14"
                     "/opt/homebrew/opt/gcc/lib/gcc/14/gcc/aarch64-apple-darwin23/14") ":"))

;;; early-init.el ends here
