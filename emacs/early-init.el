;;; early-init.el --- Emacs Early Init -*- lexical-binding: t -*-

;; Author: Ashlin Eldridge <ashlin.eldridge@gmail.com>
;; URL: https://github.com/ashlineldridge/.config
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.0"))

;;; Commentary:
;;
;; Early init configuration.

;;; Code:

;; In Emacs 29+, we can change the eln-cache directory.
;; See https://github.com/emacscollective/no-littering#native-compilation-cache.
(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (expand-file-name "var/eln-cache/" user-emacs-directory)))

(custom-set-variables
 ;; Quieten Emacs 29+ compilation warnings.
 '(native-comp-async-report-warnings-errors nil)

 ;; Disable package.el as Elpaca is used for package management.
 '(package-enable-at-startup nil)

 ;; Garbage collection is started later using GCMH.
 `(gc-cons-threshold ,most-positive-fixnum)

 ;; Disable JIT compilation.
 '(native-comp-jit-compilation nil)

 ;; Enable `use-package''s imenu support.
 '(use-package-enable-imenu-support t))

;; Keep track of start up time.
(add-hook 'emacs-startup-hook
	  (lambda ()
            (message "Emacs ready in %s seconds with %d garbage collections."
		     (emacs-init-time "%.2f") gcs-done)))

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
(setenv "EDITOR" "emacsclient")

;; Set `exec-path' and PATH explicitly so that they mirror each other.
(setq exec-path
      (mapcar #'expand-file-name
              `("~/bin"
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
                "~/.cargo/bin"
                "~/.krew/bin"
                "~/.local/bin"
                ;; Emacs expects the last value of `exec-path' to be `exec-directory'.
                ,exec-directory)))
(setenv "PATH" (mapconcat #'identity exec-path ":"))

;; This is a workaround for a bunch of warnings that get displayed by the
;; d12frosted/homebrew-emacs-plus distribution of Emacs on MacOS.
;; See https://github.com/d12frosted/homebrew-emacs-plus/issues/323 for details.
(setenv "LIBRARY_PATH"
	(mapconcat #'identity
                   '("/opt/homebrew/opt/gcc/lib/gcc/13"
                     "/opt/homebrew/opt/libgccjit/lib/gcc/13"
                     "/opt/homebrew/opt/gcc/lib/gcc/13/gcc/aarch64-apple-darwin22/13") ":"))

;; If an early-init-private.el file exists, load it. I use this file to manage
;; configuration I don't want to make public. Typically, it's just environment
;; variables and `exec-path' entries that are work-specific.
(let ((pfile (expand-file-name "early-init-private.el" user-emacs-directory)))
  (when (file-readable-p pfile)
    (load pfile)))

;;; early-init.el ends here
