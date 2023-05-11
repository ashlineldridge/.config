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
;; TODO: This doesn't completely work yet as ~/.config/emacs/eln-cache will
;; still accumulate some files due to Flycheck.
;; See: https://github.com/emacscollective/no-littering/discussions/206.
;; (when (fboundp 'startup-redirect-eln-cache)
;;   (startup-redirect-eln-cache
;;    (expand-file-name "var/eln-cache/" user-emacs-directory)))

;; Quieten Emacs 29+ down as otherwise it prints many warnings when compiling
;; packages. Set the variable to 'silent to log the warnings but not open the
;; *Warnings* buffer.
(setq native-comp-async-report-warnings-errors nil)

;; Prevent package.el from making packages available as we'll use straight.el to
;; manage packages. This variable must be disabled in this file to take effect.
(setq package-enable-at-startup nil)

;; Emacs performance settings that follow the recommendations of lsp-mode here:
;; https://emacs-lsp.github.io/lsp-mode/page/performance.
(setq gc-cons-threshold (* 100 1024 1024))
(setq read-process-output-max (* 1 1024 1024))

;; Keep track of start up time.
(add-hook 'emacs-startup-hook
	  (lambda ()
            (message "Emacs ready in %s seconds with %d garbage collections."
		     (emacs-init-time "%.2f") gcs-done)))

;; This is a workaround for a bunch of warnings that get displayed by
;; the d12frosted/homebrew-emacs-plus distribution of Emacs on MacOS.
;; See https://github.com/d12frosted/homebrew-emacs-plus/issues/323 for details.
(setenv "LIBRARY_PATH"
	(mapconcat #'identity
         '("/opt/homebrew/opt/gcc/lib/gcc/12"
           "/opt/homebrew/opt/libgccjit/lib/gcc/12"
           "/opt/homebrew/opt/gcc/lib/gcc/12/gcc/aarch64-apple-darwin21/12")
         ":"))

;; Variables for external processes.
(setenv "XDG_CONFIG_HOME" (expand-file-name "~/.config"))
(setenv "XDG_CACHE_HOME" (expand-file-name "~/.cache"))
(setenv "XDG_DATA_HOME" (expand-file-name "~/.local/share"))
(setenv "XDG_STATE_HOME" (expand-file-name "~/.local/state"))
(setenv "GNUPGHOME" (concat (getenv "XDG_DATA_HOME") "/gnupg"))
(setenv "PASSWORD_STORE_DIR" (concat (getenv "XDG_DATA_HOME") "/pass"))
(setenv "GOPATH" (expand-file-name "~/dev/go"))
(setenv "GOROOT" "/opt/homebrew/opt/go/libexec")
(setenv "DEVELOPER_DIR" "/Applications/Xcode.app/Contents/Developer")
(setenv "SDKROOT" "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk")
(setenv "PAGER" "cat")
(setenv "AWS_PAGER" "")
(setenv "KUBECTX_IGNORE_FZF" "true")
(setenv "EDITOR" "emacsclient")
(setenv "PATH" (mapconcat #'expand-file-name
                          '("~/bin"
                            "/opt/homebrew/bin"
                            "/opt/homebrew/opt/curl/bin"
                            "/opt/homebrew/opt/coreutils/libexec/gnubin"
                            "/opt/homebrew/opt/findutils/libexec/gnubin"
                            "/opt/homebrew/opt/gettext/bin"
                            "/opt/homebrew/opt/llvm/bin"
                            "/opt/homebrew/opt/go/libexec/bin"
                            "~/dev/go/bin"
                            "/usr/local/bin"
                            "/usr/bin"
                            "/bin"
                            "/opt/homebrew/sbin"
                            "/usr/sbin"
                            "/sbin"
                            "~/.cargo/bin"
                            "~/.krew/bin"
                            "~/.local/bin") ":"))

;;; early-init.el ends here
