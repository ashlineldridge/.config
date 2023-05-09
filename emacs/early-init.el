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

;; Quieten Emacs 29+ down as otherwise it prints many warnings when compiling packages.
;; See https://www.reddit.com/r/emacs/comments/l42oep/suppress_nativecomp_warnings_buffer.
(setq native-comp-async-report-warnings-errors nil)

;; Prevent package.el from making packages available as we'll use straight.el to
;; manage packages. This variable must be disabled in this file to take effect.
(setq package-enable-at-startup nil)

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

;; This is a workaround for a bunch of warnings that get displayed by
;; the d12frosted/homebrew-emacs-plus distribution of Emacs on MacOS.
;; See https://github.com/d12frosted/homebrew-emacs-plus/issues/323 for details.
(setenv "LIBRARY_PATH"
	(mapconcat #'identity
         '("/opt/homebrew/opt/gcc/lib/gcc/12"
           "/opt/homebrew/opt/libgccjit/lib/gcc/12"
           "/opt/homebrew/opt/gcc/lib/gcc/12/gcc/aarch64-apple-darwin21/12") ":"))

;;; early-init.el ends here
