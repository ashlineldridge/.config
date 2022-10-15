;; Prevent package.el from making packages available as we'll use straight.el to
;; manage packages. This variable must be disabled in this file to take effect.
(setq package-enable-at-startup nil)

;; This is a workaround for a bunch of warnings that get displayed by
;; the d12frosted/homebrew-emacs-plus distribution of Emacs on MacOS.
;; See https://github.com/d12frosted/homebrew-emacs-plus/issues/323 for details.
(require 'subr-x)
(setenv "LIBRARY_PATH"
        (string-join
         '("/opt/homebrew/opt/gcc/lib/gcc/12"
           "/opt/homebrew/opt/libgccjit/lib/gcc/12"
           "/opt/homebrew/opt/gcc/lib/gcc/12/gcc/aarch64-apple-darwin21/12") ":"))
