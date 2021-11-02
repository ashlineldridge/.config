;; Prevent package.el from making packages available as we'll use straight.el to
;; manage packages. This variable must be disabled in this file to take effect.
(setq package-enable-at-startup nil)

;; This is a workaround for a bunch of warnings that get displayed by
;; the d12frosted/homebrew-emacs-plus distribution of Emacs on MacOS.
;; See https://github.com/d12frosted/homebrew-emacs-plus/issues/323 for details.
(setenv "LIBRARY_PATH" "/usr/local/opt/gcc/lib/gcc/11:/usr/local/opt/libgccjit/lib/gcc/11:/usr/local/opt/gcc/lib/gcc/11/gcc/x86_64-apple-darwin19/11.2.0")

;; Quieten Emacs 28+ down as otherwise it prints many warnings when compiling packages.
;; See https://www.reddit.com/r/emacs/comments/l42oep/suppress_nativecomp_warnings_buffer.
(setq native-comp-async-report-warnings-errors nil)
