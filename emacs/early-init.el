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

 ;; Follow recommended lsp-mode performance settings.
 ;; See: https://emacs-lsp.github.io/lsp-mode/page/performance.
 `(gc-cons-threshold ,(* 100 1024 1024))
 `(read-process-output-max ,(* 1 1024 1024))

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
(setenv "GNUPGHOME" (concat (getenv "XDG_DATA_HOME") "/gnupg"))
(setenv "PASSWORD_STORE_DIR" (concat (getenv "XDG_DATA_HOME") "/pass"))
(setenv "GOPATH" (expand-file-name "~/dev/go"))
(setenv "GOROOT" "/opt/homebrew/opt/go/libexec")
(setenv "JAVA_HOME" "/Library/Java/JavaVirtualMachines/current/Contents/Home")
(setenv "DEVELOPER_DIR" "/Applications/Xcode.app/Contents/Developer")
(setenv "SDKROOT" "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk")
(setenv "PAGER" "cat")
(setenv "AWS_PAGER" "")
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

;; Declarations to quieten Flymake down.
(declare-function elpaca-generate-autoloads nil)
(declare-function elpaca-process-queues nil)
(declare-function elpaca-wait nil)
(declare-function elpaca-use-package-mode nil)
(declare-function no-littering-theme-backups nil)
(defvar elpaca-use-package-by-default)

;; Elpaca loads Magit so this needs to be set here.
(defvar magit-define-global-key-bindings nil)

;; Bootstrap Elpaca (https://github.com/progfolio/elpaca).
(defvar elpaca-installer-version 0.5)
(defvar elpaca-directory (expand-file-name "var/elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support.
(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t))

;; Install no-littering.
(elpaca no-littering)

;; Block until current queue is processed.
(elpaca-wait)

;; Configure no-littering.
(require 'no-littering)
(no-littering-theme-backups)

;; If an early-init-private.el file exists, load it. I use this file to manage
;; configuration I don't want to make public. Typically, it's just environment
;; variables and `exec-path' entries that are work-specific.
(let ((pfile (expand-file-name "early-init-private.el" user-emacs-directory)))
  (when (file-readable-p pfile)
    (load pfile)))

;;; early-init.el ends here
