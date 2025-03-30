;;; bootstrap.el --- Elpaca Bootstrap -*- lexical-binding: t -*-

;; Author: Ashlin Eldridge <ashlin.eldridge@gmail.com>
;; URL: https://github.com/ashlineldridge/.config
;; Version: 1.0.0
;; Package-Requires: ((emacs "30.0"))

;;; Commentary:
;;
;; Bootstrap the Elpaca package manager.
;; See: https://github.com/progfolio/elpaca.

;;; Code:

;; Forward declarations to make the byte compiler happy.
(declare-function elpaca "elpaca")
(declare-function elpaca-generate-autoloads "elpaca")
(declare-function elpaca-process-queues "elpaca")
(declare-function elpaca-wait "elpaca")
(declare-function elpaca--queued "elpaca")
(declare-function elpaca-use-package-mode "elpaca-use-package")
(declare-function no-littering-theme-backups "no-littering")
(defvar elpaca-after-init-time)
(defvar elpaca-use-package)
(defvar elpaca-use-package-by-default)
(defvar no-littering)

(defun my/emacs-startup-statistics ()
  "Print Emacs startup statistics."
  (message "Emacs startup: boot: %.2fs, init: %.2fs, packages: %d, sweeps: %d"
           (time-to-seconds (time-since before-init-time))
           (float-time (time-subtract elpaca-after-init-time before-init-time))
           (length (elpaca--queued))
           gcs-done))

;; Print startup statistics.
(add-hook 'emacs-startup-hook #'my/emacs-startup-statistics 100)

;; Elpaca loads Magit so this needs to be set here.
(defvar magit-define-global-key-bindings nil)

;; With the exception of including var/ in `elpaca-directory', the block below
;; has been copied verbatim from https://github.com/progfolio/elpaca#installer.
(defvar elpaca-installer-version 0.10)
(defvar elpaca-directory (expand-file-name "var/elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
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

;; Configure use-package behavior before it is loaded.
(setq-default
 use-package-always-defer t
 use-package-always-ensure t
 use-package-compute-statistics t
 use-package-enable-imenu-support t
 use-package-expand-minimally (not init-file-debug))

;; Install use-package support.
(elpaca elpaca-use-package
  (elpaca-use-package-mode))

;; Install no-littering early to tame Emacs config/data/other files.
(elpaca no-littering
  (require 'no-littering)
  (no-littering-theme-backups))

;; Block until current queue is processed.
(elpaca-wait)

;;; bootstrap.el ends here
