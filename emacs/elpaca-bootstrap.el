;;; elpaca-bootstrap.el --- Elpaca Bootstrap -*- lexical-binding: t -*-

;; Author: Ashlin Eldridge <ashlin.eldridge@gmail.com>
;; URL: https://github.com/ashlineldridge/.config
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;;
;; Bootstrap the Elpaca package manager.
;; See: https://github.com/progfolio/elpaca.

;;; Code:

;; Elpaca loads Magit so this needs to be set here.
(defvar magit-define-global-key-bindings nil)

;; With the exception of including var/ in `elpaca-directory', the block below
;; has been copied verbatim from https://github.com/progfolio/elpaca#installer.
(defvar elpaca-installer-version 0.6)
(defvar elpaca-directory (expand-file-name "var/elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo (expand-file-name "elpaca/" elpaca-repos-directory))
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

;; Newer versions of built-in packages are required by other packages.
;; Will keep upgrades of core built-in packages here for now.
;; See: https://github.com/progfolio/elpaca/issues/216#issuecomment-1876204588.
(defun my/elpaca-unload-seq (e)
  (and (featurep 'seq) (unload-feature 'seq t))
  (elpaca--continue-build e))

(defun my/elpaca-seq-build-steps ()
  (append (butlast (if (file-exists-p (expand-file-name "seq" elpaca-builds-directory))
                       elpaca--pre-built-steps elpaca-build-steps))
          (list #'my/elpaca-unload-seq #'elpaca--activate-package)))

(elpaca `(seq :build ,(my/elpaca-seq-build-steps)))
(elpaca transient)
(elpaca jsonrpc)

;; Block until current queue is processed.
(elpaca-wait)

;;; elpaca-bootstrap.el ends here
