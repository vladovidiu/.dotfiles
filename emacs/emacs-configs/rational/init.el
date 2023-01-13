;;; init.el -*- lexical-binding: t; -*-

;; Profile emacs startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Rational Emacs loaded in %s."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time))))))

;; Add the modules folder to the load path
(add-to-list 'load-path (expand-file-name "modules/" user-emacs-directory))

;; Set default coding system
(set-default-coding-systems 'utf-8)
(customize-set-variable 'visible-bell 1)  ; turn off beeps, make them flash!
(customize-set-variable 'large-file-warning-threshold 100000000) ;; change to ~100 MB

;; Initialize straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
      (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
       (url-retrieve-synchronously
        "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(defun rational-ensure-package (package &optional args)
  "Ensure that PACKAGE is installed on the system, via
straight.el."
  (straight-use-package package))

;; Defines the user configuration file and var and etc folders
;; and ensure they exists.
(defvar rational-config-file (expand-file-name "config.el" rational-config-path)
  "The user's configuration file.")
(defvar rational-config-etc-directory (expand-file-name "etc/" rational-config-path)
  "The user's configuration etc/ folder.")
(defvar rational-config-var-directory (expand-file-name "var/" rational-config-path)
  "The user's configuration var/ folder.")

(mkdir rational-config-etc-directory t)
(mkdir rational-config-var-directory t)

;; Load the user configuration file if it exists
(when (file-exists-p rational-config-file)
  (load rational-config-file nil 'nomessage))

;; Make GC pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
