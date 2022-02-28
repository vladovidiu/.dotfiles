;;; rational-defaults.el -*- lexical-binding: t; -*-

;;; Commnetary:
;;
;; General sane defaults
;;
;;; Code:

;; Install dependencies
(straight-use-package 'no-littering)

;; Revert dired and other buffers
(customize-set-variable 'global-auto-revert-non-file-buffers t)

;; Revert buffers when the file has changed
(global-auto-revert-mode 1)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; Set custom file
(setq custom-file
      (if (boundp 'server-socket-dir)
        (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))

(load custom-file t)

;; Sane (to me) defaults
(setq-default
 frame-resize-pixelwise t
 create-lockfiles nil
 indent-tabs-mode nil
 auto-save-default nil
 auto-windows-vscroll nil
 delete-by-moving-to-trash t
 fill-column 80
 enable-recursive-minibuffers t
 help-window-select t
 x-stretch-cursor t
 column-number-mode t
 echo-keystrokes 0.1
 use-short-answers t
 tab-always-indent 'complete
 global-auto-revert-non-file-buffers t
 vc-follow-symlinks t)

;; Change the ls output of dired
(customize-set-variable 'dired-listing-switches "-vAlahF --group-directories-first")

;; Set the backup directory
(setq backup-directory-alist '(("." . "~/.save")))

;; Turn on recentf mode
(add-hook 'after-init-hook #'recentf-mode)
(setq recentf-save-file (expand-file-name "recentf" rational-config-var-directory))

;; Do not save duplicates in kill-ring
(customize-set-variable 'kill-do-not-save-duplicates t)

;; Better support for files with long lines
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)
(global-so-long-mode 1)

;; Make shebang (#!) file executable when saved
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Enable savehist-mode for command history
(savehist-mode 1)

;; Use ibuffer instead of *Buffer List*
(global-set-key (kbd "C-x C-b") 'ibuffer)

(provide 'rational-defaults)
;;; rational-defaults.el ends here
