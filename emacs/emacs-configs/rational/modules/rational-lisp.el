;;; rational-lisp.el -*- lexical-binding: t; -*-

;;; Code:

;; Install dependencies
(straight-use-package 'lispy)
(straight-use-package 'lispyville)

;;; Lispy
(require 'lispy)

;; Start Lispy
(add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
(add-hook 'scheme-mode-hook (lambda () (lispy-mode 1)))

;;; Lispyville
(require 'lispyville)

;; Stat Lispyville
(add-hook 'lispy-mode-hook #'lispyville-mode)

;;; Sly
(use-package sly
  :straight t
  :commands (sly)
  :config
  (setq sly-lisp-implementations
		`(
		  (roswell ("ros" "run"))
		  (roswell-sbcl ("ros" "-L" "sbcl" "-Q" "-l" "~/.sbclrc" "run") :coding-system utf-8-unix)))
  (setq sly-default-lisp 'roswell)
  (setq sly-symbol-completion-mode t)
  (setq org-babel-lisp-eval-fn #'sly-eval)
  (setq inferior-lisp-program "ros run"))

(provide 'rational-lisp)
;;; rational-lisp.el ends here
