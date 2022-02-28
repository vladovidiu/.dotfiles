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

(provide 'rational-lisp)
;;; rational-lisp.el ends here
