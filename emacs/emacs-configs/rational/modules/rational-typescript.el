;;; rational-typescript.el -*- lexical-binding: t; -*-

;;; Code:

;; Install dependencies
(straight-use-package '(tsx-mode :type git :host github :repo "orzechowskid/tsx-mode.el"))

;; Start TSX mode
(require 'tsx-mode)

;; Hook
(add-to-list 'auto-mode-alist
             (cons "\\.tsx\\'" #'tsx-mode))

(add-to-list 'auto-mode-alist
             (cons "\\.ts\\'" #'tsx-mode))

(provide 'rational-typescript)

;;; rational-typescript.el ends here
