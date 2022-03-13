;;; rational-go.el -*- lexical-binding: t -*-

;;; Code:

;; Install dependencies
(straight-use-package 'go-mode)

;; Hooks
(add-hook 'go-mode-hook 'lsp-deferred)

(provide 'rational-go)
;;; rational-go.el ends here
