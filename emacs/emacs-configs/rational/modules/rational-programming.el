;;; rational-programming.el -*- lexical-binding: t; -*-

;;; Code:

;; Install dependencies
(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)
(straight-use-package 'flycheck)

;;; LSP mode
(require 'lsp-mode)
(require 'lsp-ui)

;; Settings
(customize-set-variable 'read-process-output-max (* 1024 1024))
(customize-set-variable 'lsp-keymap-prefix "C-c l")
(customize-set-variable 'lsp-completion-provider :none)
(customize-set-variable 'lsp-headerline-breadcrumb-enable nil)
(customize-set-variable 'lsp-ui-peek-enable t)
(customize-set-variable 'lsp-ui-doc-max-height 14)
(customize-set-variable 'lsp-ui-doc-max-width 72)
(customize-set-variable 'lsp-ui-doc-delay 0.75)
(customize-set-variable 'lsp-ui-doc-show-with-mouse nil)
(customize-set-variable 'lsp-ui-doc-position 'bottom)
(customize-set-variable 'lsp-ui-sideline-ignore-duplicate t)

;; Start lsp and lsp ui
(add-hook 'c-mode-hook 'lsp-deferred)
(add-hook 'c++-mode-hook 'lsp-deferred)
(add-hook 'enh-ruby-mode-hook 'lsp-deferred)
(add-hook 'lsp-mode 'lsp-ui-mode)

;;; Flycheck
(require 'flycheck)
(add-hook 'lsp-mode 'flycheck-mode)

(provide 'rational-programming)
;;; rational-programming.el ends here
