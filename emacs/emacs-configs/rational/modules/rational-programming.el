;;; rational-programming.el -*- lexical-binding: t; -*-

;;; Code:

;; Install dependencies
(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)
;; (straight-use-package 'company)
;; (straight-use-package 'company-box)
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
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)
(add-hook 'enh-ruby-mode-hook 'lsp)
(add-hook 'lsp-mode 'lsp-ui-mode)

;;; Company
;; (require 'company)

;; ;; Settings
;; (customize-set-variable 'company-minimum-prefix-length 2)
;; (customize-set-variable 'company-tooltip-limit 14)
;; (customize-set-variable 'company-tooltip-align-annotations t)
;; (customize-set-variable 'company-require-match 'never)

;; (eval-after-load 'company
;;   '(push 'company-robe company-backends))

;; ;; Hooks
;; (add-hook 'global-company-mode-hook 'evil-normalize-keymaps)

;; ;; Start Company
;; (add-hook 'after-init-hook 'global-company-mode)

;; ;;; Company Box
;; (require 'company-box)

;; ;; Settings
;; (customize-set-variable 'company-box-show-single-candidate t)
;; (customize-set-variable 'company-box-backends-colors nil)
;; (customize-set-variable 'company-box-max-candidates 50)

;; ;; Start Company Box
;; (add-hook 'company-mode-hook 'company-box-mode)

;;; Flycheck
(require 'flycheck)
(add-hook 'lsp-mode 'flycheck-mode)

(provide 'rational-programming)
;;; rational-programming.el ends here
