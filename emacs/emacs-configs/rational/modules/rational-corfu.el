;;; rational-corfu.el -*- lexical-binding: t; -*-

;;; Code:

;; Dependencies
(straight-use-package 'corfu)
(straight-use-package 'kind-icon)
(straight-use-package 'cape)
(straight-use-package '(corfu-doc :type git :host github :repo "galeo/corfu-doc"))

;; Settings
(require 'corfu)
(require 'corfu-doc)

(customize-set-variable 'corfu-cycle t)
(customize-set-variable 'corfu-auto t)
(customize-set-variable 'corfu-auto-prefix 2)
(customize-set-variable 'corfu-auto-delay 0)
(customize-set-variable 'corfu-min-width 60)
(customize-set-variable 'corfu-max-width 60)
(customize-set-variable 'corfu-count 14)
(customize-set-variable 'corfu-scroll-margin 4)
(customize-set-variable 'corfu-echo-documentation 0.25)
(customize-set-variable 'corfu-separator ?\s)
(customize-set-variable 'corfu-quit-no-match 'separator)

(customize-set-variable 'corfu-doc-delay 0.5)
(customize-set-variable 'corfu-doc-max-width 60)
(customize-set-variable 'corfu-doc-max-height 20)

;; Start
(global-corfu-mode 1)

;; Keybindings
(define-key corfu-map (kbd "TAB") 'corfu-next)
(define-key corfu-map [tab] 'corfu-next)
(define-key corfu-map (kbd "S-TAB") 'corfu-previous)
(define-key corfu-map [backtab] 'corfu-previous)
(define-key corfu-map (kbd "M-p") #'corfu-doc-scroll-down)
(define-key corfu-map (kbd "M-n") #'corfu-doc-scroll-up)
(define-key corfu-map (kbd "M-SPC") #'corfu-insert-separator)

;; LSP integration
(defun vt/corfu-setup-lsp ()
  "Use orderless completion style with lsp-capf instead of the
  default lsp-passthrough."
  (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
        '(orderless)))
(add-hook 'lsp-completion-mode-hook 'vt/corfu-setup-lsp)

;; Cape
(dolist (backend '(cape-symbol cape-keyword cape-file cape-dabbrev))
  (add-to-list 'completion-at-point-functions backend))

;; Kind Icons
(require 'kind-icon)
(customize-set-variable 'kind-icon-default-face 'corfu-default)
(customize-set-variable 'kind-icon-blend-background nil)
(customize-set-variable 'kind-icon-blend-frac 0.08)
(customize-set-variable 'kind-icon-default-style '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 0.8 :scale 1.0))
(add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)

(require 'cape)

(advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
(advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)

;; Hooks
(add-hook 'eshell-mode-hook
          (lambda () (setq-local corfu-quit-at-boundary t
                            corfu-quit-no-match t
                            )
            (corfu-mode)))
(add-hook 'corfu-mode-hook #'corfu-doc-mode)

(provide 'rational-corfu)
;;; rational-corfu.el ends here
