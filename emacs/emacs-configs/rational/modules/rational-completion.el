;;; rational-completion.el -*- lexical-binding: t; -*-

;;; Code:

;; Install dependencies
(straight-use-package 'orderless)
(straight-use-package 'vertico)
;; (straight-use-package 'consult)
(straight-use-package 'marginalia)
;; (straight-use-package 'embark)

;;; Orderless
(require 'orderless)
(customize-set-variable 'completion-styles '(orderless))
(customize-set-variable 'completion-category-overrides '((file (styles . (partial-completion)))))
(setq completion-category-defaults nil)

;;; Vertico
(require 'vertico)
(require 'vertico-directory "extensions/vertico-directory.el")
(require 'vertico-flat "extensions/vertico-flat.el")

(with-eval-after-load 'evil
  (define-key vertico-map (kbd "M-h") 'vertico-directory-up))

(customize-set-variable 'vertico-cycle t)
(customize-set-variable 'vertico-count 13)

;; Start Vertico
(vertico-mode 1)

;;; Marginalia
(require 'marginalia)
(customize-set-variable 'marginalia-align 'right)
(customize-set-variable 'marginalia-max-relative-age 0)
(customize-set-variable 'marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))

;; Start Marginalia
(marginalia-mode 1)

(provide 'rational-completion)
;;; rational-completion.el ends here
