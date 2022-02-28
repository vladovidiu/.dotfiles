;;; rational-ruby.el -*- lexical-binding: t; -*-

;;; Code:

;; Install dependencies
(straight-use-package 'enh-ruby-mode)
(straight-use-package 'yard-mode)
(straight-use-package 'inf-ruby)
(straight-use-package 'rubocop)
(straight-use-package 'robe)
(straight-use-package 'bundler)
(straight-use-package 'rake)
(straight-use-package 'rbenv)

;;; Enhanced Ruby Mode
(require 'enh-ruby-mode)
(add-to-list 'auto-mode-alist '("\\.rb\\'" . enh-ruby-mode))

;;; Yard Mode
(require 'yard-mode)

;;; Inf Ruby Mode
(require 'inf-ruby)

;;; RuboCop
(require 'rubocop)

;; Settings
(customize-set-variable 'rubocop-autocorrect-command "rubocop -A --format emacs")

;;; Robe
(require 'robe)

;;; Bundler
(require 'bundler)

;;; Rake
(require 'rake)

;;; Rbenv
(require 'rbenv)
(global-rbenv-mode)

;; Hooks
(add-hook 'enh-ruby-mode-hook 'yard-mode)
(add-hook 'enh-ruby-mode-hook 'inf-ruby-minor-mode)
(add-hook 'enh-ruby-mode-hook 'rubocop-mode)
(add-hook 'enh-ruby-mode-hook 'robe-mode)

;;; Sorbet
(defun vt/lsp-sorbet-command ()
  "Build sorbet command"
  (let ((lsp-command '("srb" "tc" "--lsp" "--disable-watchman" ".")))
    (if lsp-sorbet-use-bundler
        (append '("bundle" "exec") lsp-command)
      lsp-command)))

(lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection #'vt/lsp-sorbet-command)
    :major-modes '(ruby-mode enh-ruby-mode)
    :priority 10
    :multi-root nil
    :server-id 'ruby-sorbet
    :initialized-fn (lambda (workspace)
                      (with-lsp-workspace workspace
                        (lsp--set-configuration "sorbet")))))

(customize-set-variable 'lsp-disabled-clients '(ruby-ls sorbet-ls steep-ls))

(provide 'rational-ruby)
;;; rational-ruby.el ends here
