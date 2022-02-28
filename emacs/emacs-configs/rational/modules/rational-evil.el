;;; rational-evil.el -*- lexical-binding: t; -*-

;; Install dependencies
(straight-use-package 'evil)
(straight-use-package 'undo-fu)
(straight-use-package 'undo-fu-session)
(straight-use-package 'evil-collection)

;; Turn on undo-fu-session globally
(global-undo-fu-session-mode)

;; Set some vars that must be configured before loading evil
(setq evil-kill-on-visual-paste nil
        evil-want-integration t
        evil-search-module 'evil-search
        evil-want-keybinding nil
        evil-want-minibuffer nil
        evil-vsplit-window-right t
        evil-split-window-below t
        evil-want-C-u-scroll t
        evil-want-C-d-scroll t
        evil-want-C-i-jump t
        evil-want-Y-yank-to-eol t
        evil-undo-system 'undo-fu
        evil-want-fine-undo t)

;; Load evil.el and enable it globally
(require 'evil)
(evil-mode 1)

;; Make C-g revert to normal state
(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

;; Rebind `universal-argument' to 'C-M-u' since 'C-u' now scrolls the buffer
(global-set-key (kbd "C-M-u") 'universal-argument)

;; C-h is backspace in insert state
(define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

;; Use visual line motions even outside of visual-line-mode buffers
(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)

;; Make sure some modes start in Emacs state
(dolist (mode '(custom-mode
                eshell-mode
                term-mode))
  (add-to-list 'evil-emacs-state-modes mode))

(evil-collection-init)

(provide 'rational-evil)
;;; rational-evil.el ends here
