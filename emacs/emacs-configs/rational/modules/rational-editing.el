;;; rational-editing.el -*- lexical-binding: t; -*-

;;; Code:

;; Install dependencies
(straight-use-package 'ws-butler)
(straight-use-package 'evil-nerd-commenter)
(straight-use-package 'expand-region)

;; Set up ws-butler for trimming white space and line endings
(add-hook 'text-mode-hook 'ws-butler-mode)
(add-hook 'prog-mode-hook 'ws-butler-mode)

;; Set a global binding for better line commenting
(global-set-key (kbd "M-/") 'evilnc-comment-or-uncomment-lines)

;; Parentheses
(electric-pair-mode 1)
(show-paren-mode 1)

;; Replace visual selection
(delete-selection-mode 1)

;; Bindings for expand region
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C--") 'er/contract-region)

(provide 'rational-editing)
;;; rational-editing.el ends here
