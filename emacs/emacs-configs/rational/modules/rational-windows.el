;;; rational-windows.el -*- lexical-binding: t; -*-

;;; Code
;; Install dependencies
(straight-use-package 'popper)

;;; Popper

;; Bindings
(global-set-key (kbd "M-`") #'popper-toggle-latest)
(global-set-key (kbd "M-~") #'popper-cycle)
(global-set-key (kbd "C-x M-`") #'popper-toggle-type)

;; Settings
(setq popper-reference-buffers
      '("\\*Messages\\*"
        "\\*Warnings\\*"
        "\\*xref\\*"
        "\\*Backtrace\\*"
        "\\*eldoc\\*"
        "\\*compilation\\*"
        "^*tex"
        "^\\*sly-[^s]"
        "\\*Ement Notifications\\*"
        "Output\\*$"
        "\\*Async Shell Command\\*"
        "\\*Dtache Shell Command\\*"
        "\\*mu4e-update\\*"
        help-mode
        helpful-mode
        compilation-mode))

;; Init
(require 'popper)
(popper-mode +1)

(provide 'rational-windows)
;;; rational-windows.el ends here
