;;; rational-org.el -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Rational org sets some personal preferences for org mode


;;; Code:
(straight-use-package 'org)
(straight-use-package 'org-modern)
(straight-use-package 'org-appear)
(straight-use-package 'visual-fill-column)

(defun vt/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

;; Config
(customize-set-variable 'org-modern-timestamp t)
(customize-set-variable 'org-descriptive-links t)
(customize-set-variable 'org-hide-emphasis-markers t)

;; Hooks
(add-hook 'org-mode-hook #'org-modern-mode)
(add-hook 'org-mode-hook #'org-appear-mode)
(add-hook 'org-mode-hook #'vt/org-mode-visual-fill)
(add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

(provide 'rational-org)
;;; rational-org.el ends here
