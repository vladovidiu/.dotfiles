;;; rational-elfeed.el -*- lexical-binding: t; -*-

;;; Code:
(straight-use-package 'elfeed)
(straight-use-package 'elfeed-org)
(straight-use-package 'elfeed-goodies)

(require 'elfeed)
(require 'elfeed-org)
(require 'elfeed-goodies)

(elfeed-org)
(setq rmh-elfeed-org-files (list "~/emacs-configs/emacs-default/elfeed.org"))

(elfeed-goodies/setup)

(provide 'rational-elfeed)
;;; rational-elfeed.el ends here
