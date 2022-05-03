;;; rational-sicp.el -*- lexical-binding: t; -*-

(straight-use-package 'geiser-mit)

(setq geiser-mit-binary "/usr/bin/scheme")
(setq geiser-active-implementations '(mit))

(provide 'rational-sicp)
;;; rational-sicp.el ends here
