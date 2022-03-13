;;; config.el; -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; This file sets what modules will be used, as well as
;; face attributes, themes, etc.

;;; Code:
(require 'rational-evil)
(require 'rational-ui)
(require 'rational-defaults)
(require 'rational-editing)
(require 'rational-completion)
(require 'rational-programming)
(require 'rational-corfu)
(require 'rational-lisp)

;; Programming Languages
(require 'rational-go)
;; (require 'rational-ruby)

;; Some personal variables
(defvar vt/default-font-size 160)
(defvar vt/default-variable-font-size 160)
(defvar vt/font-family "PragmataPro Mono Liga")
(defvar vt/variable-font-family "Iosevka Aile")
(defvar vt/config-location "~/emacs-configs/minimal")
(defvar vt/frame-transparency '(90 . 90))

;; Set the font and theme customization
(defun vt/set-font-faces ()
  (set-face-attribute 'default nil :font vt/font-family :height vt/default-font-size)
  (set-face-attribute 'fixed-pitch nil :font vt/font-family :height vt/default-font-size)
  (set-face-attribute 'variable-pitch nil :font vt/variable-font-family :height vt/default-variable-font-size :weight 'regular)
  (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
  (set-face-attribute 'font-lock-keyword-face nil :slant 'italic))

(if (daemonp)
	(add-hook 'after-make-frame-functions
			  (lambda (frame)
				(setq doom-modeline-icon t)
				(with-selected-frame frame
				  (vt/set-font-faces))))
  (vt/set-font-faces))

;; Set frame translucency
(set-frame-parameter (selected-frame) 'alpha vt/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,vt/frame-transparency))

(load-theme 'modus-vivendi t)
;;; config.el ends here
