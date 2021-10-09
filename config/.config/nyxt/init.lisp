(in-package #:nyxt-user)

(define-configuration (buffer web-buffer)
	((default-modes (append '(dark-mode
							  vi-normal-mode
							  ) %slot-default%))))

(define-configuration (prompt-buffer)
	((default-modes (append '(vi-insert-mode) %slot-default%))))

(define-configuration (web-buffer nosave-buffer)
	((default-modes (append '(auto-mode
							  blocker-mode
							  force-https-mode
							  reduce-tracking-mode
							  ) %slot-default%))))

(define-configuration status-buffer
	((glyph-mode-presentation-p t)))

(define-configuration nyxt/force-https-mode:force-https-mode ((glyph "ϕ")))
(define-configuration nyxt/blocker-mode:blocker-mode ((glyph "β")))
(define-configuration nyxt/reduce-tracking-mode:reduce-tracking-mode
	((glyph "τ")))

(setf (uiop:getenv "WEBKIT_DISABLE_COMPOSITING_MODE") "1")
