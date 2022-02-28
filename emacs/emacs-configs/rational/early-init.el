;;; early-init.el -*- lexical-binding: t; -*-

;; Increase the GC threshold for faster startup
;; The default is 800kb. Measure in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Prefer loading newest compiled .el file
(setq load-prefer-newer noninteractive)

;; Native compilation settings
(when (featurep 'native-compile)
  ;; Silence compiler warnings
  (setq native-comp-async-report-warnings-errors nil)

  ;; Make native compilation happen async
  (setq native-comp-deferred-compilation t)

  ;; Set the right directory to store the native compilation cache
  (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory)))

;; Don't use package.el, but rather straight.el
(setq package-enable-at-startup nil)

;; Remove unneeded UI elements
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Make the initial buffer load faster
(setq initial-major-mode 'fundamental-mode)

;; Declare the rational-config-path
(defvar rational-config-path
  (let ((home-dir (getenv "HOME")))
    (expand-file-name "emacs-configs/rational" home-dir))
  "The rational emacs location")

;;; early-init.el ends here
