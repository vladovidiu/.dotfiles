;;; rational-ui.el -*- lexical-binding: t; -*-

;; Install dependencies
(straight-use-package 'modus-themes)
(straight-use-package 'helpful)
(straight-use-package 'elisp-demos)
(straight-use-package 'lin)

;; Enable global line highlighting
(global-hl-line-mode 1)
(setq lin-face 'lin-mac)
(setq lin-mode-hooks
      '(bongo-mode-hook
        dired-mode-hook
        elfeed-search-mode-hook
        git-rebase-mode-hook
        grep-mode-hook
        ibuffer-mode-hook
        ilist-mode-hook
        ledger-report-mode-hook
        log-view-mode-hook
        magit-log-mode-hook
        mu4e-headers-mode
        notmuch-search-mode-hook
        notmuch-tree-mode-hook
        occur-mode-hook
        org-agenda-mode-hook
        proced-mode-hook
        tabulated-list-mode-hook))
(lin-global-mode)

;; Configure modus-themes
(setq modus-themes-completions  '((matches . (extrabold background))
                                  (selection . (semibold accented))
                                  (popup . (accented)))
      modus-themes-diffs nil
      modus-themes-mixed-fonts t
      modus-themes-fringes 'subtle
      modus-themes-hl-line '(intense accented)
      modus-themes-links '(faint)
      modus-themes-mode-line 'accented-3d
      modus-themes-mode-line '(padded accented)
      modus-themes-org-blocks 'tinted-background
      modus-themes-paren-match '(intense bold)
      modus-themes-prompts '(intense bold)
      modus-themes-subtle-line-numbers t
      modus-themes-syntax '(green-strings yellow-comments alt-syntax)
      modus-themes-region '(accented)
      )
(require 'modus-themes)
(modus-themes-load-vivendi)
(global-set-key (kbd "<f5>") #'modus-themes-toggle)

;; Make `describe-*' screens more helpful!
(require 'helpful)
(global-set-key [remap describe-function] #'helpful-callable)
(global-set-key [remap describe-variable] #'helpful-variable)
(global-set-key [remap describe-key] #'helpful-key)
(global-set-key (kbd "C-h F") #'helpful-function)
(global-set-key (kbd "C-h C") #'helpful-command)
(global-set-key (kbd "C-c C-d") #'helpful-at-point)

;; Bind extra `describe-*' commands
(global-set-key (kbd "C-h K") #'describe-keymap)

;;; Line Numbers
(defcustom rational-ui-line-numbers-enabled-modes
  '(conf-mode prog-mode)
  "Modes which should display line numbers."
  :type 'list
  :group 'rational)

(defcustom rational-ui-line-numbers-disabled-modes
  '(org-mode)
  "Modes which should not display line numbers.
Modes derived from the modes defined in
`rational-ui-line-number-enabled-modes', but should not display line numbers."
  :type 'list
  :group 'rational)

(defun rational-ui--enable-line-numbers-mode ()
  "Turn on line numbers mode.
Used as hook for modes which should display line numbers."
  (display-line-numbers-mode 1))

(defun rational-ui--disable-line-numbers-mode ()
  "Turn off line numbers mode.
Used as hook for modes which should not display line numebrs."
  (display-line-numbers-mode -1))

(defun rational-ui--update-line-numbers-display ()
  "Update configuration for line numbers display."
  (if rational-ui-display-line-numbers
      (progn
        (dolist (mode rational-ui-line-numbers-enabled-modes)
          (add-hook (intern (format "%s-hook" mode))
                    #'rational-ui--enable-line-numbers-mode))
        (dolist (mode rational-ui-line-numbers-disabled-modes)
          (add-hook (intern (format "%s-hook" mode))
                    #'rational-ui--disable-line-numbers-mode))
        (setq-default
         display-line-numbers-grow-only t
         display-line-numbers-type t
         display-line-numbers-width 2))
     (progn
       (dolist (mode rational-ui-line-numbers-enabled-modes)
         (remove-hook (intern (format "%s-hook" mode))
                      #'rational-ui--enable-line-numbers-mode))
       (dolist (mode rational-ui-line-numbers-disabled-modes)
         (remove-hook (intern (format "%s-hook" mode))
                      #'rational-ui--disable-line-numbers-mode)))))

(defcustom rational-ui-display-line-numbers t
  "Whether line numbers should be enabled."
  :type 'boolean
  :group 'rational
  :set (lambda (sym val)
         (set-default sym val)
         (rational-ui--update-line-numbers-display)))

;;;; Elisp-Demos

;; also add some examples
(require 'elisp-demos)
(advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)

;; add visual pulse when changing focus, like beacon but built-in
;; from from https://karthinks.com/software/batteries-included-with-emacs/
(defun pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-one-line (point)))

(dolist (command '(scroll-up-command scroll-down-command
				                     recenter-top-bottom other-window))
  (advice-add command :after #'pulse-line))

(provide 'rational-ui)
;;; rational-ui.el ends here
