(defvar vt/default-font-size 160)
(defvar vt/default-variable-font-size 160)

;; Make frame transparency overridable
(defvar vt/frame-transparency '(90 . 90))

;; New file location for dotemacs.org
(defvar vt/config-location "~/.dotfiles/emacs/emacs-configs/emacs-default/")

;; The default is 800 kilobytes. Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defun vt/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'vt/display-startup-time)

;; Set the correct eln-cache
(add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))

;; Silence compiler warnings as they can be pretty disruptive
(setq native-comp-async-report-warnings-errors nil)

(setq straight-check-for-modifications '(check-on-save find-when-checking))
(setq straight-repository-branch "develop")
;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;; Use straight.el for use-package expressions
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Load the helper package for commands like `straight-x-clean-unused-repos'
(require 'straight-x)

(use-package no-littering)

(setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(setq-default
 create-lockfiles nil            ; do not create lock files
 intent-tabs-mode nil            ; do not use hard tabs
 auto-save-default nil           ; do not auto save
 enable-recursive-minibuffers t  ; allow commands to be run on mini buffers
 auto-windows-vscroll nil        ; lighten vertical scroll
 delete-by-moving-to-trash t     ; delete files to trash
 fill-column 80                  ; set witdh for automatic line breaks
 help-window-select t            ; focus new help windows when opened
 show-trailing-whitespace nil    ; do not display trailing whitespaces
 tab-width 4                     ; set width for tabs
 vc-follow-symlinks t            ; follow links
 )

(cd "~/")                           ; cd into home
(delete-selection-mode 1)           ; replace region when inserting text
(fset 'yes-or-no-p 'y-or-n-p)       ; replace yes/no prompts with y/n
(global-auto-revert-mode t)         ; automatically update buffers if file changes on disk
(column-number-mode t)              ; display column number in mode line
(setq backup-directory-alist '(("." . "~/.save")))
(setq echo-keystrokes 0.1)
(setq x-stretch-cursor t)

(setq
 inhibit-splash-screen t
 initial-scratch-message nil
 initial-major-mode 'org-mode)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(use-package pinentry
  :init
  (pinentry-start)
  :config
  (setq epa-pinentry-mode 'loopback))

(recentf-mode 1)
(defvar recentf-max-saved-items)
(setq recentf-max-saved-items 100)

(progn
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (scroll-bar-mode -1)
  (show-paren-mode 1))

(setq display-line-numbers-width t)

(display-time-mode 1)

(setq scroll-conservatively 101)

;; Set frame transparency
(set-frame-parameter (selected-frame) 'alpha vt/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,vt/frame-transparency))

(setq org-src-window-setup 'split-window-right)
(setq display-buffer-alist
	  `(;; top side window
		("\\*Messages.*"
		 (display-buffer-in-side-window)
		 (window-height . 0.16)
		 (side . top)
		 (slot . 1))
		("\\*\\(Backtrace\\|Warnings\\|Flycheck errors\\|Compile-Log\\)\\*"
		 (display-buffer-in-side-window)
		 (window-height . 0.16)
		 (side . top)
		 (slot . 2)
		 (window-parameters . ((no-other-window . t))))
		;; bottom side window
		("\\*\\(Embark\\)?.*Completions.*"
		 (display-buffer-in-side-window)
		 (side . bottom)
		 (slot . 0)
		 (window-parameters . ((no-other-window . t)
							   (mode-line-format . none))))
		;; right side window
		("\\*Help.*"
		 (display-buffer-in-side-window)
		 (window-width . 0.4)			; See the :hook
		 (side . right)
		 (slot . 0))
		("\\*org-roam*"
		 (display-buffer-in-side-window)
		 (window-width . 0.4)			; See the :hook
		 (side . right)
		 (slot . 0))
		("\\*WoMan.*"
		 (display-buffer-in-side-window)
		 (window-width . 0.4)			; See the :hook
		 (side . right)
		 (slot . 1))
		("\\*Apropos\\*"
		 (display-buffer-in-side-window)
		 (window-width . 0.4)			; See the :hook
		 (side . right)
		 (slot . 2))
		;; bottom buffer (NOT side window)
		("\\*\\(Output\\|Register Preview\\).*"
		 (display-buffer-at-bottom))
		("\\*.*\\(e?shell\\|v?term\\).*"
		 (display-buffer-reuse-mode-window display-buffer-at-bottom)
		 (window-height . 0.2))
		;; below current window
		("\\*Calendar.*"
		 (display-buffer-reuse-mode-window display-buffer-below-selected)
		 (window-height . shrink-window-if-larger-than-buffer))))

(set-fringe-mode 10)

(defun vt/set-font-faces ()
  (set-face-attribute 'default nil :font "PragmataPro Mono Liga" :height vt/default-font-size)
  (set-face-attribute 'fixed-pitch nil :font "PragmataPro Mono Liga" :height vt/default-font-size)
  (set-face-attribute 'variable-pitch nil :font "Iosevka Aile" :height vt/default-variable-font-size :weight 'regular))

(if (daemonp)
	(add-hook 'after-make-frame-functions
			  (lambda (frame)
				(setq doom-modeline-icon t)
				(with-selected-frame frame
				  (vt/set-font-faces))))
  (vt/set-font-faces))

(use-package all-the-icons)

(use-package doom-themes
  :disabled t
  :init (load-theme 'doom-gruvbox t)
  ;; :init (load-theme 'modus-operandi t)
  ;; :init (load-theme 'modus-vivendi t)
  (doom-themes-visual-bell-config))

(use-package modus-themes
  :init
  (setq modus-themes-completions '((matches . (extrabold background))
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
  (modus-themes-load-themes)
  (global-hl-line-mode)
  ;; (setq modus-themes-headings
  ;;       (quote ((1 . (rainbow 1.4))
  ;;               (2 . (rainbow 1.3))
  ;;               (3 . (rainbow 1.2))
  ;;               (t . t))))
  :config
  (modus-themes-load-operandi)
  :bind
  ("<f5>" . #'modus-themes-toggle))

(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(defun vt/toggle-line-numbers ()
  "Toggle line numbers in buffer"
  (interactive)
  (setq display-line-numbers
		(not (bound-and-true-p display-line-numbers))))

(defun vt/toggle-corfu-mode ()
  "Toggle corfu mode in buffer"
  (interactive)
  (if (bound-and-true-p corfu-mode)
	  (corfu-mode -1)
	(corfu-mode 1)))

(defun vt/open-config-file ()
  "Load the literate config file for Emacs"
  (interactive)
  (find-file "~/emacs-configs/emacs-default/dotemacs.org"))

(defun vt/load-secret (&optional name)
  "Read a Lisp structure from the secret file.
When NAME is provided, return the value associated to this key."
  (let ((file (expand-file-name ".secrets.eld")))
	(when (file-exists-p file)
	  (with-demoted-errors "Error while parsing secret file: %S"
		(with-temp-buffer
		  (insert-file-contents file)
		  (if-let ((content (read (buffer-string)))
				   (name))
			  (alist-get name content)
			content))))))

(defun vt/fetch-password (&rest params)
  (require 'auth-source)
  (let ((match (car (apply 'auth-source-search params))))
	(if match
		(let ((secret (plist-get match :secret)))
		  (if (functionp secret)
			  (funcall secret)
			secret))
	  (error "Password not found for %S" params))))

(defun vt/with-faicon (icon str &optional height v-adjust)
  (s-concat (all-the-icons-faicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

(defun vt/with-fileicon (icon str &optional height v-adjust)
  (s-concat (all-the-icons-fileicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

(defun vt/with-octicon (icon str &optional height v-adjust)
  (s-concat (all-the-icons-octicon icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

(defun vt/with-material (icon str &optional height v-adjust)
  (s-concat (all-the-icons-material icon :v-adjust (or v-adjust 0) :height (or height 1)) " " str))

(defun vt/start-github-review ()
  (interactive)
  (github-review-forge-pr-at-point))

(defun vt/elfeed-play-with-mpv ()
  "Play entry link with mpv."
  (interactive)
  (let ((entry (if (eq major-mode 'elfeed-show-mode) elfeed-show-entry (elfeed-search-selected :single)))
        (quality-arg "")
        (quality-val (completing-read "Max height resolution (0 for unlimited): " '("0" "480" "720") nil nil)))
    (setq quality-val (string-to-number quality-val))
    (message "Opening %s with height≤%s with mpv..." (elfeed-entry-link entry) quality-val)
    (when (< 0 quality-val)
      (setq quality-arg (format "--ytdl-format=[height<=?%s]" quality-val)))
    (start-process "elfeed-mpv" nil "mpv" quality-arg (elfeed-entry-link entry))))

(defun vt/open-with-mpv ()
  "Get URL at point and open it in mpv."
  (interactive)
  (when (org-in-regexp org-bracket-link-regexp 1)
	(let* ((url (org-link-unescape (org-match-string-no-properties 1)))
		   (quality-arg "")
		   (quality-val (completing-read "Max resolution (0 for unlimited): " '("0" "480" "720") nil nil)))
	  (if (not url)
		  (error "No url copied!")
		(setq quality-val (string-to-number quality-val))
		(message (concat "Opening: " url))
		(when (< 0 quality-val)
		  (setq quality-arg (format "--ytdl-format=[height<=?%s]" quality-val)))
		(start-process "org-mpv" nil "mpv" quality-arg url)))))

(defun vt/eww-rename-buffer ()
  "Rename EWW buffer using page title or URL."
  (let ((name (if (eq "" (plist-get eww-data :title))
				  (plist-get eww-data :url)
				(plist-get eww-data :title))))
	(rename-buffer (format "*%s # eww*" name) t)))

(defun +org--insert-item (direction)
  (let ((context (org-element-lineage
                  (org-element-context)
                  '(table table-row headline inlinetask item plain-list)
                  t)))
    (pcase (org-element-type context)
      ;; Add a new list item (carrying over checkboxes if necessary)
      ((or `item `plain-list)
       ;; Position determines where org-insert-todo-heading and org-insert-item
       ;; insert the new list item.
       (if (eq direction 'above)
           (org-beginning-of-item)
         (org-end-of-item)
         (backward-char))
       (org-insert-item (org-element-property :checkbox context))
       ;; Handle edge case where current item is empty and bottom of list is
       ;; flush against a new heading.
       (when (and (eq direction 'below)
                  (eq (org-element-property :contents-begin context)
                      (org-element-property :contents-end context)))
         (org-end-of-item)
         (org-end-of-line)))

      ;; Add a new table row
      ((or `table `table-row)
       (pcase direction
         ('below (save-excursion (org-table-insert-row t))
                 (org-table-next-row))
         ('above (save-excursion (org-shiftmetadown))
                 (+org/table-previous-row))))

      ;; Otherwise, add a new heading, carrying over any todo state, if
      ;; necessary.
      (_
       (let ((level (or (org-current-level) 1)))
         ;; I intentionally avoid `org-insert-heading' and the like because they
         ;; impose unpredictable whitespace rules depending on the cursor
         ;; position. It's simpler to express this command's responsibility at a
         ;; lower level than work around all the quirks in org's API.
         (pcase direction
           (`below
            (let (org-insert-heading-respect-content)
              (goto-char (line-end-position))
              (org-end-of-subtree)
              (insert "\n" (make-string level ?*) " ")))
           (`above
            (org-back-to-heading)
            (insert (make-string level ?*) " ")
            (save-excursion (insert "\n"))))
         (when-let* ((todo-keyword (org-element-property :todo-keyword context))
                     (todo-type    (org-element-property :todo-type context)))
           (org-todo
            (cond ((eq todo-type 'done)
                   ;; Doesn't make sense to create more "DONE" headings
                   (car (+org-get-todo-keywords-for todo-keyword)))
                  (todo-keyword)
                  ('todo)))))))

    (when (org-invisible-p)
      (org-show-hidden-entry))
    (when (and (bound-and-true-p evil-local-mode)
               (not (evil-emacs-state-p)))
      (evil-insert 1))))

(defun +org/insert-item-below (count)
  "Inserts a new heading, table cell or item below the current one."
  (interactive "p")
  (dotimes (_ count) (+org--insert-item 'below)))

(defun +org/insert-item-above (count)
  "Inserts a new heading, table cell or item above the current one."
  (interactive "p")
  (dotimes (_ count) (+org--insert-item 'above)))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package general
  :config
  (general-create-definer vt/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (vt/leader-keys
   "c" '(:ignore t :which-key "config")
   "cc" '(vt/open-config-file :which-key "open config")
   "t" '(:ignore t :which-key "toggles")
   "tl" '(vt/toggle-line-numbers :which-key "line numbers")
   "tc" '(vt/toggle-corfu-mode :which-key "corfu mode")
   "tt" '(consult-theme :which-key "choose theme")))

(use-package hydra
  :defer t)

(use-package pretty-hydra
  :straight t)

(pretty-hydra-define vt/hydra-text-scale
  (:title "Increase/decrease text size"
		  :quit-key "q"
		  :timeout 4)
  ("Scale text"
   (("j" text-scale-increase "in")
	("k" text-scale-decrease "out")
	("f" nil "finished" :exit t))))

(vt/leader-keys
  "ts" '(vt/hydra-text-scale/body :which-key "scale text"))

(setq display-buffer-base-action
	  '(display-buffer-reuse-mode-window
		display-buffer-reuse-window
		display-buffer-same-window))

;; If a popup does happen, don't resize windows to be equal-sized
(setq even-window-sizes nil)

(use-package selectrum
  :straight t
  :disabled t
  :config
  (selectrum-mode +1)
  :custom
  (selectrum-extend-current-candidate-highlight t)
  (selectrum-fix-vertical-window-height t))

(use-package selectrum-prescient
  :after selectrum
  :disabled t
  :init
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))

(use-package vertico
  :straight '(vertico :host github
					  :repo "minad/vertico"
					  :branch "main")
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package savehist
  :config
  (setq history-length 25)
  (savehist-mode 1))

(use-package consult
  :straight t
  :bind (([remap list-buffers] . consult-buffer)
		 ("C-c h" . consult-history)
		 ("C-c m" . consult-mode-command)
		 ("C-c b" . consult-bookmark)
		 ("C-s" . consult-line)
		 ("C-x b" . consult-buffer)
		 ("M-y" . consult-yank-pop)
		 ("M-g g" . consult-goto-line)
		 ("M-g M-g" . consult-goto-line)
		 ("M-g o" . consult-outline)
		 ("M-g i" . consult-imenu)
		 ("M-g I" . consult-project-imenu)
		 ("M-s f" . consult-find)
		 ("M-s L" . consult-locate)
		 ("M-s G" . consult-git-grep)
		 ("M-s r" . consult-ripgrep)
		 ("M-s l" . consult-line)
		 ("M-s e" . consult-isearch)
		 :map isearch-mode-map
		 ("M-e" . consult-isearch)
		 ("M-s e" . consult-isearch)
		 ("M-s l" . consult-line))
  :init
  (setq xref-show-xrefs-function #'consult-xref
		xref-show-definitions-function #'consult-xref)
  :config
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root))

(use-package consult-flycheck
  :bind (:map flycheck-command-map
			  ("!" . consult-flycheck)))

(use-package consult-dir
  :straight t
  :bind (("C-x C-d" . consult-dir)
		 :map vertico-map
		 ("C-x C-d" . consult-dir)
		 ("C-x C-j" . consult-dir-jump-file)))

(vt/leader-keys
  "b" '(:ignore t :which-key "buffer")
  "bb" '(consult-buffer :which-key "list buffers")
  "bB" '(persp-switch-to-buffer* :which-key "list buffers")
  "bs" '(save-buffer :which-key "save buffer"))

(use-package orderless
  :straight t
  :init
  (setq completion-styles '(orderless)
		completion-category-defaults nil
		completion-category-overrides '((file (styles . (partial-completion))))))

(use-package embark
  :straight t
  :bind
  (("C-;" . embark-act)	  ;; pick some comfortable binding
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . embark-consult-preview-minor-mode))

(use-package marginalia
  :after vertico
  :straight t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(use-package doom-modeline
  :custom-face
  (mode-line ((t (:height 0.9))))
  (mode-line-inactive ((t (:height 0.9))))
  :custom
  (doom-modeline-bar-width 3)
  (doom-modeline-height 32)
  (doom-modeline-buffer-file-name-style 'truncate-except-project)
  :init (doom-modeline-mode 1))

(defun doom-modeline-conditional-buffer-encoding ()
  "We expect the encoding to be LF UTF-8, so only show the modeline when this is not the case"
  (setq-local doom-modeline-buffer-encoding
              (unless (or (eq buffer-file-coding-system 'utf-8-unix)
                          (eq buffer-file-coding-system 'utf-8)))))

(add-hook 'after-change-major-mode-hook #'doom-modeline-conditional-buffer-encoding)

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  (setq evil-want-integration t)
  (setq evil-search-module 'evil-search)
  (setq evil-ex-complete-emacs-commands nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-shift-round nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-undo-system 'undo-fu)
  :config
  (evil-mode 1))

;; Use visual line motions even outside of visual-line-mode buffers
(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)

(use-package undo-fu
  :config
  (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
  (define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo))

(use-package undo-fu-session
  :config
  (global-undo-fu-session-mode))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-escape
  :after evil
  :config
  (setq evil-escape-excluded-states '(normal visual multiedit emacs motion))
  (setq-default evil-escape-key-sequence "jk"
                evil-escape-delay 0.15)
  (evil-escape-mode))

(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :disabled t
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3))

(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :demand t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/code")
    (setq projectile-project-search-path '("~/code")))
  (setq projectile-switch-project-action #'projectile-dired)
  :custom
  (add-to-list 'projectile-project-root-files "Gemfile"))

(vt/leader-keys
  "p" '(:ignore t :which-key "projectile")
  "pp" '(projectile-switch-project :which-key "switch project")
  "pf" '(projectile-find-file :which-key "find project file")
  "sp" '(consult-ripgrep :which-key "search in project"))

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))


(vt/leader-keys
  "g" '(:ignore t :which-key "magit")
  "gg" '(magit-status :which-key "magit status")
  "gb" '(magit-blame :which-key "magit blame"))

(use-package forge
  :after magit)

(use-package github-review
  :after magit
  :config
  (transient-append-suffix 'forge-dispatch "c u"
	'("c r" "Review pull request" vt/start-github-review)))

(defun vt/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
						  '(("^ *\\([-]\\) "
							 (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (set-face-attribute 'org-document-title nil :font "Iosevka Aile" :weight 'bold :height 1.3)
  (dolist (face '((org-level-1 . 1.2)
				  (org-level-2 . 1.1)
				  (org-level-3 . 1.05)
				  (org-level-4 . 1.0)
				  (org-level-5 . 1.1)
				  (org-level-6 . 1.1)
				  (org-level-7 . 1.1)
				  (org-level-8 . 1.1)))
	(set-face-attribute (car face) nil :font "Iosevka Aile" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(defun vt/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :commands (org-capture org-agenda)
  :hook (org-mode . vt/org-mode-setup)
  :bind (("C-x y" . vt/open-with-mpv)
		 :map org-mode-map
		 ([tab] . #'org-cycle)
		 ("C-RET" . #'+org/insert-item-below)
		 ([C-return] . #'+org/insert-item-below)
		 ("C-S-RET" . #'+org/insert-item-above)
		 ([C-S-return] . #'+org/insert-item-above)
		 ("C-M-RET" . #'org-insert-subheading)
		 ([C-M-return] . #'org-insert-subheading))
  :config
  (setq org-ellipsis " ▾"
		org-hide-emphasis-markers t
		org-src-fontify-natively t
		org-fontify-quote-and-verse-blocks t
		org-return-follows-link t
		org-cycle-separator-lines 2
		org-edit-src-content-indentation 2
		org-src-tab-acts-natively t
		org-src-preserve-indentation t
		org-fontify-done-headline t
		org-agenda-start-with-log-mode t
		org-log-done 'time
		org-log-into-drawer t
		org-roam-completion-everywhere t)

  (evil-define-key '(normal insert visual) org-mode-map (kbd "C-j") 'org-next-visible-heading)
  (evil-define-key '(normal insert visual) org-mode-map (kbd "C-k") 'org-previous-visible-heading)

  (evil-define-key '(normal insert visual) org-mode-map (kbd "M-j") 'org-metadown)
  (evil-define-key '(normal insert visual) org-mode-map (kbd "M-k") 'org-metaup)

  (setq org-agenda-files (list "~/Documents/org"))

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
          (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  (setq org-refile-targets
        '(("Archive.org" :maxlevel . 1)
          ("Tasks.org" :maxlevel . 1)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-tag-alist
        '((:startgroup)
		  ; Put mutually exclusive tags here
		  (:endgroup)
		  ("@errand" . ?E)
		  ("@home" . ?H)
		  ("@work" . ?W)
		  ("agenda" . ?a)
		  ("planning" . ?p)
		  ("publish" . ?P)
		  ("batch" . ?b)
		  ("note" . ?n)
		  ("idea" . ?i)))

  (setq org-agenda-custom-commands
        '(("d" "Dashboard"
           ((agenda "" ((org-deadline-warning-days 7)))
            (todo "NEXT"
                  ((org-agenda-overriding-header "Next Tasks")))
            (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

          ("n" "Next Tasks"
           ((todo "NEXT"
                  ((org-agenda-overriding-header "Next Tasks")))))

          ("W" "Work Tasks" tags-todo "+work-email")

          ;; Low-effort next actions
          ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
           ((org-agenda-overriding-header "Low Effort Tasks")
            (org-agenda-max-todos 20)
            (org-agenda-files org-agenda-files)))

          ("w" "Workflow Status"
           ((todo "WAIT"
                  ((org-agenda-overriding-header "Waiting on External")
                   (org-agenda-files org-agenda-files)))
            (todo "REVIEW"
                  ((org-agenda-overriding-header "In Review")
                   (org-agenda-files org-agenda-files)))
            (todo "PLAN"
                  ((org-agenda-overriding-header "In Planning")
                   (org-agenda-todo-list-sublevels nil)
                   (org-agenda-files org-agenda-files)))
            (todo "BACKLOG"
                  ((org-agenda-overriding-header "Project Backlog")
                   (org-agenda-todo-list-sublevels nil)
                   (org-agenda-files org-agenda-files)))
            (todo "READY"
                  ((org-agenda-overriding-header "Ready for Work")
                   (org-agenda-files org-agenda-files)))
            (todo "ACTIVE"
                  ((org-agenda-overriding-header "Active Projects")
                   (org-agenda-files org-agenda-files)))
            (todo "COMPLETED"
                  ((org-agenda-overriding-header "Completed Projects")
                   (org-agenda-files org-agenda-files)))
            (todo "CANC"
                  ((org-agenda-overriding-header "Cancelled Projects")
                   (org-agenda-files org-agenda-files)))))))

  (setq org-capture-templates
        `(("t" "Tasks / Projects")
          ("tt" "Task" entry (file+olp "~/Documents/org/Tasks.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

          ("j" "Journal Entries")
          ("jj" "Journal" entry
           (file+olp+datetree "~/Documents/org/Journal.org")
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
           :clock-in :clock-resume
           :empty-lines 1)
          ("jm" "Meeting" entry
           (file+olp+datetree "~/Documents/org/Journal.org")
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)

          ("w" "Workflows")
          ("we" "Checking Email" entry (file+olp+datetree "~/Documents/org/Journal.org")
           "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

          ("m" "Metrics Capture")
          ("mw" "Weight" table-line (file+headline "~/Documents/org/Metrics.org" "Weight")
           "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

  (define-key global-map (kbd "C-c j")
    (lambda () (interactive) (org-capture nil "jj")))

  (vt/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode))

(defun vt/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . vt/org-mode-visual-fill))

(use-package org-appear
  :hook (org-mode . org-appear-mode))

(use-package org-roam
  :straight t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/code/org-roam")
  (org-roam-completion-eveywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain
	  "%?"
	  :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
	  :unnarowed t)
	 ("l" "programming language" plain
	  "* Characteristics\n\n- Family: %?\n- Inspired by: \n\n* Reference:\n\n"
	  :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
	  :unnarrowed t)
	 ("b" "book notes" plain
	  "\n* Source\n\nAuthor: %^{Author}\nTitle: ${title}\nYear: %^{Year}\n\n* Summary\n\n%?"
	  :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
	  :unnarrowed t)
	 ("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
	  :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Project")
	  :unnarrowed t)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
		 ("C-c n f" . org-roam-node-find)
		 ("C-c n i" . org-roam-node-insert)
		 :map org-mode-map
		 ("C-M-i"   . completion-at-point))
  :config
  (org-roam-setup))

(use-package deft
  :after org
  :commands (deft)
  :config (setq deft-directory "~/code/org-roam"
				deft-recursive t
				deft-extensions '("org")
				deft-use-filename-as-title t)
  :bind (("C-c n n" . deft)))

(use-package dired
  :ensure nil
  :straight nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))

(use-package dired-single
  :after dired)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-open
  :config
  ;; Doesn't work as expected!
  ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
  (setq dired-open-extensions '(("png" . "feh")
                                ("mkv" . "mpv"))))

(vt/leader-keys
  "d" '(:ignore t :which-key "dired")
  "dj" '(dired-jump :which-key "jump to folder"))

;; (use-package dired-hide-dotfiles
;;   :hook (dired-mode . dired-hide-dotfiles-mode)
;;   :config
;;   (evil-collection-define-key 'normal 'dired-mode-map
;;     "H" 'dired-hide-dotfiles-mode))

(defun vt/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Bind some useful keys for evil-mode
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-normalize-keymaps)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt
  :after eshell)

(use-package eshell
  :hook (eshell-first-time-mode . vt/configure-eshell)
  :config

  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim")))

  (eshell-git-prompt-use-theme 'powerline))

(use-package emojify
  :hook (after-init . global-emojify-mode))

(use-package elfeed
  :after evil
  :commands elfeed
  :bind (
		 :map elfeed-search-mode-map
		 ("C-x y" . vt/elfeed-play-with-mpv))
  :config
  (setq elfeed-search-feed-face ":foreground #ff0000 :weight bold")
  (setq-default elfeed-search-filter "@1-week-ago +unread "))

(use-package elfeed-org
  :after elfeed
  :config (setq rmh-elfeed-org-files (list "~/emacs-configs/emacs-default/elfeed.org"))
  :init
  (elfeed-org))

(use-package elfeed-goodies
  :after elfeed
  :init
  (elfeed-goodies/setup))

(use-package lispy
  :hook ((emacs-lisp-mode . lispy-mode)
         (scheme-mode . lispy-mode)))

(use-package lispyville
  :hook ((lispy-mode . lispyville-mode))
  :config
  (lispyville-set-key-theme '(operators c-w additional
										additional-movement slurp/barf-cp
										prettify)))

(use-package smartparens
  :hook (prog-mode . smartparens-mode))

(use-package avy
  :commands (avy-goto-char avy-goto-word-0 avy-goto-line))

(vt/leader-keys
  "j" '(:ignore t :which-key "jump")
  "jj" '(avy-goto-char :which-key "jump to char")
  "jw" '(avy-goto-word-0 :which-key "jump to word")
  "jl" '(avy-goto-line :which-key "jump to line"))

(use-package ace-window
  :straight t
  :config
  (setq aw-dispatch-always t)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(global-set-key (kbd "M-o") 'ace-window)

(use-package dashboard
  :disabled t
  :init
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "Welcome Vlad!")
  (setq dashboard-center-content nil)
  (setq dashboard-items '((recents . 5)
						  (agenda . 5)
						  (bookmarks . 3)
						  (projects . 3)
						  (registers . 3)))
  :config
  (dashboard-setup-startup-hook))

;; (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

(use-package smudge
  :commands smudge-controller-toggle-play
  :custom
  (smudge-oauth2-client-id (vt/load-secret 'spotify-id))
  (smudge-oauth2-client-secret (vt/load-secret 'spotify-secret)))

(pretty-hydra-define vt/hydra-spotify
  (:title (vt/with-faicon "spotify" "Spotify" 1 -0.05))
  ("Search"
   (("t" smudge-track-search "Track" :exit t)
	("m" smudge-my-playlists "My Playlists" :exit t)
	("f" smudge-featured-playlists "Featured Playlists" :exit t)
	("u" smudge-user-playlists "User Playlists" :exit t)
	("l" smudge-playlist-search "Search Playlist" :exit t))
   "Control"
   (
	("SPC" smudge-controller-toggle-play "Play/Pause" :exit nil)
	("n" smudge-controller-next-track "Next Track" :exit nil)
	("p" smudge-controller-previous-track "Previous Track" :exit nil)
	("r" smudge-controller-toggle-repeat "Repeat" :exit nil)
	("s" smudge-controller-toggle-shuffle "Shuffle" :exit nil))
   "Manage"
   (
	("+" smudge-controller-volume-up "Volume up" :exit nil)
	("-" smudge-controller-volume-down "Volume down" :exit nil)
	("x" smudge-controller-volume-mute-unmute "Mute" :exit nil)
	("d" smudge-select-device "Select Device" :exit nil)
	("f" nil "quit" :exit t))))

(vt/leader-keys
  "m" '(vt/hydra-spotify/body :which-key "spotify"))

(setq shr-use-fonts nil
	  shr-cookie-policy nil
	  shr-discard-aria-hidden t
	  shr-image-animate nil)

(use-package eww
  :config
  (setq browse-url-browser-function 'eww-browse-url
		eww-search-prefix "https://duckduckgo.com/html?q="))

(add-hook 'eww-after-render-hook #'vt/eww-rename-buffer)
(advice-add 'eww-back-url :after #'vt/eww-rename-buffer)
(advice-add 'eww-forward-url :after #'vt/eww-rename-buffer)

(use-package sly
  :straight t
  :config
  (setq sly-lisp-implementations
		`(
		  (roswell ("ros" "run"))
		  (roswell-sbcl ("ros" "-L" "sbcl" "-Q" "-l" "~/.sbclrc" "run") :coding-system utf-8-unix)))
  (setq sly-default-lisp 'roswell)
  (setq sly-symbol-completion-mode t)
  (setq org-babel-lisp-eval-fn #'sly-eval)
  (setq inferior-lisp-program "ros run"))

(use-package slime
  :straight t
  :disabled t
  )

(defun vt/circe-nickserv-password (server)
  (vt/fetch-password :login "nerevarine" :machine "irc.libera.chat"))

(use-package circe
  :init
  (enable-circe-color-nicks)
  :custom
  (setq circe-default-nick (vt/load-secret 'circe-nick)
		circe-default-realname (vt/load-secret 'circe-nick)
		circe-reduce-lurker-spam t
		circe-network-options
		'(("Libera Chat"
		   :tls t
		   :nickserv-password (vt/load-secret 'circe-pass)
		   :nickserv-identify-challenge (format "\C-b/msg\\s-NickServ\\s-identify\\s-<%S>\C-b" (vt/load-secret 'circe-pass))
		   :nickserv-identify-command (format "PRIVMSG NickServ :IDENTIFY {%s} {%s}" (vt/load-secret 'circe-nick) (vt/load-secret 'circe-pass))
		   :nickserv-identify-confirmation "^You are now identified for .*\\.$"
		   :channels (:after-auth "#emacs"))
		  )))

(use-package restart-emacs
  :straight t)

(use-package perspective
  :after projectile
  :disabled t
  :straight t
  :bind (("C-x k" . persp-kill-buffer*)
		 ("C-x b" . persp-switch-to-buffer*)
		 ("C-x C-b" . persp-list-buffers))
  :config
  (persp-mode))

(use-package persp-projectile
  :after projectile perspective
  :disabled t
  :straight t)

(use-package nix-mode
  :mode "\\.nix\\'")

(defvar-local +electric-indent-words '()
  "The list of electric words. Typing these will trigger reindentation of the
current line.")

(use-package electric
  :config
  (setq-default electric-indent-chars '(?\n ?\^?))
  :custom
  (add-hook 'electric-indent-functions
			(defun +electric-indent-char-fn (_c)
			  (when (and (eolp) +electric-indent-words)
				(save-excursion
				  (backward-word)
				  (looking-at-p (concat "\\<" (regexp-opt +electric-indent-words))))))))


(defun set-electric! (modes &rest plist)
  "Declare that WORDS (list of strings) or CHARS (lists of chars) should trigger
electric indentation.
Enables `electric-indent-local-mode' in MODES.
\(fn MODES &key WORDS CHARS)"
  (declare (indent defun))
  (dolist (mode (doom-enlist modes))
	(let ((hook (intern (format "%s-hook" mode)))
		  (fn (intern (format "+electric--init-%s-h" mode))))
	  (cond ((null (car-safe plist))
			 (remove-hook hook fn)
			 (unintern fn nil))
			((fset
			  fn (lambda ()
				   (when (eq major-mode mode)
					 (setq-local electric-indent-inhibit nil)
					 (cl-destructuring-bind (&key chars words) plist
					   (electric-indent-local-mode 1)
					   (if chars (setq-local electric-indent-chars chars))
					   (if words (setq +electric-indent-words words))))))
			 (add-hook hook fn))))))

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
	 (lisp . t)
	 (js . t)
	 (ruby . t)
	 (python . t)))

  (setq org-confirm-babel-evaluate nil)
  (push '("conf-unix" . conf-unix) org-src-lang-modes))

(with-eval-after-load 'org
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("js" . "src js"))
  (add-to-list 'org-structure-template-alist '("ru" . "src ruby"))
  (add-to-list 'org-structure-template-alist '("lp" . "src lisp")))

;; Automatically tangle our Emacs.org config file when we save it
(defun vt/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "dotemacs.org" vt/config-location))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'vt/org-babel-tangle-config)))

(setenv "PATH"
  (concat
   "/home/vladovidiu/.rbenv/shims" ":"
   (getenv "PATH")
  )
)

(use-package lsp-mode
  :straight t
  :commands (lsp lsp-deferred)
  :hook ((typescript-mode js2-mode web-mode ruby-mode) . lsp)
  :init
  (setq lsp-keymap-prefix "C-c l") ;; Or 'C-l', 's-l'
  :custom
  (lsp-headerline-breadcrumb-enable nil)
  (add-hook 'lsp-completion-mode-hook
			(defun +lsp-init-company-backends-h ()
			  (when lsp-completion-mode
				(set (make-local-variable 'company-backends)
					 (cons +lsp-company-backends
						   (remove +lsp-company-backends
								   (remq 'company-capf company-backends))))))))

  ;; :config
  ;; (lsp-enable-which-key-integration t))

(use-package consult-lsp
  :straight t
  :custom
  (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-peek-enable t
		lsp-ui-doc-max-height 8
		lsp-ui-doc-max-width 72
		lsp-ui-doc-delay 0.75
		lsp-ui-doc-position 'at-point
		lsp-ui-sideline-ignore-duplicate t))

(use-package corfu
  :straight '(corfu :host github
					:repo "minad/corfu"
					:branch "main")
  :bind (:map corfu-map
			  ("TAB" . corfu-next)
			  ([tab] . corfu-next)
			  ("S-TAB" . corfu-previous)
			  ([backtab] . corfu-previous))
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0)
  (corfu-min-width 60)
  (corfu-max-width 60)
  (corfu-count 14)
  (corfu-scroll-margin 4)
  (corfu-echo-documentation 0.25)
  (corfu-separator ?\s)
  (corfu-quit-no-match 'separator)
  :hook ((prog-mode . corfu-mode)
		 (shell-mode . corfu-mode)
		 (org-mode . corfu-mode)
		 (sly-mode . corfu-mode)
		 (eshell-mode . corfu-mode))
  :config
  (global-corfu-mode 1))

(use-package cape
  :init
  :after corfu
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package corfu-doc
  :straight '(corfu-doc :host github
						:repo "galeo/corfu-doc"
						:branch "main")
  :custom
  (corfu-doc-max-width 60)
  (corfu-doc-max-height 20)
  :hook ((corfu-mode . corfu-doc-mode)))

(use-package company
  :after lsp-mode
  :disabled t
  :init
  (global-company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (add-hook 'global-company-mode-hook #'evil-normalize-keymaps)
  (setq company-minimum-prefix-length 2
		company-tooltip-limit 14
		company-tooltip-align-annotations t
		company-require-match 'never
		))

(use-package company-box
  :hook (company-mode . company-box-mode)
  :disabled t
  :config
  (setq company-box-show-single-candidate t
		company-box-backends-colors nil
		company-box-max-candidates 50
		company-box-icons-alist 'company-box-icons-all-the-icons
		;; Move company-box-icons--elisp to the end, because it has a catch-all
		;; clause that ruins icons from other backends in elisp buffers.
		company-box-icons-functions
		(cons #'+company-box-icons--elisp-fn
			  (delq 'company-box-icons--elisp
					company-box-icons-functions))
		company-box-icons-all-the-icons
		(let ((all-the-icons-scale-factor 0.8))
		  `((Unknown . ,(all-the-icons-material "find_in_page" :face 'all-the-icons-purple))
			(Text . ,(all-the-icons-material "text_fields" :face 'all-the-icons-green))
			(Method . ,(all-the-icons-material "functions" :face 'all-the-icons-red))
			(Function . ,(all-the-icons-material "functions" :face 'all-the-icons-red))
			(Constructor . ,(all-the-icons-material "functions" :face 'all-the-icons-red))
			(Field . ,(all-the-icons-material "functions" :face 'all-the-icons-red))
			(Variable . ,(all-the-icons-material "adjust" :face 'all-the-icons-blue))
			(Class . ,(all-the-icons-material "class" :face 'all-the-icons-red))
			(Interface . ,(all-the-icons-material "settings_input_component" :face 'all-the-icons-red))
			(Module . ,(all-the-icons-material "view_module" :face 'all-the-icons-red))
			(Property . ,(all-the-icons-material "settings" :face 'all-the-icons-red))
			(Unit . ,(all-the-icons-material "straighten" :face 'all-the-icons-red))
			(Value . ,(all-the-icons-material "filter_1" :face 'all-the-icons-red))
			(Enum . ,(all-the-icons-material "plus_one" :face 'all-the-icons-red))
			(Keyword . ,(all-the-icons-material "filter_center_focus" :face 'all-the-icons-red))
			(Snippet . ,(all-the-icons-material "short_text" :face 'all-the-icons-red))
			(Color . ,(all-the-icons-material "color_lens" :face 'all-the-icons-red))
			(File . ,(all-the-icons-material "insert_drive_file" :face 'all-the-icons-red))
			(Reference . ,(all-the-icons-material "collections_bookmark" :face 'all-the-icons-red))
			(Folder . ,(all-the-icons-material "folder" :face 'all-the-icons-red))
			(EnumMember . ,(all-the-icons-material "people" :face 'all-the-icons-red))
			(Constant . ,(all-the-icons-material "pause_circle_filled" :face 'all-the-icons-red))
			(Struct . ,(all-the-icons-material "streetview" :face 'all-the-icons-red))
			(Event . ,(all-the-icons-material "event" :face 'all-the-icons-red))
			(Operator . ,(all-the-icons-material "control_point" :face 'all-the-icons-red))
			(TypeParameter . ,(all-the-icons-material "class" :face 'all-the-icons-red))
			(Template . ,(all-the-icons-material "short_text" :face 'all-the-icons-green))
			(ElispFunction . ,(all-the-icons-material "functions" :face 'all-the-icons-red))
			(ElispVariable . ,(all-the-icons-material "check_circle" :face 'all-the-icons-blue))
			(ElispFeature . ,(all-the-icons-material "stars" :face 'all-the-icons-orange))
			(ElispFace . ,(all-the-icons-material "format_paint" :face 'all-the-icons-pink)))))

  (add-to-list 'company-box-frame-parameters '(tab-bar-lines . 0))

  (defun +company-box-icons--elisp-fn (candidate)
	(when (derived-mode-p 'emacs-lisp-mode)
	  (let ((sym (intern candidate)))
		(cond ((fboundp sym) 'ElispFunction)
			  ((boundp sym) 'ElispVariable)
			  ((featurep sym) 'ElispFeature)
			  ((facep sym) 'ElispFace))))))

(use-package tree-sitter
  :after evil
  :init (global-tree-sitter-mode)
  :hook (tree-sitter-after-on-hook . tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :after evil tree-sitter
  :config
  (tree-sitter-require 'tsx)
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx)))

(use-package flycheck
  :after lsp
  :hook (lsp-mode . flycheck-mode))

(vt/leader-keys
  "c" '(:ignore t :which-key "code")
  "ca" '(lsp-execute-code-action :which-key "code action")
  "cx" '(consult-flycheck :which-key "flycheck info"))

(use-package editorconfig
  :straight t
  :config
  (editorconfig-mode 1))

(use-package typescript-mode
  :hook ((typescript-mode . lsp-deferred)
		 (typescript-mode . rainbow-delimiters-mode)
		 (typescript-mode . tree-sitter-hl-mode))
  :config
  (setq typescript-indent-level 2))

(defun vt/set-js-indentation ()
  (setq js-indent-level 2)
  (setq evil-shift-width js-indent-level)
  (setq-default tab-width 2))

(use-package js2-mode
  :mode "\\.[mc]?js\\'"
  :mode "\\.es6\\'"
  :interpreter "node"
  :commands js2-line-break
  :config
  (setq js-chain-indent t
		;; Don't mishighlight shebang lines
		js2-skip-preprocessor-directives t
		;; let flycheck handle this
		js2-mode-show-parse-errors nil
		js2-mode-show-strict-warnings nil
		;; Flycheck provides these features, so disable them: conflicting with
		;; the eslint settings.
		js2-strict-trailing-comma-warning nil
		js2-strict-missing-semi-warning nil
		;; maximum fontification
		js2-highlight-level 3
		js2-highlight-external-variables t
		js2-idle-timer-delay 0.1)

  (add-hook 'js2-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'js2-mode-hook #'vt/set-js-indentation))

(use-package apheleia
  :hook (typescript-mode . apheleia-mode))

(progn
  (define-derived-mode typescript-tsx-mode web-mode "TypeScript-tsx")
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-tsx-mode)))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))

(use-package web-mode
  :mode "(\\.\\(html?\\|ejs\\|tsx\\|jsx\\)\\'"
  :config
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-attribute-indent-offset 2))

(use-package json-mode
  :straight t
  :config
  (setq json-reformat:indent-width 2))

(use-package rustic
  :straight t
  :bind (:map rustic-mode-map
			  ("M-j" . lsp-ui-imenu)
			  ("M-?" . lsp-find-references)
			  ("C-c C-c l" . flycheck-list-errors)
			  ("C-c C-c a" . lsp-execute-code-action)
			  ("C-c C-c r" . lsp-rename)
			  ("C-c C-c q" . lsp-workspace-restart)
			  ("C-c C-c Q" . lsp-workspace-shutdown)
			  ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  (setq rustic-format-on-save t)
  (setq lsp-rust-analyzer-server-display-inlay-hints t)
  (add-hook 'rustic-mode-hook 'vt/rustic-mode-hook))


(defun vt/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))

(use-package ruby-mode
  :config
  (setq ruby-insert-encoding-magic-comment nil)
  :hook (ruby-mode . tree-sitter-hl-mode)
  :custom
  (set-electric! 'ruby-mode :words '("else" "end" "elseif")))

(use-package yard-mode
  :hook ruby-mode)

(use-package rubocop
  :hook (ruby-mode . rubocop-mode))

(use-package rbenv
  :config
  (setq rspec-use-rvm nil)
  (add-to-list 'exec-path (expand-file-name "shims" rbenv-installation-dir)))

(use-package rake
  :defer t
  :init
  (setq rake-cache-file (concat user-emacs-directory "rake.cache"))
  (setq rake-completion-system 'default))

(use-package bundler
  :defer t)

(use-package rspec-mode
  :mode ("/\\.rspec\\'" . text-mode)
  :init
  (setq rspec-use-spring-when-possible nil)
  (add-hook 'rspec-mode-hook #'evil-normalize-keymaps))

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))
