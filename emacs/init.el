 ;;; Sets custom user directory
(setq user-emacs-directory "~/.config/emacs")
(add-to-list 'load-path "~/.config/emacs/lisp/")

;;; Set packages to install
;; Package configs
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
												 ("gnu" . "http://elpa.gnu.org/packages/")
												 ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; update packages list if we are on a new install
(unless package-archive-contents
	(package-refresh-contents))

(defvar use-package-always-ensure t)
(defvar use-package-verbose nil)

;;; Update pacakges
(use-package auto-package-update
	:defer 0.8
	:ensure f
	:config
	(setq auto-package-update-delete-old-versions t)
	(setq auto-package-update-interval 4)
	(setq auto-package-update-hide-results t)
	(auto-package-update-maybe)
	(add-hook 'auto-package-update-before-hook
						(lambda () (message "I will update packages now"))))

;;; Start emacs server if not already running
(use-package server
	:config
	(server-start))

;;; Esup: Emacs StartUp Profiler
;;       - Profile the load time of the Emacs init file
(use-package esup
	:disabled t
	:init
	(setq esup-child-max-depth 0)
	;; Use a hook so the message doesn't get clobbered by other messages.
	(add-hook
	 'emacs-startup-hook
	 (lambda ()
		 (message "Emacs ready in %s with %d garbage collections."
							(format "%.2f seconds"
											(float-time
											 (time-subtract after-init-time before-init-time)))
							gcs-done))))

;;; NeoTree
;; M-x all-the-icons-install-fonts and fc-cache -f -v
(use-package all-the-icons)
(use-package neotree
	:init
	(setq-default neo-show-hidden-files t)
	:config
	(setq all-the-icons-scale-factor 0.7))

;;; General Tweaks
(use-package emacs
	:config
	(setq initial-scratch-message "")    ;; Makes *scratch* empty.
	(setq inhibit-splash-screen t)       ;; I don't care to see the splash screen
	(setq inhibit-startup-buffer-menu t) ;; Don't show *Buffer list*
	(setq use-dialog-box nil)            ;; To disable dialog windows
	(tool-bar-mode -1)                   ;; Disabling the tool bar
	(menu-bar-mode -1)                   ;; Disabling the menu bar
	(setq create-lockfiles nil)          ;; Prevent from writing temporary .# files
	)

(use-package scroll-bar
	:ensure f
	:config
	(scroll-bar-mode 1))

(use-package frame
	:ensure f
	:defer 0.8
	:config
	;; set transparency (picom)
	(set-frame-parameter (selected-frame) 'alpha '(99 99))
	(add-to-list 'default-frame-alist '(alpha 99 99)))

;; Rename Current Buffer File
;; https://stackoverflow.com/questions/384284/how-do-i-rename-an-open-file-in-emacs
(defun rename-current-buffer-file ()
	"Renames current buffer and file it is visiting."
	(interactive)
	(let ((name (buffer-name))
				(filename (buffer-file-name)))
		(if (not (and filename (file-exists-p filename)))
				(error "Buffer '%s' is not visiting a file!" name)
			(let ((new-name (read-file-name "New name: " filename)))
				(if (get-buffer new-name)
						(error "A buffer named '%s' already exists!" new-name)
					(rename-file filename new-name 1)
					(rename-buffer new-name)
					(set-visited-file-name new-name)
					(set-buffer-modified-p nil)
					(message "File '%s' successfully renamed to '%s'"
									 name (file-name-nondirectory new-name)))))))

;; Remove trailing white space upon saving
;; Note: because of a bug in EIN we only delete trailing whitespace
;; when not in EIN mode.
(add-hook 'before-save-hook
					(lambda ()
						(when (not (derived-mode-p 'ein:notebook-multilang-mode))
							(delete-trailing-whitespace))))

;; Auto-wrap at 100 characters
;; (setq-default auto-fill-function 'do-auto-fill)
;; (setq-default fill-column 100)
;; (turn-on-auto-fill)

;; Disable auto-fill-mode in programming mode
(add-hook 'prog-mode-hook (lambda () (auto-fill-mode -1)))

;; We don't want to type yes and no all the time so, do y and n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Don't ring the bell
(setq ring-bell-function 'ignore)

;; Non-nil means draw block cursor as wide as the glyph under it.
;; For example, if a block cursor is over a tab, it will be drawn as
;; wide as that tab on the display.
(setq x-stretch-cursor t)

;; Check (on save) whether the file edited contains a shebang, if yes,
;; make it executable from
;; http://mbork.pl/2015-01-10_A_few_random_Emacs_tips
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

(setq-default tab-width 2)

(setq mode-line-format 1)

;; reformat buffer
(defun indent-buffer ()
	(interactive) (save-excursion
									(indent-region (point-min) (point-max) nil)))

;; Show only one active window when opening multiple files at the same time.
(add-hook 'window-setup-hook 'delete-other-windows)

;; show white spaces at the end of the line
(setq-default show-trailing-whitespace t)

;; Visualize spaces, tabs and newline
;; (add-hook 'prog-mode-hook 'whitespace-mode)

;; Globally Change the Default Font
;; (add-to-list 'default-frame-alist '(font . "Droid Sans Mono-10" ))
;; (set-face-attribute 'default t :font "Droid Sans Mono-10" )
(add-to-list 'default-frame-alist '(font . "DinaRemasterII 18" ))
(set-face-attribute 'default t :font "DinaRemasterII 18" )
(set-frame-font "DinaRemasterII 18" nil t)

(defvar my-font-size 180)

(set-face-attribute ;; Make mode bar small
 'mode-line nil  :height my-font-size)

(set-face-attribute ;; Set the header bar font
 'header-line nil  :height my-font-size)

(setq default-frame-alist ;; Set default window size and position
			'((top . 0) (left . 0) ;; position
				(width . 110) (height . 80)))  ;;size

(set-face-attribute ;; Set the font to size 9 (90/10).
 'default nil :height my-font-size)

(xterm-mouse-mode 1) ;; Enable mouse support in terminal

(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
	(toggle-indicate-empty-lines))

;; Do not use gpg agent when runing in terminal
;;(defadvice epg--start (around advice-epg-disable-agent activate)
;;	(let ((agent (getenv "GPG_AGENT_INFO")))
;;		(setenv "GPG_AGENT_INFO" nil)
;;		ad-do-it
;;		(setenv "GPG_AGENT_INFO" agent)))

;; disable caching
(setq epa-file-cache-passphrase-for-symmetric-encryption nil)
(setf epa-pinentry-mode 'loopback)

(use-package paren
	:defer 0.8
	:config
	;; turn on highlight matching brackets when cursor is on one
	(show-paren-mode t)

	(setq show-paren-delay 0))

(use-package delsel
	:defer 0.8
	:config
	;; Overwrite region selected
	(delete-selection-mode t))

(use-package simple
	:ensure f
	:defer 0.8
	:config

	;; Wrap Lines at Word Boundary
	(global-visual-line-mode 1)

	;; disable modeline number-mode/column-mode/time
	(setq line-number-mode nil)
	(setq column-number-mode nil))

(use-package files
	:ensure f
	:defer 0.8
	:config
	;; Prevent emacs from creating a bckup file filename~
	(setq make-backup-files nil)

	;; Disable the horrid auto-save
	(setq auto-save-default nil)

	(setq backup-directory-alist `(("." . "~/.saves"))))

(use-package find-file
	:defer 0.8
	:config
	;; Settings for searching
	(setq-default case-fold-search t ;case insensitive searches by default
								search-highlight t)) ;hilit matches when searching

(use-package hl-line
	:defer 0.8
	:config
	;; Highlight the line we are currently on
	(global-hl-line-mode 0))

(use-package vc-hooks
	:ensure f
	:defer 0.8
	:config
	;; Dont ask to follow symlink in git
	(setq vc-follow-symlinks t))

(use-package fringe
	:ensure f
	:config
	(set-fringe-mode 10))

(use-package linum
	:config
	(global-linum-mode 1))

;; close all buffers
(defun close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DOOM theme
(progn
	(use-package doom-themes
		:if window-system
		:config

		;; Global settings (defaults)
		(setq doom-themes-enable-bold nil    ; if nil, bold is universally disabled
					doom-themes-enable-italic t)   ; if nil, italics is universally disabled
		(load-theme 'doom-nord-light t)

		;; Enable flashing mode-line on errors
		(doom-themes-visual-bell-config)

		;; Enable custom neotree theme (all-the-icons must be installed!)
		(doom-themes-neotree-config)

		;; or for treemacs users
		;; use the colorful treemacs theme
		(defvar doom-themes-treemacs-theme "doom-colors")
		(doom-themes-treemacs-config)

		;; Corrects (and improves) org-mode's native fontification.
		(doom-themes-org-config)

		(set-background-color "#fff")

		;; Update neotree working dir
		(defvar neo-smart-open t)))

;;; DOOM Modeline
(use-package doom-modeline
	:ensure t
	:init (doom-modeline-mode 1)
	:config
	;; How tall the mode-line should be
	;; (setq doom-modeline-height 10)

	;; How wide the mode-line bar should be. It's only respected in GUI.
	(setq doom-modeline-bar-width 2)

	;; The limit of the window width.
	;; If `window-width' is smaller than the limit, some information won't be displayed.
	(setq doom-modeline-window-width-limit fill-column)

	;; Whether display the colorful icon for `major-mode'.
	;; It respects `all-the-icons-color-icons'.
	(setq doom-modeline-major-mode-color-icon t)

	;; Whether display the workspace name. Non-nil to display in the mode-line.
	(setq doom-modeline-workspace-name nil)

	;; Whether display the `lsp' state. Non-nil to display in the mode-line.
	(setq doom-modeline-lsp t)

	;; Whether display the modal state icon.
	;; Including `evil', `overwrite', `god', `ryo' and `xah-fly-keys', etc.
	(setq doom-modeline-modal-icon nil)

	;; Whether display icons in the mode-line.
	;; While using the server mode in GUI, should set the value explicitly.
	(setq doom-modeline-icon (display-graphic-p)))

;;(set-face-attribute
;; 'mode-line-inactive nil
;; :background (face-background 'default))

(use-package time
	:config
	;; disable modeline time
	(setq display-time-mode nil))

;; Header Line (modeline)
;; (defun header-line-render (left right)
;; 	(let* ((available-width (- (window-total-width) (length left) )))
;; 		(format (format "%%s%%%ds" available-width) left right)))

;; (setq-default header-line-format
;; 							'((:eval
;; 								 (header-line-render
;; 									(format-mode-line
;; 									 (list
;; 										(propertize " " 'display '(raise -0.25))
;; 										(propertize " " 'display '(raise -0.30))))
;; 									(format-mode-line
;; 									 (list " %l:%c "))))))

;; disabled
;;(setq header-line-format nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Centaur tabs
(use-package centaur-tabs
	:defer 0.8
	:ensure t
	:config
	(centaur-tabs-mode t)
	(setq centaur-tabs-enable-key-bindings t)

	(centaur-tabs-headline-match)
	(setq centaur-tabs-gray-out-icons 'buffer)

	;; To display themed icons from all the icons
	(setq centaur-tabs-set-icons t)

	;; To display an overline over the selected tab
	;;(setq centaur-tabs-set-bar 'under)
	;;(setq x-underline-at-descent-line t)

	;; To change the displayed string for the close button
	(setq centaur-tabs-close-button "x")

	;; Show buffer groups names instead of buffer names in tabs
	;; (setq centaur-tabs--buffer-show-groups t)

	;; To change the tab height do
	(setq centaur-tabs-height 45)

	;; Tab cycling 'default 'tabs 'groups
	;; (setq centaur-tabs-cycle-scope 'default)

	;;To display a marker indicating that a buffer has been modified (atom-style)
	(setq centaur-tabs-set-modified-marker t)

	(defun centaur-tabs-hide-tab (x)
		"Do no to show buffer X in tabs."
		(let ((name (format "%s" x)))
			(or
			 ;; Current window is not dedicated window.
			 (window-dedicated-p (selected-window))

			 ;; Buffer name not match below blacklist.
			 (string-prefix-p "*Ilist*" name)

			 ;; Is not magit buffer.
			 (and (string-prefix-p "magit" name)
						(not (file-name-extension name)))
			 ))))

;;; HideShow is a minor mode similar to OutlineMode
(use-package hideshow
	:defer 0.8
	:init
	(add-hook 'prog-mode-hook 'hs-minor-mode)
	:config
	(defvar hs-special-modes-alist
		(mapcar 'purecopy
						'((c-mode "{" "}" "/[*/]" nil nil)
							(c++-mode "{" "}" "/[*/]" nil nil)
							(bibtex-mode ("@\\S(*\\(\\s(\\)" 1))
							(java-mode "{" "}" "/[*/]" nil nil)
							(js-mode "{" "}" "/[*/]" nil)))))

;;; Async - library for async/thread processing
(use-package async)

;;; S is used by origami, etc and sometimes during Emacs
;; upgrades disappears so we try to install it on its own.
(use-package s :disabled t)

;;; F Modern API for working with files and directories
(use-package f :disabled)

;;; Diminish - Hide the minor modes in the mode line for more room
(use-package diminish
	:defer 0.8
	:init
	(eval-when-compile
		;; Silence missing function warnings
		;;(declare-function diminish "diminish.el")
		)
	:config
	(diminish 'abbrev-mode)
	(diminish 'eldoc-mode)
	(diminish 'auto-revert-mode))

;;; Dash A modern list api for Emacs. No 'cl required.
(use-package dash :disabled t)

;;; Colorize color names in buffersColorize color names in buffers
(use-package rainbow-mode
	:defer 0.8)

;;; Rainbow Delimiters -  have delimiters be colored by their depth
(use-package rainbow-delimiters
	:defer 0.8
	:init
	(eval-when-compile
		;; Silence missing function warnings
		(declare-function rainbow-delimiters-mode "rainbow-delimiters.el"))
	(add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;;; Beacon-mode: flash the cursor when switching buffers or scrolling
;;              the goal is to make it easy to find the cursor
(use-package beacon
	:disabled t
	:defer 0.8
	:diminish beacon-mode
	:init
	(eval-when-compile
		;; Silence missing function warnings
		(declare-function beacon-mode "beacon.el"))
	:config
	(setq beacon-size 20)
	(beacon-mode t))

;;; Simple-mpc A GNU Emacs major mode that acts as a front end to mpc.
(use-package simple-mpc
	:disabled t
	:init
	(eval-when-compile
		;; Silence missing function warnings
		(declare-function simple-mpc "simple-mpc-mode.el")))

;;; Clang-format
;; Create clang-format file using google style
;; clang-format -style=google -dump-config > .clang-format
;; clang-format -style=llvm -dump-config > .clang-format
(use-package clang-format :defer 0.8)

;;; Golang
(use-package go-mode
	:disabled t
	:defer 0.8)

;;; Lua
(use-package lua-mode :defer 0.8)

;;; Java
(use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))
(use-package dap-mode :after lsp-mode :config (dap-auto-configure-mode))
(use-package dap-java :ensure nil)


;;; Magit
(use-package magit
	:disabled t
	:defer 0.8)

;;; Configure flycheck
;; Note: For C++ we use flycheck with LSP mode
(use-package flycheck
	:diminish flycheck-mode
	:defer 0.8
	:init
	(eval-when-compile
		;; Silence missing function warnings
		(declare-function global-flycheck-mode "flycheck.el"))
	:config
	;; Turn flycheck on everywhere
	(global-flycheck-mode t)
	;; There are issues with company mode and flycheck in terminal mode.
	;; This is outlined at:
	;; https://github.com/abingham/emacs-ycmd
	(when (not (display-graphic-p))
		(setq flycheck-indication-mode nil)))

(use-package flycheck-pyflakes
	:after python)

(setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))

;;; EmacsSql
(use-package emacsql :defer 0.8)

;;; Org-Mode
(with-eval-after-load 'org
	(setq org-log-done 'time
				org-todo-keywords '((sequence "TODO" "INPROGRESS" "CANCELLED" "DONE"))
				org-todo-keyword-faces '(("INPROGRESS" .
																	(:foreground "blue" :weight bold))))

	;; open in the same buffer
	;; (defvar org-link-frame-setup ((file . find-file)))
	)

;; (setq org-startup-with-inline-images t)
(defvar org-startup-folded t)

;; This extension facilitates moving images from point A to point B.
(use-package org-download
	:defer 0.8
	:after org
	:config

	;; To set it for all files at once, use this
	(setq-default org-download-image-dir "~/Imágenes/org/")

	;; Drag-and-drop to `dired`
	(add-hook 'dired-mode-hook 'org-download-enable))

;; completions of structure template <s
(use-package org-tempo
	:ensure f
	:defer 0.8
	:after org)

;; org-protocol intercepts calls from emacsclient to trigger custom actions
(use-package org-protocol
	:ensure f
	:defer 0.8
	:after org)

(use-package writegood-mode
	:defer 0.8
	:after org
	:init
	(eval-when-compile
		;; Silence missing function warnings
		(declare-function writegood-mode "writegood-mode.el"))
	(add-hook 'org-mode-hook #'writegood-mode))

(use-package org-bullets
	:defer 0.8
	:after org
	:init
	(add-hook 'org-mode-hook (lambda ()
														 (org-bullets-mode 1))))
(use-package org-ref)

(use-package org-roam
	:disabled t
  :ensure t
	:init (setq org-roam-v2-ack t) ;; Acknowledge V2 upgrade
  :custom
  ;;(org-roam-directory (file-truename "/home/coliflor/Documentos/"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

(use-package org-roam-protocol
	:ensure f
	:defer 0.8
	:after org)

(use-package websocket
    :after org-roam)

(use-package org-roam-ui
	:disabled t
	:init (org-roam-ui-mode 1)
    :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;		:hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))


;;; Vlf - handle open very large files
(use-package vlf
	:disabled t
	:defer 0.8
	:config
	(require 'vlf-setup))

;;; Amx:
(use-package amx
	:defer 0.8
	:config (amx-mode))

;;; Ivy/counsel
(use-package ivy
	:defer 0.8
	:demand
	:diminish ivy-mode
	:custom
	(ivy-count-format "(%d/%d) ")
	(ivy-use-virtual-buffers t)
	(ivy-use-selectable-prompt t)
	;;	:hook
	;;	(ivy-mode . ivy-posframe-mode) ;; see the posframe block below
	:config
	(ivy-mode)
	(setq ivy-use-virtual-buffers t)
	(setq enable-recursive-minibuffers t))

(use-package counsel
	:defer 0.8
	:after ivy
	:diminish counsel-mode
	:config
	(counsel-mode 1)
	;; change default regexes
	(setq ivy-initial-inputs-alist
				'((counsel-minor . "^+")
					(counsel-package . "^+")
					(counsel-org-capture . "")
					(counsel-M-x . "")
					(counsel-describe-function . "")
					(counsel-describe-variable . "")
					(org-refile . "^")
					(org-agenda-refile . "^")
					(org-capture-refile . "^")
					(Man-completion-table . "^")
					(woman . "^"))))

(use-package ivy-rich
	:after ivy
	:custom
	(ivy-virtual-abbreviate 'full
													ivy-rich-switch-buffer-align-virtual-buffer t
													ivy-rich-path-style 'abbrev)
	:config
	(setq ivy-rich-parse-remote-buffer nil)
	(setq ivy-rich-parse-remote-file-path nil)
	;;(ivy-set-display-transformer 'ivy-switch-buffer
	;;														 'ivy-rich-switch-buffer-transformer)
	(ivy-rich-mode 1))

;;; Ivy-posframe
;; https://github.com/tumashu/ivy-posframe/issues/109
(defvar exwm--connection nil)
(defvar exwm-workspace-current-index)

(use-package ivy-posframe
	:after ivy
	:custom-face
	;;(ivy-posframe ((t (list :background (face-attribute 'default :background)))))
	(ivy-posframe-border ((t (:background "MediumPurple4"))))
	;;(ivy-posframe-cursor ((t (:background "MediumPurple4"))))
	:config
	;; custom define height of post frame per function
	(setq ivy-posframe-height-alist '((find-file . 13)
																		(t         . 13)))
	;; display at `ivy-posframe-style'
	(setq ivy-posframe-display-functions-alist
				'(
					;;(complete-symbol   . ivy-posframe-display-at-frame-center)
					(counsel-M-x           . ivy-posframe-display-at-frame-center)
					(swiper                . ivy-posframe-display-at-frame-center)
					(swiper-isearch        . ivy-posframe-display-at-frame-center)
					(counsel-ibuffer       . ivy-posframe-display-at-frame-center)
					;;(t                   . ivy-posframe-display-at-frame-center)
					(counsel-imenu         . ivy-posframe-display-at-frame-center)
					(counsel-switch-buffer . ivy-posframe-display-at-frame-center)
					(ivy-switch-buffer     . ivy-posframe-display-at-frame-center)
					))
	;; other customizations
	;; (setq ivy-posframe-hide-minibuffer t)
	;; (setq ivy-posframe-min-width 200)
	;; (setq ivy-posframe-width 200)
	;; (setq ivy-posframe-width 400)
	;; (setq ivy-posframe-height 200)
	;; (setq ivy-posframe-border-width 4)
	;; (setq ivy-posframe-parameters
	;;    '((left-fringe . 4)
	;;     (right-fringe . 4)))

	(ivy-posframe-mode 1))

(use-package swiper
	:defer 0.8
	:after ivy)

;;; Flyspell Mode for Spelling Corrections
(use-package flyspell
	:defer 0.8
	:diminish flyspell-mode
	:hook ((text-mode . flyspell-mode)
				 (prog-mode . flyspell-prog-mode)
				 (org-mode . flyspell-mode))
	:init
	(eval-when-compile
		;; Silence missing function warnings
		(declare-function flyspell-goto-next-error "flyspell.el")
		(declare-function flyspell-mode "flyspell.el")
		(declare-function flyspell-prog-mode "flyspell.el"))
	(setq flyspell-issue-welcome-flag nil)
	:config
	(defun flyspell-check-next-highlighted-word ()
		"Custom function to spell check next highlighted word."
		(interactive)
		(flyspell-goto-next-error)
		(ispell-word)))

(use-package flyspell-correct-ivy
	:after flyspell)

(use-package ispell
	:after flyspell
	:config
	(setq-default ispell-program-name "aspell"))

;;; Yaml/json/markdown/asm  mode
(use-package yaml-mode
	:defer 0.8
	:mode ("\\.yml\\'" "\\.yaml\\'"))
(use-package json-mode
	:defer 0.8
	:mode ("\\.json\\'" "\\.imp\\'"))
(use-package asm-mode
	:defer 0.8
	:mode ("\\.s\\'"))
(use-package markdown-mode
	:defer 0.8
	:mode ("\\.md\\'" "\\.markdown\\'"))

;;; Emacs font sizing with Ctrl key and mouse scroll
;; https://stackoverflow.com/questions/2091881/emacs-font-sizing-with-ctrl-key-and-mouse-scroll
(use-package faces
	:ensure f
	:defer 0.8
	:config
	(defun font-big ()
		(interactive)
		(set-face-attribute 'default nil :height
												(+ (face-attribute 'default :height) 4)))

	(defun font-small ()
		(interactive)
		(set-face-attribute 'default nil :height
												(- (face-attribute 'default :height) 4))))

;;; Delete Word Without Copying to Clipboard/kill-ring
;; http://ergoemacs.org/emacs/emacs_kill-ring.html
(defun my-delete-word (arg)
	"Delete characters forward until encountering the end of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
	(interactive "p")
	(delete-region
	 (point)
	 (progn
		 (forward-word arg)
		 (point))))

(defun my-backward-delete-word (arg)
	"Delete characters backward until encountering the beginning of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
	(interactive "p")
	(my-delete-word (- arg)))

(defun my-delete-line ()
	"Delete text from current position to end of line char.
This command does not push text to `kill-ring'."
	(interactive)
	(delete-region
	 (point)
	 (progn (end-of-line 1) (point)))
	(delete-char 1))

(defun my-delete-line-backward ()
	"Delete text between the beginning of the line to the cursor position.
This command does not push text to `kill-ring'."
	(interactive)
	(let (p1 p2)
		(setq p1 (point))
		(beginning-of-line 1)
		(setq p2 (point))
		(delete-region p1 p2)))

;;; General.el
;; which-key-dum-bindings to get the full list of current keybinds
(use-package general
	:defer 0.8
	:config
	(general-unbind
		"<f10>"
		"C-t"
		"C-z"
		;;"M-w"
		"M-<right>"
		"M-<left>")

	(general-define-key
	 :keymaps 'vterm-mode-map
	 "<XF86Paste>" 'vterm-yank)

	(general-define-key
	 :keymaps 'ivy-minibuffer-map
	 "<SunProps>" 'keyboard-escape-quit
	 "M-<tab>"    'keyboard-escape-quit
	 "M-x"        'keyboard-escape-quit
	 "<menu>"     'keyboard-escape-quit
	 "<cancel>"     'keyboard-escape-quit
	 )

	(general-define-key
	 :keymaps 'swiper-map
	 "C-s"    'keyboard-escape-quit
	 "<find>" 'keyboard-escape-quit
	 ;;swiper-stay-on-quit
	 )

	(general-define-key
	 "<SunProps>"    'counsel-M-x              ;; M-x counsel
	 "<XF86Open>"    'execute-extended-command ;; M-x menu key
	 ;"C-s"           'swiper                   ;; search with swiper
	 "<find>"        'swiper-isearch           ;; "C-r" 'swiper
	 "C-c C-f"       'clang-format-buffer      ;; clang-format indent C code
	 "<C-mouse-5>"   'font-small               ;; small buffer resize
	 "<C-mouse-4>"   'font-big                 ;; big buffer resize
	 "<S-mouse-5>"   'text-scale-decrease      ;; small buffer resize
	 "<S-mouse-4>"   'text-scale-increase      ;; big buffer resize
	 ;; centaur
	 "<XF86Back>"    'centaur-tabs-backward    ;; cicle buffers backwards
	 "<XF86Forward>" 'centaur-tabs-forward     ;; cicle buffers forward
	 ;; cut,copy,paste,delete, rename
	 "<XF86Cut>"     'clipboard-kill-region    ;; cut text
	 "<XF86Copy>"    'clipboard-kill-ring-save ;; copy text
	 "<XF86Paste>"   'clipboard-yank           ;; yank text
	 "<C-backspace>" 'my-backward-delete-word  ;; custom kill-ring
	 "<f12>"       'rename-current-buffer-file ;;rename file
	 ;; spelling
	 "<f7>"          'flyspell-buffer          ;; flyspell buffer
	 "<f8>"          'flyspell-correct-previous;; flyspell previus
	 "<f9>"          'flyspell-correct-next    ;; flyspell next
	 "<f10>"         'ispell-change-dictionary ;; change dictionary
	 ;;
	 "<mouse-3>"     'menu-bar-open            ;; open left mouse menu
	 "C-x C-z"       'suspend-frame            ;; rebind suspend-frame
	 "<C-tab>"       'hs-toggle-hiding         ;; toggle codeblock
 	 ;;windowmove
	 "M-a"           'windmove-left            ;; window move left
	 "M-d"           'windmove-right           ;; window move right
	 "M-w"           'windmove-up              ;; window move up
	 "M-s"           'windmove-down            ;; window move down
	 ;; side panels
	 "M-q"           'imenu-list-smart-toggle  ;; imenu
	 "M-<ESC>"       'neotree-toggle           ;; neotree
	 ;; ivy
	 "C-c C-r"       'ivy-resume
	 "C-x B"         'ivy-switch-buffer-other-window
	 "C-x b"         'ivy-switch-buffer
	 ;; C-z prefix
	 "C-z z"         'indent-buffer            ;; indent buffer
	 "C-z l"         'linum-mode               ;; line numbers
	 ;;"M-<tab>"        'counsel-ibuffer         ;; switch buffer
	 "M-<tab>"       'counsel-switch-buffer    ;; switch buffer
	 "C-z C-b"       'counsel-switch-buffer    ;; interactive switch buffer
	 "C-z i"         'counsel-imenu            ;; imenu
	 "C-z r"         'counsel-recentf          ;; recent files
	 ;; :keymaps 'markdown-mode-map
	 ;; "M-p" nil
	 ;; "M-n" nil

	 ;; goto-last
	 "<XF86Tools>" 'goto-last-point
	 ;; :keymaps 'org-mode-map

	 ))

;;; goto-last-point
;; https://www.emacswiki.org/emacs/goto-last-point.el

(use-package goto-last-point
	:defer 0.8
	:commands (goto-last-point-mode)
	:config (goto-last-point-mode))

;;; Which-key
(use-package which-key
	:defer 0.8
	:diminish which-key-mode
	:config (which-key-mode)
	;; Allow C-h to trigger which-key before it is done automatically
	(setq which-key-show-early-on-C-h t)
	;; make sure which-key doesn't show normally but refreshes quickly after it is
	;; triggered.
	(setq which-key-idle-delay 1)
	(setq which-key-idle-secondary-delay 1.40))

;;; Company
(use-package company
	:defer 0.8
	:diminish company-mode)

(use-package company-box
	:ensure t
	:defer 0.8
	:hook (company-mode . company-box-mode))

(use-package company-quickhelp
	:defer 0.8
	:ensure t)

;;(use-package auto-complete
;;	:init (global-auto-complete-mode))

;;; Hydra
(use-package major-mode-hydra
	:defer 0.8)
(use-package pretty-hydra
	:defer 0.8)

;;; Imenu-list
(use-package imenu-list
	:defer 0.8
	:config
	(defvar imenu-list-idle-update-delay-time 1)

	;; The size of *Ilist* window can be automatically resized every time the *Ilist* buffer is updated
	(setq imenu-list-auto-resize t))

;; in action: http://i.imgur.com/Tt2M0LC.gif
(defun coliflor/imenu-use-package ()
	(add-to-list 'imenu-generic-expression
							 '("Used Packages"
								 "\\(^\\s-*(use-package +\\)\\(\\_<.+\\_>\\)" 2)))
(add-hook 'emacs-lisp-mode-hook #'coliflor/imenu-use-package)

;;; Outshine
(use-package outshine
	:defer 0.8
	:init (outshine-mode 1)
	:config
	(add-hook 'emacs-lisp-mode-hook 'outshine-mode)

	;; Enables outline-minor-mode for *ALL* programming buffers
	(add-hook 'prog-mode-hook 'outline-minor-mode))

;;; Auto-dim
(use-package auto-dim-other-buffers
	:disabled t
	:if window-system
	:init (auto-dim-other-buffers-mode t)
	:config
	(setq auto-dim-other-buffers-dim-on-switch-to-minibuffer nil)
	(setq auto-dim-other-buffers-dim-on-focus-out t))

;;; Auto generated variables
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ivy-posframe-border ((t (:background "MediumPurple4")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
	 ["#5a5475" "#CC6666" "#C2FFDF" "#FFEA00" "#55b3cc" "#FFB8D1" "#96CBFE" "#F8F8F0"])
 '(custom-safe-themes
	 '("e1f4f0158cd5a01a9d96f1f7cdcca8d6724d7d33267623cc433fe1c196848554" "afa47084cb0beb684281f480aa84dab7c9170b084423c7f87ba755b15f6776ef" "d268b67e0935b9ebc427cad88ded41e875abfcc27abd409726a92e55459e0d01" "246a9596178bb806c5f41e5b571546bb6e0f4bd41a9da0df5dfbca7ec6e2250c" "7ea491e912d419e6d4be9a339876293fff5c8d13f6e84e9f75388063b5f794d6" "a6473f7abf949f4a6a1a9cc0dd37ea2e35ba3cea65d3442b98d65c5c5c5cb8d7" "7a994c16aa550678846e82edc8c9d6a7d39cc6564baaaacc305a3fdc0bd8725f" "93ed23c504b202cf96ee591138b0012c295338f38046a1f3c14522d4a64d7308" "e074be1c799b509f52870ee596a5977b519f6d269455b84ed998666cf6fc802a" "3df5335c36b40e417fec0392532c1b82b79114a05d5ade62cfe3de63a59bc5c6" "08a27c4cde8fcbb2869d71fdc9fa47ab7e4d31c27d40d59bf05729c4640ce834" "01cf34eca93938925143f402c2e6141f03abb341f27d1c2dba3d50af9357ce70" "37144b437478e4c235824f0e94afa740ee2c7d16952e69ac3c5ed4352209eefb" default))
 '(fci-rule-color "#B8A2CE")
 '(jdee-db-active-breakpoint-face-colors (cons "#464258" "#C5A3FF"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#464258" "#C2FFDF"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#464258" "#656565"))
 '(objed-cursor-color "#CC6666")
 '(org-link-frame-setup
	 '((vm . vm-visit-folder)
		 (vm-imap . vm-visit-imap-folder)
		 (gnus . gnus)
		 (file . find-file)
		 (wl . wl)))
 '(package-selected-packages
	 '(php-mode company-box frameshot rainbow-mode basic-mode clean-buffers minibuffer-line emacs\.\. org-download scroll-bar org-roam-protocol org-protocol org-tempo faces frame basic-theme fringe vc-hooks files simple scad-preview scad-mode go-mode vterm-toggle vterm auto-complete doom-modeline doom-themes auto-dim-other-buffers outshine imenu-list dashboard major-mode-hydra company which-key general markdown-mode json-mode yaml-mode flyspell-correct-ivy ivy-posframe ivy-rich counsel ivy amx autopair vlf org-bullets writegood-mode emacsql-sqlite3 flycheck-pyflakes flycheck magit clang-format simple-mpc beacon rainbow-delimiters diminish f async esup centaur-tabs neotree all-the-icons auto-package-update use-package))
 '(pdf-view-midnight-colors (cons "#F8F8F0" "#5a5475"))
 '(rustic-ansi-faces
	 ["#5a5475" "#CC6666" "#C2FFDF" "#FFEA00" "#55b3cc" "#FFB8D1" "#96CBFE" "#F8F8F0"])
 '(safe-local-variable-values '((eval outline-hide-body)))
 '(vc-annotate-color-map
	 (list
		(cons 20 "#C2FFDF")
		(cons 40 "#d6f894")
		(cons 60 "#eaf14a")
		(cons 80 "#FFEA00")
		(cons 100 "#f6dc00")
		(cons 120 "#eece00")
		(cons 140 "#E6C000")
		(cons 160 "#eebd45")
		(cons 180 "#f6ba8b")
		(cons 200 "#FFB8D1")
		(cons 220 "#ee9cad")
		(cons 240 "#dd8189")
		(cons 260 "#CC6666")
		(cons 280 "#b26565")
		(cons 300 "#986565")
		(cons 320 "#7e6565")
		(cons 340 "#B8A2CE")
		(cons 360 "#B8A2CE")))
 '(vc-annotate-very-old-color nil))
(put 'downcase-region 'disabled nil)
