;; $$$$$$$$\                                              $$\           $$\  $$\
;; $$  _____|                                             \__|          \__| $$ |
;; $$ |     $$$$$$\$$$$\   $$$$$$\   $$$$$$$\  $$$$$$$\   $$\ $$$$$$$\  $$\$$$$$$\
;; $$$$$\   $$  _$$  _$$\  \____$$\ $$  _____|$$  _____|  $$ |$$  __$$\ $$ \_$$  _|
;; $$  __|  $$ / $$ / $$ | $$$$$$$ |$$ /      \$$$$$$\    $$ |$$ |  $$ |$$ | $$ |
;; $$ |     $$ | $$ | $$ |$$  __$$ |$$ |       \____$$\   $$ |$$ |  $$ |$$ | $$ |$$\
;; $$$$$$$$\$$ | $$ | $$ |\$$$$$$$ |\$$$$$$$\ $$$$$$$  |  $$ |$$ |  $$ |$$ | \$$$$  |
;; \________\__| \__| \__| \_______| \_______|\_______/   \__|\__|  \__|\__|  \____/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Update pacakges
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t
        auto-package-update-interval 4)
  (auto-package-update-maybe))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Start emacs server if not already running
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Force Emacs to try to start a server.
;;(defvar my:force-server-start nil)

;; (when (or my:force-server-start
;;          (and (fboundp 'server-running-p) (not (server-running-p))))
;;  (server-start))
(server-start)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; NeoTree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; M-x all-the-icons-install-fonts and fc-cache -f -v
(use-package all-the-icons)
(use-package neotree
  :init
  (setq-default neo-show-hidden-files t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Centaur tabs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package centaur-tabs
	:init
	(centaur-tabs-mode t)
	(setq centaur-tabs-enable-key-bindings t)

	:config
	(centaur-tabs-headline-match)

	;; To change the tab height do
	(setq centaur-tabs-height 55)

	;; To display themed icons from all the icons
	(setq centaur-tabs-set-icons t)

	;; To display an overline over the selected tab
	(setq centaur-tabs-set-bar 'under)
	(setq x-underline-at-descent-line t)

	;; To disable the close button
	;; (setq centaur-tabs-set-close-button nil)

	;; To change the displayed string for the close button
	(setq centaur-tabs-close-button "x")

	;; Show buffer groups names instead of buffer names in tabs
	;; (setq centaur-tabs--buffer-show-groups t)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General Tweaks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; turn on highlight matching brackets when cursor is on one
(show-paren-mode t)
(setq show-paren-delay 0)

;; Overwrite region selected
(delete-selection-mode t)

;; Show column numbers by default
(setq column-number-mode t)

;; Prevent emacs from creating a bckup file filename~
(setq make-backup-files nil)

;; Settings for searching
(setq-default case-fold-search t ;case insensitive searches by default
							search-highlight t) ;hilit matches when searching

;; Highlight the line we are currently on
(global-hl-line-mode t)

;; Remove trailing white space upon saving
;; Note: because of a bug in EIN we only delete trailing whitespace
;; when not in EIN mode.
(add-hook 'before-save-hook
					(lambda ()
						(when (not (derived-mode-p 'ein:notebook-multilang-mode))
							(delete-trailing-whitespace))))

;; Auto-wrap at 100 characters
(setq-default auto-fill-function 'do-auto-fill)
(setq-default fill-column 100)
(turn-on-auto-fill)

;; Disable auto-fill-mode in programming mode
(add-hook 'prog-mode-hook (lambda () (auto-fill-mode -1)))

;; Wrap Lines at Word Boundary
(global-visual-line-mode 1)

;; We don't want to type yes and no all the time so, do y and n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Disable the horrid auto-save
(setq auto-save-default nil)

;; Don't ring the bell
(setq ring-bell-function 'ignore)

;; Non-nil means draw block cursor as wide as the glyph under it.
;; For example, if a block cursor is over a tab, it will be drawn as
;; wide as that tab on the display.
(setq x-stretch-cursor t)

;; Dont ask to follow symlink in git
(setq vc-follow-symlinks t)

;; Check (on save) whether the file edited contains a shebang, if yes,
;; make it executable from
;; http://mbork.pl/2015-01-10_A_few_random_Emacs_tips
(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

(setq-default tab-width 2)

;; show white spaces at the end of the line
(setq-default show-trailing-whitespace t)

;; reformat buffer
(defun indent-buffer ()
	(interactive) (save-excursion
									(indent-region (point-min) (point-max) nil)))

;; Show only one active window when opening multiple files at the same time.
(add-hook 'window-setup-hook 'delete-other-windows)

(set-fringe-mode 10)
(setq mode-line-format 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HideShow is a minor mode similar to OutlineMode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'prog-mode-hook 'hs-minor-mode)

(defvar hs-special-modes-alist
	(mapcar 'purecopy
					'((c-mode "{" "}" "/[*/]" nil nil)
						(c++-mode "{" "}" "/[*/]" nil nil)
						(bibtex-mode ("@\\S(*\\(\\s(\\)" 1))
						(java-mode "{" "}" "/[*/]" nil nil)
						(js-mode "{" "}" "/[*/]" nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; esup: Emacs StartUp Profiler
;;       - Profile the load time of the Emacs init file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package esup
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; async - library for async/thread processing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package async)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; s is used by origami, etc and sometimes during Emacs
;; upgrades disappears so we try to install it on its own.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package s)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; f Modern API for working with files and directories
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auto-package-update
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto update packages once a week
(use-package auto-package-update
	:commands (auto-package-update-maybe)
	:config
	(setq auto-package-update-delete-old-versions t)
	(setq auto-package-update-hide-results t)
	(auto-package-update-maybe)
	(add-hook 'auto-package-update-before-hook
						(lambda () (message "I will update packages now"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; diminish - Hide the minor modes in the mode line for more room
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package diminish
	:init
	(eval-when-compile
		;; Silence missing function warnings
		(declare-function diminish "diminish.el"))
	:config
	(diminish 'abbrev-mode)
	(diminish 'eldoc-mode)
	(diminish 'auto-revert-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dash A modern list api for Emacs. No 'cl required.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package dash)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rainbow Delimiters -  have delimiters be colored by their depth
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package rainbow-delimiters
	:init
	(eval-when-compile
		;; Silence missing function warnings
		(declare-function rainbow-delimiters-mode "rainbow-delimiters.el"))
	(add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; beacon-mode: flash the cursor when switching buffers or scrolling
;;              the goal is to make it easy to find the cursor
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package beacon
	:diminish beacon-mode
	:init
	(eval-when-compile
		;; Silence missing function warnings
		(declare-function beacon-mode "beacon.el"))
	:config
	(beacon-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Clang-format
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Create clang-format file using google style
;; clang-format -style=google -dump-config > .clang-format
;; clang-format -style=llvm -dump-config > .clang-format
(use-package clang-format)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Configure flycheck
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Note: For C++ we use flycheck with LSP mode
(use-package flycheck
	:diminish flycheck-mode
	:defer t
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EmacsSql
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package emacsql-sqlite3)
(use-package emacsql)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Org-Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-log-done 'time
			org-todo-keywords '((sequence "TODO" "INPROGRESS" "CANCELLED" "DONE"))
			org-todo-keyword-faces '(("INPROGRESS" .
																(:foreground "blue" :weight bold))))
(use-package writegood-mode
	:init
	(eval-when-compile
		;; Silence missing function warnings
		(declare-function writegood-mode "writegood-mode.el"))
	(add-hook 'org-mode-hook #'writegood-mode))

;; open in the same buffer
(setq org-link-frame-setup '((file . find-file)))

;; (setq org-startup-with-inline-images t)

(use-package org-bullets
	:init
	(add-hook 'org-mode-hook (lambda ()
														 (org-bullets-mode 1))))

;; completions of structure template <s
(require 'org-tempo)

(use-package org-roam
	:after org
	:requires (emacsql-sqlite3 emacsql s f)
	:init
	(add-hook 'after-init-hook 'org-roam-mode)
	:config
	(setq org-roam-index-file "~/Documentos/org-roam/20201006023712-index.org")
	(setq org-roam-directory "~/Documentos/org-roam"))

(use-package org-roam-server
	:init (org-roam-server-mode)
	:after org-roam
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8081
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-serve-files nil
        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))

;; org-protocol intercepts calls from emacsclient to trigger custom actions
(require 'org-protocol)
(require 'org-roam-protocol)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; vlf - handle open very large files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package vlf
	:config
	(require 'vlf-setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; autopair
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Automatically at closing brace, bracket and quote
(use-package autopair
	:diminish autopair-mode
	:init
	(eval-when-compile
		;; Silence missing function warnings
		(declare-function autopair-global-mode "autopair.el"))
	:config
	(autopair-global-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; amx:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package amx :defer 0.5
  :config (amx-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ivy/counsel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ivy
	:demand
	:diminish ivy-mode
	:custom
	(ivy-count-format "(%d/%d) ")
	(ivy-use-virtual-buffers t)
	(ivy-use-selectable-prompt t)
	:hook
	(ivy-mode . ivy-posframe-mode) ;; see the posframe block below
	:config
	(ivy-mode)
	(setq enable-recursive-minibuffers t))

(use-package counsel
	:after ivy
	:demand
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
	:demand
	:custom
	(ivy-virtual-abbreviate 'full
													ivy-rich-switch-buffer-align-virtual-buffer t
													ivy-rich-path-style 'abbrev)
	:config
	(setq ivy-rich-parse-remote-buffer nil)
	(setq ivy-rich-parse-remote-file-path nil)
	(ivy-set-display-transformer 'ivy-switch-buffer
															 'ivy-rich-switch-buffer-transformer)
	(ivy-rich-mode 1))

;; TODO: not working
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ivy-posframe
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ivy-posframe
	:after ivy
	:custom-face
	(ivy-posframe ((t (list :background (face-attribute 'default :background)))))
	:config
	;; custom define height of post frame per function
	(setq ivy-posframe-height-alist '((find-file . 13)
																		(t         . 13)))
	;; display at `ivy-posframe-style'
	(setq ivy-posframe-display-functions-alist
				'(
					;;(complete-symbol   . ivy-posframe-display-at-frame-center)
					;;(counsel-M-x       . ivy-posframe-display-at-frame-center)
					;;(counsel-find-file . ivy-posframe-display-at-frame-center)
					;;(ivy-switch-buffer . ivy-posframe-display-at-frame-center)
					;;(t                 . ivy-posframe-display-at-frame-center)
					;;(ivy-switch-buffer     . ivy-posframe-display-at-frame-center)
					))
	;; other customizations
	;; (setq ivy-posframe-hide-minibuffer f)
	;; (setq ivy-posframe-min-width 200)
	;; (setq ivy-posframe-width 200)
	;; (setq ivy-posframe-width 400)
	;; (setq ivy-posframe-height 200)
	;; (setq ivy-posframe-border-width 0)
	;; (setq ivy-posframe-parameters
	;;    '((left-fringe . 4)
	;;     (right-fringe . 4)))

	(ivy-posframe-mode 1))

(use-package swiper
	:after ivy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Flyspell Mode for Spelling Corrections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package flyspell
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; yaml-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package yaml-mode
	:mode ("\\.yml\\'" "\\.yaml\\'"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; json-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package json-mode
	:mode ("\\.json\\'" "\\.imp\\'"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Load asm-mode when opening assembly files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package asm-mode
	:mode ("\\.s\\'"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Use markdown-mode for markdown files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package markdown-mode
	:mode ("\\.md\\'" "\\.markdown\\'")
	:config
	(define-key markdown-mode-map (kbd "M-p") nil)
	(define-key markdown-mode-map (kbd "M-n") nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; diminish - Hide the minor modes in the mode line for more room
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package diminish
	:init
	(eval-when-compile
		;; Silence missing function warnings
		(declare-function diminish "diminish.el"))
	:config
	(diminish 'abbrev-mode)
	(diminish 'eldoc-mode)
	(diminish 'auto-revert-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Globally Change the Default Font
(add-to-list 'default-frame-alist '(font . "Droid Sans Mono-10" ))
(set-face-attribute 'default t :font "Droid Sans Mono-10" )

;; I don't care to see the splash screen
(setq inhibit-splash-screen t)

(defvar my-font-size 90)
;; Make mode bar small
(set-face-attribute 'mode-line nil  :height my-font-size)
;; Set the header bar font
(set-face-attribute 'header-line nil  :height my-font-size)
;; Set default window size and position
(setq default-frame-alist
			'((top . 0) (left . 0) ;; position
				(width . 110) (height . 90)))  ;;size
;; Enable line numbers on the LHS
(global-linum-mode -1)
;; Set the font to size 9 (90/10).
(set-face-attribute 'default nil :height my-font-size)

(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
	(toggle-indicate-empty-lines))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Emacs font sizing with Ctrl key and mouse scroll
;; https://stackoverflow.com/questions/2091881/emacs-font-sizing-with-ctrl-key-and-mouse-scroll
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun font-big ()
	(interactive)
	(set-face-attribute 'default nil :height
											(+ (face-attribute 'default :height) 8)))

(defun font-small ()
	(interactive)
	(set-face-attribute 'default nil :height
											(- (face-attribute 'default :height) 8)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Delete Word Without Copying to Clipboard/kill-ring
;; http://ergoemacs.org/emacs/emacs_kill-ring.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;; bind them to emacs's default shortcut keys:
;; (global-set-key (kbd "C-S-k") 'my-delete-line-backward) ; Ctrl+Shift+k
;; (global-set-key (kbd "C-k") 'my-delete-line)
;; (global-set-key (kbd "M-d") 'my-delete-word)
;; (global-set-key (kbd "<C-backspace>") 'my-backward-delete-word)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; which-key-dum-bindings to get the full list of current keybinds
(use-package general
	:config
	(general-unbind
		"<f10>"
		"C-t"
		"C-z"
		"M-w"
		"M-<right>"
		"M-<left>")

	(general-define-key

	 "<XF86Open>"    'execute-extended-command
	 "C-s"           'swiper                   ;; search with swiper
	 "<find>"        'swiper                   ;; "C-r" 'swiper
	 "C-c C-f"       'clang-format-buffer      ;; clang-format indent C code
	 "<C-mouse-5>"   'font-small               ;; small buffer resize
	 "<C-mouse-4>"   'font-big                 ;; big buffer resize
	 ;; centaur
	 "<XF86Back>"    'centaur-tabs-backward    ;; cicle buffers backwards
	 "<XF86Forward>" 'centaur-tabs-forward 	   ;; cicle buffers forward
	 ;;cut,copy,paste,delete
	 "<XF86Cut>"     'clipboard-kill-region    ;; cut text
	 "<XF86Copy>"    'clipboard-kill-ring-save ;; copy text
	 "<XF86Paste>"   'clipboard-yank           ;; yank text
	 "<C-backspace>" 'my-backward-delete-word  ;; custom kill-ring
	 ;;spelling
	 "<f7>"          'flyspell-buffer          ;; flyspell buffer
	 "<f8>"          'flyspell-correct-previous;; flyspell previus
	 "<f9>"          'flyspell-correct-next    ;; flyspell next
	 "<f10>"         'ispell-change-dictionary ;; change dictionary
	 ;;
	 "<mouse-3>"     'menu-bar-open 	         ;; open left mouse menu
	 "C-x C-z"       'suspend-frame 	         ;; rebind suspend-frame
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
	 ;;C-z prefix
	 "C-z z"         'indent-buffer 	         ;; indent buffer
	 "C-z o"         'coliflor/olivetti-mode 	 ;; light read mode
	 "C-z l"         'linum-mode 	             ;; line numbers
	 ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; which-key
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package which-key
	:init (which-key-mode)
	:diminish which-key-mode
	:config
	;; Allow C-h to trigger which-key before it is done automatically
	(setq which-key-show-early-on-C-h t)
	;; make sure which-key doesn't show normally but refreshes quickly after it is
	;; triggered.
	(setq which-key-idle-delay 1)
	(setq which-key-idle-secondary-delay 1.40))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package company
	:init (global-company-mode)
	:diminish company-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hydra
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package major-mode-hydra)
(use-package pretty-hydra)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; dashboard
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/seagle0128/.emacs.d/blob/1973724b355d6e9a34f74f116ccc429fb0bd0822/lisp/init-dashboard.el
;; TODO: usar centaur-dashboard como referencia
(use-package dashboard
	:ensure t
	:config
	;; Set the banner
	(setq dashboard-startup-banner "~/Proyectos/coliflor/img/salticida.es.min.png")

	;; A randomly selected footnote will be displayed. To disable it:
	(setq dashboard-set-footer t)

	;; To customize it and customize its icon;
	(setq dashboard-footer-messages '("salticida.es"))
	(setq dashboard-footer-icon
				(all-the-icons-octicon
				 "dashboard"
				 :height 1.1
				 :v-adjust -0.05
				 :face 'font-lock-keyword-face))
	:init
	(dashboard-setup-startup-hook)
	)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; imenu-list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package imenu-list
	:config
	(setq imenu-list-idle-update-delay-time 1)

	;; The size of *Ilist* window can be automatically resized every time the *Ilist* buffer is updated
	(setq imenu-list-auto-resize t)

	;; in action: http://i.imgur.com/Tt2M0LC.gif
	(defun imenu-use-package ()
		(add-to-list 'imenu-generic-expression
								 '("Used Packages"
									 "\\(^\\s-*(use-package +\\)\\(\\_<.+\\_>\\)" 2)))
	(add-hook 'emacs-lisp-mode-hook #'imenu-use-package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Olivetti
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package olivetti
  :diminish
  :config
  ;;(setq olivetti-body-width 0.7)
  (setq olivetti-minimum-body-width 100)
  ;;(setq olivetti-recall-visual-line-mode-entry-state t)
	(define-minor-mode coliflor/olivetti-mode
		"Toggle buffer-local `olivetti-mode' with additional parameters.
Fringes are disabled.  The modeline is hidden, The cursor becomes a blinking bar."
    :init-value nil
    :global nil
    (if coliflor/olivetti-mode
        (progn
          (olivetti-mode 1)
          (set-window-fringes (selected-window) 0 0)
					(setq cursor-type 'bar)
					(load-theme 'doom-solarized-light)
					(doom-modeline-mode -1)
					(setq mode-line-format nil)
					(centaur-tabs-mode -1))
      (olivetti-mode -1)
      (set-window-fringes (selected-window) nil) ; Use default width
			(setq cursor-type 'box)
			(load-theme 'doom-laserwave)
			(setq mode-line-format 1)
			(doom-modeline-mode 1)
			(centaur-tabs-mode 1))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Outshine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package outshine
	:init (outshine-mode 1)
	:config

	(add-hook 'emacs-lisp-mode-hook 'outshine-mode)

	;; Enables outline-minor-mode for *ALL* programming buffers
	(add-hook 'prog-mode-hook 'outline-minor-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auto-dim
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package auto-dim-other-buffers
  :init (auto-dim-other-buffers-mode t)
  :config
  (setq auto-dim-other-buffers-dim-on-switch-to-minibuffer nil)
  (setq auto-dim-other-buffers-dim-on-focus-out t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DOOM theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package doom-themes
	:config
	;; Global settings (defaults)
	(setq doom-themes-enable-bold nil    ; if nil, bold is universally disabled
				doom-themes-enable-italic t) ; if nil, italics is universally disabled
	(load-theme 'doom-laserwave t)

	;; Enable flashing mode-line on errors
	(doom-themes-visual-bell-config)

	;; Enable custom neotree theme (all-the-icons must be installed!)
	(doom-themes-neotree-config)

	;; or for treemacs users
	(setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
	(doom-themes-treemacs-config)

	;; Corrects (and improves) org-mode's native fontification.
	(doom-themes-org-config)

	;; Update neotree working dir
	(setq neo-smart-open t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DOOM Modeline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package doom-modeline
	:init (doom-modeline-mode 1)
	:after doom-themes
	:config
	;; How tall the mode-line should be
	;; (setq doom-modeline-height 10)

	;; How wide the mode-line bar should be. It's only respected in GUI.
	;; (setq doom-modeline-bar-width 0)

	;; The limit of the window width.
	;; If `window-width' is smaller than the limit, some information won't be displayed.

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

;; disable modeline number-mode/column-mode/time
(setq line-number-mode nil)
(setq column-number-mode nil)
(setq display-time-mode nil)

;; Header Line
(defun header-line-render (left right)
  (let* ((available-width (- (window-total-width) (length left) )))
    (format (format "%%s%%%ds" available-width) left right)))

(setq-default header-line-format
							'((:eval
								 (header-line-render
									(format-mode-line
									 (list
										(propertize " " 'display '(raise -0.25))
										(propertize " " 'display '(raise -0.30))))
									(format-mode-line
									 (list " %l:%c "))))))

;;disabled
;;(setq header-line-format nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auto generated variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
	 '("37144b437478e4c235824f0e94afa740ee2c7d16952e69ac3c5ed4352209eefb" "1d904ba8343822dff21ffae28a348975eafeb0734034ed5fa33d78bf2519e7cb" "01cf34eca93938925143f402c2e6141f03abb341f27d1c2dba3d50af9357ce70" "99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" default))
 '(package-selected-packages
	 '(focus org-roam-server org-roam emacsql-sqlite3 outshine major-mode-hydra outline-minor-faces auto-dim-other-buffers olivetti yaml-mode writegood-mode which-key vterm vlf vimish-fold use-package tabbar rainbow-delimiters origami org-bullets neotree modus-operandi-theme markdown-mode json-mode ivy-rich ivy-posframe imenu-list git-commit general fold-this flyspell-correct-ivy flycheck-pyflakes esup doom-themes doom-modeline diminish dashboard counsel company clang-format centaur-tabs beacon autopair auto-package-update auctex amx))
 '(safe-local-variable-values
	 '((eval setq ispell-local-dictionary "english")
		 (eval outline-hide-body)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ivy-posframe ((t (list :background (face-attribute 'default :background))))))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Local Variables:
;; eval: (outline-hide-body)
;; End:
