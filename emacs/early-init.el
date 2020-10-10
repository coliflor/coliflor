;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Set packages to install
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package configs
(require 'package)
;;(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; update packages list if we are on a new install
(unless package-archive-contents
  (package-refresh-contents))

(setq use-package-always-ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hide options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Do not resize the frame
(setq frame-inhibit-implied-resize t)

;; Disable the menu bar since we don't use it
;;(when (and (not (eq system-type 'darwin)) (fboundp 'menu-bar-mode))
;;	(menu-bar-mode -1))

;; Disable the toolbar at the top since it's useless
;; (if (functionp 'tool-bar-mode) (tool-bar-mode -1))

;; Disable scrollbars
;;(scroll-bar-mode -1)
;;(set-specifier horizontal-scrollbar-visible-p nil)
;;(set-specifier vertical-scrollbar-visible-p nil)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)


;; It disables tooltips via GTK+
(setq x-gtk-use-system-tooltips nil)

(setq use-file-dialog nil)
(setq use-dialog-box t)

;; I don't care to see the splash screen
(setq inhibit-splash-screen t)

;; Don't show *Buffer list* when opening multiple files at the same time.
(setq inhibit-startup-buffer-menu t)

;; Makes *scratch* empty.
(setq initial-scratch-message "")

;; Disable bar for early launch then we start doom-modeline
(setq mode-line-format nil)
;; We have to reenable this after early-init
(set-fringe-mode 0)

;; Removes *scratch* from buffer after the mode has been set.
(defun remove-scratch-buffer ()
	(if (get-buffer "*scratch*")
			(kill-buffer "*scratch*")))
(add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)

;; Removes *messages* from the buffer.
;; (setq-default message-log-max nil)
;; (kill-buffer "*Messages*")

;; Removes *Completions* from buffer after you've opened a file.
(add-hook 'minibuffer-exit-hook
					'(lambda ()
						 (let ((buffer "*Completions*"))
							 (and (get-buffer buffer)
										(kill-buffer buffer)))))

;; background color for early boot
(add-to-list 'default-frame-alist '(background-color . "#27212E"))

;; Supress cl warning
(setq byte-compile-warnings '(cl-functions))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; By default Emacs triggers garbage collection at ~0.8MB which makes
;;; startup really slow. Since most systems have at least 64MB of memory,
;;; we increase it during initialization.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;; Extra plugins and config files are stored here
(if (not (file-directory-p "~/.emacs.d/plugins/"))
    (make-directory "~/.emacs.d/plugins/"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/plugins"))
