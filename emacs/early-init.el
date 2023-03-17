;;; Hide options
;; Do not resize the frame
(setq frame-inhibit-implied-resize t)

;; Disable the menu bar since we don't use it
;; (when (and (not (eq system-type 'darwin)) (fboundp 'menu-bar-mode))
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
;; NOTE: currently not using GTK+
;;(setq x-gtk-use-system-tooltips nil)

(setq use-file-dialog nil)
(setq use-dialog-box t)

;; I don't care to see the splash screen
;;(setq inhibit-splash-screen t)

;; Don't show *Buffer list* when opening multiple files at the same time.
;; (setq inhibit-startup-buffer-menu t)

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
(add-to-list 'default-frame-alist '(background-color . "#fff"))

;; Supress cl warning
;;(setq byte-compile-warnings '(cl-functions))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; By default Emacs triggers garbage collection at ~0.8MB which makes
;;; startup really slow. Since most systems have at least 64MB of memory,
;;; we increase it during initialization.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq gc-cons-threshold-original gc-cons-threshold)
(setq gc-cons-threshold (* 1024 1024 100))

;; Set file-name-handler-alist
(setq file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

(run-with-idle-timer
 5 nil
 (lambda ()
	 (setq gc-cons-threshold gc-cons-threshold-original)
	 (setq file-name-handler-alist file-name-handler-alist-original)
	 (setq gc-cons-threshold (* 1024 1024 20))
	 (makunbound 'file-name-handler-alist-original)
	 (message "gc-cons-threshold and file-name-handler-alist restored")))

;; Extra plugins and config files are stored here
(if (not (file-directory-p "~/.config/emacs/plugins/"))
		(make-directory "~/.config/emacs/plugins/"))
(add-to-list 'load-path (expand-file-name "~/.config/emacs/plugins"))
