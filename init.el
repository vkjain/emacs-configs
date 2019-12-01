;;; package --- Summary
;; init.el --- Emacs configuration

;; INSTALL PACKAGES
;; --------------------------------------
;; just entering a unwanted comment for pushing to github as a


(require 'package)

(setq  package-archives
       '(("gnu" . "http://elpa.gnu.org/packages/")
	 ("melpa" . "http://melpa.org/packages/")
	 ("melpa-stable" . "http://stable.melpa.org/packages/")
	 ("org" . "http://orgmode.org/elpa/")))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPackages
  '(better-defaults
    material-theme))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)

;; BASIC CUSTOMIZATION
;; --------------------------------------

;; interface tweaks
;; Remove scrollbars, menu bars, and toolbars
;; when is a special form of "if", with no else clause, it reads:
;; (when <codition> <code-to-execute-1> <code-to-execute-2> ... )
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(fset 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "<f5>") 'revert-buffer)
(setq-default auto-fill-function 'do-auto-fill)  ;; helps to automatically wrap long lines

;; Reload init.el
(defun vas/reload-init()
  "Reloads the init file"
  (interactive)
  (load-file "~/.emacs.d/init.el"))

;; check OS type
;; I have checked on 15Dec2018. This works fine for me
(cond
 ((string-equal system-type "windows-nt") ; Microsoft Windows
  (progn
    (message "Microsoft Windows")))
 ((string-equal system-type "darwin") ; Mac OS X
  (progn
    (setq mac-option-key-is-meta nil)
    (setq mac-command-key-is-meta t)
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier nil)
    (message "Mac OS X")))
 ((string-equal system-type "gnu/linux") ; linux
  (progn
    (message "Linux"))))


(setq inhibit-startup-message t) ;; hide the startup message
(load-theme 'material t) ;; load material theme
;; (global-linum-mode t) ;; enable line numbers globally

;;----------------------------------------
;; additional packages 
(load-file "~/.emacs.d/elpa/org-ref/org-ref.el")
(load-file "~/.emacs.d/vaslib.el")


(setq hydra-examples-verbatim t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; changes to init.el updated to github repo at
;; github.com/vkjain/emacs-configs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun init-push ()
  (interactive)
(let ((current (buffer-file-name (current-buffer))))
      (when (magit-anything-modified-p nil current)
	(magit-call-git "add" current)
	(magit-call-git "commit" "-m" (concat current " update"))
	(magit-call-git "push" "origin")
	(magit-refresh)
	(message (concat current " has been  pushed!!!")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq save-interprogram-paste-before-kill t)

(defvar server-buffer-clients)
(when (and (fboundp 'server-start) (string-equal (getenv "TERM") 'xterm))
  (server-start)
  (defun fp-kill-server-with-buffer-routine()
    (and server-buffer-clients (server-done)))
  (add-hook 'kill-buffer-hook 'fp-kill-server-with-buffer-routine))
