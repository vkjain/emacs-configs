;; vas-noexternal.el

;; ~/.emacs and ~/.emacs.d/ are the standard locations to add init-files
;; splitting out ~/.emacs into several other files and loading those is a lot easier to manage
;; (load <filename>) will evaluate a file
;; (global-set-key <keychord> <function-name>) will set <keychord> to run <function-name>
;; (add-hook <hook> <lambda>) to run lambda at a particular event
;; the "'after-init-hook" event will run functions after the rest of the init-file has finished loading.

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


;;--------------------------------------------------
;; Fringe settings 
(fringe-mode '(8 . 0))
(setq-default indicate-buffer-boundaries 'left)

;;--------------------------------------------------
;; Reload init.el
(defun vas/reload-init()
  "Reloads the init file"
  (interactive)
  (load-file "~/.emacs.d/init.el"))

;;--------------------------------------------------
;; set keys for Apple keyboard, for emacs in OS X
(setq mac-command-modifier 'meta) ; make cmd key do Meta
(setq mac-option-modifier 'super) ; make opt key do Super
(setq mac-control-modifier 'control) ; make Control key do Control
(setq ns-function-modifier 'hyper)  ; make Fn key do Hyper

;;----------------------------------------
;; additional packages ;;;;;;;;;;;;;;;;;;;
(load-file "~/.emacs.d/vaslib.el")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(feebleline-mode t nil (feebleline))
 '(python-shell-interpreter "/usr/local/bin/python3.7"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))
