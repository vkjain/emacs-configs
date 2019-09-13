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
    ein
    elpy
    flycheck
    material-theme
    py-autopep8))

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


;;--------------------------------------------------
;; Fringe settings
;; (fringe-mode '(8 . 0))
;; (setq-default indicate-buffer-boundaries 'left)

;;--------------------------------------------------
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
(load-file "~/.emacs.d/vaslib.el")
(load-file "~/.emacs.d/elpa/org-ref-20181214.1501/org-ref.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (hydra zerodark-theme zenburn yasnippet-snippets worf window-purpose which-key wgrep-ag web-mode visual-regexp-steroids virtualenvwrapper use-package undo-tree try treemacs-projectile sphinx-frontend smex smartparens shell-switcher shell-pop py-autopep8 plantuml-mode paredit-everywhere paradox ox-twbs ox-rst ox-reveal ov origami org-ref org-present org-pdfview org-elisp-help org-ehtml org-easy-img-insert org-bullets org-bookmark-heading org-beautify-theme org-autolist org-ac orca neotree multiple-cursors multi-term moe-theme minions mbe material-theme markdown-mode magit lorem-ipsum jedi irony-eldoc ipython-shell-send indent-tools iedit ido-vertical-mode ido-ubiquitous hungry-delete highlight-indent-guides hide-mode-line helpful helm-projectile helm-helm-commands helm-gtags helm-delicious helm-dash helm-books helm-bibtexkey helm-ag graphviz-dot-mode go-snippets go-eldoc git-timemachine git-gutter geiser flymake-python-pyflakes flycheck feebleline expand-region ess-smart-underscore ess-R-object-popup esh-autosuggest epkg emmet-mode elpy elisp-slime-nav elfeed-org elfeed-goodies ein-mumamo dumb-jump diminish default-text-scale counsel company-jedi company-irony company-bibtex company-anaconda color-theme-modern cl-libify cider chicken-scheme bug-hunter buffer-move bibtex-utils bibslurp bibretrieve bibliothek better-shell better-defaults beacon base16-theme auto-yasnippet auto-highlight-symbol auto-complete-rst auto-auto-indent auctex-latexmk atomic-chrome all-the-icons-dired alect-themes aggressive-indent))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))


(setq hydra-examples-verbatim t)




(defun automatic-push ()
  (interactive)
(let ((current (buffer-file-name (current-buffer))))
      (when (magit-anything-modified-p nil current)
	(magit-call-git "add" current)
	(magit-call-git "commit" "-m" (concat current " update"))
	(magit-call-git "push" "origin")
	(magit-refresh)
	(print (concat current "is pushed!!!")))))





