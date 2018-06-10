
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
	     '("melpa2" . "http://www.mirrorservice.org/sites/melpa.org/packages/"))
(add-to-list 'package-archives
	     '("melpa3" . "http://www.mirrorservice.org/sites/stable.melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))

;;--------------------------------------------------

;; interface tweaks
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "<f5>") 'revert-buffer)

;;--------------------------------------------------

;; try
(use-package try
  :ensure t)

;; which key, brings up some help

;; --------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (try zenburn whole-line-or-region which-key web-mode use-package undo-tree sphinx-frontend smex smart-mode-line powerline plantuml-mode paredit-everywhere ox-twbs ox-rst ov org-web-tools org-present org-pdfview org-elisp-help org-ehtml org-easy-img-insert org-download org-cliplink org-bullets org-bookmark-heading org-beautify-theme org-autolist org-ac orca multiple-cursors multi-term material-theme markdown-mode magit lorem-ipsum key-chord ido-vertical-mode ido-ubiquitous hydra helm-projectile helm-helm-commands helm-gtags helm-delicious helm-dash helm-ag go-snippets go-eldoc geiser expand-region ess-smart-underscore ess-R-object-popup emmet-mode elpy elisp-slime-nav ein-mumamo counsel company-jedi cider bug-hunter better-defaults auto-yasnippet auto-complete-rst aggressive-indent ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;--------------------------------------------------

(use-package ace-window
:ensure t
:init
(progn
(setq aw-scope 'frame)
(global-set-key (kbd "C-x O") 'other-frame)
  (global-set-key [remap other-window] 'ace-window)
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 3.0))))) 
  ))

;;--------------------------------------------------

;; Swiper/Ivy/Counsel Swiper gives us a really efficient incremental
;; search with regular expressions and Ivy/Counsel replacement
;; completion functionality


   (use-package counsel
:ensure t
  :bind
  (("M-y" . counsel-yank-pop)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line)))


  (use-package ivy
  :ensure t
  :diminish (ivy-mode)
  :bind (("C-x b" . ivy-switch-buffer))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "%d/%d ")
  (setq ivy-display-style 'fancy))


  (use-package swiper
  :ensure t
  :bind (("C-s" . swiper)
	 ("C-r" . swiper)
	 ("C-c C-r" . ivy-resume)
	 ("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file))
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
    ))

;; --------------------------------------------------

;; Avy - navigate by searching for a letter on the screen and jumping to it
;; 
;; See https://github.com/abo-abo/avy for more info

(use-package avy
:ensure t
:bind ("M-s" . avy-goto-word-1)) ;; changed from char as per jcs

;; Autocomplete

(use-package auto-complete 
:ensure t
:init
(progn
(ac-config-default)
  (global-auto-complete-mode t)
  ))

;; Company

(use-package company
:ensure t
:config
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 3)

(global-company-mode t)
)

(use-package company-irony
:ensure t
:config 
(add-to-list 'company-backends 'company-irony)

)

(use-package irony
:ensure t
:config
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
)

(use-package irony-eldoc
:ensure t
:config
(add-hook 'irony-mode-hook #'irony-eldoc))

(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'my/python-mode-hook)
(use-package company-jedi
    :ensure t
    :config
    (add-hook 'python-mode-hook 'jedi:setup)
       )

(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'my/python-mode-hook)

;; company box mode
;(use-package company-box
;:ensure t
;  :hook (company-mode . company-box-mode)) 

;;--------------------------------------------------


