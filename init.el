;;; package --- Summary: Emacs init
;;; Commentary:
;;; Code:

;; Increase gc to 500MB for easy startup
(setq gc-cons-threshold (* 500 1024 1024))

(require 'package)
(setq package-archives
      '(("gnu"            . "https://elpa.gnu.org/packages/")
	("org"            . "http://orgmode.org/elpa/")
	("melpa-stable"   . "https://stable.melpa.org/packages/")
	("melpa"          . "https://melpa.org/packages/")
	("marmalade"      . "https://marmalade-repo.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'org)
  (package-install 'diminish)
  (package-install 'bind-key))

;; From use-package Readme
(eval-when-compile
  (require 'use-package))
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

;; Load config.org - my Emacs configuration
(org-babel-load-file (concat user-emacs-directory "ReadMe.org"))

;; gc - decrease threshold to 5 MB
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (* 5 1024 1024))))

;;; init.el ends here
(put 'scroll-left 'disabled nil)
