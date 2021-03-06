;;;
(require 'package)

(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives
	     '("melpa2" . "http://www.mirrorservice.org/sites/melpa.org/packages/"))
(add-to-list 'package-archives
	     '("melpa3" . "http://www.mirrorservice.org/sites/stable.melpa.org/packages/") t)
;; (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))

(eval-and-compile
  (defvar use-package-verbose t)
  (require 'cl)
  (require 'use-package)
  (require 'bind-key)
  (require 'diminish))


(add-to-list 'load-path "~/.emacs.d/elpa/bookmark-plus")
(require 'bookmark+)
 
 
;;--------------------------------------------
;; hide mode-line, verical -indent-guides
;; enable feebleline-mode
(require 'feebleline)
(feebleline-mode 1)
(hide-mode-line-mode +1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq highlight-indentation-mode 0) ;; disable default indentation within emacs
(add-to-list 'load-path "~/.emacs.d/elpa/indent-guide")
(require 'indent-guide)
(indent-guide-global-mode)
(set-face-background 'indent-guide-face "dimgray")

;;----------------------------------------
;; toggle between horizontal and vertical window split

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

(define-key ctl-x-4-map "t" 'toggle-window-split)


(use-package try
	:ensure t)
;; which key
;; Brings up some help

 (use-package which-key
	:ensure t 
	:config
	(which-key-mode))

;;Org mode
;;Org bullets makes things look pretty

    (setenv "BROWSER" "google-chrome-stable")

        (use-package org-bullets
        :ensure t
        :config
        (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; set maximum indentation for description lists
(setq org-list-description-max-indent 5)

;; prevent demoting heading also shifting text inside sections
(setq org-adapt-indentation nil)

            


            ;; (setq org-file-apps
  	    ;; 	(append '(
            ;; 		  ("\\.pdf\\'" . "evince %s")
            ;; 		  ) org-file-apps ))

            (global-set-key "\C-ca" 'org-agenda)

            (setq org-agenda-custom-commands
            '(("c" "Simple agenda view"
            ((agenda "")
            (alltodo "")))))

            (global-set-key (kbd "C-c c") 'org-capture)

            (setq org-agenda-files (list "~/Dropbox/orgfiles/gcal.org"
          			       "~/Dropbox/orgfiles/i.org"
          			       "~/Dropbox/orgfiles/schedule.org"))
            (setq org-capture-templates
          			  '(("a" "Appointment" entry (file  "~/Dropbox/orgfiles/gcal.org" )
          				   "* %?\n\n%^T\n\n:PROPERTIES:\n\n:END:\n\n")
          				  ("l" "Link" entry (file+headline "~/Dropbox/orgfiles/links.org" "Links")
          				   "* %? %^L %^g \n%T" :prepend t)
          				  ("b" "Blog idea" entry (file+headline "~/Dropbox/orgfiles/i.org" "Blog Topics:")
          				   "* %?\n%T" :prepend t)
          				  ("t" "To Do Item" entry (file+headline "~/Dropbox/orgfiles/i.org" "To Do")
          				   "* TODO %?\n%u" :prepend t)
  					  ("m" "Mail To Do" entry (file+headline "~/Dropbox/orgfiles/i.org" "To Do")
  					   "* TODO %a\n %?" :prepend t)
  					  ("g" "GMail To Do" entry (file+headline "~/Dropbox/orgfiles/i.org" "To Do")
  					   "* TODO %^L\n %?" :prepend t)
  					  ("n" "Note" entry (file+headline "~/Dropbox/orgfiles/i.org" "Note space")
          				   "* %?\n%u" :prepend t)
  					  ))
  
        (defadvice org-capture-finalize 
            (after delete-capture-frame activate)  
        "Advise capture-finalize to close the frame"  
        (if (equal "capture" (frame-parameter nil 'name))  
        (delete-frame)))

        (defadvice org-capture-destroy 
            (after delete-capture-frame activate)  
        "Advise capture-destroy to close the frame"  
        (if (equal "capture" (frame-parameter nil 'name))  
        (delete-frame)))  

        (use-package noflet
        :ensure t )
        (defun make-capture-frame ()
        "Create a new frame and run org-capture."
        (interactive)
        (make-frame '((name . "capture")))
        (select-frame-by-name "capture")
        (delete-other-windows)
        (noflet ((switch-to-buffer-other-window (buf) (switch-to-buffer buf)))
            (org-capture)))

(require 'ox-beamer)

; for inserting inactive dates
(define-key org-mode-map (kbd "C-c >") (lambda () (interactive (org-time-stamp-inactive))))

;; Ace windows for easy window switching
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
;;Swiper / Ivy / Counsel
;; Swiper gives us a really efficient incremental search with regular expressions and Ivy / Counsel replace a lot of ido or helms completion functionality

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

;; Avy - navigate by searching for a letter on the screen and jumping to it
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

;; (defun my/python-mode-hook ()
;;   (add-to-list 'company-backends 'company-jedi))

;; (add-hook 'python-mode-hook 'my/python-mode-hook)
;; (use-package company-jedi
;;     :ensure t
;;     :config
;;     (add-hook 'python-mode-hook 'jedi:setup)
;;        )

;; (defun my/python-mode-hook ()
;;   (add-to-list 'company-backends 'company-jedi))

;; (add-hook 'python-mode-hook 'my/python-mode-hook)

;; company box mode
;(use-package company-box
;:ensure t
;  :hook (company-mode . company-box-mode)) 


;; Reveal.js
(use-package ox-reveal
:ensure ox-reveal)

(setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
(setq org-reveal-mathjax t)

(use-package htmlize
:ensure t)





;; Yasnippet
(use-package yasnippet
  :ensure t
  :init
    (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)


;; Misc packages
  ; Highlights the current cursor line
(global-hl-line-mode t)
  
					; deletes all the whitespace when you hit backspace or delete
(use-package hungry-delete
  :ensure ;TODO: 
  :config
  (global-hungry-delete-mode))


(use-package multiple-cursors
  :ensure t)

					; expand the marked region in semantic increments (negative prefix to reduce region)
(use-package expand-region
  :ensure ;TODO: 
  :config 
  (global-set-key (kbd "C-=") 'er/expand-region))

(setq save-interprogram-paste-before-kill t)


(global-auto-revert-mode 1) ;; you might not want this
(setq auto-revert-verbose nil) ;; or this
(global-set-key (kbd "<f5>") 'revert-buffer)
(global-set-key (kbd "<f6>") 'revert-buffer)



;; iedit and narrow / widen dwim
					; mark and edit all copies of the marked region simultaniously. 
(use-package iedit
  :ensure t)

; if you're windened, narrow to the region, if you're narrowed, widen
; bound to C-x n
(defun narrow-or-widen-dwim (p)
"If the buffer is narrowed, it widens. Otherwise, it narrows intelligently.
Intelligently means: region, org-src-block, org-subtree, or defun,
whichever applies first.
Narrowing to org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer is already
narrowed."
(interactive "P")
(declare (interactive-only))
(cond ((and (buffer-narrowed-p) (not p)) (widen))
((region-active-p)
(narrow-to-region (region-beginning) (region-end)))
((derived-mode-p 'org-mode)
;; `org-edit-src-code' is not a real narrowing command.
;; Remove this first conditional if you don't want it.
(cond ((ignore-errors (org-edit-src-code))
(delete-other-windows))
((org-at-block-p)
(org-narrow-to-block))
(t (org-narrow-to-subtree))))
(t (narrow-to-defun))))

;; (define-key endless/toggle-map "n" #'narrow-or-widen-dwim)
;; This line actually replaces Emacs' entire narrowing keymap, that's
;; how much I like this command. Only copy it if that's what you want.
(define-key ctl-x-map "n" #'narrow-or-widen-dwim)


;; Web Mode
  (use-package web-mode
    :ensure t
    :config
	   (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
	   (add-to-list 'auto-mode-alist '("\\.vue?\\'" . web-mode))
	   (setq web-mode-engines-alist
		 '(("django"    . "\\.html\\'")))
	   (setq web-mode-ac-sources-alist
	   '(("css" . (ac-source-css-property))
	   ("vue" . (ac-source-words-in-buffer ac-source-abbrev))
         ("html" . (ac-source-words-in-buffer ac-source-abbrev))))
(setq web-mode-enable-auto-closing t))
(setq web-mode-enable-auto-quoting t) ; this fixes the quote problem I mentioned


;; Stuff to refile as I do more Screencasts
  ;;--------------------------------------------------------------------------
  ;; latex
  (use-package tex
  :ensure auctex)

  (defun tex-view ()
      (interactive)
      (tex-send-command "evince" (tex-append tex-print-file ".pdf")))
;; babel stuff



  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (emacs-lisp . t)
     (C . t)
     (js . t)
     (ditaa . t)
     (dot . t)
     (org . t)
     (shell . t )
     (latex . t)
;;  ((looking-at )tex . t )
  ))



  (use-package smartparens
  :ensure t
  :config
  (use-package smartparens-config)
  (use-package smartparens-html)
  (use-package smartparens-python)
  (use-package smartparens-latex)
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  :bind
  ( ("C-<down>" . sp-down-sexp)
   ("C-<up>"   . sp-up-sexp)
   ("M-<down>" . sp-backward-down-sexp)
   ("M-<up>"   . sp-backward-up-sexp)
  ("C-M-a" . sp-beginning-of-sexp)
   ("C-M-e" . sp-end-of-sexp)



   ("C-M-f" . sp-forward-sexp)
   ("C-M-b" . sp-backward-sexp)

   ("C-M-n" . sp-next-sexp)
   ("C-M-p" . sp-previous-sexp)

   ("C-S-f" . sp-forward-symbol)
   ("C-S-b" . sp-backward-symbol)

   ("C-<right>" . sp-forward-slurp-sexp)
   ("M-<right>" . sp-forward-barf-sexp)
   ("C-<left>"  . sp-backward-slurp-sexp)
   ("M-<left>"  . sp-backward-barf-sexp)

   ("C-M-t" . sp-transpose-sexp)
   ("C-M-k" . sp-kill-sexp)
   ("C-k"   . sp-kill-hybrid-sexp)
   ("M-k"   . sp-backward-kill-sexp)
   ("C-M-w" . sp-copy-sexp)

   ("C-M-d" . delete-sexp)

   ("M-<backspace>" . backward-kill-word)
   ("C-<backspace>" . sp-backward-kill-word)
   ([remap sp-backward-kill-word] . backward-kill-word)

   ("M-[" . sp-backward-unwrap-sexp)
   ("M-]" . sp-unwrap-sexp)

   ("C-x C-t" . sp-transpose-hybrid-sexp)

   ("C-c ("  . wrap-with-parens)
   ("C-c ["  . wrap-with-brackets)
   ("C-c {"  . wrap-with-braces)
   ("C-c '"  . wrap-with-single-quotes)
   ("C-c \"" . wrap-with-double-quotes)
   ("C-c _"  . wrap-with-underscores)
  ("C-c `"  . wrap-with-back-quotes)
  ))

  ;;--------------------------------------------

;; Hydra
  (use-package hydra 
    :ensure hydra
    :init 
    (global-set-key
    (kbd "C-x t")
	    (defhydra toggle (:color blue)
	      "toggle"
	      ("a" abbrev-mode "abbrev")
	      ("s" flyspell-mode "flyspell")
	      ("d" toggle-debug-on-error "debug")
	      ("c" fci-mode "fCi")
	      ("f" auto-fill-mode "fill")
	      ("t" toggle-truncate-lines "truncate")
	      ("w" whitespace-mode "whitespace")
	      ("q" nil "cancel")))
    (global-set-key
     (kbd "C-x j")
     (defhydra gotoline 
       ( :pre (linum-mode 1)
	      :post (linum-mode -1))
       "goto"
       ("t" (lambda () (interactive)(move-to-window-line-top-bottom 0)) "top")
       ("b" (lambda () (interactive)(move-to-window-line-top-bottom -1)) "bottom")
       ("m" (lambda () (interactive)(move-to-window-line-top-bottom)) "middle")
       ("e" (lambda () (interactive)(end-of-buffer)) "end")
       ("c" recenter-top-bottom "recenter")
       ("n" next-line "down")
       ("p" (lambda () (interactive) (forward-line -1))  "up")
       ("g" goto-line "goto-line")
       ))
    (global-set-key
     (kbd "C-c t")
     (defhydra hydra-global-org (:color blue)
       "Org"
       ("t" org-timer-start "Start Timer")
       ("s" org-timer-stop "Stop Timer")
       ("r" org-timer-set-timer "Set Timer") ; This one requires you be in an orgmode doc, as it sets the timer for the header
       ("p" org-timer "Print Timer") ; output timer value to buffer
       ("w" (org-clock-in '(4)) "Clock-In") ; used with (org-clock-persistence-insinuate) (setq org-clock-persist t)
       ("o" org-clock-out "Clock-Out") ; you might also want (setq org-log-note-clock-out t)
       ("j" org-clock-goto "Clock Goto") ; global visit the clocked task
       ("c" org-capture "Capture") ; Don't forget to define the captures you want http://orgmode.org/manual/Capture.html
	     ("l" (or )rg-capture-goto-last-stored "Last Capture"))

     ))

(defhydra multiple-cursors-hydra (:hint nil)
  "
     ^Up^            ^Down^        ^Other^
----------------------------------------------
[_p_]   Next    [_n_]   Next    [_l_] Edit lines
[_P_]   Skip    [_N_]   Skip    [_a_] Mark all
[_M-p_] Unmark  [_M-n_] Unmark  [_r_] Mark by regexp
^ ^             ^ ^             [_q_] Quit
"
  ("l" mc/edit-lines :exit t)
  ("a" mc/mark-all-like-this :exit t)
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
  ("r" mc/mark-all-in-region-regexp :exit t)
  ("q" nil)

  ("<mouse-1>" mc/add-cursor-on-click)
  ("<down-mouse-1>" ignore)
  ("<drag-mouse-1>" ignore))


;; git
(use-package magit
  :ensure t
  :init
  (progn
  (bind-key "C-x g" 'magit-status)
  ))

  (use-package git-gutter
  :ensure t
  :init
  (global-git-gutter-mode +1))

  (global-set-key (kbd "M-g M-g") 'hydra-git-gutter/body)


  (use-package git-timemachine
  :ensure t
  )
(defhydra hydra-git-gutter (:body-pre (git-gutter-mode 1)
                            :hint nil)
  "
Git gutter:
  _j_: next hunk        _s_tage hunk     _q_uit
  _k_: previous hunk    _r_evert hunk    _Q_uit and deactivate git-gutter
  ^ ^                   _p_opup hunk
  _h_: first hunk
  _l_: last hunk        set start _R_evision
"
  ("j" git-gutter:next-hunk)
  ("k" git-gutter:previous-hunk)
  ("h" (progn (goto-char (point-min))
              (git-gutter:next-hunk 1)))
  ("l" (progn (goto-char (point-min))
              (git-gutter:previous-hunk 1)))
  ("s" git-gutter:stage-hunk)
  ("r" git-gutter:revert-hunk)
  ("p" git-gutter:popup-hunk)
  ("R" git-gutter:set-start-revision)
  ("q" nil :color blue)
  ("Q" (progn (git-gutter-mode -1)
              ;; git-gutter-fringe doesn't seem to
              ;; clear the markup right away
              (sit-for 0.1)
              (git-gutter:clear))
       :color blue))


;; Load other files
(defun load-if-exists (f)
  "load the elisp file only if it exists and is readable"
  (if (file-readable-p f)
      (load-file f)))

(load-if-exists "~/Dropbox/shared/mu4econfig.el")
(load-if-exists "~/Dropbox/shared/not-for-github.el")
;; Better Shell
(use-package better-shell
    :ensure t
    :bind (("C-\"" . better-shell-shell)
           ("C-:" . better-shell-remote-open)))

;; eshell stuff
  (use-package shell-switcher
    :ensure t
    :config 
    (setq shell-switcher-mode t)
    :bind (("C-'" . shell-switcher-switch-buffer)
	   ("C-x 4 '" . shell-switcher-switch-buffer-other-window)
	   ("C-M-'" . shell-switcher-new-shell)))


  ;; Visual commands
  (setq eshell-visual-commands '("vi" "screen" "top" "less" "more" "lynx"
				 "ncftp" "pine" "tin" "trn" "elm" "vim"
				 "nmtui" "alsamixer" "htop" "el" "elinks"
				 ))
                                 (setq eshell-visual-subcommands '(("git" "log" "diff" "show")))
  (setq eshell-list-files-after-cd t)
  (defun eshell-clear-buffer ()
    "Clear terminal"
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input)))
  (add-hook 'eshell-mode-hook
	    '(lambda()
	       (local-set-key (kbd "C-l") 'eshell-clear-buffer)))

  (defun eshell/magit ()
    "Function to open magit-status for the current directory"
    (interactive)
    (magit-status default-directory)
    nil)

 ;; smart display stuff
(require 'eshell)
(require 'em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)

(add-hook 'eshell-mode-hook
  (lambda ()
    (eshell-smart-initialize)))
;; eshell here
(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))

(global-set-key (kbd "C-!") 'eshell-here)
;; Eshell prompt

 (defcustom dotemacs-eshell/prompt-git-info
  t
  "Turns on additional git information in the prompt."
  :group 'dotemacs-eshell
  :type 'boolean)

;; (epe-colorize-with-face "abc" 'font-lock-comment-face)
(defmacro epe-colorize-with-face (str face)
  `(propertize ,str 'face ,face))

(defface epe-venv-face
  '((t (:inherit font-lock-comment-face)))
  "Face of python virtual environment info in prompt."
  :group 'epe)

  (setq eshell-prompt-function
      (lambda ()
        (concat (propertize (abbreviate-file-name (eshell/pwd)) 'face 'eshell-prompt)
                (when (and dotemacs-eshell/prompt-git-info
                           (fboundp #'vc-git-branches))
                  (let ((branch (car (vc-git-branches))))
                    (when branch
                      (concat
                       (propertize " [" 'face 'font-lock-keyword-face)
                       (propertize branch 'face 'font-lock-function-name-face)
                       (let* ((status (shell-command-to-string "git status --porcelain"))
                              (parts (split-string status "\n" t " "))
                              (states (mapcar #'string-to-char parts))
                              (added (count-if (lambda (char) (= char ?A)) states))
                              (modified (count-if (lambda (char) (= char ?M)) states))
                              (deleted (count-if (lambda (char) (= char ?D)) states)))
                         (when (> (+ added modified deleted) 0)
                           (propertize (format " +%d ~%d -%d" added modified deleted) 'face 'font-lock-comment-face)))
                       (propertize "]" 'face 'font-lock-keyword-face)))))
                (when (and (boundp #'venv-current-name) venv-current-name)
                  (concat 
                    (epe-colorize-with-face " [" 'epe-venv-face) 
                    (propertize venv-current-name 'face `(:foreground "#2E8B57" :slant italic))
                    (epe-colorize-with-face "]" 'epe-venv-face))) 
                (propertize " $ " 'face 'font-lock-constant-face))))


;; Dumb jump
(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config 
  ;; (setq dumb-jump-selector 'ivy) ;; (setq dumb-jump-selector 'helm)
:init
(dumb-jump-mode)
  :ensure
)



;; Origami folding
(use-package origami
:ensure t)


;; IBUFFER
 (global-set-key (kbd "C-x C-b") 'ibuffer)
 (setq ibuffer-saved-filter-groups
	(quote (("default"
		 ("dired" (mode . dired-mode))
		 ("org" (name . "^.*org$"))
	       ("IRC" (or (mode . circe-channel-mode) (mode . circe-server-mode)))
		 ("web" (or (mode . web-mode) (mode . js2-mode)))
		 ("shell" (or (mode . eshell-mode) (mode . shell-mode)))
		 ("mu4e" (or

                (mode . mu4e-compose-mode)
                (name . "\*mu4e\*")
                ))
		 ("programming" (or
				 (mode . python-mode)
				 (mode . c++-mode)))
		 ("emacs" (or
			   (name . "^\\*scratch\\*$")
			   (name . "^\\*Messages\\*$")))
		 ))))
 (add-hook 'ibuffer-mode-hook
	    (lambda ()
	      (ibuffer-auto-mode 1)
	      (ibuffer-switch-to-saved-filter-groups "default")))

 ;; don't show these
					  ;(add-to-list 'ibuffer-never-show-predicates "zowie")
 ;; Don't show filter groups if there are no buffers in that group
 (setq ibuffer-show-empty-filter-groups nil)

 ;; Don't ask for confirmation to delete marked buffers
 (setq ibuffer-expert t)



;; Treemacs
  (use-package treemacs
    :ensure t
    :defer t
    :config
    (progn

      (setq treemacs-follow-after-init          t
            treemacs-width                      35
            treemacs-indentation                2
            treemacs-git-integration            t
            treemacs-collapse-dirs              3
            treemacs-silent-refresh             nil
            treemacs-change-root-without-asking nil
            treemacs-sorting                    'alphabetic-desc
            treemacs-show-hidden-files          t
            treemacs-never-persist              nil
            treemacs-is-never-other-window      nil
            treemacs-goto-tag-strategy          'refetch-index)

      (treemacs-follow-mode t)
      (treemacs-filewatch-mode t))
    :bind
    (:map global-map
          ([f8]        . treemacs-toggle)
          ([f9]        . treemacs-projectile-toggle)
          ("<C-M-tab>" . treemacs-toggle)
          ("M-0"       . treemacs-select-window)
          ("C-c 1"     . treemacs-delete-other-windows)
        ))
  (use-package treemacs-projectile
    :defer t
    :ensure t
    :config
    (setq treemacs-header-function #'treemacs-projectile-create-header)
)
;; misc
(defun z/nikola-deploy () ""
(interactive)
(venv-with-virtualenv "blog" (shell-command "cd ~/gh/cestlaz.github.io; nikola github_deploy"))
)

(defun z/swap-windows ()
""
(interactive)
(ace-swap-window)
(aw-flip-window)
)




(defun org-agenda-show-agenda-and-todo (&optional arg)
  (interactive "P")
  (org-agenda arg "c")
  (org-agenda-fortnight-view))


;; set up my own map
(define-prefix-command 'z-map)
(global-set-key (kbd "C-z") 'z-map) ;; was C-1
(define-key z-map (kbd "c") 'multiple-cursors-hydra/body)
(define-key z-map (kbd "m") 'mu4e)
(define-key z-map (kbd "1") 'org-global-cycle)
(define-key z-map (kbd "a") 'org-agenda-show-agenda-and-todo)
(define-key z-map (kbd "g") 'counsel-ag)

(define-key z-map (kbd "s") 'flyspell-correct-word-before-point)
(define-key z-map (kbd "i") (lambda () (interactive) (find-file "~/Dropbox/orgfiles/i.org")))
(define-key z-map (kbd "f") 'origami-toggle-node)
(define-key z-map (kbd "w") 'z/swap-windows)


  (setq user-full-name "Vasant Jain"
                          user-mail-address "vkj1428@gmail.com")
  ;;--------------------------------------------------------------------------


  (global-set-key (kbd "\e\ei")
                  (lambda () (interactive) (find-file "~/Dropbox/orgfiles/i.org")))

  (global-set-key (kbd "\e\el")
                  (lambda () (interactive) (find-file "~/Dropbox/orgfiles/links.org")))

  (global-set-key (kbd "\e\ec")
                  (lambda () (interactive) (find-file "~/.emacs.d/myinit.org")))



(global-set-key [mouse-3] 'flyspell-correct-word-before-point)


;; shell-pop
(use-package shell-pop
:ensure t
  :bind (("s-t" . shell-pop))
  :config
  (setq shell-pop-shell-type (quote ("ehell" "eshell" (lambda nil (eshell)))))
  (setq shell-pop-term-shell "eshell")
  ;; need to do this manually or not picked up by `shell-pop'
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type))



;;Wgrep
(use-package wgrep
:ensure t
)
(use-package wgrep-ag
:ensure t
)
(require 'wgrep-ag)



;; Atomic Chrome (edit in emacs)
(use-package atomic-chrome
:ensure t
:config (atomic-chrome-start-server))
(setq atomic-chrome-buffer-open-style 'frame)


;;PDF tools
(use-package pdf-tools
:ensure t)
(use-package org-pdfview
:ensure t)

(require 'pdf-tools)
(require 'org-pdfview)


;; auto-yasnippet
(use-package auto-yasnippet
:ensure t)

;; Misc
;; (setq browse-url-browser-function 'browse-url-generic
;;       browse-url-generic-program "google-chrome-stable")


(setq browse-url-browser-function 'browse-url-default-macosx-browser)

(setq auto-window-vscroll nil)

(load-file "~/.emacs.d/elpa/dired+.el")


;; entering comment boxes
(defun bjm-comment-box (b e)
"Draw a box comment around the region but arrange for the region
to extend to at least the fill column. Place the point after the
comment box."

(interactive "r")

(let ((e (copy-marker e t)))
  (goto-char b)
  (end-of-line)
  (insert-char ?  (- fill-column (current-column)))
  (comment-box b e 1)
  (goto-char e)
  (set-marker e nil)))

(global-set-key (kbd "C-c b b") 'bjm-comment-box)

;; aliases, maybe we don't need all of them. Remove ones which we don't need

(defalias 'qrr 'query-replace-regexp)
(defalias 'lml 'list-matching-lines)
(defalias 'dml 'delete-matching-lines)
(defalias 'dnml 'delete-non-matching-lines)
(defalias 'dtw 'delete-trailing-whitespace)
(defalias 'sl 'sort-lines)
(defalias 'rr 'reverse-region)
(defalias 'rs 'replace-string)

(defalias 'g 'grep)
(defalias 'gf 'grep-find)
(defalias 'fd 'find-dired)

(defalias 'rb 'revert-buffer)

(defalias 'sh 'shell)
(defalias 'fb 'flyspell-buffer)
(defalias 'sbc 'set-background-color)
(defalias 'rof 'recentf-open-files)
(defalias 'lcd 'list-colors-display)
(defalias 'cc 'calc)

; elisp
(defalias 'eb 'eval-buffer)
(defalias 'er 'eval-region)
(defalias 'ed 'eval-defun)
(defalias 'eis 'elisp-index-search)
(defalias 'lf 'load-file)

; major modes
(defalias 'hm 'html-mode)
(defalias 'tm 'text-mode)
(defalias 'elm 'emacs-lisp-mode)
(defalias 'om 'org-mode)
(defalias 'ssm 'shell-script-mode)

; minor modes
(defalias 'wsm 'whitespace-mode)
(defalias 'gwsm 'global-whitespace-mode)
(defalias 'vlm 'visual-line-mode)
(defalias 'glm 'global-linum-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setting up latex mode
;; Forward/inverse search with evince using D-bus.
;; Installation:
;; M-x package-install RET auctex RET
;; Tells emacs where to find LaTeX.
(let ((my-path (expand-file-name "/usr/local/bin:/usr/local/texlive/2018basic/bin/x86_64-darwin")))
(setenv "PATH" (concat my-path ":" (getenv "PATH")))
(add-to-list 'exec-path my-path)) 

;; AucTeX settings
(setq TeX-PDF-mode t)

(add-hook 'LaTeX-mode-hook
(lambda ()
  (push
   '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
     :help "Run latexmk on file")
    TeX-command-list)))
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))

(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
      '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setenv "PDFLATEX" "pdflatex --shell-escape")

;;--------------------------------------------------
;; tikz as default packages for Latex processing
;;--------------------------------------------------
(add-to-list 'org-latex-packages-alist
	     '("" "tikz" t))
(eval-after-load "preview"
'(add-to-list 'preview-default-preamble
	      "\\PreviewEnvironment{tikzpicture}" t))

(setq org-latex-create-formula-image-program 'imagemagick)
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/Cellar/imagemagick/7.0.8-14/bin/"))
(setq exec-path (append exec-path '("/usr/local/Cellar/imagemagick/7.0.8-14-0/bin/")))
(setq auto-mode-alist
      (append
       '(("\\.tikz\\'" . latex-mode))
       auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; scrolling command line history using shell via emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'shell-mode-hook
	  (lambda()
	    (define-key shell-mode-map (kbd "<M-up>") 'comint-previous-input)
	    (define-key shell-mode-map (kbd "<M-down>") 'comint-next-input)
	    ))

;; addresing path problem for shell
(add-to-list 'exec-path "/bin")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load leuven theme
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load-theme 'leuven)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elfeed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq elfeed-db-directory "~/Dropbox/shared/elfeeddb")

(defun elfeed-mark-all-as-read()
  (interactive)
  (mark-whole-buffer)
  (elfeed-search-untag-all-unread))

(defun bjm/elfeed-save-db-and-bury()
  "wrapper to save the elfeed db to disk before burying the buffer"
  (interactive)
  (elfeed-db-save)
  (quit-window))

(defun bjm/elfeed-load-db-and-open()
  "Wrapper to load the elfeed db from disk before opening"
  (interactve)
  (elfeed-db-load)
  (elfeed)
  (elfeed-search-update--force))
(use-package elfeed
  :ensure t
  :bind (:map elfeed-search-mode-map
	      ("q" . bjm/elfeed-save-db-and-bury)
	      ("Q" . bjm/elfeed-save-db-and-bury)
	      ("j" . mz/make-and-run-elfeed-hydra)
	      ("m" . elfeed-toggle-star)
	      ("J" . mz/make-and-run-elfeed-hydra)
	      ("M" . mz/elfeed-toggle-star)
	      )
  )

(use-package elfeed-goodies
  :ensure t
  :config
  (elfeed-goodies/setup))

(use-package elfeed-org
  :ensure t
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list "~/Dropbox/shared/elfeed.org"))
  )

;;--------------------------------------------------
;; org-ref  configuration 
;;--------------------------------------------------
;;(org-babel-load-file "org-ref.org")
(require 'org-ref)
(setq reftex-default-bibliography '("~/Dropbox/bibliography/references.bib"))

;; see org-ref for use of these variables
(setq org-ref-bibliography-notes "~/Dropbox/bibliography/notes.org"
      org-ref-default-bibliography '("~/Dropbox/bibliography/references.bib")
      org-ref-pdf-directory "~/Dropbox/bibliography/bibtex-pdfs/")

(setq bibtex-completion-bibliography "~/Dropbox/bibliography/references.bib"
      bibtex-completion-library-path "~/Dropbox/bibliography/bibtex-pdfs"
      bibtex-completion-notes-path "~/Dropbox/bibliography/helm-bibtex-notes")

;; open pdf with system pdf viewer (works on mac)
(setq bibtex-completion-pdf-open-function
  (lambda (fpath)
    (start-process "open" "*open*" "open" fpath)))

;; alternative
;; (setq bibtex-completion-pdf-open-function 'org-open-file)

(setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))

;; doi-utils
(require 'doi-utils)

;; PDF links for org-mode
(with-eval-after-load "pdf-tools"
  (use-package org-pdfview
    :config
    ;; https://lists.gnu.org/archive/html/emacs-orgmode/2016-11/msg00169.html
    ;; Before adding, remove it (to avoid clogging)
    (delete '("\\.pdf\\'" . default) org-file-apps)
    ;; https://lists.gnu.org/archive/html/emacs-orgmode/2016-11/msg00176.html
    (add-to-list 'org-file-apps
		 '("\\.pdf\\'" . (lambda (file link)
				   (org-pdfview-open link))))))

(defun my/org-ref-open-pdf-at-point ()
  "Open the pdf for bibtex key under point if it exists."
  (interactive)
  (let* ((results (org-ref-get-bibtex-key-and-file))
         (key (car results))
         (pdf-file (funcall org-ref-get-pdf-filename-function key))
     (pdf-other (bibtex-completion-find-pdf key)))
    (cond ((file-exists-p pdf-file)
       (org-open-file pdf-file))
      (pdf-other
       (org-open-file pdf-other))
      (message "No PDF found for %s" key))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;--------------------------------------------------------------------------
;; pasting png images in org-mode
;;--------------------------------------------------------------------------

(defun my/img-maker ()
 "Make folder if not exist, define image name based on time/date" 
  (setq myvar/img-folder-path (concat default-directory "img/"))

  ; Make img folder if it doesn't exist.
  (if (not (file-exists-p myvar/img-folder-path)) ;[ ] refactor thir and screenshot code.
       (mkdir myvar/img-folder-path))

  (setq myvar/img-name (concat "img_" (format-time-string "%Y_%m_%d__%H_%M_%S") ".png"))
  (setq myvar/img-Abs-Path (concat myvar/img-folder-path myvar/img-name)) ;Relative to workspace.

  (setq myvar/relative-filename (concat "./img/" myvar/img-name))
  (insert "[[" myvar/relative-filename "]]" "\n")
)

(defun my/org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the
 sub-directory (%filenameIMG) as the org-buffer and insert a link to this file."
  (interactive)
  (my/img-maker)
  ;(make-frame-invisible)
  (lower-frame)
  (call-process "import" nil nil nil myvar/img-Abs-Path)

  (raise-frame)
  ;(make-frame-visible)
  (org-display-inline-images)
  )

;;--------------------------------------------------------------------------
;; insert image from cliboard into org-file
;;--------------------------------------------------------------------------
(defun org-insert-clipboard-image (&optional file)
  (interactive "F")
  (shell-command (concat "pngpaste " file))
  (insert (concat "[[" file "]]"))
  (org-display-inline-images))

;;--------------------------------------------------

;;--------------------------------------------------
;; drag and drop images into org files
;;--------------------------------------------------

(defun my-dnd-func (event)
  (interactive "e")
  (goto-char (nth 1 (event-start event)))
  (x-focus-frame nil)
  (let* ((payload (car (last event)))
         (type (car payload))
         (fname (cadr payload))
         (img-regexp "\\(png\\|jp[e]?g\\)\\>"))
    (cond
     ;; insert image link
     ((and  (eq 'drag-n-drop (car event))
            (eq 'file type)
            (string-match img-regexp fname))
      (insert (format "[[%s]]" fname))
      (org-display-inline-images t t))
     ;; insert image link with caption
     ((and  (eq 'C-drag-n-drop (car event))
            (eq 'file type)
            (string-match img-regexp fname))
      (insert "#+ATTR_ORG: :width 300\n")
      (insert (concat  "#+CAPTION: " (read-input "Caption: ") "\n"))
      (insert (format "[[%s]]" fname))
      (org-display-inline-images t t))
     ;; C-drag-n-drop to open a file
     ((and  (eq 'C-drag-n-drop (car event))
            (eq 'file type))
      (find-file fname))
     ((and (eq 'M-drag-n-drop (car event))
           (eq 'file type))
      (insert (format "[[attachfile:%s]]" fname)))
     ;; regular drag and drop on file
     ((eq 'file type)
      (insert (format "[[%s]]\n" fname)))
     (t
      (error "I am not equipped for dnd on %s" payload)))))

(add-to-list 'load-path "~/.emacs.d/elpa/org-screenshot")
(require 'org-attach-screenshot)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; awesome-tabs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package awesome-tab
  :load-path "~/.emacs.d/elpa/awesome-tab/"
  :config
  (awesome-tab-mode t)
)
