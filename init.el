;; C-c n  for insert-user-timestamp
;; C-. n  org-velocity-read
;; Alt-X org-capture to capture new notes in notes.txt
;; C-/ to go back to the last edit
;; C-c d delete the line the point is on

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; loading init file for editing ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defun vas/edit-dotemacs ()
  "Edit the `user-init-file', in another window"
  (interactive)
(find-file-other-frame user-init-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;; no tool-bar, menu-bar, scroll-bar ;;;;;;;;;;;;;;;;;;
(mapc (lambda (mode) (when (fboundp mode) (apply mode '(-1))))
      '(tool-bar-mode menu-bar-mode scroll-bar-mode))

;;;;;;;;;;;;;;;;;;;;;;;;; no startup-screen ;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq ring-bell-function 'ignore
      inhibit-startup-screen t
      indent-tabs-mode nil)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use-package declaration
(eval-when-compile
  (add-to-list 'load-path "~/.emacs.d/elpa/use-package")
  (require 'use-package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some good key defintions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;; set keys for Apple keyboard, for emacs in OS X ;;;;;;;;;;;;;;;
(setq mac-command-modifier 'meta)       ;make cmd key do Meta
(setq mac-option-modifier 'super)       ;make opt key do Super
(setq mac-control-modifier 'control)    ;make Control key do Control
(setq ns-function-modifier 'hyper)      ;make Fn key do Hyper
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq next-line-add-newlines t) ;; this avoids using Enter key for newline
(global-set-key (kbd "C-n") 'next-line)

(defun open-next-line (arg)
  "Move to the next line and then opens a line.
    See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (when newline-and-indent
    (indent-according-to-mode)))
;; (global-set-key (kbd "s-o") 'open-next-line) 

(defun open-previous-line (arg)
  "Open a new line before the current one. 
     See also `newline-and-indent'."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
    (indent-according-to-mode)))
;;(global-set-key (kbd "M-o") 'open-previous-line)

(bind-key "M-o" 'open-previous-line)
;; (bind-key (kbd "<M-return>") 'open-previous-line)
(bind-key (kbd "C-o") 'open-next-line)  ;; vkj (2016-09-11): I override this
;; keybinding "C-o" which was part of global keymap.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; - ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; support textexpander (jww (2015-03-24): check if it's running)
;; (bind-key "A-v" #'scroll-down)
(bind-key "M-v" #'scroll-down)

;; How to set default file encoding 
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(use-package hydra)

;;; hydra for apropos		       
(defhydra hydra-apropos (:color blue)
  "Apropos"
  ("a" apropos "apropos")
  ("c" apropos-command "cmd")
  ("d" apropos-documentation "doc")
  ("e" apropos-value "val")
  ("l" apropos-library "lib")
  ("o" apropos-user-option "option")
  ("u" apropos-user-option "option")
  ("v" apropos-variable "var")
  ("i" info-apropos "info")
  ("t" tags-apropos "tags")
  ("z" hydra-customize-apropos/body "customize"))

(defhydra hydra-customize-apropos (:color blue)
  "Apropos (customize)"
  ("a" customize-apropos "apropos")
  ("f" customize-apropos-faces "faces")
  ("g" customize-apropos-groups "groups")
  ("o" customize-apropos-options "options"))

;;;;;;;;;;;;;;;;;;;;;;;;; hydra for movement ;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key
 (kbd "C-n")
 (defhydra hydra-move
   (:body-pre (next-line))
   "move"
   ("n" next-line)
   ("p" previous-line)
   ("f" forward-char)
   ("b" backward-char)
   ("a" beginning-of-line)
   ("e" move-end-of-line)
   ("v" scroll-up-command)
   ;; Converting M-v to V here by analogy.
   ("V" scroll-down-command)
   ("l" recenter-top-bottom)))

;;;;;;;;;;;;;;;;;;;;;;; Hydra for Smartparens ;;;;;;;;;;;;;;;;;;;;;;;;
  (defhydra lunaryorn-smartparens (:hint nil)
    "
Sexps (quit with _q_)
^Nav^            ^Barf/Slurp^                 ^Depth^
^---^------------^----------^-----------------^-----^-----------------
_f_: forward     _→_:          slurp forward   _R_: splice
_b_: backward    _←_:          barf forward    _r_: raise
_u_: backward ↑  _C-<right>_:  slurp backward  _↑_: raise backward
_d_: forward ↓   _C-<left>_:   barf backward   _↓_: raise forward
_p_: backward ↓
_n_: forward ↑
^Kill^           ^Misc^                       ^Wrap^
^----^-----------^----^-----------------------^----^------------------
_w_: copy        _j_: join                    _(_: wrap with ( )
_k_: kill        _s_: split                   _{_: wrap with { }
^^               _t_: transpose               _'_: wrap with ' '
^^               _c_: convolute               _\"_: wrap with \" \"
^^               _i_: indent defun"
    ("q" nil)
    ;; Wrapping
    ("(" (lambda (_) (interactive "P") (sp-wrap-with-pair "(")))
    ("{" (lambda (_) (interactive "P") (sp-wrap-with-pair "{")))
    ("'" (lambda (_) (interactive "P") (sp-wrap-with-pair "'")))
    ("\"" (lambda (_) (interactive "P") (sp-wrap-with-pair "\"")))
    ;; Navigation
    ("f" sp-forward-sexp )
    ("b" sp-backward-sexp)
    ("u" sp-backward-up-sexp)
    ("d" sp-down-sexp)
    ("p" sp-backward-down-sexp)
    ("n" sp-up-sexp)
    ;; Kill/copy
    ("w" sp-copy-sexp)
    ("k" sp-kill-sexp)
    ;; Misc
    ("t" sp-transpose-sexp)
    ("j" sp-join-sexp)
    ("s" sp-split-sexp)
    ("c" sp-convolute-sexp)
    ("i" sp-indent-defun)
    ;; Depth changing
    ("R" sp-splice-sexp)
    ("r" sp-splice-sexp-killing-around)
    ("<up>" sp-splice-sexp-killing-backward)
    ("<down>" sp-splice-sexp-killing-forward)
    ;; Barfing/slurping
    ("<right>" sp-forward-slurp-sexp)
    ("<left>" sp-forward-barf-sexp)
    ("C-<left>" sp-backward-barf-sexp)
    ("C-<right>" sp-backward-slurp-sexp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; hydras-end ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; having full elpa directory and subdirectories in load-path
(let ((default-directory  "~/.emacs.d/elpa"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))


 ;; vkj (2016-12-03): apropos can sort results by relevancy)
(setq apropos-sort-by-scores t)

 ;; vkj (2016-12-11): killing whole line on which the point is on
(require 'whole-line-or-region)
(whole-line-or-region-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; paredit-mode

;;   (add-to-list 'load-path "~/.emacs.d/init.el/paredit")
;;   (autoload 'enable-paredit-mode "paredit"
;;     "Turn on pseudo-structural editing of Lisp code."
;;     t)
;; ;;;
;; ;;; Start Paredit Mode on the fly with `M-x enable-paredit-mode RET',
;; ;;; or always enable it in a major mode `M' (e.g., `lisp') with:
;; ;;;
;;   (add-hook 'elisp-mode-hook 'enable-paredit-mode)
;; ;;;



;;;;;;;;;;; smart-mode-line;;;;;;;;;;;;;;;;;;;;

(use-package smart-mode-line
  :load-path "/Users/vasantkj/.emacs.d/elpa/smart-mode-line")
(setq sml/no-confirm-load-theme t)
(sml/setup)
(defun sml/set-shortner-func (sym val)
 "Configure `sml/shortner-func' according to 'sml/shorten-directory'.
set SYM to VAL."
 (set-default sym val)
  (if val (setq sml/shortner-func 'sml/do-shorten-directory)
   (setq sml/shortner-func 'sml/not-shorten-directory)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 ;; vkj (2016-07-10): trying again to configure emacs as server
(require 'server)
(unless (server-running-p)
  (server-start))

 ;; vkj (2016-09-17): loading leuven theme, it is beautiful
(add-to-list 'custom-theme-load-path "/Users/vasantkj/.emacs.d/elpa/emacs-leuven-theme")
(load-theme 'leuven t)
(setq leuven-scale-outline-headlines nil) ;; don't like large Org level-1 headings
;; (set-face-attribute 'org-agenda-date nil :height 90) ;; 9 pt font


(bind-key "M-j" #'delete-indentation-forward)
(bind-key "M-J" #'delete-indentation)

(bind-key "M-W" #'mark-word)

(defun mark-line (&optional arg)
  (interactive "p")
  (beginning-of-line)
  (let ((here (point)))
    (dotimes (i arg)
      (end-of-line))
    (set-mark (point))
    (goto-char here)))

(bind-key "M-L" #'mark-line)

(defun mark-sentence (&optional arg)
  (interactive "P")
  (backward-sentence)
  (mark-end-of-sentence arg))

(bind-key "M-S" #'mark-sentence)
(bind-key "M-X" #'mark-sexp)
(bind-key "M-D" #'mark-defun)

(bind-key "M-g c" #'goto-char)
(bind-key "M-g l" #'goto-line)

(defun delete-indentation-forward ()
  (interactive)
  (delete-indentation t))

;;; C-x C-

(defun duplicate-line ()
  "Duplicate the line containing point."
  (interactive)
  (save-excursion
    (let (line-text)
      (goto-char (line-beginning-position))
      (let ((beg (point)))
        (goto-char (line-end-position))
        (setq line-text (buffer-substring beg (point))))
      (if (eobp)
          (insert ?\n)
        (forward-line))
      (open-line 1)
      (insert line-text))))

(bind-key "C-x C-d" #'duplicate-line)
(bind-key "C-x C-e" #'pp-eval-last-sexp)
(bind-key "C-x C-n" #'next-line)

;;; C-x M-

(bind-key "C-x M-n" #'set-goal-column)

(defun refill-paragraph (arg)
  (interactive "*P")
  (let ((fun (if (memq major-mode '(c-mode c++-mode))
                 'c-fill-paragraph
               (or fill-paragraph-function
                   'fill-paragraph)))
        (width (if (numberp arg) arg))
        prefix beg end)
    (forward-paragraph 1)
    (setq end (copy-marker (- (point) 2)))
    (forward-line -1)
    (let ((b (point)))
      (skip-chars-forward "^A-Za-z0-9`'\"(")
      (setq prefix (buffer-substring-no-properties b (point))))
    (backward-paragraph 1)
    (if (eolp)
        (forward-char))
    (setq beg (point-marker))
    (delete-horizontal-space)
    (while (< (point) end)
      (delete-indentation 1)
      (end-of-line))
    (let ((fill-column (or width fill-column))
          (fill-prefix prefix))
      (if prefix
          (setq fill-column
                (- fill-column (* 2 (length prefix)))))
      (funcall fun nil
               )
      (goto-char beg)
      (insert prefix)
      (funcall fun nil))
    (goto-char (+ end 2))))

(bind-key "C-x M-q" #'refill-paragraph)

;;; mode-specific-map

;;; C-c

(bind-key "C-c <tab>" #'ff-find-other-file)
(bind-key "C-c SPC" #'just-one-space)


(defun delete-current-line (&optional arg)
  (interactive "p")
  (let ((here (point)))
    (beginning-of-line)
    (kill-line arg)
    (goto-char here)))

(bind-key "C-c d" #'delete-current-line)


(setq user-initials "vkj")


(defun insert-user-timestamp ()
  "Insert a quick timestamp using the value of `user-initials'."
  (interactive)
  (insert (format " ;; %s (%s): " user-initials
                  (format-time-string "%Y-%m-%d" (current-time))))) ;; vkj
;; introduced comment before user initials in the above format string


(bind-key "C-c n" #'insert-user-timestamp)
(bind-key "C-c ;" #'comment-or-uncomment-region)

;;; C-c C-

(defun delete-to-end-of-buffer ()
  (interactive)
  (kill-region (point) (point-max)))

(bind-key "C-c C-z" #'delete-to-end-of-buffer)

(defun copy-current-buffer-name ()
  (interactive)
  (let ((name (buffer-file-name)))
    (kill-new name)
    (message name)))

(bind-key "C-c C-0" #'copy-current-buffer-name)

;;; C-c M-

(defun unfill-paragraph (arg)
  (interactive "*p")
  (let (beg end)
    (forward-paragraph arg)
    (setq end (copy-marker (- (point) 2)))
    (backward-paragraph arg)
    (if (eolp)
        (forward-char))
    (setq beg (point-marker))
    (when (> (count-lines beg end) 1)
      (while (< (point) end)
        (goto-char (line-end-position))
        (let ((sent-end (memq (char-before) '(?. ?\; ?! ??))))
          (delete-indentation 1)
          (if sent-end
              (insert ? )))
        (end-of-line))
      (save-excursion
        (goto-char beg)
        (while (re-search-forward "[^.;!?:]\\([ \t][ \t]+\\)" end t)
          (replace-match " " nil nil nil 1))))))

(bind-key "C-c M-q" #'unfill-paragraph)

(defun unfill-region (beg end)
  (interactive "r")
  (setq end (copy-marker end))
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (unfill-paragraph 1)
      (forward-paragraph))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ELISP SLIME MODE FOR DOCUMENTATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'elisp-slime-nav)
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ORG-MODE SETTINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package dot-org
 :load-path ("override/org-mode/contrib/lisp/"
           "override/org-mode/lisp")
  :bind (("M-C"   . jump-to-org-agenda)
         ("M-M"   . org-inline-note)
         ("C-c a" . org-agenda)
         ("C-c S" . org-store-link)
         ("C-c l" . org-insert-link)
         ("C-. n" . org-velocity-read)))


(use-package org-download
  :load-path "/Users/vasantkj/.emacs.d/elpa/org-download")

;; change from require to use-package later
(add-to-list 'load-path "/Users/vasantkj/.emacs.d/override/org-mode/lisp")
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda() (org-bullets-mode 1)))

(eval-after-load "org"
  '(require 'ox-md nil t))

;; custom themes to be used in org-documents
(add-to-list 'load-path "/Users/vasantkj/.emacs.d/elpa/org-html-themes/setup")
;; generally good to use "theme-readtheorg.setup"

;; org-beautify theme  ;; vkj (2016-09-17): 
(add-to-list 'load-path
             "/Users/vasantkj/.emacs.d/override/org-mode/lisp/")


;; ox-minutes ;; vkj (2017-04-17)
(use-package ox-minutes
  :load-path "/Users/vasantkj/.emacs.d/elpa/ox-minutes")

;; native tab mode for source code in org-blocks  ;; vkj (2017-07-08):
(setq org-src-tab-acts-natively t)
(global-set-key (kbd "C-c c") 'org-capture)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package powerline
  :disabled t
  :load-path "site-lisp/powerline"
  :config
  (powerline-default-theme))

(use-package projectile
  :load-path "site-lisp/projectile"
  :diminish projectile-mode
  :commands projectile-global-mode
  :defer 5
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (use-package helm-projectile
    :config
    (setq projectile-completion-system 'helm)
    (helm-projectile-on))
  (projectile-global-mode))


;; enable elpy mode 
(elpy-enable)

;;  ;; vkj (2016-07-17): for setting indentation in python-mode
;;; Indentation for python
;; Ignoring electric indentation
;; If nothing works, try typing "Ctl-j" for newline and indentation
;; Enter key executes newline-and-indent
(defun set-newline-and-indent ()
  "Map the return key with `newline-and-indent'"
  (local-set-key (kbd "RET") 'newline-and-indent))
(add-hook 'python-mode-hook 'set-newline-and-indent)


;; vkj (2015-12-21): 
(use-package python-settings
  :load-path "settings"
:config
;; (use-package python-mode
;;   :load-path "site-lisp/python-mode"
;;   :mode ("\\.py\\'" . python-mode)
;;   :interpreter ("python" . python-mode)
;;   :config
;;   (defvar python-mode-initialized nil)

  (defun my-python-mode-hook ()
    (unless python-mode-initialized
      (setq python-mode-initialized t)

      (info-lookup-add-help
       :mode 'python-mode
       :regexp "[a-zA-Z_0-9.]+"
       :doc-spec
       '(("(python)Python Module Index" )
         ("(python)Index"
          (lambda
            (item)
            (cond
             ((string-match
               "\\([A-Za-z0-9_]+\\)() (in module \\([A-Za-z0-9_.]+\\))" item)
              (format "%s.%s" (match-string 2 item)
                       (match-string 1 item)))))))))

    (setq indicate-empty-lines t)
    (set (make-local-variable 'parens-require-spaces) nil)
    (setq indent-tabs-mode nil)

    (bind-key "C-c C-z" #'python-shell python-mode-map)
    (unbind-key "C-c c" python-mode-map)
(bind-key "<return>" #'newline-and-indent python-mode-map)

;; vkj (2016-01-11): introduced the lines below 
    (interactive)
    (python-shell-send-buffer)
    (python-shell-switch-to-shell))

  (add-hook 'python-mode-hook 'my-python-mode-hook))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package quickrun
  :disabled t
  :load-path "site-lisp/emacs-quickrun"
  :bind ("C-c C-r" . quickrun))

(use-package rainbow-mode
  :commands rainbow-mode)

(use-package recentf
  :defer 10
  :commands (recentf-mode
             recentf-add-file
             recentf-apply-filename-handlers)
  :preface
  (defun recentf-add-dired-directory ()
    (if (and dired-directory
             (file-directory-p dired-directory)
             (not (string= "/" dired-directory)))
        (let ((last-idx (1- (length dired-directory))))
          (recentf-add-file
           (if (= ?/ (aref dired-directory last-idx))
               (substring dired-directory 0 last-idx)
             dired-directory)))))
  :init
  (add-hook 'dired-mode-hook 'recentf-add-dired-directory)
  :config
  (recentf-mode 1))

(use-package repeat-insert
  :disabled t
  :commands (insert-patterned
             insert-patterned-2
             insert-patterned-3
             insert-patterned-4))

(use-package selectkey
  :disabled t
  :bind-keymap ("C-. b" . selectkey-select-prefix-map)
  :config
  (selectkey-define-select-key compile "c" "\\*compilation")
  (selectkey-define-select-key shell-command "o" "Shell Command")
  (selectkey-define-select-key shell "s" "\\*shell" (shell))
  (selectkey-define-select-key multi-term "t" "\\*terminal" (multi-term-next))
  (selectkey-define-select-key eshell "z" "\\*eshell" (eshell)))

(use-package session
  :if (not noninteractive)
  :load-path "site-lisp/session/lisp/"
  :preface
  (defun remove-session-use-package-from-settings ()
    (when (string= (file-name-nondirectory (buffer-file-name))
                   "settings.el")
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^ '(session-use-package " nil t)
          (delete-region (line-beginning-position)
                         (1+ (line-end-position)))))))

  ;; expanded folded secitons as required
  (defun le::maybe-reveal ()
    (when (and (or (memq major-mode  '(org-mode outline-mode))
                   (and (boundp 'outline-minor-mode)
                        outline-minor-mode))
               (outline-invisible-p))
      (if (eq major-mode 'org-mode)
          (org-reveal)
        (show-subtree))))

  (defvar server-process nil)

  (defun save-information ()
    (with-temp-message "Saving Emacs information..."
      (recentf-cleanup)

      (loop for func in kill-emacs-hook
            unless (memq func '(exit-gnus-on-exit server-force-stop))
            do (funcall func))

      (unless (or noninteractive
                  running-alternate-emacs
;;                  running-development-emacs
                  (and server-process
                       (eq 'listen (process-status server-process))))
        (server-start))))

  :config
  (add-hook 'before-save-hook 'remove-session-use-package-from-settings)
  (add-hook 'session-after-jump-to-last-change-hook 'le::maybe-reveal)
  (run-with-idle-timer 60 t 'save-information)
  (add-hook 'after-init-hook 'session-initialize t))

(use-package sh-script
  :defer t
  :init
  (defvar sh-script-initialized nil)
  (defun initialize-sh-script ()
    (unless sh-script-initialized
      (setq sh-script-initialized t)
      (info-lookup-add-help :mode 'shell-script-mode
                            :regexp ".*"
                            :doc-spec
                            '(("(bash)Index")))))

  (add-hook 'shell-mode-hook 'initialize-sh-script))

(use-package sh-toggle
  :bind ("C-. C-z" . shell-toggle))

(use-package slime
  :disabled t
  :load-path "site-lisp/slime"
  :commands (sbcl slime)
  :init
  (add-hook
   'slime-load-hook
   #'(lambda ()
       (slime-setup
        '(slime-asdf
          slime-autodoc
          slime-banner
          slime-c-p-c
          slime-editing-commands
          slime-fancy-inspector
          slime-fancy
          slime-fuzzy
          slime-highlight-edits
          slime-parse
          slime-presentation-streams
          slime-presentations
          slime-references
          slime-repl
          slime-sbcl-exts
          slime-package-fu
          slime-fontifying-fu
          slime-mdot-fu
          slime-scratch
          slime-tramp
          ;; slime-enclosing-context
          ;; slime-typeout-frame
          slime-xref-browser))

       (define-key slime-repl-mode-map [(control return)] 'other-window)

       (define-key slime-mode-map [return] 'pareditg-newline)
       (define-key slime-mode-map [(control ?h) ?F] 'info-lookup-symbol)))

  :config
  (progn
    (eval-when-compile
      (defvar slime-repl-mode-map))

    (setq slime-net-coding-system 'utf-8-unix)

    (setq slime-lisp-implementations
          '((sbcl
             ("sbcl" "--core"
              "/Users/johnw/Library/Lisp/sbcl.core-with-slime-X86-64")
             :init
             (lambda (port-file _)
               (format "(swank:start-server %S)\n" port-file)))
            (ecl ("ecl" "-load" "/Users/johnw/Library/Lisp/init.lisp"))
            (clisp ("clisp" "-i" "/Users/johnw/Library/Lisp/lwinit.lisp"))))

    (setq slime-default-lisp 'sbcl)
    (setq slime-complete-symbol*-fancy t)
    (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)

    (defun sbcl (&optional arg)
      (interactive "P")
      (let ((slime-default-lisp (if arg 'sbcl64 'sbcl))
            (current-prefix-arg nil))
        (slime)))
    (defun clisp () (interactive) (let ((slime-default-lisp 'clisp)) (slime)))
    (defun ecl () (interactive) (let ((slime-default-lisp 'ecl)) (slime)))

    (defun start-slime ()
      (interactive)
      (unless (slime-connected-p)
        (save-excursion (slime))))

    (add-hook 'slime-mode-hook 'start-slime)
    (add-hook 'slime-load-hook #'(lambda () (require 'slime-fancy)))
    (add-hook 'inferior-lisp-mode-hook #'(lambda () (inferior-slime-mode t)))

    (use-package hyperspec
      :config
      (setq common-lisp-hyperspec-root
            (expand-file-name "/Users/vasantkj/Library/Lisp/HyperSpec/")))))

(use-package smart-compile
  :disabled t
  :commands smart-compile
  :bind (("C-c c" . smart-compile)
         ("A-n"   . next-error)
         ("A-p"   . previous-error)))

;;;;;;;;;;;;;;;;;;;;;;;;;; smartparens-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; to delete a balancing quote or parens, type M-x sp-splice-sexp

(use-package smartparens-config
  ;;  :disabled t
  ;;  :ensure smartparens
  :load-path "site-lisp/smartparens"
  :config
  (progn
    (show-smartparens-global-mode t)))
(add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)

(bind-keys
 :map smartparens-mode-map
 ("C-M-a" . sp-beginning-of-sexp)
 ("C-M-e" . sp-end-of-sexp)

 ("C-<down>" . sp-down-sexp)
 ("C-<up>"   . sp-up-sexp)
 ("M-<down>" . sp-backward-down-sexp)
 ("M-<up>"   . sp-backward-up-sexp)

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
 ("C-c `"  . wrap-with-back-quotes))

(add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
(add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode)

(defmacro def-pairs (pairs)
  `(progn
     ,@(loop for (key . val) in pairs
          collect
            `(defun ,(read (concat
                            "wrap-with-"
                            (prin1-to-string key)
                            "s"))
                 (&optional arg)
               (interactive "p")
               (sp-wrap-with-pair ,val)))))

(def-pairs ((paren . "(")
            (bracket . "[")
            (brace . "{")
            (single-quote . "'")
            (double-quote . "\"")
            (back-quote . "`")))

;;;;;;;;;;;;;;;;;;;;;;; smartparens-mode ends ;;;;;;;;;;;;;;;;;;;;;;;;

(use-package smerge-mode
  :commands smerge-mode
  :config
  (setq smerge-command-prefix (kbd "C-. C-.")))

(use-package stopwatch
  :bind ("<f8>" . stopwatch))

(use-package sunrise-commander
  :load-path "site-lisp/sunrise-commander"
  :bind (("C-c j" . my-activate-sunrise)
         ("C-c C-j" . sunrise-cd))
  :commands sunrise
  :defines sr-tabs-mode-map
  :preface
  (defun my-activate-sunrise ()
    (interactive)
    (let ((sunrise-exists
           (loop for buf in (buffer-list)
                 when (string-match " (Sunrise)$" (buffer-name buf))
                 return buf)))
      (if sunrise-exists
          (call-interactively 'sunrise)
        (sunrise "/Users/vasantkj/dl/" "/Users/vasantkj/Archives/"))))

  :config
  (require 'sunrise-x-modeline)
  (require 'sunrise-x-tree)
  (require 'sunrise-x-tabs)

  (bind-key "/" #'sr-sticky-isearch-forward sr-mode-map)
  (bind-key "<backspace>" #'sr-scroll-quick-view-down sr-mode-map)
  (bind-key "C-x t" #'sr-toggle-truncate-lines sr-mode-map)

  (bind-key "q" #'sr-history-prev sr-mode-map)
  (bind-key "z" #'sr-quit sr-mode-map)

  (unbind-key "C-e" sr-mode-map)
  (unbind-key "C-p" sr-tabs-mode-map)
  (unbind-key "C-n" sr-tabs-mode-map)
  (unbind-key "M-<backspace>" sr-term-line-minor-mode-map)

  (bind-key "M-[" #'sr-tabs-prev sr-tabs-mode-map)
  (bind-key "M-]" #'sr-tabs-next sr-tabs-mode-map)

  (defun sr-browse-file (&optional file)
    "Display the selected file with the default appication."
    (interactive)
    (setq file (or file (dired-get-filename)))
    (save-selected-window
      (sr-select-viewer-window)
      (let ((buff (current-buffer))
            (fname (if (file-directory-p file)
                       file
                     (file-name-nondirectory file)))
            (app (cond
                  ((eq system-type 'darwin)       "open %s")
                  ((eq system-type 'windows-nt)   "open %s")
                  (t                              "xdg-open %s"))))
        (start-process-shell-command "open" nil (format app file))
        (unless (eq buff (current-buffer))
          (sr-scrollable-viewer (current-buffer)))
        (message "Opening \"%s\" ..." fname))))

  (defun sr-goto-dir (dir)
    "Change the current directory in the active pane to the given one."
    (interactive (list (progn
                         (require 'lusty-explorer)
                         (lusty-read-directory))))
    (if sr-goto-dir-function
        (funcall sr-goto-dir-function dir)
      (unless (and (eq major-mode 'sr-mode)
                   (sr-equal-dirs dir default-directory))
        (if (and sr-avfs-root
                 (null (posix-string-match "#" dir)))
            (setq dir (replace-regexp-in-string
                       (expand-file-name sr-avfs-root) "" dir)))
        (sr-save-aspect
         (sr-within dir (sr-alternate-buffer (dired dir))))
        (sr-history-push default-directory)
        (sr-beginning-of-buffer)))))

(use-package swiper
  :load-path "site-lisp/swiper"
  :bind ("C-. C-s" . swiper))

(use-package tablegen-mode
  :mode ("\\.td\\'" . tablegen-mode))

(use-package texinfo
  :defines texinfo-section-list
  :mode ("\\.texi\\'" . texinfo-mode)
  :config
  (defun my-texinfo-mode-hook ()
    (dolist (mapping '((?b . "emph")
                       (?c . "code")
                       (?s . "samp")
                       (?d . "dfn")
                       (?o . "option")
                       (?x . "pxref")))
      (local-set-key (vector (list 'alt (car mapping)))
                     `(lambda () (interactive)
                        (TeX-insert-macro ,(cdr mapping))))))

  (add-hook 'texinfo-mode-hook 'my-texinfo-mode-hook)

  (defun texinfo-outline-level ()
    ;; Calculate level of current texinfo outline heading.
    (require 'texinfo)
    (save-excursion
      (if (bobp)
          0
        (forward-char 1)
        (let* ((word (buffer-substring-no-properties
                      (point) (progn (forward-word 1) (point))))
               (entry (assoc word texinfo-section-list)))
          (if entry
              (nth 1 entry)
            5))))))

(use-package tiny
  :load-path "site-lisp/tiny"
  :bind ("C-. N" . tiny-expand))

(use-package tramp-sh
  :load-path "override/tramp"
  :defer t
  :config
  (add-to-list 'tramp-remote-path "/run/current-system/sw/bin"))

(use-package vkill
  :commands vkill
  :bind ("C-x L" . vkill-and-helm-occur)
  :preface
  (defun vkill-and-helm-occur ()
    (interactive)
    (vkill)
    (call-interactively #'helm-occur))

  :config
  (setq vkill-show-all-processes t))

(use-package wcount
  :disabled t
  :commands wcount-mode)

(use-package which-key
  :load-path "site-lisp/emacs-which-key"
  :diminish which-key-mode
  :commands which-key-mode
  :defer 10
  :config
  (which-key-mode 1))

(use-package whitespace
  :diminish (global-whitespace-mode
             whitespace-mode
             whitespace-newline-mode)
  :commands (whitespace-buffer
             whitespace-cleanup
             whitespace-mode)
  :defines (whitespace-auto-cleanup
            whitespace-rescan-timer-time
            whitespace-silent)
  :preface
  (defun normalize-file ()
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (whitespace-cleanup)
      (delete-trailing-whitespace)
      (goto-char (point-max))
      (delete-blank-lines)
      (set-buffer-file-coding-system 'unix)
      (goto-char (point-min))
      (while (re-search-forward "\r$" nil t)
        (replace-match ""))
      (set-buffer-file-coding-system 'utf-8)
      (let ((require-final-newline t))
        (save-buffer))))

  (defun maybe-turn-on-whitespace ()
    "Depending on the file, maybe clean up whitespace."
    (let ((file (expand-file-name ".clean"))
          parent-dir)
      (while (and (not (file-exists-p file))
                  (progn
                    (setq parent-dir
                          (file-name-directory
                           (directory-file-name
                            (file-name-directory file))))
                    ;; Give up if we are already at the root dir.
                    (not (string= (file-name-directory file)
                                  parent-dir))))
        ;; Move up to the parent dir and try again.
        (setq file (expand-file-name ".clean" parent-dir)))
      ;; If we found a change log in a parent, use that.
      (when (and (file-exists-p file)
                 (not (file-exists-p ".noclean"))
                 (not (and buffer-file-name
                           (string-match "\\.texi\\'" buffer-file-name))))
        (add-hook 'write-contents-hooks
                  #'(lambda () (ignore (whitespace-cleanup))) nil t)
        (whitespace-cleanup))))

  :init
  (add-hook 'find-file-hooks 'maybe-turn-on-whitespace t)

  :config
  (remove-hook 'find-file-hooks 'whitespace-buffer)
  (remove-hook 'kill-buffer-hook 'whitespace-buffer)

  ;; For some reason, having these in settings.el gets ignored if whitespace
  ;; loads lazily.
  (setq whitespace-auto-cleanup t
        whitespace-line-column 80
        whitespace-rescan-timer-time nil
        whitespace-silent t
        whitespace-style '(face trailing lines space-before-tab empty)))

(use-package winner
  :if (not noninteractive)
  :defer 5
  :bind (("M-N" . winner-redo)
         ("M-P" . winner-undo))
  :config
  (winner-mode 1))

(use-package workgroups
  :load-path "site-lisp/workgroups"
  :diminish workgroups-mode
  :bind-keymap ("C-\\" . wg-map)
  :config
  (workgroups-mode 1)

  (let ((workgroups-file (expand-file-name "workgroups" user-data-directory)))
    (if (file-readable-p workgroups-file)
        (wg-load workgroups-file)))

  (bind-key "C-\\" #'wg-switch-to-previous-workgroup wg-map)
  (bind-key "\\" #'toggle-input-method wg-map))

(use-package wrap-region
  :load-path "site-lisp/wrap-region"
  :commands wrap-region-mode
  :diminish wrap-region-mode
  :config
  (wrap-region-add-wrappers
   '(("$" "$")
     ("/" "/" nil ruby-mode)
     ("/* " " */" "#" (java-mode javascript-mode css-mode c-mode c++-mode))
     ("`" "`" nil (markdown-mode ruby-mode shell-script-mode)))))

(use-package yaml-mode
  :load-path "site-lisp/yaml-mode"
  :mode ("\\.ya?ml\\'" . yaml-mode))


(use-package company
  :defer 5
  :diminish
  :commands (company-mode company-indent-or-complete-common)
  :init
  (dolist (hook '(emacs-lisp-mode-hook
                  c-mode-common-hook))
    (add-hook hook
              #'(lambda ()
                  (local-set-key (kbd "<tab>")
                                 #'company-indent-or-complete-common))))
  :config
  ;; From https://github.com/company-mode/company-mode/issues/87
  ;; See also https://github.com/company-mode/company-mode/issues/123
  (defadvice company-pseudo-tooltip-unless-just-one-frontend
      (around only-show-tooltip-when-invoked activate)
    (when (company-explicit-action-p)
      ad-do-it))

  ;; See http://oremacs.com/2017/12/27/company-numbers/
  (defun ora-company-number ()
    "Forward to `company-complete-number'.
  Unless the number is potentially part of the candidate.
  In that case, insert the number."
    (interactive)
    (let* ((k (this-command-keys))
           (re (concat "^" company-prefix k)))
      (if (cl-find-if (lambda (s) (string-match re s))
                      company-candidates)
          (self-insert-command 1)
        (company-complete-number (string-to-number k)))))

  (let ((map company-active-map))
    (mapc
     (lambda (x)
       (define-key map (format "%d" x) 'ora-company-number))
     (number-sequence 0 9))
    (define-key map " " (lambda ()
                          (interactive)
                          (company-abort)
                          (self-insert-command 1))))

  (defun check-expansion ()
    (save-excursion
      (if (outline-on-heading-p t)
          nil
        (if (looking-at "\\_>") t
          (backward-char 1)
          (if (looking-at "\\.") t
            (backward-char 1)
            (if (looking-at "->") t nil))))))

  (define-key company-mode-map [tab]
    '(menu-item "maybe-company-expand" nil
                :filter (lambda (&optional _)
                          (when (check-expansion)
                            #'company-complete-common))))

  (eval-after-load "yasnippet"
    '(progn
       (defun company-mode/backend-with-yas (backend)
         (if (and (listp backend) (member 'company-yasnippet backend))
             backend
           (append (if (consp backend) backend (list backend))
                   '(:with company-yasnippet))))
       (setq company-backends
             (mapcar #'company-mode/backend-with-yas company-backends))))

  (global-company-mode 1))





(use-package auto-yasnippet
  ;; :after yasnippet
  :bind (("C-c y a" . aya-create)
	 ("C-c y e" . aya-expand)
	 ("C-c y o" . aya-open-line)))


;;;;;;;;;;;;;;;;;;;;;;;;;; loading ipython settings ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(use-package python-settings
  :load-path "settings") 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




;; ==================================================
;; CUT COPY PASTE with a single function key
;; customize emacs so that the Copy command will copy the current line when
;; there's no text selection. Same for Cut.
(defun copy-line-or-region ()
  "Copy current line, or current text selection."
  (interactive)
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (kill-ring-save (line-beginning-position) (line-beginning-position 2)) ) )

(defun cut-line-or-region ()
  "Cut the current line, or current text selection."
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (kill-region (line-beginning-position) (line-beginning-position 2)) ) )

;; Now we need to bind the CUT COPY PASTE with keys 
(global-set-key (kbd "<f2>") 'cut-line-or-region) ;; cut.
(global-set-key (kbd "<f3>") 'copy-line-or-region) ;; copy.
(global-set-key (kbd "<f4>") 'yank) ;; paste.
;; --------------------------------------------------

;; GOOD TIPS FROM STEVE YEGGE
;; if you use query-replace-regexp frequently, then create an alias
;; now you can use M-qrr to invoke the function.
(defalias 'qrr 'query-replace-regexp)

;; ==================================================
;; for typing a TEXT COMMENT and centering with comment marker
;; ==================================================

(use-package line-comment-banner
  :load-path "elpa/"
  :bind ("C-;" . line-comment-banner))

;; (require 'line-comment-banner)
;; (global-set-key (kbd "C-;") 'line-comment-banner) 

;; ;; ==================================================
;; ;; ORG-MODE SETUP
;; ;; The following enables org-mode for most buffers, and is the
;; ;; default mode for .org, .org_archive and .txt files
;; ;; ==================================================
(add-to-list 'load-path "/Users/vasantkj/.emacs.d/elpa/org")
(add-to-list 'load-path (expand-file-name "/Users/vasantkj/git/org-mode/lisp"))
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(require 'org)
(require 'ob-tangle)
;; (load-file "/Users/302001318/org-init.el") 
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
 (define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(setq org-startup-folded t)
(setq org-startup-indented t)  
;; (message "finished loading upto line 440, which is org_init_el")
(setq org-alphabetical-lists t)
;; Explicitly load required exporters
(require 'ox-html)
(require 'ox-latex)
(require 'ox-beamer)
(require 'ox-ascii)
(require 'ox-org)
(require 'ox-publish)
(setq org-reveal-root "file:///Users/vasantkj/reveal.js")  ;; vkj (2016-09-05): needed for ox-reveal
(require 'ox-reveal)  ;; vkj (2016-09-05): was added for making presentations

(add-to-list
 'org-latex-packages-alist '("" "minted"))
(add-to-list
 'org-latex-packages-alist '("" "color"))
(setq org-latex-listings 'minted)
(setq org-latex-minted-options
'(("frame" "lines")
("fontsize" "\\tiny")
("linenos" "")))

;; (use-package indent-guide
;;   :load-path "/Users/vasantkj/.emacs.d/elpa/indent-guide"
;; :config (indent-guide-global-mode)
;; (set-face-background 'indent-guide-face "dimgray")
;; (setq indent-guide-char "|")
;; )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Strike through DONE headlines ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my/modify-org-done-face ()
  (setq org-fontify-done-headline t)
  (set-face-attribute 'org-done nil :strike-through t)
  (set-face-attribute 'org-headline-done nil
                      :strike-through t
                      :foreground "light gray"))
(add-hook 'org-mode-hook 'my/modify-org-done-face)
(setq org-fontify-done-headline t)
(set-face-attribute 'org-done nil :strike-through t)
(set-face-attribute 'org-headline-done nil :strike-through t)

;; ==================================================
;; keybinding for evaluating lisp forms
;; ==================================================
(define-key global-map "\C-\M-y" 'eval-defun)

(defun up-list+ ()
  (interactive)
  (if (in-string-p)
      (while (in-string-p)
        (forward-char))
    (up-list)))

(defun eval-parent-sexp ()
  "Cause sometimes you just want to eval just the immediate form. not the top level,
but without going to the closing paren and evaling there."
  (interactive)
  (save-excursion
    ;; get out of string if in it
    (dotimes (c (if (in-string-p) 2 1))
      (up-list+))
    (let ((cmd (key-binding (kbd "C-x C-e"))))
      (if (eq cmd 'slime-eval-last-exxpression)
          (funcall cmd)
        (funcall cmd '())))))

(global-set-key (kbd "C-M-S-x") 'eval-parent-sexp)

;; my keybinding definition for evaluating last-sexp and print value in buffer rather
;; than in minibuffer
(global-set-key (kbd "C-x C-a") 'eval-print-last-sexp)
;; ;; --------------------------------------------------


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SMART PARENS MODE
;; enable smartparens on startup and hook it with some major hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package smartparens
  :disabled t
  :load-path "elpa/smartparens-1.4"
  :commands (smartparens-mode show-smartparens-mode)
  :config
  (use-package smartparens-config))


(defun uncomment-sexp (&optional n)
  "Uncomment a sexp around point."
  (interactive "P")
  (let* ((initial-point (point-marker))
         (inhibit-field-text-motion t)
         (p)
         (end (save-excursion
                (when (elt (syntax-ppss) 4)
                  (re-search-backward comment-start-skip
                                      (line-beginning-position)
                                      t))
                (setq p (point-marker))
                (comment-forward (point-max))
                (point-marker)))
         (beg (save-excursion
                (forward-line 0)
                (while (and (not (bobp))
                            (= end (save-excursion
                                     (comment-forward (point-max))
                                     (point))))
                  (forward-line -1))
                (goto-char (line-end-position))
                (re-search-backward comment-start-skip
                                    (line-beginning-position)
                                    t)
                (ignore-errors
                  (while (looking-at-p comment-start-skip)
                    (forward-char -1)))
                (point-marker))))
    (unless (= beg end)
      (uncomment-region beg end)
      (goto-char p)
      ;; Indentify the "top-level" sexp inside the comment.
      (while (and (ignore-errors (backward-up-list) t)
                  (>= (point) beg))
        (skip-chars-backward (rx (syntax expression-prefix)))
        (setq p (point-marker)))
      ;; Re-comment everything before it. 
      (ignore-errors
        (comment-region beg p))
      ;; And everything after it.
      (goto-char p)
      (forward-sexp (or n 1))
      (skip-chars-forward "\r\n[:blank:]")
      (if (< (point) end)
          (ignore-errors
            (comment-region (point) end))
        ;; If this is a closing delimiter, pull it up.
        (goto-char end)
        (skip-chars-forward "\r\n[:blank:]")
        (when (eq 5 (car (syntax-after (point))))
          (delete-indentation))))
    ;; Without a prefix, it's more useful to leave point where
    ;; it was.
    (unless n
      (goto-char initial-point))))

(defun comment-sexp--raw ()
  "Comment the sexp at point or ahead of point."
  (pcase (or (bounds-of-thing-at-point 'sexp)
             (save-excursion
               (skip-chars-forward "\r\n[:blank:]")
               (bounds-of-thing-at-point 'sexp)))
    (`(,l . ,r)
     (goto-char r)
     (skip-chars-forward "\r\n[:blank:]")
     (save-excursion
       (comment-region l r))
     (skip-chars-forward "\r\n[:blank:]"))))

(defun comment-or-uncomment-sexp (&optional n)
  "Comment the sexp at point and move past it.
If already inside (or before) a comment, uncomment instead.
With a prefix argument N, (un)comment that many sexps."
  (interactive "P")
  (if (or (elt (syntax-ppss) 4)
          (< (save-excursion
               (skip-chars-forward "\r\n[:blank:]")
               (point))
             (save-excursion
               (comment-forward 1)
               (point))))
      (uncomment-sexp n)
    (dotimes (_ (or n 1))
      (comment-sexp--raw))))


(global-set-key (kbd "C-M-;") #'comment-or-uncomment-sexp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto complete package

(use-package auto-complete
  :load-path "elpa/auto-complete-1.4"
  :init
  (require 'auto-complete-config)
  (ac-config-default)
  (add-to-list 'ac-dictionary-directories "/Users/vasantkj/.emacs.d/elpa/auto-complete-1.4/dict") )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; goto-last-change
;; vkj (2015-12-31):

(use-package goto-last-change
  :load-path "/Users/vasantkj/.emacs.d/site-lisp/")
(global-set-key  (kbd "C-x C-\\") 'goto-last-change)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-startup-with-inline-images t)  ;; vkj (2016-09-13): for image
;; inclusions in org files
(put 'narrow-to-region 'disabled nil)




;;;;;;;;;;;;;;;;;;;;;;;;; prompting during keyboard macro ;;;;;;;;;;;;;;;;;;;
;; ;; Using 'C-u C-x q' while recording a macro will, at macro
;; execution, drop in to a recursive editing mode allowing you to
;; intervene with custom editing on each macro iteration.  Pressing
;; 'C-x Q' during macro definition will present you with a minibuffer
;; promt(using recursive edit). Inserting some text and pressing RET
;; will end recursive edit and continue the definition of the macro in
;; the minibuffer.  Pressing RET again will insert the text at point.

 (defun my-macro-query (arg)
  "Prompt for input using minibuffer during kbd macro execution.
With prefix argument, allows you to selct what prompt string to use.
If the input is non-empty, it is inserted at point."
  (interactive "p")
  (let* ((query (lambda() (kbd-macro-query t)))
         (prompt ( if arg (read-from-minibuffer "PROMPT: ") "Input: "))
         (input (unwind-protect
                    (progn
                      (add-hook 'minibuffer-setup-hook query)
                      (read-from-minibuffer prompt))
                  (remove-hook 'minibuffer-setup-hook query))))
  (unless (string= "" input) (insert input))))

(global-set-key "\C-xQ" 'my-macro-query)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;; making colored face text ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vas/make-word-red (begin end)
"make current region colored red, using text properties"
(interactive "r")
(put-text-property begin end 'font-lock-face '(:foreground "red")))

(defun vas/make-word-green (begin end)
"make current region colored red, using text properties"
(interactive "r")
(put-text-property begin end 'font-lock-face '(:foreground "green")))

(defun vas/make-word-yellow (begin end)
"make current region colored red, using text properties"
(interactive "r")
(put-text-property begin end 'font-lock-face '(:foreground "yellow")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun vas/make-bold (begin end)
  "make the region bold using overlay"
  (interactive "r")
  (progn
    (overlay-put (make-overlay begin end ) 'face 'bold)
    (setq mark-active nil )))

(defun vas/undo-bold (begin end )
  (interactive "r")
  (remove-overlays begin end))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; C-c c is for capture, it's good enough for me
(define-key global-map "\C-cc" 'org-capture)
;; force utf-8
(setq org-export-coding-system 'utf-8)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(initial-scratch-message nil)
 '(org-capture-templates
   (quote
    (("n" "Note" entry
      (file "~/Documents/notes.org")
      "* %? :NOTE:
%U
%a
")
     ("l" "Link" entry
      (file+heading "~/Documents/VasLinks" "Links to visit")
      "* %? :URL: %^L %^g %U 
")
     ("c" "Calendar" entry
      (file+headline "~/Documents/notes.txt" "Inbox")
      "* APPT %? SCHEDULED: %t :PROPERTIES: :ID:
%(shell-command-to-string \"uuidgen\"):CREATED:  %U :END:" :prepend t)
     ("t" "Add Task" entry
      (file+headline "~/Documents/notes.txt" "Inbox")
      "* TODO %? SCHEDULED: %t :PROPERTIES: :ID:
%(shell-command-to-string \"uuidgen\"):CREATED:  %U :END:" :prepend t))))
 '(package-selected-packages
   (quote
    (material-theme math-at-point better-defaults org-download org-pdfview org-present org-presie org-web-tools org-cliplink org-ac org-autolist org-beautify-theme org-bookmark-heading org-bullets org-easy-img-insert org-ehtml org-elisp-help yasnippet yasnippet-bundle auto-yasnippet dot-mode sphinx-frontend ox-rst auto-complete-rst elpy company-jedi zenburn whole-line-or-region web-mode undo-tree smex smart-mode-line powerline plantuml-mode paredit-everywhere ox-twbs ov org orca multiple-cursors multi-term markdown-mode magit key-chord ido-vertical-mode ido-ubiquitous hydra helm-projectile helm-helm-commands helm-gtags helm-delicious helm-dash helm-ag go-snippets go-eldoc geiser expand-region ess-smart-underscore ess-R-object-popup emmet-mode elisp-slime-nav ein-mumamo company cider bug-hunter ample-theme)))
 '(session-use-package t nil (session)))
 '(session-use-package t nil (session))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; (add-to-list 'load-path "~/.emacs.d/elpa/multi-web-mode")
;; (require 'multi-web-mode)
;; (setq mweb-default-major-mode 'html-mode)
;; (setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
;;                   (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
;;                   (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
;; (setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
;; (multi-web-global-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; using web-mode for html editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "~/.emacs.d/elpa/web-mode")
(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (setq web-mode-engines-alist '(("django" . "\\.html\\'")))
  (setq web-mode-ac-source-alist '(("css" . (ac-source-css-property))
				   ("html" . (ac-source-words-in-buffer ac-source-abbrev))))
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-quoting t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; emmet-mode for html templating
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'web-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smex-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; extended M-x mode for command selections in minibuffer;;;;

(use-package smex
  :ensure t
  :bind (("M-x" . smex))
  :config (smex-initialize))				 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;; jedi setup for python code completion ;;;;
;; (load-file "~/.emacs.d/elpa/emacs-python-environment/python-environment.el")
;; (add-to-list 'load-path "~/.emacs.d/elpa/jedi-core-20170121.610")
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:complete-on-dot t)                 ; optional
;; (setq jedi:server-command "~/.emacs.d/elpa/jedi-core-20170121.610/jediepcserver.py")


;;;;;;;;;;;;;;;;;;;;;;;;; company-jedi setup ;;;;;;;;;;;;;;;;;;;;;;;;;

;; ==================================================
;; copy the file-name/full-path in dired buffer into clipboard
;; `w` => copy file name
;; `C-u 0 w` => copy full path
(defadvice dired-copy-filename-as-kill (after dired-filename-to-clipboard activate)
  (with-temp-buffer
    (insert (current-kill 0))
    (shell-command-on-region (point-min) (point-max)
                             (cond
                              ((eq system-type 'cygwin) "putclip")
                              ((eq system-type 'darwin) "pbcopy")
                              (t "xsel -ib")
                              )))
  (message "%s => clipboard" (current-kill 0))
  )
;; --------------------------------------------------
