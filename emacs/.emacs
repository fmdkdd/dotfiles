;; -*- lexical-binding: t; -*-

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Packages
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; To measure the time taken to load the whole file.  Care has been taken (using
;; use-package) to keep this under 500ms.
(defconst emacs-start-time (current-time))

;; Always load fresh .el files over byte-compiled ones
(setq load-prefer-newer t)

(eval-and-compile
  (package-initialize))
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(use-package utils
  :load-path "~/.emacs.d/elisp/")

;; Required for byte-compiled init file to load
(use-package bind-key)

;; Used for some `use-package` calls
(use-package diminish :ensure t)


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Theming
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(set-face-attribute 'default nil :family "Dina" :height 80 :weight 'normal)
;; To eval when others can't read my screen:
;; (set-face-attribute 'default nil :family "Fira Mono" :height 120 :weight 'normal)

(add-to-list 'custom-theme-load-path
             (locate-user-emacs-file "elisp"))
(load-theme 'amber 'no-confirm)

;; Allow to color parenthesis/braces/brackets differently from the
;; default face
(use-package rainbow-delimiters
  :ensure t
  :config
  (setq rainbow-delimiters-max-face-count 1)
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode-enable))


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Interface
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Hide useless startup messages
(setq inhibit-startup-message t)
(fset 'display-startup-echo-area-message 'ignore)

;; Unclutter UI
(tool-bar-mode   0)
(scroll-bar-mode 0)
(menu-bar-mode   0)

;; Modeline
(column-number-mode t)
(size-indication-mode t)

(setq history-length 1000)

;; Shorter yes/no answers
(fset 'yes-or-no-p 'y-or-n-p)

;; Much more useful
(setq mouse-yank-at-point t)

;; Show empty lines in the fringe
(setq-default indicate-empty-lines t)

;; Prompt keybindings
(use-package which-key
  :defer 3
  :diminish which-key-mode
  :config
  (which-key-mode 1))

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :bind (("C-c C-r" . ivy-resume)
         ("C-x b" . ivy-switch-buffer)
         ("C-x C-b" . ivy-switch-buffer))
  :config
  ;; Add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’
  (setq ivy-use-virtual-buffers t
        ;; Use absolute name for recent files
        ivy-virtual-abbreviate 'full)

  ;; Allow selecting prompt in find-file if there are no matches
  (setq ivy-use-selectable-prompt t)
  (define-key ivy-minibuffer-map (kbd "C-l") #'ivy-backward-delete-char)

  (setq ivy-on-del-error-function nil ;; don't quit when erasing all input
        ivy-height 15                 ;; number of result lines to display
        ivy-count-format ""           ;; do not count candidates
        ivy-initial-inputs-alist nil) ;; no default regexp in prompt

  (setq ivy-re-builders-alist
	;; Match prompt in any order
        '((t . ivy--regex-ignore-order)))

  ;; Turns on ivy for kill-buffer, org-refile, etc.
  (ivy-mode +1))

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-h a" . counsel-apropos)
         ("C-c i" . counsel-imenu)
         ("C-c C-i" . counsel-imenu)
         ("C-c /" . counsel-rg)
         ("C-c f l" . counsel-locate)))

(use-package swiper
  :ensure t
  :bind (("M-s M-s" . swiper)))


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Files
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Backups in .emacs.d
(setq backup-directory-alist `(("." . ,(locate-user-emacs-file "backups"))))

;; I use customize file for machine-local settings. This file goes to
;; 'emacs.d', and is not version controlled.
(setq custom-file (locate-user-emacs-file "local-preferences.el"))
(load custom-file :no-error)

;; Read-only files are in view mode
(setq view-read-only t)

;; Follow symlinks to versioned files
(setq vc-follow-symlinks t)

;; Keep track of recent files
(use-package recentf
  :demand t
  :config
  (setq recentf-max-saved-items 500
        recentf-auto-cleanup   '300)
  (recentf-mode t))

;; Save cursor location in files
(if (< emacs-major-version 25)
    (progn (require 'saveplace)
           (setq-default save-place t))
  (save-place-mode 1))

(setq delete-by-moving-to-trash t)


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Editing
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Invaluable
(show-paren-mode t)

;; Spaces over tabs
(setq-default indent-tabs-mode nil)
(setq require-final-newline t)
(delete-selection-mode 1)

;; Auto fill
(setq-default fill-column 80)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(diminish 'auto-fill-function)

;; Auto fill comments in prog modes
(add-hook 'prog-mode-hook #'auto-fill-comments)

;; Delete trailing whitespace on file save
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; This is useful just for thinking about updating copyright years.
(add-hook 'before-save-hook #'copyright-update)

;; Hippie expand everything
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;; Anzu makes query-replace more intuitive
(use-package anzu
  :ensure t
  :commands (anzu-query-replace anzu-query-replace-regexp)
  :diminish anzu-mode
  :init
  (global-set-key [remap query-replace] #'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] #'anzu-query-replace-regexp))

;; Expand-region is awesome (most of the time)
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; This is my evil-surround fix
(use-package delimiter
  :load-path "elisp/"
  :bind (("C-, s" . delimiter-surround)
         ("C-, c" . delimiter-change)
         ("C-, d" . delimiter-delete)))

(use-package markdown-mode
  :ensure t
  :mode ("\\.md" . markdown-mode))


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Org mode
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(use-package org
  :bind (("C-c o l" . org-store-link))
  :config
  ;; Org mode fucks up all my bindings
  (define-key org-mode-map (kbd "C-,") nil)
  (define-key org-mode-map (kbd "C-a") nil)

  ;; And at the time, defines stupidly long bindings for useful commands
  (define-key org-mode-map (kbd "M-p") #'outline-previous-heading)
  (define-key org-mode-map (kbd "M-n") #'outline-next-heading)

  ;; Why has org-mode picked a hard to hit key for inline code is beyond me.
  ;; Tip: C-q ` for typing a backtick after that rebinding.
  (define-key org-mode-map (kbd "`") "~")

  ;; Reuse M-, and M-. for navigation
  (define-key org-mode-map (kbd "M-.") #'org-open-at-point)
  (define-key org-mode-map (kbd "M-,") #'org-mark-ring-goto)

  (setq org-adapt-indentation    nil    ; indentation is lost space
        org-edit-src-content-indentation 0
        org-src-fontify-natively t      ; more useful
        org-log-done             t)     ; log all the things

  (setq org-default-notes-file "tasks.org")

  ;; Activate gnuplot in Babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((gnuplot . t)))

  ;; "reset" is mandatory for all gnuplot src blocks
  (with-eval-after-load 'ob-gnuplot
    (add-to-list 'org-babel-default-header-args:gnuplot
                 '(:prologue . "reset"))))

(use-package org-agenda
  :bind (("C-c o a" . org-agenda))
  :config
  ;; Custom agenda command
  (setq org-agenda-window-setup 'current-window
        org-agenda-custom-commands
        '(("n" "Agenda and all unscheduled TODO's"
           ((agenda "")
            (todo "WAIT" ((org-agenda-overriding-header "Waiting")))
            (todo "" ((org-agenda-overriding-header "Upcoming deadlines")
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'notdeadline))
                      (org-agenda-todo-ignore-deadlines 'near)))
            (todo "TODO" ((org-agenda-overriding-header "Unscheduled tasks")
                          (org-agenda-todo-ignore-scheduled 'all)
                          (org-agenda-todo-ignore-deadlines 'all))))))))

(use-package org-capture
  :bind (("C-c o c" . org-capture))
  :config
  (setq org-capture-templates
        '(("t" "Task" entry (file+headline "" "Tasks")
           "* TODO %?\nSee %a")
          ("r" "Rendez-vous" entry (file+headline "" "Rendez-vous")
           "* %?"))))


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Programming
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Indentation
(setq-default rust-indent-offset 2)
(setq-default js-indent-level 2)
(setq-default css-indent-offset 2)

(c-add-style "user-java"
             '("java"
               (c-basic-offset . 2)))
(add-hook 'java-mode-hook (lambda () (c-set-style "user-java")))
(add-hook 'java-mode-hook (lambda () (subword-mode)))

;; Syntax checking
(use-package flycheck
  :load-path "~/proj/flycheck"
  :commands (global-flycheck-mode)
  :init
  ;; Load global-flycheck-mode only before saving a file,
  ;; and clean up the hook once that happens
  (defun fmdkdd/init-flycheck ()
    (if (bound-and-true-p global-flycheck-mode)
        (remove-hook 'before-save-hook #'fmdkdd/init-flycheck)
      (global-flycheck-mode +1)))
  (add-hook 'before-save-hook #'fmdkdd/init-flycheck)
  :config
  (setq flycheck-display-errors-delay 0.125
        flycheck-check-syntax-automatically '(save)
        flycheck-emacs-lisp-load-path 'inherit
        flycheck-global-modes '(emacs-lisp-mode c-mode c++-mode rust-mode)))

;; If we load a rust buffer though, we need this
(use-package flycheck-rust
  :load-path "~/proj/flycheck-rust"
  :after rust-mode
  :config
  (add-hook 'rust-mode-hook #'flycheck-rust-setup))

(use-package flycheck-inline
  :load-path "~/proj/flycheck-inline"
  :after flycheck
  :config
  (flycheck-inline-mode +1))

;; Magit!
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :config
  ;; magit turns this on for files under git (which makes sense)
  (diminish 'auto-revert-mode)

  (setq git-commit-fill-column 72)

  ;; I don't use these, but they shadow useful global bindings
  (define-key magit-status-mode-map (kbd "M-1") nil)
  (define-key magit-status-mode-map (kbd "M-2") nil)
  (define-key magit-status-mode-map (kbd "M-3") nil))

;; Highlight TODO, FIXME keywords
(add-hook 'prog-mode-hook #'add-watchwords)

;; GTAGS are very useful for C-mode, at least
(use-package counsel-gtags
  :ensure t
  :bind (("M-." . counsel-gtags-dwim)
         ("M-," . counsel-gtags-go-backward))
  :init
  (defun fmdkdd/add-update-gtags-hook ()
    (add-hook 'after-save-hook #'counsel-gtags-update-tags nil t))
  (add-hook 'c-mode-hook #'fmdkdd/add-update-gtags-hook)
  (add-hook 'java-mode-hook #'fmdkdd/add-update-gtags-hook))

;; Racer is better for Rust
(use-package racer
  :ensure t
  :after rust-mode
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)

  (define-key rust-mode-map (kbd "C-c l") #'racer-describe))

;; xref works fine for elisp
(define-key emacs-lisp-mode-map (kbd "M-.") #'xref-find-definitions)
(define-key emacs-lisp-mode-map (kbd "M-,") #'xref-pop-marker-stack)

;; Speaking of elisp, this is nice to have in Flycheck
(defun fmdkdd/add-flycheck-checkers-in-imenu ()
  (let ((basename (file-name-nondirectory buffer-file-name)))
    (cond
     ((string= basename "flycheck.el")
      (setq imenu-generic-expression
            (cons '("Checkers" "^\\((flycheck-define-checker \\(.*\\)\\)" 2)
                  imenu-generic-expression)))
     ((string= basename "flycheck-test.el")
      (setq imenu-generic-expression
            (cons '("Tests" "^\\((flycheck-ert-def-checker-test \\(.*\\) \\(.*\\) \\(.*\\)\\)" 4)
                  imenu-generic-expression))))))
(add-hook 'find-file-hook #'fmdkdd/add-flycheck-checkers-in-imenu)

;; Why is this not built-in?
(define-key emacs-lisp-mode-map (kbd "C-c l") #'describe-thing-at-point)

;; In C, man is the better lookup (woman doesn't handle boxes, and uselessly
;; prompts for tar.gz files)
(use-package cc-mode
  :defer t
  :config
  (define-key c-mode-map (kbd "C-c l") #'man-at-point))

;; This is safe to put as local variable
(add-to-list 'safe-local-eval-forms '(fmdkdd/byte-compile-on-save))

;; Faster to type, and overrides my binding.
(use-package nxml-mode
  :defer t
  :config
  (define-key nxml-mode-map (kbd "C-c /") #'counsel-rg)
  (define-key nxml-mode-map (kbd "C-c C-/") #'nxml-finish-element))

(use-package sgml-mode
  :defer t
  :config
  (define-key sgml-mode-map (kbd "C-c /") #'counsel-rg)
  (define-key sgml-mode-map (kbd "C-c C-/") #'sgml-close-tag))


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Bindings
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Hydras help for common shortcuts
(use-package hydra
  :bind (("C-c e n" . fmdkdd/hydra-flycheck/flycheck-next-error)
         ("C-c e p" . fmdkdd/hydra-flycheck/flycheck-previous-error))
  :config
  ;; Faster browsing of flycheck errors
  (defhydra fmdkdd/hydra-flycheck ()
    "flycheck-errors"
    ("n" flycheck-next-error "next")
    ("p" flycheck-previous-error "previous")))

;; Minimize is more confusing than helpful with i3
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; This is more useful
(use-package ace-window
  :ensure t
  :bind ("C-z" . ace-window)
  :config
  (setq aw-keys '(?n ?e ?i ?o ?a ?r ?s ?t)))

;; Byebye dabbrev
(global-set-key [remap dabbrev-expand] #'hippie-expand)

;; Why are these not bound anywhere?
(global-set-key (kbd "M-j") #'join-line)
(global-set-key (kbd "C-c a") #'align-regexp)
(global-set-key (kbd "C-c f o") #'ff-find-other-file)
(global-set-key (kbd "C-c b u") #'browse-url-at-point)

;; Better bindings than the default
(global-set-key (kbd "M-0") #'delete-window)
(global-set-key (kbd "M-1") #'delete-other-windows)
(global-set-key (kbd "M-2") #'split-window-vertically)
(global-set-key (kbd "M-3") #'split-window-horizontally)
(global-set-key (kbd "C-c r") #'query-replace)
(global-set-key (kbd "C-c R") #'query-replace-regexp)
(global-set-key (kbd "C-c s") #'isearch-forward-symbol-at-point)

;; Additional goodies
(global-set-key (kbd "C-w") #'kill-region-or-backward-word)
(global-set-key (kbd "C-a") #'move-beginning-of-line-dwim)
(global-set-key (kbd "C-c f r") #'rename-file-and-buffer)
(global-set-key (kbd "C-;") #'goto-last-change)
(global-set-key (kbd "C-c o r") #'fmdkdd/save-and-reload-browser-windows)


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Misc
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Monday is the first day of the week
(with-eval-after-load 'calendar
  (setq calendar-week-start-day 1))

;; Report time to load this file
(let ((elapsed (float-time (time-subtract (current-time)
					  emacs-start-time))))
  (message "Loading %s...done (%.3fs)" load-file-name elapsed))

;; Local Variables:
;; eval: (fmdkdd/byte-compile-on-save)
;; End:
