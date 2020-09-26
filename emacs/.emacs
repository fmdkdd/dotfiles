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

(set-face-attribute 'default nil :family "Deja Vu Sans Mono" :height 110 :weight 'normal)

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
(column-number-mode)
(size-indication-mode)

(setq history-length 1000)

;; Blinking cursor sometimes disappear
(blink-cursor-mode 0)

;; Save/restore window configurations
(winner-mode)

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
  (which-key-mode))

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :bind (("C-c C-r" . ivy-resume)
         ("C-x b"   . fmdkdd/goto-anything)
         ("C-x C-b" . fmdkdd/goto-anything))
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
  (ivy-mode)

  ;; Enhanced apropos
  (ivy-set-display-transformer
   'fmdkdd/apropos
   'fmdkdd/apropos-display-transformer)

  (ivy-set-actions
   'fmdkdd/apropos
   '(("d" xref-find-definitions "jump to definition"))))

(use-package ivy-hydra
  :ensure t)

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-h a" . fmdkdd/apropos)
         ("C-c i" . counsel-imenu)
         ("C-c C-i" . counsel-imenu)
         ("C-c /" . counsel-rg)
         ("C-c f l" . counsel-locate))

  :config
  ;; Show docstring for M-x commands
  (ivy-set-display-transformer
   'counsel-M-x
   'fmdkdd/M-x-display-transformer))

(use-package smex
  :ensure t)

(use-package swiper
  :ensure t
  :bind (("M-s M-s" . swiper)))

;; Display manual in side window
(setq fit-window-to-buffer-horizontally t)
(add-to-list 'display-buffer-alist
             `(,(rx bos "*info*" eos) display-buffer-in-side-window
               (side . right) (slot . 0)
               (window-width . fit-window-to-buffer)
               (preserve-size . (t . nil))))


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Files
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Backups in .emacs.d
(setq backup-directory-alist `(("." . ,(locate-user-emacs-file "backups"))))

;; Auto saves as well
(setq auto-save-file-name-transforms
      `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
         ,(concat temporary-file-directory "\\2") t)
        (".*" ,(concat (locate-user-emacs-file "auto-saves/") "/")  t)))

;; And disable file locks (they create noise in the current directory, which
;; confuses file watches)
(setq create-lockfiles nil)

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
  (recentf-mode))

;; Save cursor location in files
(if (< emacs-major-version 25)
    (progn (require 'saveplace)
           (setq-default save-place t))
  (save-place-mode))

(setq delete-by-moving-to-trash t)

(use-package inotify-revert
  :load-path "~/.emacs.d/lisp")


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Editing
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Invaluable
(show-paren-mode)

;; Spaces over tabs
(setq-default indent-tabs-mode nil)
(setq require-final-newline t)
(delete-selection-mode)

;; Auto fill
(setq-default fill-column 80)
(add-hook 'text-mode-hook #'turn-on-auto-fill)
(diminish 'auto-fill-function)

;; One sentence per line in LaTeX
(use-package ospl
  :load-path "~/.emacs.d/lisp"
  :hook (latex-mode . turn-on-ospl))

;; Auto fill comments in prog modes
(add-hook 'prog-mode-hook #'auto-fill-comments)

;; Delete trailing whitespace on file save
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; It breaks hexl-mode though
(use-package hexl
  :init
  (remove-hook 'before-save-hook #'delete-trailing-whitespace 'local))

;; This is useful just for thinking about updating copyright years.
(add-hook 'before-save-hook #'copyright-update)

;; Flyspell eats useful bindings
(use-package flyspell
  :defer t
  :config
  (define-key flyspell-mode-map (kbd "C-;") nil))

;; Hippie expand everything
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
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
  :load-path "~/.emacs.d/elisp/"
  :bind (("C-," . delimiter-dwim)))

(use-package markdown-mode
  :ensure t
  :mode ("\\.md" . markdown-mode)
  :config
  (define-key markdown-mode-map (kbd "M-p") #'markdown-outline-previous)
  (define-key markdown-mode-map (kbd "M-n") #'markdown-outline-next))


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
        org-list-description-max-indent 5
        org-src-fontify-natively t      ; more useful
        org-log-done             t)     ; log all the things

  (setq org-default-notes-file (expand-file-name "tasks.org" org-directory))

  ;; Activate gnuplot in Babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((gnuplot . t)))

  ;; "reset" is mandatory for all gnuplot src blocks
  (use-package ob-gnuplot
    :defer t
    :config
    (add-to-list 'org-babel-default-header-args:gnuplot
                 '(:prologue . "reset"))))

(use-package org-agenda
  :bind (("C-c o a" . org-agenda))
  :config
  ;; Custom agenda command
  (setq org-agenda-window-setup 'current-window
        org-agenda-start-on-weekday nil ; start on current day
        org-agenda-span 10              ; show more days
        org-deadline-warning-days 14    ; show deadline in advance
        org-agenda-custom-commands
        '(("n" "Agenda and all unscheduled TODO's"
           ((agenda "")
            (todo "WAIT" ((org-agenda-overriding-header "Waiting")))
            (todo "" ((org-agenda-overriding-header "Upcoming deadlines")
                      (org-agenda-skip-function
                       ;; Show not done deadlines between
                       ;; org-deadline-warning-days and 90 days
                       '(lambda ()
                          (org-back-to-heading t)
                          (let ((beg (point))
	                        (end (org-entry-end-position))
	                        (planning-end (line-end-position 2)))
                            (and (not (save-excursion
		                        (when (re-search-forward org-deadline-time-regexp planning-end t)
                                          (let ((deadline (org-time-stamp-to-now (match-string 1))))
                                            (or (org-entry-is-done-p)
                                                (and (>= deadline org-deadline-warning-days)
                                                     (<= deadline 90)))))))
                                 end))))))
            (todo "TODO" ((org-agenda-overriding-header "Unscheduled tasks")
                          (org-agenda-todo-ignore-scheduled 'all)
                          (org-agenda-todo-ignore-deadlines 'all)))))
          ("Rw" "Week in review"
           agenda "" ((org-agenda-overriding-header "Week in review")
                      (org-agenda-span 'week)
                      (org-agenda-start-on-weekday 1)
                      (org-agenda-start-with-log-mode t))))))

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
(setq-default coffee-tab-width 2)

(c-add-style "user-java"
             '("java"
               (c-basic-offset . 2)))
(add-hook 'java-mode-hook (lambda () (c-set-style "user-java")))
(add-hook 'java-mode-hook (lambda () (subword-mode)))

;; Font lock is slowing done editing in c mode, turn it down a notch.
;; Minium font-locking everywhere else unless I notice.
(setq font-lock-maximum-decoration
      '((c-mode . 2)
        (c++-mode . 2)
        (emacs-lisp-mode . 2)
        (t . nil)))

;; Project navigation
(use-package projectile
  :diminish projectile-mode
  :init
  (setq projectile-dynamic-mode-line nil)
  :config
  (projectile-mode))

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
      (global-flycheck-mode)))
  (add-hook 'before-save-hook #'fmdkdd/init-flycheck)
  :config
  (setq flycheck-display-errors-delay 0.125
        flycheck-check-syntax-automatically '(save)
        flycheck-emacs-lisp-load-path 'inherit
        flycheck-global-modes '(emacs-lisp-mode c-mode c++-mode rust-mode))

  ;; Display the error list in the bottom side window, with as much
  ;; height as needed by its contents.
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*Flycheck errors*" eos) display-buffer-in-side-window
                 (side . bottom) (slot . 0)
                 (window-height . fit-window-to-buffer)
                 (preserve-size . (nil . t)))))

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
  (add-hook 'flycheck-mode-hook #'flycheck-inline-mode)
  (setq flycheck-inline-display-error-id nil))

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
    (add-hook 'after-save-hook #'counsel-gtags-update-tags nil 'local))
  (add-hook 'c-mode-hook #'fmdkdd/add-update-gtags-hook)
  (add-hook 'java-mode-hook #'fmdkdd/add-update-gtags-hook))

;; rtags is better for C++
(use-package rtags
  :ensure t
  :bind (:map c++-mode-map
              ("M-." . #'rtags-find-symbol-at-point)
              ("M-," . #'rtags-location-stack-back)))

;; Racer is better for Rust
(use-package racer
  :ensure t
  :after rust-mode
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)

  (define-key rust-mode-map (kbd "C-c l") #'racer-describe))

;; xref works fine for elisp, asm
(use-package xref-asm
  :load-path "~/emacs.d/elisp"
  :after asm-mode
  :config
  (xref-asm-activate))

;; Preview xref definitions using posframe
(use-package posframe
  :ensure t
  :config
  (setq posframe-mouse-banish nil))

(use-package xref-posframe
  :load-path "~/emacs.d/elisp"
  :bind (:map emacs-lisp-mode-map
         ("M-." . xref-posframe-dwim)
         ("M-," . xref-posframe-pop)))

(use-package asm-mode
  :bind (:map asm-mode-map
         ("C-c C-c" . #'recompile)
         ("M-." . xref-posframe-dwim)
         ("M-," . xref-posframe-pop)))

;; Speaking of elisp, this is nice to have in Flycheck
(defun fmdkdd/add-flycheck-checkers-in-imenu ()
  "Enhance imenu with flycheck macros definitions."
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

;; This one is also... helpful
(use-package helpful
  :ensure t
  :config
  (defalias #'describe-key #'helpful-key)
  (defalias #'describe-function #'helpful-callable)
  (defalias #'describe-variable #'helpful-variable)
  (defalias #'describe-symbol #'helpful-symbol))

;; Why is this not built-in?
(define-key emacs-lisp-mode-map (kbd "C-c l") #'describe-thing-at-point)

;; In C, man is the better lookup (woman doesn't handle boxes, and uselessly
;; prompts for tar.gz files)
(defun fmdkdd/c-setup ()
  (c-set-offset 'arglist-intro '++)
  (c-set-offset 'brace-list-intro 'c-basic-offset))

(use-package cc-mode
  :defer t
  :bind (:map c-mode-map
              ("C-c l" . #'man-at-point)
              ("C-c C-c" . #'recompile)
              :map c-mode-base-map
              ("C-c l" . #'man-at-point)
              ("C-c C-c" . #'recompile))
  :mode ("\\.h\\'" . c++-mode)
  :config
  (define-key c-mode-base-map (kbd "M-j") nil)
  (add-hook 'c++-mode-hook #'fmdkdd/c-setup)
  (setq compilation-ask-about-save nil))

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

(use-package tex-mode
  :defer t
  :config
  (define-key tex-mode-map (kbd "C-c /") #'counsel-rg)
  (define-key tex-mode-map (kbd "C-c C-/") #'latex-close-block))



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
(global-set-key (kbd "C-z") #'other-window)

;; This is faster for switching with multiple windows
(use-package winum
  :ensure t
  :init
  (setq winum-keymap
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "C-0") #'winum-select-window-0-or-10)
          (define-key map (kbd "C-1") #'winum-select-window-1)
          (define-key map (kbd "C-2") #'winum-select-window-2)
          (define-key map (kbd "C-3") #'winum-select-window-3)
          (define-key map (kbd "C-4") #'winum-select-window-4)
          (define-key map (kbd "C-5") #'winum-select-window-5)
          (define-key map (kbd "C-6") #'winum-select-window-6)
          (define-key map (kbd "C-7") #'winum-select-window-7)
          (define-key map (kbd "C-8") #'winum-select-window-8)
          map))
  :config
  (winum-mode))

;; Byebye dabbrev
(global-set-key [remap dabbrev-expand] #'hippie-expand)

;; Why are these not bound anywhere?
(global-set-key (kbd "M-j") #'join-line)
(global-set-key (kbd "C-c a") #'align-regexp)
(global-set-key (kbd "C-c f o") #'ff-find-other-file)
(global-set-key (kbd "C-c b u") #'browse-url-at-point)

;; Better bindings than the default
(global-set-key (kbd "M-g") #'goto-line)
(global-set-key (kbd "M-0") #'delete-window)
(global-set-key (kbd "M-1") #'delete-other-windows)
(global-set-key (kbd "M-2") #'split-window-vertically)
(global-set-key (kbd "M-3") #'split-window-horizontally)
(global-set-key (kbd "C-c r") #'query-replace)
(global-set-key (kbd "C-c R") #'query-replace-regexp)
(global-set-key (kbd "C-c s") #'isearch-forward-symbol-at-point)
(global-set-key (kbd "M-p") #'backward-paragraph)
(global-set-key (kbd "M-n") #'forward-paragraph)

;; Additional goodies
(global-set-key (kbd "C-w") #'kill-region-or-backward-word)
(global-set-key (kbd "C-a") #'move-beginning-of-line-dwim)
(global-set-key (kbd "C-c f r") #'rename-file-and-buffer)

(use-package reload-browser
  :load-path "~/.emacs.d/elisp/"
  :bind ("C-c o r" . fmdkdd/save-and-reload-browser-windows))

(use-package goto-last-change
  :load-path "~/.emacs.d/elisp/"
  :bind ("C-;" . goto-last-change))


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Misc
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

; Monday is the first day of the week
(with-eval-after-load 'calendar
  (setq calendar-week-start-day 1))

;; Add digital information units to calc
(use-package calc-units
  :defer t
  :config
  (setq math-additional-units
        '((bit   nil          "Binary digit")
          (o     "8 * bit"    "Octet")
          (B     "o"          "Byte")
          (KiB   "1024 * B"   "Kibibyte")
          (MiB   "1024 * KiB" "Mebibyte")
          (GiB   "1024 * MiB" "Gibibyte")
          (TiB   "1024 * GiB" "Tebibyte"))
        ;; Calc wants us to set this to nil for it to be recomputed
        math-units-table nil))

;; Report time to load this file
(let ((elapsed (float-time (time-subtract (current-time)
					  emacs-start-time))))
  (message "Loading %s...done (%.3fs)" load-file-name elapsed))

;; Local Variables:
;; eval: (fmdkdd/byte-compile-on-save)
;; End:
