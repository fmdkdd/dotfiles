;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Packages
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; To measure the time taken to load the whole file.  Care has been taken (using
;; use-package) to keep this under 500ms.
(defconst emacs-start-time (current-time))

(package-initialize)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))

(use-package utils
  :load-path "~/.emacs.d/elisp/")

(eval-when-compile
  ;; Required for byte-compiled init file to load
  (use-package bind-key))

;; Always load fresh .el files over byte-compiled ones
(setq load-prefer-newer t)


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

;; Focus follow mouse
;; XXX: magit-status and magit-commit move the mouse cursor, so they may end up
;; losing focus randomly with this option.  Not worth it.
;(setq mouse-autoselect-window nil)

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

;; Helm for many things
;; see https://tuhdo.github.io/helm-intro.html for an overview on configuration
(use-package helm-config
  :demand t
  :bind (("C-h a"   . helm-apropos)
         ("M-x"     . helm-M-x)
         ("C-c y"   . helm-show-kill-ring)
         ("C-x C-f" . helm-find-files)
         ("C-x b"   . helm-mini)
         ("C-c f l" . helm-locate)
         ("C-c i"   . helm-imenu)
         ("C-c I"   . helm-imenu-in-all-buffers)
         ("C-c /"   . helm-do-ag-project-root))
  :config
  (use-package helm-mode
    :diminish helm-mode
    :defer 2  ; quite slow to start and non-essential
    :config
    (helm-mode 1))

  (setq helm-split-window-in-side-p    t   ; open helm in current window
        helm-echo-input-in-header-line t   ; eyes don't have to travel that way
        helm-buffers-fuzzy-matching    t   ; fuzzy matching is btr
        helm-recentf-fuzzy-match       t
        helm-imenu-fuzzy-match         t
        helm-apropos-fuzzy-match       t
        helm-M-x-fuzzy-match           t)

  ;; Ripgrep
  (setq helm-ag-base-command "rg --color=never --no-heading --line-number --smart-case")

  ;; Remember what I use in helm, as the default sorting by length is useless
  ;; FIXME: this actually doesn't run in any commands I care about
  (helm-adaptive-mode 1))


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

;; Anzu makes isearch and query-replace more intuitive
(use-package anzu
  :defer 3
  :diminish anzu-mode
  :config
  (global-anzu-mode +1)
  (global-set-key [remap query-replace] 'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp))

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
  :mode ("\\.md" . markdown-mode))


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Org mode
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(use-package org
  :defer t
  :bind (("C-c o a" . org-agenda)
         ("C-c o c" . org-capture)
         ("C-c o l" . org-store-link))
  :config
  ;; Org mode fucks up all my bindings
  (define-key org-mode-map (kbd "C-,") nil)
  (define-key org-mode-map (kbd "C-a") nil)

  ;; And at the time, defines stupidly long bindings for useful commands
  (define-key org-mode-map (kbd "M-p") #'outline-previous-heading)
  (define-key org-mode-map (kbd "M-n") #'outline-next-heading)

  ;; C-c & is too inefficient
  (define-key org-mode-map (kbd "C-c C-b") #'org-mark-ring-goto)

  (setq org-adapt-indentation    nil    ; indentation is lost space
        org-edit-src-content-indentation 0
        org-src-fontify-natively t      ; more useful
        org-log-done             t)     ; log all the things

  (setq org-default-notes-file "tasks.org"
        org-capture-templates
        '(("t" "Task" entry (file+headline "" "Tasks")
           "* TODO %?")
          ("r" "Rendez-vous" entry (file+headline "" "Rendez-vous")
           "* %?")))

  ;; Custom agenda command
  (setq org-agenda-window-setup 'current-window
        org-agenda-custom-commands
        '(("n" "Agenda and all unscheduled TODO's"
           ((agenda "")
            (todo "NEXT" ((org-agenda-overriding-header "Next")))
            (todo "WAIT" ((org-agenda-overriding-header "Waiting")))
            (todo "TODO" ((org-agenda-overriding-header "Unscheduled tasks")
                          (org-agenda-todo-ignore-scheduled 'all)
                          (org-agenda-todo-ignore-deadlines 'all)))
            (todo "" ((org-agenda-overriding-header "Upcoming deadlines")
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'notdeadline))
                      (org-agenda-todo-ignore-deadlines 'near)))))))

  ;; Activate gnuplot in Babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((gnuplot . t))))


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Programming
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Indentation
(setq-default rust-indent-offset 2)
(setq-default js-indent-level 2)
(setq-default css-indent-offset 2)

(c-add-style "user-java"
             '("java"
               (c-basic-offset . 4)))
(add-hook 'java-mode-hook (lambda () (c-set-style "user-java")))
(add-hook 'java-mode-hook (lambda () (subword-mode)))

;; Syntax checking
(use-package flycheck
  :load-path "~/proj/flycheck"
  :defer 2 ; we want global-flycheck-mode, but not essential on startup
  :config
  (setq flycheck-display-errors-delay 0.125
        flycheck-check-syntax-automatically '(save)
        flycheck-global-modes '(emacs-lisp-mode c-mode c++-mode rust-mode))

  (global-flycheck-mode))

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
  :bind ("C-x g" . magit-status)
  :config
  ;; magit turns this on for files under git (which makes sense)
  (diminish 'auto-revert-mode)

  ;; I don't use these, but they shadow useful global bindings
  (define-key magit-status-mode-map (kbd "M-1") nil)
  (define-key magit-status-mode-map (kbd "M-2") nil)
  (define-key magit-status-mode-map (kbd "M-3") nil))

;; Highlight TODO, FIXME keywords
(add-hook 'prog-mode-hook #'add-watchwords)

;; GTAGS are very useful for C-mode, at least
(use-package helm-gtags
  :bind (("M-." . helm-gtags-dwim)
         ("M-," . helm-gtags-pop-stack))
  :init
  (defun fmdkdd/add-update-gtags-hook ()
    (add-hook 'after-save-hook #'helm-gtags-update-tags nil t))
  (add-hook 'c-mode-hook #'fmdkdd/add-update-gtags-hook)
  (add-hook 'java-mode-hook #'fmdkdd/add-update-gtags-hook))

;; Racer is better for Rust
(use-package racer
  :after rust-mode
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)

  (define-key rust-mode-map (kbd "C-c l") #'racer-describe))

;; xref works fine for elisp
(define-key emacs-lisp-mode-map (kbd "M-.") #'xref-find-definitions)
(define-key emacs-lisp-mode-map (kbd "M-,") #'xref-pop-marker-stack)

;; Why is this not built-in?
(define-key emacs-lisp-mode-map (kbd "C-c l") #'describe-thing-at-point)

;; In C, man is the better lookup (woman doesn't handle boxes, and uselessly
;; prompts for tar.gz files)
(with-eval-after-load 'cc-mode
  (define-key c-mode-map (kbd "C-c l") #'man-at-point))

;; Auto byte-compile on save
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (add-hook 'after-save-hook #'emacs-lisp-byte-compile nil t)))

;; Faster to type, and overrides my binding.
(with-eval-after-load 'nxml-mode
  (define-key nxml-mode-map (kbd "C-c /") #'helm-do-ag-project-root)
  (define-key nxml-mode-map (kbd "C-c C-/") #'nxml-finish-element))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Bindings
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; XXX: god-mode has the same downsides as evil: modal navigation is confusing.
;; In particular, hitting keys can have unexpected effects (especially as I
;; still have Spacemacs muscle memory).
;; (use-package god-mode
;;   :bind ("<escape>" . god-mode-all)
;;   :preface
;;   (defun fmdkdd/update-cursor ()
;;     (set-face-background 'cursor (if god-local-mode
;;                                      "#fff"
;;                                    "#9ed08c")))
;;   :config
;;   (define-key god-local-mode-map (kbd ".") 'repeat)
;;   (define-key god-local-mode-map (kbd "P") 'scroll-down-command)
;;   (define-key god-local-mode-map (kbd "N") 'scroll-up-command)
;;   (define-key god-local-mode-map (kbd "<") 'beginning-of-buffer)
;;   (define-key god-local-mode-map (kbd ">") 'end-of-buffer)

;;   (add-hook 'god-mode-enabled-hook #'fmdkdd/update-cursor)
;;   (add-hook 'god-mode-disabled-hook #'fmdkdd/update-cursor))

;; Hydras help for common shortcuts
(use-package hydra
  :config

  ;; Faster browsing of flycheck errors
  (defhydra fmdkdd/hydra-flycheck ()
    "flycheck-errors"
    ("n" flycheck-next-error "next")
    ("p" flycheck-previous-error "previous"))

  (global-set-key (kbd "C-c e n") 'fmdkdd/hydra-flycheck/flycheck-next-error)
  (global-set-key (kbd "C-c e p") 'fmdkdd/hydra-flycheck/flycheck-previous-error)
  ;; TODO: could remap the original flycheck bindings as well

  ;; Navigation mode
  ;; XXX: not sure about that, but at least it doesn't have keys with surprising
  ;; effects.
  ;; One downside of using hydra for navigation is that it seems to redraw the
  ;; screen on each key, making the screen happily blink.
  (defhydra fmdkdd/hydra-navigation ()
    "navigation"
    ("n" next-line "next-line")
    ("p" previous-line "previous-line")
    ("=" balance-windows "balance-windows"))

  (global-set-key (kbd "<escape>") 'fmdkdd/hydra-navigation/body))

;; Minimize is more confusing than helpful with i3
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; This is more useful
(global-set-key (kbd "C-z") 'other-window)

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
