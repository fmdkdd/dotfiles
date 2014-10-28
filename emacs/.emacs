;; Install missing packages from MELPA

(defconst fmdkdd/packages
  '(
    ;; UI
    diminish                            ; Clean mode line
    smart-mode-line                     ; Color mode line
    smex                                ; Smart M-x

    ;; Navigation
    ido-ubiquitous                      ; Ido everywhere ...
    flx-ido                             ; with flex matching
    imenu-anywhere                      ; Imenu for all buffers

    ;; Color theme
    zenburn-theme

    ;; Editing
    yasnippet                           ; Code templates
    flycheck                            ; On the fly syntax checking
    tern                                ; JS static analysis

    markdown-mode

    ;; Programming
    highlight-symbol                    ; Navigate between occurences
                                        ; of a symbol
    rainbow-delimiters                  ; Colorize nested parens
    highlight-numbers                   ; Colorize numeric literals

    magit                               ; Git management

    ;; Haskell
    haskell-mode
    flycheck-haskell
    hi2
    )
  "Packages required by this configuration")

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

(defun fmdkdd/ensure-packages ()
  "Install packages"
  (interactive)
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (package fmdkdd/packages)
    (unless (package-installed-p package)
      (package-install package))))

(fmdkdd/ensure-packages)

(defmacro lunaryorn-after (feature &rest forms)
  "After FEATURE is loaded, evaluate FORMS.

FORMS is byte compiled.

FEATURE may be a named feature or a file name, see
`eval-after-load' for details."
  (declare (indent 1) (debug t))
  `(progn
     (eval-when-compile
       ;; Require the feature during compilation to avoid compiler
       ;; errors/warnings. Since `eval-when-compile' also evaluated during macro
       ;; expansion we check whether the current file is really being compiled
       (when (bound-and-true-p byte-compile-current-file)
         ,(if (stringp feature)
              `(load ,feature :no-message :no-error)
            `(require ',feature nil :no-error))))
     ;; Register FORMS to be eval'ed after FEATURE
     (eval-after-load ',feature '(progn ,@forms))))

;; Path to extensions
(defconst fmdkdd/elisp (locate-user-emacs-file "elisp")
  "Where to load extra lisp files.")

(add-to-list 'load-path fmdkdd/elisp)

;;;; UI

;; Customization in its own file
(defconst fmdkdd/custom-file (locate-user-emacs-file "custom.el")
  "File used to store settings of Customization UI")

(lunaryorn-after cus-edit
  (setq custom-file fmdkdd/custom-file))
(load fmdkdd/custom-file :no-error)

;; Useless bars
(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 0)

;; Don't display the 'Welcome to GNU Emacs' buffer on startup.
(setq inhibit-startup-message t)

;; Skip the GNU message in minibuffer
(fset 'display-startup-echo-area-message 'ignore)

;; Replace 'yes or no' prompts by 'y or n'.
(fset 'yes-or-no-p 'y-or-n-p)

;; Font and color theme
(set-face-attribute 'default nil :family "Ubuntu Mono" :height 120)

(load-theme 'zenburn 'no-confirm)

(zenburn-with-color-variables
  (custom-theme-set-faces
   'zenburn
   `(font-lock-comment-face ((t (:foreground ,zenburn-green+2))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,zenburn-green-1))))
   `(font-lock-string-face ((t (:foreground ,zenburn-yellow-2))))))

;;; Mode line

(column-number-mode t)
(size-indication-mode t)

;; Clean mode line, with colors
(lunaryorn-after smart-mode-line
  (setq sml/no-confirm-load-theme t
        sml/theme 'respectful
        sml/name-width 25))
(sml/setup)


;;; Minibuffer

(setq history-length 1000)

;; Ido
(lunaryorn-after ido
  (setq ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess
        ido-default-file-method 'selected-window
        ido-use-faces nil))
(ido-mode t)
(ido-everywhere)
(ido-ubiquitous-mode)
(flx-ido-mode)

;; Smex
(lunaryorn-after smex
  (setq smex-save-file (locate-user-emacs-file ".smex-items")))


;;;; Buffer, Windows and Frames

;; Improve the uniquification of buffer names.
;; Makefile and Makefile<2> become Makefile|project Makefile|test
(require 'uniquify)
(setq-default uniquify-buffer-name-style 'forward)

;; Save window configurations
(winner-mode)

;; Save buffers, windows and frames on exit
(desktop-save-mode)

;; Kill stale buffers
(require 'midnight)

;;;; File handling

;; Backup files in their own directory
(setq backup-directory-alist `(("." . ,(locate-user-emacs-file "backups"))))

(setq delete-by-moving-to-trash t)

(lunaryorn-after dired
  (setq dired-auto-revert-buffer t
        dired-listing-switches "-alh"   ; Human-readable size
        ))

;; Track recent files
(lunaryorn-after recentf
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 15
        recentf-auto-cleanup 300))
(recentf-mode t)

(lunaryorn-after recentf
  (defun ido-find-recentf ()
    "Find a recent file with IDO."
    (interactive)
    (let ((file (ido-completing-read "Find recent file: " recentf-list)))
      (when file
        (find-file file)))))

;; Save position in files
(require 'saveplace)
(setq-default save-place t)

;; Read-only files are in view mode
(setq view-read-only t)

;; Follow symlinks under version control
(lunaryorn-after vc-hooks
  (setq vc-follow-symlinks t))

;;;; Editing

;; Prefer utf-8
(set-language-environment "utf-8")
(prefer-coding-system 'utf-8)

;; Spaces over tabs
(setq-default indent-tabs-mode nil)

;; Delete trailing whitespace on file save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Tab complete if the line is already indented
(setq tab-always-indent 'complete)

(electric-pair-mode)
(electric-layout-mode)

;; Indicate empty lines in the fringe
(setq-default indicate-empty-lines t)

(setq require-final-newline t)

;; Typing with an active selection replaces the selection
(delete-selection-mode)

;; Limit text modes to 70 columns
(setq-default fill-column 70)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Auto fill comments in prog modes
(defun auto-fill-comments ()
  (setq-local comment-auto-fill-only-comments t)
  (turn-on-auto-fill))

(add-hook 'prog-mode-hook 'auto-fill-comments)

(diminish 'auto-fill-function)

;; Enable narrowing
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; Faster mark popping
(setq set-mark-command-repeat-pop t)

;; Highligh current line
(global-hl-line-mode t)

;; Yasnippet everywhere
(yas-global-mode)

;; Syntax checking on the fly
(lunaryorn-after flycheck
  (setq flycheck-completion-system 'ido
        flycheck-highlighting-mode 'sexps))
(global-flycheck-mode)

(setq show-paren-delay 0.0)
(show-paren-mode t)

;; Completion
(lunaryorn-after hippie-exp
  (setq hippie-expand-try-functions-list
        '(try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-file-name-partially
          try-complete-file-name
          try-expand-all-abbrevs
          try-expand-list
          try-complete-lisp-symbol-partially
          try-complete-lisp-symbol)))

;; Spelling
(lunaryorn-after ispell
  (setq ispell-silently-savep t ; Don't ask when saving private
                               ; dictionary
        ))

(lunaryorn-after flyspell
  ;; Free M-Tab and C-M-i
  (define-key flyspell-mode-map "\M-\t" nil)
  (setq flyspell-use-meta-tab nil))

(add-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)


;; Highlight and navigate symbols
(lunaryorn-after highlight-symbol
  (setq highlight-symbol-idle-delay 0.4
        highlight-symbol-on-navigation-p t)
  (diminish 'highlight-symbol-mode))

(add-hook 'prog-mode-hook #'highlight-symbol-nav-mode)

(defvar fmdkdd/symbols-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "o") #'highlight-symbol-occur)
    (define-key map (kbd "%") #'highlight-symbol-query-replace)
    (define-key map (kbd "n") #'highlight-symbol-next-in-defun)
    (define-key map (kbd "p") #'highlight-symbol-prev-in-defun)
    map)
  "Keymap for symbol operations.")

;; Curly quotes
(require 'smart-quotes)
(add-hook 'text-mode-hook 'turn-on-smart-quotes)
(diminish 'smart-quotes-mode)

;; But not in HTML
(add-hook 'html-mode-hook 'turn-off-smart-quotes)

;;; Org mode
(lunaryorn-after org
  ;; No indentation in Org files
  (setq org-adapt-indentation nil)

  ;; Syntactic coloration of source blocks
  (setq org-src-fontify-natively t)

  ;; Nice LaTeX entities
  (setq-default org-pretty-entities t)

  ;; Fallback to DejaVu Sans Mono for mathematical symbols not present
  ;; in the default font (Ubuntu Mono)
  (set-fontset-font "fontset-default" '(#x2200 . #x22ff) (font-spec :family "DejaVu Sans Mono"))
  ;; Arrows are too narrow in a mono font
  (set-fontset-font "fontset-default" '(#x2190 . #x21ff) (font-spec :family "DejaVu Sans"))

  (setq face-font-rescale-alist
        '((".*DejaVu Sans Mono.*" . 0.9)
          (".*DejaVu Sans.*" . 0.9)))

  ;; Open links to Mozilla Archive Format Files in Firefox
  (add-to-list 'org-file-apps
               '("maff" . "firefox %s"))

  (setq org-log-into-drawer t)
  (setq org-clock-into-drawer t)

  (setq org-todo-keyword-faces
        (zenburn-with-color-variables
          `(("TODO" . org-warning)
            ("NEXT" . (:foreground ,zenburn-yellow :weight bold))
            ("WAIT" . (:foreground ,zenburn-orange :weight bold))
            ("CANCELED" . (:foreground ,zenburn-blue-1 :weight bold))
            ("DELEGATED" . (:foreground ,zenburn-green :weight bold)))))

  (setq org-agenda-custom-commands
        '(("n" "Agenda and all unscheduled TODO's"
           ((agenda "")
            (todo "NEXT" ((org-agenda-overriding-header "Next")))
            (todo "WAIT" ((org-agenda-overriding-header "Waiting")))
            (todo "TODO" ((org-agenda-overriding-header "Unscheduled tasks")
                          (org-agenda-todo-ignore-scheduled 'all)
                          (org-agenda-todo-ignore-deadlines 'all)))
            (todo "" ((org-agenda-overriding-header "Upcoming deadlines")
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'notdeadline))
                      (org-agenda-todo-ignore-deadlines 'near))))))
        org-agenda-ndays 1))

;;;; Programming

;; Nested parens
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Font lock for numeric literals
(add-hook 'prog-mode-hook #'highlight-numbers-mode)

;; Highlight the following words in comments
(defun add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\|DELETE\\|XXX\\):"
          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook #'add-watchwords)

;;; Haskell

;; Disable squiggles in hi2
(lunaryorn-after hi2
  (setq hi2-show-indentations nil))

(dolist (fun '(interactive-haskell-mode
               subword-mode
               turn-on-haskell-decl-scan
               haskell-doc-mode
               hi2-mode))
  (add-hook 'haskell-mode-hook fun))

(lunaryorn-after haskell-interactive-mode
  (dolist (fun '(subword-mode
                 haskell-doc-mode
                 rainbow-delimiters-mode))
    (add-hook 'haskell-interactive-mode-hook fun)))

(lunaryorn-after haskell-process
  (setq haskell-process-type 'ghci
        haskell-interactive-popup-errors nil))

(lunaryorn-after flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

;;; Java

(c-add-style "user-java"
             '("java"
               (c-basic-offset . 2)))

(defun fmdkdd/customize-java-mode ()
  (c-set-style "user-java"))

(add-hook 'java-mode-hook #'fmdkdd/customize-java-mode)

;;; JavaScript

(setq js-indent-level 2)

;; Tern
;; Requires `npm install --global tern`

(defun turn-on-tern ()
  (tern-mode t))
(add-hook 'js-mode-hook #'turn-on-tern)

;;; CSS
(setq css-indent-offset 2)
(add-hook 'css-mode-hook #'rainbow-mode)

;;; Miscellaneous

;; View image files as images
(auto-image-file-mode)

;; Calendar
(lunaryorn-after calendar
  (setq calendar-week-start-day 1))


;;; Mouse

;; Text pasted from the mouse middle click is inserted at point rather
;; than at the mouse cursor position.
(setq-default mouse-yank-at-point t)

;;; Key bindings

;; Disable iconify shortcuts .. useless in Xmonad
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; Bye bye Delete key!
(global-unset-key (kbd "<M-DEL>"))

;; Ergonomics
(global-set-key (kbd "C-z") 'other-window)
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-vertically)
(global-set-key (kbd "M-3") 'split-window-horizontally)

;; Best of both worlds
(defun kill-region-or-backward-word ()
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (if (and (boundp 'subword-mode) subword-mode)
        (subword-backward-kill 1)
        (backward-kill-word 1))))

(global-set-key (kbd "C-w") 'kill-region-or-backward-word)

(global-set-key [remap execute-extended-command] #'smex)
(global-set-key [remap list-buffers] #'ibuffer)
(global-set-key [remap dabbrev-expand] #'hippie-expand)

(global-set-key (kbd "M-X") #'smex-major-mode-commands)
(global-set-key (kbd "C-x f") #'ido-find-recentf)

(find-function-setup-keys)

(global-set-key (kbd "C-c i") #'imenu-anywhere)
(global-set-key (kbd "C-c s") fmdkdd/symbols-map)
(global-set-key (kbd "C-c g") #'magit-status)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c l") #'org-store-link)



;;; .emacs ends here
