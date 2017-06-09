;; -*- mode: emacs-lisp -*-

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default

   dotspacemacs-configuration-layers
   '(
     spell-checking
     (auto-completion :disabled-for org
                      :variables
                      auto-completion-enable-sort-by-usage t)
     writeroom
     org

     (syntax-checking :variables
                      syntax-checking-enable-tooltips nil)

     git
     (version-control :variables
                      version-control-diff-tool 'git-gutter)

     emacs-lisp
     javascript
     react
     html
     yaml
     ;; scala
     ;; haskell
     rust

     ;; edebug

     colemak-hjkl
     fmdkdd
     )

   dotspacemacs-additional-packages
   '(
     fish-mode                          ; Fish shell scripts
     markdown-mode                      ; markdown highlighting without the cruft
     glsl-mode                          ; GL shader language mode
     )

   dotspacemacs-excluded-packages
   '(
     ;; These come with spacemacs-base
     evil-escape          ; I bind ESC to Caps Lock key, so this is more
                          ; annoying than useful.
     centered-buffer-mode

     ;; From spacemacs-ui-visual
     golden-ratio         ; what is this even
     hl-todo              ; The fmdkdd layer does that already.
     popwin               ; less helpful than display-buffer-alist

     ;; From spacemacs-evil
     vi-tilde-fringe      ; I prefer the Emacs way of indicating EOF.

     ;; From spacemacs-org
     org-bullets

     ;; These come with html
     haml-mode
     jade-mode
     slim-mode

     ;; This comes with git
     evil-magit           ; I'm okay with evilified bindings

     smartparens                        ; Too slow on flycheck.el
     )))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."

  ;; Must add this here otherwise Spacemacs will try to load it from MELPA
  ;; (hint: it's not on MELPA).
  (add-to-list 'custom-theme-load-path "~/.emacs.d/private/zenrub-theme")

  (setq-default

   ;; Vim bindings
   dotspacemacs-editing-style 'vim

   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-command-key "SPC"

   ;; Zenrub theme!
   dotspacemacs-themes '(zenrub)

   ;; Crisply bitmap font
   dotspacemacs-default-font '("Dina"
                               :size 10
                               :powerline-scale 1.1)
   ;; Doesn't play nice with Dina
   dotspacemacs-mode-line-unicode-symbols nil

   ;; What to put on the startup screen
   dotspacemacs-startup-lists '((recents) (projects) (todos))

   ;; More recent files
   dotspacemacs-startup-recent-list-size 10

   ;; I can install layers myself, thank you
   dotspacemacs-ask-for-lazy-installation nil
   dotspacemacs-enable-lazy-installation nil

   ;; Don't highlight delimiters in the current scope
   dotspacemacs-highlight-delimiters 'any

   ;; Smooth scrolling is slooooow
   dotspacemacs-smooth-scrolling nil

   ;; Delete trailing whitespace on file save
   dotspacemacs-whitespace-cleanup 'trailing

   ;; They way it's meant to be
   dotspacemacs-scratch-mode 'emacs-lisp-mode

   ;; Y is y$
   dotspacemacs-remap-Y-to-y$ t

   ;; How do you cycle otherwise?
   dotspacemacs-enable-paste-transient-state t
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init'.  You are free to put any
user code.")

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration. You are free to put any user code."

  ;; Override these locally since I'm working on them.  This is the only
  ;; reliable way to override these packages locally.  Using :location local in
  ;; additional-packages doesn't work.
  (load-file "~/proj/flycheck/flycheck.el")
  (load-file "~/proj/flycheck-rust/flycheck-rust.el")
  (load-file "~/proj/flycheck/flycheck-inline.el")
  (with-eval-after-load 'flycheck
    (flycheck-inline-mode))

  ;; (add-to-load-path "~/proj/vibhavp-lsp")
  ;; (require 'lsp-mode)
  ;; (setenv "RLS_ROOT" "/home/fmdkdd/proj/rustls")
  ;; (setenv "LD_LIBRARY_PATH" "/home/fmdkdd/.rustup/toolchains/nightly-x86_64-unknown-linux-gnu/lib")
  ;; (global-lsp-mode t)

  ;; (with-eval-after-load 'lsp-mode
  ;;   (require 'lsp-flycheck)
  ;;   (lsp-flycheck-setup))

  (setq helm-ag-base-command "rg --color=always --no-heading --line-number --hidden --smart-case %s %s %s")

  ;; Auto-resize the flycheck error list
  (setq flycheck-error-list-after-refresh-hook
        ;; Using display-buffer-alist is not sufficient, as the list is not
        ;; populated at once.
        (lambda ()
          ;; The hooks don't run inside the error-list window, so we have to
          ;; select it first.
          (with-selected-window (flycheck-get-error-list-window)
            (shrink-window-if-larger-than-buffer))))
  )
