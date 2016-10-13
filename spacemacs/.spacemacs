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
     ;; markdown
     org

     (syntax-checking :variables
                      syntax-checking-enable-tooltips nil)

     git

     emacs-lisp
     javascript
     html
     ;; scala
     ;; haskell
     rust

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
     golden-ratio
     hl-todo              ; The fmdkdd layer does that already.

     ;; From spacemacs-evil
     vi-tilde-fringe      ; I prefer the Emacs way of indicating EOF.

     ;; From spacemacs-editing-visual
     highlight-numbers

     ;; From spacemacs-org
     org-bullets

     ;; These come with html
     haml-mode
     jade-mode
     slim-mode

     ;; This comes with git
     evil-magit           ; I'm okay with evilified bindings
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
   dotspacemacs-command-key ":"

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
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init'.  You are free to put any
user code.")

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration. You are free to put any user code."

  ;; Override these packages locally
  (add-to-list 'load-path "~/proj/flycheck")
  (add-to-list 'load-path "~/proj/flycheck-rust")

  (setq helm-ag-base-command "rg --line-number --color never --hidden --no-heading")

  ;; XXX: these should not be necessary with the Haskell layer in 0.105
  ;; (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
  ;; (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  ;; (setq haskell-interactive-popup-errors nil)
  )
