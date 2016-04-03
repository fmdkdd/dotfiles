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

     syntax-checking

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

   dotspacemacs-additional-packages '()

   dotspacemacs-excluded-packages
   '(
     ;; These come with spacemacs-base
     evil-escape          ; I bind ESC to Caps Lock key, so this is more
                                        ; annoying than useful.
     vi-tilde-fringe      ; I prefer the Emacs way of indicating EOF.
     hl-todo              ; The fmdkdd layer does that already.

     ;; These come with html
     haml-mode
     jade-mode
     slim-mode

     ;; This comes with git
     evil-magit           ; I'm okay with evilified bindings
     )

   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  (setq-default

   ;; Vim bindings
   dotspacemacs-editing-style 'vim

   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-command-key ":"

   ;; Solarized themes
   dotspacemacs-themes '(spacemacs-light
                         solarized-dark)

   ;; Ubuntu Mono
   dotspacemacs-default-font '("Ubuntu Mono"
                               :size 14
                               :weight normal
                               :width normal
                               :powerline-scale 1.25)

   ;; More recent files
   dotspacemacs-startup-recent-list-size 10

   ;; Don't highlight delimiters in the current scope
   dotspacemacs-highlight-delimiters 'any

   ;; Smooth scrolling is slooooow
   dotspacemacs-smooth-scrolling nil

   ;; Delete trailing whitespace on file save
   dotspacemacs-whitespace-cleanup 'trailing
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init'.  You are free to put any
user code.")

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration. You are free to put any user code."

  (setq racer-cmd "~/.cargo/bin/racer"
        racer-rust-src-path "/usr/local/src/rustc-1.7.0/src")

  (setq company-selection-wrap-around t)

  ;; XXX: these should not be necessary with the Haskell layer in 0.105
  ;; (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
  ;; (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  ;; (setq haskell-interactive-popup-errors nil)
  )
