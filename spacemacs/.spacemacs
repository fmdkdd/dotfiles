;; -*- mode: dotspacemacs -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (ie. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     ;; --------------------------------------------------------
     ;; Example of useful layers you may want to use right away
     ;; Uncomment a layer name and press C-c C-c to install it
     ;; --------------------------------------------------------
     auto-completion
     ;; better-defaults
     (git :variables
          git-gutter-use-fringe t)
     javascript
     scala
     haskell
     ;; markdown
     org
     ;; syntax-checking
     )
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; Either `vim' or `emacs'. Evil is always enabled but if the variable
   ;; is `emacs' then the `holy-mode' is enabled at startup.
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progess in `*Messages*' buffer.
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to a .PNG file.
   ;; If the value is nil then no banner is displayed.
   ;; dotspacemacs-startup-banner 'official
   dotspacemacs-startup-banner 'official
   ;; t if you always want to see the changelog at startup
   dotspacemacs-always-show-changelog t
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'."
   dotspacemacs-startup-lists '(recents projects)
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(solarized-light
                         solarized-dark
                         leuven
                         monokai
                         zenburn)
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Ubuntu Mono"
                               :size 14
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it.
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil the paste micro-state is enabled. While enabled pressing `p`
   ;; several times cycle between the kill ring content.
   dotspacemacs-enable-paste-micro-state t
   ;; Guide-key delay in seconds. The Guide-key is the popup buffer listing
   ;; the commands bound to the current keystrokes.
   dotspacemacs-guide-key-delay 0.4
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil ;; to boost the loading time.
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up.
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX."
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line.
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen.
   dotspacemacs-smooth-scrolling t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   dotspacemacs-smartparens-strict-mode nil
   ;; If non nil advises quit functions to keep server open when quitting.
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now.
   dotspacemacs-default-package-repository nil
   )
  ;; User initialization goes here
  )

(defun dotspacemacs/config ()
  "Configuration function.
 This function is called at the very end of Spacemacs initialization after
layers configuration."

  ;; Remap HJKL for Colemak layout.
  ;; (h, left), (j, down), (k, top)
  ;; becomes
  ;; (j, left), (k, down), (h, top)
  (define-key evil-normal-state-map "h" 'evil-previous-visual-line)
  (define-key evil-normal-state-map "k" 'evil-next-visual-line)
  (define-key evil-normal-state-map "j" 'nil)
  (define-key evil-visual-state-map "h" 'evil-previous-visual-line)
  (define-key evil-visual-state-map "k" 'evil-next-visual-line)
  (define-key evil-visual-state-map "j" 'nil)
  (define-key evil-normal-state-map "K" 'nil)
  (define-key evil-normal-state-map "L" 'spacemacs/smart-doc-lookup)
  (define-key evil-motion-state-map "K" 'evil-window-bottom)
  (define-key evil-motion-state-map "L" 'evil-lookup)
  (define-key evil-motion-state-map "h" 'evil-previous-line)
  (define-key evil-motion-state-map "k" 'evil-next-line)
  (define-key evil-motion-state-map "j" 'evil-backward-char)
  (define-key evil-motion-state-map "gh" 'evil-previous-visual-line)
  (define-key evil-motion-state-map "gk" 'evil-next-visual-line)
  (define-key evil-motion-state-map "gj" 'nil)
  (define-key evil-motion-state-map "zh" 'nil)
  (define-key evil-motion-state-map "zj" 'evil-scroll-column-left)
  (define-key evil-motion-state-map "zh" 'nil)
  (define-key evil-motion-state-map "zJ" 'evil-scroll-left)
  (define-key evil-evilified-state-map "h" 'evil-previous-visual-line)
  (define-key evil-evilified-state-map "k" 'evil-next-visual-line)
  (define-key evil-evilified-state-map "j" 'evil-backward-char)

  (evil-leader/set-key
    "jh" 'evil-goto-next-line-and-indent
    "jk" 'sp-newline
    "jj" 'spacemacs/push-mark-and-goto-beginning-of-line
    "jl" 'spacemacs/push-mark-and-goto-end-of-line)

  (evil-leader/set-key
    "wH" 'evil-window-move-very-top
    "wh" 'evil-window-up
    "wK" 'evil-window-move-very-bottom
    "wk" 'evil-window-down
    "wJ" 'evil-window-move-far-left
    "wj" 'evil-window-left
    "wL" 'evil-window-move-far-right
    "wl" 'evil-window-right)

  (define-key evil-window-map "h" 'evil-window-up)
  (define-key evil-window-map "H" 'evil-window-move-very-top)
  (define-key evil-window-map "k" 'evil-window-down)
  (define-key evil-window-map "K" 'evil-window-move-very-bottom)
  (define-key evil-window-map "j" 'evil-window-left)
  (define-key evil-window-map "J" 'evil-window-move-far-left)
  (define-key evil-window-map "\C-h" 'evil-window-up)
  (define-key evil-window-map (kbd "C-S-h") 'evil-window-move-very-top)
  (define-key evil-window-map "\C-k" 'evil-window-down)
  (define-key evil-window-map (kbd "C-S-k") 'evil-window-move-very-bottom)
  (define-key evil-window-map "\C-j" 'evil-window-left)
  (define-key evil-window-map (kbd "C-S-j") 'evil-window-move-far-left)

  (defun fmdkdd//window-manipulation-full-doc ()
    "Full documentation for window manipulation micro-state."
    "
    [?]                       display this help
    [0,9]                     go to numbered window
    [-] [/] [s] [v] [S] [V]   split windows bellow|right and focus
    [c] [C]                   close current|other windows
    [g]                       toggle golden-ratio
    [j] [k] [h] [l]           go to left|bottom|top|right
    [J] [K] [H] [L]           move windows to far/very left|bottom|top|right
    [[] []] [{] [}]           shrink/enlarge horizontaly and verticaly respectively
    [o] [w]                   other frame|window
    [R]                       rotate windows
    [u] [U]                   restore previous|next window layout")

  (defun fmdkdd//window-manipulation-move-doc ()
    "Help string for moving between windows"
    (concat "[j] [k] [h] [l] to move focus, "
            "[j] [k] [h] [L] to move window, "
            "[R]otate windows, other [f]rame, other [w]indow"))

  (spacemacs|define-micro-state window-manipulation/fmdkdd
    :doc "[?] for help"
    :evil-leader "w."
    :bindings
    ("?" nil                                   :doc (fmdkdd//window-manipulation-full-doc))
    ("0" select-window-0                       :doc (fmdkdd//window-manipulation-move-doc))
    ("1" select-window-1                       :doc (fmdkdd//window-manipulation-move-doc))
    ("2" select-window-2                       :doc (fmdkdd//window-manipulation-move-doc))
    ("3" select-window-3                       :doc (fmdkdd//window-manipulation-move-doc))
    ("4" select-window-4                       :doc (fmdkdd//window-manipulation-move-doc))
    ("5" select-window-5                       :doc (fmdkdd//window-manipulation-move-doc))
    ("6" select-window-6                       :doc (fmdkdd//window-manipulation-move-doc))
    ("7" select-window-7                       :doc (fmdkdd//window-manipulation-move-doc))
    ("8" select-window-8                       :doc (fmdkdd//window-manipulation-move-doc))
    ("9" select-window-9                       :doc (fmdkdd//window-manipulation-move-doc))
    ("-" split-window-below-and-focus          :doc (spacemacs//window-manipulation-split-doc))
    ("/" split-window-right-and-focus          :doc (spacemacs//window-manipulation-split-doc))
    ("[" spacemacs/shrink-window-horizontally  :doc (spacemacs//window-manipulation-resize-doc))
    ("]" spacemacs/enlarge-window-horizontally :doc (spacemacs//window-manipulation-resize-doc))
    ("{" spacemacs/shrink-window               :doc (spacemacs//window-manipulation-resize-doc))
    ("}" spacemacs/enlarge-window              :doc (spacemacs//window-manipulation-resize-doc))
    ("c" delete-window                         :doc (spacemacs//window-manipulation-layout-doc))
    ("C" delete-other-windows                  :doc (spacemacs//window-manipulation-layout-doc))
    ("g" spacemacs/toggle-golden-ratio         :doc (spacemacs//window-manipulation-gratio-doc))
    ("j" evil-window-left                      :doc (fmdkdd//window-manipulation-move-doc))
    ("k" evil-window-down                      :doc (fmdkdd//window-manipulation-move-doc))
    ("h" evil-window-up                        :doc (fmdkdd//window-manipulation-move-doc))
    ("l" evil-window-right                     :doc (fmdkdd//window-manipulation-move-doc))
    ("J" evil-window-move-far-left             :doc (fmdkdd//window-manipulation-move-doc))
    ("K" evil-window-move-very-bottom          :doc (fmdkdd//window-manipulation-move-doc))
    ("H" evil-window-move-very-top             :doc (fmdkdd//window-manipulation-move-doc))
    ("L" evil-window-move-far-right            :doc (fmdkdd//window-manipulation-move-doc))
    ("o" other-frame                           :doc (fmdkdd//window-manipulation-move-doc))
    ("R" rotate-windows                        :doc (fmdkdd//window-manipulation-move-doc))
    ("s" split-window-below                    :doc (spacemacs//window-manipulation-split-doc))
    ("S" split-window-below-and-focus          :doc (spacemacs//window-manipulation-split-doc))
    ("u" winner-undo                           :doc (spacemacs//window-manipulation-layout-doc))
    ("U" winner-redo                           :doc (spacemacs//window-manipulation-layout-doc))
    ("v" split-window-right                    :doc (spacemacs//window-manipulation-split-doc))
    ("V" split-window-right-and-focus          :doc (spacemacs//window-manipulation-split-doc))
    ("w" other-window                          :doc (fmdkdd//window-manipulation-move-doc)))

  (evil-define-key 'normal evil-org-mode-map
    "gh" 'org-backward-heading-same-level
    "gj" 'outline-up-heading
    "gk" 'org-forward-heading-same-level)

  (evil-define-key 'normal evil-org-mode-map
    (kbd "M-h") 'org-metaup
    (kbd "M-k") 'org-metadown
    (kbd "M-j") 'org-metaleft
    (kbd "M-H") 'org-shiftmetaup
    (kbd "M-K") 'org-shiftmetadown
    (kbd "M-J") 'org-shiftmetaleft)

  (evil-define-key 'insert evil-org-mode-map
    (kbd "M-h") 'org-metaup
    (kbd "M-k") 'org-metadown
    (kbd "M-j") 'org-metaleft
    (kbd "M-H") 'org-shiftmetaup
    (kbd "M-K") 'org-shiftmetadown
    (kbd "M-J") 'org-shiftmetaleft)

  ;; Read-only files are in view mode
  (setq view-read-only t)

  ;; Follow symlinks to versioned files
  (setq vc-follow-symlinks t)

  ;; Font setup
  ;; Fallback to DejaVu Sans Mono for mathematical symbols not present
  ;; in the default font (Ubuntu Mono)
  (set-fontset-font "fontset-default" '(#x2200 . #x22ff) (font-spec :family "DejaVu Sans Mono"))
  ;; Arrows are too narrow in a mono font
  (set-fontset-font "fontset-default" '(#x2190 . #x21ff) (font-spec :family "DejaVu Sans"))
  ;; Emoticons and other cute symbols
  (set-fontset-font "fontset-default" '(#x1f300 . #x1f6ff) (font-spec :family "Symbola"))

  ;; Scale down DejaVu fonts so they match the size of Ubuntu Mono
  ;; characters.
  (setq face-font-rescale-alist
        '((".*DejaVu Sans Mono.*" . 0.9)
          (".*DejaVu Sans.*" . 0.9)))

  ;; Delete trailing whitespace on file save
  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  ;; Automatic auto-fill
  (add-hook 'text-mode-hook 'turn-on-auto-fill)

  ;; Curly quotes
  (add-to-list 'load-path (locate-user-emacs-file "private"))
  (require 'smart-quotes)
  (add-hook 'text-mode-hook 'turn-on-smart-quotes)
  (diminish 'smart-quotes-mode)
  ;; But not in HTML
  (add-hook 'html-mode-hook 'turn-off-smart-quotes)

  ;; Highlight the following words in comments
  (defun add-watchwords ()
    (font-lock-add-keywords
     nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\|DELETE\\|XXX\\):"
            1 font-lock-warning-face t))))
  (add-hook 'prog-mode-hook #'add-watchwords)

  ;; Don't ask confirmation to save buffers when compiling
  (setq compilation-ask-about-save nil)

  ;; FIXME: I only use Ensime for its juicy shortcuts to run sbt
  ;; (remove-hook 'scala-mode-hook 'scala/configure-ensime)
  ;; (remove-hook 'scala-mode-hook 'scala/maybe-start-ensime)

  ;;; Org mode
  (eval-after-load 'org
    '(progn

       ;; No org-indent
       (remove-hook 'org-mode-hook 'org-indent-mode)

       ;; No indentation in Org files
       (setq org-adapt-indentation nil)

       ;; Syntactic coloration of source blocks
       (setq org-src-fontify-natively t)

       ;; Nice LaTeX entities
       (setq-default org-pretty-entities t)

       ;; Open links to Mozilla Archive Format Files in Firefox
       (add-to-list 'org-file-apps
                    '("maff" . "firefox %s"))

       (setq org-log-into-drawer t)
       (setq org-clock-into-drawer t)

       ;; (setq org-todo-keyword-faces
       ;;       (zenburn-with-color-variables
       ;;        `(("TODO" . org-warning)
       ;;          ("NEXT" . (:foreground ,zenburn-yellow :weight bold))
       ;;          ("WAIT" . (:foreground ,zenburn-orange :weight bold))
       ;;          ("CANCELED" . (:foreground ,zenburn-blue-1 :weight bold))
       ;;          ("DELEGATED" . (:foreground ,zenburn-green :weight bold)))))

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
             org-agenda-ndays 1)))

  (eval-after-load "org-agenda"
    '(progn
       (define-key org-agenda-mode-map "j" 'nil)
       (define-key org-agenda-mode-map "h" 'org-agenda-previous-line)
       (define-key org-agenda-mode-map "k" 'org-agenda-next-line)
       ))

  ;; View image files as images
  (auto-image-file-mode)

  ;; Calendar
  (eval-after-load 'calendar
    (setq calendar-week-start-day 1))
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-case-fold-search nil)
 '(ahs-default-range (quote ahs-range-whole-buffer))
 '(ahs-idle-interval 0.25)
 '(ahs-idle-timer 0 t)
 '(ahs-inhibit-face-list nil)
 '(org-agenda-files (quote ("~/Archimède/Thèse/notes/todo.org")))
 '(ring-bell-function (quote ignore) t)
 '(writeroom-width 0.5))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
