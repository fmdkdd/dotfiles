;;; config.el --- Spacemacs Layer configuration File
;;
;; Copyright (c) 2015 fmdkdd
;;
;; Author: fmdkdd
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Font
;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; I have this weird bug where the default face of Emacs is set to "Comic Sans"
;; ...  this kicks in when opening a new frame.  This is the only way I found to
;; fix it.  In particular, `set-frame-font' does not help.
(set-face-attribute 'default t :family "Dina" :height 78)

;; Fallback to DejaVu Sans Mono for mathematical symbols not present in the
;; default font (Ubuntu Mono)
(set-fontset-font "fontset-default" '(#x2200 . #x22ff)
                  (font-spec :family "DejaVu Sans Mono"))
;; Arrows are too narrow in a mono font
(set-fontset-font "fontset-default" '(#x2190 . #x21ff)
                  (font-spec :family "DejaVu Sans"))
;; Emoticons and other cute symbols
(set-fontset-font "fontset-default" '(#x1f300 . #x1f6ff)
                  (font-spec :family "Symbola"))
;; Box drawings arcs for Spacemacs box
(set-fontset-font "fontset-default" '(#x256d . #x2579)
                  (font-spec :family "DejaVu Sans Mono"))

;; Scale down DejaVu fonts so they match the size of Ubuntu Mono characters.
;; Ensures lines with Unicode characters have the same width as lines with ASCII
;; chars.  We cannot set the `:size' property of the above fontspecs, because
;; they do not scale with the size of the main font.
;; (setq face-font-rescale-alist
;;       '((".*DejaVu Sans Mono.*" . 1)
;;         (".*DejaVu Sans.*" . 1)))


;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; UI
;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Focus follow mouse
(setq mouse-autoselect-window t)

;; Fit temporary windows to their content
(temp-buffer-resize-mode)

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Files
;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; I use customize file for machine-local settings. This file goes to 'emacs.d',
;; and is not version controlled.
(setq custom-file (locate-user-emacs-file "local-preferences.el"))
(load custom-file)

;; Read-only files are in view mode
(setq view-read-only t)

;; Follow symlinks to versioned files
(setq vc-follow-symlinks t)


;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Editing
;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Restore continuation and truncation lines from standard Emacs
(setq-default fringe-indicator-alist
              '((truncation left-arrow right-arrow)
                (continuation left-curly-arrow right-curly-arrow)
                (overlay-arrow . right-triangle)
                (empty-line empty-line)))

;; Indicate empty lines in fringe using default Emacs bitmap
(setq-default indicate-empty-lines t)

;; Use continuation lines
(add-hook 'text-mode-hook 'fmdkdd//turn-off-truncate-lines)

;; Automatic auto-fill
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Auto fill comments in prog modes
(add-hook 'prog-mode-hook 'fmdkdd//auto-fill-comments)

;; Two spaces end a sentence
(setq sentence-end-double-space t)

;; Automatically save private dictionary
(setq ispell-silently-savep t)

;; Sticky means highlight current line in all windows, not just in the selected
;; window.  When not sticky, hl-line unhlighlights the current line before
;; re-highlighting, which causes a flash when editing.
(setq global-hl-line-sticky-flag t)

;; Actually, global highlight is distracting
(global-hl-line-mode -1)

;; Without smartparens, this is necessary
(show-paren-mode)

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Programming
;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Highlight 'XXX', 'FIXME', etc. in comments
(defface fmdkdd-comment-keyword-face
  '((t . (:inherit warning)))
  "Face used to highlight comment keywords in code (TODO, FIXME, etc.)")
(add-hook 'prog-mode-hook 'fmdkdd//add-watchwords)

;; Java indentation
(c-add-style "user-java" '("java" (c-basic-offset . 2)))
(add-hook 'java-mode-hook 'fmdkdd//customize-java-mode)

;; Coffeescript indentation
(setq coffee-tab-width 2)

;; CSS indentation
(setq css-indent-offset 2)

;; JS indentation in web mode
(setq js-indent-level 2)

;; Don't ask confirmation to save buffers when compiling
(setq compilation-ask-about-save nil)


;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Misc
;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; View image files as images
(auto-image-file-mode)

;; Monday is the first day of the week
(with-eval-after-load 'calendar
  (setq calendar-week-start-day 1))

;; Formatting BibTeX entries on C-c C-c
(with-eval-after-load 'bibtex
  (setq bibtex-align-at-equal-sign t
        bibtex-entry-format
        '(opts-or-alts                  ; remove OPTS
          required-fields               ; warn on missing required field
          numerical-fields              ; delete braces around year, num fields
          realign                       ; align on =
          delimiters                    ; change delimiters to braces
          whitespace                    ; trim space in fields
          sort-fields)))

;; Use ssh for finding remote files
(setq tramp-default-method "ssh")

;; Emacs assumes I cannot run xdg-open because I don't have Gnome.  I know
;; better.
(setq browse-url-browser-function #'browse-url-xdg-open)

;; This is useful just for thinking about updating copyright years.
(add-hook 'before-save-hook #'copyright-update)
