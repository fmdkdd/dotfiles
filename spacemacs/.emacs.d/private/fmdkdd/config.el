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

;; Scale down DejaVu fonts so they match the size of Ubuntu Mono characters.
(setq face-font-rescale-alist
      '((".*DejaVu Sans Mono.*" . 0.9)
        (".*DejaVu Sans.*" . 0.9)))


;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Files
;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Read-only files are in view mode
(setq view-read-only t)

;; Follow symlinks to versioned files
(setq vc-follow-symlinks t)

;; Delete trailing whitespace on file save
(add-hook 'before-save-hook 'delete-trailing-whitespace)


;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Editing
;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Automatic auto-fill
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Auto fill comments in prog modes
(add-hook 'prog-mode-hook 'fmdkdd//auto-fill-comments)

;; Spelling
(add-hook 'prog-mode-hook 'flyspell-prog-mode)


;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Programming
;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; Highlight the following words in comments
(add-hook 'prog-mode-hook 'fmdkdd//add-watchwords)

;; Java indentation
(c-add-style "user-java" '("java" (c-basic-offset . 2)))
(add-hook 'java-mode-hook 'fmdkdd//customize-java-mode)

;; Coffeescript indentation
(setq coffee-tab-width 2)

;; CSS indentation
(setq css-indent-offset 2)

;; Don't ask confirmation to save buffers when compiling
(setq compilation-ask-about-save nil)

;; FIXME: I only use Ensime for its juicy shortcuts to run sbt
;; (remove-hook 'scala-mode-hook 'scala/configure-ensime)
;; (remove-hook 'scala-mode-hook 'scala/maybe-start-ensime)


;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Misc
;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; View image files as images
(auto-image-file-mode)

;;
(eval-after-load 'calendar
  (setq calendar-week-start-day 1))

;; Fuzzy matching is fstr
(setq helm-M-x-fuzzy-match t)
