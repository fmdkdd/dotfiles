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

;; Fuzzy matching is fstr
(setq helm-M-x-fuzzy-match t)

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

;; Enable Babel evaluation of JavaScript
(eval-after-load 'org-babel
  (require 'ob-js))

;; SpiderMonkey is the default REPL
(setq org-babel-js-cmd "js"
      org-babel-js-function-wrapper "print(JSON.stringify(function(){%s}(), null, 2))"
      org-babel-js-procedure-wrapper "try { %s } catch (e) { print(e); }")


;; Don't ask confirmation to save buffers when compiling
(setq compilation-ask-about-save nil)

;; FIXME: I only use Ensime for its juicy shortcuts to run sbt
;; (remove-hook 'scala-mode-hook 'scala/configure-ensime)
;; (remove-hook 'scala-mode-hook 'scala/maybe-start-ensime)


;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Org mode
;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(eval-after-load 'org
  '(progn
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

     ;; FIXME: this only works when zenburn has been loaded
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
                         (org-agenda-skip-function
                          '(org-agenda-skip-entry-if 'notdeadline))
                         (org-agenda-todo-ignore-deadlines 'near))))))
           org-agenda-ndays 1)))


;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Misc
;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; View image files as images
(auto-image-file-mode)

;;
(eval-after-load 'calendar
  (setq calendar-week-start-day 1))
