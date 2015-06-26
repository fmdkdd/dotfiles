;;; packages.el --- fmdkdd Layer packages File for Spacemacs
;;
;; Copyright (c) 2015 fmdkdd
;;
;; Author: fmdkdd
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages which
;; require an initialization must be listed explicitly in the list.
(setq fmdkdd-packages
  '(org                                 ; Plain text powerhouse
    rainbow-mode                        ; CSS colors preview
    helm                                ; Better ido
    reftex-dcr                          ; Needed to view citations in Org files
    ))

;; List of packages to exclude.
(setq fmdkdd-excluded-packages '())

(defun fmdkdd/init-rainbow-mode ()
  (use-package rainbow-mode
    :defer t
    :init (add-hook 'css-mode-hook 'rainbow-mode)))

(defun fmdkdd/init-org ()
  (use-package org
    :mode ("\\.org$" . org-mode)
    :init
    ;; Enable Babel evaluation of JavaScript, Dot
    (eval-after-load 'org-babel
      (progn
        (require 'ob-js)
        (require 'ob-dot)))

    :config
    ;; No indentation in Org files
    (setq org-adapt-indentation nil)

    ;; Syntactic coloration of source blocks
    (setq org-src-fontify-natively t)

    ;; Nice LaTeX entities
    (setq-default org-pretty-entities t)

    ;; Open links to Mozilla Archive Format Files in Firefox
    (add-to-list 'org-file-apps '("maff" . "firefox %s"))

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
          org-agenda-ndays 1)

    ;; Automatically redisplay images after executing code.  Great for Dot
    ;; graphs source blocks.
    (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)

    ;; SpiderMonkey is the default REPL
    (setq org-babel-js-cmd "js"
          org-babel-js-function-wrapper "print(JSON.stringify(function(){%s}(), null, 2))"
          org-babel-js-procedure-wrapper "try { %s } catch (e) { print(e); }")

    ;; Follow citations links using Reftex.
    (org-add-link-type "cite" #'fmdkdd/org-reftex-view-citation)
    ))

(defun fmdkdd/init-helm ()
  (use-package helm
    :defer t
    :init
    ;; Fuzzy matching is fstr
    (setq helm-M-x-fuzzy-match t
          helm-recentf-fuzzy-match t
          helm-lisp-fuzzy-completion t
          helm-imenu-fuzzy-match t)
    ))

;; Autoload `reftex-view-cr-cite' to follow citations in Org files.
(defun fmdkdd/init-reftex-dcr ()
  (use-package reftex-dcr
    :commands reftex-view-cr-cite
    ))
