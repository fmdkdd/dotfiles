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
        js2-mode                            ; JavaScript mode
        (smart-quotes :location local)
        ))

(defun fmdkdd/init-rainbow-mode ()
  (use-package rainbow-mode
    :commands rainbow-mode
    :init
    (add-hook 'css-mode-hook 'rainbow-mode)
    (evil-leader/set-key "tCc" 'rainbow-mode)
    :config (spacemacs|hide-lighter rainbow-mode)))

(defun fmdkdd/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-init
    ;; Enable Babel evaluation of JavaScript, Dot.
    (require 'ob-js)
    (require 'ob-dot)
    :post-config
    ;; No indentation in Org files.
    (setq org-adapt-indentation nil)

    ;; No visual indentation either, as in deep subtrees, lines will go too far
    ;; on the right.  Seems to be reset, so setq-default should do it.
    (setq-default org-startup-indented nil)

    ;; Syntactic coloration of source blocks
    (setq org-src-fontify-natively t)

    ;; No indentation of src blocks.
    (setq org-src-preserve-indentation t)

    ;; Nice LaTeX entities
    (setq-default org-pretty-entities t)


    (defun fmdkdd//org-reftex-setup (fun)
      "Setup the Reftex environment from looking up the BIBLIOGRAPHY
keyword in an Org file, and call FUN."
      (let ((reftex-docstruct-symbol 'rds)
            rds bib)
        (save-excursion
          (save-restriction
            (widen)
            (let ((case-fold-search t)
                  (re "^#\\+bibliography:[ \t]+\\([^ \t\n]+\\)"))
              (if (not (save-excursion
                         (or (re-search-forward re nil t)
                             (re-search-backward re nil t))))
                  (error "No bibliography defined in file")
                (setq bib (concat (match-string 1) ".bib")
                      rds (list (list 'bib bib)))))))
        (funcall fun)))

    (defun fmdkdd/org-reftex-citation ()
      "Use reftex-citation to insert a citation into the buffer.
This looks for a line like

#+BIBLIOGRAPHY: foo

and derives from it that foo.bib is the bibliography file relevant
for this document.  It then installs the necessary environment for RefTeX
to work in this buffer and calls `reftex-citation'  to insert a citation
into the buffer."
      (interactive)
      (fmdkdd//org-reftex-setup
       (lambda ()
         (let ((reftex-cite-format "[[cite:%l][%l]]"))
           (call-interactively 'reftex-citation)))))

    (defun fmdkdd/org-reftex-view-citation (&optional key)
      "Follow an Org cite link using Reftex."
      (interactive)
      (let ((key (or key
                     (org-icompleting-read "Citation: " (obe-citations)))))
        (fmdkdd//org-reftex-setup
         (lambda () (reftex-view-cr-cite nil key nil)))))

    (defvar fmdkdd/papers-directory "~/Archimède/Thèse/papers"
      "Path to search for PDF files when following a cite link in Org
with `fmdkdd/org-view-paper'")

    (defun fmdkdd/org-view-paper ()
      "Find the PDF file corresponding to the cite link under point in an Org file.
If point is on link [[cite:KEY][...]], then it will look for a
file named KEY.pdf in the directory specified by
`fmdkdd/papers-directory' and open it."
      (interactive)
      (save-excursion
        (when (org-in-regexp org-bracket-link-regexp 1)
          (let ((link (org-link-unescape (org-match-string-no-properties 1))))
            (string-match org-link-re-with-space3 link)
            (let ((type (match-string 1 link))
                  (path (match-string 2 link)))
              (when (equal type "cite")
                (org-open-file (expand-file-name (concat path ".pdf")
                                                 fmdkdd/papers-directory))))))))

    ;; Follow citations links using Reftex.
    (org-add-link-type "cite" #'fmdkdd/org-reftex-view-citation)

    ;; Open links to Mozilla Archive Format Files in Firefox
    (add-to-list 'org-file-apps '("maff" . "firefox %s"))

    ;; Hide history of done state into PROPERTIES drawer.
    (setq org-log-into-drawer t)
    (setq org-clock-into-drawer t)

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

    ;; js2-mode to edit js snippets from org-mode
    (add-to-list 'org-src-lang-modes '("js" . js2))
    ;; ... and for nodejs snippets as well
    (add-to-list 'org-src-lang-modes '("nodejs" . js2))

    ;; SpiderMonkey is the default js engine for Babel
    (setq org-babel-js-cmd "js"
          org-babel-js-function-wrapper "print(JSON.stringify(function(){%s}(), null, 2))"
          org-babel-js-procedure-wrapper "try { %s } catch (e) { print(e); }")

    ;; Add nodejs language for babel.
    (defun org-babel-execute:nodejs (body params)
      "Execute a block of JavaScript (through nodejs) code with org-babel."
      (let ((org-babel-js-cmd "node"))
        (org-babel-execute:js body params)))

    ;; Customize `org-babel-eval' to add support for stderr in result output.
    (defun fmdkdd/org-babel-eval (ob-eval cmd body)
      "Run CMD on BODY.
If CMD succeeds then return its results, otherwise display
STDERR with `org-babel-eval-error-notify'."
      (let (exit-code)
        ;; "stderr" in results parameter will interleave stderr and stdout in the
        ;; results.  It will also prevent the Org-Babel Error buffer to sprout.
        (if (member "stderr" result-params)
            (progn
              (with-temp-buffer
                (insert body)
                (setq exit-code
                      ;; `org-babel-eval' used a baked-in variant of
                      ;; `shell-command-on-region' for legacy purposes.  It behaved
                      ;; badly when the err-buff argument was nil, because of an
                      ;; advice (on delete file).  Might as well use the built-in
                      ;; function.
                      (shell-command-on-region
                       (point-min) (point-max) cmd t t))
                (buffer-string)))
          ;; "stderr" absent, use standard `org-babel-eval'.
          (funcall ob-eval cmd body))))
    (advice-add 'org-babel-eval :around #'fmdkdd/org-babel-eval)

    (evil-leader/set-key-for-mode 'org-mode
      "c" nil "mc" 'fmdkdd/org-reftex-citation
      "mv" 'fmdkdd/org-view-paper)

    (evil-define-key 'normal evil-org-mode-map
      "g<" 'org-previous-link
      "g>" 'org-next-link)))

(defun fmdkdd/post-init-helm ()
  ;; Fuzzy matching is fstr
  (setq helm-M-x-fuzzy-match t
        helm-recentf-fuzzy-match t
        helm-lisp-fuzzy-completion t
        helm-imenu-fuzzy-match t))

;; Autoload `reftex-view-cr-cite' to follow citations in Org files.
(defun fmdkdd/init-reftex-dcr ()
  (use-package reftex-dcr
    :commands reftex-view-cr-cite))

(defun fmdkdd/post-init-js2-mode ()
  (add-to-list 'auto-mode-alist '("\\.es6$" . js2-mode)))

(defun fmdkdd/init-smart-quotes ()
  (use-package smart-quotes
    :commands (turn-on-smart-quotes
               turn-off-smart-quotes)
    :init
    (add-hook 'text-mode-hook 'turn-on-smart-quotes)
    (add-hook 'html-mode-hook 'turn-off-smart-quotes)
    :config
    (diminish 'smart-quotes-mode)))
