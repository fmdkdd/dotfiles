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
      '(org                            ; Plain text powerhouse
        rainbow-mode                   ; CSS colors preview
        rainbow-delimiters             ; font-lock parens, braces
        js2-mode                       ; JavaScript mode
        (org-reftex :location local)   ; Manage citations in Org files
        web-mode                       ; HTML mode, supports CSS in <style> tags
        rust-mode
        racer                          ; Completion for rust
        spaceline                      ; Customize the modeline
        page-break-lies                ; Horizontal rule for ^L character
        company                        ; Auto completion
        ))

(defun fmdkdd/init-rainbow-mode ()
  (use-package rainbow-mode
    :commands rainbow-mode
    :init
    (add-hook 'css-mode-hook 'rainbow-mode)
    (spacemacs/set-leader-keys "tCc" 'rainbow-mode)
    :config (spacemacs|hide-lighter rainbow-mode)))

(defun fmdkdd/post-init-rainbow-delimiters ()
  (setq rainbow-delimiters-max-face-count 1))

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

    ;; Edit SRC blocks in the same window.
    (setq org-src-window-setup 'current-window)

    ;; Nice LaTeX entities
    (setq-default org-pretty-entities t)

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

    ;; Handle 'cite' links via org-reftex
    (org-add-link-type "cite" #'org-reftex/follow-citation)

    (add-to-list 'org-structure-template-alist
                 '("as" "#+BEGIN_aside\n?\n#+END_aside"))
    (add-to-list 'org-structure-template-alist
                 '("sf" "#+BEGIN_side-figure\n?\n#+END_side-figure"))
    (add-to-list 'org-structure-template-alist
                 '("ff" "#+BEGIN_full-figure\n?\n#+END_full-figure"))
    (add-to-list 'org-structure-template-alist
                 '("ep" "#+BEGIN_epigraph\n?\n#+END_epigraph"))

    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "c" 'org-reftex/insert-citation
      "v" 'org-reftex/view-paper)

    (evil-define-key 'normal evil-org-mode-map
      "g<" 'org-previous-link
      "g>" 'org-next-link)))

(defun fmdkdd/init-org-reftex ()
  (use-package org-reftex
    :commands (org-reftex/insert-citation
               org-reftex/follow-citation
               org-reftex/view-paper)))

(defun fmdkdd/post-init-js2-mode ()
  (add-to-list 'auto-mode-alist '("\\.es6$" . js2-mode))

  (setq js2-basic-offset 2
        js2-strict-missing-semi-warning nil
        js2-strict-trailing-comma-warning nil))

(defun fmdkdd/post-init-web-mode ()
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

(defun fmdkdd/post-init-rust-mode ()
  (setq rust-indent-offset 2)

  (spacemacs/set-leader-keys
    "ee" #'flycheck-explain-error-at-point)

  (evil-define-key 'normal rust-mode-map
    "L" #'racer-describe))

(defun fmdkdd/pre-init-racer ()
  (spacemacs|use-package-add-hook racer
    :post-init
    (setq racer-cmd "~/.cargo/bin/racer"
          racer-rust-src-path "/usr/local/src/rustc-1.11.0/src")))

(defun fmdkdd/post-init-spaceline ()
  (setq powerline-default-separator 'alternate)

  (spaceline-define-segment which-org-headline-segment
    (fmdkdd/org-full-outline-path)
    :when (and active (eq major-mode 'org-mode)))

  ;; Spacemacs 0.200 installs its own spaceline in this user-config hook.  We
  ;; append our own configuration after it.
  (add-hook 'spacemacs-post-user-config-hook
            (lambda ()
              (spaceline-compile
               "main"
               '(((persp-name workspace-number window-number)
                  :fallback evil-state :separator "|" :face highlight-face)
                 anzu auto-compile
                 (buffer-modified buffer-id remote-host)
                 (point-position line-column)
                 major-mode
                 ((flycheck-error flycheck-warning flycheck-info)
                  :when active)
                 (version-control :when active)
                 (erc-track :when active)
                 (org-pomodoro :when active)
                 (org-clock :when active)
                 which-org-headline-segment)

               '((battery :when active)
                 (python-pyvenv :fallback python-pyenv)
                 selection-info
                 buffer-encoding-abbrev
                 (global :when active)
                 (new-version :when active)
                 buffer-position hud)))
            t))

(defun fmdkdd/post-init-page-break-lines ()
  (add-to-list 'page-break-lines-modes 'js2-mode)
  (add-to-list 'page-break-lines-modes 'web-mode))

(defun fmdkdd/pre-init-company ()
  (spacemacs|use-package-add-hook company
    :post-init
    (setq company-selection-wrap-around t)))
