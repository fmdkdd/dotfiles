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
        js2-mode                            ; JavaScript mode
        (smart-quotes :location local)      ; Auto-insertion of ‘’ instead of '
        (org-reftex :location local)        ; Manage citations in Org files
        web-mode                            ; HTML mode, supports CSS in <style> tags
        rust-mode
        ;; powerline
        ))

(defun fmdkdd/init-rainbow-mode ()
  (use-package rainbow-mode
    :commands rainbow-mode
    :init
    (add-hook 'css-mode-hook 'rainbow-mode)
    (spacemacs/set-leader-keys "tCc" 'rainbow-mode)
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


(defun fmdkdd/init-smart-quotes ()
  (use-package smart-quotes
    :commands (turn-on-smart-quotes
               turn-off-smart-quotes)
    :init
    (add-hook 'text-mode-hook 'turn-on-smart-quotes)
    (add-hook 'html-mode-hook 'turn-off-smart-quotes)

    :config
    (diminish 'smart-quotes-mode)))

(defun fmdkdd/post-init-web-mode ()
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2))

(defun fmdkdd/post-init-rust-mode ()
  (setq rust-indent-offset 2))

;; (defun fmdkdd/post-init-powerline ()
;;   (setq powerline-default-separator 'alternate)

;;   (defun fmdkdd/org-full-outline-path ()
;;     "Concatenate the results of `org-get-outline-path' and
;; `org-get-heading' to get the full outline path to the heading we
;; are currently in."
;;     (unless (org-before-first-heading-p)
;;       (let* ((path (append (org-get-outline-path)
;;                            (cons (org-get-heading t t) nil))))
;;         (org-format-outline-path path 40)))) ; XXX: not sure if the width
;;                                              ; argument works right

;;   (spacemacs|define-mode-line-segment which-org-headline-segment
;;     (fmdkdd/org-full-outline-path)
;;     :when (and active (eq major-mode 'org-mode)))

;;   (setq spacemacs-mode-line-left
;;         '(((workspace-number window-number)
;;            :fallback state-tag :separator "|" :face state-face)
;;           anzu
;;           (buffer-modified buffer-id remote-host)
;;           (point-position line-column)
;;           major-mode
;;           ((flycheck-errors flycheck-warnings flycheck-infos)
;;            :when active)
;;           (erc-track :when active)
;;           (org-pomodoro :when active)
;;           (org-clock :when active)
;;           which-org-headline-segment)

;;         spacemacs-mode-line-right
;;         '((battery :when active)
;;           selection-info
;;           buffer-encoding-abbrev
;;           ((global-mode new-version)
;;            :when active)
;;           buffer-position hud)))
