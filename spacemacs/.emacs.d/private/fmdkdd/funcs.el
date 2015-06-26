;;; funcs.el --- fmdkdd Layer functions File
;;
;; Copyright (c) 2015 fmdkdd
;;
;; Author: fmdkdd
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun fmdkdd//auto-fill-comments ()
  (setq-local comment-auto-fill-only-comments t)
  (turn-on-auto-fill))

(defun fmdkdd//add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\|DELETE\\|XXX\\):"
          1 font-lock-warning-face t))))

(defun fmdkdd//customize-java-mode ()
  (c-set-style "user-java"))

(defun fmdkdd/moz-reload ()
  "Reload the current tab of Firefox using moz-repl."
  (interactive)
  (comint-send-string
   (inferior-moz-process)
   "content.document.location.reload(true);"))

(defun fmdkdd//turn-off-truncate-lines ()
  (setq truncate-lines nil))

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
