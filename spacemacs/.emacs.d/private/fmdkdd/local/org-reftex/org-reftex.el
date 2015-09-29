;;; org-reftex.el --- Integrate RefTeX with Org mode
;;
;; Copyright (C) 2015 fmdkdd
;;
;; Author: fmdkdd
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:
;; Provides RefTeX integration in Org mode buffers.  To use, add the line:
;;
;; #+BIBLIOGRAPHY: foo
;;
;; in your document header.  Then, you can call `org-reftex/insert-citation' to
;; insert a 'cite' link with RefTeX.  The cite link contains the key of the
;; BibTeX entry.
;;
;; Use `org-reftex/view-paper' to open the PDF corresponding to the key.  This
;; searches for a file KEY.pdf under the `org-reftex/papers-directory' variable.
;;
;; Finally, you can use `C-c C-o' in Org buffers to follow a cite link by adding
;; the following line to your init:
;;
;; (org-add-link-type "cite" #'org-reftex/follow-citation)

;;; Code:

(require 'org)

(defcustom org-reftex/papers-directory "~/papers"
  "Path to search for PDF files when following a cite link in Org
with `org-reftex/view-paper'")

(defun org-reftex//setup-reftex (fun)
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

;;;###autoload
(defun org-reftex/insert-citation ()
  "Use `reftex-citation' to insert a citation into the buffer.
This looks for a line like

#+BIBLIOGRAPHY: foo

and derives from it that foo.bib is the bibliography file relevant
for this document.  It then installs the necessary environment for RefTeX
to work in this buffer and calls `reftex-citation'  to insert a citation
into the buffer."
  (interactive)
  (org-reftex//setup-reftex
   (lambda ()
     (let ((reftex-cite-format "[[cite:%l][%l]]"))
       (call-interactively 'reftex-citation)))))

;; Otherwise this function is not known
(autoload 'reftex-view-cr-cite "reftex-dcr")

;;;###autoload
(defun org-reftex/follow-citation (&optional key)
  "Follow an Org 'cite' link using Reftex."
  (interactive)
  (let ((key (or key
                 (org-icompleting-read "Citation: " (obe-citations)))))
    (org-reftex//setup-reftex
     (lambda () (reftex-view-cr-cite nil key nil)))))

;;;###autoload
(defun org-reftex/view-paper ()
  "Find the PDF file corresponding to the cite link under point in an Org file.
If point is on link [[cite:KEY][...]], then it will look for a
file named KEY.pdf in the directory specified by
`org-reftex/papers-directory' and open it."
  (interactive)
  (save-excursion
    (when (org-in-regexp org-bracket-link-regexp 1)
      (let ((link (org-link-unescape (org-match-string-no-properties 1))))
        (string-match org-link-re-with-space3 link)
        (let ((type (match-string 1 link))
              (path (match-string 2 link)))
          (when (equal type "cite")
            (org-open-file (expand-file-name (concat path ".pdf")
                                             org-reftex/papers-directory))))))))

(provide 'org-reftex)
