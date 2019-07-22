;;; delimiter.el --- Add, change and delete delimiters around point -*- lexical-binding: t; -*-

;; Copyright (c) 2017, 2019 fmdkdd
;;
;; Author: fmdkdd

;; This file is not part of GNU Emacs.

;;; Commentary:

;; I wanted the functionality of evil-surround.  There is `insert-pair'
;; built-in, but the code looks too complex for what I need it to do.  And I
;; don't understand how you pass the delimiter char in the first place.
;;
;; I've looked at embrace.el ( https://github.com/cute-jumper/embrace.el ),
;; which is again, too complex for what I need, and does something weird with
;; overlays I don't understand.
;;
;; The following works for me, and uses only basic Elisp functions.

;;; Code:

(defvar delimiter-pair-alist
  '((?\( . ?\))
    (?\[ . ?\])
    (?\{ . ?\})
    (?\< . ?\>)
    (?\" . ?\")
    (?\' . ?\')
    (?\` . ((emacs-lisp-mode . ?\')
            (t . ?\`))))
  "Alist of known delimiter pairs.

The value is either a single char corresponding to the closing
delimiter, or a list of pairs ((MODE . CHAR) ...) to specify a
mode-specific closing delimiter.")

(defun delimiter--get-closing (char)
  "The closing delimiter for CHAR."
  (let ((match (alist-get char delimiter-pair-alist)))
    (cond
     ;; If a list, try to match the mode
     ((listp match)
      (cdr (seq-find
            (pcase-lambda (`(,mode . _))
              (or (eq mode t) (derived-mode-p mode)))
            match)))
     ;; A single char, so return that
     (match)
     ;; Unknown delimiter: assume it's symmetric
     (t char))))

;;;###autoload
(defun delimiter-dwim (char)
  "Surround or delete region or word with CHAR.

When CHAR is 'd', call `delimiter-delete' instead."
  (interactive "cChar: ")
  (if (eq char ?d)
      (call-interactively #'delimiter-delete)
    (delimiter-surround char)))

;;;###autoload
(defun delimiter-surround (char)
  "Surround active region or word with CHAR.

If region is active, surround that.  Otherwise, surround the sexp
under point.

If CHAR is an opening delimiter in `insert-pair-alist', use the
closing delimiter of the pair to surround the region; use CHAR
otherwise."
  (interactive "cChar: ")
  ;; goto-char may move the region when it is active, so save it
  (let ((begin (region-beginning))
        (end   (region-end))
        (closing (delimiter--get-closing char)))
    (save-excursion
      (unless (region-active-p)
        (setq begin (progn (backward-sexp) (point))
              end (progn (forward-sexp) (point))))
      (goto-char end)
      (insert-char closing)
      (goto-char begin)
      (insert-char char))))

;;;###autoload
(defun delimiter-delete (char)
  "Delete the nearest surrounding delimiters opening with CHAR."
  (interactive "cChar to zap: ")
  (let* ((opening-regex (concat "^" (char-to-string char)))
         (closing       (delimiter--get-closing char))
         (closing-regex (concat "^" (char-to-string closing))))
    (save-excursion
      ;; skip-chars-* over search-backward because the former has a bytecode op
      (skip-chars-backward opening-regex)
      (delete-char -1)
      (skip-chars-forward closing-regex)
      (delete-char 1))))

;;;###autoload
(defun delimiter-change (char newchar)
  "Replace the surrounding CHAR delimiters with NEWCHAR."
  (interactive "cChar:\ncReplace with: ")
  (let* ((opening-regex (concat "^" (char-to-string char)))
         (closing       (delimiter--get-closing char))
         (closing-regex (concat "^" (char-to-string closing)))
         (new-closing   (delimiter--get-closing newchar)))
    (save-excursion
      ;; skip-chars-* over search-backward because the former has a bytecode op
      (skip-chars-backward opening-regex)
      (delete-char -1)
      (insert-char newchar)
      (skip-chars-forward closing-regex)
      (delete-char 1)
      (insert-char new-closing))))

(provide 'delimiter)
;;; delimiter.el ends here

;; Local Variables:
;; eval: (fmdkdd/byte-compile-on-save)
;; End:
