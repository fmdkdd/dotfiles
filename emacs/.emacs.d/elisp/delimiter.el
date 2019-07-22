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
        ;; Get corresponding char from alist; Use CHAR for closing if it's not
        ;; in the alist (assume it's a symmetric delimiter)
        (closing (or (car (alist-get char insert-pair-alist))
                     char)))
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
         (closing       (car (alist-get char insert-pair-alist)))
         (closing-regex (or (and closing (concat "^" (char-to-string closing)))
                            opening-regex)))
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
         (closing       (car (alist-get char insert-pair-alist)))
         (closing-regex (or (and closing (concat "^" (char-to-string closing)))
                            opening-regex))
         ;; Find replacement closing character in alist; use newchar if not found
         (new-closing   (or (car (alist-get newchar insert-pair-alist))
                            newchar))
         ;; Save point to restore it at the end
         (p             (point)))
    (save-excursion
      ;; skip-chars-* over search-backward because the former has a bytecode op
      (skip-chars-backward opening-regex)
      (delete-char -1)
      (insert-char newchar)
      (skip-chars-forward closing-regex)
      (delete-char 1)
      (insert-char new-closing))
    ;; Restore point to original position
    (goto-char p)))

(provide 'delimiter)
;;; delimiter.el ends here

;; Local Variables:
;; eval: (fmdkdd/byte-compile-on-save)
;; End:
