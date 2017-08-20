;;; utils.el --- Miscellaneous editing functions -*- lexical-binding: t; -*-

;; Copyright (c) 2017 fmdkdd
;;
;; Author: fmdkdd

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Soome

;;; Code:

;; Best of both worlds
(defun kill-region-or-backward-word ()
  "Kill the region if active, otherwise kill the word before point."
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (if (and (boundp 'subword-mode) subword-mode)
        (subword-backward-kill 1)
      (backward-kill-word 1))))

;; There is `write-file`, but it leaves the previous file around
;; this one is from: http://emacsredux.com/blog/2013/05/04/rename-file-and-buffer/
(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (user-error "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defun auto-fill-comments ()
  (setq-local comment-auto-fill-only-comments t)
  (turn-on-auto-fill))

(defun man-at-point ()
  "Open the man page for the symbol at point."
  (interactive)
  (let ((thing (word-at-point)))
    (man thing)))

(defun describe-thing-at-point ()
  "Describe thing under cursor."
  (interactive)
  (let ((thing (symbol-at-point)))
    (cond
     ((fboundp thing) (describe-function thing))
     ((boundp thing) (describe-variable thing)))))

(defun add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(TODO\\|FIXME\\|HACK\\|REFACTOR\\|DELETE\\|XXX\\):"
          1 font-lock-warning-face t))))

(defun move-beginning-of-line-dwim ()
  "Move to beginning of line text, or to beginning of line."
  (interactive)
  (let ((point-at-first-text))
    (save-excursion
      (beginning-of-line)
      (skip-chars-forward "[:space:]")
      (setq point-at-first-text (point)))
    ;; If we are already at the first non-whitespace char, then move to the
    ;; beginning of line
    (if (eq point-at-first-text (point))
        (beginning-of-line)
      ;; Otherwise, move to first text
      (goto-char point-at-first-text))))

(provide 'utils)
;;; utils.el ends here
