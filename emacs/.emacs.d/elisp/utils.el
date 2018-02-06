;;; utils.el --- Miscellaneous editing functions -*- lexical-binding: t; -*-

;; Copyright (c) 2017, 2018 fmdkdd
;;
;; Author: fmdkdd

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Utilities functions for my own configuration.

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
      (skip-chars-forward "[ \t]")
      (setq point-at-first-text (point)))
    ;; If we are already at the first non-whitespace char, then move to the
    ;; beginning of line
    (if (eq point-at-first-text (point))
        (beginning-of-line)
      ;; Otherwise, move to first text
      (goto-char point-at-first-text))))

;;; From https://www.emacswiki.org/emacs/download/goto-last-change.el

(defvar-local goto-last-change-undo nil
  "The `buffer-undo-list' entry of the previous `goto-last-change' command.")

(defun goto-last-change (&optional mark-point)
  "Set point to the position of the last change.
Consecutive calls set point to the position of the previous
change.  With a prefix arg (optional arg MARK-POINT non-nil), set
mark so `exchange-point-and-mark' will return point to the
current position."
  (interactive "P")
  (when (eq buffer-undo-list t)
    (error "No undo information in this buffer"))
  (when mark-point
    (push-mark))
  (let ((position nil)
        (minimal-line-distance 20)
	(undo-list (if (and (eq this-command last-command)
			    goto-last-change-undo)
		       (cdr (memq goto-last-change-undo buffer-undo-list))
		     buffer-undo-list))
	undo)
    (while (and undo-list
                (or (not position)
                    (eql position (point))
                    (and minimal-line-distance
                         ;; The first invocation always goes to the last change, subsequent ones skip
                         ;; changes closer to (point) then minimal-line-distance.
                         (memq last-command '(goto-last-change
                                              goto-last-change-with-auto-marks))
                         (< (count-lines (min position (point-max)) (point))
                            minimal-line-distance))))
      (setq undo (car undo-list))
      (cond ((and (consp undo) (integerp (car undo)) (integerp (cdr undo)))
	     ;; (BEG . END)
	     (setq position (cdr undo)))
	    ((and (consp undo) (stringp (car undo))) ; (TEXT . POSITION)
	     (setq position (abs (cdr undo))))
	    ((and (consp undo) (eq (car undo) t))) ; (t HIGH . LOW)
	    ((and (consp undo) (null (car undo)))
	     ;; (nil PROPERTY VALUE BEG . END)
	     (setq position (cdr (last undo))))
	    ((and (consp undo) (markerp (car undo)))) ; (MARKER . DISTANCE)
	    ((integerp undo))		; POSITION
	    ((null undo))		; nil
	    (t (error "Invalid undo entry: %s" undo)))
      (setq undo-list (cdr undo-list)))
    (cond (position
	   (setq goto-last-change-undo undo)
	   (goto-char (min position (point-max))))
	  ((and (eq this-command last-command)
		goto-last-change-undo)
	   (setq goto-last-change-undo nil)
	   (error "No further undo information"))
	  (t
	   (setq goto-last-change-undo nil)
	   (error "Buffer not modified")))))

(provide 'utils)
;;; utils.el ends here
