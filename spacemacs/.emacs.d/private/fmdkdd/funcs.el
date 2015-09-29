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

(defun fmdkdd//turn-off-truncate-lines ()
  (setq truncate-lines nil))
