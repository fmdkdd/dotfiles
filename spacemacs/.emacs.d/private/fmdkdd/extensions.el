;;; extensions.el --- fmdkdd Layer extensions File for Spacemacs
;;
;; Copyright (c) 2015 fmdkdd
;;
;; Author: fmdkdd
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar fmdkdd-pre-extensions '()
  "List of all extensions to load before the packages.")

(defvar fmdkdd-post-extensions
  '(smart-quotes)
  "List of all extensions to load after the packages.")

(defun fmdkdd/init-smart-quotes ()
  (use-package smart-quotes
    :commands (turn-on-smart-quotes
               turn-off-smart-quotes)
    :init
    (add-hook 'text-mode-hook 'turn-on-smart-quotes)
    (add-hook 'html-mode-hook 'turn-off-smart-quotes)
    :config
    (diminish 'smart-quotes-mode)))
