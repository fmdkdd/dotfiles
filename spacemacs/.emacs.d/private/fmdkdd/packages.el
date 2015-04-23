;;; packages.el --- fmdkdd Layer packages File for Spacemacs
;;
;; Copyright (c) 2015 fmdkdd
;;
;; Author: fmdkdd
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar fmdkdd-packages
  '(rainbow-mode                        ; CSS colors preview
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar fmdkdd-excluded-packages '()
  "List of packages to exclude.")

(defun fmdkdd/init-rainbow-mode ()
  (use-package rainbow-mode
    :defer t
    :init (add-hook 'css-mode-hook 'rainbow-mode)))
