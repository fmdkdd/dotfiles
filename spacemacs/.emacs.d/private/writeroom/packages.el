;;; packages.el --- Writeroom Layer packages File for Spacemacs
;;
;; Copyright (c) 2015 fmdkdd
;;
;; Author: fmdkdd
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar writeroom-packages
  '(writeroom-mode)
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar writeroom-excluded-packages '()
  "List of packages to exclude.")

(defun writeroom/init-writeroom-mode ()
  (use-package writeroom-mode
    :defer t
    :init
    (evil-leader/set-key "Tw" 'writeroom-mode)))
