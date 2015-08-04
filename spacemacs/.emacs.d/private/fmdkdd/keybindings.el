;;; funcs.el --- fmdkdd Layer key bindings File
;;
;; Copyright (c) 2015 fmdkdd
;;
;; Author: fmdkdd
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(evil-leader/set-key
  "oa" 'org-agenda
  "oc" 'org-capture
  "ol" 'org-store-link)

(evil-leader/set-key-for-mode 'org-mode
  "c" nil "mc" 'fmdkdd/org-reftex-citation
  "mv" 'fmdkdd/org-view-paper)

(evil-define-key 'normal evil-org-mode-map
  "g<" 'org-previous-link
  "g>" 'org-next-link)
