;;; funcs.el --- Colemak HJKL Layer key bindings File
;;
;; Copyright (c) 2015 fmdkdd
;;
;; Author: fmdkdd
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Remap HJKL for Colemak layout.
;; (h, left), (j, down), (k, top)
;; becomes
;; (j, left), (k, down), (h, top)
(define-key evil-normal-state-map "h" 'evil-previous-visual-line)
(define-key evil-normal-state-map "k" 'evil-next-visual-line)
(define-key evil-normal-state-map "j" 'nil)
(define-key evil-visual-state-map "h" 'evil-previous-visual-line)
(define-key evil-visual-state-map "k" 'evil-next-visual-line)
(define-key evil-visual-state-map "j" 'nil)
(define-key evil-normal-state-map "K" 'nil)
(define-key evil-normal-state-map "L" 'spacemacs/smart-doc-lookup)
(define-key evil-motion-state-map "K" 'evil-window-bottom)
(define-key evil-motion-state-map "L" 'evil-lookup)
(define-key evil-motion-state-map "h" 'evil-previous-line)
(define-key evil-motion-state-map "k" 'evil-next-line)
(define-key evil-motion-state-map "j" 'evil-backward-char)
(define-key evil-motion-state-map "gh" 'evil-previous-visual-line)
(define-key evil-motion-state-map "gk" 'evil-next-visual-line)
(define-key evil-motion-state-map "gj" 'nil)
(define-key evil-motion-state-map "zh" 'nil)
(define-key evil-motion-state-map "zj" 'evil-scroll-column-left)
(define-key evil-motion-state-map "zh" 'nil)
(define-key evil-motion-state-map "zJ" 'evil-scroll-left)
(define-key evil-evilified-state-map "h" 'evil-previous-visual-line)
(define-key evil-evilified-state-map "k" 'evil-next-visual-line)
(define-key evil-evilified-state-map "j" 'evil-backward-char)

(evil-leader/set-key
  "jh" 'evil-goto-next-line-and-indent
  "jk" 'sp-newline
  "jj" 'spacemacs/push-mark-and-goto-beginning-of-line
  "jl" 'spacemacs/push-mark-and-goto-end-of-line)

(evil-leader/set-key
  "wH" 'evil-window-move-very-top
  "wh" 'evil-window-up
  "wK" 'evil-window-move-very-bottom
  "wk" 'evil-window-down
  "wJ" 'evil-window-move-far-left
  "wj" 'evil-window-left
  "wL" 'evil-window-move-far-right
  "wl" 'evil-window-right)

(define-key evil-window-map "h" 'evil-window-up)
(define-key evil-window-map "H" 'evil-window-move-very-top)
(define-key evil-window-map "k" 'evil-window-down)
(define-key evil-window-map "K" 'evil-window-move-very-bottom)
(define-key evil-window-map "j" 'evil-window-left)
(define-key evil-window-map "J" 'evil-window-move-far-left)
(define-key evil-window-map "\C-h" 'evil-window-up)
(define-key evil-window-map (kbd "C-S-h") 'evil-window-move-very-top)
(define-key evil-window-map "\C-k" 'evil-window-down)
(define-key evil-window-map (kbd "C-S-k") 'evil-window-move-very-bottom)
(define-key evil-window-map "\C-j" 'evil-window-left)
(define-key evil-window-map (kbd "C-S-j") 'evil-window-move-far-left)

(defun colemak-hjkl//window-manipulation-full-doc ()
  "Full documentation for window manipulation micro-state."
  "
    [?]                       display this help
    [0,9]                     go to numbered window
    [-] [/] [s] [v] [S] [V]   split windows bellow|right and focus
    [c] [C]                   close current|other windows
    [g]                       toggle golden-ratio
    [j] [k] [h] [l]           go to left|bottom|top|right
    [J] [K] [H] [L]           move windows to far/very left|bottom|top|right
    [[] []] [{] [}]           shrink/enlarge horizontaly and verticaly respectively
    [o] [w]                   other frame|window
    [R]                       rotate windows
    [u] [U]                   restore previous|next window layout")

(defun colemak-hjkl//window-manipulation-move-doc ()
  "Help string for moving between windows"
  (concat "[j] [k] [h] [l] to move focus, "
          "[j] [k] [h] [L] to move window, "
          "[R]otate windows, other [f]rame, other [w]indow"))

(spacemacs|define-micro-state window-manipulation/colemak-hjkl
  :doc "[?] for help"
  :evil-leader "w."
  :bindings
  ("?" nil                                   :doc (colemak-hjkl//window-manipulation-full-doc))
  ("0" select-window-0                       :doc (colemak-hjkl//window-manipulation-move-doc))
  ("1" select-window-1                       :doc (colemak-hjkl//window-manipulation-move-doc))
  ("2" select-window-2                       :doc (colemak-hjkl//window-manipulation-move-doc))
  ("3" select-window-3                       :doc (colemak-hjkl//window-manipulation-move-doc))
  ("4" select-window-4                       :doc (colemak-hjkl//window-manipulation-move-doc))
  ("5" select-window-5                       :doc (colemak-hjkl//window-manipulation-move-doc))
  ("6" select-window-6                       :doc (colemak-hjkl//window-manipulation-move-doc))
  ("7" select-window-7                       :doc (colemak-hjkl//window-manipulation-move-doc))
  ("8" select-window-8                       :doc (colemak-hjkl//window-manipulation-move-doc))
  ("9" select-window-9                       :doc (colemak-hjkl//window-manipulation-move-doc))
  ("-" split-window-below-and-focus          :doc (spacemacs//window-manipulation-split-doc))
  ("/" split-window-right-and-focus          :doc (spacemacs//window-manipulation-split-doc))
  ("[" spacemacs/shrink-window-horizontally  :doc (spacemacs//window-manipulation-resize-doc))
  ("]" spacemacs/enlarge-window-horizontally :doc (spacemacs//window-manipulation-resize-doc))
  ("{" spacemacs/shrink-window               :doc (spacemacs//window-manipulation-resize-doc))
  ("}" spacemacs/enlarge-window              :doc (spacemacs//window-manipulation-resize-doc))
  ("c" delete-window                         :doc (spacemacs//window-manipulation-layout-doc))
  ("C" delete-other-windows                  :doc (spacemacs//window-manipulation-layout-doc))
  ("g" spacemacs/toggle-golden-ratio         :doc (spacemacs//window-manipulation-gratio-doc))
  ("j" evil-window-left                      :doc (colemak-hjkl//window-manipulation-move-doc))
  ("k" evil-window-down                      :doc (colemak-hjkl//window-manipulation-move-doc))
  ("h" evil-window-up                        :doc (colemak-hjkl//window-manipulation-move-doc))
  ("l" evil-window-right                     :doc (colemak-hjkl//window-manipulation-move-doc))
  ("J" evil-window-move-far-left             :doc (colemak-hjkl//window-manipulation-move-doc))
  ("K" evil-window-move-very-bottom          :doc (colemak-hjkl//window-manipulation-move-doc))
  ("H" evil-window-move-very-top             :doc (colemak-hjkl//window-manipulation-move-doc))
  ("L" evil-window-move-far-right            :doc (colemak-hjkl//window-manipulation-move-doc))
  ("o" other-frame                           :doc (colemak-hjkl//window-manipulation-move-doc))
  ("R" rotate-windows                        :doc (colemak-hjkl//window-manipulation-move-doc))
  ("s" split-window-below                    :doc (spacemacs//window-manipulation-split-doc))
  ("S" split-window-below-and-focus          :doc (spacemacs//window-manipulation-split-doc))
  ("u" winner-undo                           :doc (spacemacs//window-manipulation-layout-doc))
  ("U" winner-redo                           :doc (spacemacs//window-manipulation-layout-doc))
  ("v" split-window-right                    :doc (spacemacs//window-manipulation-split-doc))
  ("V" split-window-right-and-focus          :doc (spacemacs//window-manipulation-split-doc))
  ("w" other-window                          :doc (colemak-hjkl//window-manipulation-move-doc)))

(when (configuration-layer/layer-usedp 'org)
  (evil-define-key 'normal evil-org-mode-map
    "gh" 'org-backward-heading-same-level
    "gj" 'outline-up-heading
    "gk" 'org-forward-heading-same-level)

  (evil-define-key 'normal evil-org-mode-map
    (kbd "M-h") 'org-metaup
    (kbd "M-k") 'org-metadown
    (kbd "M-j") 'org-metaleft
    (kbd "M-H") 'org-shiftmetaup
    (kbd "M-K") 'org-shiftmetadown
    (kbd "M-J") 'org-shiftmetaleft)

  (evil-define-key 'insert evil-org-mode-map
    (kbd "M-h") 'org-metaup
    (kbd "M-k") 'org-metadown
    (kbd "M-j") 'org-metaleft
    (kbd "M-H") 'org-shiftmetaup
    (kbd "M-K") 'org-shiftmetadown
    (kbd "M-J") 'org-shiftmetaleft)

  (eval-after-load "org-agenda"
    '(progn
       (define-key org-agenda-mode-map "j" 'nil)
       (define-key org-agenda-mode-map "h" 'org-agenda-previous-line)
       (define-key org-agenda-mode-map "k" 'org-agenda-next-line)
       )))
