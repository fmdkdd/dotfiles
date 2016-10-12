;;; keybindings.el --- Colemak HJKL Layer key bindings File
;;
;; Copyright (c) 2015 fmdkdd
;;
;; Author: fmdkdd
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Remap HJKL for Colemak layout.
;; (left . h), (down . j), (top . k)
;; becomes
;; (left . j), (down . k), (top . h)

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Override layers/distribution/spacemacs-base/keybindings.el

(spacemacs/set-leader-keys
  "iK" 'spacemacs/insert-line-below-no-indent
  "iH" 'spacemacs/insert-line-above-no-indent
  "ih" 'spacemacs/evil-insert-line-above
  "ik" 'spacemacs/evil-insert-line-below
  "ij" nil
  "iJ" nil)

;; Overload this toggle to restore the right keybindings.
(spacemacs|add-toggle visual-line-navigation
  :status visual-line-mode
  :on
  (progn
    (visual-line-mode)
    (evil-define-minor-mode-key 'motion 'visual-line-mode "k" 'evil-next-visual-line)
    (evil-define-minor-mode-key 'motion 'visual-line-mode "h" 'evil-previous-visual-line)
    (when (bound-and-true-p evil-escape-mode)
      (evil-escape-mode -1)
      (setq evil-escape-motion-state-shadowed-func nil)
      (evil-define-minor-mode-key 'motion 'visual-line-mode "k" 'evil-next-visual-line)
      (evil-define-minor-mode-key 'motion 'visual-line-mode "h" 'evil-previous-visual-line)
      (evil-escape-mode))
    (evil-normalize-keymaps))
  :off
  (progn
    (visual-line-mode -1)
    (evil-normalize-keymaps))
  :documentation "Move point according to visual lines."
  :evil-leader "tL")

(spacemacs/set-leader-keys
  "wJ" 'evil-window-move-far-left
  "wj" 'evil-window-left
  "wK" 'evil-window-move-very-bottom
  "wk" 'evil-window-down
  "wH" 'evil-window-move-very-top
  "wh" 'evil-window-up)

;; Window manipulation transient state.  To override the keybinding, we follow
;; the same process as the paste transient state in packages.el.
(add-hook 'spacemacs-post-user-config-hook
 (lambda ()
   (define-key spacemacs/window-manipulation-transient-state/keymap
     "h" 'spacemacs/window-manipulation-transient-state/evil-window-up)
   (define-key spacemacs/window-manipulation-transient-state/keymap
     "k" 'spacemacs/window-manipulation-transient-state/evil-window-down)
   (define-key spacemacs/window-manipulation-transient-state/keymap
     "j" 'spacemacs/window-manipulation-transient-state/evil-window-left)
   (define-key spacemacs/window-manipulation-transient-state/keymap
     "H" 'spacemacs/window-manipulation-transient-state/evil-window-move-very-top)
   (define-key spacemacs/window-manipulation-transient-state/keymap
     "K" 'spacemacs/window-manipulation-transient-state/evil-window-move-very-bottom)
   (define-key spacemacs/window-manipulation-transient-state/keymap
     "J" 'spacemacs/window-manipulation-transient-state/evil-window-move-far-left)

   ;; Don't forget to redefine the hint as well
   (pcase-let ((`(concat ,_ (concat (format ,_ ,j ,k ,J ,K
                                            ,_ ,_ ,_ ,h ,_ ,H . ,_)
                                    . ,_)
                         . ,_)
                spacemacs/window-manipulation-transient-state/hint))
     (store-substring j 0 "k")
     (store-substring k 0 "h")
     (store-substring J 0 "K")
     (store-substring K 0 "H")
     (store-substring h 0 "j")
     (store-substring H 0 "J")))
 ;; Run this redefinition /after/ the transient state definitions
 t)
