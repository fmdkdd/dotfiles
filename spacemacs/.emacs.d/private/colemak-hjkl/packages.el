;;; packages.el --- Colemak HJKL Layer packages File for Spacemacs
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

;; Remap all HJKL bindings used as movement keys in packages loaded by Spacemacs
;; or by other layers. We want to execute our remapping after the configuration
;; from other layers, so we use `spacemacs|use-package-add-hook' on
;; `:post-config'. We add these hooks in the `pre-init' phase of package
;; loading, so that they are correctly installed /before/ the packages are
;; loaded via `use-package'.

;; Using `with-eval-after-load' is not sufficient, as we do not know if the body
;; will be executed after the `:config' phase of `use-package'.

(defconst colemak-hjkl-packages
  '(evil
    helm
    company
    ;; flycheck
    ;; org
    org-agenda
    ;; evil-org
    ;; web-mode
    magit
    ))

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Override layers/distribution/spacemacs-bootstrap/packages.el

(defun colemak-hjkl/pre-init-evil ()
  ;; spacemacs|add-hook doesn't work here, because evil is `require'd in
  ;; spacemacs-bootstrap and doesn't go through use-package.
  ;; `with-eval-after-load' is not necessary either, but doesn't hurt.
  (with-eval-after-load 'evil
    ;;~~~~~~~~~~~~~~~~~~~~~~~~~~
    ;; In elpa/evil/evil-maps.el

    ;; Window commands
    (define-key evil-window-map "j" 'evil-window-left)
    (define-key evil-window-map "J" 'evil-window-move-far-left)
    (define-key evil-window-map "k" 'evil-window-down)
    (define-key evil-window-map "K" 'evil-window-move-very-bottom)
    (define-key evil-window-map "h" 'evil-window-up)
    (define-key evil-window-map "H" 'evil-window-move-very-top)
    (define-key evil-window-map (kbd "C-S-j") 'evil-window-move-far-left)
    (define-key evil-window-map (kbd "C-S-k") 'evil-window-move-very-bottom)
    (define-key evil-window-map (kbd "C-S-h") 'evil-window-move-very-top)

    ;; Motion state
    (define-key evil-motion-state-map "j" 'evil-backward-char)
    (define-key evil-motion-state-map "k" 'evil-next-line)
    (define-key evil-motion-state-map "h" 'evil-previous-line)

    ;; Leave evil-window-top on H, but use K for evil-window-bottom. That leaves
    ;; L for evil-lookup.
    (define-key evil-motion-state-map "L" 'evil-lookup)
    (define-key evil-motion-state-map "K" 'evil-window-bottom)

    (define-key evil-motion-state-map "gk" 'evil-next-visual-line)
    (define-key evil-motion-state-map "gh" 'evil-previous-visual-line)
    (define-key evil-motion-state-map "gj" nil)

    (define-key evil-motion-state-map "zh" nil)
    (define-key evil-motion-state-map "zH" nil)
    (define-key evil-motion-state-map "zj" 'evil-scroll-column-left)
    (define-key evil-motion-state-map "zJ" 'evil-scroll-left)

    ;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ;; In elpa/evil/evil-integration.el

    (evil-add-hjkl-bindings Info-mode-map 'motion
      "0" 'evil-digit-argument-or-evil-beginning-of-line
      (kbd "M-h") 'Info-help    ; "h"
      "\C-t" 'Info-history-back ; "l"
      "\C-o" 'Info-history-back
      "x" 'Info-scroll-up ; SPC is taken in Spacemacs
      "X" 'Info-scroll-down ; might as well
      "\C-]" 'Info-follow-nearest-node
      (kbd "DEL") 'Info-scroll-down)))

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; In layers/distribution/spacemacs-bootstrap/packages.el

;; Use post-init to override the bindings made by spacemacs-bootstrap
(defun colemak-hjkl/post-init-evil ()
  (define-key evil-normal-state-map "L" 'spacemacs/evil-smart-doc-lookup)
  (define-key evil-normal-state-map "K" nil)

  ;; Transient states are defined in this hook, so we /append/ to it in order to
  ;; run this redefinition after the transient states are defined, otherwise the
  ;; keymap and hint variables won't be defined.
  (add-hook 'spacemacs-post-user-config-hook
            (lambda ()
              (define-key spacemacs/paste-transient-state/keymap (kbd "C-j") nil)
              (define-key spacemacs/paste-transient-state/keymap
                (kbd "C-h") 'spacemacs/paste-transient-state/evil-paste-pop)

              ;; The hint is a bit convoluted, but we can pattern match the
              ;; place we want.
              (pcase spacemacs/paste-transient-state/hint
                (`(concat ,_ (concat (format ,_ ,_ ,_ ,str . ,_) . ,_) . ,_)
                 (store-substring str 0 "C-h"))))
            ;; Run this redefinition /after/ the transient state definitions
            t)

  ;; comint
  (evil-define-key 'insert comint-mode-map
    (kbd "C-h") 'comint-previous-input
    (kbd "C-k") 'comint-next-input
    (kbd "C-j") nil)
  )

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; In layers/distribution/spacemacs-base/local/evil-evilified-state
(with-eval-after-load 'evil-evilified-state
  (define-key evil-evilified-state-map-original "j" 'evil-backward-char)
  (define-key evil-evilified-state-map-original "k" 'evil-next-visual-line)
  (define-key evil-evilified-state-map-original "h" 'evil-previous-visual-line))


;; The helm layer is implicitly loaded with the spacemacs distribution
(defun colemak-hjkl/pre-init-helm ()
  (spacemacs|use-package-add-hook helm
    :post-config
    ;; need this otherwise helm-find-files-map is not defined
    (helm-mode +1)

    ;; from layers/+spacemacs/spacemacs-completion/funcs.el
    ;; helm navigation on hjkl
    (define-key helm-map (kbd "C-k") 'helm-next-line)
    (define-key helm-map (kbd "C-h") 'helm-previous-line)
    (define-key helm-map (kbd "C-j") 'helm-next-source)
    (dolist (keymap (list helm-find-files-map helm-read-file-map))
      (define-key keymap (kbd "C-l") 'helm-execute-persistent-action)
      (define-key keymap (kbd "C-j") 'helm-find-files-up-one-level)
      (define-key keymap (kbd "C-h") nil))

    ;; from layers/+spacemacs/spacemacs-completion/packages.el
    ;; Yet another transient state, yeepee
    (add-hook
     'spacemacs-post-user-config-hook
     (lambda ()
       (let ((keymap spacemacs/helm-navigation-transient-state/keymap)
             (prefix "spacemacs/helm-navigation-transient-state"))
         (define-key keymap "h" (intern (format "%s/%s" prefix 'helm-previous-line)))
         (define-key keymap "j" (intern (format "%s/%s" prefix 'helm-previous-source)))
         (define-key keymap "k" (intern (format "%s/%s" prefix 'helm-next-line)))

         ;; and the hint
         (pcase-let ((`(concat ,_ (concat (format ,_ ,j ,k ,_ ,_ ,h . ,_)
                                          . ,_)
                               . ,_)
                      spacemacs/helm-navigation-transient-state/hint))
           (store-substring j 0 "h")
           (store-substring k 0 "k")
           (store-substring h 0 "j"))))
     t)))

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Override layers/auto-completion/packages.el

(defun colemak-hjkl/pre-init-company ()
  (spacemacs|use-package-add-hook company
    :post-config
    (define-key company-active-map (kbd "C-k") 'company-select-next)
    (define-key company-active-map (kbd "C-h") 'company-select-previous)
    (define-key company-active-map (kbd "C-j") 'company-show-doc-buffer)))

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Override layers/org/packages.el

;; (defun colemak-hjkl/pre-init-org ()
;;   (spacemacs|use-package-add-hook org
;;     :post-config
;;     (spacemacs/set-leader-keys-for-major-mode 'org-mode
;;       ;; More cycling options (timestamps, headlines, items, properties)
;;       "J" 'org-shiftleft
;;       "K" 'org-shiftdown
;;       "H" 'org-shiftup

;;       ;; Change between TODO sets
;;       "C-S-j" 'org-shiftcontrolleft
;;       "C-S-k" 'org-shiftcontroldown
;;       "C-S-h" 'org-shiftcontrolup

;;       ;; Subtree editing
;;       "Sl" 'org-demote-subtree
;;       "Sj" 'org-promote-subtree
;;       "Sk" 'org-move-subtree-down
;;       "Sh" 'org-move-subtree-up

;;       ;; tables
;;       "tj" 'org-table-previous-field
;;       "tJ" 'org-table-move-column-left
;;       "tk" 'org-table-next-row
;;       "tK" 'org-table-move-row-down
;;       "tH" 'org-table-move-row-up)

;;     ;; Evilify the calendar tool on C-c .
;;     (unless (eq 'emacs dotspacemacs-editing-style)
;;       (define-key org-read-date-minibuffer-local-map (kbd "M-j")
;;         (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-day 1))))
;;       (define-key org-read-date-minibuffer-local-map (kbd "M-h")
;;         (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-week 1))))
;;       (define-key org-read-date-minibuffer-local-map (kbd "M-k")
;;         (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-week 1))))
;;       (define-key org-read-date-minibuffer-local-map (kbd "M-J")
;;         (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-month 1))))
;;       (define-key org-read-date-minibuffer-local-map (kbd "M-H")
;;         (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-year 1))))
;;       (define-key org-read-date-minibuffer-local-map (kbd "M-K")
;;         (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-year 1)))))))


(defun colemak-hjkl/pre-init-org-agenda ()
  (spacemacs|use-package-add-hook org-agenda
    :post-config
    (evil-define-key 'evilified org-agenda-mode-map
      "k" 'org-agenda-next-line
      "h" 'org-agenda-previous-line
      "j" nil
      (kbd "M-k") 'org-agenda-next-item
      (kbd "M-h") 'org-agenda-previous-item
      (kbd "M-j") 'org-agenda-earlier)))

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Override elpa/evil-org/evil.org.el

;; (defun colemak-hjkl/pre-init-evil-org ()
;;   (spacemacs|use-package-add-hook evil-org
;;     :post-config
;;     (evil-define-key 'normal evil-org-mode-map
;;       "gj" 'outline-up-heading
;;       "gk" 'org-forward-heading-same-level
;;       "gh" 'org-backward-heading-same-level
;;       (kbd "M-j") 'org-metaleft
;;       (kbd "M-h") 'org-metaup
;;       (kbd "M-k") 'org-metadown
;;       (kbd "M-J") 'org-shiftmetaleft
;;       (kbd "M-H") 'org-shiftmetaup
;;       (kbd "M-K") 'org-shiftmetadown)

;;     (evil-define-key 'insert evil-org-mode-map
;;       (kbd "M-j") 'org-metaleft
;;       (kbd "M-h") 'org-metaup
;;       (kbd "M-k") 'org-metadown
;;       (kbd "M-J") 'org-shiftmetaleft
;;       (kbd "M-H") 'org-shiftmetaup
;;       (kbd "M-K") 'org-shiftmetadown)))

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Override layers/+lang/html/packages.el

;; (defun colemak-hjkl/pre-init-web-mode ()
;;   (spacemacs|use-package-add-hook web-mode
;;     :post-config
;;     (defun colemak-hjkl//web-mode-ms-doc ()
;;       (if (equal 0 spacemacs--web-mode-ms-doc-toggle)
;;           "[?] for help"
;;         "
;;   [?] display this help
;;   [h] previous [k] next   [H] previous sibling [K] next sibling
;;   [h] parent   [l] child  [c] clone [d] delete [D] kill [r] rename
;;   [w] wrap     [p] xpath
;;   [q] quit"))

;;     (spacemacs|define-micro-state web-mode
;;       :doc (colemak-hjkl//web-mode-ms-doc)
;;       :persistent t
;;       :evil-leader-for-mode (web-mode . ".")
;;       :bindings
;;       ("<escape>" nil :exit t)
;;       ("?" spacemacs//web-mode-ms-toggle-doc)
;;       ("c" web-mode-element-clone)
;;       ("d" web-mode-element-vanish)
;;       ("D" web-mode-element-kill)
;;       ("k" web-mode-element-next)
;;       ("K" web-mode-element-sibling-next)
;;       ("gk" web-mode-element-sibling-next)
;;       ("h" web-mode-element-previous)
;;       ("H" web-mode-element-sibling-previous)
;;       ("gh" web-mode-element-sibling-previous)
;;       ("j" web-mode-element-parent)
;;       ("l" web-mode-element-child)
;;       ("p" web-mode-dom-xpath)
;;       ("r" web-mode-element-rename :exit t)
;;       ("q" nil :exit t)
;;       ("w" web-mode-element-wrap))))

;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Override layers/+source-control/git/packages.el

(defun colemak-hjkl/pre-init-magit ()
  (spacemacs|use-package-add-hook magit
    :post-config
    (evil-define-key 'evilified git-rebase-mode-map "H" 'git-rebase-move-line-up)
    (evil-define-key 'evilified git-rebase-mode-map "K" 'git-rebase-move-line-down)
    (evil-define-key 'evilified git-rebase-mode-map "J" nil)

    ;; Hunk map is for visual selection of hunks.  k is bound to `magit-discard'
    ;; by default, but since we already have it on K, leave k to `evil-next-line'.
    (dolist (map (list magit-hunk-section-map
                       magit-file-section-map
                       magit-untracked-section-map
                       magit-unstaged-section-map
                       magit-staged-section-map
                       magit-branch-section-map))
      (define-key map "k" nil))

    ;; l is useless in magit, but log-popup is useful
    (dolist (map (list magit-hunk-section-map
                       magit-file-section-map
                       magit-untracked-section-map
                       magit-unstaged-section-map
                       magit-staged-section-map
                       magit-branch-section-map))
      (evil-define-key 'evilified map "l" nil))
    (evil-define-key 'evilified magit-mode-map "l" 'magit-log-popup)))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Override layers/+syntax-checking/packages.el

;; (defun colemak-hjkl/pre-init-flycheck ()
;;   (spacemacs|use-package-add-hook flycheck
;;     :post-config

;;     (defun colemak-hjkl/toggle-flycheck-error-list ()
;;       "Toggle flycheck's error list window.
;; If the error list is visible, hide it.  Otherwise, show it."
;;       (interactive)
;;       (-if-let (window (flycheck-get-error-list-window))
;;           (quit-window nil window)
;;         (flycheck-list-errors)))

;;     (evilified-state-evilify-map flycheck-error-list-mode-map
;;       :mode flycheck-error-list-mode
;;       :bindings
;;       "RET" 'flycheck-error-list-goto-error
;;       "k" 'flycheck-error-list-next-error
;;       "h" 'flycheck-error-list-previous-error)

;;     ;; key bindings
;;     (spacemacs/set-leader-keys
;;       "el" 'spacemacs/toggle-flycheck-error-list)))
