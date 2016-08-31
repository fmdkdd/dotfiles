(deftheme zenrub "A minimalist theme")

(let* (
       ;; Mostly a triad palette from http://paletton.com
       (zenrub-a0 "#d8b76c")
       (zenrub-a1 "#b08c38")
       (zenrub-a2 "#906b19")
       (zenrub-a3 "#6c4c02")
       (zenrub-a4 "#493300")

       (zenrub-c0 "#91b8b4")
       (zenrub-c1 "#639893")
       (zenrub-c2 "#41837c")
       (zenrub-c3 "#226b63")
       (zenrub-c4 "#0d443d")

       (zenrub-d0 "#9fa6b3")
       (zenrub-d1 "#737e96")
       (zenrub-d2 "#4f5d7f")
       (zenrub-d3 "#36455c")
       (zenrub-d4 "#1a2a31")

       (zenrub-green "#8fd488")
       (zenrub-red "#d75e4e")
       (zenrub-yellow "#d7c84e")

       ;; Extra colors
       (zenrub-error zenrub-red)
       (zenrub-warning zenrub-yellow))
  (custom-theme-set-faces
   'zenrub

   ;; Main text
   `(default ((t (:foreground ,zenrub-d0 :background ,zenrub-d4))))
   `(region ((t (:background ,zenrub-d2))))
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,zenrub-d2))))
   `(show-paren-match ((t (:foreground ,zenrub-green))))
   `(sp-pair-overlay-face ((t (:inherit unspecified))))

   `(error ((t (:foreground ,zenrub-error :weight bold))))
   `(warning ((t (:foreground ,zenrub-warning :weight bold))))
   `(success ((t (:foreground ,zenrub-c0 :weight bold))))
   `(link ((t (:underline t))))
   `(highlight ((t (:background ,zenrub-d3))))

   ;; Mute those colors
   `(font-lock-keyword-face ((t (:weight bold))))
   `(font-lock-function-name-face ((t (:inherit unspecified))))
   `(font-lock-constant-face ((t (:inherit unspecified))))
   `(font-lock-builtin-face ((t (:inherit unspecified))))
   `(font-lock-type-face ((t (:inherit unspecified))))
   `(font-lock-variable-name-face ((t (:inherit unspecified))))
   `(font-lock-warning-face ((t (:inherit warning))))

   ;; Here we want some color
   `(font-lock-comment-face ((t (:foreground ,zenrub-a0 :background ,zenrub-d4))))
   `(font-lock-doc-face ((t (:inherit font-lock-comment-face))))
   `(font-lock-string-face ((t (:foreground ,zenrub-c0))))

   ;; Search
   `(isearch ((t (:background ,zenrub-green
                              :foreground ,zenrub-d3))))

   ;; Helm
   `(helm-selection ((t (:inherit highlight))))
   `(helm-ff-directory ((t (:foreground ,zenrub-a0))))
   `(helm-swoop-target-word-face ((t (:background ,zenrub-green
                                                  :foreground ,zenrub-d2))))
   `(helm-swoop-target-line-face ((t (:background ,zenrub-d3))))

   ;; UI
   `(fringe ((t (:inherit unspecified))))
   `(mode-line ((t (:background ,zenrub-d3
                                :box (:line-width -1 :style released-button)))))
   `(mode-line-buffer-id ((t (:inherit unspecified))))
   `(mode-line-inactive ((t (:foreground ,zenrub-d1
                                         :box (:line-width -1 :color ,zenrub-d3)))))

   `(powerline-active1 ((t (:background ,zenrub-d4))))
   `(powerline-active2 ((t (:background ,zenrub-d4))))
   `(powerline-inactive1 ((t (:background ,zenrub-d4))))
   `(powerline-inactive2 ((t (:background ,zenrub-d4))))

   `(minibuffer-prompt ((t (:inherit unspecified))))

   ;; Company
   `(company-tooltip ((t (:background ,zenrub-d3))))
   `(company-tooltip-selection ((t (:background ,zenrub-d4))))
   `(company-tooltip-annotation ((t (:foreground ,zenrub-a0))))
   `(company-scrollbar-fg ((t (:background ,zenrub-d4))))
   `(company-scrollbar-bg ((t (:background ,zenrub-d3))))

   ;; Org mode
   `(outline-1 ((t (:foreground ,zenrub-c0))))
   `(outline-2 ((t (:foreground ,zenrub-c1))))
   `(outline-3 ((t (:foreground ,zenrub-c2))))
   `(outline-4 ((t (:foreground ,zenrub-c3))))

   `(org-meta-line ((t (:foreground ,zenrub-a1))))
   `(org-document-title ((t (:inherit org-meta-line))))
   `(org-document-info-keyword ((t (:inherit org-meta-line))))
   `(org-document-info ((t (:inherit org-meta-line))))
   `(org-done ((t (:inherit success))))
   `(org-todo ((t (:inherit error))))
   `(org-date ((t (:inherit unspecified))))
   `(org-code ((t (:inherit unspecified))))
   `(org-table ((t (:inherit unspecified))))
   `(org-block ((t (:inherit unspecified))))
   `(org-macro ((t (:inherit unspecified)))) ; I don't hide braces around macros

   ;; flyspell
   `(flyspell-incorrect ((t :underline (:style wave :color ,zenrub-error))))
   `(flyspell-duplicate ((t :underline (:style wave :color ,zenrub-warning))))

   ;; flycheck
   `(flycheck-error ((t (:underline (:style wave :color ,zenrub-error)))))
   `(flycheck-fringe-error ((t (:foreground ,zenrub-error))))
   `(spaceline-flycheck-error ((t (:foreground ,zenrub-error))))

   `(flycheck-warning ((t (:underline (:style wave :color ,zenrub-warning)))))
   `(flycheck-fringe-warning ((t (:foreground ,zenrub-warning))))
   `(spaceline-flycheck-warning ((t (:foreground ,zenrub-warning))))

   `(flycheck-info ((t (:underline (:style wave :color ,zenrub-c0)))))
   `(flycheck-fringe-info ((t (:foreground ,zenrub-c0))))
   `(spaceline-flycheck-info ((t (:foreground ,zenrub-c0))))

   ;; spacemacs modes
   `(spacemacs-normal-face ((t (:background "DarkGoldenrod2"
                                            :foreground ,zenrub-d4))))
   `(spacemacs-insert-face ((t (:background "chartreuse3"
                                            :foreground ,zenrub-d4))))
   `(spacemacs-iedit-face ((t (:background "firebrick1"
                                           :foreground ,zenrub-d4))))
   `(spacemacs-iedit-insert-face ((t (:background "firebrick1"
                                                  :foreground ,zenrub-d4))))
   `(spacemacs-lisp-face ((t (:background "HotPink1"
                                          :foreground ,zenrub-d4))))
   `(spacemacs-motion-face ((t (:background "plum3"
                                            :foreground ,zenrub-d4))))
   `(spacemacs-visual-face ((t (:background "gray"
                                            :foreground ,zenrub-d4))))
   `(spacemacs-evilified-face ((t (:background "LightGoldenrod3"
                                               :foreground ,zenrub-d4))))
   `(spacemacs-replace-face ((t (:background "chocolate"
                                             :foreground ,zenrub-d4))))
   `(spacemacs-hybrid-face ((t (:background "SkyBlue2"
                                            :foreground ,zenrub-d4))))
   `(spacemacs-emacs-face ((t (:background "SkyBlue2"
                                           :foreground ,zenrub-d4))))

   ;; Info
   `(info-string ((t (:inherit unspecified))))
   `(info-quoted-name ((t (:inherit font-lock-string-face))))
   `(info-double-quoted-name ((t (:inherit unspecified))))
   `(info-reference-item ((t (:foreground ,zenrub-c0))))
   `(info-function-ref-item ((t (:inherit unspecified))))
   `(info-macro-ref-item ((t (:inherit unspecified))))
   `(info-xref-visited ((t (:foreground ,zenrub-d1 :inherit link))))
   `(info-menu-star ((t (:inherit unspecified))))

   ;; js2
   `(js2-function-param ((t (:inherit unspecified))))
   `(js2-error ((t (:foreground ,zenrub-error))))

   ;; Elisp
   `(eval-sexp-fu-flash ((t (:background ,zenrub-c4))))
   `(eval-sexp-fu-flash-error ((t (:background ,zenrub-error))))

   ))

(provide-theme 'zenrub)
