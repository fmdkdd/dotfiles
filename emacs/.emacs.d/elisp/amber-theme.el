(deftheme amber "Another minimalist theme")

(custom-theme-set-faces
 'amber

 ;; UI

 '(default ((t (:foreground "#baaa93" :background "#292827"))))
 '(region ((t (:background "#393837"))))
 '(cursor ((t (:background "#9ed08c"))))
 '(vertical-border ((t (:foreground "#444342"))))
 '(fringe ((t (:background "#242322"))))

 '(mode-line ((t (:foreground "#d4c1a7" :background "#5c4a3d" :height 80))))
 '(mode-line-inactive ((t (:foreground "#baaa93" :background "#32302f" :height 80))))
 '(mode-line-buffer-id ((t (:foreground "#ebdbb2" :weight bold))))

 '(minibuffer-prompt ((t (:foreground "#ebdbb2"))))

 '(link ((t (:underline t))))
 '(escape-glyph ((t (:weight bold))))
 '(show-paren-match ((t (:foreground "#baaa93" :background "#6f6f6f" :weight bold))))
 '(show-paren-mismatch ((t (:background "#cc241d"))))

 ;; Search

 '(isearch ((t (:foreground "#000" :background "#cc66ab"))))
 '(lazy-highlight ((t (:background "#755d6d"))))
 '(anzu-mode-line ((t (:foreground "#f26dc7"))))

 ;; Syntax highlighting

 '(font-lock-comment-face ((t (:foreground "#deb530"))))
 '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
 '(font-lock-string-face ((t (:foreground "#adb08b"))))
 '(font-lock-warning-face ((t (:foreground "#fad22f" :weight bold))))
 '(font-lock-doc-face ((t (:inherit font-lock-comment-face))))

 '(font-lock-keyword-face ((t (:inherit unspecified :weight bold))))
 '(font-lock-preprocessor-face ((t (:inherit unspecified))))
 '(font-lock-builtin-face ((t (:inherit unspecified))))
 '(font-lock-constant-face ((t (:inherit unspecified))))
 '(font-lock-type-face ((t (:inherit unspecified))))
 '(font-lock-variable-name-face ((t (:inherit unspecified))))
 '(font-lock-function-name-face ((t (:inherit unspecified))))

 `(rainbow-delimiters-depth-1-face ((t (:foreground "#8a7b67"))))

 ;; Info

 '(Info-quoted ((t (:inherit font-lock-string-face))))
 '(info-node ((t (:inherit default :weight bold :slant italic))))
 '(info-xref ((t (:inherit link))))
 `(info-xref-visited ((t (:foreground "#998972" :inherit link))))
 '(info-menu-star ((t (:inherit unspecified))))

 ;; Helm
 '(helm-selection ((t (:background "#424140"))))
 '(helm-ff-directory ((t (:foreground "#adb08b"))))
 '(helm-ff-dotted-directory ((t (:foreground "#adb08b"))))
 '(helm-ff-symlink ((t (:inherit unspecified))))
 '(helm-ff-prefix ((t (:foreground "#000" :background "gold1"))))
 '(helm-buffer-size ((t (:inherit unspecified))))
 '(helm-buffer-process ((t (:inherit unspecified))))
 '(helm-candidate-number ((t (:inherit helm-grep-match))))
 '(helm-source-header ((t (:foreground "#ebdbb2" :background "#4a5363"))))
 '(helm-grep-file ((t (:foreground "#adb08b"))))
 '(helm-grep-finish ((t (:inherit unspecified))))

 ;; Flycheck

 '(flycheck-error ((t (:underline (:style wave :color "#de6a49")))))
 '(flycheck-warning ((t (:underline (:style wave :color "#decf49")))))
 '(flycheck-info ((t (:underline (:style wave :color "#6db0cf")))))

 '(flycheck-fringe-error ((t (:foreground "#de6a49"))))
 '(flycheck-fringe-warning ((t (:foreground "#decf49"))))
 '(flycheck-fringe-info ((t (:foreground "#6db0cf"))))

 '(flycheck-inline-error ((t (:inherit flycheck-fringe-error :weight normal))))
 '(flycheck-inline-warning ((t (:inherit flycheck-fringe-warning :weight normal))))
 '(flycheck-inline-info ((t (:inherit flycheck-fringe-info :weight normal))))

 ;; Org

 '(org-date ((t (:inherit unspecified))))
 '(org-link ((t (:inherit link))))

 )

(provide-theme 'amber)
