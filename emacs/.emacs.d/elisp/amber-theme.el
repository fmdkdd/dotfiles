(deftheme amber "Another minimalist theme")

(custom-theme-set-faces
 'amber

 ;; UI

 '(default ((t (:foreground "#baaa93" :background "#272829"))))
 '(region ((t (:background "#50515e"))))
 '(cursor ((t (:background "#9ed08c"))))
 '(vertical-border ((t (:foreground "#444"))))
 '(fringe ((t (:background "#242424"))))

 '(mode-line ((t (:foreground "#d4c1a7" :background "#5c4a3d" :height 80))))
 '(mode-line-inactive ((t (:foreground "#baaa93" :background "#32302f" :height 80))))
 '(mode-line-buffer-id ((t (:foreground "#ebdbb2" :weight bold))))

 '(minibuffer-prompt ((t (:foreground "#ebdbb2"))))

 '(link ((t (:underline t))))
 '(escape-glyph ((t (:weight bold))))
 '(show-paren-match ((t (:foreground "#baaa93" :background "#6f6f6f" :weight bold))))
 '(show-paren-mismatch ((t (:background "#cc241d"))))

 '(shadow ((t (:foreground "#8a7b67"))))

 ;; Search

 '(isearch ((t (:foreground "#000" :background "#cc66ab"))))
 '(lazy-highlight ((t (:background "#755d6d"))))
 '(anzu-mode-line ((t (:inherit unspecified))))

 ;; Syntax highlighting

 '(font-lock-comment-face ((t (:foreground "#edc133"))))
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

 '(rainbow-delimiters-depth-1-face ((t (:inherit shadow))))

 ;; Info

 '(Info-quoted ((t (:inherit font-lock-string-face))))
 '(info-node ((t (:inherit default :weight bold :slant italic))))
 '(info-xref ((t (:inherit link))))
 '(info-xref-visited ((t (:foreground "#998972" :inherit link))))
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
 '(helm-moccur-buffer ((t (:foreground "#adb08b"))))

 ;; Flycheck

 '(flycheck-error ((t (:underline (:style wave :color "#de6a49")))))
 '(flycheck-warning ((t (:underline (:style wave :color "#decf49")))))
 '(flycheck-info ((t (:underline (:style wave :color "#6db0cf")))))

 '(flycheck-fringe-error ((t (:foreground "#de6a49"))))
 '(flycheck-fringe-warning ((t (:foreground "#decf49"))))
 '(flycheck-fringe-info ((t (:foreground "#6db0cf"))))

 '(flycheck-error-list-error ((t (:inherit flycheck-fringe-error))))
 '(flycheck-error-list-warning ((t (:inherit flycheck-fringe-warning))))
 '(flycheck-error-list-info ((t (:inherit flycheck-fringe-info))))
 '(flycheck-error-list-highlight ((t (:inherit helm-selection))))

 '(flycheck-inline-error ((t (:inherit flycheck-fringe-error :weight normal))))
 '(flycheck-inline-warning ((t (:inherit flycheck-fringe-warning :weight normal))))
 '(flycheck-inline-info ((t (:inherit flycheck-fringe-info :weight normal))))

 ;; Org

 '(outline-1 ((t (:foreground "#a0ba93"))))
 '(outline-2 ((t (:foreground "#93baa7"))))
 '(outline-3 ((t (:foreground "#93adba"))))
 '(outline-4 ((t (:foreground "#9a93ba"))))

 '(org-link ((t (:inherit link))))
 '(org-meta-line ((t (:inherit shadow))))
 '(org-done ((t (:foreground "#8ec07c" :weight bold))))
 '(org-todo ((t (:foreground "#fe8019" :weight bold))))


 '(org-date ((t (:inherit unspecified))))
 '(org-code ((t (:inherit unspecified))))
 '(org-verbatim ((t (:inherit unspecified))))
 '(org-block ((t (:inherit unspecified))))
 '(org-table ((t (:inherit unspecified))))
 '(org-block ((t (:inherit unspecified))))
 '(org-macro ((t (:inherit unspecified)))) ; I don't hide braces around macros

 ;; Magit

 '(magit-popup-option-value ((t (:weight bold))))

 ;; smerge (magit in-buffer diff)
 '(smerge-refined-added ((t (:background "#336633"))))
 '(smerge-refined-removed ((t (:background "#803939"))))

 ;; Markdown

 '(markdown-blockquote-face ((t (:inherit unspecified))))
 '(markdown-code-face ((t (:inherit unspecified))))
 '(markdown-reference-face ((t (:inherit unspecified))))
 '(markdown-link-title-face ((t (:inherit unspecified))))

 ;; Rust

 '(rust-string-interpolation-face ((t (:inherit font-lock-string-face))))

 )

(provide-theme 'amber)
