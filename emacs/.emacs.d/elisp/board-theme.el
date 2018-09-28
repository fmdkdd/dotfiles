(deftheme board "A minimalist theme for use with projectors")

(custom-theme-set-faces
 'board

 ;; UI

 '(default ((t (:foreground "#000" :background "#eeebea"))))
 '(region ((t (:background "#e0e1ee"))))
 '(cursor ((t (:background "#8d6"))))
 '(vertical-border ((t (:foreground "#444"))))
 '(fringe ((t (:background "#eeebea"))))

 '(mode-line ((t (:foreground "#fed" :background "#929" :height 90))))
 '(mode-line-inactive ((t (:foreground "#eee" :background "#888" :height 90))))
 '(mode-line-buffer-id ((t (:foreground "#fed" :weight bold))))

 '(minibuffer-prompt ((t (:foreground "#000"))))

 '(link ((t (:underline t))))
 '(escape-glyph ((t (:weight bold))))
 '(show-paren-match ((t (:foreground "#444" :background "#ccc" :weight bold))))
 '(show-paren-mismatch ((t (:foreground "#ddd" :background "#cc241d" :weight bold))))

 '(shadow ((t (:foreground "#777"))))

 ;; Search

 '(isearch ((t (:foreground "#000" :background "#f9e"))))
 '(lazy-highlight ((t (:background "#edf"))))
 '(anzu-mode-line ((t (:inherit unspecified))))

 ;; Syntax highlighting

 '(font-lock-comment-face ((t (:foreground "#838"))))
 '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
 '(font-lock-string-face ((t (:foreground "#244"))))
 '(font-lock-warning-face ((t (:foreground "#727" :weight bold))))
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

 ;; Ivy

 '(ivy-current-match ((t :background "#d0d1ee")))
 '(ivy-minibuffer-match-face-2 ((t :foreground "#902090")))
 '(ivy-minibuffer-match-face-3 ((t :foreground "#902090")))
 '(ivy-minibuffer-match-face-4 ((t :foreground "#902090")))

 ;; Helm
 '(helm-selection ((t (:background "#ddd"))))
 '(helm-ff-directory ((t (:foreground "#455"))))
 '(helm-ff-dotted-directory ((t (:foreground "#455"))))
 '(helm-ff-symlink ((t (:inherit unspecified))))
 '(helm-ff-prefix ((t (:foreground "#000" :background "gold1"))))
 '(helm-ff-executable ((t (:foreground "#0a0"))))
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

 '(flycheck-fringe-error ((t (:foreground "#d20"))))
 '(flycheck-fringe-warning ((t (:foreground "#d90"))))
 '(flycheck-fringe-info ((t (:foreground "#06c"))))

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

(provide-theme 'board)
