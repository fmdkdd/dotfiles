;;; ospl.el --- One sentence per line -*- lexical-binding: t; -*-

;; Copyright 2019 fmdkdd

;; This file is not part of GNU Emacs.

;;; Commentary:

;; (add-hook 'latex-mode-hook #'turn-on-ospl)

;;; Code:

;;;###autoload
(defun ospl-paragraph ()
  "Fill paragraph at point so each sentence stands on one line."
  (interactive)
  ;; Join all lines, then split them on sentences.
  (let ((start (save-excursion (backward-paragraph) (forward-char) (point)))
        (end (make-marker)))
    (save-excursion
      ;; Unfill paragraph (join all lines)
      (forward-paragraph)
      (backward-char)
      (set-marker end (point))          ; mark end of paragraph for later
      (beginning-of-line)
      (while (> (point) start)
        (join-line)
        ;; Preserve two dots after a sentence (only if the dot was at the end of
        ;; a line)
        ;; (when (looking-back "\\." (line-beginning-position))
        ;;   (insert " "))
        (beginning-of-line))
      ;; Now split lines at sentence end.  Locally using one space after dot for
      ;; sentences, since I'm working with people lacking manners.
      (let ((sentence-end-double-space nil))
        (forward-sentence)
        (while (< (point) end)
          ;; Avoid splitting for common acronyms
          (when (not (looking-back (rx (or "e.g." "i.e.")) (line-beginning-position)))
            (default-indent-new-line t))
          (forward-sentence))))))

(defun auto-ospl ()
  "Automatically break the current paragraph at sentence end.

This function is to be set as the value for the variable
`auto-fill-function'.  If you want to fill a paragraph so that
each sentence spans exactly one line, use `ospl-paragraph'
instead."
  (when (looking-back "\\.  " (line-beginning-position))
    (default-indent-new-line t)))

;;;###autoload
(defun turn-on-ospl ()
  "Change auto fill locally to keep one sentence per line."
  (setq auto-fill-function #'auto-ospl)
  (define-key (current-local-map) (kbd "M-q") #'ospl-paragraph))

(provide 'ospl)
;;; ospl.el ends here

;; Local Variables:
;; eval: (fmdkdd/byte-compile-on-save)
;; End:
