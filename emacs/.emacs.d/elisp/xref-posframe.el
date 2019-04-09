;;; xref-posframe.el --- Show xref definitions with posframe -*- lexical-binding: t; -*-

;; Copyright 2019 fmdkdd

;; This file is not part of GNU Emacs.

;;; Commentary:

;; (global-set-key (kbd "M-.") #'xref-posframe-dwim)
;; (global-set-key (kbd "M-,") #'xref-posframe-pop)

;;; Code:

(require 'xref)
(require 'posframe)

(defvar xref-posframe-preview-lines 3
  "Number of lines to show in xref-posframe.")

(defun xref--item-string (item)
  "Return a preview string for xref ITEM."
  (save-excursion
    (let ((marker (xref-location-marker (xref-item-location item))))
      (set-buffer (marker-buffer marker))
      (xref--goto-char marker)
      (let ((beg (line-beginning-position))
            (end (progn (forward-line xref-posframe-preview-lines)
                        (line-end-position))))
        (font-lock-ensure beg end)
        (buffer-substring beg end)))))

(defvar xref-posframe--visible nil
  "Whether the posframe is visible.")

(defun xref-posframe--preview (item)
  "Show xref ITEM in a posframe."
  (posframe-show "*xref-posframe*"
                 :string (xref--item-string item)
                 :background-color "#334455"
                 :foreground-color "#baaa93")
  (setq xref-posframe--visible t)
  (add-hook 'post-command-hook #'xref-posframe--auto-clear))

;;;###autolaod
(defun xref-posframe-dwim ()
  "Show definitions for symbol at point with posframe.

Use xref to find the definitions for the symbol at point.  If
there is only one definition, preview it with posframe.  If this
command is called twice in succession, jump to the definition
instead.  If there are multiple definitions, fallback on the xref
behavior to display the candidates in a separate window."
  (interactive)
  (let* ((backend (xref-find-backend))
         (id (xref-backend-identifier-at-point backend))
         (defs (xref-backend-definitions backend id)))
    (cond
     ((not defs)
      (user-error "No definitions found for: %s" id))
     ((cdr defs)
      (xref--show-xrefs defs nil))
     ((eq last-command #'xref-posframe-dwim)
      (xref-push-marker-stack)
      (xref--pop-to-location (car defs) nil)
      (xref-posframe--clear))
     (t
      (xref-posframe--preview (car defs))))))

(defun xref-posframe--clear ()
  "Clear the xref posframe."
  (posframe-hide "*xref-posframe*")
  (setq xref-posframe--visible nil)
  (remove-hook 'post-command-hook #'xref-posframe--auto-clear))

(defun xref-posframe--auto-clear ()
  "Clear the xref posframe when this command is not `xref-posframe-dwim'."
  (when (not (eq this-command #'xref-posframe-dwim))
    (xref-posframe--clear)))

;;;###autolaod
(defun xref-posframe-pop ()
  "Dismiss the xref posframe, or pop the marker stack."
  (interactive)
  (unless xref-posframe--visible
    (xref-pop-marker-stack)))


(provide 'xref-posframe)
;;; xref-posframe.el ends here

;; Local Variables:
;; eval: (fmdkdd/byte-compile-on-save)
;; End:
