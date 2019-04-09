;;; xref-asm.el --- Xref for asm-mode -*- lexical-binding: t; -*-

;; Copyright 2019 fmdkdd

;; This file is not part of GNU Emacs.

;;; Commentary:

;; (add-hook 'asm-mode-hook #'xref-asm-activate)

;;; Code:

(require 'xref)

(defun xref-asm-xref-backend ()
  "ASM backend for xref."
  'xref-asm)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql xref-asm)))
  (symbol-name (symbol-at-point)))

(cl-defmethod xref-backend-definitions ((_backend (eql xref-asm)) symbol)
  (mapcar
   (lambda (candidate)
     (xref-make (car candidate)
                (xref-make-buffer-location (current-buffer)
                                           (cdr candidate))))
   (xref-asm-candidates symbol)))

(defun xref-asm-candidates (symbol)
  "Return list of labels matching SYMBOL."
  (save-excursion
    (goto-char (point-min))
    (let ((labels))
      ;; Match labels and constants
      (while (re-search-forward (format "^\\s *@?%s\\s *[:=]" symbol) nil t)
        (push (cons (match-string-no-properties 0) (point))
              labels))
      labels)))

;;;###autoload
(defun xref-asm-activate ()
  "Register the xref asm backend."
  (add-to-list 'xref-backend-functions #'xref-asm-xref-backend))


(provide 'xref-asm)
;;; xref-asm.el ends here

;; Local Variables:
;; eval: (fmdkdd/byte-compile-on-save)
;; End:
