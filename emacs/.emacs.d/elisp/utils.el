;;; utils.el --- Miscellaneous editing functions -*- lexical-binding: t; -*-

;; Copyright (c) 2017--2019 fmdkdd
;;
;; Author: fmdkdd

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Utilities functions for my own configuration.

;;; Code:

(require 'subr-x)
(require 'imenu)
(require 'recentf)
(require 'counsel-projectile)
(require 'ivy)
(require 'org)
(require 'subword)

;; Best of both worlds
(defun kill-region-or-backward-word ()
  "Kill the region if active, otherwise kill the word before point."
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (if (and (boundp 'subword-mode) subword-mode)
        (subword-backward-kill 1)
      (backward-kill-word 1))))

;; There is `write-file`, but it leaves the previous file around
;; this one is from: http://emacsredux.com/blog/2013/05/04/rename-file-and-buffer/
(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (user-error "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defun auto-fill-comments ()
  (setq-local comment-auto-fill-only-comments t)
  (turn-on-auto-fill))

(defun fmdkdd/async-byte-compile ()
  "Asynchronously byte compile this file.

Use the load path of the current buffer for the child Emacs
process."
  (let ((paths (mapcan (lambda (dir)
                         (list "--directory" dir))
                       load-path)))
    (make-process
     :name "async-byte-compile"
     :command
     `("emacs" "-Q" "--batch"
       ,@paths
       "--eval" "(require 'bytecomp)"
       "-f" "batch-byte-compile" ,buffer-file-name))))

(defun fmdkdd/byte-compile-on-save ()
  "Asynchronously byte compile this file on save."
  (add-hook 'after-save-hook #'fmdkdd/async-byte-compile nil 'local))

(defun man-at-point ()
  "Open the man page for the symbol at point."
  (interactive)
  (let ((thing (word-at-point)))
    (man thing)))

(defun describe-thing-at-point ()
  "Describe thing under cursor."
  (interactive)
  (let ((thing (symbol-at-point)))
    (cond
     ((fboundp thing) (describe-function thing))
     ((boundp thing) (describe-variable thing)))))

(defun add-watchwords ()
  "Add TODO: words to font-lock keywords."
  (font-lock-add-keywords
   nil '(("\\(\\<TODO\\|\\<FIXME\\|\\<HACK\\|@.+\\):"
          1 font-lock-warning-face t))))

(defun move-beginning-of-line-dwim ()
  "Move to beginning of line text, or to beginning of line."
  (interactive)
  (let ((point-at-first-text))
    (save-excursion
      (beginning-of-visual-line)
      (skip-chars-forward " \t")
      (setq point-at-first-text (point)))
    ;; If we are already at the first non-whitespace char, then move to the
    ;; beginning of line
    (if (eq point-at-first-text (point))
        (beginning-of-visual-line)
      ;; Otherwise, move to first text
      (goto-char point-at-first-text))))

(defun insert-screenshot (file-name)
  "Save screenshot to FILE-NAME and insert an Org link at point.

This calls the `import' from ImageMagick to take the screenshot,
and `optipng' to reduce the file size if the program is present."
  (interactive "FSave to file: ")
  ;; Get absolute path
  (let ((file (expand-file-name file-name)))
    ;; Create the directory if necessary
    (make-directory (file-name-directory file) 'parents)
    ;; Still, make sure to signal if the screenshot was in fact not created
    (unless (= 0 (call-process "import" nil nil nil file))
      (user-error "`import' failed to create screenshot %s" "bla"))
    (if (executable-find "optipng")
        (start-process "optipng" nil "optipng" file))
    (insert
     ;; A link relative to the buffer where it is inserted is more portable
     (format "[[file:%s]]"
             (file-relative-name file
                                 (file-name-directory buffer-file-name))))
    (when (eq major-mode 'org-mode)
      (org-redisplay-inline-images))))

(defun fmdkdd/apropos ()
  "Show all matching symbols and their docstring."
  (interactive)
  (ivy-read "Describe symbol: "
            (let (cands)
              (mapatoms
               (lambda (sym)
                 (when (or (boundp sym) (fboundp sym))
                   (push (symbol-name sym) cands))))
              cands)
            :action (lambda (cand)
                      (let ((sym (intern cand)))
                        (if (fboundp sym)
                            (describe-function sym)
                          (describe-variable sym))))
            :caller 'fmdkdd/apropos))

(defun fmdkdd/apropos-display-transformer (cand)
  "Return CAND and its docstring, if any."
  (let ((doc (docstring-first-line (intern cand))))
    (if doc
        (format "%s  %s" cand
                (propertize doc 'face '(variable-pitch shadow)))
      cand)))

(defun fmdkdd/M-x-display-transformer (cmd)
  "Return CMD appended with its keybinding and its docstring."
  (let* ((cmd-sym (intern cmd))
         (doc (or (docstring-first-line cmd-sym) ""))
         (binding (or (substitute-command-keys (format "\\[%s]" cmd-sym)) "")))
    (if (string-match-p "^M-x" binding)
        (format "%s %s"
                cmd-sym
                (propertize doc 'face '(variable-pitch shadow)))
      (format "%s (%s) %s"
          cmd-sym
          (propertize binding 'face 'font-lock-keyword-face)
          (propertize doc 'face '(variable-pitch shadow))))))

(defun docstring-first-line (sym)
  "Return the first line of the docstring for SYM."
  (elisp--docstring-first-line
   (or (and (fboundp sym) (documentation sym))
       (documentation-property sym 'variable-documentation))))

(defun fmdkdd/goto-anything ()
  "Prompt to go to buffer, file or symbol.

Look for imenu symbols, buffers, project files, recent files and
other projects."
  (interactive)
  (ivy-read "Goto: "
            (append
             ;; Active buffers
             (seq-remove
              ;; Remove ephemeral buffers
              (lambda (buf)
                (string-prefix-p " " (car buf)))
              (mapcar (lambda (buf)
                        (list (buffer-name buf) 'buffer buf))
                      (delete (current-buffer)
                              (buffer-list))))
             ;; Imenu items
             (condition-case _
                 (mapcar (lambda (item)
                       (list (format "Imenu: %s" (car item)) 'marker (cddr item)))
                     (counsel-imenu-get-candidates-from
                      (or imenu--index-alist
                          (imenu--make-index-alist 'no-error))))
               ;; Even though we pass 'no-error, there is another function
               ;; called by make-index-alist which can throw this if imenu is
               ;; not available for the buffer.  We just return no items then.
               (imenu-unavailable nil))
             ;; Recent files
             (mapcar (lambda (file)
                       (cons file 'file))
                     recentf-list)
             ;; Project files
             (when-let ((project-root (and buffer-file-name
                                           (locate-dominating-file buffer-file-name ".git"))))
               (mapcar (lambda (file)
                         (list file 'rel-file (expand-file-name file project-root)))
                       (split-string
                        (let ((default-directory project-root))
                          (shell-command-to-string "git ls-files -zco --exclude-standard"))
                        "\0")))
             ;; Project roots
             (mapcar (lambda (dir)
                       (list (format "Project: %s" dir) 'project dir))
                     projectile-known-projects))
            :action (lambda (cand)
                      (pcase cand
                        (`(,_ marker ,m) (imenu-default-goto-function nil m))
                        (`(,_ buffer ,b) (switch-to-buffer b))
                        (`(,f . file) (find-file f))
                        (`(,_ rel-file ,f) (find-file f))
                        (`(,_ project ,p) (counsel-projectile-switch-project-action p))
                        (_ (switch-to-buffer cand))))
            :caller 'fmdkdd/goto-anything))

(provide 'utils)
;;; utils.el ends here

;; Local Variables:
;; eval: (fmdkdd/byte-compile-on-save)
;; End:
