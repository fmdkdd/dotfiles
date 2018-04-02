;;; utils.el --- Miscellaneous editing functions -*- lexical-binding: t; -*-

;; Copyright (c) 2017, 2018 fmdkdd
;;
;; Author: fmdkdd

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Utilities functions for my own configuration.

;;; Code:

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
  "Asynchronously byte compile this file."
  (make-process
   :name "async-byte-compile"
   :command (list "emacs"
                  "-Q" "--batch" "--eval" "(require 'bytecomp)"
                  "-f" "batch-byte-compile" buffer-file-name)))

(defun fmdkdd/byte-compile-on-save ()
  "Asynchronously byte compile this file on save."
  (add-hook 'after-save-hook #'fmdkdd/async-byte-compile nil t))

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
  (font-lock-add-keywords
   nil '(("\\<\\(TODO\\|FIXME\\|HACK\\|REFACTOR\\|DELETE\\|XXX\\):"
          1 font-lock-warning-face t))))

(defun move-beginning-of-line-dwim ()
  "Move to beginning of line text, or to beginning of line."
  (interactive)
  (let ((point-at-first-text))
    (save-excursion
      (beginning-of-visual-line)
      (skip-chars-forward "[ \t]")
      (setq point-at-first-text (point)))
    ;; If we are already at the first non-whitespace char, then move to the
    ;; beginning of line
    (if (eq point-at-first-text (point))
        (beginning-of-visual-line)
      ;; Otherwise, move to first text
      (goto-char point-at-first-text))))

;;; From https://www.emacswiki.org/emacs/download/goto-last-change.el

(defvar-local goto-last-change-undo nil
  "The `buffer-undo-list' entry of the previous `goto-last-change' command.")

(defun goto-last-change (&optional mark-point)
  "Set point to the position of the last change.
Consecutive calls set point to the position of the previous
change.  With a prefix arg (optional arg MARK-POINT non-nil), set
mark so `exchange-point-and-mark' will return point to the
current position."
  (interactive "P")
  (when (eq buffer-undo-list t)
    (error "No undo information in this buffer"))
  (when mark-point
    (push-mark))
  (let ((position nil)
        (minimal-line-distance 20)
	(undo-list (if (and (eq this-command last-command)
			    goto-last-change-undo)
		       (cdr (memq goto-last-change-undo buffer-undo-list))
		     buffer-undo-list))
	undo)
    (while (and undo-list
                (or (not position)
                    (eql position (point))
                    (and minimal-line-distance
                         ;; The first invocation always goes to the last change, subsequent ones skip
                         ;; changes closer to (point) then minimal-line-distance.
                         (memq last-command '(goto-last-change
                                              goto-last-change-with-auto-marks))
                         (< (count-lines (min position (point-max)) (point))
                            minimal-line-distance))))
      (setq undo (car undo-list))
      (cond ((and (consp undo) (integerp (car undo)) (integerp (cdr undo)))
	     ;; (BEG . END)
	     (setq position (cdr undo)))
	    ((and (consp undo) (stringp (car undo))) ; (TEXT . POSITION)
	     (setq position (abs (cdr undo))))
	    ((and (consp undo) (eq (car undo) t))) ; (t HIGH . LOW)
	    ((and (consp undo) (null (car undo)))
	     ;; (nil PROPERTY VALUE BEG . END)
	     (setq position (cdr (last undo))))
	    ((and (consp undo) (markerp (car undo)))) ; (MARKER . DISTANCE)
	    ((integerp undo))		; POSITION
	    ((null undo))		; nil
	    (t (error "Invalid undo entry: %s" undo)))
      (setq undo-list (cdr undo-list)))
    (cond (position
	   (setq goto-last-change-undo undo)
	   (goto-char (min position (point-max))))
	  ((and (eq this-command last-command)
		goto-last-change-undo)
	   (setq goto-last-change-undo nil)
	   (error "No further undo information"))
	  (t
	   (setq goto-last-change-undo nil)
	   (error "Buffer not modified")))))

(defvar-local fmdkdd/browser-window-list nil
  "List of the browser windows that will receive a F5 key event
when `fmdkdd/reload-browser-windows' is called.

Elements of the list are cons cells (ID . FOCUS) where ID is a X
window id obtained from running 'xdotool selectwindow' (a string
representing a number), and FOCUS is a boolean indicating whether
we must give the focus to that window before sending the key
event.  See the documentation of
`fmdkdd/browser-window-no-focus-regexp' for why this field is
needed.")

(defvar fmdkdd/last-browser-window-list nil
  "List of the browser windows that were last selected by the
user through `fmdkdd/select-browser-windows'.

This value is used by `fmdkdd/reload-browser-windows' when the
buffer-local variable `fmdkdd/browser-window-list' is nil.")

(defvar fmdkdd/browser-window-no-focus-regexp "Firefox"
  "This regexp is used by `fmdkdd/select-browser-windows' on the
names of each selected window to determine if they need
windowactivate.

Google Chrome and Chromium do not listen to the key sent by
xdotool when the window does not have focus (see
https://code.google.com/p/chromium/issues/detail?id=393145).
Using windowfocus is not enough to receive the key either (it
fails randomly).  But using the windowactivate command of xdotool
shifts the focus away from the Emacs window, so we'd rather avoid
it if we can.")

(defun fmdkdd/save-and-reload-browser-windows (&optional reselect)
  "Save current buffer and call `fmdkdd/reload-browser-windows'."
  (interactive "P")
  (save-buffer)
  (fmdkdd/reload-browser-windows reselect))

(defun fmdkdd/reload-browser-windows (&optional reselect)
  "Send F5 to reload the browser windows.

The windows to reload are found by looking up the buffer-local
variable `fmdkdd/browser-window-list', then the global
`fmdkdd/last-browser-window-list' if the former is nil.  If both
are nil, it calls `fmdkdd/select-browser-windows' to ask the user
to select the browser window with the mouse cursor and sets the
aforementioned variables for future calls.

With a prefix argument, it bypasses the variables and forces the
selection of the window.  When the prefix argument is a number
greater than 1, multiple windows can be selected."
  (interactive "P")
  (fmdkdd//do-reload-browser-windows
   (if reselect (fmdkdd/select-browser-windows reselect)
     (cond
      ;; I know!  I know!
      (fmdkdd/browser-window-list)
      ;; Hmm, maybe the window you specified last time?
      (fmdkdd/last-browser-window-list)
      ;; I give up!  Please tell me.
      (t (fmdkdd/select-browser-windows))))))

(defun fmdkdd//do-reload-browser-windows (window-list)
  "Send F5 to each window of WINDOW-LIST using xdotool.

Elements of the list are cons cells (ID . FOCUS) where ID is a X
window id obtained from running 'xdotool selectwindow' (a string
representing a number), and FOCUS is a boolean indicating whether
we must give the focus to that window before sending the key
event."
  (let ((args)            ; xdotool command to reload each window
        (wactivate))      ; must we switch the focus away from the Emacs window?
    (setq args
          (seq-mapcat
           (lambda (w)
             (let ((id (car w))
                   (focus (cdr w)))
               (if focus
                   (progn
                     (setq wactivate t)
                     (list "windowactivate" "--sync" id "key" "--window" id "F5"))
                 (list "key" "--window" id "F5"))))
           window-list))
    ;; If one window needed the focus, we need to save the current Emacs window
    ;; and get focus back to it once we are done. We leverage the WINDOW_STACK
    ;; of xdotool to do that.
    (if wactivate
        (apply #'call-process `("xdotool" nil 0 nil "getactivewindow" ,@args "windowactivate"))
      (apply #'call-process `("xdotool" nil 0 nil ,@args)))))

(defun fmdkdd/select-browser-windows (&optional times)
  "Use 'xdotool selectwindow' to select TIMES windows interactively,
and save the values for future calls to
`fmdkdd/reload-browser-windows'."
  (when (not (numberp times)) (setq times 1)) ; Default value for TIMES
  (let ((windows))
    (dotimes (n times)
      (message
       (format "Select the browser window using the mouse cursor (%d/%d)"
               (1+ n) times))
      (let* ((id (string-trim
                  (shell-command-to-string "xdotool selectwindow")))
             (focus (fmdkdd//must-focus-window-to-reload-p id)))
        (setq windows (cons (cons id focus) windows))))
    ;; Oh, /these/ windows.  I'll remember next time.
    (setq fmdkdd/browser-window-list windows
          fmdkdd/last-browser-window-list windows)
    windows))

(defun fmdkdd//must-focus-window-to-reload-p (window)
  "Check if WINDOW needs to get the focus to receive a key sent
by xdotool."
  (not (string-match-p
        fmdkdd/browser-window-no-focus-regexp
        (string-trim (shell-command-to-string
                      (format "xdotool getwindowname %s" window))))))

(defun insert-screenshot (file-name)
  "Save screenshot to FILE-NAME and insert an Org link at point.

This calls the `import' from ImageMagick to take the screenshot,
and `optipng' to reduce the file size if the program is present."
  (interactive "FSave to file: ")
  (call-process "import" nil nil nil file-name)
  (start-process "optipng" nil "optipng" file-name)
  (insert (format "[[file:%s]]" file-name))
  (when (eq major-mode 'org-mode)
    (org-redisplay-inline-images)))

(provide 'utils)
;;; utils.el ends here
