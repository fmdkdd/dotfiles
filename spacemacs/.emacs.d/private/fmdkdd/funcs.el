;;; funcs.el --- fmdkdd Layer functions File
;;
;; Copyright (c) 2015 fmdkdd
;;
;; Author: fmdkdd
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun fmdkdd//auto-fill-comments ()
  (setq-local comment-auto-fill-only-comments t)
  (turn-on-auto-fill))

(defun fmdkdd//add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\|DELETE\\|XXX\\):"
          1 font-lock-warning-face t))))

(defun fmdkdd//customize-java-mode ()
  (c-set-style "user-java"))

(defun fmdkdd//turn-off-truncate-lines ()
  (setq truncate-lines nil))

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
are nil, call `fmdkdd/select-browser-windows' to ask the user to
select the browser window with the mouse cursor and sets the
forementionned variables for future calls.

With a prefix argument, it bypasses the variables and forces the
reselection of the window.  Multiple windows can be selectionned
this way with a prefix argument greater than 1."
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
  "Send F5 to each window of WINDOW-LISP using xdotool.

Elements of the list are cons cells (ID . FOCUS) where ID is a X
window id obtained from running 'xdotool selectwindow' (a string
representing a number), and FOCUS is a boolean indicating whether
we must give the focus to that window before sending the key
event."
  (let ((cmd)        ; xdotool command to reload each window
        (wactivate)) ; must we switch the focus away from the Emacs window?
    (setq cmd
          (mapconcat
           (lambda (w)
             (let ((id (car w))
                   (focus (cdr w)))
               (if focus
                   (progn
                     (setq wactivate t)
                     (format "windowactivate --sync %s key --window %s 'F5'"
                             id id))
                 (format "key --window %s 'F5'" id))))
           window-list " "))
    (shell-command
     ;; If one window needed the focus, we need to save the current Emacs window
     ;; and get focus back to it once we are done. We leverage the WINDOW_STACK
     ;; of xdotool to do that.
     (if wactivate
         (format "xdotool getactivewindow %s windowactivate" cmd)
       (format "xdotool %s" cmd)))))

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
  "Check if the window needs to get the focus to receive a key
sent by xdotool."
  (not (string-match-p
        fmdkdd/browser-window-no-focus-regexp
        (string-trim (shell-command-to-string
                      (format "xdotool getwindowname %s" window))))))

(defun fmdkdd/org-full-outline-path ()
  "Concatenate the results of `org-get-outline-path' and
`org-get-heading' to get the full outline path to the heading we
are currently in."
  (unless (org-before-first-heading-p)
    (let* ((path (append (org-get-outline-path)
                         (cons (org-get-heading t t) nil))))
      (org-format-outline-path path 40)))) ; XXX: not sure if the width
                                        ; argument works right
