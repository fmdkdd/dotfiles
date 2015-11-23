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

(defvar-local fmdkdd/browser-window-id nil
  "X window id of the browser window that will receive a Ctrl+R
key when `fmdkdd/reload-browser-window' is called.")

(defvar fmdkdd/last-browser-window-id nil
  "X window id of the last browser window to be reloaded by
  `fmdkdd/reload-browser-window'.  Used to lookup the window to
  reload when `fmdkdd/browser-window-id' is nil.")

(defun fmdkdd/save-and-reload-browser-window (&optional reselect)
  "Save current buffer and call reload browser window by calling
`fmdkdd/reload-browser-window'."
  (interactive "P")
  (save-buffer)
  (fmdkdd/reload-browser-window reselect))

(defun fmdkdd/reload-browser-window (&optional reselect)
  "Send Ctrl+R to the browser found by
`fmdkdd//find-browser-window'.

By default, it uses the content of the buffer-local variable
`fmdkdd/browser-window-id', or the last-found window id if nil.
If both are nil, it asks the user the select the browser window
with the mouse cursor.

With a prefix argument, force the reselection of the window and
save the window id to `fmdkdd/browser-window-id'."
  (interactive "P")
  (fmdkdd/do-reload-browser
   (if reselect (fmdkdd//ask-browser-window)
     (cond
      ;; I know!  I know!
      (fmdkdd/browser-window-id fmdkdd/browser-window-id)
      ;; Hmm, maybe the window you specified last time?
      (fmdkdd/last-browser-window-id fmdkdd/last-browser-window-id)
      ;; What window?  Please tell me.
      (t (fmdkdd//ask-browser-window)))))
  (message "Reloaded browser window"))

(defun fmdkdd/do-reload-browser (window-id)
  (shell-command
   ;; Just `xdotool key --window %s 'ctrl+r'` /should/ do it.  But Chrome does
   ;; not listen to the key when the window does not have focus
   ;; (see https://code.google.com/p/chromium/issues/detail?id=393145).
   ;; So, we focus on the window before sending the input.  Then we wrap the
   ;; whole thing in 'getwindowfocus ... windowfocus' to restore the focus on
   ;; Emacs, by leveraging the WINDOW_STACK of xdotool.
   (format "xdotool getwindowfocus windowfocus --sync %s key --window %s 'ctrl+r' windowfocus"
           window-id window-id)))

(defun fmdkdd//ask-browser-window ()
  (interactive)
  (message "Select the browser window using the mouse cursor")
  (let ((wid (string-trim (shell-command-to-string "xdotool selectwindow"))))
    ;; Oh, /that/ window.  I'll remember next time.
    (setq fmdkdd/browser-window-id wid
          fmdkdd/last-browser-window-id wid)
    wid))
