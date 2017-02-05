;;;; packages.el --- Edebug Layer packages File for Spacemacs
;;
;; Copyright (c) 2017 fmdkdd
;;
;; Author: fmdkdd
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq edebug-packages
      '(edebug))

(defun edebug/init-edebug ()
  (use-package edebug
    :config

    ;; With save-windows on, the transient state bindings will appear only the
    ;; first time, but not after any head is called.
    (setq edebug-save-windows nil)

    (spacemacs|define-transient-state edebug-mode
      :title "Edebug Transient State"
      :doc "
[_s_] step          [_t_] trace              [_c_] goto-point
[_i_] step-in       [_e_] eval-expression    [_S_] stop
[_o_] step-out      [_b_] set-breakpoing     [_?_] help
[_n_] next          [_I_] instrument-callee  [_q_] quit
[_f_] forward-sexp
"
      :foreign-keys warn
      :bindings
      ("q" edebug-top-level-nonstop :exit t)
      ("s" edebug-step-mode)
      ("n" edebug-next-mode)
      ("i" edebug-step-in)
      ("o" edebug-step-out)
      ("b" edebug-set-breakpoint)
      ("t" edebug-trace-mode)
      ("e" edebug-eval-last-sexp)
      ("f" edebug-forward-sexp)
      ("c" edebug-goto-here)
      ("I" edebug-instrument-callee)
      ("S" edebug-stop)
      ("?" edebug-help))

    ;; Exit the transient state when edebug-mode is off
    (defun spacemacs/toggle-edebug-mode-transient-state ()
      (if edebug-mode
        (spacemacs/edebug-mode-transient-state/body)
      (hydra-disable)))

    (add-hook 'edebug-mode-hook 'spacemacs/toggle-edebug-mode-transient-state)))
