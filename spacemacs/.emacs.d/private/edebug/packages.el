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
Execution modes     ^^Jumping                ^^Views
[_S_] stop            [_h_] goto-here          [_v_] view outside
[_s_] step            [_f_] forward-sexp       [_p_] bounce point
[_n_] next            [_o_] step-out           [_w_] where
[_t_] trace           [_i_] step-in            [_W_] toggle save windows
[_T_] rapid trace
[_g_] go              ^Breakpoints^            ^Misc^
[_c_] continue        [_b_] set                [_?_] help
[_C_] rapid continue  [_u_] unset              [_Q_] top level
[_G_] go non-stop     [_x_] conditional        [_q_] top level non-stop
^^                    [_X_] global condition   [_r_] echo previous result
^^                    [_B_] move to breakpoint [_d_] backtrace

[_I_] instrument callee [_e_] eval expression
"
      ;; Allow foreign keys without quitting the transient-state
      ;; For e.g., moving point or switching windows
      :foreign-keys run
      :bindings
      ;; Instrumenting
      ("I" edebug-instrument-callee)
      ;; Execution modes
      ("S" edebug-stop)
      ("s" edebug-step-mode)
      ("n" edebug-next-mode)
      ("t" edebug-trace-mode)
      ("T" edebug-Trace-fast-mode)
      ("g" edebug-go-mode)
      ("c" edebug-continue-mode)
      ("C" edebug-Continue-fast-mode)
      ("G" edebug-Go-nonstop-mode)
      ;; Jumping
      ("h" edebug-goto-here)
      ("f" edebug-forward-sexp)
      ("o" edebug-step-out)
      ("i" edebug-step-in)
      ;; Misc
      ("?" edebug-help)
      ("Q" top-level)
      ("q" edebug-top-level-nonstop :exit t)
      ("r" edebug-previous-result)
      ("d" edebug-backtrace)
      ;; Breakpoints
      ("b" edebug-set-breakpoint)
      ("u" edebug-unset-breakpoint)
      ("x" edebug-set-conditional-breakpoint)
      ("X" edebug-global-break-condition)
      ("B" edebug-next-breakpoint)
      ;; Views
      ("v" edebug-view-outside)
      ("p" edebug-bounce-point)
      ("w" edebug-where)
      ("W" edebug-toggle-save-windows)
      ;; Evaluation
      ("e" edebug-eval-expression))

    ;; Exit the transient state when edebug-mode is off
    (defun spacemacs/toggle-edebug-mode-transient-state ()
      (if edebug-mode
        (spacemacs/edebug-mode-transient-state/body)
      (hydra-disable)))

    (add-hook 'edebug-mode-hook 'spacemacs/toggle-edebug-mode-transient-state)))
