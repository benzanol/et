;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Lightweight advice/hook

(et-declare
 (@def advice-eval-interactive-spec (spec: Any) Any)
 ;; PLACE is evaluated as a generalized place (via `gv-ref') rather than
 ;; as an ordinary operand, similar to `setf'. Neither $body nor $fn can
 ;; express checking a place-form argument.
 (@check add-function ($todo))
 ;; PLACE is evaluated as a generalized place (via `gv-letplace') rather
 ;; than as an ordinary operand, similar to `setf'. Neither $body nor $fn
 ;; can express checking a place-form argument.
 (@check remove-function ($todo))
 (@def advice-function-mapc (f: fn2<Any~Any> function-def: Any) Nil)
 (@def advice-function-member-p (advice: Any function-def: Any) Bool))

;;; ============================================================
;;; Specific application of add-function to symbol-function for advice

(et-declare
 (@def advice-add
       (symbol: Var
        how: :around|:before|:after|:override|:after-until|:after-while|:before-until|:before-while|:filter-args|:filter-return
        function: Any
        &optional props: Alist<Symbol~Any>)
       Nil)
 (@def advice-remove (symbol: Var function: Any) Nil)
 ;; ARGS is a literal (HOW LAMBDA-LIST &optional NAME DEPTH) list that is
 ;; destructured at expansion time rather than evaluated, and BODY is
 ;; spliced into a newly generated defun/lambda that becomes the advice.
 ;; Neither $body nor $fn can express a macro that builds and installs a
 ;; new function definition from unevaluated sub-forms.
 (@check define-advice ($todo))
 (@def advice-mapc (fun: fn2<Any~Any> symbol: Symbol) Nil)
 (@def advice-member-p (advice: Any symbol: Symbol) Bool))

;;; ============================================================
