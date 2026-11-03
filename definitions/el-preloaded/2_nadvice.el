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
 (@def advice-function-mapc
       (f: fn2<AnyFn_Alist<Symbol_Any>> function-def: AnyFn)
       Nil)
 (@def advice-function-member-p
       (advice: AnyFn|Symbol|String function-def: AnyFn)
       Bool))

;;; ============================================================
;;; Specific application of add-function to symbol-function for advice

(et-declare
 (@alias AdviceFn
         [(OrigFn <: AnyFn)
          (Place <: :around|:before|:after|:override|:after-until|:after-while|:before-until|:before-while|:filter-args|:filter-return)]
         (eval et-advice-fn-type OrigFn Place))
 (@def advice-add
       (symbol: AnyFn
        how: :around|:before|:after|:override|:after-until|:after-while|:before-until|:before-while|:filter-args|:filter-return
        function: AnyFn
        &optional props: Alist<Symbol_Any>)
       Nil)
 (@def advice-remove
       ([(OrigFn <: AnyFn)] symbol: OrigFn function: AnyFn|Symbol|String)
       Nil)
 ;; ARGS is a literal (HOW LAMBDA-LIST &optional NAME DEPTH) list that is
 ;; destructured at expansion time rather than evaluated, and BODY is
 ;; spliced into a newly generated defun/lambda that becomes the advice.
 ;; Neither $body nor $fn can express a macro that builds and installs a
 ;; new function definition from unevaluated sub-forms.
 (@check define-advice ($todo))
 (@def advice-mapc
       ([(OrigFn <: AnyFn)] fun: fn2<AnyFn_Alist<Symbol_Any>> symbol: OrigFn)
       Nil)
 (@def advice-member-p
       ([(OrigFn <: AnyFn)] advice: AnyFn|Symbol|String symbol: OrigFn)
       Bool))

(et-defun et-advice-fn-type (orig-fn: EtType place-type: EtType) EtType
  "Return the function type required for advice at PLACE-TYPE on ORIG-FN."
  (let* ((places (et-type-single-literals place-type))
         (place (and (listp places) (length= places 1) (car places))))
    (unless (memq place '(:around :before :after :override
                          :after-until :after-while :before-until :before-while
                          :filter-args :filter-return))
      (error "Invalid advice place: %s" (et-pp place-type)))
    (setq orig-fn (et-expand-aliases orig-fn))
    (apply
     #'et-union
     (cl-loop
      for case in (et:type->cases orig-fn)
      collect
      (pcase (et:type-case->value case)
        ((cl-struct et:type-dt (name 'Function) (args `(,args ,ret)))
         (pcase place
           (:around (et-dt 'Function (et-alias '&Cons orig-fn args) ret))
           (:filter-args
            (et-dt 'Function (et-alias '&Cons args (et-literal nil)) args))
           (:filter-return
            (et-dt 'Function (et-alias '&Cons ret (et-literal nil)) ret))
           ((or :before :after :after-until :after-while
                :before-until :before-while)
            (et-dt 'Function args (et-any)))
           (_ (et-dt 'Function args ret))))
        ((and value (cl-struct et:type-dt (name 'PolyFunction)))
         (apply
          #'et-union
          (cl-loop
           for (_polys args ret) in (et:type--destructure-function (et-type value) t)
           collect
           (pcase place
             (:around (et-dt 'Function (et-alias '&Cons orig-fn args) ret))
             (:filter-args
              (et-dt 'Function (et-alias '&Cons args (et-literal nil)) args))
             (:filter-return
              (et-dt 'Function (et-alias '&Cons ret (et-literal nil)) ret))
             ((or :before :after :after-until :after-while
                  :before-until :before-while)
              (et-dt 'Function args (et-any)))
             (_ (et-dt 'Function args ret))))))
        (_ (error "Not a function type: %s" (et-pp orig-fn))))))))

;; `advice-add' also has an ordinary function type so that `#'advice-add'
;; can be passed as a value.  Direct calls use this checker to retain the
;; dependency between SYMBOL, HOW, and FUNCTION.
(et-set-chk
 #'advice-add
 '($pcase
   `(,_symbol ,_how ,_function)
   ($let [orig ($expect ($type AnyFn) ($at 1))]
         [place ($expect
                 ($type :around|:before|:after|:override|:after-until|:after-while|:before-until|:before-while|:filter-args|:filter-return)
                 ($at 2))]
         ($progn
          ($expect ($eval (et-advice-fn-type orig place)) ($at 3))
          ($type Nil)))
   `(,_symbol ,_how ,_function ,_props)
   ($let [orig ($expect ($type AnyFn) ($at 1))]
         [place ($expect
                 ($type :around|:before|:after|:override|:after-until|:after-while|:before-until|:before-while|:filter-args|:filter-return)
                 ($at 2))]
         ($progn
          ($expect ($eval (et-advice-fn-type orig place)) ($at 3))
          ($expect ($type Alist<Symbol_Any>) ($at 4))
          ($type Nil)))))

;;; ============================================================
