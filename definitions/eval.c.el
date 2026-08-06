;; -*- lexical-binding: t; -*-

(et-declare
 (@check or
         ($pcase
          `() ($type Nil)
          `(,_one . ,rest) ($or ($at 1) ($recurse rest))))

 (@check and
         ($pcase
          `() ($type True)
          `(,_one) ($at 1)
          `(,_one . ,rest) ($and ($at 1) ($recurse rest))))

 (@check if
         ($pcase
          `(,_cond ,_then . ,_else) ($if ($at 1) ($at 2) ($tail 3))))

 (@check cond
         ($pcase
          `() ($type Nil)
          `((,cond . ,body) . ,rest)
          ($if ($at 1 0) ($tail 1 1) ($recurse rest))))

 (@check progn ($tail 1))
 (@check prog1 ($prog1 ($at 1) ($tail 2)))

 (@check setq ($todo))

 (@check quote
         ($pcase
          `(,obj) ($eval (et-literal obj))))

 (@alias LexicalEnvironment True|Nil) ;todo
 (@def make-interpreted-closure
       (args: &List<Sexp> body: Sexp env: LexicalEnvironment
              &optional docstring: Nil|String iform: Sexp)
       InterpretedFunction)

 (@check function
         ($pcase
          `(,sym) ($eval (or (et-symbol-func-type sym)
                             (et-fatal "Not a function: %s" sym)))))

 (@def defvaralias ([(<= V Symbol)] new-alias: Var base-variable: V &optional docstring: String|Nil) V)
 (@def default-toplevel-value (symbol: Symbol) Any)
 (@def set-default-toplevel-value (symbol: Var value: Any) Any)

 (@check [defvar defconst]
         ($pcase
          (or `(,name) `(,name ,val .,_))
          ($progn ($expect ($eval (et-global-var-type name)) ($exp val))
                  ($eval (et-literal name)))))

 (@check let
         ($pcase
          `(,forms . ,_body)
          ($infer-binds (et-check-let-inits 1) ($tail 1))))

 (@check let*
         ($pcase
          `(nil . ,_body) ($tail 1)
          `((,first . ,rest) . ,body)
          ($exp `(let (,first)
                   (let* ,rest ,@body)))))

 (@check while
         ($pcase
          `(,_cond . ,_body)
          ($progn ($loop ($if ($at 1) ($tail 2) ($type Nil)))
                  ($must-be ($at 1) ($type Nil)))))

 (@def funcall-with-delayed-message
       ([R] delay: Number message: String func: fn<Nil~R>) R)

 (@def macroexpand (form: Sexp &optional environment: Alist<Symbol~Any>) Sexp)

 (@alias ThrowCatchTag NonNil)
 (@check catch ($body ThrowCatchTag)) ;todo
 (@def throw (tag: ThrowCatchTag value: Any) Never)

 (@check unwind-protect
         ($pcase
          `(,form . ,unwind) ($prog1 ($at 1) ($tail 2))))

 (@check condition-case
         ($pcase
          `(,(and err-var (pred symbolp)) ,val) ($ignore ($at 2))

          `(,(and err-var (pred symbolp)) ,val (,pat . ,body) . ,rest)
          ($temp-var var ($at 2)
                     ($branches
                      ($bind err-var ($eval (if (eq pat :success) (et-chk ($var var)) (et Any)))
                             ($tail 3 1))
                      ($recurse `(,err-var (:var ,var) ,@rest))))))

 (@def signal (error-symbol: NonNilSymbol data: Any) Never)

 (@def commandp (function: Any) Boolean)

 (@def autoload
       ([(<= V Var)] function: V file: String
        &optional docstring: String interactive: Any
        type: (or Nil @keymap @macro True))
       V)
 (@def autoload-do-load ([(<= V Var)] fundef: V &optional funname: Symbol macro-only Nil|@macro) V)

 (@def eval (form: Sexp &optional lexical: Boolean) Any)

 (@def apply ([A R] function: (fn (eval et-tuple-to-tailed A) R) &rest arguments: A) R)

 (@def run-hooks (&rest hooks: &List<fn>) Nil)
 (@def run-hook-with-args (hook: Var &rest args: &List) Any)
 (@def run-hook-with-args-until-success (hook: Var &rest args: &List) Any)
 (@def run-hook-with-args-until-failure (hook: Var &rest args: &List) Any)
 (@def run-hook-wrapped
       ([] hook: Var
        wrap-function: (fn (&Cons (fn &List Any) &List) Any)
        &rest args: &List)
       Any)

 (@def functionp ([T] object: T) (is? T AnyFn))

 (@def funcall ([A R] function: (fn A R) &rest arguments: A) R)

 (@def func-arity (function: AnyFn) Cons<Integer~Integer|@many|@unevalled>)

 (@def special-variable-p (symbol: Symbol) Boolean))

(defvar et--tuple-to-tailed-stack nil)
(et-defun et-tuple-to-tailed (tuple: EtType) EtType
  (or (et-stop-recursion et--tuple-to-tailed-stack tuple nil
        (or (et-infer tuple [T] (&Cons T Nil) T)
            (et-infer tuple [E A B] (&Cons E (&Cons A B)) (&Cons E (eval et-tuple-to-tailed (&Cons A B))))))
      (error "Not a tuple: %s" tuple)))
