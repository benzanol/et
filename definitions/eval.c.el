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
       (args: ListR<Sexp> body: Sexp env: LexicalEnvironment
              &optional docstring: Nil|String iform: Sexp)
       InterpretedFunction)

 (@check function
         ($pcase
          `(,sym) ($eval (or (et-symbol-func-type sym)
                             (et-fatal "Not a function: %s" sym)))))

 (@def defvaralias ([(<= V Symbol)] new-alias: Var base-variable: V &optional docstring: String|Nil) V)
 (@def default-toplevel-value (symbol: Symbol) Any)
 (@def set-default-toplevel-value (symbol: Var value: Any) Any)

 (@check defvar
         ($pcase
          (or `(,name) `(,name ,val .,_))
          ($progn ($expect ($eval (et-global-var-type name)) ($exp val))
                  ($eval (et-literal name)))))
 (@check defconst
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
          `(,_cond . ,body)
          ($progn ($loop ($if ($at 1) ($tail 2) ($type Nil)))
                  ($must-be ($at 1) ($type Nil)))))

 (@def funcall-with-delayed-message
       ([R] delay: Number message: String func: fn<Nil~R>) R)

 (@def macroexpand (form: Sexp &optional environment: AList<Symbol~Any>) Sexp)

 (@alias ThrowCatchTag NonNil)
 (@check catch ($body ThrowCatchTag)) ;todo
 (@def throw (tag: ThrowCatchTag value: Any) Never)

 (@check unwind-protect
         ($pcase
          `(,form . ,unwind) ($prog1 ($at 1) ($tail 2))))

 (@check condition-case
         ($pcase
          `(,(and err-var (pred symbolp)) ,val) ($ignore ($at 2))

          `(,(and err-var (pred symbolp)) ,val (:success . ,body) . ,rest)
          ($temp-var var ($at 2)
                     ($branches
                      ($bind err-var ($var var) ($tail 3 1))
                      ($recurse `(,err-var (:var ,var) ,@rest))))

          `(,(and err-var (pred symbolp)) ,val (,_other . ,body) . ,rest)
          ($temp-var var ($at 2)
                     ($branches
                      ($tail 3 1)
                      ($recurse `(,err-var (:var ,var) ,@rest))))))
 
 (@def signal (error-symbol: NonNilSymbol data: Any) Never)
 
 (@def commandp ([T] function: T) Boolean) ;todo
 )
