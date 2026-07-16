;;; lambda.el --- Type checking for lambda expressions -*- lexical-binding: t; -*-
;;; Code:

(defvar et--lambda-generic-counter 0)

(defun et--lambda-generic ()
  "Return a fresh interned generic for lambda parameter inference."
  (intern (format "P%s" (cl-incf et--lambda-generic-counter))))

(defun et--lambda-function-parts (function-type)
  "Return FUNCTION-TYPE's input and output types, or nil.

Only a concrete `Function' can provide a fixed type for each local
parameter.  A `DynFunction' instead derives its result from its call
arguments, so it cannot be used to type a lambda body from a
recommendation."
  (let* ((cases (et-type-cases (et-expand-all-aliases function-type))))
    (when (= (length cases) 1)
      (pcase (et-type-case-value (car cases))
        ((cl-struct et-datatype (name 'Function) (args `(,input ,output)))
         (cons input output))))))

(defun et--lambda-has-declaration-p (args-and-body)
  "Whether ARGS-AND-BODY contains a `declare' form with an ET clause."
  (when-let* ((declare-pos (cl-position 'declare args-and-body :key #'car-safe :start 1))
              (declare-block (nth declare-pos args-and-body)))
    (cl-find 'et declare-block :key #'car-safe)))

(defun et--lambda-parameter-types (function-type arglist)
  "Return local parameter variables for FUNCTION-TYPE and ARGLIST.

The input type of FUNCTION-TYPE is matched against the same argument
matcher used for declarations.  Its inferred generic values are the
types of ARGLIST's required, optional, keyword, and rest parameters."
  (when-let* ((parts (et--lambda-function-parts function-type)))
    (pcase-let* ((`(,required ,optional ,keys ,rest)
                  (et--parse-arglist-params arglist))
                 (params (append required optional keys rest))
                 (generics (cl-loop for _param in params collect (et--lambda-generic)))
                 (param-reprs
                  (cl-loop for param in params
                           for generic in generics
                           collect (cons param
                                         (make-et-repr :generics generics
                                                       :dnf (list (list (list 'S:GENERIC generic)))))))
                 (take-group
                  (lambda (group)
                    (cl-loop for param in group collect (assq param param-reprs))))
                 (matcher
                  (et--generate-func-input
                   t generics nil
                   (funcall take-group required)
                   (funcall take-group optional)
                   (funcall take-group keys)
                   (funcall take-group rest)))
                 (match (et-sub-match matcher (car parts))))
      (when (et-match-result-success match)
        (cl-loop for param in params
                 for type in (et-match-result-value match)
                 collect (et-new-var param type))))))

(defun et--lambda-check-declared (args-and-body)
  "Check a lambda with an ET declaration in ARGS-AND-BODY."
  (if-let* ((sig (et-at 1 (et--parse-function-signature args-and-body))))
      (et--with-scoped-datatypes (et-func-sig-scoped sig)
        (et-with-vars (et-func-sig-vars sig)
          ;; The body runs whenever the lambda is called, not here
          (et-checker-deferred
            (let* ((actual-return (et-checker-tail (1+ (et-func-sig-source-pos sig))))
                   (expected-return (et-func-sig-expected-return sig)))
              (or (et-subtype? actual-return expected-return)
                  (et-err 0 "Expected %s, found %s" expected-return actual-return))))
          (et-func-sig-func-type sig)))
    ;; A declaration without @return still owns the parameter types.  Its
    ;; return type is inferred, just as an unannotated cl-flet binding is.
    (pcase-let* ((`(,decls . ,source-pos)
                  (et-at 1 (et--find-function-declarations args-and-body)))
                 (input (et--func-decls-to-input decls))
                 (vars
                  (cl-loop for group in (et--func-declarations-param-reprs decls)
                           nconc (cl-loop for (name . repr) in group
                                          collect (et-new-var name (et-repr-to-type repr nil))))))
      (if (et-matcher-p input)
          (progn
            (et-err 0 "A generic lambda must declare an `@return' type")
            (et-checker-deferred
              (et-with-vars (mapcar (lambda (var) (et-new-var (et-var-name var) (et-never))) vars)
                (et-checker-tail (1+ source-pos))))
            (et-never))
        (et-dt 'Function input
               (et-checker-deferred
                 (et-with-vars vars (et-checker-tail (1+ source-pos)))))))))

(defun et--lambda-check-recommended (arglist recommendation)
  "Check an undeclared lambda using its recommended FUNCTION type."
  (if-let* ((parts (et--lambda-function-parts recommendation)))
      (if-let* ((vars (et--lambda-parameter-types recommendation arglist)))
          (let* ((actual-return (et-checker-deferred
                                  (et-with-vars vars (et-checker-tail 2))))
                 (expected-return (cdr parts)))
            (or (et-subtype? actual-return expected-return)
                (et-err 0 "Expected %s, found %s" expected-return actual-return))
            recommendation)
        (et-err 0 "Lambda argument list %s is incompatible with recommended input type %s"
                arglist (car parts))
        (et-checker-deferred
          (et-with-vars (mapcar (lambda (param) (et-new-var param (et-never)))
                                (apply #'append (et--parse-arglist-params arglist)))
            (et-checker-tail 2)))
        (et-never))
    (et-err 0 "Expected a concrete Function type recommendation")
    (et-checker-deferred
      (et-with-vars (mapcar (lambda (param) (et-new-var param (et-never)))
                            (apply #'append (et--parse-arglist-params arglist)))
        (et-checker-tail 2)))
    (et-never)))

(defun et--lambda-inferred-type (arglist)
  "Infer a lambda's type with unannotated parameters.

Required, optional, and keyword parameters are `Any'; a rest parameter
is `ListR<Any>'."
  (pcase-let* ((`(,required ,optional ,keys ,rest)
                (et--parse-arglist-params arglist))
               (ordinary-params (append required optional keys))
               (any-param-reprs
                (lambda (params)
                  (cl-loop for param in params collect (cons param (et-repr Any)))))
               (rest-param-reprs
                (cl-loop for param in rest collect (cons param (et-repr ListR<Any>))))
               (input
                (et--generate-func-input nil nil nil
                                         (funcall any-param-reprs required)
                                         (funcall any-param-reprs optional)
                                         (funcall any-param-reprs keys)
                                         rest-param-reprs))
               (vars
                (append (mapcar (lambda (param) (et-new-var param (et-any))) ordinary-params)
                        (mapcar (lambda (param) (et-new-var param (et-alias 'ListR (et-any)))) rest))))
    (et-dt 'Function input (et-checker-deferred
                             (et-with-vars vars (et-checker-tail 2))))))

(et-define-pcase-checker lambda `(,(and arglist (pred listp)) . ,body)
  (let* ((args-and-body (cons arglist body)))
    (cond
     ((et--lambda-has-declaration-p args-and-body)
      (et--lambda-check-declared args-and-body))
     (et--checker-recommendation
      (et--lambda-check-recommended arglist et--checker-recommendation))
     (t
      (et--lambda-inferred-type arglist)))))


;;;; Tests

(et-test
 (et-assert-resolve Function<Args<Integer>~String>
   (lambda (x)
     (declare (et (x Integer) (@return String)))
     (format "%s" x)))

 (et-assert-resolve Function<Args<Integer>~String>
   (et: Function<Args<Integer>~String>
     (lambda (x) (format "%s" x))))

 (et-assert-resolve-errors
  (et: Function<Args<Integer>~String>
    (lambda (x) x)))

 (et-assert-resolve Function<Args<Any>~Any>
   (lambda (x) x)))


;;; lambda.el ends here
