;;; Functions
;;;; lambda

;; (et-define-pcase-checker lambda body
;;   (setq body (et--funcdef-inline-to-declare (copy-tree body)))
;;   (et-checker-function-body (et-parse-function-type body #'lambda) 1))

;; (et-test
;;  ;; Inline typed args
;;  (et-assert-resolve Function<ConsR<Integer~Nil>~Integer>
;;    (lambda ([x Integer]) (+ x 1)))

;;  ;; Untyped args default to Any
;;  (et-assert-resolve Function<ConsR<Any~Nil>~Any>
;;    (lambda (x) x))

;;  ;; &optional — cdr is Nil|ConsR<String~Nil>
;;  (et-assert-resolve (Function ConsR<Integer~Nil|ConsR<String~Nil>> Integer)
;;    (lambda ([x Integer] &optional [y String]) x))

;;  ;; Multiple body forms — return type is last
;;  (et-assert-resolve Function<ConsR<Integer~ConsR<String~Nil>>~String>
;;    (lambda ([x Integer] [y String]) (+ x 1) y))

;;  ;; Empty arglist
;;  (et-assert-resolve Function<Nil~Integer>
;;    (lambda () 1))

;;  ;; &rest
;;  (et-assert-resolve (Function ConsR<Integer~ListR<Any>> Integer)
;;    (lambda ([x Integer] &rest args) x))
;;  (et-assert-resolve (Function ConsR<Any~ListR<Any>> ListR<Any>)
;;    (lambda (x &rest args) args)))

;; (et-test
;;  ;; Body return type with typed args
;;  (et-assert-resolve Function<ConsR<Integer~Nil>~Integer>
;;    (lambda ([x Integer]) (+ x 1)))

;;  ;; Untyped args default to Any, body uses them
;;  (et-assert-resolve Function<ConsR<Any~Nil>~Any>
;;    (lambda (x) x))

;;  ;; &optional arg becomes Nil|Type
;;  (et-assert-resolve Function<ConsR<Integer~Nil|ConsR<String~Nil>>~Integer>
;;    (lambda ([x Integer] &optional [y String]) (+ x 1)))

;;  ;; Multiple body forms, return type is last
;;  (et-assert-resolve Function<ConsR<Integer~ConsR<String~Nil>>~String>
;;    (lambda ([x Integer] [y String]) (+ x 1) y))

;;  ;; Empty arglist
;;  (et-assert-resolve Function<Nil~Integer>
;;    (lambda () 1)))

;; Return type annotation tests
;; (et-test
;;  ;; Inline -> return type: uses declared type
;;  (et-assert-resolve Function<ConsR<Integer~Nil>~Number>
;;    (lambda ([x Integer]) -> Number (+ x 1)))

;;  ;; Inline -> with typed args
;;  (et-assert-resolve Function<ConsR<Integer~Nil>~Number>
;;    (lambda ([x Integer]) -> Number x))

;;  ;; Inline -> is stripped from compiled output
;;  (let* ((result (et--check '(lambda (x) -> Integer 1))))
;;    (equal (et-result-compiled result) '(lambda (x) 1)))

;;  ;; Inline -> with body type mismatch produces error
;;  (et-assert-resolve-errors
;;   (lambda ([x Integer]) -> String x))

;;  ;; Inline -> with empty arglist
;;  (et-assert-resolve Function<Nil~Number>
;;    (lambda () -> Number 1))

;;  ;; Inline -> with multiple body forms
;;  (et-assert-resolve Function<ConsR<Integer~Nil>~Number>
;;    (lambda ([x Integer]) -> Number "ignored" x)))


;;;; quote

(et-define-pcase-checker quote `(,expr)
  (et-literal expr))

(et-test
 (et-assert-resolve Integer '1)
 (et-assert-resolve Number '1.1)
 (et-assert-resolve String '"hi")
 (et-assert-resolve Symbol 'a)
 (et-assert-no-resolve Integer '1.1)
 (et-assert-no-resolve Integer ''1)
 (et-assert-no-resolve Number ''1.1)
 (et-assert-no-resolve String ''"hi")
 (et-assert-no-resolve Symbol ''a)

 (et-assert-resolve ConsR<Any~Any> '(1 2 3))
 (et-assert-resolve ListR<Symbol> '(a b c))
 (et-assert-resolve ListR<Integer> '())
 (et-assert-no-resolve ListR<Integer> '(1 2 '3))
 (et-assert-no-resolve ListR<Integer> '(1 2 3.3))
 (et-assert-no-resolve ListR<Integer> ''(1 2 3))
 (et-assert-no-resolve ListR<Integer> ''())

 (et-assert-resolve ConsR<Integer~Integer> '(1 . 2))
 (et-assert-no-resolve ConsR<Integer~Integer> '(1 . 2.2))
 (et-assert-no-resolve ConsR<Integer~Integer> '(1.1 . 2))
 (et-assert-resolve ConsR<Symbol~ListR<String>> '(a "2" "3")))


;;;; function

(et-define-pcase-checker function `(,inner)
  (pcase inner
    ;; Lambda expression: typecheck it as a lambda
    (`(lambda . ,_rest)
     (et-checker-sub 1))

    ;; Symbol: look up its function type
    ((and sym (pred symbolp))
     (if-let* ((func-type (get sym 'et-function-type)))
         func-type
       (et-err nil "No function type for `%s'" sym)))

    ;; Anything else is invalid
    (_ (et-err nil "Invalid argument to function: %s" inner))))

(et-test
 (et-assert-resolve Function<ConsR<Integer~Nil>~Integer>
   #'(lambda ([x Integer]) x))

 (not (et-never-p (et-result-type (et--check '#'+))))
 (et-never-p (et-result-type (et--check '#'function))))


;;; Control flow
;;;; let*

(et-define-pcase-checker let*
    `(,(et-* [(var vars)]
             (and
              (or
               ;; Derive type
               (and `(,name ,_val)
                    (let type (et-with-vars vars (et-checker-sub 1 (length vars) 1))))
               ;; No value (nil variable)
               (and name (pred symbolp)
                    (let type (et Nil))))
              ;; Add the var to the list
              (let var (et-new-var name (et--unfreshen-type (or type (et-never)))))))
      . ,_body)

  (et-with-vars vars
    (et-checker-tail 2)))


;;;; setq

(et-define-pcase-checker setq (and args (guard (eq 0 (mod (length args) 2))))
  (cl-loop for (var _val) on args by #'cddr
           for var-pos upfrom 1 by 2
           for type = (or (et-get-symbol-type var)
                          (et-err var-pos "Assignment to free variable"))
           do (et-checker-resolve type (1+ var-pos))
           finally return type))


;;;; and/or

(defun et--and-return-type (cond-type checker)
  ;; The next case will only get evaluated if all previous were non-nil
  (let* ((non-nil-binds (et--type-binds (et--non-nil cond-type)))
         (output-type (et-with-narrow-binds non-nil-binds (funcall checker)))

         (output-non-nil (et--non-nil output-type))
         ;; If `and' returns non-nil, then both non-nil binds will be true (intersect them)
         (merged-non-nil-binds
          (et--intersect-binds nil non-nil-binds (et--type-binds output-non-nil))))

    (et--or (et--replace-type-binds output-non-nil merged-non-nil-binds)
            ;; If `and' returns nil, it could be from either `cond-type' OR `output-type' being nil
            (et--supersect cond-type (et Nil))
            (et--supersect output-type (et Nil)))))

(et-test
 (equal (et $a::2&True)
        (et--and-return-type (et $a::{1|2}&True) (lambda () (et $a::{2|3}&True)))))

(defun et--or-return-type (cond-type checker)
  ;; The next case will only get evaluated if all previous were nil
  (let* ((nil-binds (et--type-binds (et--supersect cond-type (et Nil))))
         (output-type (et-with-narrow-binds nil-binds (funcall checker)))

         (output-nil (et--supersect output-type (et Nil)))
         ;; If `or' returns nil, then both nil binds will be true (intersect them)
         (merged-nil-binds
          (et--intersect-binds nil nil-binds (et--type-binds output-nil))))

    (et--or (et--replace-type-binds output-nil merged-nil-binds)
            ;; If `or' returns non-nil, it could be from either `cond-type' OR `output-type'
            (et--non-nil cond-type)
            (et--non-nil output-type))))

(et-define-pcase-checker and args
  (cl-loop with acc-type = (et-literal t)
           for pos upfrom 1 to (length args)
           do (cl-callf et--and-return-type acc-type (lambda () (et-checker-sub pos)))
           finally return (et-simplify-type acc-type)))

(et-define-pcase-checker or args
  (cl-loop with acc-type = (et Nil)
           for pos upfrom 1 to (length args)
           do (cl-callf et--or-return-type acc-type (lambda () (et-checker-sub pos)))
           finally return (et-simplify-type acc-type)))


;;;; if

(et-define-pcase-checker if `(,_cond ,_then . ,_else)
  (let* ((cond-type (et-checker-sub 1))

         ;; Then branch: cond was non-nil
         (non-nil-cond (et--non-nil cond-type))
         (non-nil-binds (et--type-binds non-nil-cond))
         (then-type (et-with-narrow-binds non-nil-binds
                      (et-checker-sub 2)))

         ;; Else branch: cond was nil
         (nil-cond (et--supersect cond-type (et Nil)))
         (nil-binds (et--type-binds nil-cond))
         (else-type (et-with-narrow-binds nil-binds
                      (et-checker-tail 3))))

    (et-checker-hint-narrows
     0
     "IF:\\n%s" non-nil-cond
     "ELSE:\\n%s" nil-cond)

    (et-simplify-type (et--or then-type else-type))))

(et-test
 (et-subtype? (et-typecheck
               (let* ((a String|Number 4))
                 (if (stringp a) a "hello!")))
              (et String)))


;;;; let

;; Like `let*', but the value forms are all evaluated in the *outer*
;; scope (parallel binding), so earlier bindings are not visible while
;; deriving later values.  Only the body sees all the bindings.
(et-define-pcase-checker let
    `(,(et-* [(var vars)]
             (and
              (or
               ;; Derive type (evaluated outside the binding scope)
               (and `(,name ,_val)
                    (let type (et-checker-sub 1 (length vars) 1)))
               ;; No value (nil variable)
               (and name (pred symbolp)
                    (let type (et Nil))))
              ;; Add the var to the list
              (let var (et-new-var name (et--unfreshen-type (or type (et-never)))))))
      . ,_body)

  (et-with-vars vars
    (et-checker-tail 2)))

(et-test
 ;; Body sees the bindings
 (et-assert-resolve Integer (let ((a 1) (b 2)) (+ a b)))
 ;; Value forms do NOT see earlier bindings in the same `let'
 (et-subtype? (et-typecheck
               (let* ((a Integer 1))
                 (let ((a "s") (b a)) b)))
              (et Integer)))


;;;; progn / prog1

;; `progn' returns the value of the last body form (or nil if empty).
(et-define-checker progn
  (et-checker-tail 1))

(et-test
 (et-assert-resolve Integer (progn 1))
 (et-assert-resolve String (progn 1 2 "x"))
 (et-assert-resolve Nil (progn)))

;; `prog1' returns the value of FIRST, evaluating (and checking) the rest.
(et-define-pcase-checker prog1 `(,_first . ,_body)
  (prog1 (et-checker-sub 1)
    (et-checker-remaining 2)))

(et-test
 (et-assert-resolve Integer (prog1 1 "x" 'y)))


;;;; cond

;; Each clause is (CONDITION BODY...).  The result is the union of every
;; clause's result (the last BODY form, or CONDITION itself when there is
;; no body), plus nil for the case where no clause succeeds.  Inside a
;; clause's body, CONDITION is narrowed to its non-nil part, like `if'.
(et-define-pcase-checker cond clauses
  (cl-loop for clause in clauses
           for idx upfrom 1
           for cond-type = (et-checker-sub idx 0)
           for branch-type =
           (if (cdr clause)
               ;; Has body: narrow by non-nil condition, return last form
               (et-with-narrow-binds (et--type-binds (et--non-nil cond-type))
                 (et-checker-tail idx 1))
             ;; No body: returns the non-nil part of the condition
             (et--non-nil cond-type))
           collect branch-type into branch-types
           finally return (et-simplify-type
                           (apply #'et--or (et Nil) branch-types))))

(et-test
 (et-assert-resolve Integer|String|Nil (cond (1 1) (2 "x")))
 (et-assert-resolve String|Nil (cond (1 "x")))
 (et-assert-resolve Nil (cond))
 ;; Narrowing inside a clause body, like `if': `a' is narrowed to String,
 ;; so Number cannot appear in the result.
 (et-subtype? (et-typecheck
               (let* ((a String|Number 4))
                 (cond ((stringp a) a) (t "hello!"))))
              (et String|Nil)))


;;;; while

;; A `while' form always evaluates to nil; test and body are still checked.
(et-define-pcase-checker while `(,_test . ,_body)
  (et-checker-sub 1)
  (et-checker-remaining 2)
  (et Nil))


;;; Nonlocal exit
;;;; catch / throw / unwind-protect

;; `catch' returns either the last body form's value or whatever a matching
;; `throw' delivers -- and a throw can carry any value -- so its result is
;; `Any'.  The tag and body are still checked.
(et-define-pcase-checker catch `(,_tag . ,_body)
  (et-checker-sub 1)
  (et-checker-remaining 2)
  (et Any))

;; `throw' performs a nonlocal exit and never returns normally.
(et-define-type-checker throw (Args Any Any) Never)

;; `unwind-protect' returns the value of BODYFORM; UNWINDFORMS are checked
;; but their values discarded.
(et-define-pcase-checker unwind-protect `(,_body . ,_unwinds)
  (prog1 (et-checker-sub 1)
    (et-checker-remaining 2)))


;;; Funcall

(et-define-checker apply
  (let* ((func-type (et-checker-sub 1))
         (args-type (et--tailed-tuple 'ConsR (et-checker-remaining 2)))
         (output-type (et-checker-funcall func-type args-type)))
    (if (et-type-p output-type) output-type
      (et-err 0 "Cannot apply %s with args %s" func-type args-type))))

(et-define-checker funcall
  (let* ((func-type (et-checker-sub 1))
         (args-type (et--tuple 'ConsR (et-checker-remaining 2)))
         (output-type (et-checker-funcall func-type args-type)))
    (if (et-type-p output-type) output-type
      (et-err 0 "Cannot call %s with args %s" func-type args-type))))

(et-test
 (et-assert-resolve Integer (funcall (lambda ([x Integer] [y Integer]) x) 1 2))
 (et-assert-resolve-errors (funcall (lambda ([x Integer] [y Integer]) x) 1 2.5))
 (et-assert-resolve-errors (funcall (lambda ([x Integer] [y Integer]) x) 1))
 (et-assert-resolve-errors (funcall (lambda ([x Integer]) x) 1 2))
 (et-assert-resolve-errors (funcall (lambda () x) 1))
 (et-assert-resolve-errors (funcall (lambda ([x Integer]) x)))

 (et-assert-resolve Integer (apply (lambda ([x Integer] [y Integer] [z Integer]) x) (list 1 2 3)))
 (et-assert-resolve Integer (apply (lambda ([x Integer] [y Integer] [z Integer]) x) 1 2 (list 3)))
 (et-assert-resolve Integer (apply (lambda ([x Integer] [y Integer] [z Integer]) x) 1 2 3 nil))
 (et-assert-resolve-errors (apply (lambda ([x Integer] [y Integer] [z Integer]) x) 1 2 (list 3 4)))
 (et-assert-resolve-errors (apply (lambda ([x Integer] [y Integer] [z Integer]) x) 1 (list 3)))
 (et-assert-resolve-errors (apply (lambda ([x Integer] [y Integer] [z Integer]) x) (list 3)))

 (et-assert-resolve Integer (apply (lambda () 0) nil))
 (et-assert-resolve-errors (apply (lambda () 0) 1 nil))

 (et-assert-resolve Integer (apply #'+ 1 2 (list 3 4)))
 (et-assert-resolve Integer (apply #'+ 1 2 nil))
 (et-assert-resolve Number (apply #'+ 1 2 (list 3 4.4 5)))
 (et-assert-resolve 0 (apply #'+ nil))
 (et-assert-resolve-errors (apply #'+ 1 2 (list "3")))
 (et-assert-resolve-errors (apply #'+ 1 2 3)))
