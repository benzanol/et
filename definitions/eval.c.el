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
     (if-let* ((func-type (et-function-type sym)))
         func-type
       (et-err nil "No function type for `%s'" sym)))

    ;; Anything else is invalid
    (_ (et-err nil "Invalid argument to function: %s" inner))))

(et-test
 (et-assert-resolve Function<ConsR<Integer~Nil>~Integer>
   #'(lambda ([x Integer]) x))

 (not (et-never-p (et-result-value (et-result-boundary
                                    (et--check-result-type (et--check '#'+ nil nil))))))
 (et-never-p (et-result-value (et-result-boundary
                               (et--check-result-type (et--check '#'function nil nil))))))


;;; Control flow
;;;; let*

(et-define-pcase-checker let*
    `(,(et-* [(var vars)]
             (and
              (or
               ;; Derive type
               (and `(,name ,_val)
                    (let type (et-with-vars vars (et-checker-sub (list 1 (length vars) 1)))))
               ;; No value (nil variable)
               (and name (pred symbolp)
                    (let type (et Nil))))
              ;; Add the var to the list
              (let var (et-new-var name (et--unfreshen-type (or type (et-never)))))))
      . ,_body)

  (et-with-vars vars
    (et-checker-tail 2)))


;;;; setq

(et-define-pcase-checker setq
    (and args
         (guard (eq 0 (mod (length args) 2)))
         (guard (cl-loop for (var-sym _val) on args by #'cddr
                         always (symbolp var-sym))))
  (et--setf-checker))


;;;; and/or

(defun et--and-return-type (cond-type checker)
  ;; CHECKER only runs when the accumulated condition was non-nil, so it
  ;; is checked in a branch against "the `and' already exited with nil".
  ;; When the condition cannot be nil, the exited branch is Never and the
  ;; branch machinery treats CHECKER as always running.
  (let* ((non-nil-binds (et--type-binds (et--non-nil cond-type)))
         (output-type (et-never)))
    (et-checker-branches
     (lambda ()
       (cl-callf et--narrows-and et--checker-narrows non-nil-binds)
       (setq output-type (funcall checker)))
     (lambda () (et--supersect cond-type (et Nil))))

    (let* ((output-non-nil (et--non-nil output-type))
           ;; If `and' returns non-nil, then both non-nil binds will be true (intersect them)
           (merged-non-nil-binds
            (et--intersect-binds nil non-nil-binds (et--type-binds output-non-nil))))
      (et--or (et--replace-type-binds output-non-nil merged-non-nil-binds)
              ;; If `and' returns nil, it could be from either `cond-type' OR `output-type' being nil
              (et--supersect cond-type (et Nil))
              (et--supersect output-type (et Nil))))))

(et-test
 (equal (et $a::2&True)
        (et--and-return-type (et $a::{1|2}&True) (lambda () (et $a::{2|3}&True)))))

(defun et--or-return-type (cond-type checker)
  ;; CHECKER only runs when the accumulated condition was nil, so it is
  ;; checked in a branch against "the `or' already exited non-nil".
  (let* ((nil-binds (et--type-binds (et--supersect cond-type (et Nil))))
         (output-type (et-never)))
    (et-checker-branches
     (lambda ()
       (cl-callf et--narrows-and et--checker-narrows nil-binds)
       (setq output-type (funcall checker)))
     (lambda () (et--non-nil cond-type)))

    (let* ((output-nil (et--supersect output-type (et Nil)))
           ;; If `or' returns nil, then both nil binds will be true (intersect them)
           (merged-nil-binds
            (et--intersect-binds nil nil-binds (et--type-binds output-nil))))
      (et--or (et--replace-type-binds output-nil merged-nil-binds)
              ;; If `or' returns non-nil, it could be from either `cond-type' OR `output-type'
              (et--non-nil cond-type)
              (et--non-nil output-type)))))

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
  (let* ((cond-type (et-checker-sub 1)))
    (et-checker-hint-narrows
     0
     "IF:\\n%s" (et--non-nil cond-type)
     "ELSE:\\n%s" (et--supersect cond-type (et Nil)))

    (et-simplify-type
     (et-checker-sub-cond cond-type
                          (lambda () (et-checker-sub 2))
                          (lambda () (et-checker-tail 3))))))

(et-test
 (et-subtype? (et-result-value
               (et-typecheck
                (let* ((a String|Number 4))
                  (if (stringp a) a "hello!"))))
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
                    (let type (et-checker-sub (list 1 (length vars) 1))))
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
 (et-subtype? (et-result-value
               (et-typecheck
                (let* ((a Integer 1))
                  (let ((a "s") (b a)) b))))
              (et Integer)))


;;;; progn / prog1

(et-declare
 (@macro progn :progn t)
 (@macro prog1 :prog1 t))

(et-test
 (et-assert-resolve Integer (progn 1))
 (et-assert-resolve String (progn 1 2 "x"))
 (et-assert-resolve Nil (progn)))

(et-test
 (et-assert-resolve Integer (prog1 1 "x" 'y)))


;;;; cond

;; Each clause is (CONDITION BODY...).  The result is the union of every
;; clause's result (the last BODY form, or CONDITION itself when there is
;; no body), plus nil for the case where no clause succeeds.
;;
;; A cond is checked exactly as its nested-if equivalent:
;;
;;   (cond (c1 b1) (c2 b2))  ==  (if c1 b1 (if c2 b2 nil))
;;
;; So each clause's body sees its condition's non-nil narrowing, and each
;; subsequent condition (and body) is checked inside the previous
;; condition's nil branch -- the accumulated negative narrowing falls out
;; of the recursion.
(defun et--cond-clauses-type (clauses idx)
  "Return the type of the remaining cond CLAUSES, starting at path IDX."
  (pcase clauses
    ('nil (et Nil))
    (`(,clause . ,rest)
     (let* ((cond-type (et-checker-sub (list idx 0))))
       (et-checker-sub-cond cond-type
                            (lambda ()
                              (if (cdr clause) (et-checker-tail idx 1)
                                ;; No body: returns the non-nil part of the condition
                                (et--non-nil cond-type)))
                            (lambda () (et--cond-clauses-type rest (1+ idx))))))))

(et-define-pcase-checker cond clauses
  (et-simplify-type (et--cond-clauses-type clauses 1)))

(et-test
 (et-assert-resolve Integer|String|Nil (cond (1 1) (2 "x")))
 (et-assert-resolve String|Nil (cond (1 "x")))
 (et-assert-resolve Nil (cond))
 ;; Positive narrowing inside a clause body, like `if': `a' is narrowed to
 ;; String, so Number cannot appear in the result.
 (et-subtype? (et-result-value
               (et-typecheck
                (let* ((a String|Number 4))
                  (cond ((stringp a) a) (t "hello!")))))
              (et String|Nil))
 ;; Negative narrowing: in the second clause, `a' is narrowed to non-String
 ;; (Number), so the result cannot include String from the second branch.
 (et-subtype? (et-result-value
               (et-typecheck
                (let* ((a String|Number 4))
                  (cond ((stringp a) a) (t a)))))
              (et Number|Nil)))


;;;; while

;; A `while' form always evaluates to nil; test and body are still checked.
;;
;; The test and the body may run any number of times, so both are checked
;; inside `et-checker-loop-body': its discovery pass finds the vars the
;; loop can change and drops their narrows, so iteration N never trusts a
;; narrow iteration N-1 may have broken (including a narrow on a variable
;; assigned from another variable whose own narrow dies later in the
;; body). The test is checked inside the loop -- it re-runs before every
;; iteration -- and its non-nil narrowing holds at the top of the body.
;;
;; On exit, the final test evaluated to nil, so its nil narrowing is
;; applied after the loop.
(et-define-pcase-checker while `(,_test . ,_body)
  (let* ((last-test (et Any)))
    (et-checker-loop-body
     (lambda ()
       (setq last-test (et-checker-sub 1))
       (et-checker-hint-narrows 0 "WHILE:\\n%s" (et--non-nil last-test))
       (cl-callf et--narrows-and et--checker-narrows
         (et--type-binds (et--non-nil last-test)))
       (et-checker-remaining 2)))

    ;; The loop only exits once the test is nil
    (cl-callf et--narrows-and et--checker-narrows
      (et--type-binds (et--supersect last-test (et Nil)))))

  (et Nil))

(et-test
 (et-assert-resolve Nil (while nil "body"))
 ;; The body sees the test's narrows: the test runs before every iteration
 (et-assert-resolve Nil
   (let* ((a String|Number 4))
     (while (stringp a) (:assert-subtype a String))))
 ;; A narrow from above the loop survives if the body never invalidates it
 (et-assert-resolve Nil
   (let* ((a String|Number 4))
     (when (stringp a)
       (while t (:assert-subtype a String)))))
 ;; ...but not if the body assigns the variable: the assignment has already
 ;; happened by the time the body is re-entered
 (et-assert-resolve-errors
  (let* ((a String|Number 4))
    (when (stringp a)
      (while t (:assert-subtype a String) (setq a 5)))))
 ;; A narrow killed by the body invalidates the narrows that were derived from
 ;; it earlier in that same body: `b' is assigned from `a' while `a' still looks
 ;; narrowed, but `a's narrow is killed further down, so `b's must go too
 (et-assert-resolve-errors
  (let* ((a String|Integer 0)
         (b String|Integer 0))
    (when (and (integerp a) (integerp b))
      (while t
        (:assert-subtype b Integer)
        (setq b a)
        (setq a "s"))))))


;;; Nonlocal exit
;;;; catch / throw / unwind-protect

;; `catch' returns either the last body form's value or whatever a matching
;; `throw' delivers -- and a throw can carry any value -- so its result is
;; `Any'.  The tag and body are still checked.
(et-define-pcase-checker catch `(,_tag . ,_body)
  (et-checker-sub 1)
  ;; A `throw' can exit the body at any point, so no narrow the body
  ;; establishes is guaranteed afterwards.
  (et-checker-escapable (lambda () (et-checker-remaining 2)))
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

(et-declare
 (@function funcall (function &rest arguments)
            (@generics [A R])
            (function (fn A R))
            (arguments A)
            (@return R)))

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
