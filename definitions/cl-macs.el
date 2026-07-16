;;;; Function declarations

(et-declare
 (@function cl-assert (assertion &optional show-args string &rest args)
            (assertion Any) (show-args Any) (string String|Nil) (args ListR<Any>)
            (@return Nil))

 (@function cl-position (item sequence &key test test-not key start end from-end)
            (@generics [E K])
            (item Any)
            (sequence (or (and String (set E Integer)) ListR<E> VectorR<E>))
            (test (or Nil (Function (Args Any K) Any)))
            (test-not (or Nil (Function (Args Any K) Any)))
            (key (or Nil (Function (Args E) K)))
            (start Integer|Nil) (end Integer|Nil) (from-end Any)
            (@return Integer|Nil)))

(et-test
 (et-assert-resolve Integer|Nil (cl-position 2 (list 1 2 3)))
 (et-assert-resolve Integer|Nil
   (cl-position 'b (list (cons 'a 1) (cons 'b 2)) :key #'car :test #'eq))
 (et-assert-resolve Integer|Nil
   (cl-position ?b "abc" :start 1 :from-end t))
 (et-assert-resolve-errors (cl-position 1 (list 1) :start "0")))


;;;; Keywords

(defvar et--loop-keywords
  '(for as with do doing initially finally repeat
        while until always never thereis
        collect collecting append appending nconc nconcing
        concat vconcat count counting sum summing
        maximize maximizing minimize minimizing
        if when unless else and end
        into named return)
  "All recognized `cl-loop' clause keywords.")

(defun et--loop-keyword-p (sym)
  "Return non-nil if SYM is a recognized `cl-loop' keyword."
  (memq sym et--loop-keywords))

(defun et--loop-infer-elem-type (list-type)
  "Infer the element type of LIST-TYPE for `for VAR in LIST'.
Returns the element type, or Any if undetermined."
  (or (et-checker-infer list-type [T] ListR<T> T)
      (et-any)))

(defun et--loop-infer-vector-elem-type (vec-type)
  "Infer the element type of VEC-TYPE for `for VAR across ARRAY'.
Handles both vectors (element type) and strings (Integer char codes)."
  (or (et-checker-infer vec-type [T] VectorR<T> T)
      (and (et-subtype? vec-type (et String)) (et Integer))
      (et-any)))

(defun et--loop-numeric-var-type (bound-types)
  "Determine the loop variable type from numeric BOUND-TYPES.
Integer if all bounds are Integer, otherwise Number."
  (if (cl-every (lambda (bt) (et-subtype? bt (et Integer))) bound-types)
      (et Integer)
    (et Number)))


;;;; Accumulators

;; A cl-loop has a single implicit accumulator, plus one named
;; accumulator per `into VAR'. Every bare accumulation clause feeds the
;; implicit accumulator, and they must all belong to the same category
;; (you cannot `collect' and `sum' into the same place). An
;; `et--loop-acc' tracks the mutable state of one such accumulator.

(cl-defstruct et--loop-acc
  "Mutable state of one cl-loop accumulator (the bare one or an `into VAR')."
  (cat nil)        ; nil | `list' | `numeric' | `string' | `vector'
  (elems nil)      ; contributed element/value types (to be unioned)
  (nonfresh nil))  ; t once an `nconc' splices foreign conses (not fresh)

(defun et--loop-accum-category (kw)
  "Return the accumulation category of clause keyword KW."
  (pcase kw
    ((or 'collect 'collecting 'append 'appending 'nconc 'nconcing) 'list)
    ((or 'count 'counting 'sum 'summing
         'maximize 'maximizing 'minimize 'minimizing) 'numeric)
    ('concat 'string)
    ('vconcat 'vector)))

(defun et--loop-acc-add (acc kw form-type pos)
  "Record an accumulation of clause KW with FORM-TYPE into ACC.
POS is the clause index of the form, for error reporting."
  ;; The accumulated value is no longer the loop variable, so drop any
  ;; narrowing binds (e.g. `{typeof x}') it picked up: they are
  ;; meaningless once detached from the variable, and leaving them inside
  ;; an accumulator alias breaks later alias expansion when an `into VAR'
  ;; is referenced.
  (setq form-type (et--remove-type-binds form-type))
  (let ((cat (et--loop-accum-category kw)))
    (cond
     ((null (et--loop-acc-cat acc)) (setf (et--loop-acc-cat acc) cat))
     ((not (eq (et--loop-acc-cat acc) cat))
      (et-err (1+ pos) "Cannot combine a `%s' clause with `%s' accumulation"
              kw (et--loop-acc-cat acc))))
    (pcase cat
      ('list
       (pcase kw
         ;; collect: the form is itself one element
         ((or 'collect 'collecting) (push form-type (et--loop-acc-elems acc)))
         ;; append/nconc: the form is a list spliced in element-wise
         (_ (push (et--loop-infer-elem-type form-type) (et--loop-acc-elems acc))
            (when (memq kw '(nconc nconcing))
              (setf (et--loop-acc-nonfresh acc) t)))))
      ;; count yields an Integer regardless of the form; the rest add the form
      ('numeric (push (if (memq kw '(count counting)) (et Integer) form-type)
                      (et--loop-acc-elems acc)))
      ('vector (push form-type (et--loop-acc-elems acc)))
      ('string nil))))

(defun et--loop-acc-type (acc)
  "Compute the result type accumulated in ACC, or nil if ACC is empty."
  (pcase (et--loop-acc-cat acc)
    ('nil nil)
    ('list
     (let ((elem (apply #'et--or (et--loop-acc-elems acc))))
       ;; nconc shares the body's conses; collect/append build fresh spines
       (et-alias (if (et--loop-acc-nonfresh acc) 'List 'ListFresh) elem)))
    ('numeric
     (if (cl-every (lambda (ty) (et-subtype? ty (et Integer))) (et--loop-acc-elems acc))
         (et Integer) (et Number)))
    ('string (et String))
    ('vector (et-dt 'VectorFresh (apply #'et--or (et--loop-acc-elems acc))))))


;;;; Walker state

;; The walker threads a cursor over the flat clause list, plus the
;; loop-global accumulation/return state. These are genuinely
;; loop-global, so they are dynamically scoped and mutated in place;
;; lexical recursion is reserved for the things that are actually
;; lexically scoped: variable bindings (`et--binds') and conditional
;; narrowing (`et--checker-narrows').

(defvar et--loop-clauses nil "Clause list of the cl-loop being checked.")
(defvar et--loop-len 0 "Length of `et--loop-clauses'.")
(defvar et--loop-pos 0 "Cursor index into `et--loop-clauses'.")
(defvar et--loop-bare nil "The bare implicit accumulator, an `et--loop-acc'.")
(defvar et--loop-into nil "Alist of NAME -> `et--loop-acc' for `into VAR'.")
(defvar et--loop-returns nil "Types contributed by `return' clauses.")
(defvar et--loop-thereis nil "Types contributed by `thereis' clauses.")
(defvar et--loop-bool nil "Non-nil if an `always'/`never' clause is present.")
(defvar et--loop-finally nil "The `finally return' type, overriding all else.")


;;;; Cursor

(defun et--loop-peek ()
  "Return the clause at the cursor without advancing."
  (and (< et--loop-pos et--loop-len) (nth et--loop-pos et--loop-clauses)))

(defun et--loop-advance ()
  "Return the clause at the cursor and advance past it."
  (prog1 (et--loop-peek) (cl-incf et--loop-pos)))

(defun et--loop-eat (&rest kws)
  "If the cursor is on one of KWS, consume it and return non-nil."
  (when (and (< et--loop-pos et--loop-len) (memq (et--loop-peek) kws))
    (et--loop-advance) t))

(defun et--loop-check-expr ()
  "Type-check the form at the cursor in the current scope, then advance."
  (prog1 (et-checker-sub (1+ et--loop-pos)) (cl-incf et--loop-pos)))

(defun et--loop-check-body ()
  "Check consecutive forms until the next loop keyword; return the last type."
  (let ((ty (et Nil)))
    (while (and (< et--loop-pos et--loop-len) (not (et--loop-keyword-p (et--loop-peek))))
      (setq ty (et--loop-check-expr)))
    ty))


;;;; Bindings

(defun et--loop-bind (name type)
  "Bind loop variable NAME to TYPE for all subsequent clauses.
Pushes onto the loop-local `et--binds'; subsequent clauses see NAME
\(sequential, `let*'-style scope)."
  (when (and name (symbolp name))
    (push (cons name (et-new-var name (et--unfreshen-type type))) et--binds)))

(defun et--loop-bind-pattern (pat type)
  "Bind PAT, a symbol or destructuring pattern, for subsequent clauses.
A plain symbol gets TYPE.  A destructuring pattern binds each of its
symbols to Any (precise destructuring element types are not inferred)."
  (cond
   ((null pat) nil)
   ((symbolp pat) (et--loop-bind pat type))
   (t (dolist (sym (flatten-tree pat))
        (when (and sym (symbolp sym)) (et--loop-bind sym (et-any)))))))


;;;; for / as

(defun et--loop-clause-for ()
  "Parse a `for'/`as' clause, including parallel `and'-joined bindings.
Each binding's initializer is checked in the scope *before* the group is
bound, so `and'-joined bindings do not see one another (parallel), while
separate `for' clauses do (sequential)."
  (let ((group nil))
    (cl-loop
     (let* ((var (et--loop-advance))
            (type (et--loop-for-spec var)))
       (push (cons var type) group))
     ;; Continue the parallel group only if `and' is followed by another
     ;; binding (a var), not by a clause of a different kind.
     (unless (eq (et--loop-peek) 'and) (cl-return))
     (let* ((after (nth (1+ et--loop-pos) et--loop-clauses))
            (head (if (memq after '(for as)) (nth (+ 2 et--loop-pos) et--loop-clauses) after)))
       (unless (and head (or (symbolp head) (consp head)) (not (et--loop-keyword-p head)))
         (cl-return)))
     (et--loop-advance)            ; consume `and'
     (et--loop-eat 'for 'as))
    ;; Bind the whole group at once.
    (dolist (b (nreverse group))
      (et--loop-bind-pattern (car b) (cdr b)))))

(defun et--loop-for-spec (var)
  "Parse the iteration spec following `for VAR' and return VAR's type.
Initializer expressions are checked in the current scope."
  (pcase (et--loop-peek)
    ;; VAR from/upfrom/downfrom EXPR [to/... EXPR] [by EXPR]
    ((or 'from 'upfrom 'downfrom)
     (et--loop-advance)
     (let ((bounds (list (et--loop-check-expr))))
       (when (et--loop-eat 'to 'upto 'downto 'above 'below)
         (push (et--loop-check-expr) bounds))
       (when (et--loop-eat 'by) (push (et--loop-check-expr) bounds))
       (et--loop-numeric-var-type bounds)))

    ;; VAR to/upto/downto/above/below EXPR [by EXPR]  (implicit start 0)
    ((or 'to 'upto 'downto 'above 'below)
     (et--loop-advance)
     (let ((bounds (list (et Integer) (et--loop-check-expr))))
       (when (et--loop-eat 'by) (push (et--loop-check-expr) bounds))
       (et--loop-numeric-var-type bounds)))

    ;; VAR = EXPR1 [then EXPR2] ; EXPR2 sees VAR bound to its prior value
    ('=
     (et--loop-advance)
     (let ((init (et--loop-check-expr)))
       (if (et--loop-eat 'then)
           (let ((et--binds et--binds))
             (et--loop-bind-pattern var init)
             (et--or init (et--loop-check-expr)))
         init)))

    ;; VAR in/in-ref LIST [by FUNC]
    ((or 'in 'in-ref)
     (et--loop-advance)
     (let ((lst (et--loop-check-expr)))
       (when (et--loop-eat 'by) (et--loop-check-expr))
       (et--loop-infer-elem-type lst)))

    ;; VAR on LIST [by FUNC] ; VAR is bound to successive tails
    ('on
     (et--loop-advance)
     (let ((lst (et--loop-check-expr)))
       (when (et--loop-eat 'by) (et--loop-check-expr))
       lst))

    ;; VAR across/across-ref ARRAY
    ((or 'across 'across-ref)
     (et--loop-advance)
     (et--loop-infer-vector-elem-type (et--loop-check-expr)))

    ;; VAR being ...
    ('being (et--loop-advance) (et--loop-for-being))

    ;; Bare `for VAR' or an unrecognized spec
    (_ (et-any))))

(defun et--loop-for-being ()
  "Parse a `for VAR being ...' spec and return VAR's type.
May bind a secondary `using (FN VAR2)' variable."
  (et--loop-eat 'the 'each)
  (pcase (et--loop-peek)
    ((or 'elements 'element)
     (et--loop-advance)
     (et--loop-eat 'of 'of-ref)
     (let ((seq (et--loop-check-expr)))
       (prog1 (et--or (et--loop-infer-elem-type seq)
                      (et--loop-infer-vector-elem-type seq))
         (et--loop-being-using))))
    ((or 'hash-keys 'hash-key 'hash-values 'hash-value)
     (et--loop-advance)
     (et--loop-eat 'of)
     (et--loop-check-expr)
     (et--loop-being-using)
     (et-any))
    ((or 'symbols 'symbol)
     (et--loop-advance)
     (when (et--loop-eat 'of) (et--loop-check-expr))
     (et Symbol))
    ((or 'key-codes 'key-bindings 'key-seqs)
     (et--loop-advance)
     (et--loop-eat 'of)
     (et--loop-check-expr)
     (et--loop-being-using)
     (et-any))
    ((or 'overlays 'intervals)
     (et--loop-advance)
     (when (et--loop-eat 'of) (et--loop-check-expr))
     (when (et--loop-eat 'from) (et--loop-check-expr))
     (when (et--loop-eat 'to) (et--loop-check-expr))
     (et-any))
    ((or 'frames 'buffers) (et--loop-advance) (et-any))
    ('windows
     (et--loop-advance)
     (when (et--loop-eat 'of) (et--loop-check-expr))
     (et-any))
    (_ (et-any))))

(defun et--loop-being-using ()
  "Consume an optional `using (FN VAR2)' spec, binding VAR2."
  (when (et--loop-eat 'using)
    (let ((spec (et--loop-advance)))
      (when (consp spec)
        (et--loop-bind (cadr spec)
                       (if (eq (car spec) 'index) (et Integer) (et-any)))))))


;;;; with

(defun et--loop-clause-with ()
  "Parse `with VAR [= EXPR] [and VAR [= EXPR]]...'.
Separate `with' clauses are sequential; `and'-joined ones are parallel,
so their initializers are checked before any of the group is bound."
  (let ((group nil))
    (cl-loop
     (let* ((var (et--loop-advance))
            (type (if (et--loop-eat '=) (et--loop-check-expr) (et Nil))))
       (push (cons var type) group))
     (unless (et--loop-eat 'and) (cl-return)))
    (dolist (b (nreverse group))
      (et--loop-bind-pattern (car b) (cdr b)))))


;;;; Accumulation clauses

(defun et--loop-into-acc (name)
  "Return the `et--loop-acc' for `into NAME', creating it if needed."
  (or (alist-get name et--loop-into)
      (let ((acc (make-et--loop-acc)))
        (push (cons name acc) et--loop-into)
        acc)))

(defun et--loop-clause-accum (kw)
  "Process an accumulation clause whose keyword KW was already consumed.
The cursor is on the form to accumulate."
  (let* ((pos et--loop-pos)
         (form-type (et--loop-check-expr)))
    (if (et--loop-eat 'into)
        (et--loop-acc-add (et--loop-into-acc (et--loop-advance)) kw form-type pos)
      (et--loop-acc-add et--loop-bare kw form-type pos))))


;;;; Conditional clauses

(defun et--loop-clause-cond (kw)
  "Process `if'/`when'/`unless' (KW already consumed).
The condition narrows the variable bindings visible to the inner
clauses: the THEN side sees the condition's non-nil narrowing, the ELSE
side sees its nil narrowing (swapped for `unless')."
  (let* ((cond-type (et--loop-check-expr))
         (pos-binds (et--type-binds (et--non-nil cond-type)))
         (neg-binds (et--type-binds (et--supersect cond-type (et Nil))))
         (then-binds (if (eq kw 'unless) neg-binds pos-binds))
         (else-binds (if (eq kw 'unless) pos-binds neg-binds)))
    ;; The THEN and ELSE sides are parallel paths; the branch machinery
    ;; scopes each side's narrows while the cursor advances through both.
    (et-checker-branches
     (lambda ()
       (cl-callf et--narrows-and et--checker-narrows then-binds)
       (et--loop-cond-clauses)
       (et Nil))
     (lambda ()
       (when (et--loop-eat 'else)
         (cl-callf et--narrows-and et--checker-narrows else-binds)
         (et--loop-cond-clauses))
       (et Nil)))
    (et--loop-eat 'end)))

(defun et--loop-cond-clauses ()
  "Process a chain of inner conditional clauses joined by `and'."
  (et--loop-cond-one)
  (while (et--loop-eat 'and) (et--loop-cond-one)))

(defun et--loop-cond-one ()
  "Process a single inner clause of a conditional."
  (pcase (et--loop-peek)
    ((or 'collect 'collecting 'append 'appending 'nconc 'nconcing
         'concat 'vconcat 'count 'counting 'sum 'summing
         'maximize 'maximizing 'minimize 'minimizing)
     (et--loop-clause-accum (et--loop-advance)))
    ('return (et--loop-advance) (push (et--loop-check-expr) et--loop-returns))
    ((or 'do 'doing) (et--loop-advance) (et--loop-check-body))
    ((or 'if 'when 'unless) (et--loop-clause-cond (et--loop-advance)))
    (_ nil)))


;;;; finally

(defmacro et--loop-with-into-vars (&rest body)
  "Evaluate BODY with every `into VAR' bound to its accumulated type.
By the time a `finally' clause is reached, all `into' accumulations have
been walked, so their types are final."
  ;; LIMITATION: referencing a variable whose type is a recursive list
  ;; alias (e.g. `ListFresh') currently triggers a pre-existing failure
  ;; deep in the core (`et--supersect' builds a matcher from the alias
  ;; and chokes on its type argument). So `finally return VAR' for an
  ;; `into VAR' accumulator surfaces that core error. This is not
  ;; specific to this checker -- the previous cl-loop checker bound
  ;; `into' variables the same way and hit the same path.
  `(let ((et--binds et--binds))
     (cl-loop for (name . acc) in et--loop-into
              for ty = (et--loop-acc-type acc)
              when ty do (et--loop-bind name ty))
     ,@body))

(defun et--loop-clause-finally ()
  "Process a `finally' clause (the `finally' keyword already consumed)."
  (et--loop-with-into-vars
   (if (et--loop-eat 'return)
       (setq et--loop-finally (et--loop-check-expr))
     (et--loop-eat 'do)
     (et--loop-check-body))))


;;;; Walk

(defun et--loop-walk ()
  "Walk the clause list, type-checking each clause and threading scope."
  (while (< et--loop-pos et--loop-len)
    (let ((kw (et--loop-advance)))
      (pcase kw
        ((or 'for 'as) (et--loop-clause-for))
        ('with (et--loop-clause-with))
        ((or 'do 'doing) (et--loop-check-body))
        ('initially (et--loop-eat 'do) (et--loop-check-body))
        ('finally (et--loop-clause-finally))
        ('repeat (et--loop-check-expr))
        ((or 'while 'until) (et--loop-check-expr))
        ((or 'always 'never) (et--loop-check-expr) (setq et--loop-bool t))
        ('thereis (push (et--loop-check-expr) et--loop-thereis))
        ('return (push (et--loop-check-expr) et--loop-returns))
        ((or 'collect 'collecting 'append 'appending 'nconc 'nconcing
             'concat 'vconcat 'count 'counting 'sum 'summing
             'maximize 'maximizing 'minimize 'minimizing)
         (et--loop-clause-accum kw))
        ((or 'if 'when 'unless) (et--loop-clause-cond kw))
        ('named (et--loop-advance))
        ;; A stray non-keyword token (e.g. a parse desync); ignore it.
        (_ nil)))))


;;;; Result type

(defun et--loop-result-type ()
  "Compute the overall return type of the walked loop.
Precedence: `finally return' > `always'/`never' > `thereis' > the
implicit accumulator combined with any body `return' values."
  (cond
   (et--loop-finally et--loop-finally)
   (et--loop-bool (et Boolean))
   (et--loop-thereis (apply #'et--or (et Nil) et--loop-thereis))
   (t (let* ((bare (et--loop-acc-type et--loop-bare))
             ;; The value on normal loop completion
             (base (or bare (et Nil))))
        ;; A body `return' may or may not fire, so it unions with `base'
        (if et--loop-returns
            (apply #'et--or base et--loop-returns)
          base)))))


;;;; Checker

(et-define-checker cl-loop
  ;; The clauses run repeatedly, so the whole walk happens inside
  ;; `et-checker-loop-body' -- its discovery pass finds the vars the loop
  ;; can change and drops their narrows before the real pass. The walker
  ;; state lives inside the body function so each pass starts fresh.
  (let* ((result-type nil))
    (et-checker-loop-body
     (lambda ()
       (let* ((et--loop-clauses (cdr et--checker-expr))
              (et--loop-len (length et--loop-clauses))
              (et--loop-pos 0)
              ;; Loop variables are pushed onto a private copy of `et--binds'
              (et--binds et--binds)
              (et--loop-bare (make-et--loop-acc))
              (et--loop-into nil)
              (et--loop-returns nil)
              (et--loop-thereis nil)
              (et--loop-bool nil)
              (et--loop-finally nil))
         (et--loop-walk)
         ;; Return the raw type; the framework (`et-typecheck') simplifies
         ;; downstream, as it does for every other checker.
         (setq result-type (et--loop-result-type)))))
    result-type))


;;;; Tests

(et-test
 ;; ---- for VAR in LIST: infers element type ----
 (et-assert-resolve ListFresh<Integer>
   (cl-loop for x in (list 1 2 3) collect x))

 ;; ---- for VAR from/to: numeric bounds determine Integer vs Number ----
 (et-assert-resolve ListFresh<Integer>
   (cl-loop for i from 1 to 10 collect i))
 (et-assert-resolve ListFresh<Number>
   (cl-loop for i from 1.0 to 10 collect i))

 ;; ---- for VAR = EXPR then EXPR ----
 (et-assert-resolve ListFresh<Integer|String>
   (cl-loop for x = 1 then "hi" collect x))

 ;; ---- for VAR across ARRAY ----
 (et-assert-resolve ListFresh<Integer>
   (cl-loop for c across "hello" collect c))

 ;; ---- for VAR on LIST ----
 (et-assert-resolve ListFresh<ListR<Integer>>
   (cl-loop for tail on (list 1 2 3) collect tail))

 ;; ---- with VAR = EXPR ----
 (et-assert-resolve ListFresh<Integer>
   (cl-loop with base = 10
            for i from 1 to 5
            collect (+ base i)))

 ;; ---- append: fresh list of elem type ----
 (et-assert-resolve ListFresh<Integer>
   (cl-loop for x in (list (list 1) (list 2)) append x))

 ;; ---- nconc: NON-fresh list (shares conses with body) ----
 (et-assert-resolve ListR<1|2>
   (cl-loop for x in (list (list 1) (list 2)) nconc x))

 ;; ---- concat ----
 (et-assert-resolve String
   (cl-loop for x in (list "a" "b") concat x))

 ;; ---- vconcat ----
 (et-assert-resolve Vector<1|2|3>
   (cl-loop for x in (list 1 2 3) vconcat x))

 ;; ---- count ----
 (et-assert-resolve Integer
   (cl-loop for x in (list 1 2 3) count x))

 ;; ---- sum: Integer when summing Integers ----
 (et-assert-resolve Integer
   (cl-loop for x in (list 1 2 3) sum x))

 ;; ---- sum: Number when summing Numbers ----
 (et-assert-resolve Number
   (cl-loop for x in (list 1 2.5 3) sum x))

 ;; ---- maximize ----
 (et-assert-resolve Integer
   (cl-loop for x in (list 1 2 3) maximize x))

 ;; ---- always/never returns Boolean ----
 (et-assert-resolve Boolean
   (cl-loop for x in (list 1 2 3) always (integerp x)))
 (et-assert-resolve Boolean
   (cl-loop for x in (list 1 2 3) never (stringp x)))

 ;; ---- thereis: form-type | Nil ----
 (et-assert-resolve Integer|Nil
   (cl-loop for x in (list 1 2 3)
            thereis (and (integerp x) x)))

 ;; ---- return inside body ----
 (et-assert-resolve String|Nil
   (cl-loop for x in (list 1 2 3)
            if (eq x 2) return "found"))

 ;; ---- finally return overrides accumulation ----
 (et-assert-resolve String
   (cl-loop for x in (list 1 2 3)
            collect x
            finally return "done"))

 ;; ---- no accumulation returns Nil ----
 (et-assert-resolve Nil
   (cl-loop for x in (list 1 2 3) do (+ x 1)))

 ;; ---- collect into VAR: bare return is Nil, not the accumulator ----
 (et-assert-resolve Nil
   (cl-loop for x in (list 1 2 3)
            collect x into result))

 ;; ---- collect into VAR + finally return: uses accumulator ----
 (et-assert-resolve List<1|2|3>
   (cl-loop for x in (list 1 2 3)
            collect x into result
            finally return result))

 ;; ---- repeat ----
 (et-assert-resolve ListFresh<Integer>
   (cl-loop repeat 5 collect 1))

 ;; ---- while ----
 (et-assert-resolve ListFresh<Integer>
   (cl-loop for i from 1
            while (< i 10)
            collect i))

 ;; ---- when ... collect (conditional accumulation) ----
 (et-assert-resolve ListFresh<Integer>
   (cl-loop for x in (list 1 2 3)
            when (integerp x) collect x))

 ;; ---- multiple for clauses: second sees first ----
 (et-assert-resolve ListFresh<Integer>
   (cl-loop for xs in (list (list 1) (list 2))
            for x in xs
            collect x))

 ;; ---- for VAR = EXPR (no then) ----
 (et-assert-resolve ListFresh<Integer>
   (cl-loop for x = 5 repeat 3 collect x))

 ;; ---- with VAR (no = EXPR) defaults to Nil ----
 (et-assert-resolve Nil
   (cl-loop with x repeat 1 do x))

 ;; ---- for VAR to EXPR (implicit from 0) ----
 (et-assert-resolve ListFresh<Integer>
   (cl-loop for i to 5 collect i))

 ;; ---- for VAR downfrom EXPR to EXPR by EXPR ----
 (et-assert-resolve ListFresh<Integer>
   (cl-loop for i downfrom 10 to 0 by 2 collect i)))


;;; cl-destructuring-bind

(defvar et--destructuring-bind-generic-counter 0)

(defun et--destructuring-bind-generic ()
  (intern (format "D%s" (cl-incf et--destructuring-bind-generic-counter))))

(defun et--destructuring-bind-generic-repr (generic)
  (make-et-repr :generics (list generic)
                :dnf (list (list (list 'S:GENERIC generic)))))

(defun et--destructuring-bind-pattern (pattern)
  "Return (VARS GENERICS REPR) for a destructuring PATTERN.
VARS is an alist of variable names to generated generic names."
  (cond
   ((null pattern)
    (list nil nil (et-repr Nil)))
   ((symbolp pattern)
    (let ((generic (et--destructuring-bind-generic)))
      (list (list (cons pattern generic))
            (list generic)
            (et--destructuring-bind-generic-repr generic))))
   ((consp pattern)
    (pcase-let ((`(,vars ,generics ,repr)
                 (et--destructuring-bind-arglist-repr pattern)))
      (list vars generics repr)))
   (t (et-fatal nil "Invalid destructuring pattern: %s" pattern))))

(defun et--destructuring-bind-param (spec state)
  "Return (KEY VARS GENERICS REPR) for a destructuring lambda-list SPEC.
KEY is non-nil only for `&key' parameters."
  (pcase-let* ((key nil)
               (pattern
                (pcase state
                  ('required spec)
                  ('rest spec)
                  ((or 'optional 'key)
                   (let ((head (if (consp spec) (car spec) spec)))
                     (pcase head
                       (`(,(and keyword (pred keywordp)) ,var)
                        (setq key keyword)
                        var)
                       (_ head))))))
               (`(,vars ,generics ,repr)
                (et--destructuring-bind-pattern pattern)))
    (list key vars generics repr)))

(defun et--destructuring-bind-key-rest-repr (key-params)
  (if key-params
      (cl-loop for (key _vars _generics repr) in key-params
               nconc (list key repr) into plist-args
               finally return (et-repr PList ,@plist-args))
    (et-repr Nil)))

(defun et--destructuring-bind-arglist-repr (arglist)
  "Return (VARS GENERICS REPR) for destructuring lambda list ARGLIST."
  (let ((required nil)
        (optional nil)
        (key-params nil)
        (rest-param nil)
        (vars nil)
        (generics nil)
        (state 'required))
    (dolist (elt arglist)
      (pcase elt
        ('&optional (setq state 'optional))
        ((or '&rest '&body) (setq state 'rest))
        ('&key (setq state 'key))
        ('&allow-other-keys nil)
        ('&aux (setq state 'aux))
        (_
         (pcase state
           ('aux nil)
           (_
            (pcase-let ((`(,key ,param-vars ,param-generics ,repr)
                         (et--destructuring-bind-param elt state)))
              (setq vars (append vars param-vars)
                    generics (append generics param-generics))
              (pcase state
                ('required (push repr required))
                ('optional (push repr optional))
                ('key (push (list (or key (intern (format ":%s" elt)))
                                  param-vars param-generics repr)
                            key-params))
                ('rest (setq rest-param repr)))))))))
    (let* ((rest-repr (or rest-param
                          (et--destructuring-bind-key-rest-repr (nreverse key-params))))
           (repr (et--params-to-input-repr (nreverse required)
                                           (nreverse optional)
                                           rest-repr)))
      (list vars generics repr))))

(defun et--destructuring-bind-vars (vars generics types)
  (cl-loop for (name . generic) in vars
           for type = (or (alist-get generic (cl-mapcar #'cons generics types))
                          (et-never))
           collect (et-new-var name type)))

(et-define-pcase-checker cl-destructuring-bind
    `(,(and arglist (pred listp)) ,_value . ,_body)
  (pcase-let* ((value-type (et-checker-sub 2))
               (`(,vars ,generics ,repr)
                (et--destructuring-bind-arglist-repr arglist))
               (matcher (make-et-matcher :generics generics :repr repr))
               (match (et-sub-match matcher value-type)))
    (if (et-match-result-success match)
        (et-with-vars (et--destructuring-bind-vars
                       vars generics (et-match-result-value match))
          (et-checker-tail 3))
      (et-err 2 "Value type %s does not match destructuring arglist %s"
              value-type arglist)
      (et-with-vars (mapcar (lambda (binding)
                              (et-new-var (car binding) (et-never)))
                            vars)
        (et-checker-tail 3))
      (et-never))))

(et-test
 (et-assert-resolve Integer
   (cl-destructuring-bind (a b) (list 1 "x") a))
 (et-assert-resolve String
   (cl-destructuring-bind (a b) (list 1 "x") b))
 (et-assert-resolve String
   (cl-destructuring-bind ((a b) c) (list (list 1 "x") t) b))
 (et-assert-resolve ListR<String>
   (cl-destructuring-bind (a &rest rest) (list 1 "x" "y") rest))
 (et-assert-resolve Nil
   (cl-destructuring-bind (a &optional b) (list 1) b))
 (et-assert-resolve Integer
   (cl-destructuring-bind (&key name age) '(:name "Ada" :age 5) age))
 (et-assert-resolve-errors
  (cl-destructuring-bind (a b) (list 1) b)))


;;; cl-flet
;;;; Binding types

;; A local function is checked exactly like a `defun': if the binding
;; declares an `@return', its body is checked against the declared
;; signature and the declared type is used. Otherwise the parameters
;; default to Any and the return type is the type of the body.
;;
;; The resulting type is installed as the `et-function-type' property of
;; the function's symbol, which is what makes calls to it resolve. The
;; property is global, so the checker is responsible for restoring the
;; original value once the flet's scope ends.

(defun et--flet-binding-type (binding rel)
  "Return the function type of the `cl-flet' BINDING at path REL.

BINDING is either (NAME EXPR), whose type is the type of EXPR, or
\(NAME ARGLIST . BODY), whose body is checked here."
  (pcase binding
    (`(,_name ,_expr) (et-checker-sub (list rel 1)))
    (`(,_name ,(pred listp) . ,_body)
     (if-let* ((sig (et-at rel (et-at-offset 1 (et--parse-function-signature (cdr binding))))))
         (et--flet-declared-type sig rel)
       (et--flet-inferred-type (cdr binding) rel)))
    (_ (et-fatal rel "Expected (NAME ARGLIST BODY...) or (NAME EXPR)"))))

(defun et--flet-declared-type (sig rel)
  "Check the body of the flet binding at REL against its signature SIG.
Returns the declared function type."
  (et--with-scoped-datatypes (et-func-sig-scoped sig)
    (et-with-vars (et-func-sig-vars sig)
      ;; The body runs whenever the local function is called, not here
      (et-checker-deferred
        (let* ((actual-ret (et-checker-tail rel (1+ (et-func-sig-source-pos sig))))
               (expected-ret (et-func-sig-expected-return sig)))
          (or (et-subtype? actual-ret expected-ret)
              (et-err rel "Expected %s, found %s" expected-ret actual-ret))))))
  (et-func-sig-func-type sig))

(defun et--flet-inferred-type (args-and-body rel)
  "Infer the function type of the flet binding at REL from its body.

ARGS-AND-BODY is (ARGLIST . BODY), the cdr of the binding. It declares no
`@return', so parameters keep their declared reprs (Any when undeclared)
and the return type is the type of the body."
  (pcase-let* ((`(,decls . ,source-pos)
                (or (et-at rel (et-at-offset 1 (et--find-function-declarations args-and-body)))
                    (cons (et--parse-function-declarations
                           (et--parse-arglist-params (car args-and-body)) nil)
                          1)))
               (input (et--func-decls-to-input decls))
               (vars (cl-loop for group in (et--func-declarations-param-reprs decls)
                              nconc (cl-loop for (name . repr) in group
                                             collect (et-new-var name (et-repr-to-type repr nil)))))
               ;; The body runs whenever the local function is called, not here
               (return-type (et-checker-deferred
                              (et-with-vars vars (et-checker-tail rel (1+ source-pos))))))
    (unless (et-type-p input)
      (et-fatal rel "A generic function must declare an `@return' type"))
    (et-dt 'Function input return-type)))


;;;; Checker

(defun et--check-flet (sequential)
  "Type check the `cl-flet' expression being checked.

Each binding's type is installed as the `et-function-type' property of
its name, and the original properties are restored once the body has been
checked.

When SEQUENTIAL (`cl-flet*'), a binding is installed before the following
bindings are checked, so each one sees the ones before it. Otherwise
\(`cl-flet'), every binding is checked in the outer function scope, so
none of them are installed until all of them have been checked."
  (pcase-let* ((`(,bindings . ,_body) (cdr et--checker-expr))
               (originals (cl-loop for (name) in bindings
                                   collect (cons name (et-function-type name)))))
    (unwind-protect
        (let* ((types (cl-loop for binding in bindings
                               for idx upfrom 0
                               for type = (et--flet-binding-type binding (list 1 idx))
                               when sequential do (put (car binding) 'et-function-type type)
                               collect (cons (car binding) type))))
          (unless sequential
            (cl-loop for (name . type) in types do (put name 'et-function-type type)))
          (et-checker-tail 2))

      (cl-loop for (name . type) in originals
               do (put name 'et-function-type type)))))

(et-define-checker cl-flet (et--check-flet nil))
(et-define-checker cl-flet* (et--check-flet t))


;;;; Tests

(et-test
 ;; Declared signature: the body is checked against it
 (et-assert-resolve String
   (cl-flet ((name (x) (declare (et (x Integer) (@return String))) (format "%s" x)))
     (name 1)))
 (et-assert-resolve-errors
  (cl-flet ((name (x) (declare (et (x Integer) (@return String))) (format "%s" x)))
    (name "1")))

 ;; (NAME EXPR) binding
 (et-assert-resolve Integer
   (cl-flet ((inc #'1+)) (inc 1)))

 ;; `cl-flet' bindings do not see each other, `cl-flet*' bindings do
 (et-assert-resolve-errors
  (cl-flet ((a (x) (* 2 x))
            (b (x) (a x)))
    (b 1)))
 (et-assert-resolve Integer
   (cl-flet* ((a (x) (declare (et (x Integer) (@return Integer))) (* 2 x))
              (b (x) (declare (et (x Integer) (@return Integer))) (a x)))
     (b 1)))

 ;; The function type does not outlive the flet
 (et-assert-resolve-errors
  (progn (cl-flet ((tmp (x) x)) (tmp 1))
         (tmp 1))))
