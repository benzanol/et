;;; Pcase
;;;; Options

(defcustom et-pcase-warn-never-match t
  "Whether to warn about `pcase' patterns that can never match.

When non-nil, the `pcase' checker emits a warning on any case whose
pattern is incompatible with every value still reaching it -- for
example a pattern with more slots than the scrutinee can hold, like
`(S:GENERIC ,var ,extra)' against a two-element factor.  Such a case is
unreachable, so its body is not checked."
  :type 'boolean
  :group 'et)


;;;; pcase
;;;;; Pcase pattern protocol

;; A pcase pattern is checked against a SCRUTINEE TYPE: the type of the
;; value the pattern is being matched against. Checking a pattern
;; produces an `et-pcase-result' describing three things:
;;
;;   - matched-type:  the scrutinee type, narrowed to the values for
;;                    which this pattern matches. This carries over the
;;                    `typeof' annotations of the scrutinee, so that
;;                    `et--type-binds' on it narrows the original
;;                    scrutinee variable inside the pattern's code.
;;
;;   - vars:          the new `et-var's bound by this pattern (e.g. the
;;                    car/cdr bindings of a backquote cons pattern).
;;
;;   - residual-type: the scrutinee type, narrowed to the values for
;;                    which this pattern does NOT match. This is threaded
;;                    into the following cases, so that later patterns and
;;                    their code see the scrutinee narrowed by all the
;;                    patterns that failed before them.
;;
;; Each pattern type is processed by a handler registered with
;; `et-define-pcase-pattern' under the property `et-pcase-handler' of the
;; pattern's head symbol. Atomic patterns (`_', symbols, literals) are
;; handled directly by the dispatcher `et--pcase-check'.

(cl-defstruct et-pcase-result
  "Result of checking a pcase pattern against a scrutinee type."
  (matched-type nil :documentation "Narrowed scrutinee type when matched.")
  (vars nil :documentation "List of `et-var' bound by this pattern.")
  (residual-type nil :documentation "Scrutinee type when NOT matched."))

(defmacro et-define-pcase-pattern (name arglist &rest body)
  "Define a handler for the pcase pattern headed by symbol NAME.

ARGLIST is bound to (PATTERN-ARGS SCRUTINEE-TYPE BOUND-VARS PATH), where
PATTERN-ARGS is the cdr of the pattern, SCRUTINEE-TYPE is the current
scrutinee `et-type', BOUND-VARS is the list of `et-var' already bound
earlier in the enclosing pattern, and PATH is the path to the pattern
relative to the enclosing pcase form. The body must return an
`et-pcase-result'."
  (declare (indent 2))
  `(put ',name 'et-pcase-handler (lambda ,arglist ,@body)))


;;;;; Pattern dispatch

(defun et--pcase-check (pat type bound-vars path)
  "Check pcase pattern PAT against scrutinee TYPE, returning an `et-pcase-result'.

BOUND-VARS is the list of `et-var' already bound in this pattern. PATH is
the path to PAT relative to the enclosing pcase form."
  (cond
   ;; Registered compound pattern (quote, backquote, and, or, pred, ...)
   ((and (consp pat) (symbolp (car pat)) (get (car pat) 'et-pcase-handler))
    (funcall (get (car pat) 'et-pcase-handler) (cdr pat) type bound-vars path))

   ;; _ matches anything, binds nothing, always succeeds
   ((eq pat '_) (et--pcase-wild type))

   ;; Symbols: keywords/nil/t are self-quoting literals, anything else binds
   ((symbolp pat)
    (if (or (keywordp pat) (memq pat '(nil t)))
        (et--pcase-literal type pat)
      (et--pcase-symbol type pat bound-vars)))

   ;; Self-quoting atoms (numbers, strings) are literals
   ((or (numberp pat) (stringp pat))
    (et--pcase-literal type pat))

   ;; Unsupported pattern: report it, but assume nothing — it may or may
   ;; not match, and binds nothing we can see.
   (t (et-err path "Unsupported pcase pattern: %s" pat)
      (make-et-pcase-result :matched-type type :vars nil :residual-type type))))


;;;;; Atomic patterns

(defun et--pcase-wild (type)
  "Result for a pattern that always matches and binds nothing."
  (make-et-pcase-result :matched-type type :vars nil :residual-type (et-never)))

(defun et--pcase-symbol (type sym bound-vars)
  "Result for a symbol pattern SYM, binding SYM to the scrutinee TYPE.

If SYM was already bound earlier in the pattern (it appears in
BOUND-VARS), this is an equality test against the earlier binding rather
than a new binding."
  (if-let* ((prev (cl-find sym bound-vars :key #'et-var-name)))
      (let ((prev-type (et-var-type prev)))
        (make-et-pcase-result
         :matched-type (et--supersect type prev-type)
         :vars nil
         :residual-type (et--subtract type prev-type)))
    (make-et-pcase-result
     :matched-type type
     :vars (list (et-new-var sym (et--unfreshen-type type)))
     :residual-type (et-never))))

(defun et--pcase-literal (type val)
  "Result for a literal pattern matching only VAL."
  (let ((lit (et-literal val)))
    (make-et-pcase-result
     :matched-type (et--supersect type lit)
     :vars nil
     :residual-type (et--subtract type lit))))


;;;;; Quote

(et-define-pcase-pattern quote (args type _bound-vars _path)
  (et--pcase-literal type (car args)))


;;;;; pred

(defun et--pcase-pred-domain (fun)
  "Return the type domain on which predicate FUN returns non-nil.

FUN must be a symbol with a narrowing `et-function-type' (such as the
predicates defined with `et-define-predicate'). Returns nil when no such
domain can be determined."
  (when-let* (((symbolp fun))
              (ftype (get fun 'et-function-type))
              (synth (et-new-var (gensym "et-pcase-pred") (et-any)))
              (arg (et-type (make-et-type-case
                             :value (make-et-datatype :name 'Any)
                             :typeofs (list synth))))
              (out (et-checker-funcall ftype (et--tuple 'ConsR (list arg))))
              ((et-type-p out)))
    (alist-get synth (et--type-binds (et--non-nil out)))))

(defun et--pcase-pred-narrow (type fun)
  "Return (MATCHED . RESIDUAL) for narrowing TYPE by predicate FUN.

MATCHED is the part of TYPE where FUN is non-nil, RESIDUAL the part where
FUN is nil. When FUN's domain is unknown, no narrowing occurs."
  (if-let* ((domain (et--pcase-pred-domain fun)))
      (cons (et--supersect type domain) (et--subtract type domain))
    (cons type type)))

(et-define-pcase-pattern pred (args type _bound-vars _path)
  (pcase (car args)
    ;; (pred (not FUN)) matches when FUN is nil: swap matched/residual
    (`(not ,inner)
     (let ((mr (et--pcase-pred-narrow type inner)))
       (make-et-pcase-result :matched-type (cdr mr) :vars nil :residual-type (car mr))))
    (fun
     (let ((mr (et--pcase-pred-narrow type fun)))
       (make-et-pcase-result :matched-type (car mr) :vars nil :residual-type (cdr mr))))))


;;;;; app

(defun et--pcase-fun-output (fun type)
  "Return the output type of applying FUN to a value of TYPE, or Any.

FUN may be a symbol with an `et-function-type'. Other forms (lambdas,
partial applications) are not analysed and yield Any."
  (or (when-let* (((symbolp fun))
                  (ftype (get fun 'et-function-type))
                  (out (et-checker-funcall ftype (et--tuple 'ConsR (list type))))
                  ((et-type-p out)))
        out)
      (et-any)))

(et-define-pcase-pattern app (args type bound-vars path)
  ;; (app FUN PAT): PAT is matched against FUN applied to the value. The
  ;; scrutinee itself cannot be narrowed (FUN is not invertible), but PAT
  ;; still contributes its bindings. The pattern is exhaustive exactly when
  ;; PAT is exhaustive over FUN's output.
  (let* ((out (et--pcase-fun-output (car args) type))
         (res (et--pcase-check (cadr args) out bound-vars (append path '(2)))))
    (make-et-pcase-result
     :matched-type type
     :vars (et-pcase-result-vars res)
     :residual-type (if (et-never-p (et-pcase-result-residual-type res))
                        (et-never) type))))


;;;;; guard

(defun et--pcase-attach-binds (type binds)
  "Return TYPE with BINDS added to each of its cases."
  (if (null binds) type
    (make-et-type
     :label (et-type-label type)
     :cases (cl-loop for c in (et-type-cases type)
                     collect (make-et-type-case
                              :value (et-type-case-value c)
                              :binds (append binds (et-type-case-binds c))
                              :typeofs (et-type-case-typeofs c))))))

(et-define-pcase-pattern guard (args type bound-vars path)
  ;; (guard BOOLEXP): matches when BOOLEXP is non-nil. The scrutinee value
  ;; is not narrowed, but BOOLEXP may narrow other variables (e.g.
  ;; (guard (integerp x))); those narrowings are folded into matched-type.
  (let* ((bool-type (et-with-vars bound-vars
                      (et-with-narrow-binds (et--type-binds type)
                        (et-at (append path '(1)) (et--check (car args))))))
         (non-nil-binds (et--type-binds (et--non-nil bool-type))))
    (make-et-pcase-result
     :matched-type (et--pcase-attach-binds type non-nil-binds)
     :vars nil
     :residual-type type)))


;;;;; and / or

(defun et--pcase-merge-vars (a b)
  "Merge variable lists A and B, unioning the types of shared names."
  (cl-loop with names = (delete-dups (append (mapcar #'et-var-name a)
                                             (mapcar #'et-var-name b)))
           for n in names
           for av = (cl-find n a :key #'et-var-name)
           for bv = (cl-find n b :key #'et-var-name)
           collect (et-new-var n (cond ((and av bv) (et--or (et-var-type av) (et-var-type bv)))
                                       (av (et-var-type av))
                                       (t (et-var-type bv))))))

(et-define-pcase-pattern and (args type bound-vars path)
  ;; (and PAT...) matches when every sub-pattern matches. Sub-patterns are
  ;; threaded: each sees the scrutinee narrowed by the previous ones.
  (let ((cur type) (new-vars nil) (residuals nil))
    (cl-loop for sub in args
             for idx upfrom 1
             do (let ((res (et--pcase-check sub cur (append bound-vars new-vars)
                                            (append path (list idx)))))
                  (setq cur (et-pcase-result-matched-type res))
                  (setq new-vars (append new-vars (et-pcase-result-vars res)))
                  (push (et-pcase-result-residual-type res) residuals)))
    (make-et-pcase-result
     :matched-type cur
     :vars new-vars
     ;; Fails if any sub-pattern fails: union of the staged residuals.
     :residual-type (apply #'et--or (or residuals (list (et-never)))))))

(et-define-pcase-pattern or (args type bound-vars path)
  ;; (or PAT...) matches when any sub-pattern matches. Each is checked
  ;; against the same scrutinee type.
  (let ((matcheds nil) (residual type) (all-vars nil))
    (cl-loop for sub in args
             for idx upfrom 1
             do (let ((res (et--pcase-check sub type bound-vars (append path (list idx)))))
                  (push (et-pcase-result-matched-type res) matcheds)
                  ;; Fails only if every branch fails: intersect residuals.
                  (setq residual (et--supersect residual (et-pcase-result-residual-type res)))
                  (setq all-vars (et--pcase-merge-vars all-vars (et-pcase-result-vars res)))))
    (make-et-pcase-result
     :matched-type (apply #'et--or (or (nreverse matcheds) (list (et-never))))
     :vars all-vars
     :residual-type residual)))


;;;;; Backquote

(et-define-pcase-pattern \` (args type bound-vars path)
  (et--pcase-check-qpat (car args) type bound-vars (append path '(1))))

(defun et--pcase-check-qpat (qpat type bound-vars path)
  "Check a backquote-style sub-pattern QPAT against scrutinee TYPE."
  (cond
   ;; ,PAT escapes back to a normal pcase pattern
   ((and (consp qpat) (eq (car qpat) '\,))
    (et--pcase-check (cadr qpat) type bound-vars (append path '(1))))
   ;; (QPAT1 . QPAT2) matches a cons
   ((consp qpat) (et--pcase-qpat-cons qpat type bound-vars path))
   ;; [QPAT...] matches a vector
   ((vectorp qpat) (et--pcase-qpat-vector qpat type bound-vars path))
   ;; A bare atom is an equality test against itself
   (t (et--pcase-literal type qpat))))

(defun et--pcase-qpat-cons (qpat type bound-vars path)
  "Check a backquote cons sub-pattern (QPAT1 . QPAT2) against TYPE.

Mirroring how `pcase' itself evaluates a cons pattern, the car is checked
first and the cdr second, with the car's narrowing threaded into the cdr.
Concretely, the matched car type narrows the cons before the cdr type is
inferred, so a literal/constructor matched in the car selects the union
arm whose cdr the cdr-pattern (and any variables it binds) is drawn from.
Without this, each position would be inferred independently from the full
union and a binding in the cdr would pick up every arm's cdr.

A side effect is that a pattern with more slots than the scrutinee can
hold narrows to `Never': its surplus slot is eventually matched against a
`Nil' cdr, whose intersection with `Cons' is empty.  The enclosing pcase
checker treats such a case as unreachable."
  (let* ((cons-type (et--supersect type (et Cons)))
         ;; Car first.
         (car-type (or (et-checker-infer cons-type [T] ConsR<T~Any> T) (et-any)))
         (car-res (et--pcase-check-qpat (car qpat) car-type bound-vars
                                        (append path '(0))))
         (car-vars (et-pcase-result-vars car-res))
         (matched-car (et-pcase-result-matched-type car-res))
         ;; Narrow the cons by the matched car BEFORE inferring the cdr, so
         ;; the cdr (and the vars it binds) only sees the arms the car kept.
         (narrowed-cons (et--supersect cons-type (et-alias 'ConsR matched-car (et-any))))
         (cdr-type (or (et-checker-infer narrowed-cons [T] ConsR<Any~T> T) (et-any)))
         ;; The cdr of a proper list shares the same buffer position as the
         ;; whole list tail, so its path stays at PATH (best effort).
         (cdr-res (et--pcase-check-qpat (cdr qpat) cdr-type
                                        (append bound-vars car-vars) path))
         (cons-shape (et-alias 'ConsR
                               matched-car
                               (et-pcase-result-matched-type cdr-res))))
    (make-et-pcase-result
     :matched-type (et--supersect type cons-shape)
     :vars (append car-vars (et-pcase-result-vars cdr-res))
     :residual-type (et--subtract type cons-shape))))

(defun et--pcase-qpat-vector (qpat type bound-vars path)
  "Check a backquote vector sub-pattern [QPAT...] against TYPE.

Vectors are homogeneous in this type system, so all elements are checked
against the shared element type; the scrutinee is narrowed only to
\"is a vector\"."
  (let* ((vec-type (et--supersect type (et Vector)))
         (elem-type (or (et-checker-infer vec-type [T] VectorR<T> T) (et-any)))
         (all-vars nil))
    (cl-loop for qsub across qpat
             for idx upfrom 0
             do (let ((res (et--pcase-check-qpat qsub elem-type
                                                 (append bound-vars all-vars)
                                                 (append path (list idx)))))
                  (setq all-vars (append all-vars (et-pcase-result-vars res)))))
    (make-et-pcase-result
     :matched-type vec-type
     :vars all-vars
     :residual-type type)))


;;;;; pcase checker

(defun et--pcase-check-form (exhaustive)
  "Check the current `pcase'-form expression, returning its result type.

The form is assumed to have the shape (HEAD SCRUTINEE CASE...), as both
`pcase' and `pcase-exhaustive' do.  When EXHAUSTIVE is non-nil a value
falling through every pattern is an error rather than a `nil' result, so
the fallthrough `Nil' arm is omitted from the result type."
  (let* ((scrut (et-checker-sub 1))
         (cases (cddr et--checker-expr))
         ;; The scrutinee type, narrowed by each failed pattern in turn.
         (cur scrut)
         (body-types nil))
    (cl-loop for case in cases
             for ci upfrom 2
             do (let* ((res (et--pcase-check (car case) cur nil (list ci 0)))
                       (matched (et-pcase-result-matched-type res)))
                  (cond
                   ;; Only reachable cases contribute and get their body checked.
                   ((not (et-never-p matched))
                    (et-with-vars (et-pcase-result-vars res)
                      (et-with-narrow-binds (et--type-binds matched)
                        (push (et-checker-tail ci 1) body-types))))
                   ;; The pattern matched nothing, yet values still reached it:
                   ;; it can never match (e.g. too many slots for the scrutinee).
                   ;; When CUR is already Never the case is merely redundant
                   ;; (every value was consumed by an earlier pattern), which is
                   ;; not worth flagging.
                   ((and et-pcase-warn-never-match (not (et-never-p cur)))
                    (et-warn (list ci 0) "Pattern can never match a value of %s" cur)))
                  (setq cur (et-pcase-result-residual-type res))))
    ;; If some value can fall through every pattern, `pcase' returns nil --
    ;; but `pcase-exhaustive' signals an error, so it contributes nothing.
    (when (and (not exhaustive) (not (et-never-p cur)))
      (push (et Nil) body-types))
    (et-simplify-type (apply #'et--or (or (nreverse body-types) (list (et Nil)))))))

(et-define-checker pcase
  (et--pcase-check-form nil))

;; `pcase-exhaustive' matches like `pcase' but signals an error instead of
;; returning nil when no pattern matches; it therefore never contributes the
;; fallthrough `Nil' arm.
(et-define-checker pcase-exhaustive
  (et--pcase-check-form t))


;;;;; Tests

(defun et--test-pcase (expr &rest props)
  "Typecheck EXPR and assert PROPS about its type, returning an `et-result'.

This defines its own result boundary, so a single call can be evaluated
on its own to get a readable `#<SUCCESS: TYPE>' / `#<FAIL: ...>' result;
inside an `et-test' block the failed result is propagated up instead.

The value of the returned result is the checked type. PROPS is a plist of
assertions, each emitting an error diagnostic on failure. SPEC is anything
`et-parse-type' accepts (a type spec or an `et-type'):

  :resolves SPEC      the type of EXPR must be a subtype of SPEC
  :not-resolves SPEC  the type of EXPR must not be a subtype of SPEC"
  (et-result-boundary
   (let ((type (et-at 1 (et--check expr))))
     (cl-loop for (kw spec) on props by #'cddr
              for want = (et-parse-type spec)
              do (pcase kw
                   (:resolves
                    (unless (et-subtype? type want)
                      (et-err 1 "Expected subtype of %s, found %s" want type)))
                   (:not-resolves
                    (when (et-subtype? type want)
                      (et-err 1 "Expected %s not to be a subtype of %s" type want)))
                   (_ (et-fatal 0 "Unknown test property: %s" kw))))
     type)))

(et-test
 ;; --- whole-value binding ---
 (et--test-pcase '(pcase 5 (x x)) :resolves '5)

 ;; --- literal pattern then binding: residual is narrowed to String ---
 (et--test-pcase '(pcase (:type Integer|String)
                    ((pred integerp) "s")
                    (y y))
                 :resolves 'String :not-resolves 'Integer)

 ;; --- pred narrows the real scrutinee variable inside the matched body ---
 ;; The inner `et:' assertions fail the result if `a' is not narrowed.
 (et--test-pcase '(let* ((a (:type Integer|String)))
                    (pcase a
                      ((pred integerp) (et: a Integer))
                      (_ (length (et: a String)))))
                 :resolves 'Integer)

 ;; --- (pred (not FUN)) ---
 (et--test-pcase '(pcase (:type Integer|String)
                    ((pred (not stringp)) 1)
                    (y (length y)))
                 :resolves 'Integer)

 ;; --- destructuring a cons binds car and cdr with correct types ---
 (et--test-pcase '(pcase (cons 1 "2") (`(,x . ,y) x)) :resolves 'Integer)
 (et--test-pcase '(pcase (cons 1 "2") (`(,x . ,y) y)) :resolves 'String)

 ;; --- a single cons pattern is exhaustive over a cons scrutinee ---
 (et--test-pcase '(pcase (cons 1 2) (`(,x . ,y) x) (_ "unreached"))
                 :resolves 'Integer :not-resolves 'String)

 ;; --- deeply nested destructuring ---
 (et--test-pcase '(pcase (cons 1 (cons "2" 3)) (`(,x ,y . ,z) y)) :resolves 'String)
 (et--test-pcase '(pcase (cons 1 (cons "2" 3)) (`(,x ,y . ,z) z)) :resolves 'Integer)

 ;; --- literal narrowing keeps the fallthrough binding ---
 (et--test-pcase '(pcase (:type Integer) (5 5) (n n)) :resolves 'Integer)

 ;; --- and: binding plus guard ---
 (et--test-pcase '(pcase (:type Integer)
                    ((and x (guard (integerp x))) x)
                    (_ 0))
                 :resolves 'Integer)

 ;; --- or: union of branch matches, shared bindings ---
 (et--test-pcase '(pcase (:type Integer|String|Symbol)
                    ((or (pred integerp) (pred stringp)) (:type Integer|String))
                    (_ (:type Integer)))
                 :resolves 'Integer|String)

 ;; --- app applies a function before matching ---
 (et--test-pcase '(pcase (cons 1 "2") ((app car n) n)) :resolves 'Integer))


;;;; pcase-let / pcase-let*

;; These bind the variables of a pcase pattern from a value, assuming the
;; pattern matches.  They reuse the pattern protocol above to derive the
;; bound vars and the scrutinee narrowing, then check the body with those
;; in scope -- like the `let'/`let*' checkers but with destructuring.

(defun et--pcase-let-check (sequential)
  "Check the current `pcase-let'/`pcase-let*' form, returning the body type.

The form has the shape (HEAD BINDINGS BODY...), where each binding is
(PATTERN &optional VALUE).  When SEQUENTIAL is non-nil each VALUE is
checked with the variables bound by earlier bindings in scope (as in
`pcase-let*'); otherwise every VALUE is checked in the outer scope (as in
`pcase-let')."
  (let ((bindings (nth 1 et--checker-expr))
        (vars nil)
        (binds nil))
    (cl-loop for binding in bindings
             for bi upfrom 0
             do (let* ((pat (car binding))
                       ;; A binding may omit its value form, binding to nil.
                       (val-type (cond ((null (cdr binding)) (et Nil))
                                       (sequential
                                        (et-with-vars vars (et-checker-sub 1 bi 1)))
                                       (t (et-checker-sub 1 bi 1))))
                       (res (et--pcase-check pat val-type (and sequential vars)
                                             (list 1 bi 0))))
                  (setq vars (append vars (et-pcase-result-vars res)))
                  (setq binds (append binds (et--type-binds
                                             (et-pcase-result-matched-type res))))))
    (et-with-vars vars
      (et-with-narrow-binds binds
        (et-checker-tail 2)))))

(et-define-checker pcase-let*
  (et--pcase-let-check t))

(et-define-checker pcase-let
  (et--pcase-let-check nil))


;;;; pcase-dolist

;; Like `dolist', but destructures each element with a pcase pattern.
;; Always returns nil; the body is checked for side-effect diagnostics.
(et-define-checker pcase-dolist
  (let* ((spec (nth 1 et--checker-expr))
         (pat (car spec))
         (elem-type (or (et-checker-infer (et-checker-sub 1 1) [T] ListR<T> T)
                        (et-any)))
         (res (et--pcase-check pat elem-type nil (list 1 0))))
    (et-with-vars (et-pcase-result-vars res)
      (et-with-narrow-binds (et--type-binds (et-pcase-result-matched-type res))
        (et-checker-tail 2)))
    (et Nil)))


;;;; Macro tests

(et-test
 ;; --- pcase-let* destructures and binds car/cdr ---
 (et--test-pcase '(pcase-let* ((`(,x . ,y) (cons 1 "s"))) x) :resolves 'Integer)
 (et--test-pcase '(pcase-let* ((`(,x . ,y) (cons 1 "s"))) y) :resolves 'String)
 ;; --- later bindings see earlier ones (let*-style) ---
 (et--test-pcase '(pcase-let* ((`(,x . ,_) (cons 1 "s")) (z x)) z) :resolves 'Integer)
 ;; --- pcase-let binds the same way ---
 (et--test-pcase '(pcase-let ((`(,a . ,b) (cons 1 "s"))) b) :resolves 'String)
 ;; --- pcase-exhaustive narrows like pcase but drops the nil fallthrough ---
 (et--test-pcase '(pcase-exhaustive (cons 1 2) (`(,x . ,y) x))
                 :resolves 'Integer :not-resolves 'Nil)
 ;; --- pcase-dolist binds the element pattern and returns nil ---
 (et--test-pcase '(pcase-dolist (`(,k . ,v) (list (cons 1 "s"))) k) :resolves 'Nil))
