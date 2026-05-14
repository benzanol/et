;;; et-types.el --- Typesystem for emacs lisp        -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Adam Tillou

;; Author: Adam Tillou;; -*- lexical-binding: t; -*- <benzanol@nixos>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:
;;; Code:

(require 'et-check)


;;; ============================================================
;;; Definitions
;;;; Ignore certain forms

(et-define-checker declare (et Nil))


;;;; Function checkers

(et-define-pcase-checker lambda body
  (setq body (et--funcdef-inline-to-declare (copy-tree body)))
  (et-checker-function-body (et-parse-function-type body #'lambda) 1))

(et-test
 ;; Inline typed args
 (et-assert-resolve Function<ConsR<Integer~Nil>~Integer>
   (lambda ([x Integer]) (+ x 1)))

 ;; Untyped args default to Any
 (et-assert-resolve Function<ConsR<Any~Nil>~Any>
   (lambda (x) x))

 ;; &optional — cdr is Nil|ConsR<String~Nil>
 (et-assert-resolve (Function ConsR<Integer~Nil|ConsR<String~Nil>> Integer)
   (lambda ([x Integer] &optional [y String]) x))

 ;; Multiple body forms — return type is last
 (et-assert-resolve Function<ConsR<Integer~ConsR<String~Nil>>~String>
   (lambda ([x Integer] [y String]) (+ x 1) y))

 ;; Empty arglist
 (et-assert-resolve Function<Nil~Integer>
   (lambda () 1))

 ;; &rest
 (et-assert-resolve (Function ConsR<Integer~ListR<Any>> Integer)
   (lambda ([x Integer] &rest args) x))
 (et-assert-resolve (Function ConsR<Any~ListR<Any>> ListR<Any>)
   (lambda (x &rest args) args)))

(et-test
 ;; Body return type with typed args
 (et-assert-resolve Function<ConsR<Integer~Nil>~Integer>
   (lambda ([x Integer]) (+ x 1)))

 ;; Untyped args default to Any, body uses them
 (et-assert-resolve Function<ConsR<Any~Nil>~Any>
   (lambda (x) x))

 ;; &optional arg becomes Nil|Type
 (et-assert-resolve Function<ConsR<Integer~Nil|ConsR<String~Nil>>~Integer>
   (lambda ([x Integer] &optional [y String]) (+ x 1)))

 ;; Multiple body forms, return type is last
 (et-assert-resolve Function<ConsR<Integer~ConsR<String~Nil>>~String>
   (lambda ([x Integer] [y String]) (+ x 1) y))

 ;; Empty arglist
 (et-assert-resolve Function<Nil~Integer>
   (lambda () 1)))

;; Return type annotation tests
(et-test
 ;; Inline -> return type: uses declared type
 (et-assert-resolve Function<ConsR<Integer~Nil>~Number>
   (lambda ([x Integer]) -> Number (+ x 1)))

 ;; Inline -> with typed args
 (et-assert-resolve Function<ConsR<Integer~Nil>~Number>
   (lambda ([x Integer]) -> Number x))

 ;; Inline -> is stripped from compiled output
 (let* ((result (et--check '(lambda (x) -> Integer 1))))
   (equal (et-result-compiled result) '(lambda (x) 1)))

 ;; Inline -> with body type mismatch produces error
 (et-assert-resolve-errors
  (lambda ([x Integer]) -> String x))

 ;; Inline -> with empty arglist
 (et-assert-resolve Function<Nil~Number>
   (lambda () -> Number 1))

 ;; Inline -> with multiple body forms
 (et-assert-resolve Function<ConsR<Integer~Nil>~Number>
   (lambda ([x Integer]) -> Number "ignored" x)))


;;;; Var checkers

(defun et--parse-defvar-docstring (docstring)
  "Parse a @et-type annotation from DOCSTRING, returning a type or nil."
  (when (and (stringp docstring) (string-match "@et-type" docstring))
    (when-let* ((type-expr (ignore-errors (car (read-from-string docstring (match-end 0))))))
      (condition-case _err (et-parse-type type-expr)
        (error nil)))))

(et-define-pcase-checker defvar
    `(,(and (pred symbolp) name) .
      ,(or 'nil `(,val . ,(or 'nil `(,(and docstring (pred stringp)))))))
  (let* ((declared-type (et--parse-defvar-docstring docstring))
         (value-type (or (when val (et-checker-sub 2)) (et Nil))))

    (when declared-type
      (or (et-subtype? value-type declared-type)
          (et-checker-err 2 "Initial value type %s does not satisfy declared type %s"
                          (et-pp value-type) (et-pp declared-type))))

    (setcar et--checker-expr 'defvar)
    (put name 'et-variable-type (or declared-type value-type))))

(defmacro et-defvar (&rest args)
  "Define a type-checked function.

\(fn NAME [INIT-VAL] [DOCSTRING])"
  (declare (doc-string 3) (indent 2))

  (let* ((result (et--check (cons #'defvar args))))
    (et-show-result-errors result)
    (et-result-compiled result)))


;;; ============================================================
;;; Control flow
;;;; let*

(et-define-pcase-checker let*
    `(,(et-* [(var vars)]
             (and
              (or
               ;; Explicit type annotation
               (and form `(,name ,type-spec ,_val)
                    (let type (et-parse-type type-spec))
                    (let _1 (setcdr form (cddr form)))
                    (let _2 (et-with-vars vars (et-checker-resolve type 1 (length vars) 1))))
               ;; Implicit type
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


;;;; dolist

(et-define-pcase-checker dolist
    `(,(or (and form `(,name ,(app et-parse-type elem-type) ,_lst)
                (let _1 (setcdr form (cddr form)))
                (let _2 (et-checker-resolve (et-alias 'ListR elem-type) 1 1)))
           (and `(,name ,_lst)
                (let elem-type (et-checker-infer (et-checker-sub 1 1) [T] ListR<T> T))))
      . ,_body)

  (et-with-vars (list (et-new-var name elem-type))
    (et-checker-sub 1)))


;;;; pcase
;;;;; Pcase pattern protocol

;; A pcase pattern checker is a function registered via
;; `et-define-pcase-pattern'.  It receives the scrutinee type (the type
;; of the value being matched) and the pattern form, and must return an
;; `et-pcase-result' struct describing:
;;
;;   MATCHED-TYPE   — the narrowed type of the scrutinee when this
;;                    pattern matches.  For example, (pred integerp)
;;                    narrows to Integer.  Use the scrutinee type
;;                    unchanged if the pattern cannot narrow.
;;
;;   VARS           — a list of `et-var' instances for variables bound
;;                    by this pattern.  Each var has a name (symbol) and
;;                    type.  The body of the matching clause will be
;;                    typechecked with these vars in scope.
;;
;;   RESIDUAL-TYPE  — the narrowed type of the scrutinee when this
;;                    pattern does NOT match.  This is threaded through
;;                    subsequent clauses so that later patterns see a
;;                    progressively refined "what's left" type.  Defaults
;;                    to SCRUTINEE (no narrowing on failure) when nil.
;;
;; A pattern checker signals errors via `et-checker-err' / `et-checker-warn'.
;;
;; Composite patterns (and, or, guard, let, app, pred, cl-struct, etc.)
;; recursively call `et--pcase-check-pattern' on sub-patterns.
;;
;; The entry point `et--pcase-check-pattern' dispatches on the pattern
;; head symbol's `et-pcase-pattern' property.  Bare symbols are
;; variable-capture patterns; _ and otherwise are wildcards.

(cl-defstruct et-pcase-result
  "Result of checking a pcase pattern against a scrutinee type."
  (matched-type nil :documentation "Narrowed scrutinee type when matched.")
  (vars nil :documentation "List of `et-var' bound by this pattern.")
  (residual-type nil :documentation "Scrutinee type when NOT matched (nil = unchanged)."))


;;;;; Pattern registry

(defvar et--pcase-patterns (make-hash-table :test #'eq)
  "Map from pattern-head symbol to checker function.

Each function takes (SCRUTINEE-TYPE PATTERN) and returns an
`et-pcase-result'.")

(defmacro et-define-pcase-pattern (name args &rest body)
  "Register a pcase pattern checker.

NAME is the pattern head symbol (e.g. `pred', `guard', `\\=`').
ARGS is (SCRUTINEE-TYPE PATTERN) — the two arguments the checker
receives.  BODY should return an `et-pcase-result'.

\(fn NAME (SCRUTINEE-TYPE PATTERN) BODY...)"
  (declare (indent 2))
  `(puthash ',name (lambda ,args ,@body) et--pcase-patterns))

(defun et--pcase-check-pattern (scrutinee-type pattern)
  "Dispatch PATTERN against SCRUTINEE-TYPE, returning an `et-pcase-result'.

Bare symbols bind as variables (with `_' and `otherwise' as wildcards).
Lists dispatch on the car's `et-pcase-pattern' entry.
Self-quoting atoms (numbers, strings, keywords) match as literals."
  (pcase pattern
    ;; Wildcard
    ((or '_ 'otherwise 'pcase--dontcare)
     (make-et-pcase-result :matched-type scrutinee-type))

    ;; Variable capture
    ((and (pred symbolp) sym)
     (make-et-pcase-result
      :matched-type scrutinee-type
      :vars (list (et-new-var sym scrutinee-type))))

    ;; Self-quoting literals (numbers, strings, keywords)
    ((or (pred numberp) (pred stringp) (pred keywordp))
     (let* ((lit-type (et-literal pattern))
            (matched (et--supersect scrutinee-type lit-type))
            (residual (et--subtract scrutinee-type lit-type)))
       (make-et-pcase-result :matched-type matched :residual-type residual)))

    ;; Compound pattern
    (`(,head . ,_)
     (if-let* ((checker (gethash head et--pcase-patterns)))
         (funcall checker scrutinee-type pattern)
       (et-checker-warn "Unknown pcase pattern head: %s" head)
       (make-et-pcase-result :matched-type scrutinee-type)))

    (_ (et-checker-warn "Unrecognized pcase pattern: %s" pattern)
       (make-et-pcase-result :matched-type scrutinee-type))))


;;;;; pcase checker

(et-define-checker pcase
  (let* ((scrut-type (et-checker-sub 1))
         (clauses (cddr et--checker-expr))
         (remaining-type scrut-type)
         (result-types nil))

    (cl-loop
     for clause in clauses
     for clause-idx upfrom 2
     for (pat . body) = clause
     for pat-result = (et--pcase-check-pattern remaining-type pat)
     for matched = (or (et-pcase-result-matched-type pat-result) remaining-type)
     for vars = (et-pcase-result-vars pat-result)
     for residual = (et-pcase-result-residual-type pat-result)
     ;; Typecheck the body with pattern vars in scope and scrutinee narrowed
     for body-type = (et-with-vars vars
                       (et-with-narrow-binds (et--type-binds matched)
                         (if (null body) (et Nil)
                           ;; Check each body form; return type of last
                           (cl-loop for bi upfrom 1 below (length clause)
                                    for sub-type = (et-checker-sub clause-idx bi)
                                    finally return sub-type))))
     do (push body-type result-types)
     do (setq remaining-type (or residual remaining-type))
     ;; If remaining is never, later clauses are unreachable
     when (et-never-p remaining-type) return nil)

    (if result-types
        (et-simplify-type (apply #'et--or (nreverse result-types)))
      (et Nil))))


;;;;; quote pattern

(et-define-pcase-pattern quote (scrutinee-type pattern)
  ;; (quote VAL)
  (let* ((val (cadr pattern))
         (lit-type (et-literal val))
         (matched (et--supersect scrutinee-type lit-type))
         (residual (et--subtract scrutinee-type lit-type)))
    (make-et-pcase-result :matched-type matched :residual-type residual)))


;;;;; backquote pattern

(defun et--pcase-check-backquote (scrutinee-type pattern)
  "Check a backquote pattern, returning an `et-pcase-result'.

Backquote patterns are recursive:
  \\=`ATOM        — literal match (like quote)
  \\=`(A . B)     — matches a cons, recursing on car/cdr
  \\=`,PAT        — splice: delegates to the inner PAT
  \\=`,@PAT       — not supported in pcase; ignored"
  (pcase pattern
    ;; ,PAT — unquote splice, delegate to inner pattern
    (`(,'\, ,inner)
     (et--pcase-check-pattern scrutinee-type inner))

    ;; (A . B) — cons destructuring
    (`(,car-pat . ,cdr-pat)
     (let* ((cons-type (et--supersect scrutinee-type (et Cons)))
            (car-scrut (or (et-checker-infer cons-type [L] ConsR<L~Any> L) (et-any)))
            (cdr-scrut (or (et-checker-infer cons-type [R] ConsR<Any~R> R) (et-any)))
            (car-result (et--pcase-check-backquote car-scrut car-pat))
            (cdr-result (et--pcase-check-backquote cdr-scrut cdr-pat))
            (all-vars (append (et-pcase-result-vars car-result)
                              (et-pcase-result-vars cdr-result)))
            ;; Matched type: a cons narrowed by both sub-patterns
            (matched (et--supersect cons-type
                                    (et-alias 'ConsR
                                              (or (et-pcase-result-matched-type car-result)
                                                  car-scrut)
                                              (or (et-pcase-result-matched-type cdr-result)
                                                  cdr-scrut)))))
       (make-et-pcase-result :matched-type matched :vars all-vars)))

    ;; Literal atom — same as quote
    ((or (pred symbolp) (pred numberp) (pred stringp) (pred keywordp))
     (let* ((lit-type (et-literal pattern))
            (matched (et--supersect scrutinee-type lit-type))
            (residual (et--subtract scrutinee-type lit-type)))
       (make-et-pcase-result :matched-type matched :residual-type residual)))

    ;; Vector pattern — not yet supported
    (_ (et-checker-warn "Unsupported backquote pattern form: %s" pattern)
       (make-et-pcase-result :matched-type scrutinee-type))))

(et-define-pcase-pattern \` (scrutinee-type pattern)
  (et--pcase-check-backquote scrutinee-type (cadr pattern)))


;;;;; pred pattern

(et-define-pcase-pattern pred (scrutinee-type pattern)
  ;; (pred FUNC) — FUNC is a predicate; narrow by its return type
  (let* ((pred-form (cadr pattern))
         (pred-type
          (pcase pred-form
            ((and sym (pred symbolp))
             (or (get sym 'et-function-type) nil))
            (`(lambda . ,_)
             (et-result-type (et--check pred-form)))
            (_ nil)))
         ;; If the predicate has a DynFunction type, apply it to get narrowing
         (call-result
          (when pred-type
            (et--funcall pred-type (et-alias 'ConsR scrutinee-type (et-literal nil)))))
         ;; Extract binds from the True branch of the call result for narrowing
         (binds (when call-result (et--type-binds (et--non-nil call-result))))
         (matched (if binds
                      (cl-loop for (_var . type) in binds
                               with acc = scrutinee-type
                               do (setq acc (et--supersect acc type))
                               finally return acc)
                    scrutinee-type))
         (residual-binds (when call-result
                           (et--type-binds (et--supersect call-result (et Nil)))))
         (residual (if residual-binds
                       (cl-loop for (_var . type) in residual-binds
                                with acc = scrutinee-type
                                do (setq acc (et--supersect acc type))
                                finally return acc)
                     scrutinee-type)))
    (make-et-pcase-result :matched-type matched :residual-type residual)))


;;;;; guard pattern

(et-define-pcase-pattern guard (scrutinee-type pattern)
  ;; (guard EXPR) — matches when EXPR is non-nil; binds nothing
  ;; We cannot narrow the scrutinee from a guard (it's an arbitrary expr),
  ;; but we do typecheck the expression.
  (let* ((expr (cadr pattern))
         (_expr-type (et-result-type (et--check expr))))
    (make-et-pcase-result :matched-type scrutinee-type)))


;;;;; app pattern

(et-define-pcase-pattern app (scrutinee-type pattern)
  ;; (app FUNC PAT) — apply FUNC to scrutinee, match result against PAT
  (let* ((func-form (cadr pattern))
         (inner-pat (caddr pattern))
         (func-type
          (pcase func-form
            ((and sym (pred symbolp)) (get sym 'et-function-type))
            (`(lambda . ,_) (et-result-type (et--check func-form)))
            (_ nil)))
         (app-result-type
          (if func-type
              (or (et--funcall func-type
                               (et-alias 'ConsR scrutinee-type (et-literal nil)))
                  (progn (et-checker-err "app function cannot accept scrutinee type %s"
                                         (et-pp scrutinee-type))
                         (et-any)))
            (et-any)))
         (inner-result (et--pcase-check-pattern app-result-type inner-pat)))
    ;; We cannot narrow the scrutinee from app (the function is opaque),
    ;; but we propagate vars from the inner pattern.
    (make-et-pcase-result
     :matched-type scrutinee-type
     :vars (et-pcase-result-vars inner-result))))


;;;;; and pattern

(et-define-pcase-pattern and (scrutinee-type pattern)
  ;; (and PAT1 PAT2 ...) — all must match; narrow progressively
  (let* ((sub-pats (cdr pattern))
         (current-type scrutinee-type)
         (all-vars nil))
    (dolist (sub sub-pats)
      (let* ((result (et--pcase-check-pattern current-type sub)))
        (setq current-type (or (et-pcase-result-matched-type result) current-type))
        (setq all-vars (append all-vars (et-pcase-result-vars result)))))
    (make-et-pcase-result :matched-type current-type :vars all-vars)))


;;;;; or pattern

(et-define-pcase-pattern or (scrutinee-type pattern)
  ;; (or PAT1 PAT2 ...) — first match wins; vars must be consistent
  ;;
  ;; Each alternative is checked against the residual of the previous,
  ;; mirroring how pcase tries alternatives in order.  The matched type
  ;; is the union of all alternatives' matched types.  Variables bound
  ;; across alternatives must have the same names; their types are
  ;; unioned.
  (let* ((sub-pats (cdr pattern))
         (remaining scrutinee-type)
         (matched-types nil)
         (var-alists nil)) ; list of alists ((name . type) ...)

    (dolist (sub sub-pats)
      (let* ((result (et--pcase-check-pattern remaining sub))
             (matched (or (et-pcase-result-matched-type result) remaining)))
        (push matched matched-types)
        (push (cl-loop for v in (et-pcase-result-vars result)
                       collect (cons (et-var-name v) (et-var-type v)))
              var-alists)
        (setq remaining (or (et-pcase-result-residual-type result) remaining))))

    ;; Union variables by name
    (let* ((merged-vars
            (when var-alists
              (cl-loop for (name . _) in (car var-alists)
                       collect
                       (et-new-var name
                                   (apply #'et--or
                                          (cl-loop for alist in var-alists
                                                   for entry = (assq name alist)
                                                   collect (if entry (cdr entry) (et-any)))))))))
      (make-et-pcase-result
       :matched-type (apply #'et--or (nreverse matched-types))
       :vars merged-vars
       :residual-type remaining))))


;;;;; let pattern

(et-define-pcase-pattern let (scrutinee-type pattern)
  ;; (let PAT EXPR) — evaluate EXPR, match against PAT
  ;; The scrutinee of pcase is irrelevant; PAT matches against EXPR's value.
  (let* ((inner-pat (cadr pattern))
         (expr (caddr pattern))
         (expr-type (et-result-type (et--check expr)))
         (result (et--pcase-check-pattern expr-type inner-pat)))
    ;; Scrutinee is unchanged; vars come from the inner pattern
    (make-et-pcase-result
     :matched-type scrutinee-type
     :vars (et-pcase-result-vars result))))


;;;;; cl-struct pattern

(et-define-pcase-pattern cl-struct (scrutinee-type pattern)
  ;; (cl-struct NAME (SLOT PAT)... | SLOT...)
  ;; Matches if the scrutinee is an instance of struct NAME.
  ;; Each SLOT can be a bare symbol (bind to that name) or (SLOT PAT).
  (let* ((struct-name (cadr pattern))
         (slot-specs (cddr pattern))
         (struct-slots (get struct-name 'et-struct-slots))
         (struct-type (et-dt 'Struct struct-name))
         (matched (et--supersect scrutinee-type struct-type))
         (all-vars nil))

    (unless struct-slots
      (et-checker-warn "Unknown struct: %s" struct-name))

    (dolist (spec slot-specs)
      (pcase spec
        ;; (SLOT PAT) — extract slot type and match inner pattern
        (`(,slot-name ,inner-pat)
         (let* ((slot-plist (alist-get slot-name struct-slots))
                (slot-type (if slot-plist (plist-get slot-plist :type) (et-any)))
                (result (et--pcase-check-pattern slot-type inner-pat)))
           (setq all-vars (append all-vars (et-pcase-result-vars result)))))

        ;; Bare SLOT — bind the slot name to its type
        ((and (pred symbolp) slot-name)
         (let* ((slot-plist (alist-get slot-name struct-slots))
                (slot-type (if slot-plist (plist-get slot-plist :type) (et-any))))
           (push (et-new-var slot-name slot-type) all-vars)))))

    (make-et-pcase-result
     :matched-type matched
     :vars (nreverse all-vars)
     :residual-type (et--subtract scrutinee-type struct-type))))


;;;;; cl-type pattern

(et-define-pcase-pattern cl-type (scrutinee-type pattern)
  ;; (cl-type TYPE) — matches if (cl-typep scrutinee 'TYPE)
  ;; Map common cl-types to et types for narrowing.
  (let* ((cl-type-name (cadr pattern))
         (narrowed
          (pcase cl-type-name
            ('integer (et Integer))
            ('number (et Number))
            ('string (et String))
            ('symbol (et Symbol))
            ('cons (et Cons))
            ('list (et or Nil Cons))
            ('null (et Nil))
            ('vector (et Vector))
            ('boolean (et Boolean))
            (_ nil)))
         (matched (if narrowed (et--supersect scrutinee-type narrowed) scrutinee-type))
         (residual (if narrowed (et--subtract scrutinee-type narrowed) scrutinee-type)))
    (make-et-pcase-result :matched-type matched :residual-type residual)))


;;;;; pcase tests

(et-test
 ;; Helper: check that a pattern against a scrutinee type yields
 ;; the expected matched type and variable bindings.
 ;; PAT-VARS is an alist of (VAR-NAME . TYPE).

 ;; Literal match narrows and subtracts
 (let* ((r (et--pcase-check-pattern (et 1|2|3) '(quote 1))))
   (and (et-subtype? (et-pcase-result-matched-type r) (et 1))
        (et-subtype? (et-pcase-result-residual-type r) (et 2|3))
        (null (et-pcase-result-vars r))))

 ;; Wildcard binds nothing, no narrowing
 (let* ((r (et--pcase-check-pattern (et Integer) '_)))
   (and (equal (et-pcase-result-matched-type r) (et Integer))
        (null (et-pcase-result-vars r))))

 ;; Variable capture
 (let* ((r (et--pcase-check-pattern (et Integer) 'x)))
   (and (equal (et-pcase-result-matched-type r) (et Integer))
        (equal (et-var-name (car (et-pcase-result-vars r))) 'x)
        (equal (et-var-type (car (et-pcase-result-vars r))) (et Integer))))

 ;; Self-quoting number
 (let* ((r (et--pcase-check-pattern (et 1|2) 1)))
   (et-subtype? (et-pcase-result-matched-type r) (et 1)))

 ;; pred with known predicate — narrows scrutinee
 (let* ((r (et--pcase-check-pattern (et {Number|String}&{::$a}) '(pred integerp))))
   (et-subtype? (et-pcase-result-matched-type r) (et Integer)))

 ;; and pattern — progressive narrowing + var collection
 (let* ((r (et--pcase-check-pattern (et {Number|String}&{::$a}) '(and (pred integerp) x))))
   (and (et-subtype? (et-pcase-result-matched-type r) (et Integer))
        (equal (et-var-name (car (et-pcase-result-vars r))) 'x)
        (et-subtype? (et-var-type (car (et-pcase-result-vars r))) (et Integer))))

 ;; or pattern — union of matched, union of var types
 (let* ((r (et--pcase-check-pattern (et 1|2|%hi) '(or 1 2))))
   (and (et-subtype? (et-pcase-result-matched-type r) (et 1|2))
        (et-subtype? (et-pcase-result-residual-type r) (et %hi))))

 ;; backquote cons destructuring
 (let* ((r (et--pcase-check-pattern (et ConsR<Integer~String>) '(\` (,a . ,b)))))
   (and (equal (length (et-pcase-result-vars r)) 2)
        (equal (et-var-name (car (et-pcase-result-vars r))) 'a)
        (equal (et-var-name (cadr (et-pcase-result-vars r))) 'b)))

 ;; backquote literal
 (let* ((r (et--pcase-check-pattern (et 1|2|3) '(\` 1))))
   (et-subtype? (et-pcase-result-matched-type r) (et 1)))

 ;; let pattern — vars from inner, scrutinee unchanged
 (let* ((r (et--pcase-check-pattern (et Integer) '(let x (+ 1 2)))))
   (and (equal (et-pcase-result-matched-type r) (et Integer))
        (equal (et-var-name (car (et-pcase-result-vars r))) 'x)))

 ;; guard — no narrowing, no vars
 (let* ((r (et--pcase-check-pattern (et Integer) '(guard (> 1 0)))))
   (and (equal (et-pcase-result-matched-type r) (et Integer))
        (null (et-pcase-result-vars r))))

 ;; app pattern
 (let* ((r (et--pcase-check-pattern (et ListR<Integer>) '(app car x))))
   (et-subtype? (et-var-type (car (et-pcase-result-vars r))) (et Integer|Nil)))

 ;; cl-type pattern
 (let* ((r (et--pcase-check-pattern (et Any) '(cl-type integer))))
   (et-subtype? (et-pcase-result-matched-type r) (et Integer)))

 ;; Full pcase expression: literal cases with exhaustive match
 (et-subtype? (et-typecheck
               (let* ((x Integer|String 5))
                 (pcase x
                   ((pred integerp) (+ x 1))
                   ((pred stringp) x))))
              (et Integer|String))

 ;; Full pcase: variable binding in pattern
 (et-subtype? (et-typecheck
               (pcase (cons 1 "hi")
                 (`(,a . ,b) (cons b a))))
              (et Cons))

 ;; Full pcase: or pattern with fallthrough
 (et-subtype? (et-typecheck
               (pcase 1
                 ((or 1 2) "match")
                 (_ "no")))
              (et String))

 ;; Full pcase: and + pred narrowing in body
 (et-subtype? (et-typecheck
               (let* ((v Number|String 5))
                 (pcase v
                   ((and (pred integerp) n) (+ n 1))
                   (_ 0))))
              (et Integer|Number)))


;;;; setq

(et-define-pcase-checker setq (and args (guard (eq 0 (mod (length args) 2))))
  (cl-loop for (var _val) on args by #'cddr
           for var-pos upfrom 1 by 2
           for type = (or (et-get-symbol-type var)
                          (et-checker-err var-pos "Assignment to free variable"))
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

(et-define-pcase-checker when `(,_cond . ,then)
  (let* ((cond-type (et-checker-sub 1)))
    (et-checker-hint-narrows 0 "WHEN:\\n%s" (et--non-nil cond-type))
    ;; Special case for empty then block because (when cond) always returns nil
    (if (null then) (et Nil)
      (et--and-return-type cond-type (lambda () (et-checker-tail 2))))))

(et-define-pcase-checker unless `(,_cond . ,_else)
  (let* ((cond-type (et-checker-sub 1)))
    (et-checker-hint-narrows 0 "UNLESS:\\n%s" (et--supersect cond-type (et Nil)))
    ;; Special case for empty then block because (when cond) always returns nil
    (et--or-return-type cond-type (lambda () (et-checker-tail 2)))))


(et-test
 (equal (et String|Nil)
        (et--remove-type-binds
         (et-typecheck
          (let* ((a String|Number 4))
            (when (stringp a) a)))))
 (et-subtype? (et-typecheck
               (let* ((a String|Number 4))
                 (if (stringp a) a "hello!")))
              (et String)))


;;; ============================================================
;;; Function types
;;;; Quotes
;;;;; quote

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


;;;;; function

(et-define-pcase-checker function `(,inner)
  (pcase inner
    ;; Lambda expression: typecheck it as a lambda
    (`(lambda . ,_rest)
     (et-checker-sub 1))

    ;; Symbol: look up its function type
    ((and sym (pred symbolp))
     (if-let* ((func-type (get sym 'et-function-type)))
         func-type
       (et-checker-err "No function type for `%s'" sym)))

    ;; Anything else is invalid
    (_ (et-checker-err "Invalid argument to function: %s" inner))))

(et-test
 (et-assert-resolve Function<ConsR<Integer~Nil>~Integer>
   #'(lambda ([x Integer]) x))

 (not (et-never-p (et-result-type (et--check '#'+))))
 (et-never-p (et-result-type (et--check '#'function))))


;;;; Arithmetic

(et-define-type-checker (+ -) [N]
  (or Nil&{N=0} ListR<Integer>&{N=Integer} ListR<Number>&{N=Number})
  N)

(et-define-type-checker * [N]
  (or Nil&{N=1} ListR<Integer>&{N=Integer} ListR<Number>&{N=Number})
  N)

(et-define-type-checker / [N]
  (or NonNilListR<Integer>&{N=Integer} NonNilListR<Number>&{N=Number})
  N)

(et-define-type-checker (1+ 1-) [N] (Args Integer&{N=Integer}|Number&{N=Number}) N)

;; (et-typecheck-call + Integer Integer 1 2.1 3)

(et-test
 op [+ - * /]
 (et-assert-call Integer op Integer Integer 1 2 3)
 (et-assert-call Integer op Integer Integer 1 2 3)
 (et-assert-call Integer op Integer Integer 1 2 3)
 (et-assert-call Number op Integer Integer 1 2.1 3)
 (et-assert-call Integer op 1))

(et-test
 (et-assert-call 0 +)
 (et-assert-call 0 -)
 (et-assert-call 1 *)
 (et-assert-call-errors /))


;;;; Sequences
;;;;; cons

(et-define-type-checker cons [L R] (Args L R) ConsFresh<L~R>)


(et-test
 (et-assert-resolve ConsFresh<Integer~String> (cons 1 "2"))
 (et-assert-resolve Cons<Integer~String> (cons 1 "2"))
 (et-assert-no-resolve ConsFresh<Integer~String> (cons "1" 2))
 (et-assert-no-resolve Cons<Integer~String> (cons "1" 2))
 (et-assert-resolve ConsFresh<Integer~ListR<String>> (cons 1 nil))
 (et-assert-resolve Cons<Integer~List<String>> (cons 1 (cons "2" nil)))

 (et-assert-resolve List<Integer> (cons 1 (cons 2 nil)))
 (et-assert-no-resolve List<Integer> (cons 1 (cons "2" nil)))
 (et-assert-no-resolve List<Integer> (cons "1" (cons 2 nil)))
 (et-assert-no-resolve List<Integer> (cons 1 (cons 2 t))))


;;;;; list/copy-tree

(et-define-type-checker list [T] T (eval et--freshen-type-shallow T))
(et-define-type-checker copy-tree [T] (Args T) (eval et--freshen-type T))

(et-test
 (equal (et ConsFresh<Cons<1~2>~ConsFresh<Cons<3~4>~Nil>>)
        (et-typecheck-call list Cons<1~2> Cons<3~4>))
 (equal (et ConsFresh<ConsFresh<1~2>~ConsFresh<ConsFresh<3~4>~Nil>>)
        (et-typecheck-call copy-tree (TupleR Cons<1~2> Cons<3~4>)))

 (et-assert-resolve ConsR<Integer~ListR<String>> (list 1 "2"))
 (et-assert-no-resolve ConsR<Integer~String> (list "1" 2))
 (et-assert-no-resolve ConsR<Integer~String> (list))

 (et-assert-resolve ListR<Integer> (list 1 2 3))
 (et-assert-resolve ListR<Integer> (list 1))
 (et-assert-no-resolve ListR<Integer> (list 1 "2" 3)))


;;;;; car/cdr

(et-defalias MatchCar (car)
  (or (and Nil (set ,car Nil))
      (ConsR ,car Any)))

(et-defalias MatchCdr (cdr)
  (or (and Nil (set ,cdr Nil))
      (ConsR Any ,cdr)))

(et-define-type-checker car [T] (Args (MatchCar T)) T)
(et-define-type-checker cdr [T] (Args (MatchCdr T)) T)

(et-test
 (et-assert-resolve Integer (car (list 1 2.2 3)))
 (et-assert-no-resolve Integer (car (list 1.1 2 3)))
 (et-assert-resolve Integer (car (cons 1 "3")))
 (et-assert-resolve ListR<Integer> (car (cons (list 1) "3")))
 (et-assert-resolve ConsR<Integer~Any> (car (cons (list 1) "3")))
 (et-assert-resolve Integer (car (car (cons (list 1) "3"))))
 (et-assert-no-resolve Integer (car (car (cons (list 1.1) "3"))))

 (et-assert-call Never cdr Never)
 (et-assert-call Nil cdr Nil)
 (et-assert-call Nil|String cdr Nil|ConsR<Integer~String>)
 (et-assert-call-errors cdr Nil|ConsR<Integer~String>|String)
 (et-assert-call-errors cdr :any)

 (et-assert-call Never car Never)
 (et-assert-call Nil car Nil)
 (et-assert-call Nil|Integer car Nil|ConsR<Integer~String>)
 (et-assert-call-errors car Nil|ConsR<Integer~String>|String)
 (et-assert-call-errors car Any)

 (et-assert-call Nil|Integer car ListR<Integer>)

 (et-assert-call Nil|Integer|String car ListR<Integer>|ConsR<String~Nil>)

 (et-assert-error
     (et-root-check-call car ListR<Integer>|ConsR<String~Nil>|String)))

(et-test
 (et-assert-resolve ListR<Number> (cdr (list 1 2.2 3)))
 (et-assert-resolve ListR<Integer> (cdr (list 1.1 2 3)))
 (et-assert-no-resolve ListR<Integer> (car (list 1 2.2 3)))

 (et-assert-resolve Integer (cdr (cons "1" 2)))
 (et-assert-no-resolve Integer (cdr (cons 1 "2")))

 (et-assert-resolve ListR<Integer> (cdr (cons "1" (list 2))))
 (et-assert-resolve ConsR<Integer~Any> (cdr (cons "1" (list 2))))
 (et-assert-resolve ConsR<Integer~Boolean> (cdr (cons "1" (list 2))))
 (et-assert-no-resolve ConsR<Integer~Boolean> (cdr (cons "1" (list 2 3))))
 (et-assert-resolve Integer (car (cdr (cons "1" (list 2)))))

 (et-assert-resolve Boolean (cdr (cdr (cdr (list 1 2 3)))))
 (et-assert-no-resolve Boolean (cdr (cdr (list 1 2 3))))

 (et-assert-call ListR<Integer> cdr ListR<Integer>)
 (et-assert-call ListR<Integer>|String cdr ListR<Integer>|ConsR<Nil~String>)
 (et-assert-call-errors car ListR<Integer>|ConsR<String~Nil>|String))


;;;;; c[ad][ad][ad]?r

(et-define-type-checker caar [T] (Args (MatchCar (MatchCar T))) T)
(et-define-type-checker cddr [T] (Args (MatchCdr (MatchCdr T))) T)

(et-define-type-checker cadr [T] (Args (MatchCdr (MatchCar T))) T)
(et-define-type-checker cdar [T] (Args (MatchCar (MatchCdr T))) T)

(et-define-type-checker caaar [T] (Args (MatchCar (MatchCar (MatchCar T)))) T)
(et-define-type-checker cdddr [T] (Args (MatchCdr (MatchCdr (MatchCdr T)))) T)

(et-define-type-checker caadr [T] (Args (MatchCdr (MatchCar (MatchCar T)))) T)
(et-define-type-checker cddar [T] (Args (MatchCar (MatchCdr (MatchCdr T)))) T)

(et-define-type-checker caddr [T] (Args (MatchCdr (MatchCdr (MatchCar T)))) T)
(et-define-type-checker cdaar [T] (Args (MatchCar (MatchCar (MatchCdr T)))) T)

(et-define-type-checker cadar [T] (Args (MatchCar (MatchCdr (MatchCar T)))) T)
(et-define-type-checker cdadr [T] (Args (MatchCdr (MatchCar (MatchCdr T)))) T)

(et-test
 (et-assert-resolve 2 (cadr '(1 2 3)))
 (et-assert-resolve 3 (caddr '(1 2 3)))

 (et-assert-resolve 0 (cadar '((-1 0) 1 2 3)))
 (et-assert-resolve List<3> (cdddr '((-1 0) 1 2 3))))


;;;;; setcar

(et-define-type-checker setcar [A] (Args A Nil|ConsW<A~Never>) A)

(et-test
 (et-typecheck-call setcar Number ConsW<Number~Number>))


;;;;; nth/nthcdr

(et-define-type-checker nth [T] (Args Integer ListR<T>) T|Nil)

(et-define-type-checker nthcdr [T] (Args Integer ListR<T>) ListR<T>)

(et-test
 (et-assert-call Number|String|Nil nth Integer ConsR<Number~ListR<String>>)

 (et-assert-call ListR<Number|String> nthcdr Integer ConsR<Number~ListR<String>>)
 (et-assert-call ListR<Never> nthcdr Integer Nil))


;;;;; length

(et-define-type-checker length [] (Args String|ListR<Any>|VectorR<Any>) Integer)

(et-test
 (et-assert-call Integer length VectorR<Number>|ListR<String>)
 (et-assert-call-errors length VectorR<Number>|ListR<String>|Number))


;;;;; vector

(et-define-type-checker vector [T] ListR<T> Vector<T>)

(et-test
 (et-assert-call Vector<1|2> vector 1 2)
 (et-assert-call Vector<Never> vector)
 (et-assert-call Vector<Number> vector Integer Number Positive))


;;;;; aref

(et-define-type-checker aref [T] (Args VectorR<T>|{String&T=Integer} Integer) T)

(et-test
 (et-assert-call Integer aref String Integer)
 (et-assert-call Symbol|Integer aref (or VectorR<Symbol> String) Integer)
 (et-assert-call-errors aref (or VectorR<Symbol> String ListR<Any>) Integer))


;;;;; alists

(et-define-type-checker (assq assoc rassq rassoc) [C] (Args Any ListR<C&Cons>) C|Nil)
(et-define-type-checker alist-get [V] (Args Any ListR<ConsR<Any~V>>) V|Nil)

(et-test
 (et-assert-call-errors alist-get Integer ConsR<ConsR<1~2>~ConsR<3~Nil>>)
 (et-assert-call 2|Nil alist-get Integer AList<1~2>)
 (et-assert-resolve 2|4|Nil (alist-get 4 (list (cons 1 2) (cons 3 4)))))


;;;;; mapcar/mapc/mapconcat

(et-define-type-checker mapcar [T R] (Args Function<Args<T>~R> ListR<T>) ListFresh<R>)
(et-define-type-checker mapc [T R] (Args Function<Args<T>~R> ListR<T>) Nil)

(et-define-type-checker mapconcat [T R]
  (ArgsWithTail Function<Args<T>~String> ListR<T> Nil|ConsR<String~Nil>)
  String)


;;;;; append/nconc

(et-defalias AppendFresh (elem tail)
  (or ,tail (ConsFresh ,elem (AppendFresh ,elem ,tail))))

(defun et--append-return-type (input-type)
  (or (et-checker-infer input-type [] Nil Nil)
      (et-checker-infer input-type [S] (ConsR S Nil) S)
      (et-checker-infer input-type [E R] (ConsR List<E> R)
                        (AppendFresh E (eval et--append-return-type R)))))

(et-define-type-checker append [A] A (eval et--append-return-type A))

(et-define-type-checker nconc [E] ListR<List<E>> List<E>)

(et-test
 (equal (list (et Number))
        (et--sub-match
         (et-matcher [T] List<T>)
         (et-typecheck-call append List<1> List<Integer> List<Number>)))

 ;; I thought this was a bug at first, but it is actually correct!
 ;; Since the tail is List<Integer>, this is NOT a valid List<Number>,
 ;; since List<Number> means you could nconc 0.5 onto the end, which
 ;; would add 0.5 to a list of Integers. Either inferring a ListR, or
 ;; adding Nil to the end make the check valid, as is shown in the
 ;; following tests
 (equal 'INVALID
        (et--sub-match
         (et-matcher [T] List<T>)
         (et-typecheck-call append List<1> List<Number> List<Integer>)))
 (equal (list (et Number))
        (et--sub-match
         (et-matcher [T] List<T>)
         (et-typecheck-call append List<1> List<Number> List<Integer> Nil)))
 (equal (list (et Number))
        (et--sub-match
         (et-matcher [T] ListR<T>)
         (et-typecheck-call append List<1> List<Integer> List<Number> List<1>))))


;;;; Predicates

(defmacro et-define-predicate (name type)
  `(et-define-type-checker ,name [T]
     (Args T)
     (or (and True (bindsof (and T ,type)))
         (and Nil (bindsof (subtract T ,type))))))

(et-define-predicate stringp String)
(et-define-predicate numberp Number)
(et-define-predicate integerp Integer)
(et-define-predicate consp Cons)
(et-define-predicate listp Nil|Cons)
(et-define-predicate null Nil)
(et-define-predicate not Nil)

(et-test
 (et-assert-call True&{$a::Cons}
                 consp Cons&{::$a})

 (et-assert-call (or True&{$a::String} Nil&{$a::Number})
                 stringp {::$a}&{String|Number})

 ;; This tests whether et--supersect works correctly when it cannot determine a definite subtype.
 ;; There is no defined intersection of Positive and Integer, so it must make an approximation.
 ;; Approximating to never would incorrectly determine that this call will always determine nil.
 ;; So, in order to ensure correctness, it must assume a SUPERSET of the real intersection (by picking either Positive or Integer.)
 (not (et-subtype? (et-typecheck-call integerp Positive&{::$a}) (et Nil))))


;;;; Funcall

(et-define-checker apply
  (let* ((func-type (et-checker-sub 1))
         (args-type (et--tailed-tuple 'ConsR (et-checker-remaining 2)))
         (result (et--funcall func-type args-type)))
    (or result
        (et-checker-err "Cannot apply %s with args %s" (et-pp func-type) (et-pp args-type)))))

(et-define-checker funcall
  (let* ((func-type (et-checker-sub 1))
         (args-type (et--tuple 'ConsR (et-checker-remaining 2)))
         (output-type (et--funcall func-type args-type)))
    (or output-type
        (et-checker-err "Cannot call %s with args %s" (et-pp func-type) (et-pp args-type)))))

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


;;;; Equality

(et-define-type-checker (eq eql equal) [A B]
  (Args A B)
  (or Nil (and True (bindsof (and A B)))))

(et-define-type-checker = [(<= A Number) (<= B Number)]
  (Args A B)
  (or Nil (and True (bindsof (and A B)))))

(et-define-type-checker (< <= > >=) (Args Number Number) Boolean)


;;;; Plist

(defun et--plist-lookup (plist-type key-type error-fn)
  "Look up KEY-TYPE in PLIST-TYPE, returning the value type or nil.

PLIST-TYPE and KEY-TYPE should already be expanded.  KEY-TYPE must be a
literal or union of literals.  PLIST-TYPE must be a single PList case.
ERROR-FN is called with a format string and args on failure, and the
function returns nil."
  (when-let*
      ((plist-type (et-expand-all-aliases (et--remove-type-binds plist-type)))
       (key-type (et-expand-all-aliases (et--remove-type-binds key-type)))
       (keys
        (cl-loop for case in (et-type-cases key-type)
                 for val = (et-type-case-value case)
                 if (and (et-datatype-p val)
                         (eq (et-datatype-name val) 'Literal))
                 collect (car (et-datatype-args val))
                 else do (funcall error-fn "Key must be a literal, found %s" (et-pp key-type))
                 and return nil))
       (plist-args
        (pcase (et-type-cases plist-type)
          (`(,(cl-struct et-type-case
                         (value (cl-struct et-datatype (name 'PList) (args args)))))
           args)
          (_ (funcall error-fn "Expected a PList type, found %s" (et-pp plist-type))
             nil)))
       (val-types
        (cl-loop for k in keys
                 for v = (plist-get plist-args k)
                 if v collect v
                 else do (funcall error-fn "Key %s not found in %s"
                                  (et-pp (et-literal k)) (et-pp plist-type))
                 and return nil)))
    (apply #'et--or val-types)))


(et-define-pcase-checker plist-get `(,_plist ,_key)
  (let* ((plist-type (et-checker-sub 1))
         (key-type (et-checker-sub 2)))
    (et--plist-lookup plist-type key-type
                      (lambda (fmt &rest args) (apply #'et-checker-err 0 fmt args)))))


(et-define-pcase-checker plist-put `(,_plist ,_key ,_val)
  (let* ((plist-type (et-checker-sub 1))
         (key-type (et-checker-sub 2))
         (val-type (et-checker-sub 3))
         (existing-val-type
          (et--plist-lookup plist-type key-type
                            (lambda (fmt &rest args) (apply #'et-checker-err 0 fmt args)))))
    (when existing-val-type
      (if (et-subtype? val-type existing-val-type)
          plist-type
        (et-checker-err 3 "Expected %s, found %s" (et-pp existing-val-type) (et-pp val-type))))))


;;;; cl-loop

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

(defun et--loop-accumulation-type (kw form-type)
  "Compute the return type for accumulation keyword KW given FORM-TYPE.

Freshness reasoning:
  collect/append  -> ListFresh: cl-loop builds a brand-new list spine;
                     the caller owns the result and may write to it.
  nconc           -> List: cl-loop destructively splices the lists the
                     body returned, so the result shares their conses.
                     NOT fresh.
  vconcat         -> VectorFresh: cl-loop allocates a new vector.
  concat          -> String: strings are immutable in elisp.
  count           -> Integer: a counter.
  sum             -> Number if form is Number, Integer if form is Integer.
  maximize/minimize -> the form's own numeric type."
  (pcase kw
    ((or 'collect 'collecting)
     ;; Fresh list: the caller gets a new spine they fully own
     (et-alias 'ListFresh form-type))
    ((or 'append 'appending)
     ;; Fresh list: cl-loop copies each sublist into a fresh result
     (let ((elem (et--loop-infer-elem-type form-type)))
       (et-alias 'ListFresh elem)))
    ((or 'nconc 'nconcing)
     ;; NOT fresh: cl-loop nconcs the body's own conses together
     (let ((elem (et--loop-infer-elem-type form-type)))
       (et-alias 'List elem)))
    ('concat (et String))
    ('vconcat
     ;; Fresh vector: cl-loop builds a new vector from the results
     (et-dt 'VectorFresh form-type))
    ((or 'count 'counting) (et Integer))
    ((or 'sum 'summing)
     (if (et-subtype? form-type (et Integer)) (et Integer) (et Number)))
    ((or 'maximize 'maximizing 'minimize 'minimizing)
     form-type)))


(et-define-checker cl-loop
  (let* ((clauses (cdr et--checker-expr))
         (len (length clauses))
         (pos 0)
         ;; Shadow et--binds so we can push loop vars incrementally
         ;; without affecting the outer scope
         (et--binds et--binds)
         ;; Bare accumulation types (not `into VAR')
         (bare-accum-types nil)
         ;; `finally return' overrides all other return types
         (finally-return-type nil)
         ;; `return EXPR' inside body contributes to return type
         (body-return-types nil)
         ;; `always'/`never' -> Boolean
         (seen-always-never nil)
         ;; `thereis' -> form-type | Nil
         (thereis-types nil)
         (kw nil))

    (cl-flet*
        ((peek () (and (< pos len) (nth pos clauses)))
         (advance () (cl-incf pos))
         ;; Typecheck the expression at the current clause position
         ;; (1+ pos because index 0 of et--checker-expr is `cl-loop' itself)
         (check-expr ()
           (prog1 (et-checker-sub (1+ pos))
             (cl-incf pos)))
         ;; Register a loop variable visible to subsequent clauses
         (bind-var (name type)
           (push (cons name (et-new-var name (et--unfreshen-type type)))
                 et--binds))
         ;; If the token at pos is in KEYWORDS, consume it and return t
         (eat-keyword (&rest keywords)
           (when (and (< pos len) (memq (peek) keywords))
             (advance) t))
         ;; Consume a single body form (for `do', `initially', etc.)
         ;; Returns its type.
         (check-body-form () (check-expr))
         ;; Consume body forms until the next loop keyword or end
         (check-body-forms ()
           (let ((last-type (et Nil)))
             (while (and (< pos len) (not (et--loop-keyword-p (peek))))
               (setq last-type (check-expr)))
             last-type)))

      ;; Walk clauses sequentially
      (while (< pos len)
        (setq kw (peek))
        (advance)
        (pcase kw
          ;; ======== for / as ========
          ((or 'for 'as)
           (let* ((var-name (peek))
                  var-type)
             (advance) ; consume VAR name

             ;; If VAR is a destructuring pattern (a list), we can't
             ;; track individual bindings — skip the name and bind nothing
             (when (consp var-name) (setq var-name nil))

             (pcase (peek)
               ;; -- for VAR from/upfrom/downfrom EXPR [to/... EXPR] [by EXPR] --
               ((or 'from 'upfrom 'downfrom)
                (advance)
                (let ((bounds (list (check-expr))))
                  (when (eat-keyword 'to 'upto 'downto 'above 'below)
                    (push (check-expr) bounds))
                  (when (eat-keyword 'by)
                    (push (check-expr) bounds))
                  (setq var-type (et--loop-numeric-var-type bounds))))

               ;; -- for VAR to/upto/downto/above/below EXPR [by EXPR] --
               ;; (implicit start of 0)
               ((or 'to 'upto 'downto 'above 'below)
                (advance)
                (let ((bounds (list (et Integer) (check-expr))))
                  (when (eat-keyword 'by)
                    (push (check-expr) bounds))
                  (setq var-type (et--loop-numeric-var-type bounds))))

               ;; -- for VAR = EXPR1 [then EXPR2] --
               ('=
                (advance)
                (let ((init-type (check-expr)))
                  (setq var-type
                        (if (eat-keyword 'then)
                            (et--or init-type (check-expr))
                          init-type))))

               ;; -- for VAR in/in-ref LIST [by FUNC] --
               ((or 'in 'in-ref)
                (advance)
                (let ((list-type (check-expr)))
                  (when (eat-keyword 'by) (check-expr))
                  (setq var-type (et--loop-infer-elem-type list-type))))

               ;; -- for VAR on LIST [by FUNC] --
               ('on
                (advance)
                (let ((list-type (check-expr)))
                  (when (eat-keyword 'by) (check-expr))
                  ;; VAR is bound to successive tails of the list
                  (setq var-type list-type)))

               ;; -- for VAR across/across-ref ARRAY --
               ((or 'across 'across-ref)
                (advance)
                (setq var-type (et--loop-infer-vector-elem-type (check-expr))))

               ;; -- for VAR being ... --
               ('being
                (advance)
                ;; consume optional `the' / `each'
                (eat-keyword 'the 'each)
                (pcase (peek)
                  ;; elements of SEQUENCE [using (index VAR2)]
                  ((or 'elements 'element)
                   (advance)
                   (eat-keyword 'of 'of-ref)
                   (let ((seq-type (check-expr)))
                     ;; Could be list or vector; try both
                     (setq var-type
                           (et--or (et--loop-infer-elem-type seq-type)
                                   (et--loop-infer-vector-elem-type seq-type)))
                     (when (eat-keyword 'using)
                       ;; (index VAR2) — VAR2 is Integer
                       (let ((spec (peek)))
                         (advance)
                         (when (and (consp spec) (eq (car spec) 'index))
                           (bind-var (cadr spec) (et Integer)))))))

                  ;; hash-keys / hash-values of HT [using ...]
                  ((or 'hash-keys 'hash-key 'hash-values 'hash-value)
                   (advance)
                   (eat-keyword 'of)
                   (check-expr) ; typecheck the hash-table expr
                   (setq var-type (et-any))
                   (when (eat-keyword 'using)
                     ;; (hash-values VAR2) or (hash-keys VAR2)
                     (let ((spec (peek)))
                       (advance)
                       (when (consp spec)
                         (bind-var (cadr spec) (et-any))))))

                  ;; symbols [of OBARRAY]
                  ((or 'symbols 'symbol)
                   (advance)
                   (when (eat-keyword 'of) (check-expr))
                   (setq var-type (et Symbol)))

                  ;; key-codes / key-bindings / key-seqs of KEYMAP
                  ((or 'key-codes 'key-bindings 'key-seqs)
                   (advance)
                   (eat-keyword 'of)
                   (check-expr)
                   (setq var-type (et-any))
                   (when (eat-keyword 'using)
                     (let ((spec (peek)))
                       (advance)
                       (when (consp spec)
                         (bind-var (cadr spec) (et-any))))))

                  ;; overlays / intervals [of BUFFER] [from POS] [to POS]
                  ((or 'overlays 'intervals)
                   (advance)
                   (when (eat-keyword 'of) (check-expr))
                   (when (eat-keyword 'from) (check-expr))
                   (when (eat-keyword 'to) (check-expr))
                   (setq var-type (et-any)))

                  ;; frames / buffers / windows
                  ((or 'frames 'buffers)
                   (advance)
                   (setq var-type (et-any)))
                  ('windows
                   (advance)
                   (when (eat-keyword 'of) (check-expr))
                   (setq var-type (et-any)))

                  ;; Unknown being clause — skip expr, default to Any
                  (_ (setq var-type (et-any)))))

               ;; Bare `for VAR' with no recognized iteration keyword
               (_ (setq var-type (et-any))))

             ;; Bind the variable for subsequent clauses
             (when var-name
               (bind-var var-name (or var-type (et-any))))))


          ;; ======== with VAR [= EXPR] ========
          ('with
           (let ((var-name (peek)))
             (advance) ; consume VAR name
             (if (eat-keyword '=)
                 (bind-var var-name (check-expr))
               (bind-var var-name (et Nil)))))


          ;; ======== do / doing ========
          ((or 'do 'doing)
           (check-body-forms))


          ;; ======== initially ========
          ('initially
           (eat-keyword 'do)
           (check-body-forms))


          ;; ======== finally [do] EXPRS... | finally return EXPR ========
          ('finally
           (if (eat-keyword 'return)
               (setq finally-return-type (check-expr))
             (eat-keyword 'do)
             (check-body-forms)))


          ;; ======== repeat INTEGER ========
          ('repeat (check-expr))


          ;; ======== while / until ========
          ((or 'while 'until) (check-expr))


          ;; ======== always / never ========
          ((or 'always 'never)
           (check-expr)
           (setq seen-always-never t))


          ;; ======== thereis ========
          ('thereis
           (push (check-expr) thereis-types))


          ;; ======== return EXPR ========
          ('return
           (push (check-expr) body-return-types))


          ;; ======== collect/append/nconc/concat/vconcat/count/sum/max/min ========
          ((and (or 'collect 'collecting 'append 'appending
                    'nconc 'nconcing 'concat 'vconcat
                    'count 'counting 'sum 'summing
                    'maximize 'maximizing 'minimize 'minimizing)
                accum-kw)
           (let* ((form-type (check-expr))
                  (accum-type (et--loop-accumulation-type accum-kw form-type)))
             (if (eat-keyword 'into)
                 ;; `into VAR' — does NOT contribute to bare return type;
                 ;; instead creates/updates a named accumulator variable
                 (let ((into-name (peek)))
                   (advance)
                   ;; Bind the accumulator var if not already bound.
                   ;; If already bound, its type is widened by union,
                   ;; but for simplicity we just bind once with the
                   ;; first accumulation type.  The user can reference
                   ;; it in `finally return'.
                   (unless (alist-get into-name et--binds)
                     (bind-var into-name accum-type)))
               ;; Bare accumulation contributes to the return type
               (push accum-type bare-accum-types))))


          ;; ======== if / when / unless COND CLAUSE [and CLAUSE]... [else ...] ========
          ((or 'if 'when 'unless)
           (check-expr) ; typecheck the condition
           ;; Parse the body clause(s) — they are single accumulation/do/return clauses
           ;; linked by `and', with optional `else' branch, terminated by `end'
           (cl-flet
               ((parse-inner-clause ()
                  ;; An inner clause is a single keyword + its argument(s)
                  (when (< pos len)
                    (let ((inner-kw (peek)))
                      (pcase inner-kw
                        ((or 'collect 'collecting 'append 'appending
                             'nconc 'nconcing 'concat 'vconcat
                             'count 'counting 'sum 'summing
                             'maximize 'maximizing 'minimize 'minimizing)
                         (advance)
                         (let* ((form-type (check-expr))
                                (accum-type (et--loop-accumulation-type inner-kw form-type)))
                           (if (eat-keyword 'into)
                               (let ((into-name (peek)))
                                 (advance)
                                 (unless (alist-get into-name et--binds)
                                   (bind-var into-name accum-type)))
                             (push accum-type bare-accum-types))))

                        ('return
                         (advance)
                         (push (check-expr) body-return-types))

                        ((or 'do 'doing)
                         (advance)
                         (check-body-forms))

                        ;; Nested if/when/unless
                        ((or 'if 'when 'unless)
                         (advance)
                         (check-expr)
                         ;; Recurse — but for simplicity just consume
                         ;; the inner clause
                         nil)

                        ;; Unknown — skip it
                        (_ nil))))))

             (parse-inner-clause)
             ;; Consume `and CLAUSE'... chains
             (while (eat-keyword 'and)
               (parse-inner-clause))
             ;; Consume optional `else CLAUSE [and CLAUSE]...'
             (when (eat-keyword 'else)
               (parse-inner-clause)
               (while (eat-keyword 'and)
                 (parse-inner-clause)))
             ;; Consume optional `end'
             (eat-keyword 'end)))


          ;; ======== named NAME ========
          ('named (advance))


          ;; ======== Unknown keyword — skip (likely a bare form in a `do' context) ========
          ;; cl-loop allows bare forms as implicit `do' in some contexts
          (_ nil))))

    ;; --------------------------------------------------------
    ;; Compute the final return type
    ;; --------------------------------------------------------
    (cond
     ;; `finally return' overrides everything
     (finally-return-type finally-return-type)

     ;; `always'/`never' returns Boolean
     (seen-always-never (et Boolean))

     ;; `thereis' returns form-type | Nil
     (thereis-types
      (apply #'et--or (et Nil) thereis-types))

     ;; Bare accumulations and/or body `return' statements
     ((or bare-accum-types body-return-types)
      (apply #'et--or (append bare-accum-types body-return-types)))

     ;; No accumulation, no return — cl-loop returns nil
     (t (et Nil)))))


;;;; Some string/symbol functions

(et-define-type-checker gensym [] Nil|Args<String> NonNilSymbol)
(et-define-type-checker intern [] (Args String) Symbol)
(et-define-type-checker symbol-name [] (Args Symbol) String)
(et-define-type-checker format [] (ArgsWithTail String ListR<Any>) String)
(et-define-type-checker error [] (ArgsWithTail String ListR<Any>) Never)


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


;;; ============================================================
;;; et macros

(et-define-pcase-checker et? `(,type-spec ,_expr)
  (let* ((actual (et-checker-sub 1))
         (declared (et-parse-type type-spec)))
    (unless (et-subtype? actual declared)
      (et-checker-err 0 "Expected %s, found %s" (et-pp declared) (et-pp actual)))
    actual))

(et-define-pcase-checker et! `(,type-spec ,_expr)
  (let* ((actual (et-checker-sub 1))
         (declared (et-parse-type type-spec)))
    (when (et-never-p (et--supersect actual declared))
      (et-checker-err 0 "Types %s and %s have no overlap. Use et!! to supress this warning."
                      (et-pp declared) (et-pp actual)))
    actual))

(et-define-pcase-checker et!! `(,type-spec ,_expr)
  (let* ((actual (et-checker-sub 1))
         (declared (et-parse-type type-spec)))
    actual))


;;; ============================================================
;;; Provide

(provide 'et-types)


;;; et-types.el ends here
