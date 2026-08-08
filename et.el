;;; et.el --- Typesystem for emacs lisp -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Adam Tillou

;; Author: Adam Tillou <adam.tillou@gmail.com>
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

(require 'cl-lib)
(require 'et-macros)
(require 'seq)


(defvar et-debug nil
  "Perform extra debug checks.")

(defmacro et (&rest args)
  `(et-parse-type (et-q ,(if (eq (length args) 1) (car args) args))))


;;; ============================================================
;;; Type declarations

(et-declare
 (@alias EtRel &Tree<Integer>) ; An (unflattened) relative path
 (@alias EtPath List<Integer>)
 (@alias EtSeverity (or @error @warning @hint))
 (@alias EtDiagnostic (Tuple EtPath EtSeverity String)))

(et-declare
 (@alias EtLabel (Plist :field Symbol :position Number|Nil))
 (@alias EtType *et:type)
 (@alias EtVar *et:type-var)
 (@alias EtBinds Alist<EtVar~EtType>))

(et-declare
 (@alias EtDatatypeRole (or @CONST @CO @CONTRA @ISO))
 (@alias EtDatatypeProps
         (Plist :args (or List<EtDatatypeRole> (Function (Args &List) List<EtDatatypeRole>))
                :overlap True|List<Symbol>
                :predicate (Function Any True|List)))
 (@alias EtDatatypeName (or @Any @Literal @NonNil
                            @Symbol @NonNilSymbol @Var @Number @Integer @Positive @Negative @String
                            @ConsFull @ConsFresh @VectorFull @VectorFresh @Plist
                            @Function @DynFunction
                            @Struct))
 (@alias EtDatatypeArgs &List)

 (@alias EtPolymorphConstraint
         (Tuple (or @P:LEQ @P:NLEQ @P:GEQ @P:NGEQ) EtType))
 (@alias EtPolymorphicTypes Alist<EtGeneric~&List<EtPolymorphConstraint>>)

 (@alias EtGeneric Var)
 (@alias EtGenVec (&Vector (or EtGeneric (&Tuple (or @<= @>=) EtGeneric Any))))
 ;; Alias GenVecs also have an (=) option (for default values)
 (@alias EtAliasGenVec (&Vector (or EtGeneric (&Tuple (or @= @<= @>=) EtGeneric Any))))

 (@alias EtAliasName Var)
 (@alias EtAliasDefinitionPlist
         (Plist :generics List<EtGeneric>
                :defaults Alist<EtGeneric~EtRepr>
                :constraints List<EtTypeConstraint>
                :repr (or Nil EtRepr)
                :type (or Nil EtType)
                :spec EtSpec
                :default-specs Alist<EtGeneric~EtSpec>
                ;; non-nil NAME => this alias is the ro version of NAME
                :ro-from Nil|EtAliasName
                ;; non-nil NAME => this alias is NOT ro, and its read-only equivalent is NAME
                :ro-name Nil|EtAliasName
                ;; t => this is a ro alias, but is no different from its non-ro equivalent
                :ro-redundant Boolean))
 (@symbol-property et-alias EtAliasDefinitionPlist))

(et-declare
 (@alias EtConstrainResult *et:match-result<List<EtMatchConstraint>>)
 (@alias EtMatchResult *et:match-result<List<EtType>>)

 (@alias EtMatchStack (List (Tuple @SUB|@SUPER EtRepr EtType)))
 (@alias EtTypeConstraint
         (Tuple (or @Q:GEQ @Q:LEQ) EtGeneric EtType))
 (@alias EtNoinferConstraint
         (Tuple (or @R:LEQ @R:GEQ) EtRepr EtType))
 (@alias EtMatchFunctionConstraint
         (Tuple @R:FN
                ;; The DynFunction matcher and output (sub)
                *et:match-matcher EtRepr
                ;; The Function input and output (super)
                EtRepr EtRepr))
 (@alias EtMatchConstraint
         (or EtTypeConstraint EtNoinferConstraint
             EtMatchFunctionConstraint))

 (@alias EtReprFactor
         (or (TupleStar @S:DT EtDatatypeName List)
             (TupleStar @S:ALIAS EtAliasName List<EtType>)
             (Tuple @S:GENERIC EtGeneric)
             (Tuple @S:POLY EtGeneric)
             (Tuple @S:NOINFER EtRepr Alist<EtGeneric~EtRepr>)
             (TupleStar @S:OP Var List)
             (Tuple @S:SET EtRepr EtType)))

 (@alias EtReprCase (&List EtReprFactor))
 (@alias EtReprDnf (&List EtReprCase))

 (@alias EtRepr *et:repr)

 ;; Could have a stricter type. Maybe later.
 (@alias EtSpec Any))

(et-declare
 (@alias EtIndeterminate (Tuple (or @I:LEQ @I:GEQ EtGeneric EtType)))
 (@alias EtSubtypeResult *et:match-result<List<EtIndeterminate>>))


;;; ============================================================
;;; Customization

(defgroup et nil
  "Customization group for et."
  :prefix "et-"
  :group 'tools)

(defcustom et-print-labels nil
  "Show type labels when printing types."
  :type 'boolean)

(defcustom et-print-narrows nil
  "Show implied narrows when printing types."
  :type 'boolean)


;;; ============================================================
;;; Results - `et:result'
;;;; Explanation

;; When type-checking an expression, it is important to know where
;; each error occurred in the expression. Since emacs lisp code is
;; made up of nested lists, we can express positions of the code as
;; paths to the correct expression, where each element of the path is
;; the index in the next expression.
;;
;; An `et:result' struct represents the result of performing some
;; action on an expression. It contains the output value, whether it
;; succeeded or failed, and a list of diagnostics which occurred, and
;; where they occurred in the expression.
;;
;; To collect an et:result, wrap the corresponding code in an
;; `et-result-boundary'. This will declare a collection of dynamically
;; scoped variables for collecting information about checking. If an
;; error is thrown, the result boundary will catch an error, and
;; observe the current value of `et:result--path' to see where the
;; error occurred.
;;
;; The `et-at' macro should be used whenever processing a
;; sub-expression of the current expression. The `et-error-boundary'
;; function should also be used to continue from a certain location in
;; the event of an error.


;;;; Dynamic variables

;; Each result boundary will dynamically bind these variables. At the
;; end of the result boundary, they will be collected to create the
;; result.

(et-defvar et:result--active? Boolean nil)

(et-defvar et:result--path List<Integer> nil
  "The path to the current expression being processed.")

(et-defvar et:result--path-offset Integer 0
  "An offset for future appends to `et:result--path'.

This is relevant if the current expression being processed is actually
the cdr of a larger expression. For example, when type checking a
function body, the body of the function is the `cddr' (offset=2) of a
lambda expression, or the `cdddr' (offset=3) of a defun.")

(et-defvar et:result--sticky-path Boolean nil
  "Whether to inhibit modifications to `et:result--path'.

`et:result--path' should only be changed when the expression being evaluated
corresponds to an expression present in the buffer. When calling a
function which thinks it is operating on a buffer expression, with an
expression that is not actually in the buffer, ensure that
`et:result--sticky-path' is non-nil to avoid creating an invalid path.")

(et-defvar et:result--diagnostics List<EtDiagnostic> nil
  "Diagnostics collected for the current result.")

(et-defvar et:result--diagnostic-prefixes List<String> nil
  "Prefixes to show for all diagnostics, in reverse order.")

(et-defvar et:result--failed Boolean nil)

(et-defun et-cur-result-failed? () Boolean et:result--failed)


;;;; Struct

(et-defstruct et:result
  (value nil :et-generics [T] :et T|Nil)
  (failed nil :et Boolean)
  (diagnostics nil :et List<EtDiagnostic>))

(cl-defmethod cl-print-object ((result et:result) stream)
  (cl-flet* ((count-str (count) (if (eq count 0) "" (format " (+%s)" count))))

    (if (et:result->failed result)
        (cl-loop with others = 0
                 for (_path severity msg) in (et:result->diagnostics result)
                 when (eq 'error severity) collect (format "[%s]" msg) into strs
                 else do (cl-callf 1+ others)
                 finally do (princ (format "#<FAIL: %s%s>" (string-join strs " ") (count-str others)) stream))

      (princ (format "#<SUCCESS: %s%s>" (cl-prin1-to-string (et:result->value result))
                     (count-str (length (et:result->diagnostics result))))
             stream))))


;;;; Paths

(defun et:result--resolve-path (rel)
  (et-declare (rel EtRel) (@return EtPath))

  (if et:result--sticky-path et:result--path
    (if-let* ((flat (flatten-tree (list rel))))
        (append et:result--path (list (+ et:result--path-offset (car flat))) (cdr flat))
      et:result--path)))

(defmacro et-at (rel &rest body)
  (declare (indent 1) (et ($body EtRel)))
  (let* ((orig-var (gensym 'orig)))
    ;; On error, we want the path to stay where it is, hence using setq instead of let
    `(let ((,orig-var et:result--path))
       (setq et:result--path (et:result--resolve-path ,rel))
       (prog1 (let ((et:result--path-offset 0)) ,@body)
         (setq et:result--path ,orig-var)))))

(defmacro et-at-offset (offset &rest body)
  (declare (indent 1) (et ($body Integer)))
  ;; On error, we want the path to stay where it is, hence using setq instead of let
  `(let ((et:result--path-offset (+ et:result--path-offset ,offset)))
     ,@body))


(defmacro et-with-sticky-path (&rest body)
  "Evaluate BODY with a sticky path at path REL."
  (et-declare ($body))
  `(let* ((et:result--sticky-path t)) ,@body))

(defmacro et-without-sticky-path (&rest body)
  "Evaluate BODY with a sticky path at path REL."
  (et-declare ($body))
  `(let* ((et:result--sticky-path nil)) ,@body))


;;;; Diagnostics

(defmacro et-with-diagnostic-prefix (prefix-obj &rest body)
  (declare (indent 1))
  `(let* ((prefix ,prefix-obj)
          (et:result--diagnostic-prefixes
           (if (null prefix) et:result--diagnostic-prefixes
             (cons (et-pp prefix) et:result--diagnostic-prefixes))))
     ,@body))

(defun et:result--diagnostic-message (fmt args)
  (et-declare (fmt String) (args &List) (@return String))
  (cl-loop for prefix in et:result--diagnostic-prefixes
           collect (format "[%s]" (et-pp prefix)) into prefixes
           finally return
           (let* ((msg (if args (apply #'format fmt (mapcar #'et-pp args)) (et-pp fmt))))
             (string-join (nreverse (cons msg prefixes)) " "))))

(defun et:result--make-diagnostic (rel severity fmt &rest args)
  (et-declare (rel EtRel) (severity EtSeverity) (fmt String) (args &List)
              (@return Nil))
  (unless et:result--active? (error "Not in a result boundary"))
  (push (list (et:result--resolve-path rel) severity
              (et:result--diagnostic-message fmt args))
        et:result--diagnostics)
  ;; Intentionally return nil
  nil)

(defmacro et:result--define-diagnostics-function (name severity &optional failed)
  (declare (et ($expand)))
  `(defun ,name (relative fmt &rest args)
     ,(format "Create a diagnostic with severity `%s'." severity)
     (apply #'et:result--make-diagnostic relative ',severity fmt args)
     ,@(when failed (list '(setq et:result--failed t)))
     nil))

(et:result--define-diagnostics-function et-err error t)
(et:result--define-diagnostics-function et-warn warning)
(et:result--define-diagnostics-function et-hint hint)

(defun et-fatal (rel fmt &rest args)
  (et-declare (rel EtRel) (fmt String) (args &List) (@return Nil))
  (et-at rel
    (error "%s" (et:result--diagnostic-message fmt args))))


;;;; Boundaries

(defmacro et-wrap-errors (format &rest body)
  "Add context to errors thrown in BODY."
  (declare (indent 1) (et ($body String)))
  `(condition-case-unless-debug err (progn . ,body)
     (error (error ,format (error-message-string err)))))

(defmacro et-error-boundary (relative &rest body)
  (declare (indent 1) (et ($body EtRel)))
  `(et-at ,relative
     (condition-case-unless-debug err (progn . ,body)
       (error (et-err nil (error-message-string err))))))

(defmacro et-result-boundary (&rest body)
  (declare (et ($expand)))

  `(let* ((et:result--active? t)
          (et:result--path nil)
          (et:result--path-offset 0)
          (et:result--sticky-path nil)
          (et:result--diagnostics nil)
          (et:result--failed nil)
          (et:result--diagnostic-prefixes nil))
     (et:result-new
      :value (et-error-boundary nil ,@body)
      :failed et:result--failed
      :diagnostics et:result--diagnostics)))

(et-defun et-propagate-result ([T] result: *et:result<T> &optional map-path: Nil|fn<EtPath~EtPath>) T
  (cl-assert et:result--active?)
  (cl-loop for (path severity msg) in (et:result->diagnostics result)
           for new-path = (if map-path (funcall map-path path) path)
           do (et:result--make-diagnostic new-path severity msg))
  (when (et:result->failed result) (setq et:result--failed t))
  (et:result->value result))

(defmacro et-failed-boundary (&rest body)
  "Evaluate BODY with `et:result--failed' temporarily bound to nil.

Sometimes, we care whether a particular function call failed. Checking
`et:result--failed' normally isn't sufficient, because it already might
be non-nil."
  (declare (et ($body)))
  `(let* ((value-and-failed
           (let* ((et:result--failed nil))
             (cons (progn ,@body) et:result--failed))))
     (setq et:result--failed (or et:result--failed (cdr value-and-failed)))
     (car value-and-failed)))


;;; ============================================================
;;; Utils - `et:util'
;;;; Modify struct

;; TODO: Create a custom checker
(defun et-copy-with (struct &rest changes)
  "Return a copy of STRUCT with properties CHANGES."
  (unless (cl-struct-p struct)
    (error "Not a struct: %s" struct))
  (cl-loop with type = (type-of struct)
           with copy = (funcall (intern (format "%s-copy" type)) struct)
           for (key val) on changes by #'cddr
           for slot = (intern (substring (symbol-name key) 1))
           do (setf (cl-struct-slot-value type slot copy) val)
           finally return copy))


;;;; Repeat

(defmacro et-repeat (var repls &rest body)
  (declare (indent 2) (et ($expand)))
  (cl-assert (vectorp repls))
  (cl-loop for repl across repls
           collect (cl-subst repl var body) into all
           finally return (cons #'ignore all)))


;;;; Quote macro

(eval-and-compile
  (defun et:util--copy-quotes (expr)
    (cond ((and (eq (car-safe expr) #'quote) (consp (cdr-safe expr)))
           (list #'copy-tree expr))
          ((consp expr) (cons (et:util--copy-quotes (car expr)) (et:util--copy-quotes (cdr expr))))
          (t expr))))


(defmacro et-q (expr)
  "Like `backquote', but return copies of all list literals.

This avoids the undefined behavior caused by mutating list literals by
applying `copy-tree' to all list ltierals before they are returned. This
could be improved in the future by replacing all list literals with
instances of `list' and `cons', but this is not currently a high
priority."
  (declare (et ($expand)))
  (et:util--copy-quotes (cdr (backquote-process expr))))

(defmacro et-ql (&rest exprs)
  (declare (et ($expand)))
  (et:util--copy-quotes (cdr (backquote-process exprs))))


;;;; Dnf intersection

(et-defun et-dnf-intersect ([T] &rest dnfs: &List<&List<&List<T>>>) List<List<T>>
  "Return the DNF of intersecting DNFS."

  (pcase dnfs
    ('() (list (list)))
    (`(,a) a)
    (`(,a ,b ,c . ,rest) (et-dnf-intersect a (apply #'et-dnf-intersect b c rest)))
    (`(,a ,b)
     (cl-loop for a-case in a
              nconc
              (cl-loop for b-case in b
                       collect (append a-case b-case))))))


;;;; Stop recursion

(et-defvar et:util--stop-recursion-unset-marker Symbol (gensym "unset@"))

(et-defun et-stop-recursion-unset? (obj: Any) Boolean
  (eq obj et:util--stop-recursion-unset-marker))

(defmacro et-stop-recursion (var elem default &rest body)
  "This allows defining recursive algorithms that loop.

A function implementing this kind of algorithm should define a stack
variable, which holds the current call stack. Each call to this macro
will add ELEM to the call stack. If ELEM already existed in the call
stack, then DEFAULT will be evaluated, stored, and returned. If ELEM is
ever encountered again, this stored value will be returned.

When execution returns to the original stack frame, the frame will have
access to the default value that was created, as the cdar of the stack
variable."
  (declare (indent 3) (et ($expand)))
  `(let ((elem ,elem))
     (if-let* ((entry (assoc elem ,var)))
         (if (et-stop-recursion-unset? (cdr entry))
             (setcdr entry ,default)
           (cdr entry))

       (let ((,var (cons (cons elem et:util--stop-recursion-unset-marker) ,var)))
         ,@body))))


;;;; Gen vec parsing

(et-defun et-genvec-generics (genvec: Nil|EtGenVec) List<EtGeneric>
  (when genvec
    (cl-loop for gen-spec across genvec
             for gen =
             (pcase gen-spec
               (`(,(or '= '<= '>=) ,(and gen (pred symbolp)) ,_type-spec) gen)
               ((pred symbolp) gen-spec)
               (_ (error "Invalid generic entry: %s" gen-spec)))
             unless (memq gen generics) collect gen into generics
             finally return generics)))

(et-defun et-genvec-constraints (genvec: Nil|EtGenVec) List<EtTypeConstraint>
  "Parse a generic vector to a list of generics and constraints."
  (when genvec
    (cl-loop for gen-spec across genvec
             nconc
             (pcase gen-spec
               (`(,(or (and '<= (let op 'Q:LEQ))
                       (and '>= (let op 'Q:GEQ)))
                  ,(and gen (pred symbolp)) ,type-spec)
                (list (list op gen (et-parse-type type-spec))))))))


;;;; Traverse tree

(et-defun et:util-traverse-tree ([T] tree: &Tree<T> path: &List<Integer>) T
  (if (null path) tree
    (when (>= (car path) (length tree))
      (error "Index out of bounds: %s %s" (car path) tree))
    (et:util-traverse-tree (nth (car path) tree) (cdr path))))


;;;; With advice

(defmacro et-with-advice (symbol how fn &rest body)
  (declare (indent 3) (et ($body Var Var AnyFn)))
  (let* ((symvar (gensym "sym")) (fnvar (gensym "fn")))
    `(let* ((,symvar ,symbol) (,fnvar ,fn))
       (advice-add ,symvar ,how ,fnvar)
       (unwind-protect (progn ,@body)
         (advice-remove ,symvar ,fnvar)))))


;;; ============================================================
;;; Types - `et:type'
;;;; Type structs

(et-defstruct et:type-var
  "A variable currently in scope."
  name type)

(cl-defmethod cl-print-object ((var et:type-var) stream)
  (princ (format "#var<%s>" (et:type-var->name var)) stream))

(et-defstruct et:type-dt
  "A datatype factor of an `et-type'."
  (name nil :et Var)
  (args nil :et List<EtType|Any>))

(et-defstruct et:type-alias
  "A type alias factor of an `et-type'."
  (name nil :et Var)
  (args nil :et List<EtType>))

(et-defstruct et:type-case
  "Struct representing a case of an `et-type'.

BINDS is a list of (`et-var' . `et-type').

TYPEOFS is a list of `et-var'.

VALUE is an instance of either `et-datatype' or `et-alias'."
  (value nil :et *et:type-dt|*et:type-alias)
  (binds nil :et EtBinds)
  (typeofs nil :et List<EtVar>)
  ;; List of polymorphic types
  (polymorphs nil :et List<EtGeneric>))

(et-defstruct et:type
  "Struct representing a root-level et type.

  CASES is a list of `et-type-case' instances being unioned."
  (cases nil :et List<*et:type-case>)
  (label nil :et Nil|EtLabel))

(et-defun et:type--validate ([] type: EtType) EtType
  "Check that a type is valid."
  (unless (et:type-p type)
    (error "Not a type: %s" type))

  (when et-debug
    (dolist (case (et:type->cases type))
      (let* ((val (et:type-case->value case)))
        (cond
         ((et:type-dt-p val)
          ;; Check that all of the arguments have the correct role
          (et:dt-map-type-args (et:type-dt->name val) (et:type-dt->args val) #'et:type--validate))
         ((et:type-alias-p val) (mapc #'et:type--validate (et:type-alias->args val)))
         (t (error "Expected datatype or alias, found %s" val))))

      (dolist (x (et:type-case->binds case))
        (or (and (consp x) (et:type-var-p (car x)) (et:type--validate (cdr x)))
            (error "Expected bind, found %s" x)))
      (dolist (x (et:type-case->typeofs case))
        (or (et:type-var-p x) (error "Expected typeof var, found %s" x)))
      (dolist (x (et:type-case->polymorphs case))
        (or (symbolp x) (error "Expected polymorphic type, found %s" x)))))

  type)

(advice-add #'et:type-new :filter-return #'et:type--validate)


;;;; Polymorphic types
;;;;; Bookkeeping

(et-defvar et:type--polymorphs EtPolymorphicTypes nil
  "Polymorphic types in scope.")

(defun et:type--make-polymorphs (matcher)
  (declare (et (matcher *et:match-matcher) (@return EtPolymorphicTypes)))

  (cl-loop for name in (et:match-matcher->generics matcher)
           when (assq name et:type--polymorphs)
           do (error "Polymorphic type `%s' is already defined" name)
           for qs = (cl-loop for (op gen q-type) in (et:match-matcher->constraints matcher)
                             when (eq gen name)
                             collect (list (pcase op ('Q:LEQ 'P:LEQ) ('Q:GEQ 'P:GEQ))
                                           q-type))
           collect (cons name qs)))

(defmacro et:type--with-polymorphs (polys &rest body)
  (declare (indent 1) (et ($expand)))
  `(let* ((et:type--polymorphs (append ,polys et:type--polymorphs)))
     ,@body))

(et-defun et:type-is-polymorph? (name: Symbol) Boolean
  (not (not (assq name et:type--polymorphs))))

(et-defun et:type-polymorph-constraints (name: Symbol) List<EtTypeConstraint>
  (cdr (or (assq name et:type--polymorphs)
           (error "Polymorphic type `%s' not defined" name))))


;;;;; Expand overloads

(defun et:type--indeterminates-to-overloads (base-polys indeterminates)
  "Produce polymorphic environments for every resolution of INDETERMINATES.

Source: gpt-5.6.sol high."
  (et-declare
   (base-polys EtPolymorphicTypes)
   (indeterminates &List<EtIndeterminate>)
   (@return List<EtPolymorphicTypes>))

  (cl-labels ((add-constraint (polys generic constraint)
                (let* ((copy (copy-tree polys))
                       (entry (assq generic copy)))
                  (unless entry
                    (error "Indeterminate generic `%s' is not in scope" generic))
                  (push constraint (cdr entry))
                  copy)))
    (cl-loop
     with overloads = (list base-polys)
     for (i-op generic type)
     in (delete-dups (copy-sequence indeterminates))
     for p-ops =
     (pcase i-op
       ('I:LEQ '(P:LEQ P:NLEQ))
       ('I:GEQ '(P:GEQ P:NGEQ))
       (_ (error "Invalid indeterminate operator: %s" i-op)))
     do
     (setq overloads
           (cl-loop for polys in overloads
                    nconc
                    (cl-loop for p-op in p-ops
                             collect
                             (add-constraint
                              polys generic (list p-op type)))))
     finally return overloads)))


;;;;; In function body

(defmacro et-with-function-polymorphs (func-type overload &rest body)
  "Evaluate BODY inside the typing context of FUNC-TYPE.

This will bind all of the polymorphic types implied by FUNC-TYPE.

If OVERLOAD is non-nil, then evaluate BODY multiple times, once for each
of the possible overloads of FUNC-TYPE.

Returns a list of results of BODY: if OVERLOAD is nil, this list will
always have one element."
  (declare (indent 2))

  `(cl-loop
    with overloads = (et:type--destructure-function ,func-type ,overload)
    for (polys input-type expected-ret) in overloads
    for overload-idx upfrom 1
    collect
    (et-with-diagnostic-prefix (when (cdr overloads) (format "Overload %s" overload-idx))
      (et:type--with-polymorphs polys
        ,@body))))

(defun et:type--destructure-function (func-type &optional overloads)
  "Destructure FUNC-TYPE into its input and output types.

Converting the input of a DynFunction to a type requires concrete types
for each generic. This requires creating a polymorphic type for each
generic, which are returned along side the input and output types.

If OVERLOADS is non-nil, and FUNC-TYPE is a DynFunction, then
automatically detect different overload definitions, and return them
each as a separate entry. The only thing that differs between overloads
is the constraints on each polymorphic type, and whatever effect those
constraints have on the resolved return type.

For example, the function

[T] (Args T) => (or String (if-nil? T Never Nil))

would have overloads

(((T (P:LEQ Nil))) Args<T> String)
and
(((T (P:NLEQ Nil))) Args<T> String|Nil)

This is a common pattern for functions that have a `noerror' argument."
  (et-declare (func-type EtType)
              (overloads Boolean)
              (@return (List (Tuple EtPolymorphicTypes EtType EtType))))

  (pcase (et-type-single (et-expand-aliases func-type))
    ((cl-struct et:type-dt (name 'DynFunction) (args `(,i-matcher ,o-repr)))
     (let* ((make-gen-repls
             ;; Must be called within et:type--with-polymorphs
             (lambda ()
               (cl-loop for gen in (et:match-matcher->generics i-matcher)
                        collect (cons gen (et-parse-type gen)))))
            (base-polys (et:type--make-polymorphs i-matcher))
            (all-polys
             (et:type--with-polymorphs base-polys
               (if (not overloads) (list base-polys)
                 (et:type--indeterminates-to-overloads
                  ;; Calculate the indeterminates by parsing o-repr
                  base-polys (cdr (et-repr-to-type-and-indes o-repr (funcall make-gen-repls))))))))
       (cl-loop for polys in all-polys
                collect
                (et:type--with-polymorphs polys
                  (let* ((gen-repls (funcall make-gen-repls))
                         (in (et-repr-to-type (et:match-matcher->repr i-matcher) gen-repls))
                         (out (et-repr-to-type o-repr gen-repls)))
                    (list polys in out))))))

    ((cl-struct et:type-dt (name 'Function) (args `(,i-type ,o-type)))
     (list (list nil i-type o-type)))
    (_ (error "Invalid function type: %s" (et-pp func-type)))))


;;;; Alias internals

(et-defun et:type--alias-props (name: EtAliasName &optional noerror: [N])
          (or EtAliasProps (if-nil? N Never Nil))
  (or (get name 'et-alias) (unless noerror (error "Alias `%s' not defined" name))))

(et-defun et:type-alias-ro-name (name: EtAliasName) EtAliasName
  (if-let* ((props (et:type--alias-props name))
            (ro-name (plist-get props :ro-name))
            (ro-props (get ro-name 'et-alias))
            ((not (plist-get ro-props :ro-redundant))))
      ro-name
    name))

(et-defun et:type-alias-display-name (name: EtAliasName) EtAliasName
  (let* ((props (et:type--alias-props name)))
    (if (plist-get props :ro-redundant)
        (or (plist-get props :ro-from)
            (error "Alias marked redundant without :ro-from"))
      name)))

(et-defun et:type-identify-alias (name: EtAliasName genvec: EtGenVec spec: EtSpec props: &List) Nil
  (when (plist-get (et:type--alias-props name t) :read-only)
    (error "Alias %s is already defined, and is read-only" name))

  ;; Parse the default specs
  (let* ((def-specs nil)
         (raw-genvec (cl-loop for item across genvec
                              collect
                              (if (not (and (listp item) (eq '= (car item)))) item
                                (setf (alist-get (cadr item) def-specs) (caddr item))
                                (cadr item))
                              into genlist
                              finally return (vconcat genlist)))
         (ro-from (plist-get props :ro-from))
         (ro-name (unless ro-from (intern (format "&%s" name))))
         (plist (cl-list* :genvec raw-genvec
                          :generics (et-genvec-generics genvec)
                          :spec spec
                          :default-specs def-specs
                          :ro-name ro-name
                          props)))

    (put name 'et-alias plist)
    (when ro-name
      (et:type-identify-alias ro-name genvec spec
                              (cl-list* :ro-from name props)))

    nil))

(et-defun et:type-constrain-alias (name: EtAliasName) Nil
  (let* ((props (et:type--alias-props name)))
    (plist-put props :constraints (et-genvec-constraints (plist-get props :genvec)))

    (when-let* ((ro-name (plist-get props :ro-name)))
      (et:type-constrain-alias ro-name))

    nil))

(et-defun et:type-declare-alias (name: EtAliasName) Nil
  (let* ((props (et:type--alias-props name))
         (gens (plist-get props :generics))
         (def-specs (plist-get props :default-specs))
         (ro-name (plist-get props :ro-name))
         (base-repr (et-parse-repr (plist-get props :spec) gens)))

    (plist-put props :repr (if ro-name base-repr (et:repr-to-read-only base-repr)))
    (plist-put props :defaults
               (cl-loop for gen in gens
                        for idx upfrom 0
                        for def-spec = (alist-get gen def-specs)
                        when def-spec
                        collect (cons gen (et-parse-repr def-spec (take idx gens)))))

    (when ro-name
      (et:type-declare-alias ro-name)
      ;; Check if the ro version is redundant (the same)
      (let* ((ro-props (et:type--alias-props ro-name)))
        (when (equal base-repr (plist-get ro-props :repr))
          (plist-put ro-props :ro-redundant t)))))
  nil)

(et-defun et:type-defalias (name: EtAliasName genvec: EtGenVec spec: EtSpec props: &List) Nil
  "Identify, constrain, and declare an alias."
  (et:type-identify-alias name genvec spec props)
  (et:type-declare-alias name)
  (et:type-constrain-alias name))

(et-defun et:type-alias-arity (name: EtAliasName) Nil|Cons<Integer~Integer>
  (let* ((props (et:type--alias-props name t))
         (gens (plist-get props :generics))
         (defs (plist-get props :default-specs)))
    (when props
      (cons (- (length gens)
               (length (seq-take-while (lambda (gen) (alist-get gen defs))
                                       (reverse gens))))
            (length gens)))))

(defmacro et-defalias (name genvec spec &rest props)
  "Alias NAME types to return the specific type.

\(fn NAME GENERIC-VECTOR BODY...)"
  (declare (indent 2))
  `(et:type-defalias ',name ,genvec ',spec (list ,@props)))

(et-defun et:type-alias-call ([] name: EtAliasName
                              args: (&List (is-non-nil? B EtType EtRepr))
                              totype: [B]
                              &optional scope: &List<EtGeneric>)
          (is-non-nil? B EtType EtRepr)
  "Expand the alias with name NAME, passing arguments ARGS.

SCOPE is the generic scope of the caller, which the expansion is
produced into. It is only relevant when TOTYPE is nil."
  (let* ((plist (et:type--alias-props name))
         (generics (plist-get plist :generics))
         (repr (or (plist-get plist :repr)
                   (error "Alias %s defined incorrectly: Missing repr" name)))
         (defaults (plist-get plist :defaults))

         (gen-repls
          (if (> (length args) (length generics))
              (error "Alias %s expected %s arguments, but %s were provided"
                     name (length generics) (length args))

            (cl-loop for gen in generics
                     for idx upfrom 0
                     for val =
                     (or (nth idx args)
                         ;; Default reprs are defined with all previous gens as their gen scope
                         (when-let* ((repr (alist-get gen defaults)))
                           (if totype
                               (et-repr-to-type repr gen-repls)
                             (et:repr-substitute-generics repr gen-repls nil)))
                         (error "Argument %s not provided, and has no default value" gen))
                     collect (cons gen val) into gen-repls
                     finally return gen-repls))))


    ;; Replace S:GENERIC with the specified arg
    (if totype (et-repr-to-type repr gen-repls)
      (if (null generics) repr
        (et:repr-substitute-generics repr gen-repls scope)))))


;;;; Expanding aliases

(et-defun et:type-alias-expand (alias: *et:type-alias) EtType
  "Expand an alias to a type."
  (et:type-alias-call (et:type-alias->name alias) (et:type-alias->args alias) :totype))

(et-defun et-expand-aliases (type: EtType &optional repeat: Integer) EtType
  (if (eq 0 repeat) type
    (cl-loop with next-repeat = (when (numberp repeat) (1- repeat))
             for case in (et:type->cases type)
             for val = (et:type-case->value case)
             append (if (not (et:type-alias-p val)) (list case)
                      (et:type->cases
                       (et-expand-aliases (et:type-alias-expand val) next-repeat)))
             into new-cases
             finally return (et:type-new :cases new-cases))))


;;;; Helpers

(et-defun et-type (&rest cases: &List<*et:type-case|*et:type-dt|*et:type-alias>) EtType
  "Construct a new `et-type' out of CASES.

Each of CASES should be an instance of `et-type-case', or alternatively
a valid `et:type-case->value'."
  (cl-loop for c in cases
           collect (if (et:type-case-p c) c
                     ;; Checking is done inside of `et:type-new'
                     (et:type-case-new :value c))
           into cases
           finally return (et:type-new :cases cases)))

(et-defun et-type-single (type: EtType) *et:type-dt|*et:type-alias|Nil
  "Assume type is a single case, and extract the case value."
  (when (eq 1 (length (et:type->cases type)))
    (et:type-case->value (car (et:type->cases type)))))


;;;; Parse/print type

(et-defun et-parse-type (spec: Any) EtType
  "Parse SPEC as an `et-type'."
  (et-repr-to-type (et-parse-repr spec nil)))

(et-defun et-pp-type (type: EtType) String
  (or (ignore-errors (et-repr-to-string (et-type-to-repr type)))
      (format "%s" type)))

(cl-defmethod cl-print-object ((type et:type) stream)
  (princ (et-pp-type type) stream))

(et-defun et-pp (arg: Any) String
  (if (stringp arg) arg (cl-prin1-to-string arg)))


;;;; Utils

(defun et-never-p (type)
  (null (et:type->cases type)))

(defun et-dt (name &rest args)
  (cl-assert (et:dt-name? name))
  (et-type (et:type-dt-new :name name :args args)))

(defun et-alias (name &rest args)
  (cl-assert (symbolp name))
  (cl-assert (string-match-p "^[A-Z]" (symbol-name name)))
  (et-type (et:type-alias-new :name name :args args)))

(defun et-any () (et-dt 'Any))
(defun et-never () (et:type-new :cases nil))
(defun et-literal (val) (et-dt 'Literal val))

(defun et:type-tuple-spec (cons args)
  (if (null args) (et-ql Nil)
    (et-ql ,cons ,(car args) ,(et:type-tuple-spec cons (cdr args)))))

(defun et:type-tuple-star-spec (cons types)
  (pcase types
    (`(,last) last)
    (`(,next . ,rest)
     (et-ql ,cons ,next ,(et:type-tuple-star-spec cons rest)))
    (_ (error "No tail provided"))))


;;; ============================================================
;;; Datatypes - `et:dt'
;;;; Datatypes

(defvar et:dt--datatypes
  (et! Alist<EtDatatypeName~EtDatatypeProps>
    '((Any :args nil :overlap t :predicate (lambda (v) t))
      ;; Literal<VALUE> is a type matching only the value VALUE
      ;; A literal CANNOT be ephemeral (like a buffer), it must be printable and readable
      (Literal :args (CONST) :overlap nil :predicate (lambda (v me) (equal v me)))
      (NonNil :args nil :overlap t :predicate (lambda (v) v))
      (Symbol :args nil :overlap (Function DynFunction) :predicate symbolp)
      (NonNilSymbol :args nil :overlap (Function DynFunction) :predicate (lambda (v) (and v (symbolp v))))
      (Var :args nil :overlap (Function DynFunction) :predicate (lambda (v) (and v (symbolp v) (not (eq v t)))))
      (Number :args nil :overlap nil :predicate numberp)
      (Integer :args nil :overlap (Positive Negative) :predicate integerp)
      (Positive :args nil :overlap nil :predicate (lambda (v) (and (numberp v) (> v 0))))
      (Negative :args nil :overlap nil :predicate (lambda (v) (and (numberp v) (< v 0))))
      (String :args nil :overlap nil :predicate stringp)

      ;; ConsFull<CAR-READ CAR-WRITE CDR-READ CDR-WRITE> is a cons cell.
      ;; CAR-READ/CDR-READ are the output types of calling car/cdr on
      ;; the cons cell. CAR-WRITE/CDR-WRITE are the types that are valid
      ;; to write to the cons cell. The most general cons cell is thus
      ;; ConsFull<Any Never Any Never>.
      (ConsFull :args (CO CONTRA CO CONTRA) :overlap (ConsFresh Function DynFunction Plist) :intersect t
                :predicate (lambda (v l _1 r _2) (when (consp v) `((,(car v) . ,l) (,(cdr v) . ,r)))))
      ;; When you create a new cons cell with cons/list/quote/etc, you
      ;; get a ConsFresh. This can be thought of as an "undetermined"
      ;; cons cell: in that it knows what it contains, but it has not
      ;; yet decided what can be written to it. A ConsFresh can be
      ;; converted to a ConsFull as long as the read types of the
      ;; ConsFull are supertypes of the arg types of the ConsFresh.
      (ConsFresh :args (CO CO) :overlap (Function DynFunction Plist) :intersect t
                 :predicate (lambda (v l r) (when (consp v) `((,(car v) . ,l) (,(cdr v) . ,r)))))

      ;; VectorFull<ELEM-READ ELEM-WRITE>: same idea as ConsFull
      (VectorFull :args (CO CONTRA) :overlap (VectorFresh) :intersect t
                  :predicate (lambda (v e _) (when (vectorp v) (or (cl-loop for x across v collect (cons x e)) t))))
      ;; Same idea as ConsFresh
      (VectorFresh :args (CO) :overlap nil :intersect t
                   :predicate (lambda (v e) (when (vectorp v) (or (cl-loop for x across v collect (cons x e)) t))))

      ;; Function<ARGLIST-TYPE OUTPUT-TYPE> is a function with a fixed
      ;; input and output type.
      (Function :args (CONTRA CO) :overlap (DynFunction) :intersect t)
      ;; DynFunction<ARGLIST-MATCHER OUTPUT-REPR> is a
      ;; function whose output depends on the input. For a given
      ;; ARGLIST-TYPE, the output of the function is determined by
      ;; inferring ARGLIST-TYPE against ARGLIST-MATCHER, with
      ;; OUTPUT-REPR as the output.
      (DynFunction :args (CONST CONST) :overlap nil)

      ;; Plist<PROP1 VAL1 PROP2 VAL2 ...> is a covariant, unordered
      ;; plist.
      (Plist
       :args (lambda (args)
               (cl-loop for (_prop _val) on args by #'cddr
                        nconc (list 'CONST 'ISO)))
       :overlap nil
       :predicate et:dt--literal-is-plist
       :intersect et:dt--plist-intersect-args)

      ;; Struct<NAME~GENERCIC-PARAMS...>
      (Struct :args (lambda (args)
                      (if-let* ((name (car args))
                                (plist (get name 'et-struct))
                                (arg-count (length (plist-get plist :generics))))
                          (if (eq (length (cdr args)) arg-count)
                              (cons 'CONST (make-list (length (cdr args)) 'ISO))
                            (error "Struct %s takes %s arguments" name arg-count))
                        (error "Not a struct: %s" name)))
              :overlap nil :intersect nil)

      ;; Ephemeral, non-readable emacs datatypes (buffer, window, etc)
      (Emacs :args (CONST)
             :overlap nil
             ;; a literal can never be an emacs datatype, so predicate=nil
             :predicate nil)))
  "Datatypes.")


;;;; Datatype helpers

(et-defun et:dt-name? ([N] name: N) (is? N EtDatatypeName)
  "Check if NAME is a datatype name."
  (not (not (assq name et:dt--datatypes))))

(et-defun et:dt-arg-roles (dt-name: EtDatatypeName dt-args: &List) List<EtDatatypeRole>
  "Returns a list of `CONST' | `CO' | `CONTRA' | `ISO'.

The resulting list must be the exact length of DT-ARGS, and each element
corresponds to the role of each argument in `dt-args'. `CONST' indicates
an argument which is a literal Lisp value. `CO'/`CONTRA'/`ISO' indicate
that the argument is a type argument, and whether the type argument is
covariant, contravariant, or isovariant."
  (declare (et (dt-name Var) (dt-args &List)
               (@return List<EtDatatypeRole>)))

  (pcase (plist-get (or (alist-get dt-name et:dt--datatypes)
                        (error "Invalid datatype: %s %s" dt-name dt-args))
                    :args)
    ((and (pred functionp) func) (funcall func dt-args))
    (other (copy-tree other))))


;;;; Datatype mappers

(defun et:dt-map-args (dt-name dt-args func)
  "Apply FUNC to each argument, returning the resulting list.

FUNC is called with two arguments, ARG and ROLE, where role is one of
`CONST', `CO', `CONTRA', or `ISO'."
  (cl-loop for arg in dt-args
           for role in (et:dt-arg-roles dt-name dt-args)
           collect (funcall func arg role)))

(defun et:dt-map-type-args (dt-name dt-args func)
  "Like `et:dt-map-args', but the identify for CONST args.

FUNC is called with one argument, the current argument"
  (declare (et (@generics [T R])
               (dt-name Var) (dt-args &List<T|Any>)
               (func (Function (Args T) R))
               (@return List)))

  (cl-loop for arg in dt-args
           for role in (et:dt-arg-roles dt-name dt-args)
           if (eq role 'CONST) collect arg
           else collect (funcall func (et! T arg))))


;;;; Plist helpers
;;;;; Intersect args

(defun et:dt--plist-intersect-args (args1 args2 intersect _union)
  (let* ((all-props (cl-loop for (p) on (append args1 args2) by #'cddr collect p)))
    (cl-loop for prop in (delete-dups all-props)
             for val1 = (plist-get args1 prop)
             for val2 = (plist-get args2 prop)
             for intersection = (if val1 (if val2 (funcall intersect val1 val2) val1) val2)
             when (et-never-p intersection) return 'INVALID
             nconc (list prop intersection))))


;;;;; Literal is plist

(defun et:dt--literal-is-plist (v &rest plist-args)
  "Predicate for a `Literal' value V being a subtype of a Plist.

PLIST-ARGS are the Plist datatype's arguments (K1 V1 K2 V2 ...). This is
the `:predicate' of the Plist datatype, so it follows that contract: a
nil return fails the match, a t return succeeds it, and a list of
\(SUB-VAL . ARG) pairs requires each literal SUB-VAL to be a subtype of
ARG (see the `Literal' case of `et:dt-constraints').

V matches when it is a plist and, for each key, its value is a subtype
of that key's type. A key absent from V yields a nil value, so a key is
\"optional\" exactly when its type admits nil. Extra keys in V are
allowed, and order does not matter."
  (when (plistp v)
    (or (cl-loop for (prop val) on plist-args by #'cddr
                 collect (cons (plist-get v prop) val))
        t)))


;;;;; Type is plist

(defun et:dt--cons-is-plist (cons-args plist-args co mk-super)
  "Constraints for ConsFull to be a subtype of Plist.

A plist is a flat list (K1 V1 K2 V2 ...).  The ConsFull car is a key.
If it matches a required Plist key, the cdr must be a cons whose car
satisfies that key's value type and whose cdr covers the remaining
keys.  If it does not match, the cdr must be a cons (skipping the
value) whose cdr still covers all required keys.  Extra keys are
allowed and order does not matter.

MK-SUPER builds the synthesized `&Cons' super value in the caller's
language (a type or a matcher repr); see `et:dt-constraints'."
  (let ((car-read (et-expand-aliases (nth 0 cons-args)))
        (cdr-read (nth 2 cons-args)))
    (pcase (et:type->cases car-read)
      (`(,(cl-struct et:type-case
                     (value (cl-struct et:type-dt (name 'Literal) (args `(,prop))))))
       (let* ((pval (plist-get plist-args prop))
              (rest-plist (copy-tree plist-args)))
         (when pval (cl-remf rest-plist prop))
         (funcall co cdr-read (funcall mk-super pval rest-plist))))
      (_ (et:match-failed)))))

(defun et:dt-cons-plist-super-type (car rest-plist)
  "Build the type `&Cons<CAR~tail>', where tail is `Plist<REST-PLIST>' or Any.
CAR is the matched value type, or nil for any value.  Used as the
MK-SUPER argument of `et:dt--cons-is-plist' when matching against types."
  (et-alias '&Cons (or car (et-any))
            (if rest-plist (apply #'et-dt 'Plist rest-plist) (et-any))))

(defun et:dt-cons-plist-super-matcher (car rest-plist scope)
  "Build the matcher repr `&Cons<CAR~tail>', tail being `Plist<REST-PLIST>' or Any.
CAR is the matched value repr, or nil for any value.  SCOPE is the
generic scope of the enclosing matcher.  Used as the MK-SUPER argument
of `et:dt--cons-is-plist' when matching against matchers."
  (let ((any-mr (et:repr-new :generics scope :dnf (et-q (((S:DT Any)))))))
    (et:repr-new
     :generics scope
     :dnf (et-q (((S:ALIAS &Cons
                           ,(or car any-mr)
                           ,(if rest-plist
                                (et:repr-new :generics scope :dnf (et-q (((S:DT Plist ,@rest-plist)))))
                              any-mr))))))))


;;;; Overlapping

(et-defun et:dt-might-overlap-nontrivial? (a-dt: *et:type-dt b-dt: *et:type-dt) Boolean
  "Return whether datatypes A and B might overlap.

This function assumes that neither A nor B is a subtype of the other.
This is what is meant by \"nontrivial\"."
  (let* ((a (et:type-dt->name a-dt))
         (b (et:type-dt->name b-dt)))

    (when (< (cl-position b et:dt--datatypes :key #'car) (cl-position a et:dt--datatypes :key #'car))
      (cl-rotatef a b)
      (cl-rotatef a-dt b-dt))

    (pcase (plist-get (alist-get a et:dt--datatypes) :overlap)
      ('t t)
      ;; A cons can only be a function if its car is `lambda'
      ((guard (and (memq a '(ConsFull ConsFresh)) (memq b '(Function DynFunction))))
       (et-subtype? (et @lambda) (car (et:type-dt->args a-dt))))
      ;; Is an emacs internal datatype a function
      ((guard (and (memq a '(Function DynFunction)) (eq b 'Emacs)))
       (memq (car (et:type-dt->args b-dt)) '(interpreted-function byte-code-function subr)))

      (overlap (not (not (memq b overlap)))))))


;;;; Intersecting

(defun et:dt-intersect-args-nontrivial (name args1 args2 intersect union)
  "Return a list of arguments intersecting ARGS1 and ARGS2.

The goal of this function is to determine a list of arguments
INTERSECTION-ARGS such that (NAME INTERSECTION-ARGS) is a subtype of
both (NAME ARGS1) and (NAME ARGS2).

If no such list is found, then return the symbol `INVALID'.

This function is designed for `nontrivial' cases in that it assumes that
neither datatype is already a subset of the other, in which case the
subset args would be a trivial solution to this function. This is so
that this function can focus on the non-trivial cases where neither is a
subset of the other.

INTERSECT and UNION are functions which each take 2 elements from
ARGS1/ARGS2 and return a new arg, either the intersection or union of
the two args respectively."
  (et-declare (name EtDatatypeName)
              (args1 EtDatatypeArgs) (args2 EtDatatypeArgs)
              (intersect AnyFn) (union AnyFn)
              (@return List))

  (pcase (plist-get (alist-get name et:dt--datatypes) :intersect)
    ((and func (pred functionp)) (funcall func args1 args2 intersect union))
    ('t (cl-loop for role in (et:dt-arg-roles name args1)
                 for arg1 in args1
                 for arg2 in args2
                 for new-arg = (pcase role
                                 ('CO (funcall intersect arg1 arg2))
                                 ('CONTRA (funcall union arg1 arg2))
                                 ('ISO (if (equal arg1 arg2) arg1 'INVALID))
                                 (_ (error "Unexpected arg role: %s" role)))
                 when (or (eq new-arg 'INVALID) (et-never-p new-arg)) return 'INVALID
                 collect new-arg))
    (_ 'INVALID)))


;;;; Datatype matching

(defun et:dt-constraints (sub-name sub-args super-name super-args co contra iso co-literal mk-super dyn-fn)
  "Determine when one datatype to be a subtype of another.

Returns an EtConstrainResult required for (SUB-NAME SUB-ARGS) to be a
subtype of (SUPER-NAME SUPER-ARGS), using provided functions provided
for checking the sub-arguments.

Specifically, (funcall CO/CONTRA/ISO sub-arg super-arg) will return the
constraints necessary for sub-arg/super-arg to be a
subtype/supertype/equal (respectively) of super-arg. The reason these
must be provided as different functions is that the sub and super
datatypes may have different arg types. For example one might be a
matcher and another might be a type.

Also, (funcall CO-LITERAL val super-arg) checks if the literal val is a
subtype of super-arg.

The ConsFull/Plist case is the only one that synthesizes a brand new
super value (a `&Cons') instead of passing existing super-args to CO.
Since super-args may be either types or matcher reprs depending on the
caller, MK-SUPER builds that synthesized super value in the caller's
language. See `et:dt--cons-is-plist'.

DYN-FN (a function or nil) handles a `DynFunction' sub against a
`Function' super whose args are matcher reprs, which cannot be resolved
until the matcher's generics are known. It is called with SUB-ARGS and
SUPER-ARGS and should return an EtConstrainResult recording the deferred
check (see the `R:FN' constraint)."
  (cl-flet ((valid-if (valid)
              (if valid (et:match-result-new :success t)
                (et:match-failed))))

    (pcase (list sub-name super-name)
      ((guard (and (eq sub-name super-name)
                   (equal sub-args super-args)))
       (valid-if t))

      (`(,_ Any) (valid-if t))
      (`(Literal ,_)
       (let* ((pred (plist-get (alist-get super-name et:dt--datatypes) :predicate)))
         (pcase (apply (or pred #'ignore) (car sub-args) super-args)
           ('nil (valid-if nil))
           ('t (valid-if t))
           (sub (cl-loop for (sub-val . arg) in sub
                         collect (funcall co-literal sub-val arg) into results
                         finally return (apply #'et:match-result-and results))))))

      (`(Integer Number) (valid-if t))
      (`(Positive Number) (valid-if t))
      (`(Negative Number) (valid-if t))

      (`(ConsFresh ConsFull)
       (et:match-result-and (funcall co (car sub-args) (car super-args))
                            (funcall co (cadr sub-args) (caddr super-args))))
      (`(VectorFresh VectorFull) (funcall co (car sub-args) (car super-args)))

      (`(DynFunction Function)
       (let* ((func-input (car super-args))
              (func-output (cadr super-args)))
         (cond
          ;; We can use a simple funcall + subtype approach
          ((and (et:type-p func-input) (et:type-p func-output))
           (let* ((dyn-result (et:algebra-funcall (apply #'et-dt 'DynFunction sub-args) func-input)))
             (valid-if (and (et:match-result->success dyn-result)
                            (et-subtype? (et:match-result->value dyn-result) func-output)))))
          ;; The super args are matcher reprs; defer the check to
          ;; constraint satisfaction, once the generics are known.
          (dyn-fn (funcall dyn-fn sub-args super-args))
          ;; This is never actually reached in the current codebase
          (t (valid-if nil)))))

      (`(,_ NonNil) (valid-if (not (eq sub-name 'Symbol))))
      (`(,_ Symbol) (valid-if (memq sub-name '(NonNilSymbol Var))))
      (`(,_ NonNilSymbol) (valid-if (eq sub-name 'Var)))

      (`(ConsFull Plist)
       (et:dt--cons-is-plist sub-args super-args co mk-super))

      (`(Plist Plist)
       (cl-loop for (prop super-val) on super-args by #'cddr
                for sub-val = (plist-get sub-args prop)
                unless sub-val return (valid-if nil)
                collect (funcall co sub-val super-val) into results
                finally return (apply #'et:match-result-and results)))

      (`(Struct Struct)
       (apply
        #'et:match-result-and
        (valid-if (eq (car sub-args) (car super-args)))
        (valid-if (eq (length sub-args) (length super-args)))
        ;; For now, assume that all struct args are isovariant
        (cl-loop for sub in (cdr sub-args)
                 for super in (cdr super-args)
                 collect (funcall iso sub super))))

      ((guard (eq sub-name super-name))
       ;; Datatypes of the same type (except Plist and Struct) should have the same number of arguments
       (unless (eq (length sub-args) (length super-args))
         (et-fatal nil "Arg length mismatch: %s %s %s %s" sub-name sub-args super-name super-args))
       (cl-loop for sub-arg in sub-args
                for super-arg in super-args
                for role in (et:dt-arg-roles super-name super-args)
                ;; Skipping duplicates is especially helpful for ConsFull/VectorFull,
                ;; where arguments are often duplicated
                unless (member (cons sub-arg super-arg) already-checked)
                collect (cons sub-arg super-arg) into already-checked
                and collect
                (pcase role
                  ;; Const args must be equal to match
                  ('CONST (valid-if (equal sub-arg super-arg)))
                  ('CO (funcall co sub-arg super-arg))
                  ('CONTRA (funcall contra sub-arg super-arg))
                  ('ISO (funcall iso sub-arg super-arg))
                  (_ (error "Unknown argument role: %s" role)))
                into results
                finally return (apply #'et:match-result-and results)))

      (_ (valid-if nil)))))


;;; ============================================================
;;; Reprs - `et:repr'
;;;; Struct

(et-defstruct et:repr
  "A general representation for both types and matchers.

GENERICS is the generic scope this repr was constructed under. It must
never be read for any purpose other than assertions."
  (dnf nil :et EtReprDnf)
  (generics nil :et List<EtGeneric>)
  (label nil :et EtLabel))

;; A repr is a general format that can be converted to either a
;; matcher or a type. It is a list of cases, each of which is a list
;; of factors. A factor is one of the following:
;;
;; \(`S:DT' NAME ARGS...)
;; \(`S:ALIAS' NAME ARGS...)
;; \(`S:GENERIC' VAR) - In matchers, this compiles to a matcher generic. In
;;   types, you must provide replacements for each generic when parsing the
;;   repr to a type.
;; \(`S:NOINFER' REPR ENV) - Convert REPR to a type without using it for
;;   inference. REPR defines its own generic scope: ENV maps each of its
;;   generics to a repr in the surrounding scope.
;; \(`S:POLY' NAME) - A scoped polymorphic datatype
;; \(`S:OP' OP ARGS...) - Apply a custom operation to the arguments. In
;;   matchers, this is deferred by wrapping it in an `S:NOINFER'.
;; \(`S:SET' MATCHER TYPE) - Match a custom MATCHER against TYPE. As a
;;   type, this converts to `Any'.


;;;; Parsing

(et-defvar et:repr--parsing-generics List<EtGeneric> nil)

(et-defun et-parse-repr (spec: EtSpec generics: &List<EtGeneric> &optional label: EtLabel) EtRepr
  (let* ((et:repr--parsing-generics generics)
         (repr (et:repr--parse-0 spec)))
    (setf (et:repr->label repr) label)
    repr))

(et-defun et:repr--parse-0 (spec: EtSpec &optional extra-generics: &List<EtGeneric>) EtRepr
  (let* ((et:repr--parsing-generics (append extra-generics et:repr--parsing-generics)))
    (cond
     ((and (consp spec) (symbolp (car spec)))
      (et:repr--parse-factor (car spec) (cdr spec)))
     ((symbolp spec) (et:repr--parse-string (symbol-name spec)))
     ((stringp spec) (et:repr--parse-string spec))
     ((numberp spec) (et:repr--parse-0 (list 'Literal spec)))
     ((et:type-p spec) (et:repr--parse-0 (list 'type spec)))
     ((et:repr-p spec) spec)
     (t (error "Invalid spec: %s" spec)))))

(et-defun et:repr--parse-factor (name: Symbol args: &List<EtSpec>) EtRepr
  (or
   ;; and/or
   (when (memq name '(and or))
     (let* ((ps (mapcar #'et:repr--parse-0 args))
            (ds (mapcar #'et:repr->dnf ps)))
       (et:repr-new :generics et:repr--parsing-generics
                    :dnf (apply (if (eq name 'and) #'et-dnf-intersect #'nconc) ds))))

   ;; read-only
   (when (eq name 'read-only)
     (et:repr-to-read-only (et:repr--parse-0 (car args))))

   ;; Parse a built-in spec segment
   (when-let* ((handler (get name 'et-spec-parse)))
     (et:repr-new :generics et:repr--parsing-generics
                  :dnf (list (list (cons (car handler) (apply (cdr handler) args))))))

   ;; Parse a generic
   (when (memq name et:repr--parsing-generics)
     (et:repr-new :generics et:repr--parsing-generics
                  :dnf (list (list (list 'S:GENERIC name)))))

   ;; Parse a datatype
   (when (et:dt-name? name)
     (et:repr--parse-factor 'dt (cons name args)))

   ;; Parse a polymorphic type
   (when (et:type-is-polymorph? name)
     (et:repr-new :generics et:repr--parsing-generics
                  :dnf (list (list (list 'S:POLY name)))))

   ;; Parse a spec macro
   (when-let* ((macro (get name 'et-spec-macro)))
     (et:repr--parse-0 (apply macro args)))

   ;; Parse a custom op
   (when-let* ((op (get name 'et-op)))
     (et:repr--parse-factor 'op (cons name args)))

   ;; Parse an alias
   (when (et:type-alias-arity name)
     (et:repr--parse-factor 'alias (cons name args)))

   (error "Invalid type name: %s" name)))


;;;; Parse string

(et-defvar et:repr--test-variables Alist<Symbol~EtVar>
           (list (cons '$a (et:type-var-new :name '$a :type (et-dt 'Any)))
                 (cons '$b (et:type-var-new :name '$b :type (et-dt 'Any)))
                 (cons '$c (et:type-var-new :name '$c :type (et-dt 'Any)))))

(et-defun et:repr--parse-string (s: String) EtRepr
  (when (string-empty-p s) (error "Empty type expression"))

  (cl-loop for or-seg in (et:repr--split-at-depth s ?|)
           when (string-empty-p or-seg)
           do (error "Empty segment in union type: %s" s)
           collect
           (cl-loop for and-seg in (et:repr--split-at-depth or-seg ?^)
                    when (string-empty-p and-seg)
                    do (error "Empty segment in intersection type: %s" s)
                    collect (et:repr->dnf (et:repr--parse-atom and-seg)) into and-parts
                    finally return (apply #'et-dnf-intersect and-parts))
           into or-parts
           finally return
           (et:repr-new :generics et:repr--parsing-generics
                        :dnf (apply #'nconc or-parts))))

(et-defun et:repr--parse-atom (s: String) EtRepr
  "Parse a single type atom into an `et-type'."
  (cond
   ;; Literal number
   ((string-match "^[0-9]+\\(\\.[0-9]+\\)?$" s)
    (et:repr--parse-0 (list 'Literal (string-to-number s)) nil))

   ;; Parenthesized expression
   ((string-match "^{\\(.*\\)}$" s)
    (et:repr--parse-string (substring s 1 -1)))

   ;; @symbol  ->  Literal symbol
   ((string-match "^@\\(.*\\)$" s)
    (et:repr--parse-0 (list 'Literal (intern (match-string 1 s)))))

   ;; %string  ->  Literal string
   ((string-match "^%\\(.*\\)$" s)
    (et:repr--parse-0 (list 'Literal (match-string 1 s))))

   ;; $TestVar=Type  ->  Bind to TestVar
   ((string-match "^\\(\\$[a-z]\\)::\\(.*\\)$" s)
    (et:repr--parse-0
     (list 'bind (or (alist-get (intern (match-string 1 s)) et:repr--test-variables)
                     (error "Invalid test variable: %s" (match-string 1 s)))
           (match-string 2 s))))

   ;; ::$TestVar  ->  Typeof TestVar
   ((string-match "^::\\(\\$[a-z]\\)$" s)
    (et:repr--parse-0
     (list 'typeof (or (alist-get (intern (match-string 1 s)) et:repr--test-variables)
                       (error "Invalid test variable: %s" (match-string 1 s))))))

   ;; Var=Type  ->  Matcher set
   ((string-match "^\\([-a-zA-Z0-9]*\\)=\\(.*\\)$" s)
    (et:repr--parse-0 (list 'set (intern (match-string 1 s)) (match-string 2 s))))

   ;; Name or Name<...> or *struct or *struct<...>
   ((string-match "^\\*?\\([-&:a-zA-Z0-9]+\\)\\(?:<\\(.*\\)>\\)?$" s)
    (let* ((is-struct (string-match-p "^\\*" s))
           (name (intern (match-string 1 s)))
           (inner (match-string 2 s))
           (arg-strs (when inner (et:repr--split-at-depth inner ?~))))

      ;; Force the arg name to get parsed as a constant symbol
      (when is-struct
        (push name arg-strs)
        (setq name 'Struct))

      (cl-loop for s in arg-strs
               for role in (if (et:dt-name? name)
                               (et:dt-arg-roles name arg-strs)
                             (make-list (length arg-strs) nil))
               collect (if (not (eq role 'CONST)) s
                         (cond ((symbolp s) s) ; struct name
                               ((string-match-p ":.*" s) (intern s))
                               ((string-match-p "@.*" s) (intern (substring s 1)))
                               ((string-match-p "%.*" s) (substring s 1))
                               ((string-match-p "[0-9]+\\(\\.[0-9]+\\)?" s)
                                (string-to-number s))
                               (t (error "Invalid constant format: %s" s))))
               into args
               finally return (et:repr--parse-0 (cons name args)))))

   (t (error "Invalid parse syntax: %s" s))))


(et-defun et:repr--split-at-depth (s: String delim: String) List<String>
  "Split string S on character DELIM at depth 0 only.
Depth tracks < > and { } nesting."
  (let ((depth 0) (start 0) (result '()))
    (dotimes (i (length s))
      (let ((c (aref s i)))
        (cond ((memq c '(?< ?{)) (cl-incf depth))
              ((memq c '(?> ?})) (cl-decf depth))
              ((and (eq c delim) (= depth 0))
               (push (substring s start i) result)
               (setq start (1+ i))))))
    (push (substring s start) result)
    (nreverse result)))


;;;; Repr to type

(et-defvar et:repr--totype-gen-repls &Alist<EtGeneric~EtType> nil)
(et-defvar et:repr--totype-indeterminates @NO|List<EtIndeterminate> 'NO)

(et-defun et:repr--totype-0 (repr: EtRepr) EtType
  (cl-loop for case in (et:repr->dnf repr)
           collect
           (cl-loop for (name . args) in case
                    for totype = (or (get name 'et-repr-to-type)
                                     (error "Invalid type repr: %s" name))
                    for out = (apply totype args)
                    for type = (cond ((et:type-p out) out)
                                     ((et:type-case-p out) (et:type-new :cases (list out)))
                                     (t (et:type-new :cases out)))
                    collect type into and-types
                    finally return (apply #'et-supersect and-types))
           into or-types
           finally return
           (let* ((ored (apply #'et-union or-types)))
             (if (et:type->label ored) ored
               (et-copy-with ored :label (et:repr->label repr))))))

(et-defun et-repr-to-type-and-indes (repr: EtRepr gen-repls: &Alist<EtGeneric~EtType>)
          Cons<EtType~List<EtIndeterminate>>
  "Convert REPR to an `et-type'.

GEN-REPLS is an alist of symbols to `et-type's. Each time S:GENERIC
appears in REPR, it will be replaced with the corresponding value
in GEN-REPLS, if it exists."
  (when et-debug
    (unless (seq-set-equal-p (mapcar #'car gen-repls) (et:repr->generics repr) #'eq)
      (error "Converting with %s, but repr has generics %s"
             (mapcar #'car gen-repls) (et:repr->generics repr))))

  (let* ((et:repr--totype-gen-repls gen-repls)
         (et:repr--totype-indeterminates nil))
    (cons (et:repr--totype-0 repr)
          et:repr--totype-indeterminates)))

(defun et-repr-to-type (repr &optional gen-repls)
  (car (et-repr-to-type-and-indes repr gen-repls)))


;;;; Repr to read-only

(et-defun et:repr-to-read-only (repr: EtRepr) EtRepr
  "Convert REPR to its read-only equivalent."
  (let* ((sub #'et:repr-to-read-only)
         (gens (et:repr->generics repr)))
    (cl-loop
     for case in (et:repr->dnf repr)
     collect
     (cl-loop
      for factor in case
      collect (pcase factor
                (`(S:DT ConsFull ,lr ,_lw ,rr ,_rw)
                 (let* ((never (et-parse-repr 'Never gens)))
                   (et-q (S:DT ConsFull ,(funcall sub lr) ,never ,(funcall sub rr) ,never))))

                (`(S:DT VectorFull ,r ,_w)
                 (let* ((never (et-parse-repr 'Never gens)))
                   (et-q (S:DT VectorFull ,(funcall sub r) ,never))))

                (`(S:ALIAS ,name . ,args)
                 (et-q (S:ALIAS ,(et:type-alias-ro-name name) . ,(mapcar sub args))))

                (`(S:DT ,name . ,args)
                 (et-q (S:DT ,name ,@(et:dt-map-type-args name args sub))))

                (_ factor)))
     into new-dnf
     finally return (et-copy-with repr :dnf new-dnf))))


;;;; Replacement for matchers

(defun et:repr--factor-substitute-generics (factor gen-repls generics)
  (declare (et (factor EtReprFactor)
               (gen-repls Alist<EtGeneric~EtRepr>)
               (generics List<EtGeneric>)
               (@return EtReprDnf)))

  (let* ((sub (lambda (r) (et:repr-substitute-generics r gen-repls generics))))
    (pcase factor
      (`(S:GENERIC ,var)
       ;; Don't use alist-get, because the value of the replacement can be nil
       (if-let* ((entry (assq var gen-repls)))
           (et:repr->dnf (cdr entry))
         (error "Replacement for %s not provided" var)))
      (`(S:DT ,name . ,args)
       (et-q (((S:DT ,name . ,(et:dt-map-type-args name args sub))))))
      (`(S:ALIAS ,name . ,args)
       (et-q (((S:ALIAS ,name . ,(mapcar sub args))))))
      (`(S:POLY ,_name) factor)
      (`(S:SET ,matcher ,type)
       (et-q (((S:SET ,(funcall sub matcher) ,type)))))
      (`(S:NOINFER ,tr ,env)
       (et-q (((S:NOINFER ,tr ,(cl-loop for (g . r) in env
                                        collect (cons g (funcall sub r))))))))
      (`(S:OP . ,_)
       (et-q (((S:NOINFER ,(et:repr-new :generics (mapcar #'car gen-repls)
                                        :dnf (list (list factor)))
                          ,gen-repls)))))
      (_ (error "Invalid matcher repr factor: %s" factor)))))

(defun et:repr-substitute-generics (repr gen-repls generics)
  (declare (et (repr EtRepr)
               (gen-repls Alist<EtGeneric~EtRepr>)
               (generics List<EtGeneric>)
               (@return EtRepr)))

  (when et-debug
    (unless (seq-set-equal-p (mapcar #'car gen-repls) (et:repr->generics repr) #'eq)
      (error "Substituting %s, but repr has generics %s"
             (mapcar #'car gen-repls) (et:repr->generics repr))))

  (pcase (et:repr->dnf repr)
    (`(((S:GENERIC ,var)))
     ;; If this is a single generic, use the label from the generic
     ;; instead of from the outer repr
     (or (alist-get var gen-repls)
         (error "Replacement for %s not provided" var)))
    (dnf
     (cl-loop for case in dnf
              nconc
              (cl-loop for factor in case
                       collect
                       (et:repr--factor-substitute-generics factor gen-repls generics)
                       into and-structs
                       finally return (apply #'et-dnf-intersect and-structs))
              into new-dnf
              finally return (et-copy-with repr :dnf new-dnf :generics generics)))))


;;;; To string

(cl-defmethod cl-print-object ((repr et:repr) stream)
  (princ (format "#R<%s>" (et-repr-to-string repr)) stream))

(et-defvar et:repr--tostring-loops List<Symbol> nil)

(et-defun et-repr-to-string (repr: EtRepr) String
  (let* ((et:repr--tostring-loops nil))
    (et:repr--tostring-0 repr)))

(et-defun et:repr--tostring-0 (repr: EtRepr) String
  (cl-loop for factors in (et:repr->dnf repr)
           collect
           (cl-loop for (name . args) in factors
                    for print = (or (get name 'et:repr-print)
                                    (error "Invalid repr: %s" name))
                    unless (and (not et-print-narrows)
                                (eq name 'S:OP)
                                (memq (car args) '(typeof bind)))
                    collect (apply print args) into and-strings
                    finally return
                    (if-let* ((strs (delete-dups (remove "Any" and-strings))))
                        (string-join strs " & ") "Any"))
           into or-strings
           finally return
           (let* ((str (if or-strings (string-join or-strings " | ") "Never")))
             (if (and et-print-labels (et:repr->label repr))
                 (format "%s[%s]" (et:repr->label repr) str)
               str))))

(et-defun et:repr--tostring-named (name: Symbol args: List) String
  (pcase (cons name args)
    (`(Literal ,val)
     (format "`%s'" (prin1-to-string val)))

    (`(Struct ,name . ,args)
     (if (null args) (format "*%s" name)
       (format "*%s<%s>" name (string-join (mapcar #'et:repr--tostring-0 args) " "))))

    ((or `(ConsFull ,left-sub ,_1 ,right-sub ,_2)
         `(,(or 'Cons '&Cons 'WriteCons '&WriteCons) ,left-sub ,right-sub))
     (let ((elems (list (et:repr--tostring-0 left-sub))))
       (while (pcase right-sub
                ((and (pred listp) d)
                 (when (and (= (length d) 1) (= (length (car d)) 1))
                   (pcase (car (car d))
                     ((or `(S:DT ConsFull ,car-sub ,_1 ,cdr-sub ,_2)
                          `(S:ALIAS ,(or 'Cons '&Cons 'WriteCons '&WriteCons) ,car-sub ,cdr-sub))
                      (nconc elems (list (et:repr--tostring-0 car-sub)))
                      (setq right-sub cdr-sub)
                      t))))))
       (let ((tail-nil-p
              (and (= (length right-sub) 1)
                   (= (length (car right-sub)) 1)
                   (equal (car (car right-sub)) '(S:DT Literal nil)))))
         (if tail-nil-p
             (format "(%s)" (mapconcat #'identity elems " "))
           (format "(%s . %s)"
                   (mapconcat #'identity elems " ")
                   (et:repr--tostring-0 right-sub))))))

    (`(DynFunction ,matcher ,output-type)
     (format "(%s) -> %s" (et-pp-matcher matcher) (et:repr--tostring-0 output-type)))

    ;; Display recursive loop aliases inline
    ((and (let loop-def (get name 'et-loop-alias)) (guard loop-def))
     (if-let* ((idx (cl-position name et:repr--tostring-loops)))
         (format "#%s#" (1+ idx))
       (push name et:repr--tostring-loops)
       (format "#%s={%s}" (length et:repr--tostring-loops)
               (et-pp-type loop-def))))

    (_
     (let* ((name-str (symbol-name name))
            (strs (if (not (et:dt-name? name))
                      (mapcar #'et:repr--tostring-0 args)
                    (et:dt-map-args
                     name args
                     (lambda (arg role)
                       (if (eq role 'CONST) (format "%s" arg)
                         (et-repr-to-string arg)))))))
       (if (null args) name-str
         (format "%s<%s>" name-str (string-join strs ", ")))))))


;;;; Repr factor definitions

(defmacro et:repr--deffactor (repr-sym spec-sym arglist &rest plist)
  (declare (indent 3) (et ($expand)))
  (let* ((parse (or (plist-get plist :parse) (error "No :parse field provided")))
         (print (or (plist-get plist :print) (error "No :print field provided")))
         (totype (plist-get plist :to-type))
         (ignore (cons #'ignore (remq '&optional (remq '&rest arglist)))))
    `(progn
       (put ',spec-sym 'et-spec-parse (cons ',repr-sym (lambda ,arglist ,ignore ,parse)))
       (put ',repr-sym 'et:repr-print (lambda ,arglist ,ignore ,print))
       ,@(when totype `((put ',repr-sym 'et-repr-to-type (lambda ,arglist ,ignore ,totype)))))))

(et:repr--deffactor S:DT dt (name &rest args)
  :parse (cons name (et:dt-map-type-args name args #'et:repr--parse-0))
  :to-type
  (let* ((new-args (et:dt-map-type-args name args #'et:repr--totype-0)))
    (list (et:type-case-new :value (et:type-dt-new :name name :args new-args))))
  :print (et:repr--tostring-named name args))

(et:repr--deffactor S:ALIAS alias (name &rest args)
  :parse
  (pcase-let*
      ((`(,min . ,max) (or (et:type-alias-arity name) (error "Not an alias: %s" name)))
       (_ (unless (and (<= min (length args)) (>= max (length args)))
            (error "Alias %s requires %s arguments, got %s" name
                   (if (eq min max) min (format "%s-%s" min max))
                   (length args)))))
    (cons name (mapcar #'et:repr--parse-0 args)))
  :to-type
  (let* ((new-args (mapcar #'et:repr--totype-0 args)))
    (list (et:type-case-new :value (et:type-alias-new :name name :args new-args))))
  :print (et:repr--tostring-named (et:type-alias-display-name name) args))

(et:repr--deffactor S:POLY poly (name)
  :parse (progn (et:type-polymorph-constraints name) (list name))
  :to-type
  (let* ((constrs (cl-remove 'P:LEQ (et:type-polymorph-constraints name)
                             :key #'car :test-not #'eq)))
    (cl-loop for case in (et:type->cases (apply #'et-supersect (mapcar #'cadr constrs)))
             collect (et-copy-with case :polymorphs (cons name (et:type-case->polymorphs case)))))
  :print (format "^%s" name))

(et:repr--deffactor S:GENERIC generic (var)
  :parse (if (memq var et:repr--parsing-generics) (list var) (error "Generic %s not defined" var))
  :to-type (or (alist-get var et:repr--totype-gen-repls) (error "Generic %s not defined" var))
  :print (format "@%s" var))

(et:repr--deffactor S:SET set (dnf type)
  :parse (list (et:repr--parse-0 dnf) (et-parse-type type))
  :to-type (if (et-subtype? type (et:repr--totype-0 dnf)) (et-any) (et-never))
  :print (format "{match %s to %s}" (et:repr--tostring-0 dnf) (et-pp-type type)))

(et:repr--deffactor S:NOINFER noinfer (repr &optional env)
  :parse (list (et:repr--parse-0 repr)
               (cl-loop for g in et:repr--parsing-generics
                        collect (cons g (et:repr--parse-0 g))))
  :to-type (et-repr-to-type repr (cl-loop for (g . r) in env
                                          collect (cons g (et:repr--totype-0 r))))
  :print (format "{noinfer %s}" (et:repr--tostring-0 repr)))

(et:repr--deffactor S:OP op (op-name &rest args)
  :parse (cons op-name
               (if-let* ((op (et:repr--get-op op-name)) (parse (et:repr--op->parse op)))
                   (apply parse args) (et:repr--op-parse-fallback op args)))
  :to-type (let* ((op (et:repr--get-op op-name)))
             (apply (et:repr--op->eval op) (et:repr--op-pre-eval op args)))
  :print  (if-let* ((tostring (et:repr--op->to-string (et:repr--get-op op-name))))
              (apply tostring args) (et:repr--op-to-string-fallback op-name args)))


;;;; Spec macros

(defmacro et-defspec (name arglist &rest body)
  (declare (indent 2))
  `(progn (put ',name 'et-spec-macro (lambda ,arglist ,@body))
          (put ',(intern (format "&%s" name)) 'et-spec-macro
               (lambda ,arglist (list 'read-only (progn ,@body))))))

(et-defspec Nil () `(Literal nil))
(et-defspec True () `(Literal t))
(et-defspec Never () `(or))

(et-defspec fn (&rest args)
  (let* ((genvec (when (vectorp (car args)) (pop args)))
         (in (or (pop args) 'Nil))
         (out (or (pop args) 'Any)))
    (or (null args) (error "Too many arguments for `fn'"))
    (if genvec
        (list 'DynFunction (et-parse-matcher in genvec et:repr--parsing-generics)
              (et-parse-repr out (append (et-genvec-generics genvec) et:repr--parsing-generics)))
      (list 'Function (et:repr--parse-0 in) (et:repr--parse-0 out)))))


;;;; Op macros

(defun et:repr--get-op (op-name)
  (or (get op-name 'et-op) (error "Op not defined: %s" op-name)))

(defun et:repr--op-parse-fallback (op args)
  (cl-loop with generics = nil
           for type in (et:repr--op->args op)
           for (arg . arg-tail) on args by #'cdr
           collect
           (pcase type
             (:const arg)
             ((or :repr :type) (et:repr--parse-0 arg))
             (:generics (setq generics arg))
             (:matcher (et-parse-matcher arg generics)))
           into exprs
           finally return
           (append
            exprs
            (pcase (et:repr--op->rest-arg op)
              (:const arg-tail)
              ((or :repr :type) (mapcar #'et:repr--parse-0 arg-tail))))))

(defun et:repr--op-to-string-fallback (op-name args)
  (format "{%s %s}" op-name (mapconcat #'et-pp args " ")))

(defun et:repr--op-pre-eval (op parsed-args)
  "Convert reprs to types (in the current totype context)."
  (cl-loop for type in (et:repr--op->args op)
           for (arg . arg-tail) on parsed-args by #'cdr
           collect (if (eq type :type) (et:repr--totype-0 arg) arg) into exprs
           finally return
           (append
            exprs
            (pcase (et:repr--op->rest-arg op)
              ((or :repr :const) arg-tail)
              (:type (mapcar #'et:repr--totype-0 arg-tail))))))

(eval-and-compile
  (et-defstruct et:repr--op
    (args nil :et (List (or @:type @:repr @:const @:generics @:matcher)))
    (rest-arg nil :et (or Nil @:type @:repr @:const))
    ;; Arguments are raw specs
    (parse nil :et (or Nil (Function &List List)))
    ;; Arguments are types, matchers, and consts
    (eval nil :et (Function &List List))
    ;; Arguments are types, matchers, and consts
    (to-string nil :et (or Nil (Function &List List))))

  (defun et:repr--generate-op (arglist parse eval tostring)
    (let* ((parse-arg (lambda (form)
                        (pcase form
                          (`[,name ,type] (cons name type))
                          ((pred symbolp) (cons form :type)))))
           args rest-arg fn-arglist)

      ;; Interpret the arguments
      (cl-loop with (rest-mode after-generics) = nil
               for form in arglist
               if rest-mode
               do (if rest-arg (error "Multiple rest parameters")
                    (setq rest-arg (funcall parse-arg form))
                    (or (memq (cdr rest-arg) '(:type :repr :const))
                        (error "Invalid rest parameter: %s" form)))
               else if (eq form '&rest) do (setq rest-mode t)
               else do
               (let* ((arg (funcall parse-arg form)))
                 (pcase (cdr arg)
                   (:matcher (or after-generics (error "Matcher must be preceeded by generics"))
                             (setq after-generics nil))
                   ((guard after-generics) (error "Generics must be followed by matcher"))
                   (:generics (setq after-generics t))
                   ((or :type :repr :const))
                   (_ (error "Invalid parameter: %s" form)))
                 (push arg args))

               finally do (setq args (nreverse args)))
      (setq fn-arglist (append (mapcar #'car args)
                               (when rest-arg (list '&rest (car rest-arg)))))

      (et:repr--op-new
       :args (mapcar #'cdr args)
       :rest-arg (cdr rest-arg)
       :parse (when parse `(lambda ,fn-arglist ,parse))
       :eval `(lambda ,fn-arglist ,eval)
       :to-string (when tostring `(lambda ,fn-arglist ,tostring))))))


(defmacro et:repr--defop (name arglist &rest plist)
  (declare (indent 2))
  `(put ',name 'et-op
        ,(et:repr--generate-op arglist
                               (plist-get plist :parse)
                               (car (last plist))
                               (plist-get plist :to-string))))


;;;; Op definitions

(defmacro et:repr--capture-indeterminates (&rest body)
  `(let* ((val-and-indes
           (let* ((et:repr--totype-indeterminates nil))
             (cons (progn ,@body) et:repr--totype-indeterminates))))
     (cl-callf append et:repr--totype-indeterminates (cdr val-and-indes))
     val-and-indes))

(defmacro et:repr--indeterminate-if (cond yes no)
  `(pcase-let* ((answer-and-indes (et:repr--capture-indeterminates ,cond)))
     (if (cdr answer-and-indes) (et-union ,yes ,no)
       (if (car answer-and-indes) ,yes ,no))))

(et-defun et:repr--op-subtype? (sub: EtType super: EtType) Boolean
  "Determine subtype, making note of any indeterminates."
  (let* ((indes-or-no (et-subtype-indeterminates sub super)))
    (if (not (listp indes-or-no)) nil
      (cl-callf append et:repr--totype-indeterminates indes-or-no)
      (null indes-or-no))))

(defmacro et:repr--record-indeterminates (&rest body)
  "Run BODY while recording subtype indeterminates.

The advice strategy is perhaps a slightly unhinged way of achieving
this, but the alternatives are either:

1. Make `et-subtype?' inherintly mutate global state (gross)

2. Write duplicate versions of several functions with
`et-subtype?' -> `et:repr--op-subtype?'

3. Modify the original functions to return indeterminates (objectively
the correct option, but I am too lazy at the moment.) A clean eventual
solution would involve modifying `et-match-result' to have an
indeterminates slot."
  `(et-with-advice #'et-subtype? :override #'et:repr--op-subtype?
     ,@body))

(et:repr--defop subtract (a b)
  :to-string (format "{%s - %s}" (et:repr--tostring-0 a) (et:repr--tostring-0 b))
  (et-subtract a b))

(et:repr--defop type ([type :const])
  :to-string (format "type:%s" (et-pp type))
  type)

(et:repr--defop bind ([var :const] type)
  :to-string (format "{%s : %s}" (et:type-var->name var) (et:repr--tostring-0 type))
  (et:type-case-new
   :value (et:type-dt-new :name 'Any)
   :binds (list (cons var type))))

(et:repr--defop typeof ([var :const])
  :to-string (format "{= %s}" (et:type-var->name var))
  (et:type-case-new :value (et:type-dt-new :name 'Any) :typeofs (list var)))

(et:repr--defop bindsof (type)
  (if (et-never-p type) nil
    (list (et:type-case-new
           :value (et:type-dt-new :name 'Any)
           :binds (et-type-binds type)))))

(et:repr--defop infer (type [genvec :generics] [matcher :matcher] [yes :repr] [no :repr])
  :parse
  (list (et:repr--parse-0 type)
        genvec
        (et-parse-matcher matcher genvec et:repr--parsing-generics)
        (et:repr--parse-0 yes (et-genvec-generics genvec))
        (et:repr--parse-0 no))
  :to-string (format "{if %s matches %s then %s else %s}"
                     (et:repr--tostring-0 type) (et-pp-matcher matcher)
                     (et:repr--tostring-0 yes) (et:repr--tostring-0 no))
  (let* ((result
          (et:repr--record-indeterminates
           (et:algebra-infer matcher type yes et:repr--totype-gen-repls))))
    (if (et:match-result->success result) (et:match-result->value result)
      ;; Lazily evaluate (only if matching failed)
      (et:repr--totype-0 no))))

(et:repr--defop extends? (sub super [yes :repr] [no :repr])
  :to-string (format "{if %s extends %s then %s else %s}"
                     (et:repr--tostring-0 sub) (et:repr--tostring-0 super)
                     (et:repr--tostring-0 yes) (et:repr--tostring-0 no))
  (et:repr--indeterminate-if (et:repr--op-subtype? sub super)
                             (et:repr--totype-0 yes) (et:repr--totype-0 no)))

(et:repr--defop if? (type [yes :repr] [no :repr])
  :to-string (format "{if %s then %s else %s}"
                     (et:repr--tostring-0 type) (et:repr--tostring-0 yes) (et:repr--tostring-0 no))
  ;; (TYPE is always true) <=> (Nil is not a subtype of TYPE)
  (et:repr--indeterminate-if (et:repr--op-subtype? (et Nil) type)
                             (et:repr--totype-0 no) (et:repr--totype-0 yes)))

(et:repr--defop if-nil? (type [yes :repr] [no :repr])
  :to-string (format "{if %s is nil then %s else %s}"
                     (et:repr--tostring-0 type) (et:repr--tostring-0 yes) (et:repr--tostring-0 no))
  ;; (TYPE is always nil) <=> (TYPE is a subtype of Nil)
  (et:repr--indeterminate-if (et:repr--op-subtype? type (et Nil))
                             (et:repr--totype-0 yes) (et:repr--totype-0 no)))

(et-defspec match (type &rest options)
  (pcase options
    ('nil 'Never)
    (`([,pat ,out] . ,rest) `(extends? ,type ,pat ,out (match ,type ,@rest)))
    (`([,(and genvec (pred vectorp)) ,pat ,out] . ,rest)
     `(infer ,type ,genvec ,pat ,out (match ,type ,@rest)))
    (`(,default) default)
    (_ (error "Invalid format for switch"))))

(et:repr--defop eval ([func :const] &rest args)
  :to-string (format "{eval %s on %s}" func (mapconcat #'et:repr--tostring-0 args ", "))
  (apply func args))

(et:repr--defop is? (type is)
  :to-string (format "{%s is %s}" (et:repr--tostring-0 type) (et:repr--tostring-0 is))
  (et-union (et:algebra-when (et-supersect type is) (et True))
            (et:algebra-when (et-subtract type is) (et Nil))))

;; Same as 'is', but the 'nil' case doesn't necessarily mean the value IS NOT the type
(et:repr--defop is-a? (type is)
  :to-string (format "{%s is %s}" (et:repr--tostring-0 type) (et:repr--tostring-0 is))
  (et-union (et:algebra-when (et-supersect type is) (et True))
            (et Nil)))

(et-defspec overlapping? (a b yes &optional no)
  `(extends? (and ,a ,b) Never ,(or no 'Never) ,yes))

(et-defspec replace-in (type from to)
  `(or (subtract ,type ,from) (overlapping? ,type ,from ,to)))

(et:repr--defop freshen-shallow (type)
  (et:algebra-freshen-type-shallow type))

(et:repr--defop freshen-deep (type)
  (et:algebra-freshen-type type))


;;;; Type to repr

(et-defun et-type-to-repr (type: EtType) EtRepr
  "Convert an `et-type' to a repr."
  (cl-loop for case in (et:type->cases type)
           for value = (et:type-case->value case)
           collect
           (cons
            (pcase value
              ((cl-struct et:type-dt name args)
               (et-ql S:DT ,(et:type-dt->name value)
                      ,@(et:dt-map-type-args name args #'et-type-to-repr)))
              ((cl-struct et:type-alias name args)
               (et-ql S:ALIAS ,name ,@(mapcar #'et-type-to-repr args)))
              (_ (error "Unsupported type case value: %s" value)))

            (nconc
             (cl-loop for name in (et:type-case->polymorphs case)
                      collect (et-ql S:POLY ,name))
             (cl-loop for (var . type) in (et:type-case->binds case)
                      collect (et-ql S:OP bind ,var ,(et-type-to-repr type)))
             (cl-loop for var in (et:type-case->typeofs case)
                      collect (et-ql S:OP typeof ,var))))
           into repr-dnf
           finally return
           (et:repr-new :dnf repr-dnf
                        :label (et:type->label type))))


;;; ============================================================
;;; Matching - `et:match'
;;;; Struct

(et-defstruct et:match-matcher
  "A type pattern which is matched against by a concrete type.

DNF is the struct representing the matcher."
  (generics nil :et List<EtGeneric>)
  (repr nil :et EtRepr)
  (constraints nil :et List<EtTypeConstraint>))

(et-defun et:match--validate (matcher: *et:match-matcher) *et:match-matcher
  "Check that a matcher is valid."
  (or (et:match-matcher-p matcher)
      (error "Not a matcher: %s" matcher))

  (when et-debug
    (let* ((generics (et:match-matcher->generics matcher)))
      (dolist (generic generics)
        (or (symbolp generic) (error "Generics must be a list of symbols")))

      (unless (cl-subsetp (et:repr->generics (et:match-matcher->repr matcher)) generics)
        (error "Matcher repr generics %s exceed matcher generics %s"
               (et:repr->generics (et:match-matcher->repr matcher)) generics))

      (cl-loop for q in (et:match-matcher->constraints matcher)
               do (pcase q
                    (`(,(or 'Q:LEQ 'Q:GEQ)
                       ,(and gen (guard (memq gen generics)))
                       ,(pred et:type-p)))
                    (_ (error "Invalid constraint: %s" q))))

      (cl-flet ((genericp (var) (or (and (symbolp var) (memq var generics))
                                    (error "Not a generic: %s" var))))
        (dolist (case (et:repr->dnf (et:match-matcher->repr matcher)))
          (dolist (factor case)
            (pcase factor
              (`(S:DT ,(and name (pred symbolp)) . ,args)
               (et:dt-map-args
                name args
                (lambda (arg role)
                  (pcase role
                    ('CONST nil)
                    ((or 'CO 'CONTRA 'ISO) (et:match-matcher-new :generics generics :repr arg))
                    (_ (error "Unknown role type: %s" role))))))
              (`(S:ALIAS ,(pred symbolp) . ,args)
               (dolist (arg args) (et:match-matcher-new :generics generics :repr arg)))
              (`(S:GENERIC ,(pred genericp)))
              (`(S:POLY ,(pred symbolp)))
              (`(S:SET ,_ ,(pred et:type-p)))
              (`(S:NOINFER ,(pred et:repr-p) ,_))
              (`(S:OP . ,_))
              (_ (error "Invalid match factor: %s" factor))))))))

  matcher)

(advice-add #'et:match-matcher-new :filter-return #'et:match--validate)


;;;; Parse/print matcher

(defmacro et-matcher (genvec &rest args)
  (declare (indent 1) (et ($expand)))
  (or (vectorp genvec) (error "Write the generics as a vector"))
  `(et-parse-matcher (et-q ,(if (eq (length args) 1) (car args) args))
                     ,genvec))

(et-defun et-parse-matcher (spec: Any genvec: &Vector<Any> &optional extra-gens: List<EtGeneric>)
          *et:match-matcher
  "Parse SPEC as an `et-matcher' with GENVEC.

GENVEC is a list of symbols and constraint specs. A constraint
spec is one of:
  (= GEN TYPE-SPEC)
  (>= GEN TYPE-SPEC)
  (<= GEN TYPE-SPEC)

A generic can be implicitly defined by just providing a constraint
involving that generic. For example, the spec [(<= T Number)] is the
same as [T (<= T Number)]."
  (let* ((generics (et-genvec-generics genvec)))
    (et:match-matcher-new
     :generics generics
     :constraints (et-genvec-constraints genvec)
     :repr (et-parse-repr spec (append generics extra-gens)))))

(et-defun et-pp-matcher (matcher: *et:match-matcher) String
  "Format an `et-matcher' into a human-readable string."
  (let* ((generics (et:match-matcher->generics matcher))
         (body (et-repr-to-string (et:match-matcher->repr matcher))))
    (format "[%s] %s" (mapconcat #'symbol-name generics " ") body)))

(cl-defmethod cl-print-object ((matcher et:match-matcher) stream)
  (princ (format "#<%s>" (et-pp-matcher matcher)) stream))


;;;; Match results

(defvar et:match--constraints-stack nil
  "Stack of calls to `et--sub/super-constraints' for preventing loops.

Used by `et-stop-recursion', with ELEM=(`sub'|`super' M-REPR TYPE).
Thus, this variable stores a list of (ELEM . DEFAULT) pairs.")

(et-defstruct et:match-result
  "The result of checking a type against a matcher."
  (success nil :et-generics [V] :et Boolean)
  (stack nil :et EtMatchStack)
  (value nil :et V))

(defun et:match-failed ()
  (et-declare (@generics [T]) (@return *et:match-result<T>))
  (et:match-result-new
   :success nil
   :stack (mapcar #'car et:match--constraints-stack)))

(defun et:match-succeeded (value)
  (et-declare (@generics [T]) (value T) (@return *et:match-result<T>))
  (et:match-result-new :success t :value value))

(defun et:match-result-and (&rest results)
  (declare (et (@generics [T])
               (results &List<EtMatchResult<List<T>>>)
               (@return EtMatchResult<List<T>>)))
  (cl-loop for result in results
           if (not (et:match-result->success result))
           return result
           append (et:match-result->value result) into constraints
           finally return (et:match-succeeded (delete-dups constraints))))


;;;; Expand matcher aliases

(defun et:match--expand-aliases (matcher)
  (declare (et (matcher *et:match-matcher) (@return *et:match-matcher)))

  (et:match-matcher-new :repr (et:match--expand-repr-aliases (et:match-matcher->repr matcher)
                                                             (et:match-matcher->generics matcher))
                        :generics (et:match-matcher->generics matcher)
                        :constraints (et:match-matcher->constraints matcher)))

(defun et:match--expand-repr-aliases (repr scope)
  (declare (et (repr EtRepr)
               (scope List<EtGeneric>)
               (@return EtRepr)))

  (cl-loop for case in (et:repr->dnf repr)
           append
           (cl-loop for factor in case
                    collect
                    (pcase factor
                      (`(S:ALIAS ,name . ,args)
                       (et:repr->dnf
                        (et:match--expand-repr-aliases
                         (et:type-alias-call name args nil scope)
                         scope)))
                      (other (list (list other))))
                    into and-terms
                    finally return (apply #'et-dnf-intersect and-terms))
           into new-cases
           finally return (et:repr-new :dnf new-cases :generics scope :label (et:repr->label repr))))


;;;; Iso match

(et-defun et:match--iso-constraints (matcher: *et:match-matcher type: EtType) EtConstrainResult
  "Entrypoint for calculating constraints."
  (let* ((et:match--constraints-stack nil))
    (et:match--iso-constraints-0 matcher type)))

(et-defun et:match--iso-constraints-0 (matcher: *et:match-matcher type: EtType) EtConstrainResult
  (et:match-result-and
   (et:match--sub-constraints-0 matcher type)
   (et:match--super-constraints-0 matcher type)))


;;;; Sub constraints

(et-defun et:match-sub-constraints (matcher: *et:match-matcher type: EtType) EtConstrainResult
  "Entrypoint for calculating constraints."
  (let* ((et:match--constraints-stack nil))
    (et:match--sub-constraints-0 matcher type)))

(et-defun et:match--sub-constraints-0 (matcher: *et:match-matcher type: EtType) EtConstrainResult
  (et-stop-recursion et:match--constraints-stack (list 'sub (et:match-matcher->repr matcher) type)
                     (et:match-succeeded nil)
    (et:match--sub-constraints-1 matcher type)))

(et-defun et:match--sub-constraints-1 (matcher: *et:match-matcher type: EtType) EtConstrainResult
  (setq matcher (et:match--expand-aliases matcher))

  (cl-loop for case in (et:type->cases type)
           for result-no-binds = (et:match--sub-constraints-2 matcher case)
           ;; For GEQ constraints, add in binds
           for binds = (append (et:type-case->binds case)
                               (cl-loop for var in (et:type-case->typeofs case)
                                        collect (cons var (et:algebra-remove-binds (et-type case)))))
           for result-with-binds =
           (if (or (not binds) (not (et:match-result->success result-no-binds))) result-no-binds
             (cl-loop for q in (et:match-result->value result-no-binds)
                      collect
                      (if (not (eq 'Q:GEQ (car q))) q
                        (let* ((type (et-supersect (caddr q) (et:algebra-replace-binds (et-any) binds))))
                          (list 'Q:GEQ (cadr q) type)))
                      into qs
                      finally return (et:match-succeeded qs)))
           collect result-with-binds into results
           finally return (apply #'et:match-result-and results)))

(et-defun et:match--sub-constraints-2 (matcher: *et:match-matcher case: *et:type-case) EtConstrainResult
  (cl-loop for match-case in (et:repr->dnf (et:match-matcher->repr matcher))
           for result-1 =
           (cl-loop for match-factor in match-case
                    for gens = (et:match-matcher->generics matcher)
                    collect (et:match--sub-or-super-constraints-3 match-factor case gens) into results
                    finally return (apply #'et:match-result-and results))
           when (et:match-result->success result-1) return result-1
           ;; If all cases failed, fallback to 2.2 (expand aliases) or 2.3 (fail)
           finally return
           (let* ((val (et:type-case->value case)))
             (if (et:type-alias-p val)
                 (et:match--sub-constraints-0
                  matcher
                  (cl-loop with exp = (et:type-alias-expand val)
                           for c in (et:type->cases exp)
                           collect (et:type-case-new
                                    :value (et:type-case->value c)
                                    :binds (et:type-case->binds case)
                                    :typeofs (et:type-case->typeofs case))
                           into cases finally return (et:type-new :label (et:type->label exp) :cases cases)))
               ;; result-1 has the stack trace we want
               result-1))))

(defun et:match--sub-or-super-constraints-3 (match-factor case generics &optional is-super)
  "sub-3 and super-3 are similar enough that combining them is simpler."
  (declare (et (match-factor EtReprFactor)
               (case *et:type-case)
               (generics List<EtGeneric>)
               (@return EtConstrainResult)))

  (pcase match-factor
    (`(S:GENERIC ,var)
     (let* ((q (list (if is-super 'Q:LEQ 'Q:GEQ) var (et-type case))))
       (et:match-succeeded (list q))))
    (`(S:SET ,mr ,type)
     (funcall (if is-super #'et:match--super-constraints-0 #'et:match--sub-constraints-0)
              (et:match-matcher-new :repr mr :generics generics) type))
    (`(,(or 'S:NOINFER 'S:OP 'S:POLY) . ,_)
     (let* ((req (list (if is-super 'R:LEQ 'R:GEQ)
                       (et:repr-new :generics generics :dnf (list (list match-factor)))
                       (et-type case))))
       (et:match-succeeded (list req))))
    (`(S:DT ,mdt-name . ,mdt-args)
     (pcase (et:type-case->value case)
       ((and alias (pred et:type-alias-p))
        (if (not is-super) (et:match-failed)
          (et:match--super-constraints-0 (et:match-matcher-new :generics generics :repr (et:repr-new :generics generics :dnf (list (list match-factor))))
                                         (et:type-alias-expand alias))))
       ((and dt (pred et:type-dt-p))
        (et:match--sub-or-super-constraints-4
         mdt-name mdt-args (et:type-dt->name dt) (et:type-dt->args dt)
         generics is-super))
       (_ (error "Unsupported matching datatype"))))
    (_ (error "Invalid match factor"))))

(defun et:match--sub-or-super-constraints-4 (m-name m-args t-name t-args generics &optional is-super)
  (declare (et (m-name Var) (m-args List)
               (t-name Var) (t-args List)
               (generics List<EtGeneric>)
               (@return EtConstrainResult)))

  (cl-flet ((make-matcher (mr) (et:match-matcher-new :repr mr :generics generics)))
    (if (not is-super)
        ;; subtype matching (super=MATCHER > sub=TYPE)
        (et:dt-constraints
         t-name t-args m-name m-args
         (lambda (type ms) (et:match--sub-constraints-0 (make-matcher ms) type))
         (lambda (type ms) (et:match--super-constraints-0 (make-matcher ms) type))
         (lambda (type ms) (et:match--iso-constraints-0 (make-matcher ms) type))
         (lambda (literal ms) (et:match--sub-constraints-0 (make-matcher ms) (et-literal literal)))
         ;; The synthesized super (Plist side, m-args) is a matcher repr
         (lambda (car rest-plist) (et:dt-cons-plist-super-matcher car rest-plist generics))
         ;; A DynFunction type against a Function matcher factor can only
         ;; be resolved once the matcher's generics are known, so record
         ;; an R:FN constraint for constraint satisfaction.
         (lambda (dyn-args fn-args)
           (et:match-succeeded (list (list 'R:FN (car dyn-args) (cadr dyn-args)
                                           (car fn-args) (cadr fn-args))))))
      ;; supertype matching (sub=MATCHER < super=TYPE)
      (et:dt-constraints
       m-name m-args t-name t-args
       (lambda (ms type) (et:match--super-constraints-0 (make-matcher ms) type))
       (lambda (ms type) (et:match--sub-constraints-0 (make-matcher ms) type))
       (lambda (ms type) (et:match--iso-constraints-0 (make-matcher ms) type))
       (lambda (literal type)
         (let ((literal-m (make-matcher (et:repr-new :generics generics :dnf (et-q (((S:DT Literal ,literal))))))))
           (et:match--super-constraints-0 literal-m type)))
       ;; The synthesized super (Plist side, t-args) is a type
       #'et:dt-cons-plist-super-type
       nil))))


;;;; Super constraints

(et-defun et:match--super-constraints-0 (matcher: *et:match-matcher type: EtType) EtConstrainResult
  ;; The 'super' version of scoped constraint checking
  (et-stop-recursion et:match--constraints-stack (list 'super (et:match-matcher->repr matcher) type)
                     (et:match-succeeded nil)
    (et:match--super-constraints-1 matcher type)))

(et-defun et:match--super-constraints-1 (matcher: *et:match-matcher type: EtType) EtConstrainResult
  (setq matcher (et:match--expand-aliases matcher))

  (cl-loop for m-case in (et:repr->dnf (et:match-matcher->repr matcher))
           collect (et:match--super-constraints-2 m-case type (et:match-matcher->generics matcher))
           into results
           finally return (apply #'et:match-result-and results)))

(et-defun et:match--super-constraints-2 (match-case: EtReprCase type: EtType generics: List<EtGeneric>)
          EtConstrainResult
  (or
   (pcase match-case
     ;; A single match factor, at that is a S:GENERIC match factor
     (`((S:GENERIC ,var))
      (et:match-succeeded (list (list 'Q:LEQ var type)))))

   (cl-loop for case in (et:type->cases type)
            for result =
            (cl-loop for match-factor in match-case
                     collect (et:match--sub-or-super-constraints-3 match-factor case generics 'SUPER)
                     into results
                     finally return (apply #'et:match-result-and results))
            when (et:match-result->success result) return result
            ;; If all cases failed, return never
            finally return (et:match-failed))))


;;;; Satisfy constraints

(et-defun et:match--type-contains-binds? (type: EtType) Boolean
  (cl-loop for c in (et:type->cases type)
           thereis
           (or (et:type-case->binds c) (et:type-case->typeofs c)
               (pcase (et:type-case->value c)
                 ((cl-struct et:type-alias args)
                  (cl-loop for arg in args thereis (et:match--type-contains-binds? arg)))
                 ((cl-struct et:type-dt args)
                  (cl-loop for arg in args
                           thereis (and (et:type-p arg) (et:match--type-contains-binds? arg))))))))

(et-defun et-sub-match
    (matcher: *et:match-matcher type: EtType &optional largest: Boolean) EtMatchResult
  "Match TYPE as a subtype of MATCHER.

When LARGEST is non-nil, prefer the largest inferred generic types."
  (let* ((result (et:match-sub-constraints matcher type)))
    (if (not (et:match-result->success result)) result
      ;; If matching succeeded, try to determine the optimal generic types
      (let* ((qs (append (et:match-result->value result) (et:match-matcher->constraints matcher)))
             (types (et:match--satisfy-constraints
                     (et:match-matcher->generics matcher) qs largest)))
        (if (eq types 'INVALID)
            (et:match--sub-match-failed-result matcher type qs)
          (et:match-result-new :success t :value types))))))

(et-defun et:match--sub-match-failed-result
    (matcher: EtMatcher type: EtType qs: &List<EtTypeConstraint>) EtMatchResult
  "Create a failed match result with the desired failure stack.

This function exists purely to make the failed result stack contain the
information necessary to give the user better failure diagnostics."
  (let* ((repls
          (cl-loop for gen in (et:match-matcher->generics matcher)
                   for uppers = (cl-loop for (op g type) in qs
                                         collect (if (and (eq op 'Q:LEQ) (eq g gen)) type (et Any)))
                   when uppers
                   collect (cons gen (et-type-to-repr (apply #'et-supersect uppers)))))
         (mrepr (et:repr-substitute-generics (et:match-matcher->repr matcher) repls nil))
         (diagnostic
          (when (and repls (eq (length repls) (length (et:match-matcher->generics matcher))))
            (et:match-sub-constraints (et:match-matcher-new :repr mrepr) type))))
    (or (when (not (et:match-result->success diagnostic)) diagnostic)
        (et:match-failed))))

(et-defun et-iso-match (matcher: *et:match-matcher type: EtType) EtMatchResult
  "Like `et-sub-match', but require MATCHER and TYPE to be equivalent.

Uses `et:match--iso-constraints' (both sub- and super-constraints) instead of
`et:match-sub-constraints', so a successful match means TYPE is isomorphic to
MATCHER under the inferred generics, not merely a subtype of it."
  (let* ((result (et:match--iso-constraints matcher type)))
    (if (not (et:match-result->success result)) result
      ;; If matching succeeded, try to determine the optimal generic types
      (let* ((qs (append (et:match-result->value result) (et:match-matcher->constraints matcher)))
             (types (et:match--satisfy-constraints
                     (et:match-matcher->generics matcher) qs)))
        (if (eq types 'INVALID) (et:match-failed)
          (et:match-result-new :success t :value types))))))

(et-defun et:match--satisfy-constraints
    (generics: &List<EtGeneric> constraints: &List<EtMatchConstraint>
               &optional largest: Boolean)
    List<EtType>|@INVALID
  "Return a list of types for GENERICS satisfying CONSTRAINTS.

Returns `INVALID' if impossible. This function allows the resulting
types to be the never type. When LARGEST is non-nil, include upper-bound
constraints when constructing each inferred type."
  (cl-loop
   for gen in generics
   for gen-result =
   (let* ((guess
           (cl-loop for (fact g type) in constraints
                    when (and (eq g gen) (or largest (eq fact 'Q:GEQ)))
                    collect type into types
                    finally return (apply #'et-union types))))
     (if (cl-loop for (fact g type) in constraints
                  always
                  (or (not (eq g gen))
                      (not (eq fact 'Q:LEQ))
                      (et-subtype? guess type)))
         guess 'INVALID))
   when (equal gen-result 'INVALID)
   do (cl-return 'INVALID)

   collect (cons gen gen-result) into gen-repls

   finally return
   (cl-loop for (fact tr type) in constraints
            when (memq fact '(R:LEQ R:GEQ))
            do
            (let* ((replaced (et-repr-to-type tr gen-repls))
                   (valid? (if (eq fact 'R:LEQ) (et-subtype? replaced type) (et-subtype? type replaced))))
              (unless valid? (cl-return 'INVALID)))
            finally return
            (et:match--satisfy-fn-constraints constraints gen-repls largest))))

(et-defun et:match--satisfy-fn-constraints
    (constraints: &List<EtMatchConstraint> gen-repls: &Alist<EtGeneric~EtType>
                  &optional largest: Boolean)
    List<EtType>|@INVALID
  "Process the `R:FN' constraints in CONSTRAINTS, with known generics.

Each constraint is (R:FN DYN-MATCHER DYN-OUT-REPR FN-IN-MR FN-OUT-MR),
recorded when a `DynFunction' type was matched against a `Function'
matcher factor. Each constraint first suggests more precise values for
the generics in FN-IN-MR from the DynFunction's input matcher. A
suggestion is retained only when LARGEST is non-nil and all R:FN
constraints remain valid.

With the input generics determined, FN-IN-MR resolves to a concrete
arglist type and the DynFunction infers its output. FN-OUT-MR must
accommodate that output: a bare generic is enlarged to include it
\(rechecking that generic's upper bounds), while anything else must
already be a supertype of it.

Returns the final list of generic types, or `INVALID'."
  (cl-labels
      ((to-type (repr repls)
         ;; The reprs come from a matcher, so they can contain factors
         ;; which are invalid in a type, in which case the match fails.
         (condition-case-unless-debug nil (et-repr-to-type repr repls)
           (error 'INVALID)))
       (upper-bounds-satisfied? (repls)
         (cl-loop for (fact gen type) in constraints
                  always
                  (or (not (eq fact 'Q:LEQ))
                      (when-let* ((entry (assq gen repls)))
                        (et-subtype? (cdr entry) type)))))
       (fn-valid? (q repls)
         (pcase-let* ((`(,_ ,dyn-matcher ,dyn-out-repr ,fn-in-mr ,fn-out-mr) q)
                      (input (to-type fn-in-mr repls))
                      (result (unless (eq input 'INVALID)
                                (et:algebra-infer dyn-matcher input dyn-out-repr))))
           (and
            result
            (et:match-result->success result)
            (pcase (et:repr->dnf fn-out-mr)
              (`(((S:GENERIC ,var)))
               (when-let* ((entry (assq var repls)))
                 (let* ((trial-repls (copy-alist repls))
                        (trial-entry (assq var trial-repls)))
                   (setcdr trial-entry
                           (et-union (cdr entry) (et:match-result->value result)))
                   (upper-bounds-satisfied? trial-repls))))
              (_
               (let* ((out-type (to-type fn-out-mr repls)))
                 (and (not (eq out-type 'INVALID))
                      (et-subtype? (et:match-result->value result) out-type))))))))
       (all-fn-valid? (repls)
         (cl-loop for q in constraints
                  when (eq (car q) 'R:FN)
                  always (fn-valid? q repls)))
       (input-suggestions (q)
         (pcase-let* ((`(,_ ,dyn-matcher ,dyn-out-repr ,fn-in-mr ,fn-out-mr) q)
                      (dyn-generics (et:match-matcher->generics dyn-matcher))
                      (out-type (to-type fn-out-mr gen-repls))
                      (out-constraints
                       (unless (eq out-type 'INVALID)
                         (let* ((et:match--constraints-stack nil))
                           (et:match--super-constraints-0
                            (et:match-matcher-new
                             :generics dyn-generics
                             :repr dyn-out-repr)
                            out-type))))
                      (dyn-constraints
                       (append
                        (et:match-matcher->constraints dyn-matcher)
                        (when (and out-constraints
                                   (et:match-result->success out-constraints))
                          (et:match-result->value out-constraints))))
                      (dyn-types
                       (cl-loop
                        for gen in dyn-generics
                        for lowers =
                        (cl-loop for (fact g type) in dyn-constraints
                                 when (and (eq g gen) (eq fact 'Q:GEQ))
                                 collect type)
                        for uppers =
                        (cl-loop for (fact g type) in dyn-constraints
                                 when (and (eq g gen) (eq fact 'Q:LEQ))
                                 collect type)
                        for lower = (apply #'et-union lowers)
                        for upper = (if uppers (apply #'et-supersect uppers) (et-any))
                        unless (et-subtype? lower upper) return 'INVALID
                        collect upper))
                      (dyn-repls
                       (unless (eq dyn-types 'INVALID)
                         (cl-mapcar #'cons dyn-generics dyn-types)))
                      (dyn-input
                       (when dyn-repls
                         (to-type (et:match-matcher->repr dyn-matcher) dyn-repls)))
                      (outer-generics (mapcar #'car gen-repls))
                      (suggestion-constraints
                       (unless (or (null dyn-repls)
                                   (eq dyn-input 'INVALID))
                         (let* ((et:match--constraints-stack nil))
                           (et:match--super-constraints-0
                            (et:match-matcher-new
                             :generics outer-generics
                             :repr fn-in-mr)
                            dyn-input)))))
           (when (and suggestion-constraints
                      (et:match-result->success suggestion-constraints))
             (let* ((suggestions
                     (et:match--satisfy-constraints
                      outer-generics
                      (et:match-result->value suggestion-constraints)
                      t)))
               (unless (eq suggestions 'INVALID) suggestions))))))

    ;; Let each R:FN constraint contribute input recommendations. Apply
    ;; them independently so an output-dependent argument can remain
    ;; Never without discarding useful recommendations for other args.
    (when largest
      (cl-loop for q in constraints
               when (eq (car q) 'R:FN)
               do
               (cl-loop for entry in gen-repls
                        for suggestion in (input-suggestions q)
                        for old = (cdr entry)
                        do
                        (setcdr entry (et-union old suggestion))
                        (unless (and (upper-bounds-satisfied? gen-repls)
                                     (all-fn-valid? gen-repls))
                          (setcdr entry old)))))

    ;; Validate the final inputs and update bare-generic outputs.
    (cl-loop
     for q in constraints
     when (eq (car q) 'R:FN)
     do
     (pcase-let* ((`(,_ ,dyn-matcher ,dyn-out-repr ,fn-in-mr ,fn-out-mr) q)
                  (input (to-type fn-in-mr gen-repls))
                  (result (unless (eq input 'INVALID)
                            (et:algebra-infer dyn-matcher input dyn-out-repr))))
       (unless (and result (et:match-result->success result)) (cl-return 'INVALID))

       (pcase (et:repr->dnf fn-out-mr)
         (`(((S:GENERIC ,var)))
          (let* ((entry (or (assq var gen-repls) (cl-return 'INVALID)))
                 (enlarged (et-union (cdr entry) (et:match-result->value result))))
            (setcdr entry enlarged)
            (unless (upper-bounds-satisfied? gen-repls)
              (cl-return 'INVALID))))
         (_
          (let* ((out-type (to-type fn-out-mr gen-repls)))
            (unless (and (not (eq out-type 'INVALID))
                         (et-subtype? (et:match-result->value result) out-type))
              (cl-return 'INVALID))))))

     finally return (mapcar #'cdr gen-repls))))


;;; ============================================================
;;; Type algebra - `et:algebra'
;;;; Union

(et-defun et:algebra-union (&rest types: List<EtType>) EtType
  "Return the exact type union of TYPES."
  (cl-loop with label = nil
           for type in types
           when (et:type->label type) do (setq label (et:type->label type))
           nconc (apply #'list (et:type->cases type)) into cases
           finally return (et:type-new :label label :cases cases)))

(et-defun et-union (&rest types: List<EtType>) EtType
  (et:algebra-simplify-type (apply #'et:algebra-union types)))


;;;; Subtype

(et-defvar et:algebra--subtype-stack List<Cons<EtType~EtType>> nil
  "Stack of calls to `et:algebra--subtype-0' with the form (SUBTYPE . SUPERTYPE).")

(et-defun et-subtype? (sub: EtType super: EtType) Boolean
  "Entrypoint for determining subtype."
  (null (et-subtype-indeterminates sub super)))

(et-defun et-subtype-indeterminates (sub: EtType super: EtType) @NO|List<EtIndeterminate>
  "Calculate subtype, recording branches along the way.

`NO' => Not subtype
`nil' => Definitely subtype
non-empty list => Might be subtype depending on the conditions."
  (let* ((et:algebra--subtype-stack nil)
         (result (et:algebra--subtype-0 sub super)))
    (if (not (et:match-result->success result)) 'NO
      (et:match-result->value result))))

(et-defun et:algebra-subtype-result (sub: EtType super: EtType) EtSubtypeResult
  (let ((et:algebra--subtype-stack nil))
    (et:algebra--subtype-0 sub super)))

;; This function could just use `et-sub-match', but it needs to take
;; into account binds.

(et-defun et:algebra--subtype-0 (sub: EtType super: EtType) EtSubtypeResult
  (if (equal sub super) (et:match-succeeded nil) ; Not strictly necessary, but improves efficiency
    (et-stop-recursion et:algebra--subtype-stack (cons sub super)
                       (et:match-succeeded nil)
      (et:algebra--subtype-1 sub super))))

(et-defun et:algebra--subtype-1 (sub: EtType super: EtType) EtSubtypeResult
  (setq sub (et-expand-aliases sub))
  (setq super (et-expand-aliases super))

  (cl-loop for sub-case in (et:type->cases sub)
           collect
           (cl-loop for super-case in (et:type->cases super)
                    collect (et:algebra--sub-case sub-case super-case) into or-results
                    finally return (et:algebra--subtype-result-or or-results))
           into and-results
           finally return (apply #'et:match-result-and and-results)))

(et-defun et:algebra--subtype-result-or (or-results: &List<EtSubtypeResult>)
          EtSubtypeResult
  (setq or-results (cl-remove-if-not #'et:match-result->success or-results))
  (if (null or-results) (et:match-failed)
    ;; If there is any unconditionally true result (no indeterminates), return it.
    ;; Otherwise, add together all of the indeterminates, as to not discriminate.
    (or (cl-find-if-not #'et:match-result->value or-results)
        (et:match-succeeded
         (apply #'append (mapcar #'et:match-result->value or-results))))))

(et-defun et:algebra-sub-dt? (sub: *et:type-dt super: *et:type-dt) Boolean
  (et:match-result->success (et:algebra--sub-dt sub super)))

(et-defun et:algebra--sub-dt (sub: *et:type-dt super: *et:type-dt)
          EtSubtypeResult
  (et:dt-constraints
   (et:type-dt->name sub) (et:type-dt->args sub)
   (et:type-dt->name super) (et:type-dt->args super)
   (lambda (a b) (et:algebra--subtype-0 a b))
   (lambda (a b) (et:algebra--subtype-0 b a))
   (lambda (a b) (et:match-result-and (et:algebra--subtype-0 a b) (et:algebra--subtype-0 b a)))
   (lambda (literal b) (et:match-result-and (et:algebra--subtype-0 (et-literal literal) b)))
   #'et:dt-cons-plist-super-type
   nil))

(et-defun et:algebra--sub-binds (sub-binds: EtBinds super-binds: EtBinds) EtSubtypeResult
  (cl-loop for (var . super-type) in super-binds
           for sub-type = (alist-get var sub-binds)
           collect (if sub-type (et:algebra--subtype-0 sub-type super-type)
                     (et:match-failed))
           into results
           finally return (apply #'et:match-result-and results)))

(et-defun et:algebra--sub-case (sub: *et:type-case super: *et:type-case) EtSubtypeResult
  (let* ((result
          (et:match-result-and
           (if (cl-subsetp (et:type-case->typeofs super) (et:type-case->typeofs sub))
               (et:match-succeeded nil) (et:match-failed))

           ;; For all polymorphs in the super-type,
           ;; the subtype must have the same polymorphs,
           ;; or be guaranteed smaller than all of the polymorphs
           (cl-loop for poly in (et:type-case->polymorphs super)
                    collect
                    (if (memq poly (et:type-case->polymorphs sub)) (et:match-succeeded nil)
                      ;; Look for a lower bound on the polymorph
                      ;; which guarantees that it will be larger than sub,
                      ;; or that guarantees that it will NOT be larger than sub
                      (cl-loop for (op lower-bound) in (et:type-polymorph-constraints poly)
                               ;; The polymorph is DEFINITELY NOT larger than sub
                               when (and (eq op 'P:NGEQ)
                                         ;; (LB !<= poly) & (LB <= sub) implies (sub !<= poly)
                                         (et-subtype? lower-bound (et-type sub)))
                               return (et:match-failed)
                               ;; The polymorph is larger than sub if the subtype succeeds
                               for or-result = (when (eq op 'P:GEQ)
                                                 (et:algebra--subtype-1 (et-type sub) lower-bound))
                               when or-result collect or-result into or-results
                               finally return
                               ;; Based on what the polymorph ends up resolving to,
                               ;; sub MAY OR MAY NOT be smaller than the polymorph
                               (let* ((res (et:match-succeeded
                                            (list (list 'I:GEQ poly (et-type sub))))))
                                 (et:algebra--subtype-result-or (cons res or-results)))))
                    into results
                    finally return (apply #'et:match-result-and results))

           ;; Macro expansion in `et:algebra--subtype-0' means that the value should always be a datatype
           (et:algebra--sub-dt (et:type-case->value sub) (et:type-case->value super))
           (et:algebra--sub-binds (et:type-case->binds sub) (et:type-case->binds super))))
         (success (et:match-result->success result))
         (indes (et:match-result->value result))
         (sub-polys (et:type-case->polymorphs sub)))

    (if (or (and success (null indes)) (null sub-polys)) result
      ;; If the above failed, and there are polymorphs in the subtype,
      ;; our last hope is having subtype be conditional on those polymorphs
      (cl-loop for poly in sub-polys
               ;; There is a constraint that super CANNOT be larger than this polymorph.
               when (cl-loop for (op upper-bound) in (et:type-polymorph-constraints poly)
                             ;; (poly !<= UB) & (super <= UB) implies (poly !<= super)
                             thereis (and (eq op 'P:NLEQ) (et-subtype? (et-type super) upper-bound)))
               return (et:match-failed)
               collect (list 'I:LEQ poly (et-type super)) into new-indes
               finally return (et:match-succeeded (append new-indes indes))))))


;;;; Simplify

(et-defun et:algebra-simplify-type (type: EtType) EtType
  (cl-loop for (case . rest) on (et:type->cases type)
           ;; Check if this case is redundant
           unless (cl-loop for c in (append new-cases rest)
                           ;; case is a subtype of c, so case is redundant
                           thereis
                           (and (et-subtype? (et:type-new :cases (list case))
                                             (et:type-new :cases (list c)))))

           collect case into new-cases
           finally return (et:type-new :label (et:type->label type) :cases new-cases)))


;;;; Intersection

;; `et-subsect' generates a type which is a subset of ALL of the
;; provided types. In other words, it will approximate a smaller type
;; (i.e. never)

;; Thus, if v is (et-subsect A B), then v is both A and B, but but v
;; could be A and B but NOT (et-subsect A B).

;; This makes `et-subsect' useful for satisfying a list of subtype
;; constraints.

;; `et-supersect' generates a type which is a SUPERSET of the
;; intersection of the provided types. It will approximate by choosing
;; one of the types to use as the innacurate intersection.

;; Thus, if v is both A and B, then it is (et-supersect A B), but v
;; could be (et-supersect A B) but NOT be A or NOT be B.

;; This makes `et-supersect' useful for type narrowing, because unlike
;; `et-subsect', it will never make the type TOO narrow.

;; For example,
;; (et-subsect Integer Positive) -> Never
;; (et-supersect Integer Positive) -> Integer

(et-defun et-subsect (&rest types: &List<EtType>) EtType
  (apply #'et:algebra--intersect t types))

(et-defun et-supersect (&rest types: &List<EtType>) EtType
  (apply #'et:algebra--intersect nil types))

(et-defun et-non-nil-of (type: EtType) EtType
  (et-supersect type (et NonNil)))

(et-defun et-nil-of (type: EtType) EtType
  (et-supersect type (et Nil)))

(et-defun et:algebra--intersect (subsect?: Boolean &rest types: &List<EtType>) EtType
  "Return the type intersection of TYPES."
  (pcase types
    ('nil (et-any))
    (`(,only) only)
    (`(,a ,b ,c . ,rest) (et:algebra--intersect subsect? a (apply #'et:algebra--intersect subsect? b c rest)))
    (`(,a ,b)
     (cl-loop for a-case in (et:type->cases a)
              nconc
              (cl-loop for b-case in (et:type->cases b)
                       nconc (et:algebra--intersect-cases subsect? a-case b-case))
              into all-cases
              finally return
              (let ((result (et:type-new :cases all-cases
                                         :label (or (et:type->label a) (et:type->label b)))))
                ;; This assertion should pass if there are no bugs
                (when (and et-debug subsect?)
                  (or (and (et-subtype? result a) (et-subtype? result b))
                      (error "`et-subsect' determined incorrect intersection")))
                result)))))

(et-defun et:algebra--intersect-binds (subsect?: Boolean a-binds: EtBinds b-binds: EtBinds) EtBinds
  "Create a list of binds which are a subtype of both A-BINDS and B-BINDS."
  (cl-loop for (var . binds) in (seq-group-by #'car (append a-binds b-binds))
           collect (cons var (apply #'et:algebra--intersect subsect? (mapcar #'cdr binds)))))

(et-defun et:algebra--case-expand-alias (case: *et:type-case alias: *et:type-alias) List<*et:type-case>
  (cl-loop for exp-case in (et:type->cases (et:type-alias-expand alias))
           collect
           (et:type-case-new
            :value (et:type-case->value exp-case)
            :binds (append (et:type-case->binds case)
                           (et:type-case->binds exp-case))
            :typeofs (append (et:type-case->typeofs case)
                             (et:type-case->typeofs exp-case))
            :polymorphs (append (et:type-case->polymorphs case)
                                (et:type-case->polymorphs exp-case)))))

(et-defun et:algebra--intersect-cases (subsect?: Boolean a-case: *et:type-case b-case: *et:type-case)
          List<*et:type-case>
  "Return a list of cases resulting from intersecting A-CASE and B-CASE."
  (let* ((a (et:type-case->value a-case))
         (b (et:type-case->value b-case))
         (a-val-type (et-type (et:type-case-new :value a)))
         (b-val-type (et-type (et:type-case-new :value b)))
         ;; Check if one of the values is a subtype of the other
         (sub-val (cond ((et-subtype? a-val-type b-val-type) a)
                        ((et-subtype? b-val-type a-val-type) b)))
         (make-case
          (lambda (val)
            (et:type-case-new
             :value val
             :binds (et:algebra--intersect-binds
                     subsect?
                     (et:type-case->binds a-case) (et:type-case->binds b-case))
             :typeofs (seq-uniq (append (et:type-case->typeofs a-case) (et:type-case->typeofs b-case)) #'eq)
             :polymorphs (seq-uniq (append (et:type-case->polymorphs a-case) (et:type-case->polymorphs b-case)))))))

    (cond
     (sub-val (list (funcall make-case sub-val)))

     ((et:type-alias-p a)
      (cl-loop for exp-case in (et:algebra--case-expand-alias a-case a)
               nconc (et:algebra--intersect-cases subsect? exp-case b-case)))
     ((et:type-alias-p b)
      (cl-loop for exp-case in (et:algebra--case-expand-alias b-case b)
               nconc (et:algebra--intersect-cases subsect? a-case exp-case)))

     ((and (et:type-dt-p a) (et:type-dt-p b))
      (let ((dt (et:algebra--intersect-datatypes subsect? a b)))
        (if (not (eq dt 'INVALID)) (list (funcall make-case dt))
          ;; This is where subsect and supersect differ
          (if subsect? nil
            (if (et:dt-might-overlap-nontrivial? a b)
                (list (funcall make-case a))
              nil)))))

     (t (error "Invalid type values")))))

(et-defun et:algebra--intersect-datatypes (subsect?: Boolean a: *et:type-dt b: *et:type-dt)
          (or *et:type-dt @INVALID)
  "Returns the datatype resulting from intersecting A and B, or `INVALID'."
  (let* ((a-name (et:type-dt->name a))
         (b-name (et:type-dt->name b))
         (a-args (et:type-dt->args a))
         (b-args (et:type-dt->args b)))
    (cond
     ((et:algebra-sub-dt? a b) a)
     ((et:algebra-sub-dt? b a) b)

     ((eq a-name b-name)
      (let ((arg-intersection
             (et:dt-intersect-args-nontrivial
              a-name a-args b-args
              (lambda (a b) (et:algebra--intersect subsect? a b)) #'et-union)))

        (if (eq arg-intersection 'INVALID) 'INVALID
          (et:type-dt-new :name a-name :args arg-intersection))))

     (t 'INVALID))))


;;;; Subtract

(et-defun et-subtract (a: EtType b: EtType) EtType
  "Subtract type A from B.

This function errs on the side of subtracting less. In other words,
returning A itself is a valid approximation."
  (cl-loop for a-case in (et:type->cases a)
           nconc
           (cl-loop for b-case in (et:type->cases b)
                    nconc (et:algebra--subtract-cases a-case b-case))
           into all-cases
           finally return
           (let ((result (et:type-new :cases all-cases)))
             result)))

(et-defun et:algebra--subtract-binds (a-binds: EtBinds b-binds: EtBinds) EtBinds
  (cl-loop for (var . a-type) in a-binds
           for b-type = (alist-get var b-binds)
           collect (cons var (if b-type (et-subtract a-type b-type) a-type))))

(et-defun et:algebra--subtract-cases (a-case: *et:type-case b-case: *et:type-case) List<*et:type-case>
  (let* ((a (et:type-case->value a-case))
         (b (et:type-case->value b-case))
         (a-val-type (et-type (et:type-case-new :value a)))
         (b-val-type (et-type (et:type-case-new :value b)))
         (make-case
          (lambda (val)
            (et:type-case-new
             :value val
             :binds (et:algebra--subtract-binds (et:type-case->binds a-case) (et:type-case->binds b-case))
             :typeofs (et:type-case->typeofs a-case)
             :polymorphs (et:type-case->polymorphs a-case)))))

    (cond
     ;; Subtracting gives never
     ((et-subtype? a-val-type b-val-type) nil)

     ((et:type-alias-p a) (cl-loop for exp-case in (et:algebra--case-expand-alias a-case a)
                                   nconc (et:algebra--subtract-cases exp-case b-case)))
     ((et:type-alias-p b) (cl-loop for exp-case in (et:algebra--case-expand-alias b-case b)
                                   nconc (et:algebra--subtract-cases a-case exp-case)))

     ;; Todo: Handle more complex cases
     (t (list (funcall make-case a))))))


;;;; Recursively modify type

(defun et:algebra--transform-type (type type-fn case-fn)
  (et-declare (type EtType)
              (type-fn (or Nil (fn (args EtType &List<*et:type-case>) EtType)))
              (case-fn (or Nil (fn (args EtType *et:type-case *et:type-alias|*et:type-dt) *et:type-case)))
              (@return EtType))

  (let* ((sub-fn (lambda (sub) (et:algebra--transform-type sub type-fn case-fn)))
         (map-case-fn
          (lambda (case)
            (let* ((val (pcase (et:type-case->value case)
                          ((cl-struct et:type-alias name args)
                           (et:type-alias-new :name name :args (mapcar sub-fn args)))
                          ((cl-struct et:type-dt name args)
                           (let* ((new-args (et:dt-map-type-args name args sub-fn)))
                             (et:type-dt-new :name name :args new-args)))
                          (v (error "Invalid case val: %s" v)))))
              (if case-fn (funcall case-fn type case val)
                (et-copy-with case :value val)))))
         (new-cases (mapcar map-case-fn (et:type->cases type))))
    (if type-fn (funcall type-fn type new-cases)
      (et-copy-with type :cases new-cases))))


;;;; Binds utils

(et-defun et-add-typeof (type: EtType var: EtVar) EtType
  (et-supersect
   type
   (et-type (et:type-case-new :value (et:type-dt-new :name 'Any)
                              :typeofs (list var)))))

(et-defun et-remove-type-binds-and-polys (type: EtType polys: &List<EtGeneric>) EtType
  (et:algebra--transform-type
   type nil
   (lambda (_ case value)
     (et-copy-with case :value value :binds nil :typeofs nil
                   :polymorphs
                   (cl-remove-if (lambda (x) (memq x polys))
                                 (et:type-case->polymorphs case))))))

(et-defun et:algebra-remove-binds (type: EtType) EtType
  "Recursively remove bindings/typeofs from TYPE."
  (et:algebra--transform-type
   type nil
   (lambda (_ case value)
     (et-copy-with case :value value :binds nil :typeofs nil))))

(et-defun et-type-binds (type: EtType) EtBinds
  ;; binds is an alist of `et-var' to a list of types (which will be `et-union'ed)
  (let* ((binds-alist nil))
    (dolist (case (et:type->cases type))
      (let* ((binds (et:type-case->binds case))
             (typeofs (et:type-case->typeofs case)))
        (dolist (var (delete-dups (append (mapcar #'car binds) typeofs nil)))
          (let* ((b-type (alist-get var binds))
                 (ti-type (when (memq var typeofs)
                            (et:algebra-remove-binds (et-type case)))))
            (push (if (and b-type ti-type) (et-supersect b-type ti-type)
                    (or b-type ti-type))
                  (alist-get var binds-alist))))))
    (cl-loop for (var . types) in (nreverse binds-alist)
             collect (cons var (apply #'et-union (nreverse types))))))

(et-defun et:algebra-replace-binds (type: EtType binds: EtBinds) EtType
  (et:algebra--transform-type
   type nil
   (lambda (ty case value)
     (et-copy-with case :value value :binds (when (eq ty type) binds) :typeofs nil))))

(et-defun et:algebra-when (cond: EtType type: EtType) EtType
  (if (et-never-p cond) cond
    (let* ((binds (et-type-binds cond)))
      (if (cl-loop for (_ . type) in binds thereis (et-never-p type))
          (et-never)
        (et:algebra-replace-binds type binds)))))


;;;; Label utils

(et-defun et-remove-type-label (type: EtType) EtType
  (cl-assert (et:type-p type))
  (et-copy-with type :label nil))

(et-defun et:algebra--remove-labels (type: EtType) EtType
  "Recursively remove labels from TYPE."
  (cl-assert (et:type-p type))

  (cl-loop for case in (et:type->cases type)
           for val = (et:type-case->value case)
           collect
           (et-copy-with
            case
            :value
            (pcase val
              ((cl-struct et:type-alias name args)
               (et:type-alias-new :name name :args (mapcar #'et:algebra--remove-labels args)))
              ((cl-struct et:type-dt name args)
               (let ((new-args (et:dt-map-type-args name args #'et:algebra--remove-labels)))
                 (et:type-dt-new :name name :args new-args)))
              (_ (error "Invalid case val: %s" val))))
           into cases
           finally return (et:type-new :cases cases)))


;;;; Infer

(defmacro et-infer (type genvec matcher-spec output-spec)
  `(let* ((matcher ,(et-parse-matcher matcher-spec genvec))
          (result (et:algebra-infer matcher ,type ,(et-parse-repr output-spec (et-genvec-generics genvec)))))
     (when (et:match-result->success result)
       (et:match-result->value result))))

(defun et:algebra-infer (matcher type output-repr &optional extra-repls)
  "Infer TYPE against MATCHER, then convert OUTPUT-REPR to a type.

Specifically, this will first call `et-sub-match' on MATCHER and TYPE
to determine values for the generics defined in MATCHER. Then, if this
succeeds, it will convert OUTPUT-REPR to a type, replacing each generic
with the value determined by `et-sub-match', as well as EXTRA-REPLS if
provided.

This returns an `et-match-result' in case matching fails."
  (declare (et (matcher *et:match-matcher) (type EtType) (output-repr EtRepr)
               (extra-repls Alist<EtGeneric~EtType>)
               (@return *et:match-result<EtType>)))

  (let* ((gens-result (et-sub-match matcher type)))
    (if (not (et:match-result->success gens-result)) gens-result

      (cl-loop for gen in (et:match-matcher->generics matcher)
               for gen-type in (et:match-result->value gens-result)
               collect (cons gen gen-type) into new-repls
               finally return
               (et:match-result-new
                :success t :value
                (et-repr-to-type
                 output-repr
                 (nconc new-repls extra-repls)))))))


;;;; Funcall

(et-defun et-funcall (func-type: EtType arglist-type: EtType) EtType
  (let* ((result (et:algebra-funcall func-type arglist-type)))
    (when (et:match-result->success result)
      (et:match-result->value result))))

(et-defun et:algebra-funcall (func-type: EtType arglist-type: EtType) *et:match-result<EtType>
  "Determine the return type of calling FUNC-TYPE with ARGLIST-TYPE."
  (setq func-type (et-expand-aliases func-type))

  (cl-loop for case in (et:type->cases func-type)
           for val = (et:type-case->value case)
           for result =
           (pcase val
             ((cl-struct et:type-dt (name 'Function) (args `(,param-type ,return-type)))
              (let* ((matcher (et:match-matcher-new :repr (et-type-to-repr param-type)))
                     (result (et-sub-match matcher arglist-type)))
                (if (et:match-result->success result)
                    (et:match-succeeded return-type)
                  result)))
             ((cl-struct et:type-dt (name 'DynFunction) (args `(,matcher ,output-repr)))
              (et:algebra-infer matcher arglist-type output-repr))
             (_ (et:match-failed)))
           unless (et:match-result->success result) return result
           collect (et:match-result->value result) into types
           finally return
           (et:match-result-new :success t :value (apply #'et-supersect types))))


;;;; Tuple

(et-defun et-tuple (cons: Symbol types: &List<EtType>) EtType
  (if (null types) (et-literal nil)
    (et-alias cons (car types) (et-tuple cons (cdr types)))))

(et-defun et-tailed-tuple (cons: Symbol types: &List<EtType>) EtType
  (pcase types
    (`(,last) last)
    (`(,next . ,rest) (et-alias cons next (et-tailed-tuple cons rest)))
    (_ (error "No tail provided"))))


;;;; Freshen/unfreshen
;;;;; Inner transform

(defvar et:algebra--rec-transform-stack nil)

(defvar et:algebra--rec-transform-datatypes-loops nil
  "Loop aliases created by `et:algebra--rec-transform-datatypes-inner'.
A list of (TEMP-SYMBOL . DEFINITION), newest first. Each entry is a
recursive (\"loop\") alias the inner pass generated, named with a
deterministic but throwaway uninterned TEMP-SYMBOL. The wrapper
`et:algebra--rec-transform-datatypes' rewrites these into stable, interned,
structure-derived names before returning.")

(defun et:algebra--rec-transform-datatypes-inner (type transform)
  "Recursively transform datatypes in a type.

TRANSFORM is a function which takes (dt-name dt-args) and returns a new
`et-datatype' or `et-alias' (type-case value).

Recursive (\"loop\") types are broken with a placeholder alias whose
name is deterministic within the enclosing
`et:algebra--rec-transform-datatypes' call (\"Loop0\", \"Loop1\", ...),
recorded on `et:algebra--rec-transform-datatypes-loops'. These temporary
uninterned names are rewritten by the wrapper."
  (et-stop-recursion et:algebra--rec-transform-stack (list type)
                     (let* ((idx (length et:algebra--rec-transform-datatypes-loops))
                            (sym (make-symbol (format "Loop%d" idx))))
                       (push (cons sym nil) et:algebra--rec-transform-datatypes-loops)
                       (et-type (et:type-alias-new :name sym :args nil)))

    (cl-loop for case in (et:type->cases type)
             for val = (et:type-case->value case)
             nconc
             (pcase val
               ((cl-struct et:type-alias)
                (let* ((binds (et:type-case->binds case))
                       (typeofs (et:type-case->typeofs case))
                       (polys (et:type-case->polymorphs case))
                       (expanded (et:type-alias-expand val))
                       (expanded-transformed (et:algebra--rec-transform-datatypes-inner expanded transform)))
                  (if (equal expanded expanded-transformed) (list case)
                    (if (not (or binds typeofs polys)) (et:type->cases expanded-transformed)
                      ;; Add the binds from TYPE
                      (cl-loop for exp-case in (et:type->cases expanded-transformed)
                               collect (et-copy-with case :value (et:type-case->value exp-case)))))))

               ((cl-struct et:type-dt name args)
                (list (et-copy-with case :value (funcall transform name args)))))
             into new-cases
             finally return
             (let* ((type (et:type-new :label (et:type->label type) :cases new-cases))
                    (no-binds (et:algebra-remove-binds type))
                    (alias-type (cdar et:algebra--rec-transform-stack)))
               (if (not (et-stop-recursion-unset? alias-type))
                   (let* ((alias (et:type-case->value (car (et:type->cases alias-type))))
                          (sym (et:type-alias->name alias)))
                     (setcdr (assq sym et:algebra--rec-transform-datatypes-loops) no-binds)
                     (et:type-defalias sym [] no-binds nil)
                     alias-type)
                 type)))))


;;;;; Generalized substitution

;; A single pass that rewrites loop-alias references throughout a type.
;; SUBS is an alist mapping each loop alias's (no-arg) NAME to a
;; replacement `et-type'.  This generalizes the older rename-only pass:
;; a rename is just the special case where the replacement is the loop
;; alias under a new name, while collapsing a loop to a canonical alias
;; (e.g. List<Number>) supplies an arbitrary type.  Subtrees containing
;; no substituted alias are returned unchanged, so unaffected structure
;; is shared rather than copied.
;;
;; Replacements are spliced in verbatim, not re-traversed: the caller is
;; responsible for ensuring they contain no further substitutable loop
;; references (see `et:algebra--rec-transform-datatypes').

(defun et:algebra--rec-subst (type subs)
  "Return TYPE with loop aliases replaced through SUBS.
SUBS is an alist of (NAME . REPLACEMENT-TYPE)."
  (let* ((changed nil)
         (new-cases
          (cl-loop for case in (et:type->cases type)
                   for val = (et:type-case->value case)
                   nconc
                   (pcase val
                     ;; A loop reference: splice in its replacement cases.
                     ((and (cl-struct et:type-alias (name name) (args args))
                           (guard (null args))
                           (let repl (assq name subs))
                           (guard repl))
                      (setq changed t)
                      (copy-sequence (et:type->cases (cdr repl))))
                     (_
                      (let ((new-val (et:algebra--rec-subst-value val subs)))
                        (if (eq new-val val) (list case)
                          (setq changed t)
                          (list (et-copy-with case :value new-val)))))))))
    (if (not changed) type
      (et:type-new :label (et:type->label type) :cases new-cases))))

(defun et:algebra--rec-subst-value (val subs)
  "Substitute loop references nested in type-case VALUE's arguments."
  (pcase val
    ((cl-struct et:type-alias name args)
     (let ((new-args (et:algebra--rec-subst-list args subs)))
       (if (eq new-args args) val (et:type-alias-new :name name :args new-args))))
    ((cl-struct et:type-dt name args)
     (let ((new-args (et:algebra--rec-subst-dt-args name args subs)))
       (if (eq new-args args) val (et:type-dt-new :name name :args new-args))))
    (_ (error "Invalid case val: %s" val))))

(defun et:algebra--rec-subst-list (types subs)
  "Substitute loop references across TYPES; return TYPES itself if unchanged."
  (let ((new (mapcar (lambda (ty) (et:algebra--rec-subst ty subs)) types)))
    (if (cl-every #'eq new types) types new)))

(defun et:algebra--rec-subst-dt-args (name args subs)
  "Substitute loop references in datatype ARGS, skipping CONST args.
Return ARGS itself when nothing changed."
  (let ((new (cl-loop for arg in args
                      for role in (et:dt-arg-roles name args)
                      collect (if (eq role 'CONST) arg
                                (et:algebra--rec-subst arg subs)))))
    (if (cl-every #'eq new args) args new)))

(defun et:algebra--type-mentions-alias? (type names)
  "Non-nil if TYPE references any alias whose name is in NAMES."
  (cl-loop for case in (et:type->cases type)
           thereis
           (pcase (et:type-case->value case)
             ((cl-struct et:type-alias name args)
              (or (memq name names)
                  (cl-some (lambda (a) (et:algebra--type-mentions-alias? a names)) args)))
             ((cl-struct et:type-dt args)
              (cl-some (lambda (a) (and (et:type-p a) (et:algebra--type-mentions-alias? a names)))
                       args)))))


;;;;; Transform datatypes

(defvar et:algebra--rec-transform-active nil
  "Non-nil while inside the outermost `et:algebra--rec-transform-datatypes' call.
Used to share a single loop scope across nested calls (see below).")

(defun et:algebra--rec-canonical-collapses (loops canonical-loops)
  "Return an alist of (SYM . REPLACEMENT-TYPE) for collapsible LOOPS.

CANONICAL-LOOPS is a list of (MATCHER . REPR) pairs.  A loop collapses
when its definition `et-iso-match'es some MATCHER and, after substituting
the loops already chosen for collapse, the matched arguments mention no
remaining loop alias.  The replacement is then REPR with those arguments
substituted for its generics -- so the loop generated for, e.g.,
`ListFresh<Number>' collapses to the readable `List<Number>'.

Requiring loop-free arguments keeps replacements closed: a loop whose
canonical form would still reference a loop (including itself, as with a
genuinely non-list recursive type) is left for the digest-based renaming
instead."
  (when canonical-loops
    (let* ((loop-syms (mapcar #'car loops))
           ;; Pre-match each loop once: (SYM GENERICS REPR ARG-TYPES).
           (matched
            (cl-loop for (sym . def) in loops
                     for hit =
                     (cl-loop for (matcher . repr) in canonical-loops
                              for res = (et-iso-match matcher def)
                              when (et:match-result->success res)
                              return (list (et:match-matcher->generics matcher)
                                           repr (et:match-result->value res)))
                     when hit collect (cons sym hit)))
           (collapsed nil)
           (progress t))
      ;; Fixpoint: a loop becomes collapsible once every loop its
      ;; arguments reference has itself collapsed to a loop-free type.
      (while progress
        (setq progress nil)
        (cl-loop for (sym generics repr arg-types) in matched
                 unless (assq sym collapsed)
                 do (let ((resolved (mapcar (lambda (ty) (et:algebra--rec-subst ty collapsed)) arg-types)))
                      (unless (cl-some (lambda (ty) (et:algebra--type-mentions-alias? ty loop-syms)) resolved)
                        (push (cons sym (et-repr-to-type repr (cl-mapcar #'cons generics resolved)))
                              collapsed)
                        (setq progress t)))))
      collapsed)))

(defun et:algebra--rec-transform-datatypes (type transform &optional canonical-loops)
  "Transform datatypes in TYPE, giving loop aliases stable interned names.

Wraps `et:algebra--rec-transform-datatypes-inner'. The inner pass names each
recursive (\"loop\") alias deterministically but with throwaway
uninterned symbols.

CANONICAL-LOOPS, when given, is a list of (MATCHER . REPR) pairs: any
generated loop equivalent to MATCHER is collapsed to REPR with the
matched generics substituted in (see `et:algebra--rec-canonical-collapses'),
which keeps unfreshened types readable (e.g. `List<Number>' rather than
an anonymous loop).  Collapsed loops are dropped entirely; their
references are replaced by the closed canonical type.

Every loop that is not collapsed has one structural digest derived from
all such definitions, and is rewritten to an interned, digest-based name
(`@Loop<DIGEST>-<INDEX>'). Interning lets these names survive a
serialized-cache round-trip with `eq' identity; the structural digest
makes structurally identical loops reuse the same name across sessions.

Only the outermost call establishes the loop scope and performs the
collapsing/renaming.  A TRANSFORM may recurse back in (e.g.
`et:algebra-unfreshen-type' unfreshening a sub-argument), and
`et:algebra--rec-transform-stack' spans those nested calls -- so a loop detected
in a nested call belongs to an outer frame.  Nested calls therefore
delegate straight to the inner pass, sharing the outermost call's loop
scope, so each placeholder and its definition always land together.

The digest is intentionally shallow: it hashes the stringified loop
definitions without expanding the aliases they reference. That is safe
because the call cache also keys on full structure, so a changed inner
alias that the digest fails to distinguish is still caught by the cache's
structural key -- a stale result is never served, only a cache miss."
  (if et:algebra--rec-transform-active
      (et:algebra--rec-transform-datatypes-inner type transform)
    (let* ((et:algebra--rec-transform-active t)
           (et:algebra--rec-transform-datatypes-loops nil)
           (result (et:algebra--rec-transform-datatypes-inner type transform))
           (loops (nreverse et:algebra--rec-transform-datatypes-loops)))
      (if (null loops) result
        (let* ((collapsed (et:algebra--rec-canonical-collapses loops canonical-loops))
               (kept (cl-remove-if (lambda (l) (assq (car l) collapsed)) loops)))
          (if (null kept)
              ;; Every loop collapsed to a canonical alias.
              (et:algebra--rec-subst result collapsed)
            (let* ((digest (secure-hash 'md5 (let ((print-level nil) (print-length nil))
                                               (prin1-to-string (mapcar #'cdr kept)))))
                   (renames (cl-loop for (sym . _def) in kept
                                     for idx upfrom 0
                                     collect (cons sym (intern (format "@Loop%s-%d" digest idx)))))
                   ;; The full rewrite: collapsed loops -> canonical type,
                   ;; kept loops -> their stable interned name.  The
                   ;; renamed alias is built directly, since `@Loop...'
                   ;; names do not satisfy the `et-alias' constructor.
                   (subs (append collapsed
                                 (cl-loop for (sym . new-name) in renames
                                          collect (cons sym (et:type-new
                                                             :cases (list (et:type-case-new
                                                                           :value (et:type-alias-new :name new-name)))))))))
              ;; Redefine each kept loop alias under its stable name.
              (cl-loop for (old . new) in renames
                       for new-def = (et:algebra--rec-subst (alist-get old kept) subs)
                       do (et:type-defalias new [] new-def nil)
                       do (put new 'et-loop-alias new-def))
              (et:algebra--rec-subst result subs))))))))


;;;;; Freshen/unfreshen

(defvar et:algebra--unfreshen-canonical-loops nil
  "Cached canonical (MATCHER . REPR) collapses for `et:algebra-unfreshen-type'.
Loops produced while unfreshening that match MATCHER are rewritten to
REPR with the matched generics substituted in, so the loop generated for
`ListFresh<Number>' collapses to the readable `List<Number>'.  Built
lazily because `List' is not yet defined when this file loads.")

(defun et:algebra--unfreshen-canonical-loops ()
  "Return the canonical loop collapses used when unfreshening."
  (or et:algebra--unfreshen-canonical-loops
      (setq et:algebra--unfreshen-canonical-loops
            (list (cons (et-matcher [E] List<E>)
                        (et-parse-repr 'List<E> '(E)))))))

(et-defun et:algebra-unfreshen-type (type: EtType) EtType
  (et:algebra-remove-binds
   (et:algebra--rec-transform-datatypes
    type
    (lambda (name args)
      (pcase name
        ('ConsFresh (et:type-alias-new :name 'Cons :args (mapcar #'et:algebra-unfreshen-type args)))
        ('VectorFresh (et:type-alias-new :name 'Vector :args (mapcar #'et:algebra-unfreshen-type args)))
        (_ (et:type-dt-new :name name :args args))))
    (et:algebra--unfreshen-canonical-loops))))

(et-defun et:algebra-freshen-type (type: EtType) EtType
  (et:algebra-remove-binds
   (et:algebra--rec-transform-datatypes
    type
    (lambda (name args)
      (pcase name
        ('ConsFull
         (let* ((new-args (list (et:algebra-freshen-type (car args)) (et:algebra-freshen-type (caddr args)))))
           (et:type-dt-new :name 'ConsFresh :args new-args)))
        ('VectorFull (et:type-alias-new :name 'VectorFresh :args (list (et:algebra-freshen-type (car args)))))
        (_ (et:type-dt-new :name name :args args)))))))

(et-defun et:algebra-freshen-type-shallow (type: EtType) EtType
  (et:algebra-remove-binds
   (et:algebra--rec-transform-datatypes
    type
    (lambda (name args)
      (pcase name
        ('ConsFull
         (let* ((new-args (list (car args) (et:algebra-freshen-type-shallow (caddr args)))))
           (et:type-dt-new :name 'ConsFresh :args new-args)))
        ('VectorFull
         (et:type-dt-new :name 'VectorFresh :args (list (car args))))

        (_ (et:type-dt-new :name name :args args)))))))


;;;; Deep expand aliases

(et-defun et-expand-aliases-at-depth (type: EtType depth: Integer) EtType
  (if (<= depth 0) type
    (cl-loop for case in (et:type->cases (et-expand-aliases type))
             for dt = (et:type-case->value case)
             for new-dt =
             (et:type-dt-new
              :name (et:type-dt->name dt)
              :args (et:dt-map-type-args
                     (et:type-dt->name dt) (et:type-dt->args dt)
                     (lambda (type) (et-expand-aliases-at-depth type (1- depth)))))
             collect (et-copy-with case :value new-dt) into new-cases
             finally return (et:type-new :label (et:type->label type) :cases new-cases))))


;;;; Public functions

(et-defun et-reify-type (type: EtType) EtType
  (et:algebra-unfreshen-type (et-remove-type-binds-and-polys type nil)))


;;; ============================================================
;;; Define aliases
;;;; Basic aliases

(et-defalias Boolean [] (or Nil True))

;; All functions are a subtype of AnyFn
(et-defalias AnyFn [] (Function Never Any))
(et-defalias IdFn [T] (Function T T))

(et-defalias Vector [(= E Any)] VectorFull<E~E>)
(et-defalias WriteVector [(= E Any)] VectorFull<Any~E>)

(et-defalias Indirect [T] T)


;;;; Cons aliases

(et-defalias Cons [(= L Any) (= R L)] (ConsFull L L R R))
(et-defalias WriteCons [(= L Any) (= R L)] (ConsFull Never L Never R))

(et-defalias ListFresh [(= E Any)] (or Nil (ConsFresh E (ListFresh E))))
(et-defalias List [(= E Any)] (or Nil (Cons E (List E))))

(et-defalias Tree [(= E Any)] (or (List (Tree E)) E))
(et-defalias ConsTree [(= E Any)] (or (Cons (ConsTree E) (ConsTree E)) E))

(et-defalias Alist [K V] (List (Cons K V)))

(et-defalias PlistOf [K V] (or Nil (Cons K (Cons V (PlistOf K V)))))

(et-defspec Tuple (&rest args) (et:type-tuple-spec 'Cons args))
(et-defspec Args (&rest args) (et:type-tuple-spec '&Cons args))
(et-defspec Tuple* (&rest args) (et:type-tuple-star-spec 'Cons args))
(et-defspec Args* (&rest args) (et:type-tuple-star-spec '&Cons args))

(et-defalias Sexp []
  (or Symbol String Number
      (Cons Sexp Sexp)
      (Vector Sexp)))
(et-defalias Sexps [] List<Sexp>)


;;;; Emacs aliases

(defvar et-aliased-emacs-types
  '(buffer marker window frame window-configuration overlay
           terminal process
           thread mutex condition-variable
           font-spec font-entity font-object
           xwidget xwidget-view
           char-table bool-vector obarray
           finalizer
           interpreted-function byte-code-function subr))

(dolist (sym et-aliased-emacs-types)
  (let* ((alias (string-replace "-" "" (capitalize (format "%s" sym)))))
    (et:type-defalias (intern alias) [] `(Emacs ,sym) nil)))

(et-defalias Closure [] (or InterpretedFunction ByteCodeFunction))
(et-defalias Font [] (or FontSpec FontEntity FontObject))
(et-defalias IntOrMarker [] (or Integer Marker))
(et-defalias NumOrMarker [] (or Number Marker))


;;; ============================================================
;;; Provide

(provide 'et)


;;; et.el ends here
