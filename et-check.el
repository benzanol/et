;;; et-check.el --- Type checking for et.el -*- lexical-binding: t; -*-

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

(require 'et)
(require 'et-macros)
(require 'seq)


;;; ============================================================
;;; Type declarations

(et-declare
 (@alias EtNarrows AList<*et:type-var~*et:type>)
 ;; True = keep the current recommendation
 (@alias EtRec Nil|True|*et:type)
 (@alias EtIdentifyPlist (KVPList @:constrain|@:populate|@:declare Nil|fn))
 (@alias EtIdentifyFn (fn Nil EtIdentifyPlist))

 (@alias EtChecked Nil|*et:type)
 (@alias EtCheckFn (fn Nil *et:type|Nil))

 (@alias EtCheckable
         (or *et:type EtCheckFn
             ;; car=t => then use the current recommendation
             ;; car=* => check the tail AND use the current recommendation
             (ConsR Symbol ListR<Any>)))

 (@symbol-property et-identify EtIdentifyFn)
 (@symbol-property et-checker EtCheckFn)
 (@symbol-property et-check-macro fn<Any~*et:type>))

(et-declare
 (@alias EtFuncParameters [T]
         (Tuple ListR<T> ListR<T> ListR<T> Nil|TupleR<T>))
 (@alias EtFuncDeclarations
         (PList :parameters EtFuncParameters<Var>
                :props KVPList<Var~Any>
                ;; Either a function type or a custom checker
                :definition (or Nil *et:type (fn Nil *et:type))))

 (@symbol-property et-symbol-func-type *et:type)
 (@symbol-property et-function-parameters EtFuncParameters<Var>)
 (@symbol-property et-function-props KVPList<Var~Any>))


;;; ============================================================
;;; Symbol properties
;;;; Function type

(et-defun et-symbol-func-type (name: Var) *et:type|Nil
  (when (get name 'et-deferred-declare)
    (ignore (et-result-boundary (funcall (get name 'et-deferred-declare))))
    (put name 'et-deferred-declare nil))

  (get name 'et-symbol-func-type))

(et-defun et-symbol-func-props (name: Var) Nil|KVPList<Any~Any>
  (get name 'et-function-props))

(et-defun et-symbol-func-params (name: Var) Nil|KVPList<Any~Any>
  (get name 'et-function-parameters))

(et-defun et-symbol-set-func-decls (name: Var decls: EtFuncDeclarations) Nil
  "Assign relevant symbol properties to FUNC."
  (put name 'et-function-props (plist-get decls :props))
  (put name 'et-function-parameters (plist-get decls :parameters))
  (pcase (plist-get decls :definition)
    ((and type (pred et:type-p)) (put name 'et-symbol-func-type type))
    ((and chk (pred functionp)) (put name 'et-checker chk))))


;;;; Variable type

(et-defun et-set-global-var-type (name: Var type: *et:type) Nil
  (put name 'et-variable-type type)
  (put name 'et-variable-var (et:type-var-new :name name :type type)))

(et-defun et-global-var-type (name: Var) *et:type|Nil
  (get name 'et-variable-type))

(et-defun et-global-var (name: Var) *et:type|Nil
  (get name 'et-variable-var))


;;;; Identifier

(et-defvar et:process--identifier-symbols List<Symbol> nil)

(et-defun et-set-identifier (sym: Var fn: EtIdentifyFn) Nil
  (push sym et:process--identifier-symbols)
  (put sym 'et-identify fn))

(et-defun et-symbol-identifier (sym: Var) Nil|EtIdentifyFn
  (get sym 'et-identify))


;;;; Checker

(et-defun et-set-checker (sym: Var fn: fn) Nil
  (put sym 'et-checker fn)
  nil)

(et-defun et-symbol-checker (sym: Var) Nil|EtCheckFn
  (get sym 'et-checker))


;;; ============================================================
;;; Checking - `et:check'
;;;; Symbol bindings

(et-defvar et:check--binds AList<Var~*et:type-var> nil
  "Stack of (SYMBOL . `et:type-var').")

(et-defun et-new-var (name: Var type: *et:type) *et:type
  (et:type-var-new :name name :type type))

(et-defun et-get-symbol-var (sym: Var) *et:type-var|Nil
  (cl-assert (symbolp sym))
  (or (alist-get sym et:check--binds)
      (et-global-var sym)))

(defmacro et-with-vars (vars &rest body)
  (declare (indent 1) (et (@progn ListR<*et:type-var>)))
  (let* ((loop `(cl-loop for var in ,vars
                         do (cl-assert (et:type-var-p var))
                         collect (cons (et:type-var->name var) var))))
    `(let* ((et:check--binds (append ,loop et:check--binds)))
       ,@body)))

(defmacro et-with-binds (binds &rest body)
  "VARS can contain nil values."
  (declare (indent 1) (et (@progn (ListR (or Nil (ConsR Var *et:type))))))
  (let* ((loop `(cl-loop for entry in ,binds
                         when entry collect
                         (let* ((name (car entry)) (type (cdr entry)))
                           (cl-assert (symbolp name)) (cl-assert (et:type-p name))
                           (cons name (et:type-var-new :name name :type type))))))
    `(let* ((et:check--binds (append ,loop et:check--binds)))
       ,@body)))


;;;; Function bindings/checking

(et-defvar et:check--fbinds AList<Symbol~*et:type> nil
  "Stack of (SYMBOL . `et:type').")

(et-defun et-get-fbind (name: Symbol) *et:type|Nil
  (or (alist-get name et:check--fbinds)
      (et-symbol-func-type name)))

(defmacro et-with-fbinds (fbinds &rest body)
  (declare (indent 1) (et (@progn AList<Symbol~*et:type>)))
  `(let* ((et:check--fbinds (append ,fbinds et:check--fbinds)))
     ,@body))

(et-defun et:check--function () EtChecked
  (let* ((func (car (et-cur-expr)))
         (func-type (et-get-fbind func)))

    (let* ((arg-types (cl-loop for pos upfrom 1 below (length (et-cur-expr))
                               for type = (et-check-at pos)
                               collect (et-copy-with type :label (list :position pos))))
           (args-type (et-tuple 'Cons arg-types))
           (output-result (et-funcall func-type args-type)))
      (cond
       ((et:match-result->success output-result) (et:match-result->value output-result))
       ;; If `et--result-failed' is already true, that means one of the arguments was invalid,
       ;; which means the true error was in the arguments, not this call
       ((et-cur-result-failed?) nil)
       ;; The stack in OUTPUT-RESULT is a list of (`sub'/`super' MATCHER TYPE)
       (t
        ;; Find the stack frame corresponding to the position
        (cl-loop
         with (arg-pos arg-type param-name param-repr) = nil
         for (_ mrepr type) in (et:match-result->stack output-result)
         ;; Collect the argument position
         for pos = (plist-get (et:type->label type) :position)
         when pos do (setq arg-pos pos arg-type type)
         ;; Collect the parameter type
         for name = (plist-get (et:repr->label mrepr) :field)
         when name do (setq param-name name param-repr mrepr)
         ;; Display the error message
         finally do
         (if (and arg-type param-repr)
             (et-err arg-pos "[%s] Expected %s, found %s" param-name
                     (et-repr-to-string param-repr) arg-type)
           (pcase (et-type-single func-type)
             ((or (cl-struct et:type-dt (name 'Function) (args `(,input ,_)))
                  (and (cl-struct et:type-dt (name 'DynFunction) (args `(,matcher ,_)))
                       (let input (et-repr-to-string (et:match-matcher->repr matcher)))))
              (et-err (or arg-pos 0) "Expected %s, got %s" input args-type))
             (_ (et-err (or arg-pos 0) "`%s' has type %s\\nInvalid arguments: %s"
                        func func-type args-type))))))))))


;;;; The checking context

;; The checking context provides 3 things: an expr, a recommendation,
;; and an alist of narrows.

(et-defvar et:check--context-expr Nil|Sexp nil)

(et-defvar et:check--context-recommendation Nil|*et:type nil
  "A hint to the current checker of what type its expression should be.

This should ONLY be used in the checker for `lambda', in order to guess
at the function type. It should not be used in any other checkers.

This variable should only be modified by `et:check-check'.")

(et-defvar et:check--context-narrows EtNarrows nil
  "A list of narrows in the current `et:check-check' environment.

This should ONLY be modified by `et:check-check', the existing sub-checker
functions, and the existing narrows modification functions. It should
only be read by functions in this file.")

(et-defun et-cur-expr (&rest path: EtRel) Sexp|Nil
  (et:util-traverse-tree et:check--context-expr (flatten-list path)))

(et-defun et-cur-recommendation () *et:type|Nil
  et:check--context-recommendation)

(et-defun et-recommendation-type (rec: EtRec) *et:type|Nil
  (if (eq rec t) (et-cur-recommendation) rec))


;;;; Narrows helpers

(et-defun et-cur-narrows () EtNarrows
  et:check--context-narrows)

;; Setting narrows

(et-defun et-set-narrows ([(<= T EtNarrows)] narrows: T) T
  (setq et:check--context-narrows narrows))

(et-defun et-kill-all-narrows () Nil
  (setq et:check--context-narrows nil))

(et-defun et-update-var-narrow (var: *et:type-var &optional type: *et:type|Nil) Nil
  (let* ((removed (cl-remove var et:check--context-narrows :key #'car)))
    (setq et:check--context-narrows
          (if (null type) removed (cons (cons var type) removed)))
    nil))

(defmacro et-with-narrows (narrows &rest body)
  (declare (indent 1) (et (@expand)))
  `(let* ((et:check--context-narrows ,narrows)) ,@body))

(defmacro et-when-type (type &rest body)
  "Evaluate BODY with narrows implied by TYPE.

If TYPE is never, then return never without evaluating BODY (BODY is
unreachable)."
  (declare (indent 1))
  (if (et-never-p type) (et-never)
    (et-with-narrows (et:check--narrows-and (et-cur-narrows) (et-type-binds type))
      ,@body)))

;; Accessing narrows

(et-defun et:check--narrows-and (a: EtNarrows b: EtNarrows) EtNarrows
  (cl-loop for var in (seq-uniq (mapcar #'car (append a b)) #'eq)
           for t1 = (alist-get var a) for t2 = (alist-get var b)
           collect (cons var (if t1 (if t2 (et-supersect t1 t2) t1) t2))))

(et-defun et:check-var-type (var: *et:type-var) *et:type
  "Get the current narrowed type of a variable."
  (or (alist-get var et:check--context-narrows)
      (et:type-var->type var)))

(et-defun et:check-symbol-type (sym: Symbol) Nil|*et:type
  (when-let* ((var (et-get-symbol-var sym)))
    (et:check-var-type var)))

(et-defun et:check-var-in-scope? (var: *et:type-var) Boolean
  (not (not (or (eq var (et-global-var (et:type-var->name var)))
                (rassq var et:check--binds)))))


;;;; Check

(et-defstruct et:check--result
  (type nil :et *et:type)
  (narrows nil :et AList<*et:type-var~*et:type>))

(et-defun et:check-check (expr: Sexp narrows: EtNarrows recommendation: Nil|*et:type)
          *et:check--result
  "Generates an `et-result' resulting from typechecking EXPR.

If EXPR is self quoting (a number, string, etc.), the resulting type
will be a literal type representing the literal value.

If EXPR is a symbol VAR, then check if it exists as a variable in the
local scope. Return not just the variable's type, but also {typeof VAR}
so that future calls can perform type narrowing for the variable.

Otherwise, EXPR is (FUNC ARGS...).

If FUNC is a symbol with the `et-checker' property set, the value of
`et-checker' is assumed to be a checker function. A checker function has
no arguments, and runs in an environment with `et:check--context-expr' set to
a copy of EXPR. A checker function should set or mutate
`et:check--context-expr' in order to remove type annotations or otherwise
perform compilation. The `:compiled' field of the result will be set to
the final value of `et:check--context-expr'.

If FUNC is a symbol with the `et-symbol-func-type' property set to an
`et-type', then the arguments to the function will first be checked
individually, and then will be passed to `et-funcall' as a list to
determine the output type."
  (let* ((et:check--context-expr expr)
         (et:check--context-narrows narrows)
         (et:check--context-recommendation recommendation))
    (et:check--result-new
     :type
     (et-failed-boundary
      (or (et-error-boundary nil
            (or (et:check--check-1)
                (unless (et-cur-result-failed?) (et-err nil "Type checking failed mysteriously"))))
          (et-never)))
     ;; Remove any narrows for variables no longer in scope
     :narrows (seq-filter (lambda (narrow) (et:check-var-in-scope? (car narrow)))
                          et:check--context-narrows))))

(et-defun et:check--check-1 () *et:type
  (pcase et:check--context-expr
    (`(,func . ,_args)
     ;; If this function is lazily declared, and hasn't been declared yet, do it now
     (when (get func 'et-deferred-declare)
       (ignore (et-result-boundary (funcall (get func 'et-deferred-declare))))
       (put func 'et-deferred-declare nil))

     ;; Call the checker
     (if-let* ((checker (et-checker-for func)))
         (let* ((output (funcall checker)))
           (if (or (null output) (et:type-p output)) output
             (et-fatal nil "Checker for `%s' had invalid return: %s" func output)))
       (et-err 0 "No type for `%s'" func)))

    ;; Type check a variable (a symbol which is neither a keyword, nil, or t)
    ((and sym (pred symbolp) (pred (not keywordp)) (guard sym) (guard (not (eq sym t))))
     (et:check-check-var sym))

    (expr (et-literal expr))))

(et-defun et-checker-for (sym: Symbol) EtCheckFn|Nil
  (if (et-get-fbind sym) #'et:check--function
    (et-symbol-checker sym)))

(et-defun et:check-check-var (sym: Var) EtChecked
  "Check a variable symbol.

This exists as a helper function so that it can be advised in order to
intercept all accesses to a variable when trying to determine a suitable
type for it."
  (if-let* ((var (et-get-symbol-var sym)))
      (et-add-typeof (et:check-var-type var) var)
    (et-err nil "Variable not in scope: %s" sym)))


;;;; Sub checkers
;;;;; Check at

(et-defun et-check-at (rec: EtRec &rest path: EtRel) *et:type
  "Type check the sub expression at PATH, returning the type or never.

This assumes that the child will ALWAYS run, and thus that the resulting
narrows should ALWAYS be applied.

Unlike `et-at' and the like, PATH must be relative to the current
checking expr root, not the current `et-at' root. This function should
not be called within `et-at', it should always be called at the root of
the current checking expr."
  (let* ((flat (flatten-tree path))
         (expr (et:util-traverse-tree et:check--context-expr flat))
         (recommendation (et-recommendation-type rec))
         (checked (et-at flat (et:check-check expr et:check--context-narrows recommendation))))
    (setq et:check--context-narrows (et:check--result->narrows checked))
    (et:check--result->type checked)))


;;;;; Check tail

(et-defun et-check-tail (rec: EtRec &rest first-path: EtRel) *et:type
  "Check a sequence of expressions, returning the type of the last one."
  (setq first-path (flatten-tree first-path))
  (let* ((parent-path (butlast first-path))
         (parent-expr (et-cur-expr parent-path))
         (start (car (last first-path))))

    (cl-loop for idx upfrom start below (length parent-expr)
             for sub-rec = (when (eq idx (1- (length parent-expr))) rec)
             for ret = (et-check-at sub-rec (append parent-path (list idx)))
             finally return (or ret (et Nil)))))


;;;;; Check expr

(et-defun et-check-expr (rec: EtRec expr: Sexp mappings: AList<EtPath~EtPath>) EtChecked
  "Check EXPR, mapping paths in EXPR back into the current expr."
  (et-propagate-result
   (et-result-boundary
    (et:check-check expr et:check--context-narrows
                    (et-recommendation-type rec)))
   (lambda (path)
     (cl-loop for (from . to) in mappings
              when (and (>= (length path) (length from))
                        (equal from (seq-subseq path 0 (length from))))
              return (append to (seq-subseq path (length from)))
              ;; If none match, map to nil
              finally return nil))))


;;;;; Check expansion

(et-defun et-check-expansion (rec: EtRec expansion: Sexp) *et:type
  "Type check EXPANSION, an expr which was built from the current expr.

EXPANSION is not literally present in the current expression, but it was
built from the current expression, so parts of the current expression
probably exist somewhere inside of EXPANSION, and should be mapped back
onto the original expression."
  (let* ((mappings (et:check--expansion-path-mappings (et-cur-expr) expansion)))
    (et-check-expr rec expansion mappings)))

(et-defun et:check--put-cons-paths (ht: Hashmap<Sexp~EtPath> path: EtPath expr: Sexp) Nil
  (when (consp expr)
    (puthash expr path ht)
    (cl-loop for item in expr
             for idx upfrom 0
             do (et:check--put-cons-paths ht (append path (list idx)) item))))

(et-defun et:check--path-mappings-1 (ht: Hashmap<Sexp~EtPath> path: EtPath expr: Sexp)
          AList<EtPath~EtPath>
  (when (consp expr)
    (if-let* ((p (gethash expr ht)))
        (list (cons p path))
      (cl-loop for item in expr
               for idx upfrom 0
               append (et:check--path-mappings-1 ht (append path (list idx)) item)))))

(et-defun et:check--expansion-path-mappings (orig: Sexp expansion: Sexp) AList<EtPath~EtPath>
  "Determine a collection of path mappings from EXPANSION into ORIG."
  (let* ((ht (make-hash-table :test #'eq)))
    (et:check--put-cons-paths ht nil expansion)
    (et:check--path-mappings-1 ht nil orig)))


;;;; Check macros

(et-defun et-check (chk: EtCheckable) *et:type
  (funcall `(lambda () (et-chk ,chk))))

(defmacro et-chk (chk)
  (pcase chk
    ((pred et:type-p) chk)
    ((pred functionp) `(funcall ,chk))
    ((and `(,sym . ,args) (let mac (get sym 'et-check-macro)) (guard mac))
     `(,mac ,@args))
    ;; (`(,(and sym (pred symbolp)) . ,args) `(,sym ,@args))
    (`(,(and sym (pred symbolp)) . ,_) (error "Invalid checker macro: %s" sym))
    (_ (error "Invalid checker expression"))))

(defmacro et-define-check-macro (name arglist &rest body)
  "Define a checker macro.

A checker macro isn't itself a checker. Instead, a checker macro is a
function that takes zero or more arguments, and generates a checker
function based on those arguments.

The BODY of the checker macro should return a lisp expression that will
be used as the body of the checker function."
  (declare (indent 2) (doc-string 3))
  (pcase name
    ((or `[,abbrev ,fname]
         (and abbrev (let fname (intern (format "et--checker-macro-%s" abbrev)))))
     `(progn (cl-defmacro ,fname ,arglist ,@body)
             (put ',abbrev 'et-check-macro #',fname)))))

(defmacro et-define-check-shortcut (name arglist &rest body)
  (declare (indent 2) (doc-string 3))
  `(et-define-check-macro ,name ,arglist
     ,@(when (stringp (car body)) (list (pop body)))
     (list #'et-chk ,(car body))))


(et-defun et:check--rec-p (obj: Any) Boolean
  (or (eq t obj) (et:type-p obj)))

(defun et:check--rec-and (path)
  (if (et:check--rec-p (car path))
      (list (car path) (cdr path))
    (list nil path)))

(et-define-check-macro $at (&rest path)
  `(apply #'et-check-at (et:check--rec-and ,path)))
(et-define-check-macro $at! (&rest path)
  `(et-check-at t ,path))

(et-define-check-macro $tail (&rest first-path)
  `(apply #'et-check-tail (et:check--rec-and ,first-path)))
(et-define-check-macro $tail! (&rest first-path)
  `(et-check-tail t ,first-path))

(et-define-check-macro $exp (&rest rec-and-val)
  `(let* ((rav ,rec-and-val))
     (et-check-expansion (when (cdr rav) (pop rav)) (car rav))))
(et-define-check-macro $exp! (exp)
  `(et-check-expansion t ,exp))

(et-define-check-macro $type (spec)
  (et-parse-type spec))

(et-define-check-macro $var (var)
  `(et:type-var->type ,var))


;;; ============================================================
;;; Function def handling - `et:func'
;;;; Create function type

(defun et-create-function-type (generics constraints input-repr output-repr)
  "Build a Function or DynFunction type from reprs."
  (et-declare (generics ListR<EtGeneric>)
              (constraints ListR<EtTypeConstraint>)
              (input-repr EtRepr)
              (output-repr EtRepr)
              (@return *et:type))

  (if generics
      (et-dt 'DynFunction
             (et:match-matcher-new :generics generics
                                   :constraints constraints
                                   :repr input-repr)
             output-repr)
    (et-dt 'Function
           (et-repr-to-type input-repr nil)
           (et-repr-to-type output-repr nil))))


;;;; Parse arglist params

(et-defun et-parse-arglist (arglist: ListR<Var>) EtFuncParameters<Var>
  "Parse ARGLIST into (REQUIRED OPTIONAL KEY REST).

ARGLIST is a plain parameter list with no type annotations — just
symbols and default-value forms. REQUIRED, OPTIONAL, KEY, and REST are
each lists of parameter name symbols. The list REST has at most 1
element."
  (let ((required nil)
        (optional nil)
        (key-params nil)
        (rest-param nil)
        (state 'required))
    (dolist (elt arglist)
      (pcase elt
        ('&optional (setq state 'optional))
        ('&rest (setq state 'rest))
        ('&key (setq state 'key))
        ((or `(,name . ,_) name)
         (pcase state
           ('required (push name required))
           ('optional (push name optional))
           ('key (push name key-params))
           ('rest (if rest-param (error "Multiple rest parameters")
                    (setq rest-param name)))))))

    (let* ((all (append required optional key-params (list rest-param))))
      (unless (eq (length all) (length (delete-dups all)))
        (error "Duplicate parameter names")))

    (list (nreverse required)
          (nreverse optional)
          (nreverse key-params)
          (when rest-param (list rest-param)))))


;;;; Finding/parsing declarations

(et-defun et:func--declares-path (expr: Sexp) List<Integer>
  (if-let* ((etd-pos (cl-position 'et-declare expr :key #'car-safe)))
      (list etd-pos)
    (if-let* ((decl-pos (cl-position 'declare expr :key #'car-safe))
              (et-pos (cl-position 'et (nth decl-pos expr) :key #'car-safe)))
        (list decl-pos et-pos)
      nil)))

(et-defun et-find-and-parse-func-decls (params: EtFuncParameters<Var> exprs: ListR<Sexp>)
          Nil|EtFuncDeclarations
  (when-let* ((declare-path (et:func--declares-path exprs)))
    (let* ((declares (cdr (et:util-traverse-tree exprs declare-path))))
      (et-at declare-path
        (et-at-offset 1
          (et-parse-func-decls params declares))))))

(et-defun et-parse-func-decls (params: EtFuncParameters<Var> declares: ListR<Any>)
          Nil|EtFuncDeclarations
  "Parse function declarations.

This assumes that the current path points to DECLARES."
  (let* ((param-list (apply #'append params))
         (param-alist (et: AListR<Var~*et:repr> nil))
         (generics (et: ListR<EtGeneric> nil))
         (constraints (et: ListR<EtTypeConstraint> nil))
         (props (et: KVPList<Var~Any> nil))

         (return-repr (et: Nil|*et:repr nil))
         (function-type (et: Nil|*et:type nil))
         (checker (et: (or Nil (fn Nil *et:type)) nil))

         (strategy (et: Nil|@return|@function|@checker nil))
         (use-strategy
          (lambda (strat)
            (if (null strategy) (setq strategy strat)
              (unless (eq strat strategy)
                (error "Form not valid for strategy %s" strategy))))))

    ;; Parse the fields of the declare block
    (dotimes (form-idx (length declares))
      (et-error-boundary form-idx
        (pcase (nth form-idx declares)
          ;; return strategy
          (`(@generics ,(and gv (pred vectorp)))
           (funcall use-strategy 'return)
           (unless (eq 0 form-idx) (et-fatal 0 "@generics clauses must be first"))
           (et-at 1
             (setq generics (et-genvec-generics gv)
                   constraints (et-genvec-constraints gv))))
          (`(@generics . ,_) (et-fatal 0 "Expected (@generics [VARS...])"))
          (`(@return ,spec)
           (funcall use-strategy 'return)
           (when return-repr (et-fatal 0 "Multiple @return clauses"))
           (when checker (et-fatal 0 "Cannot have both a custom checker and function type"))
           (et-at 1 (setq return-repr (et-parse-repr spec generics))))
          (`(@return . ,_) (et-fatal 0 "Expected (@return TYPE)"))

          ;; function strategy
          (`(@function ,spec)
           (funcall use-strategy 'function)
           (when function-type (et-fatal 0 "Multiple @function clauses"))
           (et-at 1 (setq function-type (et-parse-type spec))))
          (`(@function . ,_) (et-fatal 0 "Expected (@function TYPE)"))

          ;; checker strategy
          ((or
            ;; The checker can be either FUNC or (FUNC ARGS...)
            `(@checker . ,fn-and-args)
            (and `(,abbrev . ,args)
                 (let fn (alist-get abbrev et:func--checker-macros))
                 (guard fn)
                 (let fn-and-args (cons fn args))))
           (funcall use-strategy 'checker)
           (when checker (et-fatal 0 "Multiple checkers specified"))
           (setq checker (if (cdr fn-and-args) fn-and-args (car fn-and-args))))

          ;; Props
          (`(@skip)
           (when (plist-get props :skip) (et-fatal 0 "Multiple @skip clauses"))
           (setq props (cl-list* :skip t props)))
          (`(@show . ,show) ; Can contain `narrows', `vars', `all'
           (when (plist-get props :show) (et-fatal 0 "Multiple @show clauses"))
           (setq props (cl-list* :show show props)))

          ;; Parameters
          (`(,(and name (guard (memq name param-list))) ,spec)
           (funcall use-strategy 'return)
           (when (alist-get name param-alist) (et-fatal 0 "Param defined multiple times"))
           (et-at 1 (setf (alist-get name param-alist)
                          (et-parse-repr spec generics))))

          (_ (et-fatal 0 "Invalid form")))))

    (list
     :parameters params
     :props props
     :definition
     (cond (function-type)
           (return-repr
            (let* ((fn (lambda (p) (or (alist-get p param-alist) (et-parse-repr 'Any generics))))
                   (input-repr (et:func--params-to-input generics params fn #'identity)))
              (et-create-function-type generics constraints input-repr return-repr)))
           (checker)))))


;;;; Construct input repr

(et-defun et-untyped-func-input (params: EtFuncParameters<T>) *et:type
  "Guess an input type based only on the (untyped) parameters."
  (et-repr-to-type
   (et:func--params-to-input nil params (lambda (_) (et Any)) #'identity)))

(defun et:func--params-to-input (generics params fn key-fn)
  "Build an input repr from parameter specs.

FN converts a parameter (T) to the repr that should be used for it, and"
  (et-declare (@generics [T])
              (generics ListR<EtGeneric>) (params EtFuncParameters<T>)
              (fn Function<T~EtRepr>) (key-fn Function<T~Symbol>)
              (@return EtRepr))

  ;; Convert an entry (VAR . REPR) to a labeled repr
  (pcase-let*
      ((`(,req-ps ,opt-ps ,key-ps ,rest-ps) params)
       (rest-repr
        (cond
         (rest-ps (when key-ps (error "Cannot have both key and rest parameters"))
                  (funcall fn (car rest-ps)))
         (key-ps
          (cl-loop for key-p in key-ps
                   nconc (list (intern (format ":%s" (funcall key-fn key-p)))
                               (funcall fn key-p))
                   into plist-args
                   finally return (et-parse-repr `(PList ,@plist-args) generics)))
         ((et-parse-repr 'Nil generics)))))

    (named-let loop ((req req-ps) (opt opt-ps))
      (pcase (list req opt)
        (`((,cur . ,rest) ,_)
         (et-parse-repr (list 'ConsR (funcall fn cur) (loop rest opt))
                        generics (list :field (funcall key-fn cur))))
        (`(,_ (,cur . ,rest))
         ;; The remaining optional/rest arguments may contain generics.
         ;; In the tail=Nil case, all of these generics should be assigned to nil.
         ;;
         ;; This will create some crazy looking parameter types if there
         ;; are generics deep in the optional parameters, but it is
         ;; required so that those generics will be correctly inferred as
         ;; Nil rather than Never.
         (cl-loop for repr in (append opt (list rest-repr))
                  ;; Only if the repr is JUST a generic
                  when (pcase repr ((cl-struct et:repr (dnf `(((S:GENERIC ,_))))) t))
                  collect repr into gen-params
                  finally return
                  (let* ((tail (loop nil rest)))
                    (et-parse-repr
                     `(or (and Nil ,@gen-params)
                          (ConsR ,(funcall fn cur) ,tail))
                     generics (list :field (funcall key-fn cur))))))
        (_ rest-repr)))))


;;;; Determine param types

(defun et:func--param-types (params func-input-type)
  "Determine the parameter types for a particular function.

If the function has generics, then this MUST be called with the
polymorphic types defined, as those polymorphic types probably appear in
FUNC-INPUT-TYPE."
  (et-declare (params EtFuncParameters<Var>)
              (func-input-type *et:type)
              (@return AList<Var~*et:type>))

  ;; First, we want to construct a matcher representing the parameters
  (let* ((params-with-gens
          (cl-loop with idx = 0
                   for group in params
                   collect (cl-loop for var in group
                                    for gen = (intern (format "P%s" (cl-incf idx)))
                                    collect (cons var gen))))
         (param-gens-alist (apply #'append params-with-gens))
         (param-gens (mapcar #'cdr param-gens-alist))
         (params-repr
          (et:func--params-to-input
           param-gens params-with-gens
           (lambda (p) (et-parse-repr (cdr p) param-gens))
           #'car))
         (params-matcher (et:match-matcher-new :generics param-gens :repr params-repr))
         (match-result (et-sub-match params-matcher func-input-type))
         (matches (if (et:match-result->success match-result) (et:match-result->value match-result)
                    (error "Function type is not compatible with parameters"))))
    (cl-loop for (param . _sym) in param-gens-alist
             for type in matches
             collect (cons param type))))


;;;; Create the environment of inside of the function body

(defcustom et-check-function-overloads t
  "Whether to check all overloads of functions."
  :group 'et
  :type 'boolean)

(defmacro et-in-function-body (func-type params &rest body)
  (declare (indent 2) (et (@with *et:type EtFuncParameters<Var>)))
  `(et-with-function-polymorphs ,func-type et-check-function-overloads
     (let* ((param-types (et:func--param-types ,params input-type)))

       ;; If there are overloads, there many be some states
       ;; where optional params are not nillable
       (unless et-check-function-overloads
         ;; Ensure that optional parameters are nillable
         (dolist (opt (cadr params))
           (unless (et-subtype? (et Nil) (alist-get opt param-types))
             (et-err nil "Optional parameter %s is not nillable" opt))))

       (et-with-binds param-types
         ;; The body runs whenever the function is called, not here
         ,@body))))


;;; ============================================================
;;; Processing - `et:process'
;;;; Identify exprs

(et-defun et-identify-expr (expr: Sexp) Nil|EtIdentifyPlist
  (when-let* ((func (car-safe expr))
              (identify (when (symbolp func) (et-symbol-identifier func))))
    (apply identify (cdr expr))))

(et-defun et-identify-exprs (exprs: Sexps) List<Nil|EtIdentifyPlist>
  (cl-loop for expr in exprs
           for idx upfrom 0
           collect (et-at idx (et-identify-expr expr))))


;;;; Process exprs

(defvar et:process--phase nil "The current processing phase.")
(defvar et:process--expr nil "The expression currently being processed.")

(et-defun et-process-exprs (exprs: Sexps &key check test eval only-check) Nil
  (let* ((identifications (et-identify-exprs exprs)))

    ;; Evaluate the buffer
    ;; This must occur first, in case the buffer defines custom type keywords, macros, etc
    (when (or test eval)
      (cl-loop for expr in exprs
               for pos upfrom 0
               ;; Do not evaluate defuns that are already defined.
               ;; This exists purely to avoid issues with et type-checking itself.
               unless (pcase expr
                        (`(,(or 'defun 'cl-defun 'defmacro) ,name . ,_)
                         (symbol-function name)))
               do
               (let* ((et:process--phase :eval)
                      (et:process--expr expr))
                 (et-error-boundary pos
                   (et-wrap-errors "Runtime error: %s" (eval expr))))))

    ;; For each expression, et:process--identify-expr returns a list of
    ;; process-plists. A process-plist describes how to process that
    ;; expression in each phase. For each phase, we want to perform
    ;; the correct action across all expressions.

    (dolist (phase '(:constrain :populate :declare))
      (cl-loop for expr in exprs
               for plist in identifications
               for idx upfrom 0
               for func = (plist-get plist phase)
               when func do
               (let* ((et:process--phase phase)
                      (et:process--expr expr))
                 (et-error-boundary idx (funcall func)))))

    ;; Type-check all root-level expressions
    (when (or check only-check)
      (cl-loop for expr in exprs
               for pos upfrom 0
               when (and (consp expr)
                         (or (et-symbol-func-type (car expr))
                             (et-symbol-checker (car expr)))
                         ;; Only check a certain index when only-check is specified
                         (or (null only-check) (eq only-check pos)))
               do
               (let* ((et:process--phase :check)
                      (et:process--expr expr))
                 (et-error-boundary pos (et:check-check expr nil nil)))))

    ;; Run tests
    (when test
      ;; Run the tests
      (cl-loop
       for expr in exprs
       for pos upfrom 0
       when (eq #'et-test (car expr))
       do
       (cl-loop for test in (cdr expr)
                for test-idx upfrom 1
                do
                (let* ((et:process--phase :test)
                       (et:process--expr test))
                  (et-at (list pos test-idx)
                    (pcase (eval test)
                      ('nil (et-warn nil "Evaluated to nil"))
                      ((and result (pred et:result-p))
                       (when (et:result->failed result)
                         (et-propagate-result result)))))))))
    nil))


;;;; Processing helpers

(et-defun et-process-buffer-exprs () Sexps
  (save-excursion
    (goto-char (point-min))
    (cl-loop while t
             for expr = (condition-case _ (read (current-buffer))
                          (error (cl-return exprs)))
             collect expr into exprs)))

(et-defun et-process-file-exprs (file: String) Sexps
  (with-temp-buffer (insert-file-contents file) (et-process-buffer-exprs)))


;;; ============================================================
;;; Core control flow - `et:flow'
;;;; Check reading/setting a variable

(et-defun et-check-set-var (var: *et:type-var type: *et:type) EtChecked
  "Should be called whenever VAR is assigned to TYPE."
  (if (not (et-subtype? type (et:type-var->type var)))
      (et-err nil "Variable %s is not assignable to type %s" var type)
    (et-update-var-narrow var type)
    type))


;;;; Check a variable binding, determining the variable type

(defvar et:flow--guessing-var-type nil)

(et-define-check-macro $guess-binds (inits body)
  "Check an expression with a variable bound to the best estimated type.

This will first perform a trial run of BODY to determine a good guess
for what SYM should be bound to. Then, it will check BODY again with SYM
bound to that type.

TODO: Only perform estimation for certain types, such as literals and
fresh types."
  (et-declare (inits AList<Var~*et:type>)
              (body EtCheckable))
  `(et-with-binds (et:flow--guess-var-types ,inits ',body)
     (et-chk body)))

(et-defun et:flow--guess-var-types (inits: AList<Var~*et:type> body: EtCheckable>) AList<Var~*et:type>
  (if et:flow--guessing-var-type
      ;; Prevent exponential blow-ups in time complexity due to nested bindings
      (cl-loop for (sym . init) in inits
               collect (cons sym (et-reify-type init)))

    (let* ((var-recs
            (cl-loop for (sym . init) in inits
                     for var = (et:type-var-new :name sym :type (et-reify-type init))
                     collect (cons var nil)))
           (advice (lambda (v)
                     (when-let* ((entry (assq v var-recs)))
                       (push (et-cur-recommendation) (cdr entry)))))
           (et:flow--guessing-var-type t))
      (et-with-advice #'et:check-check-var :before advice
        (et-with-vars (mapcar #'car var-recs)
          (et-check body)))

      (cl-loop for (var . recs) in var-recs
               for reified = (et:type-var->type var)
               for union = (apply #'et-union recs)
               for type = (if (et-subtype? reified union) union
                            (et-union union reified))
               collect (cons (et:type-var->name var) type)))))


;;;; Check branches

(et-define-check-macro [$branches et-check-branches] (&rest branches)
  "Type check parallel code paths, one of which must execute.

In order to ensure the correctness of this function, on every execution
of the parent function, one or more of the branches must execute. For
example, for a non-exaustive pcase, a case MUST be added which just
returns nil. This is to ensure the correctness of the resulting type AND
the resulting narrows.

This is the mother-of-all sub-checkers, as all sequences of checks can
be expressed as a sequence of parallel checkers in sequence."
  (let* ((types-var (gensym "all-types"))
         (narrows-var (gensym "all-narrows")))
    `(let* ((,types-var nil)
            (,narrows-var nil))
       ,(cl-loop for chk in branches
                 collect
                 `(et-with-narrows (et-cur-narrows)
                    (let* ((type (et-chk ,chk)))
                      ;; If the case returns never, assume that it will never exit
                      (unless (et-never-p type)
                        (push type ,types-var)
                        (push (et-cur-narrows) ,narrows-var)))))

       (et-set-narrows (when ,narrows-var (cl-reduce #'et:flow--narrows-or all-narrows)))
       (apply #'et-union ,types-var))))

(et-defun et:flow--narrows-or (a: EtNarrows b: EtNarrows) EtNarrows
  (cl-loop for (var . t1) in a
           for t2 = (alist-get var b)
           when t2 collect (cons var (et:algebra-simplify-type (et-union t1 t2)))))


;;;; Check loop

(et-define-check-macro $loop (body)
  `(et-check-loop ',body))

(et-defun et-check-loop (chk: EtCheckable) *et:type
  "Check BODY-FN, a block that may execute zero or more times.

Pass 1 (diagnostics discarded via `et-result-boundary') discovers the
vars the body can change. Pass 2 checks the body for real, with those
vars' narrows dropped, so iteration N never trusts a narrow iteration
N-1 may have broken. Afterward, the active narrows holds the join of the
zero-iteration path and the after-an-iteration path.

Returns the body's type (from the real pass)."
  (let* ((entry (et-cur-narrows))
         (dirty (et-with-narrows entry
                  (ignore (et-result-boundary (et-check chk)))
                  (et:flow--narrows-changed-vars entry (et-cur-narrows)))))
    (et-set-narrows (cl-remove-if (lambda (n) (memq (car n) dirty)) entry))
    (prog1 (et-check chk)
      ;; Exit: 0 iterations (entry) OR >=1 iterations (current narrows).
      (et-set-narrows (et:flow--narrows-or entry (et-cur-narrows))))))

(et-defun et:flow--narrows-changed-vars (before: EtNarrows after: EtNarrows) List<*et:type-var>
  "Vars whose narrow entry differs between BEFORE and AFTER."
  (cl-loop for var in (seq-uniq (mapcar #'car (append before after)) #'eq)
           unless (equal (alist-get var before) (alist-get var after))
           collect var))


;;;; Check escapable

(et-define-check-macro $escapable (body)
  `(et-check-escapable ',body))

(et-defun et-check-escapable (chk: EtCheckable) *et:type
  "Check BODY-FN, a block that may be exited nonlocally at any point.

Code after the enclosing form (e.g. `catch', `with-local-quit') runs
whether BODY-FN completed or aborted partway through, so the only narrows
that survive are the entry narrows on vars the body never touches: a
narrow the body created (or an entry narrow on a var the body assigns)
cannot be trusted, since the exit may have happened before or after the
change.

Returns the body's type."
  (let* ((entry (et-cur-narrows))
         (type (et-check chk))
         (dirty (et:flow--narrows-changed-vars entry (et-cur-narrows))))
    (et-set-narrows (cl-remove-if (lambda (n) (memq (car n) dirty)) entry))
    type))


;;;; Check deferred

(et-define-check-macro [$deferred et-check-deferred] (body)
  "Check BODY as code that runs at some later, unknown time.

It sees none of the current flow's narrows, and the narrows it produces
do not escape into the current flow."
  `(et-with-narrows nil (et-chk ,body)))


;;;; Custom environments

(et-defstruct et-env
  "An environment in which to check an expression.

Contains things like binds, function binds, and narrows in which an
expression can be evaluated. This is used for defining checkers like
let, cl-flet, pcase, and anything else that binds variables or
functions."
  (narrows nil :et EtNarrows)
  (binds nil :et AList<Var~*et:type>)
  (fbinds nil :et AList<Var~*et:type>))

(et-define-check-macro [$env et-check-with-env] (env chk)
  (let* ((envsym (gensym "env")))
    `(let* ((,envsym ,env))
       (et-with-binds (et-env->binds ,envsym)
         (et-with-fbinds (et-env->fbinds ,envsym)
           (et-with-narrows (et-env->narrows ,envsym)
             (et-chk ,chk)))))))

(et-define-check-macro [$envs et-check-env-branches] (envs &rest branches)
  (let* ((envs-sym (gensym "env")))
    `(let* ((,envs-sym ,envs))
       (et-check-branches
        ,@(cl-loop for b in branches
                   for idx upfrom 0
                   collect `($env (nth idx ,envs-sym) ,b))))))


;;; ============================================================
;;; Control flow helpers - `et:helpers'
;;;; Use a temporary variable

(et-define-check-macro $temp-var (var type body)
  `(let* ((,var (et:type-var-new :name ',var :type (et-chk ,type))))
     (et-chk ,body)))

(et-define-check-macro $when-var (var type body)
  `(et-when-type (et-supersect (et:check-var-type (et-chk ,var)) ,type)
     (et-chk ,body)))


;;;; Check conditionals

(et-define-check-shortcut [$if et-check-if] (cond then else)
  `($temp-var cond-var ,cond
              ($branches
               ($when-var cond-var ($type NonNil) ,then)
               ($when-var cond-var ($type Nil) ,else))))

(et-define-check-shortcut [$or et-check-or] (cond else)
  `($temp-var cond-var ,cond
              ($if ($var cond-var) ($var cond-var) ,else)))

(et-define-check-shortcut [$and et-check-and] (cond then)
  `($if ,cond ,then ($type Nil)))


;;;; Useful check macros

(et-define-check-macro $eval (&rest exprs)
  `(progn ,@exprs))

(et-define-check-macro $pcase (&rest repls)
  "Replace the expression, then typecheck the replacement.

The replacement can also be just a single type, in which case that type
will be the result."
  `(pcase (cdr (et-cur-expr))
     ,@(cl-loop for (pat chk) in repls
                collect `(,pat (et-chk ,chk)))
     (_ (et-fatal nil "Invalid `%s' format" (car (et-cur-expr))))))


;;; ============================================================
;;; Provide

(provide 'et-check)


;;; et-check.el ends here
