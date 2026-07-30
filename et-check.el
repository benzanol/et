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
 (@alias EtRec Nil|*et:type)
 (@alias EtIdentifyPlist (KVPList @:constrain|@:populate|@:declare Nil|fn))
 (@alias EtIdentifyFn (fn Nil EtIdentifyPlist))
 (@alias EtCheckerFn [T] (fn T Nil|*et:type))
 (@alias EtChecker (or EtCheckerFn<Nil>
                       (Cons EtCheckerFn<ListR<Any>> List<Any>)))

 (@symbol-property et-identify EtIdentifyFn)
 (@symbol-property et-checker EtChecker))

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

(et-defun et-symbol-checker (sym: Var) Nil|EtChecker
  (get sym 'et-checker))


;;; ============================================================
;;; Checking - `et:check'
;;;; Symbol bindings

(defvar et:check--binds nil
  "Stack of (SYMBOL . `et:type-var').")

(et-defun et-new-var (name: Var type: *et:type) *et:type
  (et:type-var-new :name name :type type))

(et-defun et-get-symbol-var (sym: Var) *et:type-var|Nil
  (cl-assert (symbolp sym))
  (or (alist-get sym et:check--binds)
      (et-global-var sym)))

(defmacro et-with-binds (binds &rest body)
  "VARS can contain nil values."
  (declare (indent 1) (et (@progn (ListR (or Nil (ConsR Var *et:type))))))
  `(let* ((vs (cl-loop for entry in ,binds
                       when entry collect
                       (let* ((name (car entry)) (type (cdr entry)))
                         (cl-assert (symbolp name)) (cl-assert (et:type-p name))
                         (cons name (et:type-var-new :name name :type type)))))
          (et:check--binds (append vs et:check--binds)))
     ,@body))


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

(et-defun et-cur-recommendation () *et:type|Nil
  et:check--context-recommendation)

(et-defun et-cur-expr (&rest path: EtRel) Sexp|Nil
  (et:util-traverse-tree et:check--context-expr (flatten-list path)))


;;;; Narrows helpers

(et-defun et-kill-all-narrows () Nil
  (setq et:check--context-narrows nil))

(et-defun et-cur-narrows () EtNarrows
  et:check--context-narrows)

(defmacro et-with-narrows (narrows &rest body)
  (declare (indent 1) (et (@expand)))
  `(let* ((et:check--context-narrows ,narrows)) ,@body))

(et-defun et:check--narrows-and (a: EtNarrows b: EtNarrows) EtNarrows
  (cl-loop for var in (seq-uniq (mapcar #'car (append a b)) #'eq)
           for t1 = (alist-get var a) for t2 = (alist-get var b)
           collect (cons var (if t1 (if t2 (et-supersect t1 t2) t1) t2))))

(et-defun et-apply-type-narrows (type: *et:type) Nil
  "Load bindings implied by TYPE into the current narrows scope."
  (cl-callf et:check--narrows-and et:check--context-narrows
    (et-type-binds type)))

(et-defun et-set-narrows ([(<= T EtNarrows)] narrows: T) T
  (setq et:check--context-narrows narrows))

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

(et-defun et:check--check-0 (expr: Sexp narrows: EtNarrows recommendation: Nil|*et:type)
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

     (pcase nil
       ;; Custom checker
       ((and (let checker (et-symbol-checker func)) (guard checker)
             (let output (if (functionp checker) (funcall checker) (apply checker))))
        (if (or (null output) (et:type-p output)) output
          (et-fatal nil "Checker for `%s' had invalid return: %s" func output)))

       ;; Function type property
       ((and (let func-type (et-symbol-func-type func)) (guard func-type))
        (let* ((arg-types (cl-loop for pos upfrom 1
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
                            func func-type args-type)))))))))

       (_ (et-err 0 "No type for `%s'" func))))

    ;; Type check a variable (a symbol which is neither a keyword, nil, or t)
    ((and sym (pred symbolp) (pred (not keywordp)) (guard sym) (guard (not (eq sym t))))
     (pcase nil
       ;; Check if there is a variable (local OR global scoped) associated with it
       ((and (let var (et-get-symbol-var sym)) (guard var))
        (et-add-typeof (et:check-var-type var) var))

       (_ (et-err nil "Free variable: %s" sym))))

    (expr (et-literal expr))))


;;;; Macroexpansion handling

(defvar et:check--expansion-expr nil
  "An expression which got macroexpanded in an `et:check-check'.")

(et-defvar et:check--expansion-waiting Boolean nil
  "We are in a macroexpand, waiting to find a matching expr.")

(et-defun et:check-check (expr: Sexp narrows: EtNarrows recommendation: Nil|*et:type)
          *et:check--result
  "Adjust the path based on a macro expansion if necessary.

When macroexpanding an expression, the inner expressions will end up at
different paths than the original expression. It is impossible to
deterministically decide where the subexpressions ended up, but the best
heuristic is by searching (using `equal') for each nested call to
`et:check-check' inside of the original expression.

This is a thin wrapper around `et:check--check-0' which checks if there
was a past macroexpansion, checks if EXPR appeared in the original
expanded expression, and adjusts the path if so."

  (if (not et:check--expansion-waiting)
      (et:check--check-0 expr narrows recommendation)

    (let* ((path (et:sub--path-in-tree expr et:check--expansion-expr)))
      (if (not (listp path)) (et:check--check-0 expr narrows recommendation)
        (let* ((et:check--expansion-waiting nil))
          (et-without-sticky-path
            (et-at path
              (et:check--check-0 expr narrows recommendation))))))))

(defun et-check-expansion (expanded &optional recommendation)
  "Type check EXPANSION, an expr which was built from the current expr.

EXPANSION is not literally present in the current expression, but it was
built from the current expression, so parts of the current expression
probably exist somewhere inside of EXPANSION, and should be mapped back
onto the original expression."
  (let* (;; Only the very first expanded expr exists in the actual code.
         ;; If we get an expansion inside an expansion, keep the original expr
         (et:check--expansion-expr (or et:check--expansion-expr (et-cur-expr)))
         (result
          (et-with-sticky-path
            (et:check-check expanded et:check--context-narrows recommendation))))
    (setq et:check--context-narrows (et:check--result->narrows result))
    (et:check--result->type result)))

(defun et:sub--path-in-tree (expr tree)
  "If EXPR exists in TREE, return its path, or return `NO' otherwise."
  (if (equal expr tree) nil
    (cl-loop for subtree in tree ; safe even if tree is not a list
             for idx upfrom 0
             for path = (et:sub--path-in-tree expr subtree)
             unless (eq path 'NO) return (cons idx path)
             finally return 'NO)))


;;;; Sub-check

(et-defun et-check-at (rec: EtRec &rest path: EtRel) *et:type
  "Type check the sub expression at PATH, returning the type or never.

This assumes that the child will ALWAYS run, and thus that the resulting
narrows should ALWAYS be applied."
  (let* ((flat (flatten-tree path))
         (expr (et:util-traverse-tree et:check--context-expr flat))
         (checked (et-at flat (et:check-check expr et:check--context-narrows rec))))
    (setq et:check--context-narrows (et:check--result->narrows checked))
    (et:check--result->type checked)))


;;;; Narrows modifiers

(defun et-checker-on-set-var (var type)
  "Should be called whenever VAR is assigned to TYPE.

This should be called anytime a variable is set."
  (et-declare (var *et:type-var) (type *et:type) (@return Nil))

  (setq et:check--context-narrows
        (cons (cons var type)
              (cl-remove var et:check--context-narrows :key #'car))))


;;;; Tests

(defun et-root-check-type (expr)
  (et-declare (expr Sexp) (@return *et:type))
  (let* ((result (et-result-boundary (et:check--result->type (et:check-check expr nil nil)))))
    (when (et:result->failed result) (error "Type-checking failed"))
    (et:result->value result)))

(defmacro et-assert-resolve (type expr &optional not)
  (declare (indent 1))
  `(et-result-boundary
    (let* ((t-type (et-at 1 (et ,type)))
           (r-type (et-at 2 (et:check--result->type (et:check-check ',expr nil nil)))))
      (or (,(if not #'not #'identity) (et-subtype? r-type t-type))
          (et-err 0 "Expected %s, got %s" t-type r-type)))))

(defmacro et-assert-no-resolve (type expr)
  (declare (indent 1))
  `(et-assert-resolve ,type ,expr 'NOT))

(defmacro et-assert-resolve-errors (expr)
  `(et-result-boundary
    (or (et:result->failed (et-result-boundary (et:check--result->type (et:check-check ',expr nil nil))))
        (et-err 0 "Didn't fail"))))

(et-test
 (et-assert-resolve Integer 1)
 (et-assert-resolve Number 1)
 (et-assert-resolve Positive 1)
 (et-assert-resolve Negative -1)
 (et-assert-resolve Number 1.1)
 (et-assert-no-resolve Number "1")
 (et-assert-no-resolve Integer 1.1)
 (et-assert-no-resolve Positive -1)
 (et-assert-no-resolve Positive 0)
 (et-assert-no-resolve Negative 1)
 (et-assert-no-resolve Negative 0)

 (et-assert-resolve String "1")
 (et-assert-no-resolve String 1)

 (et-assert-resolve Symbol nil)
 (et-assert-resolve Symbol t)
 (et-assert-no-resolve Symbol 1)
 (et-assert-no-resolve Symbol "1")

 (et-assert-resolve Boolean t)
 (et-assert-resolve Boolean nil)
 (et-assert-no-resolve Boolean 1)
 (et-assert-no-resolve Boolean "1"))

(et-test
 ;; and - value must satisfy all constituent types
 (et-assert-resolve Boolean&Symbol&True&@t t)
 (et-assert-no-resolve Boolean&Integer t)
 (et-assert-no-resolve Boolean&Integer 1)
 (et-assert-no-resolve Boolean&Integer nil)

 ;; Two or types
 (et-assert-resolve Boolean|Integer t)
 (et-assert-resolve Boolean|Integer nil)
 (et-assert-resolve Boolean|Integer 1)
 (et-assert-no-resolve Boolean|Integer "1")

 ;; Three or types
 (et-assert-resolve Boolean|Integer|String t)
 (et-assert-resolve Boolean|Integer|String 1)
 (et-assert-resolve Boolean|Integer|String "1")

 ;; Nested - and inside or
 (et-assert-resolve Integer|Boolean&Symbol t)
 (et-assert-resolve Integer|Boolean&Symbol 1)

 ;; Nested - or inside and
 (et-assert-resolve Boolean&{Symbol|Integer} t)
 (et-assert-resolve Boolean&{Symbol|Integer} nil)
 (et-assert-no-resolve Boolean&{Symbol|Integer} 1))


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

(et-defvar et:func--checker-abbrevs AList<Sym~EtCheckerFn<Any>>)

(et-defun et-define-checker-abbrev (abbrev: Sym checker: EtCheckerFn<Any>) Nil
  (setf (alist-get abbrev et:func--checker-abbrevs)
        checker))

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
                 (let fn (alist-get abbrev et:func--checker-abbrevs))
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

(et-defun et-untyped-func-input (params: EtFuncParameters<T) *et:repr
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
;;; Provide

(provide 'et-check)


;;; et-check.el ends here
