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
(require 'et-annotate)
(require 'find-func)
(require 'pp)
(require 'seq)


;;; ============================================================
;;; Globals
;;;; Type declarations

(et-declare
 (@alias EtNarrows Alist<EtVar_EtType>)
 ;; True = keep the current recommendation
 (@alias EtRec Nil|EtType)
 (@alias EtProcessingPhase :constrain|:populate|:declare)
 (@alias EtIdentifyPlist (PlistOf EtProcessingPhase fn))
 (@alias EtIdentifyFn (fn Nil EtIdentifyPlist))

 (@alias EtCheckFn (fn Nil EtType))

 (@alias EtChk
         (or EtType EtCheckFn
             ;; car=t => then use the current recommendation
             ;; car=* => check the tail AND use the current recommendation
             (&Cons Symbol &List)))

 (@symbol-property et-identify EtIdentifyFn)
 (@symbol-property et-checker EtCheckFn)
 (@symbol-property et-chk EtChk)
 (@symbol-property et-check-macro fn<Any_EtType>))

(et-declare
 (@alias EtFuncParameters [T]
         (tuple &List<T> &List<T> &List<T> Nil|&tuple<T>))
 (@alias EtFuncDeclarations
         (Plist :parameters EtFuncParameters<Var>
                :props PlistOf<Var_Any>
                ;; Either a function type or a custom checker
                :definition (or Nil EtType (fn Nil EtType))))

 (@symbol-property et-symbol-func-type EtType)
 (@symbol-property et-function-parameters EtFuncParameters<Var>)
 (@symbol-property et-function-props PlistOf<Var_Any>))


;;;; Symbol properties
;;;;; Deferred declare

(et-defvar et-no-defer-declare Bool nil
  "If non-nil, then don't defer the declare phase.")

(et-defun et-run-deferred-declare (name: Symbol) Boolean
  (when (get name 'et-deferred-declare)
    (ignore (et-result-boundary (funcall (get name 'et-deferred-declare))))
    (put name 'et-deferred-declare nil)))


(defmacro et-deferred-declare (name &rest body)
  (declare (indent 1))
  `(let* ((fn (lambda () ,@body)))
     (if et-no-defer-declare (funcall fn)
       (put ,name 'et-deferred-declare fn))))


;;;;; Function type

(et-defun et-symbol-func-type (name: Var) EtType|Nil
  (et-run-deferred-declare name)
  (get name 'et-symbol-func-type))

(et-defun et-symbol-func-props (name: Var) Nil|PlistOf<Any_Any>
  (get name 'et-function-props))

(et-defun et-symbol-func-params (name: Var) Nil|PlistOf<Any_Any>
  (get name 'et-function-parameters))

(et-defun et-symbol-set-func-decls (name: Var decls: EtFuncDeclarations) Nil
  "Assign relevant symbol properties to FUNC."
  (put name 'et-function-props (plist-get decls :props))
  (put name 'et-function-parameters (plist-get decls :parameters))
  (pcase (plist-get decls :definition)
    ((and type (pred et:type-p)) (put name 'et-symbol-func-type type))
    ((and chk (pred functionp)) (put name 'et-checker chk))))


;;;;; Variable type

(et-defun et-set-global-var-type (name: Var type: EtType) Nil
  (put name 'et-variable-type type)
  (put name 'et-variable-var (et:type-var-new :name name :type type)))

(et-defun et-global-var-type (name: Var) EtType|Nil
  (get name 'et-variable-type))

(et-defun et-global-var (name: Var) EtType|Nil
  (get name 'et-variable-var))

(et-defop global-value (type)
  (pcase (et-type-single type)
    ((cl-struct et:type-dt (name 'Literal) (args `(,(and sym (pred symbolp)))))
     (or (et-global-var-type sym)
         (error "Unknown type for symbol %s" sym)))
    ((guard (et-subtype? type (et Symbol))) (et Any))
    (_ (error "Not a subtype of Symbol"))))


;;;;; Identifier

(et-defvar et:process--identifier-symbols List<Symbol> nil)

(et-defun et-set-identifier (sym: Var fn: EtIdentifyFn) Nil
  (push sym et:process--identifier-symbols)
  (put sym 'et-identify fn))

(et-defun et-symbol-identifier (sym: Var) Nil|EtIdentifyFn
  (get sym 'et-identify))


;;;;; Checker

(et-defun et-set-checker (sym: Var fn: fn) Nil
  (put sym 'et-checker fn)
  nil)

(et-defun et-symbol-checker (sym: Var) Nil|EtCheckFn
  (get sym 'et-checker))

(et-defun et-set-chk (sym: Var chk: EtChk) Nil
  (et-set-checker sym (eval `(lambda () (et-chk ,chk))))
  (put sym 'et-chk chk))

(et-defun et-symbol-chk (sym: Var) EtChk?
  (get sym 'et-chk))


;;; ============================================================
;;; Checking - `et:check'
;;;; Symbol bindings

(et-defvar et:check--binds Alist<Var_EtVar> nil
  "Stack of (SYMBOL . `et:type-var').")

(et-defun et-new-var (name: Var type: EtType) EtType
  (et:type-var-new :name name :type type))

(et-defun et-get-symbol-var (sym: Var) EtVar|Nil
  (cl-assert (symbolp sym))
  (or (alist-get sym et:check--binds)
      (et-global-var sym)))

(defmacro et-with-vars (vars &rest body)
  (declare (indent 1) (et (@progn &List<EtVar>)))
  (let* ((loop `(cl-loop for var in ,vars
                         do (cl-assert (et:type-var-p var))
                         collect (cons (et:type-var->name var) var))))
    `(let* ((et:check--binds (append ,loop et:check--binds)))
       ,@body)))

(defmacro et-with-binds (binds &rest body)
  "VARS can contain nil values."
  (declare (indent 1) (et (@progn (&List (or Nil (&Cons Var EtType))))))
  (let* ((loop `(cl-loop for entry in ,binds
                         when entry collect
                         (let* ((name (car entry)) (type (cdr entry)))
                           (cl-assert (symbolp name)) (cl-assert (et:type-p type))
                           (cons name (et:type-var-new :name name :type type))))))
    `(let* ((et:check--binds (append ,loop et:check--binds)))
       ,@body)))


;;;; Function bindings/checking

(et-defvar et:check--fbinds Alist<Symbol_EtType> nil
  "Stack of (SYMBOL . `et:type').")

(et-defun et-get-fbind (name: Symbol) EtType|Nil
  (or (alist-get name et:check--fbinds)
      (et-symbol-func-type name)))

(et-defun et-checker-for (sym: Symbol) EtCheckFn|Nil
  ;; Lexical function bindings shadow global declarations.  For globals, an
  ;; explicit checker is more specific than the ordinary function type.
  (if (assq sym et:check--fbinds) #'et:check--function
    (or (et-symbol-checker sym)
        (when (et-symbol-func-type sym) #'et:check--function))))

(defmacro et-with-fbinds (fbinds &rest body)
  (declare (indent 1) (et (@progn Alist<Symbol_EtType>)))
  `(let* ((et:check--fbinds (append ,fbinds et:check--fbinds)))
     ,@body))

(et-defun et:check--function () EtType
  (let* ((func (car (et-cur-expr)))
         (func-type (et-get-fbind func)))

    ;; First, count the number of arguments passed, then construct a matcher
    ;; to determine a type recommendation for each argument
    (let* ((arg-gens (cl-loop for _ in (cdr (et-cur-expr))
                              collect (intern (symbol-name (gensym "Arg")))))
           (fn-repr (et-parse-repr `(Function (&tuple ,@arg-gens)
                                              ,(or (et-cur-recommendation) 'Any))
                                   arg-gens))
           (fn-matcher (et:match-matcher-new :generics arg-gens :repr fn-repr))
           ;; Compute the recommendations
           (m-res (ignore-errors (et-sub-match fn-matcher func-type t)))
           (recs (and m-res (et:match-result->success m-res) (et:match-result->value m-res)))
           ;; (_ (debug))
           ;; Use those recommendations to check the actual arguments
           (arg-types (cl-loop for pos upfrom 1 below (length (et-cur-expr))
                               for rec = (nth (1- pos) recs)
                               for type = (et-check-at rec pos)
                               for unf = (et:algebra-unfreshen-to-target type rec)
                               collect (et-copy-with unf :label (list :position pos))))
           (args-type (et-tuple '&Cons arg-types))
           (output-result (et:algebra-funcall func-type args-type)))
      (cond
       ((et:match-result->success output-result) (et:match-result->value output-result))
       ;; If `et--result-failed' is already true, that means one of the arguments was invalid,
       ;; which means the true error was in the arguments, not this call
       ((et-cur-result-failed?) (et-never))
       ;; The stack in OUTPUT-RESULT is a list of (`sub'/`super' MATCHER TYPE)
       (t
        ;; Find the stack frame corresponding to the position
        (cl-loop
         with (arg-pos arg-type param-name param-repr) = nil
         for (_ mrepr type) in (et:match-result->stack output-result)
         ;; Collect the argument position
         for pos = (plist-get (et:type->label type) :position)
         when pos do (setq arg-pos pos arg-type type param-repr mrepr)
         ;; Collect the parameter type
         for name = (plist-get (et:repr->label mrepr) :field)
         when (and name (null param-name)) do (setq param-name name)
         ;; Display the error message
         finally do
         (if (and arg-type param-repr)
             (et-fatal arg-pos "[%s] Expected %s, found %s" param-name
                       (et-repr-to-string param-repr) arg-type)
           (pcase (et-type-single func-type)
             ((or (cl-struct et:type-dt (name 'Function) (args `(,input ,_)))
                  (and (cl-struct et:type-dt (name 'PolyFunction) (args `(,matcher ,_)))
                       (let input (et-repr-to-string (et:match-matcher->repr matcher)))))
              (et-fatal (or arg-pos 0) "Expected %s, got %s" input args-type))
             (_ (et-fatal (or arg-pos 0) "`%s' has type %s\\nInvalid arguments: %s"
                          func func-type args-type))))))))))


;;;; The checking context

;; The checking context provides 3 things: an expr, a recommendation,
;; and an alist of narrows.

(et-defvar et:check--context-expr Nil|Sexp nil)

(et-defvar et:check--context-recommendation EtRec nil
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

(et-defun et-cur-recommendation () EtType|Nil
  et:check--context-recommendation)

(defmacro et-with-recommendation (rec &rest body)
  (declare (indent 1))
  `(let* ((et:check--context-recommendation ,rec)) ,@body))


;; Other variables

(et-defvar et:check--unreachable Boolean nil
  "The next expression to be checked is unreachable.")

(et-defvar et:check-preliminary-pass? Boolean nil
  "Non-nil if this is a preliminary checking pass.

Preliminary checking passes happen in `let' expressions to guess types
for untyped variables, and in loops to determine the final loop narrows.")

(defmacro et:check-preliminary-pass (&rest body)
  (let* ((outsym (gensym "output")))
    `(let* ((et:check-preliminary-pass? t)
            (,outsym nil))
       (ignore
        (et-result-boundary
         ;; Do not preserve narrows from this pass
         (et-with-narrows (et-cur-narrows)
           (setq ,outsym (progn ,@body)))))
       ,outsym)))


;;;; Narrows helpers

(et-defun et-cur-narrows () EtNarrows
  et:check--context-narrows)

;; Setting narrows

(et-defun et-set-narrows ([(T <: EtNarrows)] narrows: T) T
  (setq et:check--context-narrows narrows))

(et-defun et-kill-all-narrows () Nil
  (setq et:check--context-narrows nil))

(et-defun et-update-var-narrow (var: EtVar &optional type: EtType|Nil) Nil
  (let* ((removed (cl-remove var et:check--context-narrows :key #'car)))
    (setq et:check--context-narrows
          (if (null type) removed (cons (cons var type) removed)))
    nil))

(et-defun et-apply-type-narrows (type: EtType) Nil
  (cl-callf et:check--narrows-and et:check--context-narrows
    (et-type-binds type))
  nil)

(defmacro et-with-narrows (narrows &rest body)
  (declare (indent 1) (et (@expand)))
  `(let* ((et:check--context-narrows ,narrows)) ,@body))

(defmacro et-when-type (type &rest body)
  "Evaluate BODY with narrows implied by TYPE.

If TYPE is never, then return never without evaluating BODY (BODY is
unreachable)."
  (declare (indent 1))
  (let* ((typesym (gensym "type")))
    `(let* ((,typesym ,type)
            (et:check--unreachable (or et:check--unreachable (et-never-p ,typesym))))
       (et-with-narrows (et:check--narrows-and (et-cur-narrows) (et-type-binds ,typesym))
         (nth (if et:check--unreachable 0 1)
              (list (et Never)
                    (progn ,@body)))))))

;; Accessing narrows

(et-defun et:check--narrows-and (a: EtNarrows b: EtNarrows) EtNarrows
  (cl-loop for var in (seq-uniq (mapcar #'car (append a b)) #'eq)
           for t1 = (alist-get var a) for t2 = (alist-get var b)
           collect (cons var (if t1 (if t2 (et-supersect t1 t2) t1) t2))))

(et-defun et:check-var-type (var: EtVar) EtType
  "Get the current narrowed type of a variable."
  (or (alist-get var et:check--context-narrows)
      (et:type-var->type var)))

(et-defun et:check-symbol-type (sym: Symbol) Nil|EtType
  (when-let* ((var (et-get-symbol-var sym)))
    (et:check-var-type var)))

(et-defun et:check-var-in-scope? (var: EtVar) Boolean
  (not (not (or (eq var (et-global-var (et:type-var->name var)))
                (rassq var et:check--binds)))))

(et-defun et-pp-narrows (narrows: EtNarrows &optional sep: String|Nil) String
  (cl-loop for (var . type) in narrows
           collect (format "%s: %s" (et:type-var->name var) (et-pp type)) into strs
           finally return (string-join strs (or sep "\\n"))))


;;;; Check

(et-defstruct et:check--result
  (type nil :et EtType)
  (narrows nil :et Alist<EtVar_EtType>))

(et-defun et:check-check (expr: Sexp narrows: EtNarrows recommendation: EtRec)
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
            (if (and et:check--unreachable (not et:check-preliminary-pass?))
                (et-warn nil "Unreachable code")
              (let* ((type (et:check--check-1)))
                (if (et:type-p type) type
                  (et-err nil "Invalid checker return: %s" type)))))
          (et-never)))
     ;; Remove any narrows for variables no longer in scope
     :narrows (seq-filter (lambda (narrow) (et:check-var-in-scope? (car narrow)))
                          et:check--context-narrows))))

(et-defun et:check--check-1 () EtType
  (pcase et:check--context-expr
    (`(,(and func (pred symbolp)) . ,_args)
     ;; If this function is lazily declared, and hasn't been declared yet, do it now
     (et-run-deferred-declare func)

     ;; Call the checker
     (if-let* ((checker (et-checker-for func)))
         (et-wrap-errors (format "Checker for `%s' errored: %%s" func)
           (funcall checker))
       (et-fatal 0 "No checker for `%s'" func)))

    ;; Type check a variable (a symbol which is neither a keyword, nil, or t)
    ((and sym (pred symbolp) (pred (not keywordp)) (guard sym) (guard (not (eq sym t))))
     (if-let* ((var (et-get-symbol-var sym)))
         (et:flow-check-var var)
       (et-fatal nil "Variable not in scope: %s" sym)))

    (expr (et-literal expr))))


;;;; Sub checkers
;;;;; Check at

(et-defun et-check-at (rec: EtRec|True &rest path: EtRel) EtType
  "Type check the sub expression at PATH, returning the type or never.

This assumes that the child will ALWAYS run, and thus that the resulting
narrows should ALWAYS be applied.

Unlike `et-at' and the like, PATH must be relative to the current
checking expr root, not the current `et-at' root. This function should
not be called within `et-at', it should always be called at the root of
the current checking expr."
  (when (eq rec t) (setq rec (et-cur-recommendation)))
  (let* ((flat (flatten-tree path))
         (expr (et:util-traverse-tree et:check--context-expr flat))
         (checked (et-at flat (et:check-check expr et:check--context-narrows rec))))
    (setq et:check--context-narrows (et:check--result->narrows checked))
    (et:check--result->type checked)))


;;;;; Check tail

(et-defun et-check-tail (rec: EtRec|True &rest first-path: EtRel) EtType
  "Check a sequence of expressions, returning the type of the last one."
  (when (eq rec t) (setq rec (et-cur-recommendation)))
  (setq first-path (flatten-tree first-path))
  (let* ((parent-path (butlast first-path))
         (parent-expr (et-cur-expr parent-path))
         (start (car (last first-path))))

    (cl-loop for idx upfrom start below (length parent-expr)
             for sub-rec = (when (eq idx (1- (length parent-expr))) rec)
             for ret = (et-check-at sub-rec (append parent-path (list idx)))
             finally return (or ret (et Nil)))))


;;;;; Check expr

(et-defun et-check-expr (rec: EtRec|True expr: Sexp mappings: Alist<EtPath_EtPath>) EtType
  "Check EXPR, mapping paths in EXPR back into the current expr."
  (when (eq rec t) (setq rec (et-cur-recommendation)))
  (let* ((checked
          (et-propagate-result
           (et-result-boundary
            (et:check-check expr et:check--context-narrows rec))
           (lambda (path)
             (cl-loop for (from . to) in mappings
                      when (and (>= (length path) (length from))
                                (equal from (seq-subseq path 0 (length from))))
                      return (append to (seq-subseq path (length from)))
                      ;; If none match, map to nil
                      finally return nil)))))
    (setq et:check--context-narrows (et:check--result->narrows checked))
    (et:check--result->type checked)))


;;;;; Check expansion

(et-defun et-check-expansion (rec: EtRec|True expansion: Sexp) EtType
  "Type check EXPANSION, an expr which was built from the current expr.

EXPANSION is not literally present in the current expression, but it was
built from the current expression, so parts of the current expression
probably exist somewhere inside of EXPANSION, and should be mapped back
onto the original expression."
  (when (eq rec t) (setq rec (et-cur-recommendation)))
  (let* ((mappings (et:check--expansion-path-mappings (et-cur-expr) expansion)))
    (et-check-expr rec expansion mappings)))

(et-defun et:check--put-cons-paths (ht: Hashmap<Sexp_EtPath> path: EtPath expr: Sexp) Nil
  (when (consp expr)
    (puthash expr path ht)
    (cl-loop for item in expr
             for idx upfrom 0
             do (et:check--put-cons-paths ht (append path (list idx)) item))))

(et-defun et:check--path-mappings-1 (ht: Hashmap<Sexp_EtPath> path: EtPath expr: Sexp)
          Alist<EtPath_EtPath>
  (when (consp expr)
    (if-let* ((p (gethash expr ht)))
        (list (cons p path))
      (cl-loop for item in expr
               for idx upfrom 0
               append (et:check--path-mappings-1 ht (append path (list idx)) item)))))

(et-defun et:check--expansion-path-mappings (orig: Sexp expansion: Sexp) Alist<EtPath_EtPath>
  "Determine a collection of path mappings from EXPANSION into ORIG."
  (let* ((ht (make-hash-table :test #'eq)))
    (et:check--put-cons-paths ht nil expansion)
    (et:check--path-mappings-1 ht nil orig)))


;;;; Check macros

(et-defun et-assert-type (type: EtType) EtType
  (or (et:type-p type) (error "Not a type: %s" type))
  type)

(defmacro et-chk (chk &rest other)
  (if other `(et-chk ($progn ,chk ,@other))
    (pcase chk
      ((pred et:type-p) chk)
      ((pred functionp) `(et-assert-type (funcall ,chk)))
      ((and `(,sym . ,args) (let mac (get sym 'et-check-macro)) (guard mac))
       `(et-assert-type (,mac ,@args)))
      ;; (`(,(and sym (pred symbolp)) . ,args) `(,sym ,@args))
      (`(,(and sym (pred symbolp)) . ,_) (error "Invalid checker macro: %s" sym))
      (_ (error "Invalid checker expression: %s" chk)))))

(defmacro et-define-macro-checker (name arglist &rest body)
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
     `(eval-and-compile
        (cl-defmacro ,fname ,arglist ,@body)
        (put ',abbrev 'et-check-macro #',fname)))))

(defmacro et-define-check-shortcut (name arglist &rest body)
  (declare (indent 2) (doc-string 3))
  `(et-define-macro-checker ,name ,arglist
     ,@(when (stringp (car body)) (list (pop body)))
     (list #'et-chk ,(car body))))

(et-defun et-check-macro-p (sym: Symbol) Boolean
  (not (not (get sym 'et-check-macro))))


;;;; Leaf check macros

(et-define-macro-checker $at (&rest path)
  `(et-check-at (et-cur-recommendation) (list ,@path)))

(et-define-macro-checker $tail (&rest first-path)
  `(et-check-tail (et-cur-recommendation) (list ,@first-path)))

(et-define-macro-checker $exp (expr)
  `(et-check-expansion (et-cur-recommendation) ,expr))

(et-define-macro-checker $exps (exprs)
  `(cl-loop for (expr . rest) on ,exprs
            for last = (et-check-expansion (unless rest (et-cur-recommendation)) expr)
            finally return last))


;;; ============================================================
;;; Processing - `et:process'
;;;; Identify exprs

(et-defvar et-identifying-expr Sexp|Nil nil)

(et-defun et-identify-expr (expr: Sexp) EtIdentifyPlist?
  (when-let* ((func (car-safe expr))
              (identify (when (symbolp func) (et-symbol-identifier func))))
    (let* ((et-identifying-expr expr))
      (apply identify (cdr expr)))))

(et-defun et-identify-exprs (exprs: Sexps) List<EtIdentifyPlist?>
  (cl-loop for expr in exprs
           for idx upfrom 0
           collect (et-error-boundary idx (et-identify-expr expr))))


;;;; Top-level et-declare

(et-defun et:process--perform-declare-phase (plists: &List<EtIdentifyPlist> phase: EtProcessingPhase) Nil
  (cl-loop for plist in plists
           for pos upfrom 1
           for func = (plist-get plist phase)
           when func do (et-error-boundary pos (funcall func))))

(et-defun et:process--identify-declare (forms: Sexps) EtIdentifyPlist
  (let* ((plists (et-at-offset 1 (et-identify-exprs forms))))
    (list
     :constrain (lambda () (et:process--perform-declare-phase plists :constrain))
     :populate (lambda () (et:process--perform-declare-phase plists :populate))
     :declare (lambda () (et:process--perform-declare-phase plists :declare)))))

(et-defun et:process--identify-declares (exprs: Sexps) List<Nil|EtIdentifyPlist>
  (cl-loop for expr in exprs
           collect (when (eq #'et-declare (car-safe expr))
                     (et:process--identify-declare (cdr expr)))))


;;;; Process exprs

(defvar et:process--phase nil "The current processing phase.")
(defvar et:process--expr nil "The expression currently being processed.")

(defcustom et-allow-unrecognized t
  "Allow top-level forms which don't have a registered checker."
  :group 'et
  :type 'boolean)

(et-defun et-process-exprs (exprs: Sexps &key check test eval only-check) List<EtType>
  ;; If checking is enabled for this file, that means we want diagnostics,
  ;; so disable deferred identification IFF checking is enabled
  (let* ((et-no-defer-declare check)
         ;; Identify declares first, so that they can set up other symbols as top-level identifiers
         (declare-idents (et:process--identify-declares exprs))
         (non-declare-idents (et-identify-exprs exprs))
         (identifications (cl-loop for p1 in declare-idents
                                   for p2 in non-declare-idents
                                   collect (or p1 p2)))
         check-results)

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
                   (et-wrap-errors "Runtime error: %s" (eval expr t))))))

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
                         (or (not et-allow-unrecognized)
                             (et-symbol-func-type (car expr))
                             (et-symbol-checker (car expr)))
                         ;; Only check a certain index when only-check is specified
                         (or (null only-check) (eq only-check pos)))
               do
               (let* ((et:process--phase :check)
                      (et:process--expr expr))
                 (et-error-boundary pos
                   (push (et:check--result->type (et:check-check expr nil nil))
                         check-results)))))

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

    (nreverse check-results)))


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

(et-defun et-process-directory (dir: String &rest args) *et:result<Any>
  (interactive (list (read-directory-name "DProcess Directory: ") :eval t))
  (et-result-boundary
   (let* ((files (list (expand-file-name dir))))
     (while-let ((file (pop files)))
       (cond
        ((file-directory-p file)
         (cl-callf append files (directory-files file t "^[^.]")))
        ((file-regular-p file)
         (et-error-boundary nil
           (apply #'et-process-exprs (et-process-file-exprs file) args))))))))


;;; ============================================================
;;; Core control flow - `et:flow'
;;;; Check reading a variable

(et-defun et:flow-check-var (var: EtVar) EtType
  "Check a variable.

This exists as a helper function so that it can be advised in order to
intercept all accesses to a variable when trying to determine a suitable
type for it."
  (et-add-typeof (et:check-var-type var) var))

(et-define-macro-checker $var (var)
  `(et:check-var-type ,var))

(et-define-macro-checker $symbol-value (sym-chk)
  `(pcase (et-type-single (et-chk ,sym-chk))
     ((cl-struct et:type-dt (name 'Literal) (args `(,(and sym (pred symbolp)))))
      (et-check-expr (et-cur-recommendation) sym nil))
     (_ (error "Unable to resolve single variable symbol"))))


;;;; Check setting a variable

(et-defun et:flow-check-set (var: EtVar type: EtType) EtType
  "Should be called whenever VAR is assigned to TYPE."
  (if (not (et-subtype? type (et:type-var->type var)))
      (et-err nil "Variable %s is not assignable to type %s" var type)
    (let* ((narrow (et-reify-type type)))
      ;; If TYPE is assignable to var, but the reified version is not,
      ;; then just keep the variable as-is
      (when (et-subtype? narrow (et:type-var->type var))
        (et-update-var-narrow var narrow))))
  type)

(et-define-macro-checker $symbol-set (sym-chk type-chk)
  `(pcase (et-type-single (et-chk ,sym-chk))
     ((cl-struct et:type-dt (name 'Literal) (args `(,(and sym (pred symbolp)))))
      (if-let* ((var (et-get-symbol-var sym))
                (val (et-with-recommendation (et:check-var-type var)
                       (et-chk ,type-chk))))
          (et:flow-check-set var val)
        (et-fatal nil "Variable not in scope: %s" sym)))
     (_ (et-fatal 0 "Unable to resolve single variable symbol for set"))))


;;;; Check places

(et-declare
 (@alias EtCheckedPlace (Cons EtType fn1<EtType>)))

(et-defun et:flow-check-place (expr: Sexp) EtCheckedPlace
  (if (symbolp expr)
      (if-let* ((var (et-get-symbol-var expr)))
          (cons (et:flow-check-var var)
                (lambda (newtype)
                  (et:flow-check-set var newtype)))
        (et-fatal nil "Variable not in scope: %s" expr))
    (if-let* ((fn (when (consp expr) (get (car expr) 'et-place-checker))))
        (apply fn (cdr expr))
      (et-fatal nil "Not a place: %s" (or (car-safe expr) expr)))))

(put 'plist-get 'et-place-checker
     (lambda (plist prop &optional _pred)
       (pcase-let* ((pt (et-check-expansion nil prop))
                    (`(,cur . ,update) (et:flow-check-place plist)))
         (cons (or (et-infer cur [V] (PlistLike ,pt V) V)
                   (et-fatal nil "Expected plist, got %s" cur))
               (lambda (vt)
                 (funcall update (et PlistPut ,cur ,pt ,vt)))))))

(put 'alist-get 'et-place-checker
     (lambda (key alist &optional _def _remove _pred) ; todo
       (pcase-let* ((kt (et-check-expansion nil key))
                    (`(,cur . ,update) (et:flow-check-place alist)))
         (cons (or (et-infer cur [V] (Alist ,kt V) V)
                   (et-fatal nil "Expected alist, got %s" cur))
               (lambda (vt)
                 (funcall update (et AlistPut ,cur ,kt ,vt)))))))

(put 'car 'et-place-checker
     (lambda (cons) ; todo
       (pcase-let* ((`(,cur . ,update) (et:flow-check-place cons)))
         (cons (or (et-infer cur [AW] (ConsRW Any AW Any Never) AW)
                   (et-fatal nil "Expected cons, got %s" cur))
               (lambda (car)
                 (funcall update
                          (et infer ,cur [DR DW] (ConsRW Any Never DR DW)
                              (ConsRW ,car ,car DR DW) Never)))))))

(put 'cdr 'et-place-checker
     (lambda (cons) ; todo
       (pcase-let* ((`(,cur . ,update) (et:flow-check-place cons)))
         (cons (or (et-infer cur [DW] (ConsRW Any Never Any DW) DW)
                   (et-fatal nil "Expected cons, got %s" cur))
               (lambda (cdr)
                 (funcall update
                          (et infer ,cur [AR AW] (ConsRW AR AW Any Never)
                              (ConsRW DR DW ,cdr ,cdr) Never)))))))

;; V var
;; (AList Symbol (PList :prop Integer :other String))
;; V (alist-get 'key var)
;; Nil|(PList :key Integer :other String)
;; V (plist-get (alist-get 'key var) :prop)
;; ERR - plist may not exist


;;;; Check a variable binding, determining the variable type

(et-define-macro-checker $infer-binds (inits body)
  "Check an expression with a variable bound to the best estimated type.

This will first perform a trial run of BODY to determine a good guess
for what SYM should be bound to. Then, it will check BODY again with SYM
bound to that type.

TODO: Only perform estimation for certain types, such as literals and
fresh types."
  (et-declare (inits Alist<Var_EtCheckFn>) (body EtChk))
  `(et-with-binds (et:flow--guess-var-types ,inits (lambda () (et-chk ,body)))
     (et-chk ,body)))

(et-defun et:flow--guess-var-types (inits: Alist<Var_EtCheckFn> body: EtCheckFn) Alist<Var_EtType>
  (if et:check-preliminary-pass?
      ;; Prevent exponential blow-ups in time complexity due to nested bindings
      (cl-loop for (sym . fn) in inits
               collect (cons sym (et-reify-type (funcall fn))))

    (let* ((polys
            (cl-loop for (sym . _) in inits
                     collect (list (et:flow--new-poly-name
                                    (format "%sType" sym)))))
           (var-uses
            (et:type--with-polymorphs polys
              (et:check-preliminary-pass
               (cl-loop for (sym . fn) in inits
                        for (poly) in polys
                        for init-type = (et-reify-type (funcall fn))
                        for var =
                        (et:type-var-new
                         :name sym
                         :type
                         (et-supersect init-type
                                       (et-parse-type poly)))
                        collect (cons var (cons nil nil))))))
           (get-advice
            (lambda (var)
              (when-let* ((entry (assq var var-uses))
                          (rec (et-cur-recommendation)))
                (push rec (cadr entry)))))
           (set-advice
            (lambda (var type)
              (when-let* ((entry (assq var var-uses)))
                (push type (cddr entry))))))

      (et:type--with-polymorphs polys
        (et:check-preliminary-pass
         (et-with-advice #'et:flow-check-var :before get-advice
           (et-with-advice #'et:flow-check-set :before set-advice
             (et-with-vars (mapcar #'car var-uses)
               (funcall body))))))

      (cl-loop
       for (sym . fn) in inits
       for (_ . solved) in
       (et:type--with-polymorphs polys
         (et:flow--solve-var-types var-uses polys))
       for reified = (et:algebra-simplify-type (et-reify-type solved))
       for type = (et-with-recommendation reified (funcall fn))
       for final = (et:algebra-unfreshen-type
                    (et-union type reified))
       collect (cons sym final)))))

(defun et:flow--new-poly-name (name)
  "Return an interned polymorph name based on NAME that is not in use."
  (let* ((base (format "%s" name))
         (candidate (intern base))
         (suffix 0))
    (while (et:type-is-polymorph? candidate)
      (setq suffix (1+ suffix)
            candidate (intern (format "%s%d" base suffix))))
    candidate))

(defun et:flow--solve-var-types (var-recs polys)
  "Solve the temporary polymorphs in VAR-RECS.

POLYS contains one temporary polymorph for each entry in VAR-RECS, in
the same order.  Each entry has the form (VAR GETS . SETS).  Prefer a
type which is a subtype of every recommendation in GETS and a supertype
of the initializer and every type in SETS.  If those bounds are
incompatible, keep the lower bound: assignments are facts, whereas get
recommendations are contextual hints.

References to another entry's polymorph are enlarged with that entry's
solution.  A nested reference to the entry currently being solved
becomes a recursive alias."
  (let* ((poly-names (mapcar #'car polys))
         (count (length var-recs))
         (states (make-vector count nil))
         (solutions (make-vector count nil))
         (recursive (make-vector count nil))
         (loop-names
          (cl-loop
           for poly in poly-names
           collect
           (let* ((base (format "@FlowLoop-%s" poly))
                  (candidate (intern base))
                  (suffix 0))
             (while (get candidate 'et-alias)
               (setq suffix (1+ suffix)
                     candidate (intern (format "%s-%d" base suffix))))
             candidate)))
         (loop-refs
          (mapcar
           (lambda (name)
             (et:type-new
              :cases
              (list (et:type-case-new
                     :value (et:type-alias-new :name name :args nil)))))
           loop-names)))

    (cl-labels
        ((raw-union (types)
           ;; Do not simplify a union containing a not-yet-defined loop
           ;; alias.  It will be simplified after the alias is defined.
           (setq types (delq nil types))
           (cond
            ((null types) (et-never))
            ((null (cdr types)) (car types))
            (t (et:type-new
                :cases (apply #'append
                              (mapcar (lambda (type)
                                        (copy-sequence (et:type->cases type)))
                                      types))))))

         (rewrite-value (value owner depth)
           (pcase value
             ((cl-struct et:type-dt name args)
              (et:type-dt-new
               :name name
               :args
               (cl-loop for arg in args
                        for role in (et:dt-arg-roles name args)
                        collect (if (eq role 'CONST) arg
                                  (rewrite-type arg owner (1+ depth))))))
             ((cl-struct et:type-alias name args)
              (et:type-alias-new
               :name name
               :args (mapcar (lambda (arg)
                               (rewrite-type arg owner (1+ depth)))
                             args)))
             (_ (error "Invalid type value: %s" value))))

         (rewrite-case (case owner depth)
           (let* ((case-polys (et:type-case->polymorphs case))
                  (owned-polys
                   (cl-remove-if-not
                    (lambda (poly) (memq poly poly-names))
                    case-polys))
                  (base
                   (et:type-new
                    :cases
                    (list
                     (et-copy-with
                      case
                      :value (rewrite-value (et:type-case->value case)
                                            owner depth)
                      :polymorphs
                      (cl-set-difference case-polys owned-polys :test #'eq)))))
                  additions
                  has-self-reference)
             (dolist (poly owned-polys)
               (let ((idx (cl-position poly poly-names :test #'eq)))
                 (cond
                  ;; The top-level polymorph on a variable's initializer
                  ;; marks that variable; it is not itself a recursive use.
                  ((and (= idx owner) (= depth 0)))
                  ((= idx owner)
                   (aset recursive owner t)
                   (setq has-self-reference t)
                   (push (nth owner loop-refs) additions))
                  (t
                   (push (solve idx) additions)))))
             (let ((types (cons base (nreverse additions))))
               (if has-self-reference (raw-union types)
                 (apply #'et-union types)))))

         (rewrite-type (type owner depth)
           (raw-union
            (mapcar (lambda (case) (rewrite-case case owner depth))
                    (et:type->cases type))))

         (normalize-loop-reference (type name reference)
           ;; Once NAME denotes the complete recursive solution, Nil | NAME
           ;; is just NAME when Nil is already a root case of that solution.
           ;; Removing that redundant nested Nil lets the ordinary canonical
           ;; loop matcher recognize both List and &List.
           (let* ((cases (et:type->cases type))
                  (has-reference
                   (cl-some
                    (lambda (case)
                      (pcase (et:type-case->value case)
                        ((cl-struct et:type-alias (name n) (args (pred null)))
                         (eq n name))))
                    cases))
                  (only-reference-or-nil
                   (cl-every
                    (lambda (case)
                      (pcase (et:type-case->value case)
                        ((cl-struct et:type-alias (name n) (args (pred null)))
                         (eq n name))
                        ((cl-struct et:type-dt
                                    (name 'Literal)
                                    (args (and args (guard (equal args '(nil))))))
                         t)
                        (_ nil)))
                    cases)))
             (if (and has-reference only-reference-or-nil)
                 reference
               (raw-union
                (mapcar
                 (lambda (case)
                   (et:type-new
                    :cases
                    (list
                     (et-copy-with
                      case
                      :value
                      (pcase (et:type-case->value case)
                        ((cl-struct et:type-dt (name dt-name) args)
                         (et:type-dt-new
                          :name dt-name
                          :args
                          (cl-loop for arg in args
                                   for role in (et:dt-arg-roles dt-name args)
                                   collect
                                   (if (eq role 'CONST) arg
                                     (normalize-loop-reference
                                      arg name reference)))))
                        ((cl-struct et:type-alias (name alias-name) args)
                         (et:type-alias-new
                          :name alias-name
                          :args
                          (mapcar (lambda (arg)
                                    (normalize-loop-reference
                                     arg name reference))
                                  args))))))))
                 cases)))))

         (canonical-list-loop (definition reference)
           (when (= (length (et:type->cases definition)) 2)
             (let* ((nil-case
                     (cl-find-if
                      (lambda (case)
                        (pcase (et:type-case->value case)
                          ((cl-struct et:type-dt
                                      (name 'Literal)
                                      (args (and args
                                                 (guard (equal args '(nil))))))
                           t)))
                      (et:type->cases definition)))
                    (cons-case
                     (cl-find-if (lambda (case) (not (eq case nil-case)))
                                 (et:type->cases definition))))
               (when (and nil-case cons-case)
                 (pcase (et:type-case->value cons-case)
                   ((cl-struct et:type-dt (name 'ConsFresh)
                               (args `(,elem ,tail)))
                    (when (equal tail reference)
                      (et-alias 'List (et:algebra-simplify-type elem))))
                   ((cl-struct et:type-alias
                               (name (and name (or 'Cons '&Cons)))
                               (args `(,elem ,tail)))
                    (when (equal tail reference)
                      (et-alias (if (eq name '&Cons) '&List 'List)
                                (et:algebra-simplify-type elem))))
                   ((cl-struct et:type-dt (name 'ConsRW)
                               (args `(,read ,write ,tail-read ,tail-write)))
                    (when (equal tail-read reference)
                      (cond
                       ((and (et-never-p write) (et-never-p tail-write))
                        (et-alias '&List (et:algebra-simplify-type read)))
                       ((and (equal read write)
                             (equal tail-read tail-write))
                        (et-alias 'List
                                  (et:algebra-simplify-type read)))))))))))

         (canonicalize-loop (idx definition)
           (let* ((name (nth idx loop-names))
                  (reference (nth idx loop-refs)))
             (setq definition
                   (normalize-loop-reference definition name reference))
             (or
              (canonical-list-loop definition reference)
              (progn
                (et:type-defalias name [] definition nil)
                (put name 'et-loop-alias definition)
                ;; This creates the same stable, digest-named aliases used
                ;; for other non-list recursive types in et.el.
                (et:algebra-unfreshen-type reference)))))

         (contains-fresh-p (type)
           ;; A Fresh datatype is an inferred shape whose write semantics
           ;; may still be selected by a recommendation on the second pass.
           (cl-some
            (lambda (case)
              (pcase (et:type-case->value case)
                ((cl-struct et:type-dt name args)
                 (or (memq name '(ConsFresh VectorFresh HashTableFresh))
                     (cl-some (lambda (arg)
                                (and (et:type-p arg)
                                     (contains-fresh-p arg)))
                              args)))
                ((cl-struct et:type-alias name args)
                 (ignore name)
                 (cl-some (lambda (arg)
                            (and (et:type-p arg)
                                 (contains-fresh-p arg)))
                          args))))
            (et:type->cases type)))

         (solve (idx)
           (pcase (aref states idx)
             ('done (aref solutions idx))
             ('solving
              ;; This case is only needed for a cycle through another
              ;; variable.  Direct self references are handled above.
              (aset recursive idx t)
              (nth idx loop-refs))
             (_
              (aset states idx 'solving)
              (let* ((entry (nth idx var-recs))
                     (var (car entry))
                     (gets (cadr entry))
                     (sets (cddr entry))
                     (initial (et:type-var->type var))
                     ;; The initializer is an assignment too.  Rewrite
                     ;; the lower and upper bounds separately: flattening
                     ;; GETS and SETS together would lose their variance.
                     (lower (rewrite-type
                             (apply #'et-union initial sets) idx 0))
                     (fresh-lower-p (contains-fresh-p lower))
                     (uppers (mapcar (lambda (type)
                                       (rewrite-type type idx 0))
                                     gets))
                     ;; Install a recursive lower bound before comparing it
                     ;; with its upper bounds.  Besides making ordinary
                     ;; subtype checks possible, this gives the structural
                     ;; Fresh check below a closed type to operate on.
                     (lower-check
                      (if (aref recursive idx)
                          (canonicalize-loop idx lower)
                        (et:algebra-simplify-type lower)))
                     (upper (and uppers
                                 (condition-case nil
                                     (apply #'et-supersect uppers)
                                   (error nil))))
                     (ideal-p
                      (lambda (candidate)
                        ;; A failed or unsupported subtype comparison only
                        ;; makes this candidate unsuitable; inference-bound
                        ;; solving itself must never fail.
                        (condition-case nil
                            (and (et-subtype? lower-check candidate)
                                 (cl-every
                                  (lambda (bound)
                                    (et-subtype? candidate bound))
                                  uppers))
                          (error nil))))
                     (fresh-ideal-p
                      (lambda (candidate)
                        ;; A preliminary Fresh type has fixed read shape but
                        ;; deliberately undetermined write semantics.  If its
                        ;; shape fits CANDIDATE after both sides are freshened,
                        ;; the second checking pass may use CANDIDATE to choose
                        ;; those writes.  Do not apply this relaxation to an
                        ;; already-fixed RW lower bound.
                        (and fresh-lower-p
                             (condition-case nil
                                 (and
                                  (et-subtype?
                                   (et:algebra-freshen-type lower-check)
                                   (et:algebra-freshen-type candidate))
                                  (cl-every
                                   (lambda (bound)
                                     (et-subtype? candidate bound))
                                   uppers))
                               (error nil)))))
                     (acceptable-p
                      (lambda (candidate)
                        (or (funcall ideal-p candidate)
                            (funcall fresh-ideal-p candidate))))
                     (rewritten
                      (cond
                       ((null uppers) lower-check)
                       ((funcall ideal-p lower-check) lower-check)
                       ;; `et-supersect' is deliberately an approximate
                       ;; intersection, so verify it against every actual
                       ;; upper bound before accepting it.
                       ((and upper (funcall acceptable-p upper)) upper)
                       ;; One of the original recommendations is sometimes
                       ;; a better representable intersection than
                       ;; `et-supersect'.  Prefer the broadest one which
                       ;; really satisfies all constraints.
                       ((cl-find-if acceptable-p uppers))
                       ((let ((intersection
                               (condition-case nil
                                   (apply #'et-subsect uppers)
                                 (error nil))))
                          (and (funcall acceptable-p intersection)
                               intersection)))
                       ;; No representable ideal solution was found.  Sets
                       ;; (including the initializer) describe values which
                       ;; really occur, so preserving their union is the
                       ;; safest useful best effort.
                       (t lower-check)))
                     (solution
                      (if (eq rewritten lower-check) lower-check
                        (et:algebra-simplify-type rewritten))))
                (aset solutions idx solution)
                (aset states idx 'done)
                solution)))))

      (cl-loop for entry in var-recs
               for idx upfrom 0
               collect (cons (car entry) (solve idx))))))


;;;; Check branches

(et-define-macro-checker [$branches et-check-branches] (&rest branches)
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
       ,@(cl-loop for chk in branches
                  collect
                  `(et-with-narrows (et-cur-narrows)
                     (let* ((type (et-chk ,chk)))
                       ;; If the case returns never, assume that it will never exit
                       (unless (et-never-p type)
                         (push type ,types-var)
                         (push (et-cur-narrows) ,narrows-var)))))

       (et-set-narrows (when ,narrows-var (cl-reduce #'et:flow--narrows-or ,narrows-var)))
       (apply #'et-union ,types-var))))

(et-defun et:flow--narrows-or (a: EtNarrows b: EtNarrows) EtNarrows
  (cl-loop for (var . t1) in a
           for t2 = (alist-get var b)
           when t2 collect (cons var (et:algebra-simplify-type (et-union t1 t2)))))


;;;; Check loop

(et-define-macro-checker $loop (body)
  `(et-check-loop (lambda () (et-chk ,body))))

(et-defun et-check-loop (fn: EtCheckFn) EtType
  "Check BODY-FN, a block that may execute zero or more times.

Pass 1 (diagnostics discarded via `et-result-boundary') discovers the
vars the body can change. Pass 2 checks the body for real, with those
vars' narrows dropped, so iteration N never trusts a narrow iteration
N-1 may have broken. Afterward, the active narrows holds the join of the
zero-iteration path and the after-an-iteration path.

Returns the body's type (from the real pass)."
  (let* ((entry (et-cur-narrows))
         (dirty (et-with-narrows entry
                  (et:check-preliminary-pass (funcall fn))
                  (et:flow--narrows-changed-vars entry (et-cur-narrows)))))
    (et-set-narrows (cl-remove-if (lambda (n) (memq (car n) dirty)) entry))
    (prog1 (funcall fn)
      ;; Exit: 0 iterations (entry) OR >=1 iterations (current narrows).
      (et-set-narrows (et:flow--narrows-or entry (et-cur-narrows))))))

(et-defun et:flow--narrows-changed-vars (before: EtNarrows after: EtNarrows) List<EtVar>
  "Vars whose narrow entry differs between BEFORE and AFTER."
  (cl-loop for var in (seq-uniq (mapcar #'car (append before after)) #'eq)
           unless (equal (alist-get var before) (alist-get var after))
           collect var))


;;;; Check escapable

(et-define-macro-checker $escapable (body)
  `(et-check-escapable (lambda () (et-chk ,body))))

(et-defun et-check-escapable (fn: EtCheckFn) EtType
  "Check BODY-FN, a block that may be exited nonlocally at any point.

Code after the enclosing form (e.g. `catch', `with-local-quit') runs
whether BODY-FN completed or aborted partway through, so the only narrows
that survive are the entry narrows on vars the body never touches: a
narrow the body created (or an entry narrow on a var the body assigns)
cannot be trusted, since the exit may have happened before or after the
change.

Returns the body's type."
  (let* ((entry (et-cur-narrows))
         (type (funcall fn))
         (dirty (et:flow--narrows-changed-vars entry (et-cur-narrows))))
    (et-set-narrows (cl-remove-if (lambda (n) (memq (car n) dirty)) entry))
    type))


;;;; Check deferred

(et-define-macro-checker [$deferred et-check-deferred] (body)
  "Check BODY as code that runs at some later, unknown time.

It sees none of the current flow's narrows, and the narrows it produces
do not escape into the current flow."
  `(et-with-narrows nil (et-chk ,body)))


;;; ============================================================
;;; Control flow helpers - `et:helpers'
;;;; Custom environments

(et-defstruct et-env
  "An environment in which to check an expression.

Contains things like binds, function binds, and narrows in which an
expression can be evaluated. This is used for defining checkers like
let, cl-flet, pcase, and anything else that binds variables or
functions."
  (narrows nil :et EtNarrows)
  (binds nil :et Alist<Var_EtType>)
  (fbinds nil :et Alist<Var_EtType>))

(et-define-macro-checker $env (env chk)
  (let* ((envsym (gensym "env")))
    `(let* ((,envsym ,env))
       (et-with-binds (et-env->binds ,envsym)
         (et-with-fbinds (et-env->fbinds ,envsym)
           ;; If any of the narrows are the never type, then don't even bother
           ;; checking the inner expression, and just return never
           (if (cl-loop for (_ . type) in (et-env->narrows ,envsym)
                        thereis (et-never-p type))
               (et-never)
             (et-with-narrows (et-env->narrows ,envsym)
               (et-chk ,chk))))))))

(et-define-macro-checker $envs (envs &rest branches)
  (let* ((envs-sym (gensym "env")))
    `(let* ((,envs-sym ,envs))
       (et-check-branches
        ,@(cl-loop for b in branches
                   for idx upfrom 0
                   collect `($env (nth idx ,envs-sym) ,b))))))


;;;; Chk bindings

(et-define-macro-checker $bind (&rest forms)
  "Bind a collection of variables.

The beginning of FORMS are [VAR CHK] or [VAR SYMBOL CHK]. If SYMBOL is
provided, the variable will be made available in the local scope bound
to that symbol."
  (pcase (car forms)
    ((pred (not vectorp)) `(et-chk ,@forms))
    (`[,(and var (pred symbolp)) ,symbol ,chk]
     `(let* ((,var (et:type-var-new :name ,symbol :type (et-reify-type (et-chk ,chk)))))
        (et-with-vars (list ,var) (et-chk ($bind ,@(cdr forms))))))
    (`[,(and var (pred symbolp)) ,chk]
     `(let* ((,var (et:type-var-new :name ',var :type (et-reify-type (et-chk ,chk)))))
        (et-chk ($bind ,@(cdr forms)))))
    (_ (error "Invalid $bind format: %s" forms))))

(et-define-macro-checker $cl-bind (pattern type-chk body-chk)
  `(et-with-binds (et:helpers--cl-destructure ,pattern (et-chk ,type-chk))
     (et-chk ,body-chk)))

(et-defun et:helpers--cl-destructure (pat: Sexp type: EtType) Alist<Var_EtType>
  (pcase pat
    ('nil nil)
    ((and (pred symbolp) (guard (not (eq pat t)))) (list (cons pat type)))
    (`(&optional . ,rest) (et:helpers--cl-destructure rest type))
    (`(&rest ,rest . ,more)
     (when more (error "Multiple parameters after &rest"))
     (et:helpers--cl-destructure rest))
    (`(,car . ,cdr)
     (let* ((res (et:algebra-infers (et-parse-matcher '&Cons<A_B> [A B]) type
                                    (list (et-parse-repr 'A '(A B)) (et-parse-repr 'B '(A B)))))
            (val (et:match-result->value res)))
       (if (et:match-result->success res)
           (append (et:helpers--cl-destructure car (car val))
                   (et:helpers--cl-destructure cdr (cadr val)))
         (error "Cl destructure does not match. Expected cons, found %s" type))))
    (other (error "Invalid cl destructure pattern: %s" other))))


;;;; Chk narrows

(et-define-macro-checker $when-is (type-chk is-chk &rest body-chks)
  `(et-when-type (et-supersect (et-chk ,type-chk) (et-chk ,is-chk))
     (et-chk ,@body-chks)))

(et-define-macro-checker $must-be (type-chk is-chk)
  (let* ((typesym (gensym "type")))
    `(let* ((,typesym (et-supersect (et-chk ,type-chk) (et-chk ,is-chk))))
       (if (et-never-p ,typesym) (et-fatal nil "Unexitable") ;TODO
         (et-apply-type-narrows ,typesym))
       (et Nil))))


;;;; Chk utils

(et-define-macro-checker $let (&rest args)
  (cl-loop while (vectorp (car args))
           collect `(,(aref (car args) 0) (et-chk ,(aref (pop args) 1))) into lets
           finally return
           `(let* ,lets (et-chk ,@args))))

(et-define-macro-checker $eval (expr &rest exprs)
  `(progn ,expr ,@exprs))

(et-define-macro-checker $if-eval (cond then-chk &rest else-chks)
  `(if ,cond (et-chk ,then-chk) (et-chk ,@else-chks)))

(et-define-macro-checker $type (spec) (et-parse-type spec))
(et-define-macro-checker $nil () (et Nil))

(et-define-macro-checker $ignore (&rest chks)
  `(et-with-recommendation nil (et-chk ,@chks)))

(et-define-macro-checker $with-rec (rec-chk &rest body-chks)
  `(et-with-recommendation (et-chk ,rec-chk) (et-chk ,@body-chks)))

(et-define-check-shortcut $recurse (rest)
  `($exp (cons (car (et-cur-expr)) ,rest)))

(et-define-macro-checker $expect (expected-chk chk)
  (let* ((typesym (gensym "type"))
         (expsym (gensym "expected")))
    `(let* ((,expsym (et-chk ,expected-chk))
            (,typesym (et-with-recommendation ,expsym (et-chk ,chk))))
       (unless (et-subtype? ,typesym ,expsym)
         (et-fatal nil "Expected %s, found %s" ,expsym ,typesym))
       ,typesym)))

(et-define-macro-checker $infer (gv match out chk)
  (let* ((typesym (gensym "type")))
    `(let* ((,typesym (et-chk ,chk)))
       (or (et:match-result->value
            (et:algebra-infer (et-parse-matcher ',match ,gv)
                              ,typesym
                              (et-parse-repr ',out (et-genvec-generics ,gv))))
           (et-fatal nil "Expected %s, found %s" ',match ,typesym)))))

(et-define-macro-checker $progn (first-chk &rest chks)
  (cl-loop with all = (cons first-chk chks)
           for chk in (butlast all)
           collect `(et-chk ($ignore ,chk)) into exprs
           finally return `(progn ,@exprs (et-chk ,(car (last all))))))

(et-define-macro-checker $prog1 (first-chk &rest chks)
  (cl-loop for chk in chks
           collect `(et-chk ($ignore ,chk)) into exprs
           finally return `(prog1 (et-chk ,first-chk) ,@exprs)))

(et-define-macro-checker $tail-tuple (&rest first-path)
  (let* ((path (flatten-list first-path))
         (parent (et-cur-expr (butlast path)))
         (start (or (car (last path)) 1)))
    (et-tuple
     '&Cons
     (cl-loop for pos from start below (length (et-cur-expr))
              collect (et-chk ($at pos))))))


;;;; Conditionals

(et-define-check-shortcut $if (cond-chk then-chk else-chk)
  `($let [cond ,cond-chk]
         ($branches
          ($when-is ($eval cond) ($type NonNil) ,then-chk)
          ($when-is ($eval cond) ($type Nil) ,else-chk))))

(et-define-check-shortcut $or (cond-chk else-chk)
  `($let [cond ,cond-chk]
         ($branches
          ($let [success ($eval (et-supersect cond (et NonNil)))]
                ($if-eval
                 (et-never-p success) ($eval success)
                 ($eval
                  (et-apply-type-narrows success)
                  success)))
          ($when-is ($eval cond) ($type Nil) ,else-chk))))

(et-define-check-shortcut $and (cond-chk then-chk)
  `($if ,cond-chk ,then-chk ($type Nil)))


;;;; Chk root level expr

(et-define-macro-checker $pcase (&rest repls)
  "Replace the expression, then typecheck the replacement.

The replacement can also be just a single type, in which case that type
will be the result."
  `(pcase (cdr (et-cur-expr))
     ,@(cl-loop for (pat chk) on repls by #'cddr
                collect `(,pat (et-chk ,chk)))
     (_ (et-fatal nil "Invalid `%s' format" (car (et-cur-expr))))))

(et-define-macro-checker $arglist (arglist &rest chks)
  `(cl-destructuring-bind ,arglist (cdr (et-cur-expr))
     (et-chk ,@chks)))

(et-define-macro-checker $expand ()
  `(if (macrop (car (et-cur-expr)))
       (et-check-expansion nil (macroexpand-1 (et-cur-expr)))
     (et-fatal 0 "Macro not defined: %s" (car (et-cur-expr)))))

(et-define-check-shortcut $body (&rest specs)
  `($progn
    ,@(cl-loop for spec in specs
               for pos upfrom 1
               collect `($expect ($type ,spec) ($at ,pos)))
    ($tail ,(1+ (length specs)))))

(et-define-check-shortcut $fn (&rest specs)
  `($progn
    ,@(cl-loop for spec in (butlast specs)
               for pos upfrom 1
               collect `($expect ($type ,spec) ($at ,pos)))
    ($type ,(car (last specs)))))

(et-define-check-shortcut $fn-type (args ret)
  `($progn
    ($expect ($type ,args)
             ($tail-tuple 1))
    ($type ,ret)))


;;;; Keyword checkers

(defmacro et:helper--define-utility-checker (kwd arglist &rest body)
  (declare (indent 2))
  `(et-set-checker
    ,kwd
    (lambda ()
      (apply
       (et-defun ,(intern (format "et:helpers--check-%s" kwd)) ,arglist EtType ,@body)
       (cdr (et-cur-expr))))))

(et:helper--define-utility-checker :type (spec: EtSpec)
  (et-parse-type spec))

(et:helper--define-utility-checker :eval (expr: Sexp)
  (et-hint nil (et-pp (eval expr t)))
  (et Nil))

(et:helper--define-utility-checker :var (var: EtVar)
  (et:check-var-type var))

;; Hinting

(et:helper--define-utility-checker :pp (expr: Sexp)
  (et-hint nil "%s" (eval expr t))
  (et Nil))

(et:helper--define-utility-checker :narrows ()
  (et-hint nil (et-pp-narrows (et-cur-narrows) "   "))
  (et Nil))

(et:helper--define-utility-checker :assert (expr: Sexp)
  (if (eval expr t) (et Nil)
    (et-fatal nil "Returned nil")))

(et:helper--define-utility-checker :assert-type= (expr: Sexp spec: EtSpec)
  (let* ((expr-type (et-reify-type (et-chk ($exp expr))))
         (spec-type (et-parse-type spec)))
    (unless (et-type=? expr-type spec-type)
      (et-err nil "Expected %s, found %s" spec-type expr-type))
    (et Nil)))

(et:helper--define-utility-checker :assert-is? (expr: Sexp spec: EtSpec)
  (let* ((expr-type (et-reify-type (et-chk ($exp expr))))
         (spec-type (et-parse-type spec)))
    (unless (et-subtype? expr-type spec-type)
      (et-err nil "Expected %s, found %s" spec-type expr-type))
    (et Nil)))

(et:helper--define-utility-checker :assert-error (_expr: Sexp)
  (condition-case _err (et-chk ($at 1))
    (error (et Nil))
    (:success (et-fatal nil "Didn't error"))))

(et:helper--define-utility-checker :typeof (_expr: Sexp &optional id)
  (let* ((type (et-chk ($at 1))))
    (if (null id) (et-hint nil type)
      (et-with-diagnostic-prefix id (et-hint nil type)))
    type))

(et:helper--define-utility-checker :typeof+ (_expr &optional id)
  (let* ((type (et-chk ($at 1)))
         (et-print-labels t)
         (et-print-narrows t))
    (et-with-diagnostic-prefix id (et-hint nil type))
    type))

(et:helper--define-utility-checker :expand (&rest spec: EtSpec)
  (let* ((type (et-expand-aliases (et-parse-type spec))))
    (et-hint nil type)
    type))


;;; ============================================================
;;; Function def handling - `et:func'
;;;; Create function type

(defun et-create-function-type (generics constraints input-repr output-repr)
  "Build a Function or PolyFunction type from reprs."
  (et-declare (generics &List<EtGeneric>)
              (constraints &List<EtTypeConstraint>)
              (input-repr EtRepr)
              (output-repr EtRepr)
              (@return EtType))

  (if generics
      (et-dt 'PolyFunction
             (et:match-matcher-new :generics generics
                                   :constraints constraints
                                   :repr input-repr)
             output-repr)
    (et-dt 'Function
           (et-repr-to-type input-repr nil)
           (et-repr-to-type output-repr nil))))


;;;; Parse arglist params

(et-defun et-parse-arglist (arglist: &List<Var>) EtFuncParameters<Var>
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

(et-defun et-find-and-parse-func-decls (params: EtFuncParameters<Var> exprs: &List<Sexp>)
          Nil|EtFuncDeclarations
  (when-let* ((declare-path (et:func--declares-path exprs)))
    (let* ((declares (cdr (et:util-traverse-tree exprs declare-path))))
      (et-at declare-path
        (et-at-offset 1
          (et-parse-func-decls params declares))))))

(et-defun et-parse-func-decls (params: EtFuncParameters<Var> declares: &List)
          Nil|EtFuncDeclarations
  "Parse function declarations.

This assumes that the current path points to DECLARES."
  (let* ((param-list (apply #'append params))
         (param-alist (et: &Alist<Var_EtRepr> nil))
         (generics (et: &List<EtGeneric> nil))
         (constraints (et: &List<EtTypeConstraint> nil))
         (props (et: PlistOf<Var_Any> nil))

         (return-repr (et: Nil|EtRepr nil))
         (function-type (et: Nil|EtType nil))
         (checker (et: (or Nil (fn Nil EtType)) nil))

         (strategy (et: Nil|@return|@function|@checker nil))
         (use-strategy
          (lambda (strat)
            (if (null strategy) (setq strategy strat)
              (unless (eq strat strategy)
                (error "Form not valid for strategy %s" strategy)))))

         (extra-gv nil))

    ;; Find any inline generics and assign them
    (pcase-dolist (`(,var ,type) declares)
      (when (and (memq var param-list) (vectorp type))
        (pcase type
          (`[,var] (push var extra-gv))
          (`[,var ,op ,constr] (push (list var op constr) extra-gv)))))
    (when extra-gv
      (setq extra-gv (apply #'vector (nreverse extra-gv)))
      (setq generics (et-genvec-generics extra-gv)
            constraints (et-genvec-constraints extra-gv)))

    ;; Parse the fields of the declare block
    (dotimes (form-idx (length declares))
      (et-error-boundary form-idx
        (pcase (nth form-idx declares)
          ;; return strategy
          (`(@generics ,(and gv (pred vectorp)))
           (funcall use-strategy 'return)
           (when extra-gv (setq gv (vconcat gv extra-gv)))
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
          ((or `(@check ,chk)
               (and `(,(and sym (pred symbolp) (guard (et-check-macro-p sym))) . ,args)
                    (let chk (cons sym args))))
           (funcall use-strategy 'checker)
           (when checker (et-fatal 0 "Multiple checkers specified"))
           (setq checker (eval `(lambda () (et-chk ,chk)))))

          ;; Props
          (`(@skip)
           (when (plist-get props :skip) (et-fatal 0 "Multiple @skip clauses"))
           (setq props (cl-list* :skip t props)))
          (`(@show . ,show) ; Can contain `narrows', `vars', `all'
           (when (plist-get props :show) (et-fatal 0 "Multiple @show clauses"))
           (setq props (cl-list* :show show props)))

          ;; Parameters
          (`(,(and name (guard (memq name param-list))) ,spec-or-vec)
           (funcall use-strategy 'return)
           (when (alist-get name param-alist) (et-fatal 0 "Param defined multiple times"))
           (pcase spec-or-vec
             ((or `[,spec] `[,spec ,_ ,_] spec)
              (et-at 1 (setf (alist-get name param-alist)
                             (et-parse-repr spec generics))))))

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

(et-defun et-untyped-func-input (params: EtFuncParameters<T>) EtType
  "Guess an input type based only on the (untyped) parameters."
  (et-repr-to-type
   (et:func--params-to-input nil params (lambda (_) (et Any)) #'identity)))

(defun et:func--params-to-input (generics params fn key-fn)
  "Build an input repr from parameter specs.

FN converts a parameter (T) to the repr that should be used for it, and"
  (et-declare (@generics [T])
              (generics &List<EtGeneric>) (params EtFuncParameters<T>)
              (fn Function<T_EtRepr>) (key-fn Function<T_Symbol>)
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
                   finally return (et-parse-repr `(Plist ,@plist-args) generics)))
         ((et-parse-repr 'Nil generics)))))

    (named-let loop ((req req-ps) (opt opt-ps))
      (pcase (list req opt)
        (`((,cur . ,rest) ,_)
         (et-parse-repr (list '&Cons (funcall fn cur) (loop rest opt))
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
                          (&Cons ,(funcall fn cur) ,tail))
                     generics (list :field (funcall key-fn cur))))))
        (_ rest-repr)))))


;;;; Determine param types

(defun et:func--param-types (params func-input-type)
  "Determine the parameter types for a particular function.

If the function has generics, then this MUST be called with the
polymorphic types defined, as those polymorphic types probably appear in
FUNC-INPUT-TYPE."
  (et-declare (params EtFuncParameters<Var>)
              (func-input-type EtType)
              (@return Alist<Var_EtType>))

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
  (declare (indent 2) (et (@with EtType EtFuncParameters<Var>)))
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


;;;; Check function body

(defun et-check-function-body (params func-type body-path)
  "The path should point to the function expr.

Returns the type of the last expression in the body."
  (et-declare (func-type EtType) (params EtFuncParameters<Var>) (body-path &Tree<Integer>)
              (@return EtType))
  (let* ((returns
          (et-in-function-body func-type params
            (let* ((exp-ret et:type-expected-return)
                   (actual-ret (et:algebra-remove-binds
                                (et-chk ($with-rec
                                         ($eval exp-ret)
                                         ($deferred ($tail body-path)))))))
              (or (et-subtype? actual-ret exp-ret)
                  (et-err 0 "Expected %s, found %s" exp-ret actual-ret))
              (et-remove-type-binds-and-polys actual-ret (mapcar #'car polys))))))
    (apply #'et-union returns)))


;;; ============================================================
;;; Identifiers - `et:identify'
;;;; require

(et-set-identifier #'require #'et:identify-require)

(et-defvar et:identify--require-loaded-libs List<Symbol> nil
  "List of libraries which have been processed due to a `require'.")

(et-defun et:identify-require (name: Symbol) EtIdentifyPlist
  (let* ((dir (when buffer-file-name (file-name-parent-directory buffer-file-name)))
         (load-path (cons dir load-path))
         (library (or (locate-library (symbol-name (eval name)))
                      (error "Library `%s' not found" name))))
    (unless (member library et:identify--require-loaded-libs)
      (push library et:identify--require-loaded-libs)
      ;; Process the buffer without propagating diagnostics
      (et-process-exprs (et-process-file-exprs library)))))


;;;; defstruct

(et-set-identifier #'cl-defstruct #'et:identify-cl-defstruct)

(et-defun et:identify-cl-defstruct (&rest body) EtIdentifyPlist
  "Identify a `cl-defstruct' expression, returning a processing plist."
  (let* ((name-or-opts (car body))
         (name (if (consp name-or-opts) (car name-or-opts) name-or-opts))
         (opts (when (consp name-or-opts) (cdr name-or-opts)))
         (conc-name (if-let* ((entry (assq :conc-name opts))) (cadr entry)
                      (intern (format "%s-" name))))
         (constructor (if-let* ((entry (assq :constructor opts))) (cadr entry)
                        (intern (format "make-%s" name))))
         (copier (if-let* ((entry (assq :copier opts))) (cadr entry)
                   (intern (format "copy-%s" name))))
         (predicate (if-let* ((entry (assq :predicate opts))) (cadr entry)
                      (intern (format "%s-p" name))))
         (slots-start (if (stringp (cadr body)) 2 1))
         (slot-forms (nthcdr slots-start body))
         slots genvec genvec-rel generics)

    ;; Parse slots
    (dotimes (slot-idx (length slot-forms))
      (let ((rel (+ 1 slots-start slot-idx)))
        (et-error-boundary rel
          (pcase (nth slot-idx slot-forms)
            ((and s (pred symbolp))
             (push (list rel s nil nil) slots))
            (`(,(and s (pred symbolp)) ,default . ,plist)
             (push (list rel s default
                         (when-let* ((pos (cl-position :et plist))
                                     ((= 0 (mod pos 2))))
                           (cons (+ 3 pos) (nth (1+ pos) plist))))
                   slots)
             (when-let* ((pos (cl-position :et-generics plist))
                         ((= 0 (mod pos 2))))
               (if (> slot-idx 0) (et-err rel "Generics must be set in the first slot")
                 (setq genvec (nth (1+ pos) plist)
                       genvec-rel (list rel (+ 3 pos))
                       generics (et-genvec-generics genvec)))))
            (_ (et-err rel "Invalid slot format"))))))

    (setq slots (nreverse slots))
    (put name 'et-struct (list :generics generics))

    (list
     :constrain
     (lambda ()
       (when genvec
         (et-at genvec-rel
           (plist-put (get name 'et-struct) :constraints
                      (et-genvec-constraints genvec)))))

     :populate
     (lambda ()
       (let* ((plist (or (get name 'et-struct)
                         (et-fatal 0 "Struct `%s' not defined" name)))
              (constraints (plist-get plist :constraints))
              ;; Repr helpers shared across all generated functions
              ;; STRUCT-REPR is the struct's own type, e.g. *Name<T1 T2>
              (struct-repr (et-parse-repr (et-q (Struct ,name ,@generics)) generics))
              ;; ARGLIST-REPR is the single-argument arglist (STRUCT), used by
              ;; accessors and the copier
              (arglist-repr (et-parse-repr (et-q (&Cons ,struct-repr Nil)) generics)))

         ;; --- Predicate ---
         (when predicate
           (let* ((never-args (make-list (length generics) 'Never))
                  (output-repr
                   (et-parse-repr
                    (et-q (or (and True (bindsof (and T (Struct ,name ,@never-args))))
                              (and Nil (bindsof (minus T (Struct ,name ,@never-args))))))
                    '(T))))
             (put predicate 'et-symbol-func-type
                  (et-dt 'PolyFunction (et-parse-matcher '(&tuple T) [T]) output-repr))))

         ;; --- Accessors ---
         (dolist (slot slots)
           (pcase-let* ((`(,rel ,slot-name ,_default ,type-info) slot)
                        (accessor (if conc-name (intern (format "%s%s" conc-name slot-name))
                                    slot-name)))
             (et-error-boundary rel
               (let* ((slot-repr (if type-info
                                     (et-at (car type-info)
                                       (et-parse-repr (cdr type-info) generics))
                                   (et-parse-repr 'Any generics))))
                 (put accessor 'et-symbol-func-type
                      (et-create-function-type generics constraints
                                               arglist-repr slot-repr))))))

         ;; --- Constructor ---
         (when constructor
           (let* ((input-repr
                   (cl-loop for (rel slot-name _default type-info) in slots
                            for slot-repr = (if type-info
                                                (et-error-boundary rel
                                                  (et-at (car type-info)
                                                    (et-parse-repr (cdr type-info) generics)))
                                              (et-parse-repr 'Any nil))
                            nconc (list (intern (format ":%s" slot-name)) slot-repr) into args
                            finally return (et-parse-repr (if args `(&Plist ,@args) 'Nil) nil))))
             (put constructor 'et-symbol-func-type
                  (et-create-function-type generics constraints
                                           input-repr struct-repr))))

         ;; --- Copier ---
         (when copier
           (put copier 'et-symbol-func-type
                (et-create-function-type generics constraints
                                         arglist-repr struct-repr))))))))


;;;; Identify via evaluation

;; If a form of these types is encountered in the file, evaluate it
(et-set-identifier 'et-defalias #'et:identify-via-eval)
(et-set-identifier 'et-defsyntax #'et:identify-via-eval)
(et-set-identifier 'et-defop #'et:identify-via-eval)

(et-defun et:identify-via-eval (&rest _body) EtIdentifyPlist
  (eval et-identifying-expr t)
  nil)


;;;; @variable/defvar

(et-set-identifier #'et-defvar #'et:identify-et-defvar)
(et-set-identifier '@variable #'et:identify-et-defvar)

(et-defun et:identify-et-defvar (name: Var spec: EtSpec &rest _) EtIdentifyPlist
  (list :declare (lambda () (et-set-global-var-type name (et-at 2 (et-parse-type spec))))))


;;;; @check/@def/def*

(et-set-identifier #'defun #'et:identify-defun)
(et-set-identifier #'cl-defun #'et:identify-defun)
(et-set-identifier #'defmacro #'et:identify-defun)

(et-defun et:identify-defun (name: Var arglist: &List &rest rest: Sexps) EtIdentifyPlist
  (list
   :declare
   (lambda ()
     (let* ((params (et-at 2 (et-parse-arglist arglist)))
            (decls (et-at-offset 3 (et-find-and-parse-func-decls params rest))))
       (when decls (et-symbol-set-func-decls name decls))))))

(et-set-identifier '@def #'et:identify-@def)

(et-defun et:identify-@def (names: Var|&Vector<Var> arglist: &List ret: EtSpec &rest declares: Sexps)
          EtIdentifyPlist
  (dolist (name (if (vectorp names) (append names nil) (list names)))
    (et-deferred-declare name
      (let* ((defun-expr (macroexpand-1 `(et-defun ,name ,arglist ,ret (declare (et ,@declares))))))
        (funcall (plist-get (apply #'et:identify-defun (cdr defun-expr)) :declare))))))

(et-set-identifier '@check #'et:identify-@check)

(et-defun et:identify-@check (names: Var|&Vector<Var> chk: Sexp) EtIdentifyPlist
  (list
   :declare
   (lambda ()
     (dolist (name (if (vectorp names) (append names nil) (list names)))
       (et-deferred-declare name
         (et-set-chk name chk))))))


;;;; @alias

(et-set-identifier '@alias #'et:identify-@alias)

(et-defun et:identify-@alias (&rest args) EtIdentifyPlist
  "Identify an alias definition, returning a processing plist.

During identification, declares the alias name and generics.
Returns a plist with :constrain and :populate functions."
  (let* ((name (pop args))
         (_ (or (symbolp name) (et-fatal 1 "Alias name must be a symbol")))
         (genvec (if (vectorp (car args)) (pop args) []))
         (spec (pop args)))

    (et:type-identify-alias name genvec spec args)
    (list
     :constrain
     (lambda ()
       ;; Parse the generic constraints (which reference other types)
       (when genvec (et-at 2 (et:type-constrain-alias name))))

     :populate
     (lambda ()
       ;; Parse the spec into a repr
       (et-at (if genvec 3 2) (et:type-declare-alias name))))))


;;;; @expand/macroexpand

;; Make a symbol identify by identifying its macro expansion
;; This is different from the $expand checker because it
;; applies to identification, not just checking.

(et-set-identifier '@expand #'et:identify-@expand)

(defun et:identify-@expand (symbol)
  (et-set-identifier symbol #'et:identify-macroexpand)
  nil)

(defun et:identify-macroexpand (&rest _body)
  "Identify an expression by identifying its macro expansion."
  (let* ((expanded (macroexpand-1 et-identifying-expr)))
    (et-at 0 (et-with-sticky-path (et-identify-expr expanded)))))


;;;; @symbol-property

(et-set-identifier '@symbol-property #'et:identify-@get)

(et-defun et:identify-@get (&rest body) EtIdentifyPlist
  "Identify a symbol property declaration, returning a processing plist.

During identification, just validates the format.
Returns a plist with :declare to set the symbol type."
  (pcase body
    (`(,(and symbol (pred symbolp)) ,spec)
     (list
      :declare
      (lambda ()
        (et-at 2
          (put symbol 'et-symbol-property-type (et-parse-type spec))))))

    (_ (et-fatal nil "Expected format (@symbol-property SYMBOL TYPE)"))))


;;;; @syntax

(et-set-identifier '@syntax #'et:identify-@syntax)

(et-defun et:identify-@syntax (name arglist &rest body) EtIdentifyPlist
  (eval `(et-defsyntax ,name ,arglist ,@body) t)
  nil)


;;;; @eval

(et-set-identifier '@eval #'et:identify-@eval)

(et-defun et:identify-@eval (&rest body) EtIdentifyPlist
  (eval `(progn ,@body) t)
  nil)


;;; ============================================================
;;; Testing
;;;; Load definitions

(defvar et:check-setup-definitions nil)

(defun et:check-setup-definitions ()
  (unless et:check-setup-definitions
    (setq et:check-setup-definitions t)
    (let* ((lib (file-name-directory (find-library-name "et"))))
      (et-process-directory (expand-file-name "definitions" lib)
                            :eval t))))


;;;; Macros

(defmacro et-is? (expr type)
  `(et-subtype? (et:result->value
                 (et-result-boundary
                  (et:check--result->type (et:check-check ',expr nil nil))))
                (et ,type)))

(defmacro et-typecheck (expr)
  `(et:result->value
    (et-result-boundary
     (et:check--result->type (et:check-check ',expr nil nil)))))

(defmacro et-check (&rest exprs)
  `(et-result-boundary (et-process-exprs ',exprs :check t)))

(defun et-last-sexp ()
  "Typecheck the sexp before the point."
  (interactive)
  (et:check-setup-definitions)
  (let* ((res (et-result-boundary
               (et-process-exprs (list (pp-last-sexp))
                                 :check t))))
    (cl-callf car-safe (et:result->value res))
    (message "%s" (et-pp res))))


;;; ============================================================
;;; Provide

(provide 'et-check)


;;; et-check.el ends here
