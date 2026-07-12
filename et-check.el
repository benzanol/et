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
;;; Bindings
;;;; Symbol bindings

(defvar et--binds nil
  "Stack of (SYMBOL . `et-var').")

(defun et-new-var (name type)
  (make-et-var :name name :type type))

(defun et-get-symbol-var (sym)
  (cl-assert (symbolp sym))
  (alist-get sym et--binds))

(defmacro et-with-vars (vars &rest body)
  "VARS can contain nil values."
  (declare (indent 1))
  `(let* ((vs (cl-loop for var in ,vars
                       when var do (cl-assert (et-var-p var))
                       and collect (cons (et-var-name var) var)))
          (et--binds (append vs et--binds)))
     ,@body))


;;;; Narrowed bindings

(defvar et--narrow-binds nil
  "Stack of (`et-var' . `et-type').")

(defun et-current-var-type (variable)
  (or (alist-get variable et--narrow-binds)
      (et-var-type variable)))

(defun et-get-symbol-type (sym)
  (cl-assert (symbolp sym))
  (when-let ((var (et-get-symbol-var sym)))
    (et-current-var-type var)))

(defmacro et-with-narrow-binds (binds &rest body)
  (declare (indent 1))
  `(let ((et--narrow-binds (append ,binds et--narrow-binds)))
     ,@body))


;;;; Printing narrows

(defun et-pp-narrows (narrows &optional sep)
  (cl-loop for (var . type) in narrows
           collect (format "%s: %s" (et-var-name var) (et-pp type)) into strs
           finally return (string-join strs (or sep "\\n"))))

(defvar et-display-narrows nil
  "Whether to display narrowed types on if/when/etc blocks.")

(defun et-checker-hint-narrows (path &rest types)
  "Display a list of binds to the user at path=(0).

TYPES is (FMT1 TYPE1 FMT2 TYPE2 ...)."
  (when et-display-narrows
    (cl-loop for (fmt type) on types by #'cddr
             for binds = (et--type-binds type) ; TODO: display just binds instead of whole type
             when binds do (et-hint path fmt (et-pp-narrows binds)))))


;;; ============================================================
;;; Checking
;;;; Check

(defvar et--checker-expr nil)

(defun et--check (expr)
  "Generates an `et-result' resulting from typechecking EXPR.

If EXPR is self quoting (a number, string, etc.), the resulting type
will be a literal type representing the literal value.

If EXPR is a symbol VAR, then check if it exists as a variable in the
local scope. Return not just the variable's type, but also {typeof VAR}
so that future calls can perform type narrowing for the variable.

Otherwise, EXPR is (FUNC ARGS...).

If FUNC is a symbol with the `et-checker' property set, the value of
`et-checker' is assumed to be a checker function. A checker function has
no arguments, and runs in an environment with `et--checker-expr' set to
a copy of EXPR. A checker function should set or mutate
`et--checker-expr' in order to remove type annotations or otherwise
perform compilation. The `:compiled' field of the result will be set to
the final value of `et--checker-expr'.

If FUNC is a symbol with the `et-function-type' property set to an
`et-type', then the arguments to the function will first be checked
individually, and then will be passed to `et-funcall' as a list to
determine the output type."
  (let* ((et--checker-expr expr))
    (et-failed-boundary
     (or (et-error-boundary nil
           (or (et--check-1)
               (unless et--result-failed (et-err nil "Type checking failed mysteriously"))))
         (et-never)))))

(defun et--check-1 ()
  (pcase et--checker-expr
    (`(,func . ,_args)
     (pcase nil
       ;; Custom checker
       ((and (let checker (get func 'et-checker)) (guard checker)
             (let output (funcall checker)))
        (if (or (null output) (et-type-p output)) output
          (et-fatal nil "Checker for `%s' had invalid return: %s" func output)))

       ;; Function type property
       ((and (let func-type (get func 'et-function-type)) (guard func-type))
        (let* ((arg-types (cl-loop for type in (et-checker-remaining 1) for pos upfrom 1
                                   collect (et-copy-with type :label (list :position pos))))
               (args-type (et--tuple 'Cons arg-types))
               (output-result (et-funcall func-type args-type)))
          (cond
           ((et-match-result-success output-result) (et-match-result-value output-result))
           ;; If `et--result-failed' is already true, that means one of the arguments was invalid,
           ;; which means the true error was in the arguments, not this call
           (et--result-failed nil)
           ;; The stack in OUTPUT-RESULT is a list of (`sub'/`super' MATCHER TYPE)
           (t
            ;; Find the stack frame corresponding to the position
            (cl-loop
             with (arg-pos arg-type param-name param-repr) = nil
             for (_ mrepr type) in (et-match-result-stack output-result)
             ;; Collect the argument position
             for pos = (plist-get (et-type-label type) :position)
             when pos do (setq arg-pos pos arg-type type)
             ;; Collect the parameter type
             for name = (plist-get (et-repr-label mrepr) :field)
             when name do (setq param-name name param-repr mrepr)
             ;; Display the error message
             finally do
             (if (and arg-type param-repr)
                 (et-err arg-pos "[%s] Expected %s, found %s" param-name
                         (et-repr-to-string param-repr) arg-type)
               (et-err (or arg-pos 0) ; its possible to have an arg label without a param label
                       "`%s' has type %s\\nInvalid arguments: %s" func func-type args-type)))))))

       (_ (et-err 0 "No type for `%s'" func))))

    ;; Type check a variable (a symbol which is neither a keyword, nil, or t)
    ((and sym (pred symbolp) (pred (not keywordp)) (guard sym) (guard (not (eq sym t))))
     (pcase nil
       ;; Check if the variable is locally scoped
       ((and (let var (et-get-symbol-var sym)) (guard var))
        (et-add-typeof (et-current-var-type var) var))

       ;; Check if it is a global variable with a type
       ((and (let type (get sym 'et-variable-type)) (guard type))
        type)

       (_ (et-err nil "Free variable: %s" sym))))

    (expr (et-literal expr))))


(defmacro et-check-call (func &rest args)
  `(let ((result (et-result-boundary
                  (et--check '(,func ,@(cl-loop for a in args collect `(:type ,a)))))))
     (or (mapcar #'caddr (et-result-diagnostics result)) (et-result-value result))))


;;;; Root level functions

(defmacro et-typecheck (body)
  (et-result-boundary (et-simplify-type (et--check body))))

(defmacro et-typecheck-call (func &rest arg-types)
  (cl-loop for type in arg-types
           if (eq (car-safe type) :eval-type) collect type into arg-exprs
           else collect (list :type type) into arg-exprs
           finally return `(et-typecheck (,func ,@arg-exprs))))


;;;; Tests

(defmacro et-assert-resolve (type expr &optional not)
  (declare (indent 1))
  `(et-result-boundary
    (let* ((t-type (et-at 1 (et ,type)))
           (r-type (et-at 2 (et--check ',expr))))
      (or (,(if not #'not #'identity) (et-subtype? r-type t-type))
          (et-err 0 "Expected %s, got %s" t-type (et-result-type result))))))

(defmacro et-assert-no-resolve (type expr)
  (declare (indent 1))
  `(et-assert-resolve ,type ,expr 'NOT))

(defmacro et-assert-resolve-errors (expr)
  `(et-result-boundary
    (or (et-result-failed (et-result-boundary (et--check ',expr)))
        (et-err 0 "Didn't fail"))))

(defmacro et-assert-call (type-spec func &rest arg-types)
  `(et-result-boundary
    (let* ((type (et ,type-spec))
           (params (cl-loop for a in ',arg-types collect (list :type a)))
           (ret-type (et--check (cons ',func params))))
      (or (equal type ret-type)
          (et-err 0 "Expected %s, got %s" type ret-type)))))

(defmacro et-assert-call-errors (func &rest arg-types)
  `(et-result-boundary
    (let* ((params (cl-loop for a in ',arg-types collect (list :type a)))
           (result (et-result-boundary (et--check (cons ',func params)))))
      (unless (et-result-failed result)
        (error "Succeeded with %s" (cl-prin1-to-string (et-result-type result)))))))

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
;;; Defining checkers
;;;; Define checker

(defmacro et-define-checker (funcs &rest body)
  "A checker should return a type, or nil if the expression is invalid."
  (declare (indent 1))
  (cl-assert (or (symbolp funcs) (seq-every-p #'symbolp funcs)))

  `(let* ((checker (lambda () ,@body)))
     ,@(cl-loop for func in (if (symbolp funcs) (list funcs) funcs)
                collect `(setf (get ',func 'et-checker) checker))
     ',funcs))


;;;; Pcase checker

(defmacro et-define-pcase-checker (funcs pattern &rest body)
  "A checker should return a type, or nil if the expression is invalid."
  (declare (indent 2))
  `(et-define-checker ,funcs
     (pcase (cdr et--checker-expr)
       (,pattern . ,body)
       ,@(unless (symbolp pattern) (et-ql (_ (error "Pcase checker didn't match")))))))


;;;; Type checker

(defmacro et-define-type-checker (funcs &rest arguments)
  "Define a type checker by assigning a function type to FUNCS.

FUNCS is the function or list of functions to define the checker for.

GENERICS is an optional vector of generic type variable symbols.

ARGLIST is a parsable expression for the argument list matcher.

RETURN is a parsable expression for the return type.

\(fn FUNC [GENERICS] ARGLIST RETURN)"
  (declare (indent 2))

  (let* ((gen-vec (when (vectorp (car arguments)) (pop arguments)))
         (arglist-spec (car arguments))
         (return-spec (cadr arguments)))
    (unless (eq (length arguments) 2)
      (error "Incorrect number of arguments"))
    `(et--define-type-checker ',funcs ,gen-vec (backquote ,arglist-spec) (backquote ,return-spec))))

(defun et--define-type-checker (funcs gen-vec arglist-spec return-spec)
  (let* ((func-type
          (if gen-vec
              (let* ((matcher (et-parse-matcher arglist-spec gen-vec))
                     (output-repr (et-parse-repr return-spec (et-matcher-generics matcher) 'TYPE)))
                (et-dt 'DynFunction matcher output-repr))
            (et-dt 'Function (et-parse-type arglist-spec) (et-parse-type return-spec)))))
    (dolist (func (if (symbolp funcs) (list funcs) funcs))
      (put func 'et-checker nil)
      (put func 'et-function-type func-type))))


;;; ============================================================
;;; Checker helpers
;;;; Check subexprs

(defun et--traverse-tree (tree path)
  (if (null path) tree
    (when (>= (car path) (length tree))
      (error "Index out of bounds: %s %s" (car path) tree))
    (et--traverse-tree (nth (car path) tree) (cdr path))))

(defun et-checker-sub (&rest path)
  "Type check the sub expression at PATH, returning the type or never."
  (let* ((flat (flatten-tree path))
         (expr (et--traverse-tree et--checker-expr flat)))
    (et-at flat (et--check expr))))

(defun et-checker-remaining (&rest first-path)
  (cl-assert et--checker-expr)
  (cl-assert first-path)
  (setq first-path (flatten-tree first-path))

  (let* ((parent-path (butlast first-path))
         (parent-expr (et--traverse-tree et--checker-expr parent-path))
         (start (car (last first-path))))

    (cl-loop for idx upfrom start below (length parent-expr)
             collect (et-checker-sub (append parent-path (list idx))))))

(defun et-checker-tail (&rest first-path)
  (or (car (last (et-checker-remaining first-path))) (et-literal nil)))


;;;; Resolve

(defun et-checker-resolve (type &rest path)
  "Type check an expression at PATH, ensuring that it satisfied TYPE.

TYPE is a type or an expression parseable to a type.

PATH is the path to the subexpression."
  (unless (et-type-p type) (setq type (et-parse-type type)))
  (let* ((expr-type (et-checker-sub path)))
    (unless (et-subtype? expr-type type)
      (et-err path "Expected %s, found %s" type expr-type))
    type))


;;;; Infer

(defmacro et-checker-infer (type gen-vec matcher-spec output-spec)
  (let* ((gens (et--gen-vec-generics gen-vec))
         (constraints (et--gen-vec-constraints gen-vec)))
    `(et--checker-infer ,type ',gens ',constraints ,(list '\` matcher-spec) ,(list '\` output-spec))))

(defun et--checker-infer (type gens constraints matcher-spec output-spec)
  (let* ((result
          (et-infer (make-et-matcher
                     :generics gens
                     :constraints constraints
                     :repr (et-parse-repr matcher-spec gens 'MATCHER))
                    type
                    (et-parse-repr output-spec gens 'TYPE))))
    (when (et-match-result-success result)
      (et-match-result-value result))))


;;;; Nicer funcall

(defun et-checker-funcall (func-type arglist-type)
  (declare (et (func-type *et-type) (arglist-type *et-type)
               (@return *et-type)))
  (let* ((result (et-funcall func-type arglist-type)))
    (when (et-match-result-success result)
      (et-match-result-value result))))


;;; ============================================================
;;; Processing
;;;; Function definitions
;;;;; Parse declarations

(cl-defstruct et--func-declarations
  "The result of parsing the function declarations."
  param-reprs input-type return-repr generics constraints props)

(defun et--find-function-declarations (body)
  (when-let*
      ((declare-pos (cl-position 'declare body :key #'car-safe :start 1))
       (declare-block (nth declare-pos body))
       (et-pos (cl-position 'et declare-block :key #'car-safe))
       (et-block (nth et-pos declare-block))
       ;; Start the actual parsing
       (param-groups (et-at 0 (et--parse-arglist-params (car body)))))
    (et-at (list declare-pos et-pos)
      (et-at-offset 1 ; after the `et'
        (cons (et--parse-function-declarations param-groups (cdr et-block))
              (1+ declare-pos))))))

(defun et--assign-function-declarations (func decls)
  "Assign relevant symbol properties to FUNC."
  (declare (et (func Var) (decls *et--func-declarations) (@return Nil)))
  (put func 'et-function-declarations decls)

  ;; Assign `et-function-type' if applicable
  (when-let* ((type (et--func-decls-to-type decls)))
    (put func 'et-function-type type))

  ;; Assign the checker if applicable
  (when-let* ((checker (plist-get (et--func-declarations-props decls) :checker)))
    (put func 'et-checker checker)))

(defun et--func-decls-to-input (decls)
  (pcase-let* (((cl-struct et--func-declarations param-reprs generics constraints) decls))
    (apply #'et--generate-func-input nil generics constraints param-reprs)))

(defun et--func-decls-to-type (decls)
  (when-let* ((input (et--func-decls-to-input decls))
              (return-repr (et--func-declarations-return-repr decls)))
    (if (et-matcher-p input)
        (et-dt 'DynFunction input return-repr)
      (et-dt 'Function input (et-repr-to-type return-repr)))))

(defun et--parse-function-declarations (param-groups declares)
  (let* ((param-reprs
          (cl-loop for group in param-groups
                   collect (cl-loop for name in group collect (cons name (et-repr Any)))))
         any-params return-repr gen-vec generics constraints props)

    ;; Parse the fields of the declare block
    (dotimes (form-idx (length declares))
      (et-error-boundary form-idx
        (pcase (nth form-idx declares)
          (`(@return ,spec)
           (when return-repr (et-fatal 0 "Multiple @return clauses"))
           (et-at 1
             (setq return-repr (et-parse-repr spec generics 'TYPE))))
          (`(@return . ,_) (et-fatal 0 "Expected (@return TYPE)"))

          (`(@generics ,(and gv (pred vectorp)))
           (when gen-vec (et-fatal 0 "Multiple @generic clauses"))
           (when return-repr (et-fatal 0 "@generic must come before @return"))
           (when any-params (et-fatal 0 "@generic must come before parameter declarations"))
           (et-at 1
             (setq gen-vec gv
                   generics (et--gen-vec-generics gv)
                   constraints (et--gen-vec-constraints gv))))
          (`(@generics . ,_) (et-fatal 0 "Expected (@generics [VARS...])"))

          (`(@skip)
           (when (plist-get props :skip) (et-fatal 0 "Multiple @skip clauses"))
           (setq props (cl-list* :skip t props)))

          ;; Can contain `narrows', `vars', `all'
          (`(@show . ,show)
           (when (plist-get props :show) (et-fatal 0 "Multiple @show clauses"))
           (setq props (cl-list* :skip show props)))

          (`(@checker ,checker)
           (when (plist-get props :checker) (et-fatal 0 "Multiple checkers specified"))
           (setq props (cl-list* :checker checker props)))

          (`(@expand)
           (when (plist-get props :checker) (et-fatal 0 "Multiple checkers specified"))
           (setq props (cl-list* :checker #'et-macroexpand-checker props)))
          (`(@progn)
           (when (plist-get props :checker) (et-fatal 0 "Multiple checkers specified"))
           (setq props (cl-list* :checker #'et-progn-checker props)))
          (`(@prog1)
           (when (plist-get props :checker) (et-fatal 0 "Multiple checkers specified"))
           (setq props (cl-list* :checker #'et-prog1-checker props)))

          ;; Parameters
          (`(,(and name (pred symbolp)) ,spec)
           (let* ((entry (cl-loop for group in param-reprs
                                  for entry = (assq name group)
                                  when entry return entry
                                  finally do (et-fatal 0 "Not a parameter: %s" name))))
             (et-at 1
               (setq any-params t)
               (setcdr entry (et-parse-repr spec generics 'BOTH)))))

          (_ (error "Invalid format")))))

    (make-et--func-declarations
     :param-reprs param-reprs
     :return-repr return-repr
     :generics generics
     :constraints constraints
     :props props)))


;;;;; Parse signature

;; Function processing happens in two phases:
;;
;; 1. `et--parse-defun-signature' must be run after all types are
;; defined.
;;
;; 2. Checking the function body must happen after all functions and
;; variabels are defined.

(cl-defstruct et-func-sig
  (declarations nil :et *et--func-declarations)
  (func-type nil :et *et-type)
  (props nil :et List<Any>)
  ;; Things necessary for typechecking the body
  (source nil :et List<Any>) ; Some nthcdr of the function body containing the code
  (source-pos nil :et Integer) ; Position of source RELATIVE TO ARGLIST (1 if right after arglist)
  ;; Both vars and expected-return may contain the scoped types
  (scoped nil :et (List (Tuple EtGeneric Symbol List<EtTypeConstraint>)))
  (vars nil :et List<*et-var>)
  (expected-return nil :et *et-type))

(defun et--parse-function-signature (body)
  "The current path should point to ARGLIST."
  (declare (et (body List<Any>)
               (@return Nil|*et-result<*et-func-sig>)
               (@skip)))

  (when-let* ((found (et--find-function-declarations body))
              (decls (car found))
              (source-pos (cdr found))
              (return-repr (et--func-declarations-return-repr decls)))

    ;; Construct the function signature
    (let* ((input (et--func-decls-to-input decls))
           (param-reprs (et--func-declarations-param-reprs decls))
           (generics (et--func-declarations-generics decls))
           (scoped (et--make-scoped-datatypes (when (et-matcher-p input) input))))
      (et--with-scoped-datatypes scoped
        (make-et-func-sig
         :declarations decls
         :func-type (et--func-decls-to-type decls) ; will be non-nil if return-repr is non-nil
         :props (et--func-declarations-props decls)
         ;; Things necessary for typechecking the body
         :source (nthcdr source-pos body)
         :source-pos source-pos
         ;; Both vars and expected-return may contain the scoped types
         :scoped scoped
         :vars
         (cl-loop for param-group in param-reprs nconc
                  (cl-loop for (name . repr) in param-group
                           ;; Replace each generic in the parameter reprs with the corresponding scoped datatype
                           for type = (cl-loop for gen in generics
                                               for scoped-args in scoped
                                               collect (cons gen (apply #'et-dt 'Scoped scoped-args)) into gen-repls
                                               finally return (et-repr-to-type repr gen-repls))
                           collect (make-et-var :name name :type type)))
         :expected-return
         (cl-loop for (name unique constraints) in scoped
                  collect (cons name (et-dt 'Scoped name unique constraints))
                  into gen-repls
                  finally return (et-repr-to-type return-repr gen-repls)))))))


;;;;; Parse arglist params

(defun et--parse-arglist-params (arglist)
  "Parse ARGLIST into (REQUIRED OPTIONAL KEY REST).

ARGLIST is a plain parameter list with no type annotations — just
symbols and default-value forms. REQUIRED, OPTIONAL, KEY, and REST are
each lists of parameter name symbols. The list REST has at most 1
element."
  (declare (et (arglist List<Var>)
               (@return (Tuple List<Var> List<Var> List<Var> List<Var>))))

  (let ((required nil)
        (optional nil)
        (key-params nil)
        (rest-param nil)
        (state 'required))
    (dolist (elt arglist)
      (pcase elt
        ('&optional (setq state 'optional))
        ('&rest     (setq state 'rest))
        ('&key      (setq state 'key))
        ((or `(,name . ,_) name)
         (pcase state
           ('required (push name required))
           ('optional (push name optional))
           ('key      (push name key-params))
           ('rest     (setq rest-param name))))))
    (list (nreverse required)
          (nreverse optional)
          (nreverse key-params)
          (when rest-param (list rest-param)))))


;;;;; Input type

(defun et--generate-func-input (always-matcher generics constraints required optional key-params rest-params)
  "Build an input matcher or type from parameter specs.

GENERICS is a list of generic symbols, or nil.

REQUIRED, OPTIONAL, KEY/REST-PARAMS are alists of (SYMBOL . REPR).

REST-PARAMS will have at most 1 entry.

Returns an `et-matcher' if GENERICS is non-nil, or an `et-type' if not."
  (declare (et (always-matcher Boolean)
               (generics ListR<EtGeneric>)
               (constraints ListR<EtTypeConstraint>)
               (required AList<Symbol~EtBR|Nil>)
               (optional AList<Symbol~EtBR|Nil>)
               (key-params AList<Symbol~EtBR|Nil>)
               (rest-params AList<Symbol~EtBR|Nil>)))

  ;; Convert an entry (VAR . REPR) to a labeled repr
  (let* ((fn (lambda (var repr)
               (et-copy-with (or repr (et-repr Nil))
                             :label (list :field var))))
         (fn1 (lambda (e) (funcall fn (car e) (cdr e))))
         (rest-repr
          (pcase (car rest-params)
            (`(,var . ,repr) (funcall fn var repr))
            ((guard key-params)
             (cl-loop for (var . repr) in key-params
                      nconc (list (intern (format ":%s" var)) (funcall fn var repr)) into plist-args
                      finally return (et-repr PList ,@plist-args)))

            ('nil (et-repr Nil))
            (x (error "Invalid rest param: %s" x))))

         (req-reprs (mapcar fn1 required))
         (opt-reprs (mapcar fn1 optional))
         (input-repr (et--params-to-input-repr req-reprs opt-reprs rest-repr)))

    (if (or always-matcher generics)
        (make-et-matcher
         :generics generics
         :constraints constraints
         :repr input-repr)
      (et-repr-to-type input-repr nil))))

(defun et--params-to-input-repr (req opt rest)
  (declare (et (req ListR<EtBR>) (opt ListR<EtBR>) (rest EtBR)
               (@return EtBR)))
  (cond
   (req (et-repr ConsR ,(car req) ,(et--params-to-input-repr (cdr req) opt rest)))
   (opt (et-repr or Nil (ConsR ,(car opt) ,(et--params-to-input-repr nil (cdr opt) rest))))
   (t rest)))

(et-test
 (equal (et--generate-func-input nil nil nil '((x . (((S:DT Integer)))) (y . (((S:DT Any))))) nil nil nil)
        (et ConsR<Integer~ConsR<Any~Nil>>))

 (equal (et--generate-func-input nil nil nil '((x . (((S:DT Integer)))) (y . (((S:DT String))))) nil nil
                                 '((args . (((S:ALIAS ListR (((S:DT Any)))))))))
        (et ConsR<Integer~ConsR<String~ListR<Any>>>))

 (equal (et-matcher-dnf
         (et--generate-func-input nil '(T) nil '((x . (((S:GENERIC T))))) '((y . (((S:DT Number)))))
                                  nil '((args . (((S:ALIAS ListR (((S:DT String))))))))))
        (et-matcher-dnf (et-matcher [T] ConsR<T~{Nil|ConsR<Number~ListR<String>>}>)))

 (equal (et-matcher-dnf
         (et--generate-func-input nil '(T) nil '((a . (((S:GENERIC T))))) nil '((scale . (((S:DT Number)))) (flag . (((S:DT Any))))) nil))
        (et-matcher-dnf (et-matcher [T] ConsR<T~PList<:scale~Number~:flag~Any>>))))


;;;;; Make function type

(defun et--make-function-type (generics constraints input-repr output-repr)
  "Build a Function or DynFunction type from reprs.

If GENERICS is non-nil, returns a DynFunction with a matcher built from
GENERICS, CONSTRAINTS, and INPUT-REPR, and OUTPUT-REPR as the output.

If GENERICS is nil, returns a Function with INPUT-REPR and
OUTPUT-REPR each converted to concrete types."
  (declare (et (generics ListR<EtGeneric>)
               (constraints ListR<EtTypeConstraint>)
               (input-repr EtBR)
               (output-repr EtTR)))

  (if generics
      (et-dt 'DynFunction
             (make-et-matcher :generics generics
                              :constraints constraints
                              :repr input-repr)
             output-repr)
    (et-dt 'Function
           (et-repr-to-type input-repr nil)
           (et-repr-to-type output-repr nil))))


;;;;; Identification

(defun et--identify-defun (body)
  (list
   :declare
   (lambda ()
     (when-let* ((name (cadr body))
                 (sig (et-at-offset 2 (et--parse-function-signature (cddr body)))))
       (put name 'et-function-signature sig)
       (et--assign-function-declarations name (et-func-sig-declarations sig))))))

(defun et--identify-defmacro (body)
  (list
   :declare
   (lambda ()
     (when-let* ((found (et-at-offset 2 (et--find-function-declarations (cddr body))))
                 (decls (car found)))
       (et--assign-function-declarations (cadr body) decls)))))

(defun et--identify-function-directive (form)
  (list
   :declare
   (lambda ()
     (pcase-let*
         ((`(@function ,name ,arglist . ,declares) form)
          (param-groups (et-at 2 (et--parse-arglist-params arglist)))
          (decls (et-at-offset 3 (et--parse-function-declarations param-groups declares))))
       (et--assign-function-declarations name decls)))))

(defun et--identify-macro-directive (form)
  (let* ((name (cadr form)))
    (list
     :declare
     (lambda ()
       (put name 'et-checker
            (or (when (plist-get form :expand) #'et-macroexpand-checker)
                (when (plist-get form :progn) #'et-progn-checker)
                (when (plist-get form :prog1) #'et-prog1-checker)
                (plist-get form :checker)
                (et-fatal 0 "No checker specified")))))))


;;;;; Checker

(et-define-pcase-checker defun `(,name . ,_)
  (when-let* ((sig (get name 'et-function-signature))
              ((not (plist-get (et-func-sig-props sig) :skip))))

    (et--with-scoped-datatypes (et-func-sig-scoped sig)
      (et-with-vars (et-func-sig-vars sig)
        (let* ((actual-ret (et-checker-tail (+ 2 (et-func-sig-source-pos sig))))
               (expected-ret (et-func-sig-expected-return sig)))
          (or (et-subtype? actual-ret expected-ret)
              (et-err 0 "Expected %s, found %s" expected-ret actual-ret))))))
  (et-literal name))


;;;; Identify cl-defstruct

(defun et--identify-cl-defstruct (expr)
  "Identify a `cl-defstruct' expression, returning a processing plist."
  (let* ((body (cdr expr))
         (name-or-opts (car body))
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
         slots gen-vec gen-vec-rel generics)

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
                 (setq gen-vec (nth (1+ pos) plist)
                       gen-vec-rel (list rel (+ 3 pos))
                       generics (et--gen-vec-generics gen-vec)))))
            (_ (et-err rel "Invalid slot format"))))))

    (setq slots (nreverse slots))
    (put name 'et-struct (list :generics generics))

    (list
     :constrain
     (lambda ()
       (when gen-vec
         (et-at gen-vec-rel
           (plist-put (get name 'et-struct) :constraints
                      (et--gen-vec-constraints gen-vec)))))

     :populate
     (lambda ()
       (let* ((plist (or (get name 'et-struct)
                         (et-fatal 0 "Struct `%s' not defined" name)))
              (constraints (plist-get plist :constraints))
              ;; The target for input reprs: a matcher when generic, else a plain type
              (input-target (if generics 'BOTH 'TYPE))
              ;; Repr helpers shared across all generated functions
              ;; STRUCT-REPR is the struct's own type, e.g. *Name<T1 T2>
              (struct-repr (et-parse-repr (et-q (Struct ,name ,@generics)) generics 'BOTH))
              ;; ARGLIST-REPR is the single-argument arglist (STRUCT), used by
              ;; accessors and the copier
              (arglist-repr (et-parse-repr (et-q (ConsR ,struct-repr Nil)) generics 'BOTH)))

         ;; --- Predicate ---
         (when predicate
           (let* ((never-args (make-list (length generics) 'Never))
                  (output-repr
                   (et-parse-repr
                    (et-q (or (and True (bindsof (and T (Struct ,name ,@never-args))))
                              (and Nil (bindsof (subtract T (Struct ,name ,@never-args))))))
                    '(T) 'TYPE)))
             (put predicate 'et-function-type
                  (et-dt 'DynFunction (et-parse-matcher '(Args T) [T]) output-repr))))

         ;; --- Accessors ---
         (dolist (slot slots)
           (pcase-let* ((`(,rel ,slot-name ,_default ,type-info) slot)
                        (accessor (if conc-name (intern (format "%s%s" conc-name slot-name))
                                    slot-name)))
             (et-error-boundary rel
               (let* ((slot-repr (if type-info
                                     (et-at (car type-info)
                                       (et-parse-repr (cdr type-info) generics 'TYPE))
                                   (et-repr Any))))
                 (put accessor 'et-function-type
                      (et--make-function-type generics constraints
                                              arglist-repr slot-repr))))))

         ;; --- Constructor ---
         (when constructor
           (let* ((input-repr
                   (cl-loop for (rel slot-name _default type-info) in slots
                            for slot-repr = (if type-info
                                                (et-error-boundary rel
                                                  (et-at (car type-info)
                                                    (et-parse-repr (cdr type-info) generics input-target)))
                                              (et-repr Any))
                            nconc (list (intern (format ":%s" slot-name)) slot-repr) into args
                            finally return (et-parse-repr (if args `(PList ,@args) 'Nil) nil 'TYPE))))
             (put constructor 'et-function-type
                  (et--make-function-type generics constraints
                                          input-repr struct-repr))))

         ;; --- Copier ---
         (when copier
           (put copier 'et-function-type
                (et--make-function-type generics constraints
                                        arglist-repr struct-repr))))))))


;;;; Identify alias directive

(defun et--identify-alias-directive (form)
  "Identify an alias definition, returning a processing plist.

During identification, declares the alias name and generics.
Returns a plist with :constrain and :populate functions."
  (let* ((args (cdr form))
         (orig-args args)
         (name (pop args))
         (_ (or (symbolp name)
                (et-fatal 1 "Alias name must be a symbol")))
         (gen-vec (if (vectorp (car args)) (pop args) []))
         (pb (condition-case nil (et--props-and-body args)
               (error (et-fatal nil "Expected format (@alias NAME [GENERICS] [PROPS...] TYPE)"))))

         ;; Identification phase: declare the alias name, generics, and spec
         ;; (but don't parse repr or constraints yet)
         (_ (apply #'et--declare-alias name gen-vec (cdr pb) (car pb)))

         (spec-idx (length orig-args))
         (gen-vec-idx (when (vectorp (nth 1 orig-args)) 2)))

    (list
     :constrain
     (lambda ()
       ;; Parse the generic constraints (which reference other types)
       (when gen-vec-idx
         (et-at gen-vec-idx
           (let* ((props (get name 'et-alias)))
             (plist-put props :constraints
                        (et--gen-vec-constraints (plist-get props :gen-vec)))))))

     :populate
     (lambda ()
       ;; Parse the spec into a repr
       (et-at spec-idx
         (let* ((props (or (get name 'et-alias)
                           (et-fatal nil "Alias `%s' not declared" name)))
                (spec (plist-get props :spec))
                (generics (plist-get props :generics))
                (target (plist-get props :target)))
           (plist-put props :repr
                      (et-parse-repr spec generics (or target 'BOTH)))))))))


;;;; Identify variable directive

(defun et--identify-variable-directive (form)
  "Identify a variable definition, returning a processing plist.

During identification, just validates the format.
Returns a plist with :declare to set the variable type."
  (pcase (cdr form)
    (`(,(and name (pred symbolp)) ,spec)
     (list
      :declare
      (lambda ()
        (et-at 2
          (put name 'et-variable-type (et-parse-type spec))))))

    (_ (et-fatal nil "Expected format (@variable NAME TYPE)"))))

(et-define-pcase-checker defvar `(,(and (pred symbolp) name) . ,rest)
  (if-let* ((declared-type (get name 'et-variable-type))
            (value-type (or (when (car rest) (et-checker-sub 2)) (et Nil))))
      (unless (et-subtype? value-type declared-type)
        (et-err 2 "Expected %s, found %s" declared-type value-type))

    ;; Otherwise, declare it as any
    (put name 'et-variable-type (et Any)))

  (et-literal name))


;;;; Get/put types

(defun et--identify-symbol-property-directive (form)
  "Identify a symbol property declaration, returning a processing plist.

During identification, just validates the format.
Returns a plist with :declare to set the symbol type."
  (pcase (cdr form)
    (`(,(and symbol (pred symbolp)) ,spec)
     (list
      :declare
      (lambda ()
        (et-at 2
          (put symbol 'et-symbol-property-type (et-parse-type spec))))))

    (_ (et-fatal nil "Expected format (@symbol-property SYMBOL TYPE)"))))


;;;; Identify expr

(defvar et--processed-requires nil
  "List of libraries which have been processed due to a `require'.")

(defun et--identify-expr (expr)
  (cons
   expr
   (et-error-boundary nil
     (pcase expr

       ;; Load required files
       (`(require ,name)
        (let* ((dir (when buffer-file-name (file-name-parent-directory buffer-file-name)))
               (load-path (cons dir load-path))
               (library (or (locate-library (symbol-name (eval name)))
                            (error "Library `%s' not found" name))))
          (unless (member library et--processed-requires)
            (push library et--processed-requires)
            ;; Process the buffer without propagating diagnostics
            (et--process-exprs (et--file-exprs library)))))

       ;; Process a root declaration block
       ((or `(et-declare . ,forms)
            (and (pred vectorp) (app (lambda (v) (append v nil)) `(et . ,forms))))
        (cl-loop
         for form in forms
         for pos upfrom 1
         for plist =
         (et-error-boundary pos
           (pcase form
             (`(@alias . ,_) (et--identify-alias-directive form))
             (`(@variable . ,_) (et--identify-variable-directive form))
             (`(@function . ,_) (et--identify-function-directive form))
             (`(@macro . ,_) (et--identify-macro-directive form))
             (`(@symbol-property . ,_) (et--identify-symbol-property-directive form))))
         collect
         (cons pos plist)))

       ;; Process a defun/defmacro
       (`(defun ,(pred symbolp) ,(pred listp) . ,_)
        (list (cons nil (et--identify-defun expr))))
       (`(defmacro ,(pred symbolp) ,(pred listp) . ,_)
        (list (cons nil (et--identify-defmacro expr))))

       ;; Process a struct
       (`(cl-defstruct . ,_)
        (list (cons nil (et--identify-cl-defstruct expr))))))))


;;;; Process exprs

(defvar et--processing-phase nil "Used for debugging `et--process-exprs'.")
(defvar et--processing-expr nil "Used for debugging `et--process-exprs'.")

(cl-defun et--process-exprs (exprs &key check test eval)
  (let* ((identified (et-result-map #'et--identify-expr exprs)))

    ;; Evaluate the buffer
    ;; This must occur first, in case the buffer defines custom type keywords
    (when (or test eval)
      (cl-loop for expr in exprs
               for pos upfrom 0
               do
               (let* ((et--processing-phase :eval)
                      (et--processing-expr expr))
                 (et-error-boundary pos
                   (et-wrap-errors "Runtime error: %s" (eval expr))))))

    ;; For each expression, et--identify-expr returns a list of
    ;; process-plists. A process-plist describes how to process that
    ;; expression in each phase. For each phase, we want to perform
    ;; the correct action across all expressions.

    (dolist (phase '(:constrain :populate :declare))
      (cl-loop for (expr . expr-plists) in identified
               for idx upfrom 0
               do
               (let* ((et--processing-phase phase)
                      (et--processing-expr expr))
                 (et-at idx
                   (cl-loop for (path . plist) in expr-plists
                            for func = (plist-get plist phase)
                            when func do (et-error-boundary path (funcall func)))))))

    ;; Type-check all root-level expressions
    (when check
      (cl-loop for expr in exprs
               for pos upfrom 0
               when (and (consp expr)
                         (or (get (car expr) 'et-function-type)
                             (get (car expr) 'et-checker)))
               do
               (let* ((et--processing-phase :check)
                      (et--processing-expr expr))
                 (et-error-boundary pos (et--check expr)))))

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
                (let* ((et--processing-phase :test)
                       (et--processing-expr test))
                  (et-at (list pos test-idx)
                    (pcase (eval test)
                      ('nil (et-warn nil "Evaluated to nil"))
                      ((and result (pred et-result-p))
                       (when (et-result-failed result)
                         (et-propagate-result result)))))))))))


;;;; Processing helpers

(defun et--buffer-exprs ()
  (save-excursion
    (goto-char (point-min))
    (cl-loop while t
             for expr = (condition-case _ (read (current-buffer))
                          (error (cl-return exprs)))
             collect expr into exprs)))

(defun et--file-exprs (file)
  (with-temp-buffer (insert-file-contents file) (et--buffer-exprs)))


;;; ============================================================
;;; Utils
;;;; Testing checkers

(et-define-pcase-checker :type `(,spec)
  (setq et--checker-expr "dummy") (et-parse-type spec))

(et-define-pcase-checker :eval-type `(,expr)
  (setq et--checker-expr "dummy") (eval expr))

(et-define-pcase-checker :assert-subtype `(,_expr ,type-spec)
  (let ((expr-type (et-checker-sub 1)))
    (or (et-subtype? expr-type (et-parse-type type-spec))
        (et-err nil "Not subtype: %s" expr-type))
    (setq et--checker-expr "dummy")
    (et Nil)))

(et-define-pcase-checker :assert-error `(,_expr)
  (condition-case _err (et-checker-sub 1)
    (error (setq et--checker-expr nil) (et-literal nil))
    (:success (et-err nil "Didn't error"))))

(et-define-pcase-checker :typeof `(,_expr)
  (let* ((type (et-checker-sub 1)))
    (et-hint nil (et--remove-type-binds type))
    (setq et--checker-expr (cadr et--checker-expr))
    type))

(et-define-pcase-checker :typeof+ `(,_expr)
  (let* ((type (et-checker-sub 1)))
    (et-hint nil type)
    (setq et--checker-expr (cadr et--checker-expr))
    type))

(et-define-pcase-checker :expand spec
  (let* ((type (et-expand-all-aliases (et-parse-type spec))))
    (et-hint nil type)
    type))

(et-define-pcase-checker :narrows `()
  (cl-loop for (var . type) in (reverse et--narrow-binds)
           collect (format "%s: %s" (et-var-name var) (et-pp type)) into strs
           finally do
           (et-hint nil (string-join strs "\\n")))
  (setq et--checker-expr nil)
  (et Nil))

(et-define-pcase-checker :eval `(,expr)
  (et-hint nil (cl-prin1-to-string (eval expr)))
  (setq et--checker-expr nil)
  (et Nil))


;;;; Annotation macros

;; `declare' forms carry no runtime value; type them as Nil so they are
;; ignored wherever they appear.
(et-define-checker declare (et Nil))

(et-define-pcase-checker et: `(,type-spec ,_expr)
  (let* ((actual (et-checker-sub 2))
         (declared (et-parse-type type-spec)))
    (unless (et-subtype? actual declared)
      (et-err 0 "Expected %s, found %s" (et-pp declared) (et-pp actual)))
    declared))

(et-define-pcase-checker et! `(,type-spec ,_expr)
  (let* ((actual (et-checker-sub 2))
         (declared (et-parse-type type-spec)))
    (when (and (not (et-never-p actual))
               (not (et-never-p declared))
               (et-never-p (et--supersect actual declared)))
      (et-err 0 "Types %s and %s have no overlap. Use et!! to supress this warning."
              (et-pp declared) (et-pp actual)))
    declared))

(et-define-pcase-checker et!! `(,type-spec ,_expr)
  (let* ((_actual (et-checker-sub 2))
         (declared (et-parse-type type-spec)))
    declared))


;;;; Pcase et-*

(pcase-defmacro et-2* (vars pat1 pat2)
  "Match alternating pairs in a flat list, collecting bindings.
Groups the list into pairs and delegates to et-*.

Example:
  (pcase \\='(setq a 1 b 2)
    (`(setq . ,(et-2* [(var vars) (val vals)]
                      (and (pred symbolp) var) val))
     (list vars vals)))
  => ((a b) (1 2))"
  `(and (pred listp)
        (pred (lambda (l) (cl-evenp (length l))))
        (app (lambda (l)
               (cl-loop for (a b) on l by #'cddr
                        collect (list a b)))
             (et-* ,vars (\` ((\, ,pat1) (\, ,pat2)))))))

(pcase-defmacro et-* (vars pattern)
  "Match a list where each element matches PATTERN, collecting bindings.
VARS is a vector of variable specs. Each spec is one of:
  SYMBOL           — binds SYMBOL to the collected list
  (SINGULAR PLURAL) — SINGULAR is used inside PATTERN to match each element,
                      PLURAL is bound to the collected list in the body.
                      Inside PATTERN, PLURAL refers to the matches so far
                      (not including the current element).

Example:
  (pcase \\='((a 1) (b 2))
    ((et-* [(name names) val] `(,name ,val))
     (list names val)))
  => ((a b) (1 2))"
  (let* ((specs (mapcar (lambda (v)
                          (if (consp v)
                              (list :singular (car v)
                                    :plural (cadr v)
                                    :acc (gensym (symbol-name (cadr v))))
                            (list :singular v
                                  :plural v
                                  :acc (gensym (symbol-name v)))))
                        (append vars nil)))
         (lst (gensym "lst"))
         (elt (gensym "elt"))
         (plural-vars (cl-remove-if-not
                       (lambda (s) (not (eq (plist-get s :singular)
                                            (plist-get s :plural))))
                       specs)))
    `(and
      (pred listp)
      (app (lambda (,lst)
             (let ,(mapcar (lambda (s) (list (plist-get s :acc) nil))
                           specs)
               (when (cl-every
                      (lambda (,elt)
                        (let ,(mapcar (lambda (s)
                                        `(,(plist-get s :plural)
                                          (reverse ,(plist-get s :acc))))
                                      plural-vars)
                          (pcase ,elt
                            (,pattern
                             ,@(mapcar (lambda (s)
                                         `(push ,(plist-get s :singular)
                                                ,(plist-get s :acc)))
                                       specs)
                             t)
                            (_ nil))))
                      ,lst)
                 (list ,@(mapcar (lambda (s) `(nreverse ,(plist-get s :acc)))
                                 specs)))))
           (,'\` (,@(mapcar (lambda (s)
                              (list '\, (plist-get s :plural)))
                            specs)))))))


;;;; Macroexpand checker

(defvar et--macroexpand-expr nil
  "An expression which got macroexpanded in an `et--check'.

When macroexpanding an expression, the inner expressions will end up at
different paths than the original expression. It is impossible to
deterministically decide where the subexpressions ended up, but the best
heuristic is by searching (using `equal') for each nested call to
`et--check' inside of the original expression.")

(defun et--path-in-tree (expr tree)
  "If EXPR exists in TREE, return its path, or return `NO' otherwise."
  (if (equal expr tree) nil
    (cl-loop for subtree in tree ; safe even if tree is not a list
             for idx upfrom 0
             for path = (et--path-in-tree expr subtree)
             unless (eq path 'NO) return (cons idx path)
             finally return 'NO)))

(defun et--macroexpand-check-advice (func expr)
  (if (or (null et--macroexpand-expr)
          (null et--sticky-path))
      (funcall func expr)

    (let* ((path (et--path-in-tree expr et--macroexpand-expr)))
      (if (eq path 'NO) (funcall func expr)
        (let* ((et--sticky-path nil))
          (et-at path (funcall func expr)))))))

(advice-add #'et--check :around #'et--macroexpand-check-advice)

(defun et-macroexpand-checker ()
  "Type checker which expands a macro and type-checks the expansion."

  (let* ((expanded (macroexpand-1 et--checker-expr))
         ;; Only the very root macroexpand expr exists in the actual code.
         ;; If we get another expansion inside an expansion, keep the original root-level expr
         (et--macroexpand-expr (or et--macroexpand-expr et--checker-expr)))
    (et-with-sticky-path
     (et--check expanded))))


;;;; Progn/prog1 checkers

(defun et-progn-checker ()
  (et-checker-tail 1))

(defun et-prog1-checker ()
  (prog1 (et-checker-sub 1)
    (et-checker-remaining 2)))


;;; ============================================================
;;; Provide

(provide 'et-check)


;;; et-check.el ends here
