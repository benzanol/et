;;; et-check.el --- Type checking for et.el       -*- lexical-binding: t; -*-

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
  (when (and et-display-narrows (not et-running-tests))
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
individually, and then will be passed to `et--funcall' as a list to
determine the output type."
  (let* ((et--checker-expr expr))
    (et-failed-boundary
     (or (et-error-boundary nil (et--check-1))
         (progn (unless et--result-failed (et-err nil "Type checking failed mysteriously"))
                (et-never))))))

(defun et--check-1 ()
  (pcase et--checker-expr
    (`(,func . ,_args)
     (pcase nil
       ;; Custom checker
       ((and (let checker (get func 'et-checker)) (guard checker)
             (let output (funcall checker)))
        (if (or (null output) (et-type-p output)) output
          (et-fatal "Checker for `%s' had invalid return: %s" func output)))

       ;; Function type property
       ((and (let func-type (get func 'et-function-type)) (guard func-type))
        (let* ((args-type (et--tuple 'ConsR (et-checker-remaining 1)))
               (output-type (et--funcall func-type args-type)))
          (or output-type
              ;; If `et--result-failed' is already true, that means one of the arguments was invalid,
              ;; which means the true error was in the arguments, not this call
              (unless et--result-failed
                (et-err 0 "`%s' has type %s\\nInvalid arguments: %s" func
                        (et-pp func-type) (et-pp (et--remove-type-binds args-type)))))))

       (_ (et-err 0 "No type for `%s'" func))))

    ;; Type check a variable (a symbol which is neither a keyword, nil, or t)
    ((and sym (pred symbolp) (pred (not keywordp)) (guard sym) (guard (not (eq sym t))))
     (pcase nil
       ;; Check if the variable is locally scoped
       ((and (let var (et-get-symbol-var sym)) (guard var))
        (et--supersect
         (et-current-var-type var)
         (et-type (make-et-type-case :value (make-et-datatype :name 'Any)
                                     :typeofs (list var)))))

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
           (result (et--check (cons ',func params))))
      (or (equal type (et-result-type result))
          (et-err 0 "Expected %s, got %s" type (et-result-type result))))))

(defmacro et-assert-call-errors (func &rest arg-types)
  `(et-result-boundary
    (let* ((params (cl-loop for a in ',arg-types collect (list :type a)))
           (result (et--check (cons ',func params))))
      (unless (et-result-diagnostics result)
        (error "Succeeded with %s" (cl-prin1-to-string (et-result-type result))))
      t)))

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
         (return-spec (cadr arguments))
         (func-type
          (if gen-vec
              (let* ((matcher (et-parse-matcher arglist-spec gen-vec))
                     (output-struct (et--parse-struct return-spec (et-matcher-generics matcher) 'TYPE)))
                (et-dt 'DynFunction matcher output-struct))
            (et-dt 'Function (et-parse-type arglist-spec) (et-parse-type return-spec)))))
    (unless (eq (length arguments) 2)
      (error "Incorrect number of arguments"))

    (cl-loop for func in (if (symbolp funcs) (list funcs) funcs)
             collect `(put ',func 'et-checker nil) into exprs
             collect `(put ',func 'et-function-type ,func-type) into exprs
             finally return `(ignore ,@exprs))))


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
  (pcase-let* ((gens (et--gen-vec-generics gen-vec)))
    `(et--infer ,(make-et-matcher
                  :generics gens
                  :constraints (et--gen-vec-constraints gen-vec)
                  :dnf (et--parse-struct matcher-spec gens 'MATCHER))
                ,type
                (et-q ,(et--parse-struct output-spec gens 'TYPE)))))


;;; ============================================================
;;; Processing
;;;; Function definitions
;;;;; Signature

;; Function processing happens in two phases:
;;
;; 1. `et--parse-defun-signature' must be run after all types are
;; defined.
;;
;; 2. Checking the function body must happen after all functions and
;; variabels are defined.

(cl-defstruct et-func-sig
  (func-type nil :et *et-type)
  (props nil :et List)
  ;; Things necessary for typechecking the body
  (source nil :et List<Any>)
  (source-pos nil :et Integer) ; Position of source RELATIVE TO ARGLIST (1 if right after arglist)
  ;; Both vars and expected-return may contain the scoped types
  (scoped nil :et (List (Tuple EtGeneric Symbol List<EtConstraint>)))
  (vars nil :et List<*et-var>)
  (expected-return nil :et *et-type))

(defun et--parse-function-signature (body)
  "The current path should point to ARGLIST."
  (declare (et (body List)
               (@return Nil|*et-res<*et-func-sig>)))

  (when-let* ((arglist (car body))
              (declare-pos (1+ (cl-position 'declare (cdr body) :key #'car-safe)))
              (declare-block (nth declare-pos body))
              (et-pos (cl-position 'et declare-block :key #'car-safe))
              (et-block (nth et-pos declare-block)))

    (let* ((param-structs ; (name . struct)[]
            (et-at 0
              (cl-loop for group in (et--parse-arglist-params arglist)
                       collect (cl-loop for name in group collect (cons name nil)))))
           any-params return-struct gen-vec generics constraints props)

      ;; Parse the fields of the declare block
      (dotimes (form-idx (length et-block))
        (et-error-boundary (list declare-pos et-pos form-idx)
          (pcase (nth form-idx et-block)
            ((guard (eq 0 form-idx))) ; Skip the `et' symbol

            (`(@return ,spec)
             (when return-struct (et-fatal 0 "Multiple @return clauses"))
             (et-at 1
               (setq return-struct (et--parse-struct spec generics 'TYPE))))
            (`(@return . ,_) (et-fatal 0 "Expected (@return TYPE)"))

            (`(@generics ,(and gv (pred vectorp)))
             (when gen-vec (et-fatal 0 "Multiple @generic clauses"))
             (when return-struct (et-fatal 0 "@generic must come before @return"))
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

            (`(,(and name (pred symbolp)) ,spec)
             (let* ((entry (cl-loop for group in param-structs
                                    for entry = (cl-find name group :key #'cadr)
                                    when entry return entry
                                    finally do (et-fatal 0 "Not a parameter: %s" name))))
               (et-at 1
                 (setq any-params t)
                 (setcdr entry (et--parse-struct spec generics 'BOTH)))))

            (_ (error "Invalid format")))))

      ;; Construct the function signature
      (let* ((input (apply #'et--generate-func-input generics constraints param-structs))
             (scoped (et--make-scoped-datatypes (when (et-matcher-p input) input))))
        (et--with-scoped-datatypes scoped
          (make-et-func-sig
           :func-type (if (et-matcher-p input) (et-dt 'DynFunction input return-struct)
                        (et-dt 'Function input (et-structure-to-type return-struct nil)))
           :props props
           ;; Things necessary for typechecking the body
           :source (nthcdr declare-pos body)
           :source-pos (1+ declare-pos)
           ;; Both vars and expected-return may contain the scoped types
           :scoped scoped
           :vars
           (cl-loop for param-group in param-structs nconc
                    (cl-loop for (name . struct) in param-group
                             for type = (et-structure-to-type struct)
                             collect (make-et-var :name name :type type)))
           :expected-return
           (cl-loop for (name unique constraints) in scoped
                    collect (cons name (et-dt 'Scoped name unique constraints))
                    into gen-repls
                    finally return (et-structure-to-type return-struct gen-repls))))))))


;;;;; Parse arglist params

(defun et--parse-arglist-params (arglist)
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

(defun et--generate-func-input (generics constraints required optional key-params rest-params)
  "Build an input matcher or type from parameter specs.

GENERICS is a list of generic symbols, or nil.

REQUIRED, OPTIONAL, KEY/REST-PARAMS are alists of (SYMBOL . TYPE-STRUCT).

REST-PARAMS will have at most 1 entry.

Returns an `et-matcher' if GENERICS is non-nil, or an `et-type' if not."

  (let* ((required-structs (mapcar #'cdr required))
         (optional-structs (mapcar #'cdr optional))
         (tail (pcase (car rest-params)
                 (`(,_ . ,struct) struct)
                 ((and (guard key-params)
                       (let plist-args
                         (cl-loop for (name . struct) in key-params
                                  nconc (list (intern (format ":%s" name)) struct))))
                  (et-q (((S:DT PList ,@plist-args)))))
                 ('nil (et-q (((S:DT Literal nil)))))
                 (x (error "Invalid rest param: %s" x))))
         (opt-tail (et--func-sig-optional-tail optional-structs tail))
         (struct (et--func-sig-required-chain required-structs opt-tail)))

    (if generics
        (make-et-matcher
         :generics generics
         :constraints constraints
         :dnf struct)
      (et-structure-to-type struct))))

(defun et--func-sig-required-chain (structs tail)
  "Chain required STRUCTS into nested ConsR, terminated by TAIL."
  (if (null structs) tail
    (et-q (((S:ALIAS ConsR
                     ,(car structs)
                     ,(et--func-sig-required-chain (cdr structs) tail)))))))

(defun et--func-sig-optional-tail (opt-structs rest-tail)
  "Build the optional suffix of an arglist structure.

Each optional param introduces (or Nil ConsR<type~...>).
REST-TAIL is the structure for the &rest/&key tail, or nil for Nil."
  (pcase opt-structs
    ('nil (or rest-tail (et-q (((S:DT Literal nil))))))
    (`(,first . ,remaining)
     (let* ((inner (et--func-sig-optional-tail remaining rest-tail)))
       (et-q (((S:DT Literal nil))
              ((S:ALIAS ConsR ,first ,inner))))))))


(et-test
 (equal (et--generate-func-input nil nil '((x . (((S:DT Integer)))) (y . (((S:DT Any))))) nil nil nil)
        (et ConsR<Integer~ConsR<Any~Nil>>))

 (equal (et--generate-func-input nil nil '((x . (((S:DT Integer)))) (y . (((S:DT String))))) nil nil
                                 '((args . (((S:ALIAS ListR (((S:DT Any)))))))))
        (et ConsR<Integer~ConsR<String~ListR<Any>>>))

 (equal (et--generate-func-input '(T) nil '((x . (((S:GENERIC T))))) '((y . (((S:DT Number)))))
                                 nil '((args . (((S:ALIAS ListR (((S:DT String)))))))))
        (et-matcher [T]
          ConsR<T~{Nil|ConsR<Number~ListR<String>>}>))

 (equal (et--generate-func-input '(T) nil '((a . (((S:GENERIC T))))) nil '((scale . (((S:DT Number)))) (flag . (((S:DT Any))))) nil)
        (et-matcher [T]
          ConsR<T~PList<:scale~Number~:flag~Any>>)))


;;;;; Identification

(defun et--identify-defun (body)
  (list
   :declare
   (lambda ()
     (when-let* ((name (cadr body))
                 (sig (et-at-offset 2 (et--parse-function-signature (cddr body)))))
       (put name 'et-function-signature name)
       (put name 'et-function-type (et-func-sig-func-type sig))))))


;;;;; Make function type

(defun et--make-function-type (generics constraints input-struct output-struct)
  "Build a Function or DynFunction type from structures.

If GENERICS is non-nil, returns a DynFunction with a matcher built from
GENERICS, CONSTRAINTS, and INPUT-STRUCT, and OUTPUT-STRUCT as the output
structure.

If GENERICS is nil, returns a Function with INPUT-STRUCT and
OUTPUT-STRUCT each converted to concrete types."
  (if generics
      (et-dt 'DynFunction
             (make-et-matcher :generics generics
                              :constraints constraints
                              :dnf input-struct)
             output-struct)
    (et-dt 'Function
           (et-structure-to-type input-struct nil)
           (et-structure-to-type output-struct nil))))


;;;;; Checker

(et-define-pcase-checker defun `(,name . ,_)
  (when-let* ((sig (get name 'et-function-signature)))
    (et--with-scoped-datatypes (et-func-sig-scoped sig)
      (et-with-vars (et-func-sig-vars sig)
        (let* ((actual-ret (et-checker-tail (+ 2 (et-func-sig-source-pos sig))))
               (expected-ret (et-func-sig-expected-return sig)))
          (or (et-subtype? actual-ret expected-ret)
              (et-err 0 "Expected %s, found %s" actual-ret expected-ret))))))
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
              ;; Generic structure helpers used across all generated functions
              (gen-structs (mapcar (lambda (g) (list (list (list 'S:GENERIC g)))) generics))
              (struct-struct (et-q (((S:DT Struct ,name ,@gen-structs)))))
              (consR-struct-nil (et-q (((S:ALIAS ConsR ,struct-struct (((S:DT Literal nil)))))))))

         ;; --- Predicate ---
         (when predicate
           (let* ((placeholder-struct
                   (et--parse-struct
                    '(or (and True (bindsof (and T *placeholder)))
                         (and Nil (bindsof (subtract T *placeholder))))
                    '(T) 'TYPE))
                  (output-struct
                   (cl-subst (cons 'S:DT (cons 'Struct (cons name gen-structs)))
                             '(S:DT Struct placeholder)
                             placeholder-struct :test #'equal)))
             (put predicate 'et-function-type
                  (et-dt 'DynFunction (et-parse-matcher 'Any [T]) output-struct))))

         ;; --- Accessors ---
         (dolist (slot slots)
           (pcase-let* ((`(,rel ,slot-name ,_default ,type-info) slot)
                        (accessor (if conc-name (intern (format "%s%s" conc-name slot-name))
                                    slot-name)))
             (et-error-boundary rel
               (let* ((slot-struct (if type-info
                                       (et-at (car type-info)
                                         (et--parse-struct (cdr type-info) generics
                                                           (if generics 'TYPE 'TYPE)))
                                     (et-q (((S:DT Any)))))))
                 (put accessor 'et-function-type
                      (et--make-function-type generics constraints
                                              consR-struct-nil slot-struct))))))

         ;; --- Constructor ---
         (when constructor
           (let* ((input-struct
                   (cl-loop for (rel slot-name _default type-info) in slots
                            nconc (list (intern (format ":%s" slot-name))
                                        (if type-info
                                            (et-error-boundary rel
                                              (et-at (car type-info)
                                                (et--parse-struct (cdr type-info) generics
                                                                  (if generics 'BOTH 'TYPE))))
                                          (et-q (((S:DT Any))))))
                            into args
                            finally return (if args (et-q (((S:DT PList ,@args))))
                                             (et-q (((S:DT Literal nil))))))))
             (put constructor 'et-function-type
                  (et--make-function-type generics constraints
                                          input-struct struct-struct))))

         ;; --- Copier ---
         (when copier
           (put copier 'et-function-type
                (et--make-function-type generics constraints
                                        consR-struct-nil struct-struct))))))))


;;;; Identify alias

(defun et--identify-alias-def (args)
  "Identify an alias definition, returning a processing plist.

During identification, declares the alias name and generics.
Returns a plist with :constrain and :populate functions."
  (let* ((orig-args args)
         (name (pop args))
         (_ (or (symbolp name)
                (et-fatal 0 "Alias name must be a symbol")))
         (gen-vec (if (vectorp (car args)) (pop args) []))
         (pb (condition-case nil (et--props-and-body args)
               (error (et-fatal 0 "Expected format (@alias NAME [GENERICS] [PROPS...] TYPE)")))))

    ;; Identification phase: declare the alias name, generics, and spec
    ;; (but don't parse structure or constraints yet)
    (apply #'et--declare-alias name gen-vec (cdr pb) (car pb))

    (let* ((spec-idx (- (length orig-args) 1))
           (gen-vec-idx (when (vectorp (nth 1 orig-args)) 1)))

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
         ;; Parse the spec into a structure
         (et-at spec-idx
           (let* ((props (or (get name 'et-alias)
                             (et-fatal nil "Alias `%s' not declared" name)))
                  (spec (plist-get props :spec))
                  (generics (plist-get props :generics))
                  (restrict (plist-get props :restrict)))
             (plist-put props :structure
                        (et--parse-struct spec generics restrict)))))))))


;;;; Identify variable def

(defun et--identify-variable-def (args)
  "Identify a variable definition, returning a processing plist.

During identification, just validates the format.
Returns a plist with :declare to set the variable type."
  (pcase args
    (`(,(and name (pred symbolp)) ,spec)
     (list
      :declare
      (lambda ()
        (et-at 1
          (put name 'et-variable-type (et-parse-type spec))))))

    (_ (et-fatal 0 "Expected format (@variable NAME TYPE)"))))


;;;; Identify expr

(defun et--identify-expr (expr)
  (et-error-boundary nil
    (pcase expr

      ;; Process a root declaration block
      ((and (pred vectorp) (app (lambda (v) (append v nil)) `(et . ,forms)))
       (cl-loop
        for form in forms
        for form-idx upfrom 0
        collect
        (et-error-boundary form-idx
          (pcase form
            (`(@alias . ,args) (et--identify-alias-def args))
            (`(@variable . ,args) (et--identify-variable-def args))))))

      ;; Process a defun
      (`(defun ,(pred symbolp) ,(pred listp) . ,_)
       (list (et--identify-defun expr)))

      ;; Process a struct
      (`(cl-defstruct . ,_)
       (list (et--identify-cl-defstruct expr))))))


;;;; Process exprs

(defun et--process-exprs (exprs)
  (let* ((identified (et-result-map #'et--identify-expr exprs)))

    ;; For each expression, et--identify-expr returns a list of
    ;; process-plists. A process-plist describes how to process that
    ;; expression in each phase. For each phase, we want to perform
    ;; the correct action across all expressions.

    (dolist (phase '(:constrain :populate :declare))
      (cl-loop for expr-plists in identified
               for idx upfrom 0
               do (et-at idx
                    (dolist (plist expr-plists)
                      (when-let* ((func (plist-get plist phase)))
                        (funcall func))))))

    ;; Check all root-level expressions
    (cl-loop for expr in exprs
             for idx upfrom 0
             when (and (consp expr)
                       (or (get (car expr) 'et-function-signature)
                           (get (car expr) 'et-checker)))
             do (et-at idx (et--check expr)))))

(defun et--process-buffer ()
  (save-excursion
    (goto-char (point-min))
    (et--process-exprs
     (cl-loop while t
              for expr = (condition-case _ (read (current-buffer))
                           (error (cl-return exprs)))
              collect expr into exprs))))


;;;; Flycheck check

(defun et--flycheck-check-file ()
  "Entry point for batch-mode type checking."
  (let* ((filename (pop command-line-args-left)))
    (with-temp-buffer
      (insert-file-contents filename)
      (emacs-lisp-mode)
      (cl-loop for (path severity msg)
               in (et-result-diagnostics (et-result-boundary (et--process-buffer)))
               do (goto-char (point-min))
               do (ignore-errors (et--traverse-buffer-expr path))
               for start-line = (line-number-at-pos)
               for start-col = (1+ (current-column))
               do (ignore-errors (forward-sexp))
               do (princ (format "%s:%d:%d:%s:%s: %s: %s path=%s\n"
                                 filename
                                 start-line start-col
                                 (line-number-at-pos) (1+ (current-column))
                                 severity msg path))
               (flycheck-mode 1)))))


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
  (let ((type (et-checker-sub 1)))
    (et-hint nil (et--remove-type-binds type))
    (setq et--checker-expr (cadr et--checker-expr))
    type))

(et-define-pcase-checker :typeof+ `(,_expr)
  (let ((type (et-checker-sub 1)))
    (et-hint nil type)
    (setq et--checker-expr (cadr et--checker-expr))
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


;;; ============================================================
;;; Provide

(provide 'et-check)


;;; et-check.el ends here
