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
             when binds do (et-checker-hint path fmt (et-pp-narrows binds)))))


;;; ============================================================
;;; Result

[et (@alias EtDiagnostic (Tuple List<Integer> Symbol String))]

(cl-defstruct et-res
  (value nil :et-generics [T] :et T|Nil)
  (failed nil :et Boolean)
  (diagnostics nil :et List<EtDiagnostic>))

(defvar et--res-path nil)
(defvar et--res-diagnostics nil)
(defvar et--res-failed nil)

(defmacro et--res (&rest body)
  (declare (et (@generics [T])
               (@return *et-res<T>)))

  `(let* ((et--res-path nil)
          (et--res-diagnostics nil)
          (et--res-failed nil))
     (make-et-res
      :value (et--res-error-boundary nil ,@body)
      :failed et--res-failed
      :diagnostics et--res-diagnostics)))

(defmacro et--res-at (rel &rest body)
  (declare (indent 1))
  (let* ((orig-var (gensym 'path)))
    ;; On error, we want the path to stay where it is, hence using setq instead of let
    `(let* ((,orig-var et--res-path))
       (setq et--res-path (append et--res-path (flatten-list (list ,rel))))
       (prog1 (progn ,@body)
         (setq et--res-path ,orig-var)))))

(defmacro et--res-error-boundary (rel &rest body)
  (declare (indent 1))
  (let* ((inner
          `(condition-case err (progn . ,body)
             (error (setq et--res-failed t)
                    (push (list (append et--res-path nil) 'error (error-message-string err))
                          et--res-diagnostics)
                    nil))))
    (if (eq rel nil) inner `(et--res-at ,rel ,inner))))

(defun et--res-diag (rel severity fmt &rest args)
  (setq rel (flatten-list (list rel)))
  (let* ((str (if args (apply #'format fmt args) fmt)))
    (push (list (append et--res-path rel) severity str) et--res-diagnostics)
    nil))

(defun et--res-fatal (rel fmt &rest args)
  (setq et--res-path (append et--res-path (flatten-list (list rel))))
  (if args (apply #'error fmt args) (error "%s" fmt)))


;;; ============================================================
;;; Checking
;;;; Result struct

(cl-defstruct et-result
  "Result of parsing an expression.

TYPE is an `et-type'.

DIAGNOSTICS is a list (PATH SEVERITY STRING)[] of diagnostics resulting
from type checking the expression. If this value is non-nil, then TYPE
is not guaranteed to be non-nil (but it might be), and the entire call
tree should propagate these errors.

COMPILED is the compiled version of the expression that was being
checked.

FAILED is a boolean that is `t' when type checking was invalid."
  type diagnostics compiled failed)


;;;; Check

(defvar et--checker-diagnostics nil
  "Diagnostics signalled on the current call to this checker.")

(defvar et--checker-expr nil
  "The current expr.")

(defvar et--checker-failed nil
  "Whether type checking the current expr failed.")

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
  (let* ((et--checker-diagnostics nil)
         (et--checker-expr (copy-tree expr))
         (et--checker-failed nil)
         (return-type nil))

    (pcase expr
      (`(,func . ,_args)
       (pcase nil
         ;; Custom checker
         ((and (let checker (get func 'et-checker)) (guard checker))
          (condition-case out (funcall checker)
            (et-checker-fatal)
            (error (et-checker-err "Checker for `%s' threw error: %s" func (error-message-string out)))
            (:success (if (or (null out) (et-type-p out)) (setq return-type out)
                        (et-checker-err "Checker for `%s' had invalid return: %s" func out)))))

         ;; Function type property
         ((and (let func-type (get func 'et-function-type)) (guard func-type))
          (let* ((args-type (et--tuple 'ConsR (et-checker-remaining 1)))
                 (output-type (condition-case err (et--funcall func-type args-type)
                                (error (et-checker-err 0 "%s" (error-message-string err))))))
            (if output-type (setq return-type output-type)
              ;; If `et--checker-failed' is already true, that means one of the arguments was invalid,
              ;; which means the true error was in the arguments, not this call
              (unless et--checker-failed
                (et-checker-err "`%s' has type %s\\nInvalid arguments: %s" func
                                (et-pp func-type)
                                (et-pp (et--remove-type-binds args-type)))))))

         (_ (et-checker-err '(0) "No type for `%s'" func))))

      ;; Type check a variable (a symbol which is neither a keyword, nil, or t)
      ((and sym (pred symbolp) (pred (not keywordp)) (guard sym) (guard (not (eq sym t))))
       (pcase nil
         ;; Check if the variable is locally scoped
         ((and (let var (et-get-symbol-var sym)) (guard var))
          (setq return-type
                (et--supersect
                 (et-current-var-type var)
                 (et-type (make-et-type-case :value (make-et-datatype :name 'Any)
                                             :typeofs (list var))))))

         ;; Check if it is a global variable with a type
         ((and (let type (get sym 'et-variable-type)) (guard type))
          (setq return-type type))

         (_ (et-checker-err "Free variable: %s" sym))))

      (expr (setq return-type (et-literal expr))))

    ;; If it returned nil, then it failed
    (when (null return-type) (setq et--checker-failed t))
    ;; This shouldn't happen: checkers should always report a real error if returning nil
    (when (and et--checker-failed (null et--checker-diagnostics))
      (et-checker-err "Type checking failed mysteriously"))

    (make-et-result :type (or return-type (et-never))
                    :diagnostics (nreverse et--checker-diagnostics)
                    :compiled et--checker-expr
                    :failed et--checker-failed)))

(defmacro et-check-call (func &rest args)
  `(let ((result (et--check '(,func ,@(cl-loop for a in args collect `(:type ,a))))))
     (or (mapcar #'caddr (et-result-diagnostics result)) (et-result-type result))))


;;;; Diagnostic helpers

(defun et--checker-diagnostic (severity &rest args)
  (let* ((path (if (stringp (car args)) nil
                 (flatten-tree (pop args)))))
    (push (list path severity (if (cdr args) (apply #'format args) (car args)))
          et--checker-diagnostics)
    nil))

(defmacro et--define-diagnostics-function (name severity &optional failed)
  `(defun ,name (&rest args)
     ,(format "Create a checker diagnostic with severity `%s'.\n\n%s" severity
              "(fn [PATH] FORMAT-STRING ARGS...)")
     ,@(when failed (list '(setq et--checker-failed t)))
     (apply #'et--checker-diagnostic ',severity args)
     nil))

(et--define-diagnostics-function et-checker-err error t)
(et--define-diagnostics-function et-checker-warn warning)
(et--define-diagnostics-function et-checker-hint hint)

(define-error 'et-checker-fatal "Signalled by a checker which has a fatal problem.")
(defun et-checker-fatal (path fmt &rest args)
  (apply #'et--checker-diagnostic 'fatal path fmt args)
  (signal 'et-checker-fatal nil))


;;;; Root level functions

(defun et-show-result-errors (result &optional path offset)
  "Display the errors contained in RESULT.

PATH is a path to add before the path of each message.

OFFSET is a numeric offset to add to the first entry in each existing
path."

  (cl-loop for (mpath _severity message) in (et-result-diagnostics result)
           for new-path = (append path (if (and mpath offset)
                                           (cons (+ (car mpath) offset) (cdr mpath))
                                         mpath))
           do (et-error new-path message)))

(defmacro et-typecheck (body)
  (let* ((result (et--check body)))
    (et-with-error-path '(1) (et-show-result-errors result))
    (et-simplify-type (et-result-type result))))

(defmacro et-typecheck-call (func &rest arg-types)
  (cl-loop for type in arg-types
           if (eq (car-safe type) :eval-type) collect type into arg-exprs
           else collect (list :type type) into arg-exprs
           finally return `(et-typecheck (,func ,@arg-exprs))))

(defmacro et-compile (body)
  (let* ((result (et--check body)))
    (et-error '(0) (et-pp (et-result-type result)))
    (et-with-error-path '(1) (et-show-result-errors result))
    (et-result-compiled result)))


;;;; Tests

(defmacro et-assert-resolve (type expr &optional not)
  (declare (indent 1))
  `(let* ((t-type (et-with-error-path '(1) (et ,type)))
          (result (et--check ',expr)))
     (et-with-error-path '(2) (et-show-result-errors result))
     (or (,(if not #'not #'identity) (et-subtype? (et-result-type result) t-type))
         (et-error '(0) "Expected %s, got %s" (et-pp t-type) (et-pp (et-result-type result))))))

(defmacro et-assert-resolve-errors (expr)
  `(or (et-result-diagnostics (et--check ',expr))
       (et-error '(0) "No errors")))

(defmacro et-assert-no-resolve (type expr)
  (declare (indent 1))
  `(et-assert-resolve ,type ,expr 'NOT))

(defmacro et-assert-call (type-spec func &rest arg-types)
  `(let* ((type (et ,type-spec))
          (params (cl-loop for a in ',arg-types collect (list :type a)))
          (result (et--check (cons ',func params))))
     (et-with-error-path '(2) (et-show-result-errors result))
     (or (equal type (et-result-type result))
         (et-error '(0) "Expected %s, got %s" (et-pp type) (et-pp (et-result-type result))))))

(defmacro et-assert-call-errors (func &rest arg-types)
  `(let* ((params (cl-loop for a in ',arg-types collect (list :type a)))
          (result (et--check (cons ',func params))))
     (unless (et-result-diagnostics result)
       (error "Succeeded with %s" (cl-prin1-to-string (et-result-type result))))
     t))

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
;;;; Check subexpr

(defun et--traverse-tree (path tree)
  (if (null path) tree
    (when (>= (car path) (length tree))
      (error "Index out of bounds: %s %s" (car path) tree))
    (et--traverse-tree (cdr path) (nth (car path) tree))))

(defun et-checker-sub (&rest path)
  "Type check the sub expression at PATH, returning the type or never."
  (cl-assert et--checker-expr)
  (setq path (flatten-tree path))

  (let* ((path-last (car (last path)))
         (parent-expr (et--traverse-tree (butlast path) et--checker-expr))
         (sub-expr (nth path-last parent-expr))
         (sub-result (et--check sub-expr)))
    (cl-assert (et-result-p sub-result))

    ;; Diagnostics in the result have paths relative to sub-expr
    ;; Rebase them to be relative to et--checker-expr
    (cl-loop for (p severity message) in (et-result-diagnostics sub-result)
             do (et--checker-diagnostic severity (append path p) message))

    ;; If the child failed, then this one failed as well
    (when (et-result-failed sub-result)
      (setq et--checker-failed t))

    ;; Update the current expr to be the new compiled version
    (setf (nth path-last parent-expr) (et-result-compiled sub-result))

    ;; Return just the inner type, or never
    (or (et-result-type sub-result) (et-never))))


;;;; Check multiple subexprs

(defun et-checker-remaining (&rest first-path)
  (cl-assert et--checker-expr)
  (cl-assert first-path)
  (setq first-path (flatten-tree first-path))

  (let* ((parent-path (butlast first-path 1))
         (parent-expr (et--traverse-tree parent-path et--checker-expr))
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
      (et-checker-err path "Expected %s, found %s" (et-pp type) (et-pp expr-type)))
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
;;;; Preprocess a defun
;;;;; Root

(cl-defstruct et-func-sig
  (func-type nil :et *et-type)
  (props nil :et List)
  ;; Things necessary for typechecking the body
  (source nil :et List<Any>)
  (source-path nil :et List<Integer>)
  ;; Both vars and expected-return may contain the scoped types
  (scoped nil :et (List (Tuple EtGeneric Symbol List<EtConstraint>)))
  (vars nil :et List<*et-var>)
  (expected-return nil :et *et-type))

;; This function takes a function name, arglist, and body, and

(defun et--parse-defun-signature (arglist-pos arglist body)
  (declare (et (arglist List)
               (body List)
               (@return Nil|*et-res<*et-func-sig>)))

  (when-let* ((declare-pos (cl-position 'declare body :key #'car-safe))
              (declare-block (nth declare-pos body))
              (et-pos (cl-position 'et declare-block :key #'car-safe))
              (et-block (nth et-pos declare-block)))

    (et--res
     (let* ((param-structs ; (name . struct)[]
             (et--res-at (list arglist-pos)
               (cl-loop for group in (et--parse-arglist-params arglist)
                        collect (cl-loop for name in group collect (cons name nil)))))
            any-params return-struct gen-vec generics constraints props)

       ;; Parse the fields of the declare block
       (dotimes (form-idx (length et-block))
         (et--res-error-boundary (list (+ arglist-pos declare-pos) et-pos form-idx)
           (pcase (nth form-idx et-block)
             ((guard (eq 0 form-idx))) ; Skip the `et' symbol

             (`(@return ,spec)
              (when return-struct (et--res-fatal 0 "Multiple @return clauses"))
              (et--res-at 1
                (setq return-struct (et--parse-struct spec generics 'TYPE))))
             (`(@return . ,_) (et--res-fatal 0 "Expected (@return TYPE)"))

             (`(@generics ,(and gv (pred vectorp)))
              (when gen-vec (et--res-fatal 0 "Multiple @generic clauses"))
              (when return-struct (et--res-fatal 0 "@generic must come before @return"))
              (when any-params (et--res-fatal 0 "@generic must come before parameter declarations"))
              (et--res-at 1
                (setq gen-vec gv
                      generics (et--gen-vec-generics gv)
                      constraints (et--gen-vec-constraints gv))))
             (`(@generics . ,_) (et--res-fatal 0 "Expected (@generics [VARS...])"))

             (`(@skip)
              (when (plist-get props :skip) (et--res-fatal 0 "Multiple @skip clauses"))
              (setq props (cl-list* :skip t props)))

             ;; Can contain `narrows', `vars', `all'
             (`(@show . ,show)
              (when (plist-get props :show) (et--res-fatal 0 "Multiple @show clauses"))
              (setq props (cl-list* :skip show props)))

             (`(,(and name (pred symbolp)) ,spec)
              (let* ((entry (cl-loop for group in param-structs
                                     for entry = (cl-find name group :key #'cadr)
                                     when entry return entry
                                     finally do (et--res-fatal 0 "Not a parameter: %s" name))))
                (et--res-at 1
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
            :source (nthcdr (1+ declare-pos) body)
            :source-path (list (+ arglist-pos 1 declare-pos))
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
                     finally return (et-structure-to-type return-struct gen-repls)))))))))


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


;;;; Preprocess cl-defstruct

(defun et--preprocess-cl-defstruct (body)
  "Preprocess a `cl-defstruct' expression."

  (let* ((orig-path et--preprocess-path)
         (name-or-opts (car body))
         (name (if (consp name-or-opts) (car name-or-opts) name-or-opts))
         (opts (when (consp name-or-opts) (cdr name-or-opts)))
         ;; Parse options for renamed functions
         (conc-name (if-let* ((entry (assq :conc-name opts)))
                        (cadr entry) (intern (format "%s-" name))))
         (constructor (if-let* ((entry (assq :constructor opts)))
                          (cadr entry) (intern (format "make-%s" name))))
         (copier (if-let* ((entry (assq :copier opts)))
                     (cadr entry) (intern (format "copy-%s" name))))
         (predicate (if-let* ((entry (assq :predicate opts)))
                        (cadr entry) (intern (format "%s-p" name))))

         ;; Skip docstring
         (slots-start (if (stringp (cadr body)) 2 1))
         (slot-forms (nthcdr slots-start body))
         slots gen-vec generics)

    (dotimes (slot-idx (length slot-forms))
      (setq et--preprocess-path (append orig-path (list (+ 1 slots-start slot-idx))))

      (pcase (nth slot-idx slot-forms)
        ((and name (pred symbolp)) (push (list et--preprocess-path name) slots))

        (`(,(and name (pred symbolp)) ,default . ,plist)
         ;; Process the slot type
         (if-let* ((type-pos (cl-position :et plist)) ((= 0 (mod type-pos 2))))
             (push (list et--preprocess-path name default
                         (cons (append et--preprocess-path (list (+ 3 type-pos))) (nth (1+ type-pos) plist)))
                   slots)
           (push (list et--preprocess-path name default) slots))

         ;; Process generics
         (when-let* ((gv-pos (cl-position :et-generics plist)) ((= 0 (mod gv-pos 2))))
           (cl-callf append et--preprocess-path (list (+ 3 gv-pos)))
           (if (= 0 slot-idx)
               (setq gen-vec (cons (append et--preprocess-path (list (+ 3 gv-pos)))
                                   (nth (1+ gv-pos) plist))
                     generics (et--gen-vec-generics (cdr gen-vec)))
             (error "Generics must be set in the first slot"))))

        (_ (error "Invalid slot format"))))

    (setq )
    (put name 'et-struct (list :generics generics))

    (list orig-path name gen-vec slots
          (list conc-name constructor copier predicate))))


;;;; Preprocess helpers

(defun et--preprocess-alias-def (args)
  (if-let* ((spec-pos (length args))
            (name (pop args))
            ((symbolp name))
            (gen-vec (if (vectorp (car args)) (pop args) []))
            (pb (ignore-errors (et--props-and-body args))))
      ;; Just declare the alias, don't ensure its validity by parsing yet
      (progn (apply #'et--declare-alias name gen-vec (cdr pb) (car pb))
             (list (append et--preprocess-path (list spec-pos)) name))

    (cl-callf append et--preprocess-path (list 0))
    (error "Expected format (@alias NAME [GENERICS] [PROPS...] TYPE)")))

(defun et--preprocess-variable-def (args)
  (pcase args
    (`(,(and name (pred symbolp)) ,spec)
     (list et--preprocess-path name spec))

    (_ (cl-callf append et--preprocess-path (list 0))
       (error "Expected format (@variable NAME TYPE)"))))


;;;; Preprocess

(defun et--preprocess (exprs)
  (let* ((declared-aliases nil) ; List<(Spec-path Symbol)>
         (declared-vars nil) ; List<(Expr-path Symbol Spec)>
         (declared-defuns nil)
         (declared-structs nil)
         (errors nil)
         (et--preprocess-path nil))

    ;; Process all exprs, collecting things that were declared without parsing anything
    (dotimes (expr-idx (length exprs))
      (setq et--preprocess-path (list expr-idx))
      (condition-case err
          (pcase (nth expr-idx exprs)
            ;; Process a root declaration block
            ((and (pred vectorp) (app (lambda (v) (append v nil)) `(et . ,forms)))
             (dotimes (form-idx (length forms))
               (setq et--preprocess-path (list expr-idx (1+ form-idx)))
               (pcase (nth form-idx forms)
                 (`(@alias . ,args) (push (et--preprocess-alias-def args) declared-aliases))
                 (`(@variable . ,args) (push (et--preprocess-variable-def args) declared-vars)))))
            ;; Process a defun
            (`(defun ,(and name (pred symbolp)) ,(and arglist (pred listp)) . ,args)
             (when-let* ((decl (et--preprocess-defun name arglist args))) (push decl declared-defuns)))
            ;; Process a struct
            (`(cl-defstruct . ,body)
             (push (et--preprocess-cl-defstruct body) declared-structs)))

        (error (push (cons et--preprocess-path (error-message-string err)) errors))))

    (list errors
          (nreverse declared-aliases)
          (nreverse declared-vars)
          (nreverse declared-defuns)
          (nreverse declared-structs))))


;;;; Populate defun
;;;;; Root

(defun et--preprocess-defun (name arglist args)
  (declare
   (et (@return (or Nil
                    (Tuple List<Integer> ; Path
                           Symbol ; Name
                           (or Nil (Cons List<Integer> Vector)) ; Generics
                           (Cons List<Integer> Any) ; Return
                           (Cons List<Integer> List<Any>) ; Source
                           (List Any) ; Extra props
                           (List (Tuple List<Integer> Symbol Any)) ; Required
                           (List (Tuple List<Integer> Symbol Any)) ; Optional
                           (List (Tuple List<Integer> Symbol Any)) ; Key
                           (List (Tuple List<Integer> Symbol Any)) ; Rest
                           )))))

  (when-let* ((orig-path et--preprocess-path)
              (declare-pos (cl-position 'declare args :key #'car-safe))
              (declare-block (nth declare-pos args))
              (et-pos (cl-position 'et declare-block :key #'car-safe))
              (et-block (nth et-pos declare-block)))
    (cl-callf append et--preprocess-path (list (+ declare-pos 3) et-pos))

    (let* ((source (cons (append orig-path (list (+ 4 declare-pos)))
                         (nthcdr (1+ declare-pos) args)))
           (et-block-path et--preprocess-path)
           (params
            (cl-loop for group in (et--parse-arglist-params arglist)
                     for group-idx upfrom 0
                     collect
                     (cl-loop for name in group
                              for spec = (if (eq 3 group-idx) 'ListR<Any> 'Any)
                              collect (list nil name spec))))
           return gen-vec props)

      (dotimes (form-idx (length et-block))
        (setq et--preprocess-path (append et-block-path (list form-idx)))

        (pcase (nth form-idx et-block)
          ((guard (eq 0 form-idx))) ; Skip the `et' symbol

          (`(@return ,spec)
           (when return (error "Multiple @return clauses"))
           (setq return (cons (append et--preprocess-path (list 1)) spec)))
          (`(@return . ,_) (error "Expected (@return TYPE)"))

          (`(@generics ,(and gv (pred vectorp)))
           (when gen-vec (error "Multiple @generic clauses"))
           (setq gen-vec (cons (append et--preprocess-path (list 1)) gv)))
          (`(@generics . ,_) (error "Expected (@generics [...])"))

          (`(@skip)
           (when (plist-get props :skip) (error "Multiple @skip clauses"))
           (setq props (cl-list* :skip t props)))

          ;; Can contain `narrows', `vars', `all'
          (`(@show . ,show)
           (when (plist-get props :show) (error "Multiple @show clauses"))
           (setq props (cl-list* :skip show props)))

          (`(,(and name (pred symbolp)) ,spec)
           (cl-loop for group in params
                    for entry = (cl-find name group :key #'cadr)
                    when entry
                    do (progn (setcar entry (append et--preprocess-path (list 1)))
                              (setf (caddr entry) spec)
                              (cl-return nil))
                    finally do (error "Not a parameter: %s" name)))

          (_ (error "Invalid format"))))

      (when return
        (list orig-path name
              gen-vec return source
              props
              params)))))

(cl-defun et--populate-defun (name gen-vec return source props param-types)
  (let* ((orig-path et--preprocess-path)

         (_ (setq et--preprocess-path (car gen-vec)))
         (generics (et--gen-vec-generics (cdr gen-vec)))
         (constraints (et--gen-vec-constraints (cdr gen-vec)))

         ;; Parse each param to a struct
         (param-structs
          (cl-loop for group in param-types
                   collect
                   (cl-loop for (path name spec) in group
                            do (setq et--preprocess-path path)
                            collect (cons name (et--parse-struct spec generics 'BOTH)))))

         (input (apply #'et--generate-func-input generics constraints param-structs))

         (_ (setq et--preprocess-path (car return)))
         (return-struct (et--parse-struct (cdr return) generics 'TYPE))
         (func-type
          (if generics
              (et-dt 'DynFunction input return-struct)
            (et-dt 'Function input (et-structure-to-type return-struct nil)))))

    (when (et-matcher-p input) (put name 'et-function-matcher input))
    (put name 'et-function-type func-type)

    ;; We return a function that can later (after populating is
    ;; complete) be used to type-check this function.
    (unless (plist-get props :skip)
      (let* ((scoped (et--make-scoped-datatypes (when (et-matcher-p input) input)))
             (vars-and-ret
              (et--with-scoped-datatypes scoped
                (cons (cl-loop for param-group in param-types nconc
                               (cl-loop for (path name struct) in param-group
                                        do (setq et--preprocess-path path)
                                        for type = (et-structure-to-type struct)
                                        collect (make-et-var :name name :type type)))
                      (cl-loop for (name unique constraints) in scoped
                               collect (cons name (et-dt 'Scoped name unique constraints))
                               into gen-repls
                               finally return (et-structure-to-type return-struct gen-repls))))))
        (list orig-path
              (car source)
              (cdr source)
              scoped
              (car vars-and-ret)
              (cdr vars-and-ret))))))


;;;; Populate cl-defstruct

(cl-defun et--populate-defstruct (name gen-vec slots (conc-name constructor copier predicate))
  (let* ((plist (or (get name 'et-struct) (error "Struct `%s' not defined" name)))
         (generics (plist-get plist :generics))

         ;; Parse the constraints
         (_ (cl-callf append et--preprocess-path (car gen-vec)))
         (constraints (et--gen-vec-constraints (cdr gen-vec)))
         (_ (plist-put plist :constraints constraints))
         ;; Create a "default" struct type for use in the predicate
         (default-generic-vals (et--match-satisfy-constraints-smallest generics constraints))
         (_ (when (eq 'INVALID default-generic-vals)
              (error "Unsatisfiable constraints")))
         (struct-type (apply #'et-dt 'Struct name default-generic-vals)))

    ;; Predicate: (Any) -> True&{bindsof Struct<NAME>} | Nil&{bindsof ¬Struct<NAME>}
    (when predicate
      (let* ((matcher (et-parse-matcher 'Any [T]))
             (placeholder-struct
              (et--parse-struct
               '(or (and True (bindsof (and T *placeholder)))
                    (and Nil (bindsof (subtract T *placeholder))))
               '(T) 'TYPE))
             (gs (mapcar (lambda (g) (list (list (list 'S:GENERIC g)))) generics))
             (output-struct
              (cl-subst (cons 'S:DT (cons 'Struct (cons name gs)))
                        (list 'S:DT 'Struct 'placeholder)
                        placeholder-struct
                        :test #'equal)))
        (put predicate 'et-function-type
             (et-dt 'DynFunction matcher output-struct))))

    ;; Accessors: (Struct<NAME G...>) -> SLOT-TYPE
    (dolist (slot slots)
      (pcase-let* ((`(,path ,slot-name ,_default . ,type-info) slot)
                   (type-spec-entry (car type-info))
                   (accessor-name (if conc-name
                                      (intern (format "%s%s" conc-name slot-name))
                                    slot-name)))
        (setq et--preprocess-path path)
        (if (null generics)
            ;; No generics: plain Function
            (let* ((slot-type (if type-spec-entry
                                  (progn (setq et--preprocess-path (car type-spec-entry))
                                         (et-parse-type (cdr type-spec-entry)))
                                (et-any)))
                   (arg-type (et-alias 'ConsR struct-type (et-literal nil))))
              (put accessor-name 'et-function-type
                   (et-dt 'Function arg-type slot-type)))
          ;; Has generics: DynFunction so generics propagate
          (let* ((slot-struct (if type-spec-entry
                                  (progn (setq et--preprocess-path (car type-spec-entry))
                                         (et--parse-struct (cdr type-spec-entry) generics 'TYPE))
                                (et-q (((S:DT Any))))))
                 (input-struct
                  (et-q (((S:ALIAS ConsR
                                   (((S:DT Struct ,name
                                           ,@(mapcar (lambda (g) (list (list (list 'S:GENERIC g))))
                                                     generics))))
                                   (((S:DT Literal nil))))))))
                 (matcher (make-et-matcher :generics generics
                                           :constraints constraints
                                           :dnf input-struct)))
            (put accessor-name 'et-function-type
                 (et-dt 'DynFunction matcher slot-struct))))))

    ;; Constructor: (&key SLOTS...) -> Struct<NAME G...>
    (when constructor
      (if (null generics)
          ;; No generics: plain Function
          (let* ((plist-args
                  (cl-loop for (path slot-name _default . type-info) in slots
                           for type-spec-entry = (car type-info)
                           do (setq et--preprocess-path path)
                           nconc (list (intern (format ":%s" slot-name))
                                       (if type-spec-entry
                                           (progn (setq et--preprocess-path (car type-spec-entry))
                                                  (et-parse-type (cdr type-spec-entry)))
                                         (et-any))))))
            (put constructor 'et-function-type
                 (et-dt 'Function
                        (if plist-args (apply #'et-dt 'PList plist-args) (et-literal nil))
                        struct-type)))
        ;; Has generics: DynFunction
        (let* ((plist-struct-args
                (cl-loop for (path slot-name _default . type-info) in slots
                         for type-spec-entry = (car type-info)
                         do (setq et--preprocess-path path)
                         nconc (list (intern (format ":%s" slot-name))
                                     (if type-spec-entry
                                         (progn (setq et--preprocess-path (car type-spec-entry))
                                                (et--parse-struct (cdr type-spec-entry) generics 'BOTH))
                                       (et-q (((S:DT Any))))))))
               (input-struct (if plist-struct-args
                                 (et-q (((S:DT PList ,@plist-struct-args))))
                               (et-q (((S:DT Literal nil))))))
               (output-struct
                (et-q (((S:DT Struct ,name
                              ,@(mapcar (lambda (g) (list (list (list 'S:GENERIC g))))
                                        generics))))))
               (matcher (make-et-matcher :generics generics
                                         :constraints constraints
                                         :dnf input-struct)))
          (put constructor 'et-function-type
               (et-dt 'DynFunction matcher output-struct)))))

    ;; Copier: (Struct<NAME G...>) -> Struct<NAME G...>
    (when copier
      (if (null generics)
          (put copier 'et-function-type
               (et-dt 'Function
                      (et-alias 'ConsR struct-type (et-literal nil))
                      struct-type))
        (let* ((struct-struct
                (et-q (((S:DT Struct ,name
                              ,@(mapcar (lambda (g) (list (list (list 'S:GENERIC g))))
                                        generics))))))
               (input-struct
                (et-q (((S:ALIAS ConsR ,struct-struct (((S:DT Literal nil))))))))
               (matcher (make-et-matcher :generics generics
                                         :constraints constraints
                                         :dnf input-struct)))
          (put copier 'et-function-type
               (et-dt 'DynFunction matcher struct-struct)))))))


;;;; Populate

(defun et--populate (pre-aliases pre-vars pre-defuns pre-structs)
  (pcase-let* ((errors nil)
               (defuns nil)
               (et--preprocess-path nil))

    ;; Parse alias spec->structure
    (cl-loop for (path var) in pre-aliases
             do (condition-case err (et--initialize-alias var)
                  (error (push (cons path (error-message-string err)) errors))))

    ;; Parse variable types
    (cl-loop for (path name spec) in pre-vars
             do (condition-case err (put name 'et-variable-type (et-parse-type spec))
                  (error (push (cons path (error-message-string err)) errors))))

    ;; Parse function signatures to types
    (dolist (args pre-defuns)
      (setq et--preprocess-path (car args))
      (condition-case err (push (apply #'et--populate-defun (cdr args)) defuns)
        (error (push (cons et--preprocess-path (error-message-string err)) errors))))

    ;; Generate function signatures for struct functions
    (dolist (info pre-structs)
      (setq et--preprocess-path (car info))
      (condition-case err (apply #'et--populate-defstruct (cdr info))
        (error (push (cons et--preprocess-path (error-message-string err)) errors))))

    (list errors (delq nil defuns))))


;;;; Process buffer

(defvar et--preprocessed-files nil
  "List of files that have been preprocessed.")

(defvar et--processing nil
  "Currently performing preprocessing.

@et-type Nil|@PREPROCESS|@POPULATE|@VALIDATE")

(defun et--process (exprs)
  (pcase-let* ((et--processing 'PREPROCESS)
               (`(,pre-errors . ,pre-rest) (et--preprocess exprs))

               (et--processing 'POPULATE)
               (`(,pop-errors ,pop-funcs) (apply #'et--populate pre-rest))

               (et--processing 'VALIDATE)
               (ret-errors nil)
               (check-diagnostics nil))

    ;; Validate the funcs
    (cl-loop for (orig-path source-path source scoped vars ret) in pop-funcs
             do (setq source (or source (list nil)))
             ;; Loop through each expression in the source
             for res-type =
             (cl-loop for expr in source
                      for expr-idx upfrom (car (last source-path))
                      for expr-path = (append (butlast source-path) (list expr-idx))
                      for result =
                      (et--with-scoped-datatypes scoped (et-with-vars vars (et--check expr)))
                      ;; Propogate diagnostics
                      do
                      (cl-loop for (path severity str) in (et-result-diagnostics result)
                               do (push (list (append expr-path path) severity str) check-diagnostics))
                      ;; Return the type of the final result
                      finally return (et-result-type result))
             ;; Check that the result value matches the declared return value
             unless (et-subtype? res-type ret)
             do (push (cons orig-path (format "Expected %s, got %s" (et-pp ret) (et-pp res-type)))
                      ret-errors))

    ;; Convert all diagnostics to the same format: (PATH SEVERITY MESSAGE)
    (nconc
     (cl-loop for (path . msg) in (append pre-errors pop-errors ret-errors)
              collect (list path 'error msg))
     check-diagnostics)))

(defun et--process-buffer ()
  (interactive)

  (save-excursion
    (goto-char (point-min))
    (et--process
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
      (cl-loop for (path severity msg) in (et--process-buffer)
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
        (et-checker-err "Not subtype: %s" (et-pp expr-type)))
    (setq et--checker-expr "dummy")
    (et Nil)))

(et-define-pcase-checker :assert-error `(,_expr)
  (condition-case _err (et-checker-sub 1)
    (error (setq et--checker-expr nil) (et-literal nil))
    (:success (et-checker-err "Didn't error"))))

(et-define-pcase-checker :typeof `(,_expr)
  (let ((type (et-checker-sub 1)))
    (et-checker-warn (et-pp (et--remove-type-binds type)))
    (setq et--checker-expr (cadr et--checker-expr))
    type))

(et-define-pcase-checker :typeof+ `(,_expr)
  (let ((type (et-checker-sub 1)))
    (et-checker-warn (et-pp type))
    (setq et--checker-expr (cadr et--checker-expr))
    type))

(et-define-pcase-checker :narrows `()
  (cl-loop for (var . type) in (reverse et--narrow-binds)
           collect (format "%s: %s" (et-var-name var) (et-pp type)) into strs
           finally do
           (et-checker-warn (string-join strs "\\n")))
  (setq et--checker-expr nil)
  (et Nil))

(et-define-pcase-checker :eval `(,expr)
  (et-checker-warn (cl-prin1-to-string (eval expr)))
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
