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

(defvar et-display-narrows t
  "Whether to display narrowed types on if/when/etc blocks.")

(defun et-checker-hint-narrows (path &rest types)
  "Display a list of binds to the user at path=(0).

TYPES is (FMT1 TYPE1 FMT2 TYPE2 ...)."
  (when et-display-narrows
    (cl-loop for (fmt type) on types by #'cddr
             for binds = (et--type-binds type) ; TODO: display just binds instead of whole type
             when binds do (et-checker-hint path fmt (et-pp-narrows binds)))))


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
                 (output-type (et--funcall func-type args-type)))
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

(defun et-show-result-errors (result)
  (cl-loop for (path _severity message) in (et-result-diagnostics result)
           do (et-error path message)))

(defmacro et-typecheck (body)
  (let* ((result (et--check body)))
    (et-with-error-path '(1) (et-show-result-errors result))
    (et--remove-type-binds (et-simplify-type (et-result-type result)))))

(defmacro et-typecheck-call (func &rest arg-types)
  (cl-loop for type in arg-types
           collect (list :type type) into arg-exprs
           finally return (list #'et-typecheck (cons func arg-exprs))))

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

  (let* ((generics (when (vectorp (car arguments)) (append (pop arguments) nil)))
         (arglist-spec (car arguments))
         (return-spec (cadr arguments))
         (matcher (et-parse-matcher arglist-spec generics))
         (output-struct (et-parse-structure return-spec generics))
         (func-type (et-dt 'DynFunction matcher output-struct)))
    (unless (eq (length arguments) 2)
      (error "Incorrect number of arguments"))

    (cl-loop for func in (if (symbolp funcs) (list funcs) funcs)
             collect `(setf (get ',func 'et-function-type) ,func-type) into exprs
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

(defmacro et-checker-infer (type gens matcher-spec output-spec)
  (cl-assert (vectorp gens))
  (setq gens (append gens nil))
  `(et--infer ,(et-parse-matcher matcher-spec gens)
              (et-parse-type ,type)
              (et-q ,(et-parse-structure output-spec gens))))


;;; ============================================================
;;; Function types
;;;; Struct

(cl-defstruct et-func-sig
  "Parsed function signature.

BODY is the function body with inline annotations stripped.
SCOPED is the list of scoped datatype entries.
VARS is a list of `et-var' for all parameters.
INPUT is a matcher (if generics) or type (if not) for the arglist.
EXPECTED-RETURN is the declared return type (with scoped datatypes), or nil."
  body scoped vars input expected-return)


;;;; Docstring parsing

(defun et--parse-docstring-annotations (docstring)
  "Extract type annotations from DOCSTRING.

Returns (GENERICS RETURN-SPEC . ARG-SPECS) where:
  GENERICS is a list of generic symbols, or nil.
  RETURN-SPEC is a type spec form, or nil.
  ARG-SPECS is an alist of (DOWNCASE-SYMBOL . SPEC).

Recognizes:
  `@et-generics' [T U]    — generic type variables
  `@et-return' TYPE-SPEC   — return type
  ARGNAME : TYPE-SPEC    — per-parameter type (ARGNAME uppercase)"

  (let* ((generics nil)
         (return-spec nil)
         (arg-specs nil)
         (case-fold-search nil))
    (when (stringp docstring)
      (when (string-match "@et-generics +\\(.*\\)" docstring)
        (let* ((expr (ignore-errors (car (read-from-string (match-string 1 docstring))))))
          (when (vectorp expr)
            (setq generics (append expr nil)))))

      (when (string-match "@et-return +\\(.*\\)" docstring)
        (setq return-spec
              (ignore-errors (car (read-from-string (match-string 1 docstring))))))

      (let* ((pos 0))
        (while (string-match
                "^\\([A-Z][-A-Z0-9_]*\\)\\s-+:\\s-+\\(\\S-.*\\)"
                docstring pos)
          (let* ((name (intern (downcase (match-string 1 docstring))))
                 (spec (ignore-errors (car (read-from-string (match-string 2 docstring))))))
            (when spec (push (cons name spec) arg-specs)))
          (setq pos (match-end 0)))))
    (cons generics (cons return-spec (nreverse arg-specs)))))


;;;; Arglist parsing

(defun et--parse-funcdef-params (arglist doc-arg-specs)
  "Parse ARGLIST into parameter alists, stripping inline type vectors.

DOC-ARG-SPECS is an alist of (SYMBOL . SPEC) from docstring parsing.

Destructively modifies ARGLIST to replace [name Type] vectors with bare
symbols.

Returns (REQUIRED OPTIONAL KEY REST) where REQUIRED, OPTIONAL, KEY are
alists of (SYMBOL . TYPE-SPEC-OR-NIL) and REST is a single cons or nil.

Signals an error on malformed parameters."
  (let* ((required nil)
         (optional nil)
         (key-params nil)
         (rest-param nil)
         (mode 'standard)
         (cell arglist))

    (while cell
      (pcase (car cell)
        ('&optional (setq mode 'optional))
        ('&rest     (setq mode 'rest))
        ('&key      (setq mode 'key))
        (elem
         (let* ((name nil)
                (spec nil))
           (pcase elem
             ((and (pred vectorp) (guard (>= (length elem) 2)))
              (setq name (aref elem 0)
                    spec (aref elem 1))
              (or (symbolp name) (error "Parameter name must be a symbol: %s" name))
              (setcar cell name))

             ((pred consp)
              (setq name (car elem))
              (or (symbolp name) (error "Parameter name must be a symbol: %s" name))
              (setq spec (alist-get name doc-arg-specs)))

             ((pred symbolp)
              (setq name elem
                    spec (alist-get elem doc-arg-specs)))

             (_ (error "Invalid parameter: %s" elem)))

           (pcase mode
             ('standard (push (cons name spec) required))
             ('optional (push (cons name spec) optional))
             ('key      (push (cons name spec) key-params))
             ('rest
              (when rest-param (error "Multiple &rest parameters"))
              (setq rest-param (cons name spec)))))))
      (setq cell (cdr cell)))

    (list (nreverse required)
          (nreverse optional)
          (nreverse key-params)
          rest-param)))


;;;; Make input

(defun et-func-sig-to-input (generics required optional key-params rest-param)
  "Build an input matcher or type from parameter specs.

GENERICS is a list of generic symbols, or nil.
REQUIRED, OPTIONAL, KEY-PARAMS are alists of (SYMBOL . TYPE-SPEC-OR-NIL).
REST-PARAM is either nil or a single (SYMBOL . TYPE-SPEC-OR-NIL) cons.

Returns an `et-matcher' if GENERICS is non-nil, or an `et-type' if not."
  (let* ((parse (lambda (spec) (et-parse-structure (or spec 'Any) generics)))
         (required-structs (mapcar (lambda (p) (funcall parse (cdr p))) required))
         (optional-structs (mapcar (lambda (p) (funcall parse (cdr p))) optional))
         (tail (pcase rest-param
                 (`(,_ . ,spec) (funcall parse (or spec 'ListR<Any>)))
                 ((and (guard key-params)
                       (let plist-args
                         (cl-loop for (name . spec) in key-params
                                  nconc (list (intern (format ":%s" name))
                                              (funcall parse spec)))))
                  (et-q (((S:DT PList ,@plist-args)))))))
         (opt-tail (et--func-sig-optional-tail optional-structs tail))
         (struct (et--func-sig-required-chain required-structs opt-tail)))

    (if generics
        (make-et-matcher
         :generics generics
         :dnf (et-structure-to-matcher-dnf struct generics))
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
 (equal (et-func-sig-to-input nil '((x . Integer) (y . nil)) nil nil nil)
        (et ConsR<Integer~ConsR<Any~Nil>>))

 (equal (et-func-sig-to-input nil '((x . Integer) (y . String)) nil nil '(args . nil))
        (et ConsR<Integer~ConsR<String~ListR<Any>>>))

 (equal (et-func-sig-to-input '(T) '((x . T)) '((y . Number)) nil '(args . ListR<String>))
        (et-matcher [T]
          ConsR<T~{Nil|ConsR<Number~ListR<String>>}>))

 (equal (et-func-sig-to-input '(T) '((a . T)) nil '((scale . Number) (flag . nil)) nil)
        (et-matcher [T]
          ConsR<T~PList<:scale~Number~:flag~Any>>)))


;;;; Main entry point

(defun et-parse-function-type (body)
  "Parse function signature from BODY, returning an `et-func-sig'.

BODY is the forms after the function name in a defun: an optional
generics vector, the arglist, an optional `-> RETURN-SPEC', an optional
docstring, then body forms.

Signals an error if anything is malformed."
  (let* ((forms (copy-tree body))
         (generics nil)
         (return-spec nil))

    ;; Consume leading generics vector
    (when (vectorp (car forms))
      (setq generics (append (pop forms) nil)))

    (or (listp (car forms))
        (error "Expected argument list, got %s" (type-of (car forms))))
    (let* ((arglist (pop forms)))

      ;; Consume inline return type
      (when (eq (car forms) '->)
        (pop forms)
        (or forms (error "Missing type after `->'"))
        (setq return-spec (pop forms)))

      ;; Extract docstring only when followed by more body forms
      (let* ((docstring (when (and (stringp (car forms)) (cdr forms))
                          (car forms)))
             (doc-info (et--parse-docstring-annotations docstring))
             (doc-generics (car doc-info))
             (doc-return (cadr doc-info))
             (doc-arg-specs (cddr doc-info)))

        ;; Inline annotations take precedence
        (or generics (setq generics doc-generics))
        (or return-spec (setq return-spec doc-return))

        (pcase-let* ((`(,required ,optional ,key-params ,rest-param)
                      (et--parse-funcdef-params arglist doc-arg-specs))
                     (all-params (append required optional key-params
                                         (when rest-param
                                           (list (cons (car rest-param)
                                                       (or (cdr rest-param) 'ListR<Any>))))))
                     ;; Build input using generics (external view)
                     (input (et-func-sig-to-input generics required optional key-params rest-param))
                     ;; Scoped datatypes for body-internal use
                     (scoped (et--make-scoped-datatypes
                              (when (et-matcher-p input) input)))
                     (vars nil)
                     (expected-return nil))

          ;; Parse parameter types and return type with scoped datatypes bound
          (et--with-scoped-datatypes scoped
            (setq vars
                  (cl-loop for (name . spec) in all-params
                           collect (make-et-var :name name
                                                :type (et-parse-type (or spec 'Any)))))
            (when return-spec
              (setq expected-return (et-parse-type return-spec))))

          (make-et-func-sig
           :body (cons arglist forms)
           :scoped scoped
           :vars vars
           :input input
           :expected-return expected-return))))))


;;;; Tests

(defun et--func-sig-test-equal (a b)
  "Test equality of two `et-func-sig' structs, tolerating scoped gensyms."
  (and (equal (et-func-sig-body a) (et-func-sig-body b))
       (equal (mapcar #'car (et-func-sig-scoped a))
              (mapcar #'car (et-func-sig-scoped b)))
       (equal (mapcar #'et-var-name (et-func-sig-vars a))
              (mapcar #'et-var-name (et-func-sig-vars b)))
       (equal (et-func-sig-input a) (et-func-sig-input b))
       (eq (not (not (et-func-sig-expected-return a)))
           (not (not (et-func-sig-expected-return b))))))

(et-test
 ;; 1. Inline generics + inline typed args + arrow return + multi-form body
 (et--func-sig-test-equal
  (et-parse-function-type '([T U] ([x T] [y U]) -> ConsR<T~U> (message "hi") (cons x y)))
  (make-et-func-sig
   :body '((x y) (message "hi") (cons x y))
   :scoped '((T) (U))
   :vars (list (make-et-var :name 'x) (make-et-var :name 'y))
   :input (et-matcher [T U] ConsR<T~ConsR<U~Nil>>)
   :expected-return t))

 ;; 2. No generics + docstring annotations + &optional + &rest
 (et--func-sig-test-equal
  (et-parse-function-type
   '((a &optional b (c 10) &rest args)
     "A : Integer\nB : String\nC : Number\nARGS : ListR<Symbol>\n@et-return Boolean"
     (or b a)))
  (make-et-func-sig
   :body '((a &optional b (c 10) &rest args)
           "A : Integer\nB : String\nC : Number\nARGS : ListR<Symbol>\n@et-return Boolean"
           (or b a))
   :scoped nil
   :vars (list (make-et-var :name 'a) (make-et-var :name 'b)
               (make-et-var :name 'c) (make-et-var :name 'args))
   :input (et ConsR<Integer~Nil|ConsR<String~Nil|ConsR<Number~ListR<Symbol>>>>)
   :expected-return t))

 ;; 3. Docstring generics + &key + no inline annotations
 (et--func-sig-test-equal
  (et-parse-function-type
   '((x &key scale flag)
     "@et-generics [T]\nX : T\nSCALE : Number\n@et-return VectorR<T>"
     (vector x)))
  (make-et-func-sig
   :body '((x &key scale flag)
           "@et-generics [T]\nX : T\nSCALE : Number\n@et-return VectorR<T>"
           (vector x))
   :scoped '((T))
   :vars (list (make-et-var :name 'x) (make-et-var :name 'scale)
               (make-et-var :name 'flag))
   :input (et-matcher [T] ConsR<T~PList<:scale~Number~:flag~Any>>)
   :expected-return t))

 ;; 4. Inline overrides docstring + mixed inline/bare required
 (et--func-sig-test-equal
  (et-parse-function-type
   '([U] ([a U] b) -> ListR<U>
     "@et-generics [T]\nA : T\nB : String\n@et-return VectorR<T>"
     (list a b)))
  (make-et-func-sig
   :body '((a b)
           "@et-generics [T]\nA : T\nB : String\n@et-return VectorR<T>"
           (list a b))
   :scoped '((U))
   :vars (list (make-et-var :name 'a) (make-et-var :name 'b))
   :input (et-matcher [U] ConsR<U~ConsR<String~Nil>>)
   :expected-return t))

 ;; 5. No return annotation
 (et--func-sig-test-equal
  (et-parse-function-type '((x) x))
  (make-et-func-sig
   :body '((x) x)
   :scoped nil
   :vars (list (make-et-var :name 'x))
   :input (et ConsR<Any~Nil>)
   :expected-return nil))

 ;; 6. Empty arglist + no return
 (et--func-sig-test-equal
  (et-parse-function-type '(() 1))
  (make-et-func-sig
   :body '(() 1)
   :scoped nil
   :vars nil
   :input (et Nil)
   :expected-return nil)))


;;;; Make func type

(defun et--make-func-type (input return-type scoped)
  "Build a Function or DynFunction type from INPUT and RETURN-TYPE.

INPUT is a matcher (if generics exist) or a type (if not).
RETURN-TYPE is the resolved return type, possibly containing scoped datatypes.
SCOPED is the list of scoped datatype entries from `et--make-scoped-datatypes'."
  (if (et-matcher-p input)
      (let* ((output-struct (et-type-to-structure return-type)))
        (dolist (s scoped)
          (setq output-struct (cl-subst `(S:GENERIC ,(car s))
                                        `(S:DT Scoped . ,s)
                                        output-struct
                                        :test #'equal)))
        (et-dt 'DynFunction input output-struct))
    (et-dt 'Function input return-type)))


;;;; Preprocessing

(defun et--read-all (string)
  "Read all s-expressions from STRING, returning a list."
  (let* ((pos 0) forms)
    (condition-case nil
        (while t
          (let* ((result (read-from-string string pos)))
            (push (car result) forms)
            (setq pos (cdr result))))
      (end-of-file (nreverse forms)))))

(defvar et--preprocessed-files nil)

(defun et-preprocess-file (file)
  "Preprocess FILE by extracting defun signatures and registering function types."
  (unless (member file et--preprocessed-files)
    (push file et--preprocessed-files)
    (dolist (form (et--read-all (with-temp-buffer
                                  (insert-file-contents file)
                                  (buffer-string))))
      (pcase form
        (`(,(or 'defun 'cl-defun) ,(and name (pred symbolp)) . ,body)
         (when-let* ((sig (ignore-errors (et-parse-function-type body)))
                     ((et-func-sig-expected-return sig)))
           (put name 'et-function-signature sig)
           (put name 'et-function-type
                (et--make-func-type (et-func-sig-input sig)
                                    (et-func-sig-expected-return sig)
                                    (et-func-sig-scoped sig)))))))))


;;;; Checker function body

(defun et-checker-function-body (sig offset)
  "Typecheck a function signature.

OFFSET is the position of the start of the body (the arglist or
generics)."

  (let* ((orig-body (nthcdr offset et--checker-expr))
         (stripped-body (et-func-sig-body sig))
         ;; Decorators include the generic vector, and "-> TYPE"
         (decorator-count (- (length orig-body) (length stripped-body)))
         ;; The start of the inside of the function
         (inside-offset (+ offset 1 decorator-count)))

    ;; Typecheck the body with scoped datatypes and parameter vars bound
    (prog1
        (et--with-scoped-datatypes (et-func-sig-scoped sig)
          (et-with-vars (et-func-sig-vars sig)
            (let* ((expected (et-func-sig-expected-return sig))
                   (actual (et-checker-tail inside-offset)))

              (when (and expected (not (et-subtype? actual expected)))
                (et-checker-err 0 "Expected %s, got %s" (et-pp expected) (et-pp actual)))
              (et--make-func-type (et-func-sig-input sig)
                                  (if expected expected (et--remove-type-binds actual))
                                  (et-func-sig-scoped sig)))))

      ;; Remove inline type annotations and generics vector
      (setf (nthcdr offset et--checker-expr)
            (cons (car stripped-body)
                  (nthcdr inside-offset et--checker-expr))))))


;;; ============================================================
;;; Utils
;;;; Testing checkers

(et-define-pcase-checker :type `(,spec)
  (setq et--checker-expr "dummy") (et-parse-type spec))

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
