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

(defun et-var-type (variable)
  (or (alist-get variable et--narrow-binds)
      (et-var-type variable)))

(defun et-get-symbol-type (sym)
  (cl-assert (symbolp sym))
  (when-let ((var (et-get-symbol-var sym)))
    (et-var-type var)))

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

(defun et-checker-hint-narrows (&rest types)
  "Display a list of binds to the user at path=(0).

TYPES is (FMT1 TYPE1 FMT2 TYPE2 ...)."
  (when et-display-narrows
    (cl-loop for (fmt type) on types by #'cddr
             for binds = (et--type-binds type) ; TODO: display just binds instead of whole type
             when binds do (et-checker-hint fmt (et-pp-narrows binds)))))


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
checked."
  type diagnostics compiled)


;;;; Check

(defvar et--checker-diagnostics nil
  "Diagnostics signalled on the current call to this checker.")

(defvar et--checker-expr nil
  "The current expr.")

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
              (et-checker-err "Function %s not defined on %s" func (et-pp args-type)))))

         (_ (et-checker-err '(0) "No type for `%s'" func))))

      ;; Type check a variable
      ((and sym (pred symbolp) (guard sym) (guard (not (eq sym t))))

       (if-let* ((var (et-get-symbol-var sym)))
           (setq return-type
                 (et--supersect
                  (et-var-type var)
                  (et-type (make-et-type-case :value (make-et-datatype :name 'Any)
                                              :typeofs (list var)))))

         (et-checker-err "Free variable: %s" var)))

      (expr (setq return-type (et-literal expr))))

    (cl-assert (or et--checker-diagnostics return-type))
    (make-et-result :type (or return-type (et-never))
                    :diagnostics (nreverse et--checker-diagnostics)
                    :compiled et--checker-expr)))

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

(defmacro et--define-diagnostics-function (name severity)
  `(defun ,name (&rest args)
     ,(format "Create a checker diagnostic with severity `%s'.\n\n%s" severity
              "(fn [PATH] FORMAT-STRING ARGS...)")
     (apply #'et--checker-diagnostic ',severity args)
     nil))

(et--define-diagnostics-function et-checker-err error)
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
    (et-result-type result)))

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

  (let* ((generics
          (when (vectorp (car arguments))
            (cl-loop for var across (car arguments)
                     do (or (symbolp var) (error "Generic vars must be symbols"))
                     do (or (let ((case-fold-search nil))
                              (string-match-p "^[A-Z]" (format "%s" var)))
                            (error "Generic vars must start with an uppercase letter")))
            (append (pop arguments) nil)))
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


;;;; Function parsing
;;;;; Arglist structure generation

(defun et--generate-arglist-structure (std-structs opt-structs rest-or-keys)
  "Build a structure representing a function's argument list.

STD-STRUCTS is a list of structures for required parameters.
OPT-STRUCTS is a list of structures for &optional parameters.
REST-OR-KEYS is one of:
  nil              — no rest or key parameters
  (:rest . STRUCT) — a structure for the &rest type (already ListR-wrapped)
  ALIST            — an alist of (NAME . STRUCTURE) for &key

The resulting structure is a nested ConsR chain where:
  - Required args are unconditional ConsR links.
  - Optional args each introduce (or Nil ConsR<type~...>).
  - &rest appends the rest structure as the tail.
  - &key generates a PList as the tail."
  (et--generate-arglist-required-structure
   std-structs
   (et--generate-arglist-tail-structure opt-structs rest-or-keys)))

(defun et--generate-arglist-required-structure (structs tail)
  "Chain required STRUCTS into nested ConsR structure, terminated by TAIL."
  (if (null structs) tail
    (et-q (((S:ALIAS ConsR
                     ,(car structs)
                     ,(et--generate-arglist-required-structure (cdr structs) tail)))))))

(defun et--generate-arglist-tail-structure (opt-structs rest-or-keys)
  "Build the optional/rest/key tail of an arglist structure."
  (cond
   ((and (null opt-structs) (null rest-or-keys))
    (et-q (((S:DT Literal nil)))))
   ((and (null opt-structs) (eq (car-safe rest-or-keys) :rest))
    (cdr rest-or-keys))
   ((and (null opt-structs) (listp rest-or-keys))
    (et--generate-keys-plist-structure rest-or-keys))
   (t
    (let* ((inner (et--generate-arglist-tail-structure (cdr opt-structs) rest-or-keys)))
      (et-q (((S:DT Literal nil))
             ((S:ALIAS ConsR ,(car opt-structs) ,inner))))))))

(defun et--generate-keys-plist-structure (keys-alist)
  "Build a PList structure from KEYS-ALIST, an alist of (NAME . STRUCTURE)."
  (cl-loop for (name . struct) in keys-alist
           nconc (list (intern (format ":%s" name)) struct) into plist-args
           finally return (et-q (((S:DT PList ,@plist-args))))))


;;;;; Docstring type parsing

(defun et--parse-docstring-types (docstring)
  "Parse type annotations from DOCSTRING.

Returns (GENERICS RETURN-SPEC . ARG-SPECS) where GENERICS is a list of
generic symbols (or nil), RETURN-SPEC is a type spec for the return type
\(or nil), and ARG-SPECS is an alist of (DOWNCASE-SYMBOL . SPEC).

Recognizes:
  @et-generics [T U] — declares generic type variables
  @et-return TYPE-SPEC — declares the return type
  ARGNAME : TYPE-SPEC — assigns TYPE-SPEC to the lowercased ARGNAME"
  (let* ((generics nil)
         (return-spec nil)
         (arg-specs nil))
    (when (and docstring (stringp docstring))
      ;; Parse @et-generics VECTOR
      (when (string-match "@et-generics\\s-+\\(\\[.*?\\]\\)" docstring)
        (let* ((gen-expr (car (read-from-string (match-string 1 docstring)))))
          (when (vectorp gen-expr)
            (setq generics (append gen-expr nil)))))

      ;; Parse @et-return TYPE-SPEC
      (when (string-match "@et-return\\s-+\\(\\S-.*\\)" docstring)
        (let* ((type-str (match-string 1 docstring))
               (type-expr (ignore-errors (car (read-from-string type-str)))))
          (when type-expr
            (setq return-spec type-expr))))

      ;; Parse ARGNAME : TYPE-SPEC
      (let ((pos 0))
        (while (string-match
                "^\\([A-Z][-A-Z0-9_]*\\)\\s-+:\\s-+\\(\\S-.*\\)"
                docstring pos)
          (let* ((name (intern (downcase (match-string 1 docstring))))
                 (type-str (match-string 2 docstring))
                 (type-expr (ignore-errors (car (read-from-string type-str)))))
            (when type-expr
              (push (cons name type-expr) arg-specs)))
          (setq pos (match-end 0)))))
    (cons generics (cons return-spec (nreverse arg-specs)))))


;;;;; Arglist parsing

(defun et--parse-funcdef-arglist (arglist doc-arg-specs generics arglist-pos)
  "Parse ARGLIST into structure-level parameter components.

DOC-ARG-SPECS is an alist of (NAME . SPEC) from docstring parsing.
GENERICS is the list of generic type variable symbols (or nil).
ARGLIST-POS is for error reporting.

Returns (STD-STRUCTS OPT-STRUCTS REST-OR-KEYS ALL-VARS) where:
  STD-STRUCTS is a list of structures for required params.
  OPT-STRUCTS is a list of structures for optional params.
  REST-OR-KEYS is nil, (:rest . STRUCTURE) for a rest param, or a
    key alist of (NAME . STRUCTURE).
  ALL-VARS is a list of `et-var' (with types for body checking)."
  (let* ((std-structs nil) (opt-structs nil)
         (key-entries nil) (rest-entry nil)
         (all-vars nil)
         (mode 'standard))

    (dolist (elem arglist)
      (pcase elem
        ('&optional (setq mode 'optional))
        ('&rest     (setq mode 'rest))
        ('&key      (setq mode 'key))
        (_
         (let* ((inline-p (and (vectorp elem) (>= (length elem) 2)))
                (name (pcase elem
                        ((guard inline-p) (aref elem 0))
                        ((pred consp)     (car elem))
                        ((pred symbolp)   elem)
                        (_                (et-checker-err arglist-pos
                                                          "Invalid parameter: %s" elem)
                                          nil)))
                (spec (pcase elem
                        ((guard inline-p) (aref elem 1))
                        (_ (alist-get name doc-arg-specs))))
                (struct (condition-case err
                            (et-parse-structure (or spec 'Any) generics)
                          (error
                           (et-checker-err arglist-pos
                                           "Invalid type for `%s': %s"
                                           name (error-message-string err))
                           (et-parse-structure 'Any nil))))
                (type (condition-case nil
                          (et-structure-to-type struct)
                        (error (et-any)))))

           (when name
             (push (et-new-var name type) all-vars)
             (pcase mode
               ('standard (push struct std-structs))
               ('optional (push struct opt-structs))
               ('rest     (setq rest-entry
                                (cons :rest
                                      (et-parse-structure
                                       (et-q (ListR (:structure ,struct))) generics))))
               ('key      (push (cons name struct) key-entries))))))))

    (list (nreverse std-structs)
          (nreverse opt-structs)
          (or rest-entry (when key-entries (nreverse key-entries)))
          (nreverse all-vars))))


;;;;; Function definition checker

(defun et-checker-funcdef (start-path)
  "Type-check a function definition at START-PATH in `et--checker-expr'.

START-PATH points to the first element after the keyword (e.g. 1 for
lambda, 2 for defun). That position may hold a generics vector
\(followed by the arglist) or the arglist directly.

A return type can be declared either inline with -> after the arglist:
  (lambda ([x Integer]) -> Integer x)
or via @et-return in the docstring. Inline takes precedence.

When a return type is declared, the body's inferred type is checked
against it and the declared type is used.

Returns a Function type when no generics are present, or a DynFunction
type when generics are present."
  (let* ((elems (nthcdr start-path et--checker-expr))
         (generics nil)
         (arglist-pos start-path)
         (return-spec nil)
         (return-struct nil))

    ;; Detect and remove inline generics vector
    (when (vectorp (car elems))
      (setq generics (append (car elems) nil))
      (setcdr (nthcdr (1- start-path) et--checker-expr) (cddr elems))
      (setq elems (nthcdr start-path et--checker-expr)))

    (unless (listp (car elems))
      (et-checker-err arglist-pos "Expected argument list, got %s" (type-of (car elems)))
      (signal 'et-checker-fatal nil))

    ;; Detect and remove inline return type: -> SPEC after arglist
    (when (eq (cadr elems) '->)
      (setq return-spec (caddr elems))
      (setcdr elems (cdddr elems)))

    ;; Parse docstring (must come after arglist, and body must follow)
    (let* ((after-arglist (cdr elems))
           (docstring (when (and (stringp (car after-arglist))
                                 (cdr after-arglist))
                        (car after-arglist)))
           (doc-info (et--parse-docstring-types docstring))
           (doc-generics (car doc-info))
           (doc-return-spec (cadr doc-info))
           (doc-arg-specs (cddr doc-info)))

      ;; Inline -> takes precedence over docstring @et-return
      (setq return-spec (or return-spec doc-return-spec))
      ;; Inline generics vector takes precedence over docstring
      (unless generics (setq generics doc-generics))

      (pcase-let* ((`(,std-structs ,opt-structs ,rest-or-keys ,all-vars)
                    (et--parse-funcdef-arglist (car elems) doc-arg-specs generics arglist-pos))
                   (arglist-struct
                    (et--generate-arglist-structure std-structs opt-structs rest-or-keys))
                   (body-start (+ arglist-pos (if docstring 2 1))))

        ;; Parse the return type spec into a structure
        (when return-spec
          (setq return-struct
                (condition-case err
                    (et-parse-structure return-spec generics)
                  (error
                   (et-checker-err arglist-pos "Invalid return type: %s"
                                   (error-message-string err))
                   nil))))

        ;; Strip inline annotations from compiled arglist
        (et--strip-inline-types (car elems))

        ;; Type-check the body and validate against declared return type
        (let* ((body-type (et-with-vars all-vars (et-checker-tail body-start)))
               (return-type
                (pcase return-struct
                  ('nil body-type)
                  (_ (let* ((declared-type
                             (condition-case nil (et-structure-to-type return-struct)
                               (error (et-any)))))
                       (unless (et-subtype? body-type declared-type)
                         (et-checker-err "Body type %s does not satisfy declared return type %s"
                                         (et-pp body-type) (et-pp declared-type)))
                       declared-type)))))
          (if generics
              (et-dt 'DynFunction
                     (make-et-matcher
                      :generics generics
                      :dnf (et-structure-to-matcher-dnf arglist-struct generics))
                     (or return-struct (et-type-to-structure return-type)))
            (et-dt 'Function
                   (et-structure-to-type arglist-struct)
                   return-type)))))))

(defun et--strip-inline-types (arglist)
  "Destructively replace [name Type] vectors in ARGLIST with bare names."
  (let ((cell arglist))
    (while cell
      (when (and (vectorp (car cell)) (>= (length (car cell)) 2))
        (setcar cell (aref (car cell) 0)))
      (setq cell (cdr cell)))))


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
