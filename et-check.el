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
  (or (alist-get sym et--binds)
      (get sym 'et-variable-var)))

(defmacro et-with-vars (vars &rest body)
  "VARS can contain nil values."
  (declare (indent 1))
  `(let* ((vs (cl-loop for var in ,vars
                       when var do (cl-assert (et-var-p var))
                       and collect (cons (et-var-name var) var)))
          (et--binds (append vs et--binds)))
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
             for binds = (et:algebra-type-binds type) ; TODO: display just binds instead of whole type
             when binds do (et-hint path fmt (et-pp-narrows binds)))))


;;; ============================================================
;;; Checking
;;;; The checking context

(et-declare
 (@alias EtNarrows AList<*et-var~*et-type>))

;; The checking context provides 3 things: an expr, a recommendation,
;; and an alist of narrows.

(defvar et--checker-expr nil)

(defun et-function-type (symbol)
  (when (get symbol 'et-deferred-declare)
    (ignore (et-result-boundary (funcall (get symbol 'et-deferred-declare))))
    (put symbol 'et-deferred-declare nil))

  (get symbol 'et-function-type))

(et-defvar et--checker-recommendation Nil|*et-type nil
  "A hint to the current checker of what type its expression should be.

This should ONLY be used in the checker for `lambda', in order to guess
at the function type. It should not be used in any other checkers.

This variable should only be modified by `et--check'.")

(et-defvar et--checker-narrows EtNarrows nil
  "A list of narrows in the current `et--check' environment.

This should ONLY be modified by `et--check', the existing sub-checker
functions, and the existing narrows modification functions. It should
only be read by functions in this file.")

(defun et-kill-all-narrows ()
  (setq et--checker-narrows nil))

(defun et-current-var-type (var)
  "Get the current narrowed type of a variable."
  (et-declare (var *et-var) (@return *et-type))
  (or (alist-get var et--checker-narrows)
      (et-var-type var)))

(defun et-get-symbol-type (sym)
  (et-declare (sym Var) (@return Nil|*et-type))
  (cl-assert (symbolp sym))
  (when-let* ((var (et-get-symbol-var sym)))
    (et-current-var-type var)))


;;;; Check

(cl-defstruct et--check-result
  (type nil :et *et-type)
  (narrows nil :et AList<*et-var~*et-type>))

(defun et--var-in-scope? (var)
  (or (eq var (get (et-var-name var) 'et-variable-var))
      (rassq var et--binds)))

(defun et--check (expr narrows recommendation)
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
  (declare (et (expr Sexp) (narrows EtNarrows) (recommendation Nil|*et-type)
               (@return *et--check-result)))

  (let* ((et--checker-expr expr)
         (et--checker-narrows narrows)
         (et--checker-recommendation recommendation))
    (make-et--check-result
     :type
     (et-failed-boundary
      (or (et-error-boundary nil
            (or (et--check-1)
                (unless et--result-failed (et-err nil "Type checking failed mysteriously"))))
          (et-never)))
     ;; Remove any narrows for variables no longer in scope
     :narrows (seq-filter (lambda (narrow) (et--var-in-scope? (car narrow)))
                          et--checker-narrows))))

(defun et--check-1 ()
  (declare (et (@return *et-type)))

  (pcase et--checker-expr
    (`(,func . ,_args)
     ;; If this function is lazily declared, and hasn't been declared yet, do it now
     (when (get func 'et-deferred-declare)
       (ignore (et-result-boundary (funcall (get func 'et-deferred-declare))))
       (put func 'et-deferred-declare nil))

     (pcase nil
       ;; Custom checker
       ((and (let checker (get func 'et-checker)) (guard checker)
             (let output (funcall checker)))
        (if (or (null output) (et-type-p output)) output
          (et-fatal nil "Checker for `%s' had invalid return: %s" func output)))

       ;; Function type property
       ((and (let func-type (et-function-type func)) (guard func-type))
        (let* ((arg-types (cl-loop for type in (et-checker-remaining 1) for pos upfrom 1
                                   collect (et-copy-with type :label (list :position pos))))
               (args-type (et-tuple 'Cons arg-types))
               (output-result (et-funcall func-type args-type)))
          (cond
           ((et-match-result-success output-result) (et-match-result-value output-result))
           ;; If `et--result-failed' is already true, that means one of the arguments was invalid,
           ;; which means the true error was in the arguments, not this call
           ((et-result-failed?) nil)
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
               (pcase (et-type-single func-type)
                 ((or (cl-struct et-datatype (name 'Function) (args `(,input ,_)))
                      (and (cl-struct et-datatype (name 'DynFunction) (args `(,matcher ,_)))
                           (let input (et-repr-to-string (et-matcher-repr matcher)))))
                  (et-err (or arg-pos 0) "Expected %s, got %s" input args-type))
                 (_ (et-err (or arg-pos 0) "`%s' has type %s\\nInvalid arguments: %s" func func-type args-type)))))))))

       (_ (et-err 0 "No type for `%s'" func))))

    ;; Type check a variable (a symbol which is neither a keyword, nil, or t)
    ((and sym (pred symbolp) (pred (not keywordp)) (guard sym) (guard (not (eq sym t))))
     (pcase nil
       ;; Check if there is a variable (local OR global scoped) associated with it
       ((and (let var (et-get-symbol-var sym)) (guard var))
        (et-add-typeof (et-current-var-type var) var))

       (_ (et-err nil "Free variable: %s" sym))))

    (expr (et-literal expr))))


;;;; Sub-checkers
;;;;; Series sub checkers

(defun et--traverse-tree (tree path)
  (if (null path) tree
    (when (>= (car path) (length tree))
      (error "Index out of bounds: %s %s" (car path) tree))
    (et--traverse-tree (nth (car path) tree) (cdr path))))

(cl-defun et-checker-sub (path &key recommendation)
  "Type check the sub expression at PATH, returning the type or never.

This assumes that the child will ALWAYS run, and thus that the resulting
narrows should ALWAYS be applied."
  (et-declare (path TreeR<Integer>) (recommendation Nil|*et-type)
              (@return *et-type))

  (let* ((flat (flatten-tree path))
         (expr (et--traverse-tree et--checker-expr flat))
         (checked (et-at flat (et--check expr et--checker-narrows recommendation))))
    (setq et--checker-narrows (et--check-result-narrows checked))
    (et--check-result-type checked)))

(defun et-checker-remaining (&rest first-path)
  "Check a sequence of expressions, returning all of their types.

This is useful for functions like `list'. If you only need the last
type, use `et-checker-tail'."
  (et-declare (first-path TreeR<Integer>) (@return List<*et-type>))

  (cl-assert et--checker-expr)
  (cl-assert first-path)
  (setq first-path (flatten-tree first-path))

  (let* ((parent-path (butlast first-path))
         (parent-expr (et--traverse-tree et--checker-expr parent-path))
         (start (car (last first-path))))

    (cl-loop for idx upfrom start below (length parent-expr)
             collect (et-checker-sub (append parent-path (list idx))))))

(defun et-checker-tail (&rest first-path)
  "Check a sequence of expressions, returning the type of the last one."
  (et-declare (first-path TreeR<Integer>) (@return *et-type))
  (or (car (last (et-checker-remaining first-path))) (et Nil)))


;;;;; Parallel sub checker

(defun et--narrows-or (a b)
  (et-declare (a EtNarrows) (b EtNarrows) (@return EtNarrows))
  (cl-loop for (var . t1) in a
           for t2 = (alist-get var b)
           when t2 collect (cons var (et:algebra-simplify-type (et-union t1 t2)))))

(defun et-checker-branches (&rest branches)
  "Type check parallel code paths, one of which must execute.

In order to ensure the correctness of this function, on every execution
of the parent function, one or more of the branches must execute. For
example, for a non-exaustive pcase, a case MUST be added which just
returns nil. This is to ensure the correctness of the resulting type AND
the resulting `et--checker-narrows'."
  (et-declare (branches (ListR fn<Nil~*et-type>)) (@return *et-type))

  (let* ((all-types (et: List<*et-type> nil))
         (all-narrows (et: List<EtNarrows> nil)))

    (dolist (fn branches)
      ;; Temporarily bind et--checker-narrows to itself so that modifications
      ;; are scoped to this block
      (let* ((et--checker-narrows et--checker-narrows)
             (type (funcall fn)))
        ;; If the case returns never, assume that it will never exit
        (unless (et-never-p type)
          (push type all-types)
          (push et--checker-narrows all-narrows))))

    (setq et--checker-narrows
          (when all-narrows (cl-reduce #'et--narrows-or all-narrows)))
    (et:algebra-simplify-type (apply #'et-union all-types))))


;;;;; Condition sub checker

(defun et--narrows-and (a b)
  (et-declare (a EtNarrows) (b EtNarrows) (@return EtNarrows))
  (cl-loop for var in (seq-uniq (mapcar #'car (append a b)) #'eq)
           for t1 = (alist-get var a) for t2 = (alist-get var b)
           collect (cons var (if t1 (if t2 (et-supersect t1 t2) t1) t2))))

(defun et-checker-sub-cond (cond-type then else)
  (et-declare (cond-type *et-type) (then fn) (else fn)
              (@return *et-type))
  (et-checker-branches
   (lambda ()
     (cl-callf et--narrows-and et--checker-narrows
       (et:algebra-type-binds (et-non-nil-of cond-type)))
     (funcall then))
   (lambda ()
     (cl-callf et--narrows-and et--checker-narrows
       (et:algebra-type-binds (et-nil-of cond-type)))
     (funcall else))))


;;;;; Loop sub checker

(defun et--narrows-changed-vars (before after)
  "Vars whose narrow entry differs between BEFORE and AFTER."
  (cl-loop for var in (seq-uniq (mapcar #'car (append before after)) #'eq)
           unless (equal (alist-get var before) (alist-get var after))
           collect var))

(defun et-checker-loop-body (body-fn)
  "Check BODY-FN, a block that may execute zero or more times.

Pass 1 (diagnostics discarded via `et-result-boundary') discovers the
vars the body can change. Pass 2 checks the body for real, with those
vars' narrows dropped, so iteration N never trusts a narrow iteration
N-1 may have broken. Afterward `et--checker-narrows' holds the join of
the zero-iteration path and the after-an-iteration path.

Returns the body's type (from the real pass)."
  (let* ((entry et--checker-narrows)
         (dirty (let* ((et--checker-narrows entry))
                  (ignore (et-result-boundary (funcall body-fn)))
                  (et--narrows-changed-vars entry et--checker-narrows))))
    (setq et--checker-narrows
          (cl-remove-if (lambda (n) (memq (car n) dirty)) entry))
    (prog1 (funcall body-fn)
      ;; Exit: 0 iterations (entry) OR >=1 iterations (current narrows).
      (setq et--checker-narrows (et--narrows-or entry et--checker-narrows)))))


;;;;; Escapable sub checker

(defun et-checker-escapable (body-fn)
  "Check BODY-FN, a block that may be exited nonlocally at any point.

Code after the enclosing form (e.g. `catch', `with-local-quit') runs
whether BODY-FN completed or aborted partway through, so the only narrows
that survive are the entry narrows on vars the body never touches: a
narrow the body created (or an entry narrow on a var the body assigns)
cannot be trusted, since the exit may have happened before or after the
change.

Returns the body's type."
  (let* ((entry et--checker-narrows)
         (type (funcall body-fn))
         (dirty (et--narrows-changed-vars entry et--checker-narrows)))
    (setq et--checker-narrows
          (cl-remove-if (lambda (n) (memq (car n) dirty)) entry))
    type))


;;;;; Deferred checking

(defmacro et-checker-deferred (&rest body)
  "Check BODY as code that runs at some later, unknown time.

It sees none of the current flow's narrows, and the narrows it produces
do not escape into the current flow."
  (declare (indent 0))
  `(let ((et--checker-narrows nil)) ,@body))


;;;; Narrows modifiers

(defun et-checker-on-set-var (var type)
  "Should be called whenever VAR is assigned to TYPE.

This should be called anytime a variable is set."
  (et-declare (var *et-var) (type *et-type) (@return Nil))

  (setq et--checker-narrows
        (cons (cons var type)
              (cl-remove var et--checker-narrows :key #'car))))


;;;; Tests

(defun et-root-check-type (expr)
  (et-declare (expr Sexp) (@return *et-type))
  (let* ((result (et-result-boundary (et--check-result-type (et--check expr nil nil)))))
    (when (et-result-failed result) (error "Type-checking failed"))
    (et-result-value result)))

(defmacro et-assert-resolve (type expr &optional not)
  (declare (indent 1))
  `(et-result-boundary
    (let* ((t-type (et-at 1 (et ,type)))
           (r-type (et-at 2 (et--check-result-type (et--check ',expr nil nil)))))
      (or (,(if not #'not #'identity) (et-subtype? r-type t-type))
          (et-err 0 "Expected %s, got %s" t-type r-type)))))

(defmacro et-assert-no-resolve (type expr)
  (declare (indent 1))
  `(et-assert-resolve ,type ,expr 'NOT))

(defmacro et-assert-resolve-errors (expr)
  `(et-result-boundary
    (or (et-result-failed (et-result-boundary (et--check-result-type (et--check ',expr nil nil))))
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
                     (output-repr (et-parse-repr return-spec (et-matcher-generics matcher))))
                (et-dt 'DynFunction matcher output-repr))
            (et-dt 'Function (et-parse-type arglist-spec) (et-parse-type return-spec)))))
    (dolist (func (if (symbolp funcs) (list funcs) funcs))
      (put func 'et-checker nil)
      (put func 'et-function-type func-type))))


;;; ============================================================
;;; Useful helpers
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
                     :repr (et-parse-repr matcher-spec gens))
                    type
                    (et-parse-repr output-spec gens))))
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
;;;; Identify expr

(defvar et--processed-requires nil
  "List of libraries which have been processed due to a `require'.")

(defmacro et-define-identifier (name arglist &rest body)
  "Define an identifier for a particular symbol.

The arguments to the identifier will be the cdr of the expression which
triggered identification. The function will be called in a result
boundary, with the current path set to said expression.

The identifier should return a plist that can contain the properties
:constrain, :populate, and :declare, each of which is a 0-argument
function, which will be called during the corresponding phase of
pre-processing. Most identifiers should only use :declare, except in the
rare event that the identifier is declaring some custom type."
  (declare (indent 2))
  `(let* ((identify
           (lambda . ,(cl--transform-lambda
                       (cons arglist body)
                       (intern (format "et-identify:%s" name))))))
     ,@(cl-loop for sym in (if (listp name) name (list name))
                collect `(put ',sym 'et-identify identify))))

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
            (et--process-exprs (et--file-exprs library))))
        nil)

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
             (`(@symbol-property . ,_) (et--identify-symbol-property-directive form))
             (`(@checker . ,_) (et--identify-checker-directive form))))
         collect
         (cons pos plist)))

       ;; Custom identifier
       (`(,(and name (pred symbolp)) . ,rest)
        (when-let* ((identify (get name 'et-identify)))
          (list (cons nil (apply identify rest)))))))))


;;;; Process exprs

(defvar et--processing-phase nil "Used for debugging `et--process-exprs'.")
(defvar et--processing-expr nil "Used for debugging `et--process-exprs'.")

(cl-defun et--process-exprs (exprs &key check test eval only-check)
  (let* ((identified (et-result-map #'et--identify-expr exprs)))

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
    (when (or check only-check)
      (cl-loop for expr in exprs
               for pos upfrom 0
               when (and (consp expr)
                         (or (et-function-type (car expr))
                             (get (car expr) 'et-checker))
                         ;; Only check a certain index when only-check is specified
                         (or (null only-check) (eq only-check pos)))
               do
               (let* ((et--processing-phase :check)
                      (et--processing-expr expr))
                 (et-error-boundary pos (et--check expr nil nil)))))

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


;;;; Functions
;;;;; Types

(et-declare
 (@alias EtFuncParameters [T]
         (Tuple ListR<T> ListR<T> ListR<T> Nil|TupleR<T>))
 (@alias EtFuncDeclarations
         (PList :parameters EtFuncParameters<Var>
                :props KVPList<Var~Any>
                ;; Either a function type or a custom checker
                :definition (or Nil *et-type (fn Nil *et-type))))
 (@symbol-property et-checker (fn Nil *et-type))
 (@symbol-property et-function-type *et-type)
 (@symbol-property et-function-parameters EtFuncParameters<Var>)
 (@symbol-property et-function-props KVPList<Var~Any>))


;;;;; Parse arglist params

(defun et--func-parse-params (arglist)
  "Parse ARGLIST into (REQUIRED OPTIONAL KEY REST).

ARGLIST is a plain parameter list with no type annotations — just
symbols and default-value forms. REQUIRED, OPTIONAL, KEY, and REST are
each lists of parameter name symbols. The list REST has at most 1
element."
  (et-declare (arglist List<Var>)
              (@return EtFuncParameters<Var>))

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


;;;;; Make function type

(defun et--make-function-type (generics constraints input-repr output-repr)
  "Build a Function or DynFunction type from reprs."
  (et-declare (generics ListR<EtGeneric>)
              (constraints ListR<EtTypeConstraint>)
              (input-repr EtRepr)
              (output-repr EtRepr))

  (if generics
      (et-dt 'DynFunction
             (make-et-matcher :generics generics
                              :constraints constraints
                              :repr input-repr)
             output-repr)
    (et-dt 'Function
           (et-repr-to-type input-repr nil)
           (et-repr-to-type output-repr nil))))


;;;;; Finding/parsing declarations

(defun et--func-declares-path (expr)
  (et-declare (expr Sexp) (@return List<Integer>))

  (if-let* ((etd-pos (cl-position 'et-declare expr :key #'car-safe)))
      (list etd-pos)
    (if-let* ((decl-pos (cl-position 'declare expr :key #'car-safe))
              (et-pos (cl-position 'et (nth decl-pos expr) :key #'car-safe)))
        (list decl-pos et-pos)
      nil)))

(defun et--func-find-and-parse-decls (params exprs)
  (et-declare (params EtFuncParameters<Var>) (exprs ListR<Sexp>))
  (when-let* ((declare-path (et--func-declares-path exprs)))
    (let* ((declares (cdr (et--traverse-tree exprs declare-path))))
      (et-at declare-path
        (et-at-offset 1
          (et--func-parse-declarations params declares))))))

(defun et--func-parse-declarations (params declares)
  "Parse function declarations.

This assumes that the current path points to DECLARES."
  (et-declare (params EtFuncParameters<Var>)
              (declares ListR<Any>))

  (let* ((param-list (apply #'append params))
         (param-alist (et: AListR<Var~*et-repr> nil))
         (generics (et: ListR<EtGeneric> nil))
         (constraints (et: ListR<EtTypeConstraint> nil))
         (props (et: KVPList<Var~Any> nil))

         (return-repr (et: Nil|*et-repr nil))
         (function-type (et: Nil|*et-type nil))
         (checker (et: (or Nil (fn Nil *et-type)) nil))

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
             (setq generics (et--gen-vec-generics gv)
                   constraints (et--gen-vec-constraints gv))))
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
          ((or `(@checker ,chk)
               (and `(@checker ,fn . ,args) (let chk `(lambda () (,fn ,@args))))
               (and `(@expand) (let chk #'et-macroexpand-checker))
               (and `(@progn) (let chk #'et-progn-checker))
               (and `(@prog1) (let chk #'et-prog1-checker)))
           (funcall use-strategy 'checker)
           (when checker (et-fatal 0 "Multiple checkers specified"))
           (setq checker chk))

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
                   (input-repr (et--func-params-to-input generics params fn #'identity)))
              (et--make-function-type generics constraints input-repr return-repr)))
           (checker)))))


;;;;; Construct input repr

(defun et--func-params-untyped-input (params)
  (et-declare (params EtFuncParameters<T>) (@return *et-type))
  (et-repr-to-type
   (et--func-params-to-input nil params (lambda (_) (et Any)) #'identity)))

(defun et--func-params-to-input (generics params fn key-fn)
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
                  when (pcase repr ((cl-struct et-repr (dnf `(((S:GENERIC ,_))))) t))
                  collect repr into gen-params
                  finally return
                  (let* ((tail (loop nil rest)))
                    (et-parse-repr
                     `(or (and Nil ,@gen-params)
                          (ConsR ,(funcall fn cur) ,tail))
                     generics (list :field (funcall key-fn cur))))))
        (_ rest-repr)))))


;;;;; Determine param types

(defun et--func-param-types (params func-input-type)
  "Determine the parameter types for a particular function.

If the function has generics, then this MUST be called with the
polymorphic types defined, as those polymorphic types probably appear in
FUNC-INPUT-TYPE."
  (et-declare (params EtFuncParameters<Var>)
              (func-input-type *et-type)
              (@return AList<Var~*et-type>))

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
          (et--func-params-to-input
           param-gens params-with-gens
           (lambda (p) (et-parse-repr (cdr p) param-gens))
           #'car))
         (params-matcher (make-et-matcher :generics param-gens :repr params-repr))
         (match-result (et-sub-match params-matcher func-input-type))
         (matches (if (et-match-result-success match-result) (et-match-result-value match-result)
                    (error "Function type is not compatible with parameters"))))
    (cl-loop for (param . _sym) in param-gens-alist
             for type in matches
             collect (cons param type))))


;;;;; Identifiers

(defun et--func-assign-decls (name decls)
  "Assign relevant symbol properties to FUNC."
  (et-declare (name Var) (decls EtFuncDeclarations) (@return Nil))

  (put name 'et-function-props (plist-get decls :props))
  (put name 'et-function-parameters (plist-get decls :parameters))
  (pcase (plist-get decls :definition)
    ((and type (pred et-type-p)) (put name 'et-function-type type))
    ((and chk (pred functionp)) (put name 'et-checker chk))))

(defun et--identify-defun (name arglist &rest rest)
  (list
   :declare
   (lambda ()
     (let* ((params (et-at 2 (et--func-parse-params arglist))))
       (when-let* ((decls (et-at-offset 3 (et--func-find-and-parse-decls params rest))))
         (et--func-assign-decls name decls))))))

(et-define-identifier (defun cl-defun defmacro) (name arglist &rest rest)
  (apply #'et--identify-defun name arglist rest))
(et-define-identifier et-defun (&rest rest)
  (apply #'et--identify-defun (cdr (macroexpand-1 (cons #'et-defun rest)))))

(defun et--func-declare-from-directive (name arglist declares)
  (let* ((param-groups (et-at 2 (et--func-parse-params arglist)))
         (decls (et-at-offset 3 (et--func-parse-declarations param-groups declares))))
    (et--func-assign-decls name decls)))

(defvar et-defer-declarations t)

(defun et--identify-function-directive (form)
  (list
   :declare
   (lambda ()
     (pcase-let* ((`(@function ,name ,arglist . ,declares) form))
       (if et-defer-declarations
           (put name 'et-deferred-declare
                (lambda () (et--func-declare-from-directive name arglist declares)))
         (et--func-declare-from-directive name arglist declares))))))

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


;;;;; Check body

(et-defvar et-checking-defun Nil|Var nil
  "The defun currently being checked.")

(et-defvar et-func-check-overloads Boolean t
  "Whether to check all overloads of functions.")

(defun et--func-check-body (params func-type body-path)
  "The path should point to the function expr.

Returns the type of the last expression in the body."
  (et-declare (func-type *et-type) (params EtFuncParameters<Var>) (body-path TreeR<Integer>)
              (@return *et-type))

  (cl-loop
   with overloads = (et--func-destructure func-type et-func-check-overloads)
   for (polys input-type expected-ret) in overloads
   for overload-idx upfrom 1
   collect
   (et-with-diagnostic-prefix (when (cdr overloads) (format "Overload %s" overload-idx))
     (et--with-polymorphic-types polys
       (let* ((param-types (et--func-param-types params input-type))
              (param-vars (cl-loop for (p . type) in param-types collect (et-new-var p type))))

         ;; Ensure that optional parameters are nillable
         (dolist (opt (cadr params))
           (unless (et-subtype? (et Nil) (alist-get opt param-types))
             (et-err nil "Optional parameter %s is not nillable" opt)))

         (et-with-vars param-vars
           ;; The body runs whenever the function is called, not here
           (et-checker-deferred
             (let* ((actual-ret (et-checker-tail body-path)))
               (or (et-subtype? actual-ret expected-ret)
                   (et-err 0 "Expected %s, found %s" expected-ret actual-ret))
               (et-remove-type-binds-and-polys actual-ret (mapcar #'car polys))))))))
   into rets
   finally return (et:algebra-simplify-type (apply #'et-union rets))))


;;;;; Checkers

(et-define-pcase-checker (defun cl-defun) `(,name . ,_)
  (when-let* ((func-type (et-function-type name))
              ((not (plist-get (get name 'et-function-props) :skip)))
              (et-checking-defun name))
    (et--func-check-body (get name 'et-function-parameters) func-type 3))
  (et-literal name))

(et-define-pcase-checker et-defun `(,name . ,_)
  (when-let* ((func-type (et-function-type name))
              ((not (plist-get (get name 'et-function-props) :skip)))
              (et-checking-defun name))
    (et--func-check-body (get name 'et-function-parameters) func-type 4))
  (et-literal name))


(et-define-pcase-checker lambda `(,arglist . ,body)
  (let* ((params (et-at 1 (et--func-parse-params arglist)))
         (untyped-input nil)
         (func-type
          (or (when-let* ((decls (et-at-offset 2 (et--func-find-and-parse-decls params body)))
                          (func-type (plist-get decls :definition)))
                (when (et-type-p func-type) func-type))
              ;; From recommendation
              et--checker-recommendation
              ;; All params are Any
              (progn (setq untyped-input (et--func-params-untyped-input params))
                     (et-dt 'Function untyped-input (et Any)))))
         (actual-ret (et--func-check-body params func-type 2)))
    ;; If the function is untyped, then we should use the actual return type
    (if untyped-input (et-dt 'Function untyped-input actual-ret) func-type)))


;;;; Variables

(defun et--declare-variable-type (name type)
  (put name 'et-variable-type type)
  (put name 'et-variable-var (make-et-var :name name :type type)))

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
          (et--declare-variable-type name (et-parse-type spec))))))

    (_ (et-fatal nil "Expected format (@variable NAME TYPE)"))))

(et-define-identifier et-defvar (name spec &rest _)
  (list
   :declare
   (lambda ()
     (et-at 2
       (et--declare-variable-type name (et-parse-type spec))))))

(et-define-pcase-checker (defvar et-defvar) `(,(and (pred symbolp) name) . ,_)
  (if-let* ((declared-type (get name 'et-variable-type))
            (value-pos (if (eq #'defvar (car et--checker-expr)) 2 3))
            (value-type (or (when (nth value-pos et--checker-expr)
                              (et-checker-sub value-pos))
                            (et Nil))))
      (unless (et-subtype? value-type declared-type)
        (et-err 2 "Expected %s, found %s" declared-type value-type))

    ;; Otherwise, declare it as any
    (et--declare-variable-type name (et Any)))

  (et-literal name))


;;;; Identify cl-defstruct

(et-define-identifier cl-defstruct (&rest args)
  (et--identify-cl-defstruct (cons #'cl-defstruct args)))

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
              ;; Repr helpers shared across all generated functions
              ;; STRUCT-REPR is the struct's own type, e.g. *Name<T1 T2>
              (struct-repr (et-parse-repr (et-q (Struct ,name ,@generics)) generics))
              ;; ARGLIST-REPR is the single-argument arglist (STRUCT), used by
              ;; accessors and the copier
              (arglist-repr (et-parse-repr (et-q (ConsR ,struct-repr Nil)) generics)))

         ;; --- Predicate ---
         (when predicate
           (let* ((never-args (make-list (length generics) 'Never))
                  (output-repr
                   (et-parse-repr
                    (et-q (or (and True (bindsof (and T (Struct ,name ,@never-args))))
                              (and Nil (bindsof (subtract T (Struct ,name ,@never-args))))))
                    '(T))))
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
                                       (et-parse-repr (cdr type-info) generics))
                                   (et-parse-repr 'Any generics))))
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
                                                    (et-parse-repr (cdr type-info) generics)))
                                              (et-parse-repr 'Any nil))
                            nconc (list (intern (format ":%s" slot-name)) slot-repr) into args
                            finally return (et-parse-repr (if args `(PList ,@args) 'Nil) nil))))
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
                (generics (plist-get props :generics)))
           (plist-put props :repr
                      (et-parse-repr spec generics))))))))


;;;; Identify checker directive

(defun et--identify-checker-directive (form)
  (pcase (cdr form)
    (`(,(and sym (pred symbolp)) ,(and arglist (pred listp)) . ,body)
     (let* ((fn `(lambda ,arglist ,@body)))
       `(:declare
         ,(lambda ()
            (put sym 'et-checker (lambda () (apply fn (cdr et--checker-expr))))))))
    (_ (et-fatal nil "Expected format (@checker NAME ARGLIST BODY...)"))))


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

(et-define-pcase-checker :typeof `(,_expr . ,rest)
  (let* ((type (et-checker-sub 1))
         (id (car rest)))
    (if (null id) (et-hint nil type)
      (et-with-diagnostic-prefix id (et-hint nil type)))
    type))

(et-define-pcase-checker :typeof+ `(,_expr . ,rest)
  (let* ((type (et-checker-sub 1))
         (id (car rest))
         (et-print-labels t)
         (et-print-narrows t))
    (if (null id) (et-hint nil type)
      (et-with-diagnostic-prefix id (et-hint nil type)))
    type))

(et-define-pcase-checker :expand spec
  (let* ((type (et-expand-all-aliases (et-parse-type spec))))
    (et-hint nil type)
    type))

(et-define-pcase-checker :narrows `()
  (cl-loop for (var . type) in (reverse et--checker-narrows)
           collect (format "%s: %s" (et-var-name var) (et-pp type)) into strs
           finally do
           (et-hint nil (string-join strs "\\n")))
  (setq et--checker-expr nil)
  (et Nil))

(et-define-pcase-checker :eval `(,expr)
  (et-hint nil (et-pp (eval expr)))
  (setq et--checker-expr nil)
  (et Nil))


;;;; Annotation macros

;; `declare' forms carry no runtime value; type them as Nil so they are
;; ignored wherever they appear.
(et-define-checker declare (et Nil))
(et-define-checker et-declare (et Nil))

(et-define-pcase-checker et: `(,type-spec ,_expr)
  (let* ((declared (et-parse-type type-spec))
         (actual (et-checker-sub 2 :recommendation declared)))
    (unless (et-subtype? actual declared)
      (et-err 0 "Expected %s, found %s" declared actual))
    declared))

(et-define-pcase-checker et! args
  (pcase (length args)
    (1 (et-checker-sub 1) (et Never))
    (2 (let* ((declared (et-parse-type (car args))))
         (et-checker-sub 2 :recommendation declared)
         declared))
    (n (et-fatal 0 "Wrong number of arguments: %s" n))))

(put 'et-defun 'checker #'et-macroexpand-checker)


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

(et-defvar et--macroexpand-waiting Boolean nil
  "We are in a macroexpand, waiting to find a matching expr.")

(defun et--path-in-tree (expr tree)
  "If EXPR exists in TREE, return its path, or return `NO' otherwise."
  (if (equal expr tree) nil
    (cl-loop for subtree in tree ; safe even if tree is not a list
             for idx upfrom 0
             for path = (et--path-in-tree expr subtree)
             unless (eq path 'NO) return (cons idx path)
             finally return 'NO)))

(defun et--macroexpand-check-advice (func &rest args)
  (if (not et--macroexpand-waiting)
      (apply func args)

    (let* ((path (et--path-in-tree (car args) et--macroexpand-expr)))
      (if (eq path 'NO) (apply func args)
        (let* ((et--macroexpand-waiting nil))
          (et-without-sticky-path
            (et-at path (apply func args))))))))

(advice-add #'et--check :around #'et--macroexpand-check-advice)

(defun et-checker-expansion (expanded &optional recommendation)
  "Type check EXPANSION, an expr which was built from the current expr.

EXPANSION is not literally present in the current expression, but it was
built from the current expression, so parts of the current expression
probably exist somewhere inside of EXPANSION, and should be mapped back
onto the original expression."
  (let* (;; Only the very root macroexpand expr exists in the actual code.
         ;; If we get another expansion inside an expansion, keep the original root-level expr
         (et--macroexpand-expr (or et--macroexpand-expr et--checker-expr))
         (result
          (et-with-sticky-path
            (et--check expanded et--checker-narrows recommendation))))
    (setq et--checker-narrows (et--check-result-narrows result))
    (et--check-result-type result)))

(defun et-macroexpand-checker ()
  "Type checker which expands a macro and type-checks the expansion."
  (unless (macrop (car et--checker-expr))
    (et-fatal 0 "Macro not defined: %s" (car et--checker-expr)))
  (et-checker-expansion (macroexpand-1 et--checker-expr)))


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
