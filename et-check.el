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

(defun et--get-symbol-variable (sym)
  (cl-assert (symbolp sym))
  (alist-get sym et--binds))

(defmacro et-with-vars (vars &rest body)
  (declare (indent 2))
  `(let* ((vs (cl-loop for var in ,vars
                       do (cl-assert (et-var-p var))
                       collect (cons (et-var-name var) var)))
          (et--binds (append vs et--binds)))
     ,@body))


;;;; Narrowed bindings

(defvar et--narrow-binds nil
  "Stack of (`et-var' . `et-type').")

(defun et--get-variable-type (variable)
  (or (alist-get variable et--narrow-binds)
      (et-var-type variable)))

(defun et--get-symbol-type (sym)
  (cl-assert (symbolp sym))
  (when-let ((var (et--get-symbol-variable sym)))
    (et--get-variable-type var)))

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
;;;; Define checker

(defmacro et-define-checker (funcs &rest body)
  "A checker should return a type, or nil if the expression is invalid."
  (declare (indent 1))
  (cl-assert (or (symbolp funcs) (seq-every-p #'symbolp funcs)))

  `(let* ((checker (lambda . ,(cl--transform-lambda
                               (cons () body)
                               (format "et--checker:%s" funcs)))))
     ,@(cl-loop for func in (if (symbolp funcs) (list funcs) funcs)
                collect `(setf (get ',func 'et-checker) checker))
     ',funcs))

(defmacro et-define-pcase-checker (funcs &rest cases)
  "A checker should return a type, or nil if the expression is invalid."
  (declare (indent 1))
  `(et-define-checker ,funcs (pcase (cdr et--checker-expr) ,@cases)))


;;;; Type checker

(defun et--type-checker-body (arglist-matcher return-struct exprs)
  (let* ((arg-types (cl-loop for _expr in exprs
                             for idx upfrom 1
                             collect (et-check-path idx)))
         (args-type (cl-loop with acc = (et-literal nil)
                             for arg-type in (nreverse arg-types)
                             do (setq acc (et-dt 'Cons:RR arg-type acc))
                             finally return acc))
         (result (et--sub-match arglist-matcher args-type)))
    (when (eq result 'INVALID)
      (error "Invalid arguments! Expected %s, got %s"
             (et-pp-matcher arglist-matcher) (et-pp args-type)))

    ;; Replace all places where the generic variable appeared in the return type
    ;; with the value determined for that generic
    (cl-loop for gen in (et-matcher-generics arglist-matcher)
             for type in result
             do (setq return-struct
                      (cl-subst (et-q (S:TYPE ,type))
                                (et-q (S:ALIAS ,gen))
                                return-struct
                                :test #'equal)))

    (et-structure-to-type return-struct)))

(defmacro et-define-type-checker (funcs &rest arguments)
  "Define a checker using argument and return types.

FUNCS is the function to define the checker for, or a list of functions
to define the checker for.

GENERICS is a vector of symbols, representing generic variables. Each
generic variable should be uppercase.

ARGLIST is a parsable expression to use to match the arglist against.

RETURN is a parsable expression to use for the return type. This can use
the generic variable names as aliases, and they will be correctly
substituted.

\(fn FUNC [GENERICS] ARGLIST RETURN)"
  (declare (indent 2))

  (let ((generics
         (when (vectorp (car arguments))
           ;; Make sure the generics have the correct format
           (cl-loop for var across (car arguments)
                    do (or (symbolp var) (error "Generic vars must be symbols"))
                    do (or (let ((case-fold-search nil))
                             (string-match-p "^[A-Z]" (format "%s" var)))
                           (error "Generic vars must start with an uppercase letter")))
           (append (pop arguments) nil))))
    (unless (eq (length arguments) 2)
      (error "Incorrect number of arguments"))

    `(et-define-checker ,funcs (&rest exprs)
                        (et--type-checker-body
                         ,(et-parse-matcher (car arguments) generics)
                         (copy-tree ',(et-parse-structure (cadr arguments) nil))
                         exprs))))


;;;; Checker return type

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
  "Returns the type of the current expr, if typechecking did not error."
  (let* ((et--checker-diagnostics nil)
         (et--checker-expr expr)
         (return-type nil))

    (pcase expr
      (`(,func . ,_args)
       (if-let* ((checker (get func 'et-checker)))
           (condition-case result (funcall checker)
             (et-checker-fatal (et-never))
             (error (et-checker-diagnostic () "Checker for `%s' threw error: %s" result)
                    (et-never))
             (:success
              (if (et-type-p result) (setq return-type result)
                (et-checker-diagnostic () "Checker for `%s' had invalid return: %s" result))))

         (et-checker-diagnostic '(0) "No checker for `%s'" func)))

      ((and sym (pred symbolp) (guard sym) (guard (not (eq sym t))))

       (if-let* ((var (et--get-symbol-variable sym)))
           (setq return-type
                 (et--supersect
                  (et--get-variable-type var)
                  (et-type (make-et-type-case :value (make-et-datatype :name 'Any)
                                              :typeofs (list var)))))

         (et-checker-diagnostic () "Free variable: %s" var)))

      (expr (setq return-type (et-literal expr))))

    (cl-assert (or et--checker-diagnostics return-type))
    (make-et-result :type return-type
                    :diagnostics (nreverse et--checker-diagnostics)
                    :compiled et--checker-expr)))


(defun et-checker-diagnostic (path severity fmt &rest args)
  (push (list path severity (if args (apply #'format fmt args) fmt))
        et--checker-diagnostics))

(defun et-checker-err (fmt &rest args) (apply #'et-checker-diagnostic nil 'error fmt args))
(defun et-checker-warn (fmt &rest args) (apply #'et-checker-diagnostic nil 'warning fmt args))
(defun et-checker-hint (fmt &rest args) (apply #'et-checker-diagnostic nil 'hint fmt args))

(define-error 'et-checker-fatal "Signalled by a checker which has a fatal problem.")
(defun et-checker-fatal (path fmt &rest args)
  (apply #'et-checker-diagnostic 'fatal path fmt args)
  (signal 'et-checker-fatal nil))


;;;; Check wrappers

(defun et--traverse-tree (path tree)
  (if (null path) tree
    (when (>= (car path) (length tree))
      (error "Index out of bounds: %s %s" (car path) tree))
    (et--traverse-tree (cdr path) (nth (car path) tree))))

(defun et-check-path (&rest path)
  (cl-assert et--checker-expr)
  (setq path (flatten-tree path))

  (let* ((root-expr et--checker-expr)
         (sub-expr (et--traverse-tree path root-expr))
         (sub-result (et--check sub-expr)))
    (cl-assert (et-result-p sub-result))
    ;; Diagnostics in the result have paths relative to sub-expr
    ;; Rebase them to be relative to et--checker-expr
    (cl-loop for (p severity message) in (et-result-diagnostics sub-result)
             do (et-checker-diagnostic (append path p) severity message))
    ;; Return just the inner type
    (et-result-type sub-result)))

(defun et-check-tail (&rest first-path)
  (cl-assert et--checker-expr)
  (cl-assert first-path)
  (setq first-path (flatten-tree first-path))

  (let* ((parent-path (butlast first-path 1))
         (parent-expr (et--traverse-tree parent-path et--checker-expr))
         (start (car (last first-path)))
         ;; The type for an empty tail is nil
         (last-type (et-literal nil)))

    (cl-loop for idx upfrom start below (length parent-expr)
             do (setq last-type (et-check-path (append parent-path (list idx))))
             finally return last-type)))


;;;; Root level functions

(defun et--typecheck (body &optional noemit)
  (let* ((results (mapcar #'et--check body))
         (types (mapcar #'et-result-type results))
         (compiled (mapcar #'et-result-compiled results))
         (blackbox (lambda (x) x)))

    ;; Display diagnostics
    (unless noemit
      (cl-loop for result in results
               for pos upfrom 1
               do (cl-loop for (path _severity message) in (et-result-diagnostics result)
                           do (et-error (cons pos path) message))))

    ;; Return the (type . compiled)
    (progn
      (cons
       (funcall blackbox (car (last types)))
       (if (eq (length compiled) 1) (funcall blackbox (car compiled))
         (cons #'progn compiled))))))

(defmacro et-typecheck (&rest body)
  `(et--typecheck ',body))

(defmacro et-typecheck-func (func &rest arg-types)
  (cl-loop for type in arg-types
           collect (list :type type) into arg-exprs
           finally return (cons #'et-typecheck (cons func arg-exprs))))

(defmacro et-assert-resolve (type expr)
  (declare (indent 1))
  `(let* ((t-type (et-with-error-path (list 1) (et ,type)))
          (expr-type (et-with-error-path (list 2) (car (et--typecheck '(,expr) t)))))
     (or (et-subtype? expr-type t-type)
         (error "Expr=%s" (cl-prin1-to-string expr-type)))))

(defmacro et-assert-no-resolve (type expr)
  (declare (indent 1))
  `(condition-case val (et-assert-resolve ,type ,expr)
     (error t)
     (:success (error "=> %s" val))))


;;;; Tests

(et-test
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
 (et-assert-no-resolve Symbol a) ; Not self-quoting
 (et-assert-no-resolve Symbol 1)
 (et-assert-no-resolve Symbol "1")

 (et-assert-resolve Boolean t)
 (et-assert-resolve Boolean nil)
 (et-assert-no-resolve Boolean a)
 (et-assert-no-resolve Boolean 1)
 (et-assert-no-resolve Boolean "1"))

(et-test
 ;; and - value must satisfy all constituent types
 (et-assert-resolve Boolean&Symbol&True&@t t)
 (et-assert-error   (et-root-resolve 'Boolean&Integer t))
 (et-assert-error   (et-root-resolve 'Boolean&Integer 1))
 (et-assert-error   (et-root-resolve 'Boolean&Integer nil))

 ;; Two or types
 (et-assert-resolve Boolean|Integer t)
 (et-assert-resolve Boolean|Integer nil)
 (et-assert-resolve Boolean|Integer 1)
 (et-assert-error   (et-root-resolve 'Boolean|Integer "1"))
 (et-assert-error   (et-root-resolve 'Boolean|Integer 'a))

 ;; Three or types
 (et-assert-resolve Boolean|Integer|String t)
 (et-assert-resolve Boolean|Integer|String 1)
 (et-assert-resolve Boolean|Integer|String "1")
 (et-assert-error   (et-root-resolve 'Boolean|Integer|String 'a))

 ;; Nested - and inside or
 (et-assert-resolve Integer|Boolean&Symbol t)
 (et-assert-resolve Integer|Boolean&Symbol 1)
 (et-assert-error   (et-root-resolve 'Integer|Boolean&Symbol 'a))

 ;; Nested - or inside and
 (et-assert-resolve Boolean&{Symbol|Integer} t)
 (et-assert-resolve Boolean&{Symbol|Integer} nil)
 (et-assert-error   (et-root-resolve 'Boolean&{Symbol|Integer} 1)))


;;; ============================================================
;;; Utils
;;;; Testing checkers

(et-define-pcase-checker :type
  (`(,spec) (setq et--checker-expr "dummy") (et-parse-type spec)))

(et-define-pcase-checker :assert-subtype
  (`(,_expr ,type-spec)
   (let ((expr-type (et-check-path 1)))
     (or (et-subtype? expr-type (et-parse-type type-spec))
         (et-checker-err "Not subtype: %s" (et-pp expr-type)))
     (setq et--checker-expr "dummy")
     (et Nil))))

(et-define-pcase-checker :assert-error
  (`(,_expr)
   (condition-case _err (et-check-path 1)
     (error (setq et--checker-expr nil) (et-literal nil))
     (:success (et-checker-err "Didn't error")))))

(et-define-pcase-checker :typeof
  (`(,_expr)
   (let ((type (et-check-path 1)))
     (et-checker-warn (et-pp type))
     (setq et--checker-expr (cadr et--checker-expr))
     type)))

(et-define-pcase-checker :narrows
  (`()
   (cl-loop for (var . type) in (reverse et--narrow-binds)
            collect (format "%s: %s" (et-var-name var) (et-pp type)) into strs
            finally do
            (et-checker-warn (string-join strs "\\n")))
   (setq et--checker-expr nil)
   (et Nil)))

(et-define-pcase-checker :eval
  (`(,expr)
   (et-checker-warn (cl-prin1-to-string (eval expr)))
   (setq et--checker-expr nil)
   (et Nil)))


;;;; Pcase et-*

(pcase-defmacro et-* (vars pattern)
  "Match a list where each element matches PATTERN, collecting bindings.
VARS is a list of symbols that PATTERN binds.
Each variable is bound to the list of values it took across all elements.

Example:
  (pcase \\='((a 1) (b 2))
    ((et-* (name val) `(,name ,val))
     (list name val)))
  => ((a b) (1 2))"

  (let ((lst (gensym "lst"))
        (elt (gensym "elt"))
        (accumulators (mapcar (lambda (v) (cons v (gensym (symbol-name v))))
                              (append vars nil))))
    `(and (pred listp)
          (app (lambda (,lst)
                 (let ,(mapcar (lambda (a) (list (cdr a) nil)) accumulators)
                   (when (cl-every (lambda (,elt)
                                     (pcase ,elt
                                       (,pattern
                                        ,@(mapcar (lambda (a)
                                                    `(push ,(car a) ,(cdr a)))
                                                  accumulators)
                                        t)
                                       (_ nil)))
                                   ,lst)
                     (list ,@(mapcar (lambda (a) `(reverse ,(cdr a)))
                                     accumulators)))))
               (,'\` (,@(mapcar (lambda (v) (list '\, v)) (append vars nil))))))))


;;; ============================================================
;;; Provide

(provide 'et-check)


;;; et-check.el ends here
