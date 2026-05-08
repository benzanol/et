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
;;; Utils
;;;; Error/warn

(defun et-warn (path msg &rest args)
  (setq msg (concat msg (et--error-message-suffix path)))
  (apply #'byte-compile-warn msg args))


;;; ============================================================
;;; Bindings
;;;; Symbol bindings

(defvar et--binds nil
  "Stack of (SYMBOL . `et-var').")

(defun et--get-symbol-variable (sym)
  (cl-assert (symbolp sym))
  (alist-get sym et--binds))

(defmacro et-with-bind (sym type &rest body)
  (declare (indent 2))
  `(let* ((sym ,sym)
          (var (make-et-var :name sym :type ,type))
          (et--binds (cons (cons sym var) et--binds)))
     ,@body))

(defmacro et-with-binds (binds &rest body)
  (declare (indent 2))
  `(let* ((et--binds (append ,binds et--binds)))
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

(defvar et-display-narrows nil
  "Whether to display narrowed types on if/when/etc blocks.")

(defun et-warn-narrows (&rest types)
  "Display a list of binds to the user at path=(0).

TYPES is (FMT1 TYPE1 FMT2 TYPE2 ...)."
  (when et-display-narrows
    (cl-loop for (fmt type) on types by #'cddr
             for binds = (et--type-binds type) ; TODO: display just binds instead of whole type
             when binds
             collect (format fmt (et-pp-narrows binds)) into strs
             finally do
             (when strs
               (et-warn '(0) "%s" (string-join strs "\\n"))))))


;;; ============================================================
;;; Checking
;;;; Path

(defvar et--current-expr nil)
(defvar et--current-path nil)

(defmacro et-with-path (path &rest body)
  (declare (indent 1))
  (let ((path-var (make-symbol "path"))
        (parent-var (make-symbol "parent")))
    `(let* ((,path-var ,path)
            (et--current-path (append et--current-path ,path-var))
            (,parent-var (when ,path-var (et--traverse-tree (butlast ,path-var) et--current-expr)))
            (et--current-expr (if ,path-var (nth (car (last ,path-var)) ,parent-var)
                                et--current-expr)))
       (unwind-protect (progn ,@body)
         (when ,path-var
           (setf (nth (car (last ,path-var)) ,parent-var)
                 et--current-expr))))))

(defun et--traverse-tree (path tree)
  (if (null path) tree
    (when (>= (car path) (length tree))
      (error "Index out of bounds: %s %s" (car path) tree))
    (et--traverse-tree (cdr path) (nth (car path) tree))))


;;;; Define checker

(defmacro et-define-checker (funcs arglist &rest body)
  (declare (indent 2))
  (cl-assert (or (symbolp funcs) (seq-every-p #'symbolp funcs)))
  (cl-assert (listp arglist))

  `(let* ((checker (lambda . ,(cl--transform-lambda
                               (cons arglist body)
                               (format "et--checker:%s" funcs)))))
     ,@(cl-loop for func in (if (symbolp funcs) (list funcs) funcs)
                collect `(setf (get ',func 'et-checker) checker))
     ',funcs))


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


;;;; Check

(defun et-check ()
  "Returns the type of the current expr, if typechecking did not error."
  (et--verify-type
   (pcase et--current-expr
     (`(,func . ,args)
      (or (apply (or (get func 'et-checker) (error "No checker for function: %s" func))
                 args)
          (error "Checker for %s returned nil" func)))

     ((and sym (pred symbolp) (guard sym) (guard (not (eq sym t))))

      (let ((var (or (et--get-symbol-variable sym)
                     (error "Free variable: %s" sym))))
        (et--and
         (et--get-variable-type var)
         (et-type (make-et-type-case :value (make-et-datatype :name 'Any)
                                     :typeofs (list var))))))

     (expr (et-literal expr)))))


;;;; Check position helpers

(defun et-check-path (&rest path)
  (et-with-path path (et-check)))

(defun et-check-tail (start)
  (cl-loop for idx upfrom start below (length et--current-expr)
           for type = (et-with-path (list idx) (et-check))
           finally return (or type (et-literal nil))))


;;;; Root level functions

(defmacro et--root (expr &rest body)
  (declare (indent 1))
  `(progn
     (cl-assert (null et--current-expr))
     (cl-assert (null et--current-path))
     (cl-assert (null et--binds))
     (let ((et--current-expr ,expr))
       ,@body)))

(defmacro et-root-block (&rest body)
  (et--root (cons #'progn body)
    (et-check-tail 1)
    et--current-expr))

(defun et-root-check (expr)
  (et--with-error-path (list 1)
    (et--root expr (et-check))))

(defmacro et-root-check-call (func &rest arg-types)
  `(et--root ',(cons func (cl-loop for type in arg-types collect (list :type type)))
     (et-check)))

(defun et-resolve (type)
  (let ((expr-type (et-check)))
    (unless (et-subtype? expr-type type)
      (error "Type %s is not assignable to type %s"
             (et-pp expr-type) (et-pp type)))))

(defun et-root-resolve (type expr)
  (et--root expr (et-resolve (et-parse-type type))))


;;;; Tests

(et-test
 (et-assert-success (et-root-resolve 'Number 1))
 (et-assert-success (et-root-resolve 'Positive 1))
 (et-assert-success (et-root-resolve 'Negative -1))
 (et-assert-success (et-root-resolve 'Number 1.1))
 (et-assert-error (et-root-resolve 'Number "1"))
 (et-assert-error (et-root-resolve 'Integer 1.1))
 (et-assert-error (et-root-resolve 'Positive -1))
 (et-assert-error (et-root-resolve 'Positive 0))
 (et-assert-error (et-root-resolve 'Negative 1))
 (et-assert-error (et-root-resolve 'Negative 0))

 (et-assert-success (et-root-resolve 'String "1"))
 (et-assert-error (et-root-resolve 'String 1))

 (et-assert-success (et-root-resolve 'Symbol nil))
 (et-assert-success (et-root-resolve 'Symbol t))
 (et-assert-error (et-root-resolve 'Symbol 'a)) ; Not self-quoting
 (et-assert-error (et-root-resolve 'Symbol 1))
 (et-assert-error (et-root-resolve 'Symbol "1"))

 (et-assert-success (et-root-resolve 'Boolean t))
 (et-assert-success (et-root-resolve 'Boolean nil))
 (et-assert-error (et-root-resolve 'Boolean 'a))
 (et-assert-error (et-root-resolve 'Boolean 1))
 (et-assert-error (et-root-resolve 'Boolean "1")))

(et-test
 ;; and - value must satisfy all constituent types
 (et-assert-success (et-root-resolve 'Boolean&Symbol&True&@t t))
 (et-assert-error   (et-root-resolve 'Boolean&Integer t))
 (et-assert-error   (et-root-resolve 'Boolean&Integer 1))
 (et-assert-error   (et-root-resolve 'Boolean&Integer nil))

 ;; Two or types
 (et-assert-success (et-root-resolve 'Boolean|Integer t))
 (et-assert-success (et-root-resolve 'Boolean|Integer nil))
 (et-assert-success (et-root-resolve 'Boolean|Integer 1))
 (et-assert-error   (et-root-resolve 'Boolean|Integer "1"))
 (et-assert-error   (et-root-resolve 'Boolean|Integer 'a))

 ;; Three or types
 (et-assert-success (et-root-resolve 'Boolean|Integer|String t))
 (et-assert-success (et-root-resolve 'Boolean|Integer|String 1))
 (et-assert-success (et-root-resolve 'Boolean|Integer|String "1"))
 (et-assert-error   (et-root-resolve 'Boolean|Integer|String 'a))

 ;; Nested - and inside or
 (et-assert-success (et-root-resolve 'Integer|Boolean&Symbol t))
 (et-assert-success (et-root-resolve 'Integer|Boolean&Symbol 1))
 (et-assert-error   (et-root-resolve 'Integer|Boolean&Symbol 'a))

 ;; Nested - or inside and
 (et-assert-success (et-root-resolve 'Boolean&{Symbol|Integer} t))
 (et-assert-success (et-root-resolve 'Boolean&{Symbol|Integer} nil))
 (et-assert-error   (et-root-resolve 'Boolean&{Symbol|Integer} 1)))


;;; ============================================================
;;; Utils
;;;; Testing checkers

(et-define-checker :type (spec)
  (et-parse-type spec))

(et-define-checker :assert-subtype (_expr type-spec)
  (let ((expr-type (et-check-path 1)))
    (or (et-subtype? expr-type (et-parse-type type-spec))
        (error "Not subtype: %s" (et-pp expr-type)))
    (setq et--current-expr "dummy") ; To put into the compiled code in place of the (:assert-subtype) expr
    (et-literal nil)))

(et-define-checker :assert-error (_expr)
  (condition-case _err (et-check-path 1)
    (error (setq et--current-expr nil) (et-literal nil))
    (:success (error "Didn't error"))))

(et-define-checker :typeof (_expr)
  (et-warn '(0) "%s" (et-pp (et-check-path 1)))
  (setq et--current-expr nil)
  (et-literal nil))

(et-define-checker :narrows ()
  (cl-loop for ((var . _) . type) in (reverse et--narrow-binds)
           collect (format "%s: %s" var (et-pp type)) into strs
           finally do
           (et-warn '(0) "%s" (string-join strs "\\n")))
  (setq et--current-expr nil)
  (et-literal nil))


;;; ============================================================
;;; Provide

(provide 'et-check)


;;; et-check.el ends here
