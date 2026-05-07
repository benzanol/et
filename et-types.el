;;; et-types.el --- Typesystem for emacs lisp        -*- lexical-binding: t; -*-

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

(eval-and-compile
  (add-to-list 'load-path "~/.emacs.d/my-packages/typesystem")
  (require 'et))


;;; ============================================================
;;; Control flow
;;;; let

(et-define-checker let* (varlist &rest _body)
  ;; Process let forms
  (cl-loop
   with let-binds-rev = nil
   for form in varlist
   for idx upfrom 0
   do
   (et-with-path (list 1 idx)
     (pcase form
       ;; Binding with a type annotation
       (`(,var ,type ,val)
        ;; Parse the type
        (et-with-path (list 1) (setq type (et-parse-type type)))
        ;; Ensure the value fits the type
        (et-with-binds let-binds-rev (et-with-path (list 2) (et-resolve type)))
        ;; Push the binding
        (setq et--current-expr (list var val))
        (push (cons var type) let-binds-rev))

       ;; Binding with no type annotation
       (`(,var ,_val)
        (let ((type (et-with-binds let-binds-rev (et-with-path (list 1) (et-check)))))
          (push (cons var type) let-binds-rev)
          (et-warn (list 0) "%s: %s" var (et-pp type))))

       (wrong (error "Invalid let binding: %s" wrong))))

   finally return
   (et-with-binds let-binds-rev
     (et-check-tail 2))))


;;;; dolist

(et-define-checker dolist (spec &rest)
  (let (variable type)
    (pcase spec
      ;; With explicit type
      (`(,var ,etype ,_val)
       (et-with-path (list 1 1) (setq type (et-parse-type etype)))
       (setq variable var)
       ;; Ensure the value fits the type
       (et-with-path (list 1 2) (et-resolve (et-alias 'List:R type))))

      ;; With implicit type
      (`(,var ,_val)
       (setq variable var)
       (et-with-path (list 1 1)
         (let* ((list-type (et-check))
                (infer (et--sub-match (et-matcher [T] List:R<T>) list-type)))
           (when (eq infer 'INVALID) (error "Expected list, found %s" (et-pp list-type)))
           (setq type (car infer))))
       (et-warn (list 1 0) "%s: %s" var (et-pp type)))

      (_ (error "Invalid dolist variable spec")))

    ;; Check the body
    (et-with-binds (list (cons variable type))
      (et-check-tail 2))

    (et Nil)))


;;;; setq

(et-define-checker setq (&rest args)
  (unless (eq (mod (length args) 2) 0)
    (et-with-path (list (length args))
      (error "Unmatched variable")))

  (cl-loop for (var _val) on args by #'cddr
           for idx upfrom 0 by 2
           for type = (or (et--get-symbol-bind var)
                          (et-with-path (list (1+ idx))
                            (error "Assignment to free variable")))
           do (et-with-path (list (+ idx 2))
                (et-resolve type))

           finally return type))


;;;; and/or

(defun et--non-nil (type)
  ;; TODO: Should be
  ;; (et-subtract cond-type (et Nil))
  (et--and type (et True)))

(defun et--and-return-type (cond-type checker)
  ;; The next case will only get evaluated if all previous were non-nil
  (let* ((non-nil-binds (et--type-binds (et--non-nil cond-type)))
         (output-type (et-with-narrow-binds non-nil-binds (funcall checker)))

         (output-non-nil (et--non-nil output-type))
         ;; If `and' returns non-nil, then both non-nil binds will be true (intersect them)
         (merged-non-nil-binds
          (et--intersect-binds non-nil-binds (et--type-binds output-non-nil))))

    (et--or (et--replace-type-binds output-non-nil merged-non-nil-binds)
            ;; If `and' returns nil, it could be from either `cond-type' OR `output-type' being nil
            (et--and cond-type (et Nil))
            (et--and output-type (et Nil)))))

(defun et--or-return-type (cond-type checker)
  ;; The next case will only get evaluated if all previous were nil
  (let* ((nil-binds (et--type-binds (et--and cond-type (et Nil))))
         (output-type (et-with-narrow-binds nil-binds (funcall checker)))

         (output-nil (et--and output-type (et Nil)))
         ;; If `or' returns nil, then both nil binds will be true (intersect them)
         (merged-nil-binds
          (et--intersect-binds nil-binds (et--type-binds output-nil))))

    (et--or (et--replace-type-binds output-nil merged-nil-binds)
            ;; If `or' returns non-nil, it could be from either `cond-type' OR `output-type'
            (et--non-nil cond-type)
            (et--non-nil output-type))))

(et-define-checker and (&rest args)
  (cl-loop with acc-type = (et-literal t)
           for pos upfrom 1 to (length args)
           do (cl-callf et--and-return-type acc-type
                (lambda () (et-with-path (list pos) (et-check))))
           finally return acc-type))

(et-define-checker or (&rest args)
  (cl-loop with acc-type = (et Nil)
           for pos upfrom 1 to (length args)
           do (cl-callf et--or-return-type acc-type
                (lambda () (et-with-path (list pos) (et-check))))
           finally return acc-type))


;;;; if

(et-define-checker if (_cond _then &rest _else)
  (let* ((cond-type (et-check-path 1)))

    (et-warn-narrows "non-nil:\\n%s" (et Nil) ; (et-subtract cond-type (et Nil))
                     "nil:\\n%s" (et--and cond-type (et Nil)))

    (et--or (et--and-return-type cond-type (lambda () (et-check-path 2)))
            (et--or-return-type cond-type (lambda () (et-check-tail 3))))))

;; (et-assert-equal (et $a::2&True)
;;   (et--and-return-type (et $a::{1|2}&True) (lambda () (et $a::{2|3}&True))))

(et-define-checker when (_cond &rest then)
  (let* ((cond-type (et-check-path 1)))
    (et-warn-narrows "non-nil:\\n%s" (et--non-nil cond-type))
    ;; Special case for empty then block because (when cond) always returns nil
    (if (null then) (et Nil)
      (et--and-return-type cond-type (lambda () (et-check-tail 2))))))

(et-define-checker unless (_cond &rest _else)
  (let* ((cond-type (et-check-path 1)))
    (et-warn-narrows "nil:\\n%s" (et--and cond-type (et Nil)))
    ;; Special case for empty then block because (when cond) always returns nil
    (et--or-return-type cond-type (lambda () (et-check-tail 2)))))


;;; ============================================================
;;; Function types
;;;; Quoted

(et-define-checker quote (expr)
  (et-literal expr))


;;;; Arithmetic

(et-define-type-checker + [N]
  (:or Nil&{N=0} List:R<Integer>&{N=Integer} List:R<Number>&{N=Number})
  N)

(et-define-type-checker - [N]
  (:or Nil&{N=0} List:R<Integer>&{N=Integer} List:R<Number>&{N=Number})
  N)

(et-define-type-checker * [N]
  (:or Nil&{N=1} List:R<Integer>&{N=Integer} List:R<Number>&{N=Number})
  N)

(et-define-type-checker / [N]
  (:or NonNilList:R<Integer>&{N=Integer} NonNilList:R<Number>&{N=Number})
  N)

(et-define-type-checker 1+ [N] (Args Integer&{N=Integer} Number&{N=Number}) N)
(et-define-type-checker 1- [N] (Args Integer&{N=Integer} Number&{N=Number}) N)


;;;; Sequences

(et-define-type-checker cons [L R] (Args L R) Cons<L~R>)
(et-define-type-checker list [T] T T)

(et-define-type-checker car [L] (Args (:or Nil&L=Nil Cons:RR<L~Any>)) L)
(et-define-type-checker cdr [R] (Args (:or Nil&R=Nil Cons:RR<Any~R>)) R)

(et-define-type-checker nth [T] (Args Integer List:R<T>) T|Nil)

(et-define-type-checker nthcdr [T] (Args Integer List:R<T>) List<T>)

(et-define-type-checker length [] (Args String|List:R<Any>|Vector:R<Any>) Integer)

(et-define-type-checker aref [T] (Args Vector:R<T>|{String&T=Integer} Integer) T)


;;;; Predicates

(defmacro et-define-predicate (name type)
  `(et-define-type-checker ,name [T]
     (Tuple:R T)
     (:or (:and True (:binds-of (:and T ,type))) Nil)))

(et-define-predicate stringp String)
(et-define-predicate numberp Number)
(et-define-predicate integerp Integer)
;; Todo: We need a Cons:-- type which all cons types extend
(et-define-predicate consp Cons:RR<Any~Any>|Cons:WW<Any~Any>|Cons:RW<Any~Any>|Cons:WR<Any~Any>)
(et-define-predicate listp Nil|Cons:RR<Any~Any>)
(et-define-predicate null Nil)
(et-define-predicate not Nil)


;;; ============================================================
;;; Provide

(provide 'et-types)


;;; et-types.el ends here
