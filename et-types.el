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

(require 'et-check)


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
           for type = (or (et--get-symbol-type var)
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

(et-test
 (et-assert-success (et-root-resolve 'Integer ''1))
 (et-assert-success (et-root-resolve 'Number ''1.1))
 (et-assert-success (et-root-resolve 'String ''"hi"))
 (et-assert-success (et-root-resolve 'Symbol ''a))
 (et-assert-error (et-root-resolve 'Integer ''1.1))
 (et-assert-error (et-root-resolve 'Integer '''1))
 (et-assert-error (et-root-resolve 'Number '''1.1))
 (et-assert-error (et-root-resolve 'String '''"hi"))
 (et-assert-error (et-root-resolve 'Symbol '''a))

 (et-assert-success (et-root-resolve 'Cons<Any~Any> ''(1 2 3)))
 (et-assert-success (et-root-resolve 'List<Symbol> ''(a b c)))
 (et-assert-success (et-root-resolve 'List<Integer> ''()))
 (et-assert-error (et-root-resolve 'List<Integer> ''(1 2 '3)))
 (et-assert-error (et-root-resolve 'List<Integer> ''(1 2 3.3)))
 (et-assert-error (et-root-resolve 'List<Integer> '''(1 2 3)))
 (et-assert-error (et-root-resolve 'List<Integer> '''()))

 (et-assert-success (et-root-resolve 'Cons<Integer~Integer> ''(1 . 2)))
 (et-assert-error (et-root-resolve 'Cons<Integer~Integer> ''(1 . 2.2)))
 (et-assert-error (et-root-resolve 'Cons<Integer~Integer> ''(1.1 . 2)))
 (et-assert-success (et-root-resolve 'Cons<Symbol~List<String>> ''(a "2" "3"))))


;;;; Arithmetic

(et-define-type-checker (+ -) [N]
  (:or Nil&{N=0} List:R<Integer>&{N=Integer} List:R<Number>&{N=Number})
  N)

(et-define-type-checker * [N]
  (:or Nil&{N=1} List:R<Integer>&{N=Integer} List:R<Number>&{N=Number})
  N)

(et-define-type-checker / [N]
  (:or NonNilList:R<Integer>&{N=Integer} NonNilList:R<Number>&{N=Number})
  N)

(et-define-type-checker (1+ 1-) [N] (Args Integer&{N=Integer} Number&{N=Number}) N)


(et-test
 op [+ - * /]
 (et-assert-equal (et Integer) (et-root-check-call op Integer Integer 1 2 3))
 (et-assert-equal (et Integer) (et-root-check-call op Integer Integer 1 2 3))
 (et-assert-equal (et Number) (et-root-check-call op Integer Integer 1 2.1 3))
 (et-assert-equal (et Integer) (et-root-check-call op 1)))

(et-test
 (et-assert-equal (et 0) (et-root-check-call +))
 (et-assert-equal (et 0) (et-root-check-call -))
 (et-assert-equal (et 1) (et-root-check-call *))
 (et-assert-error (et-root-check-call /)))


;;;; Sequences
;;;;; cons

(et-define-type-checker cons [L R] (Args L R) Cons<L~R>)

(et-test
 (et-assert-success (et-root-resolve 'Cons:RR<Integer~String> '(cons 1 "2")))
 (et-assert-error (et-root-resolve 'Cons:RR<Integer~String> '(cons "1" 2)))
 (et-assert-success (et-root-resolve 'Cons:RR<Integer~List:R<String>> '(cons 1 nil)))
 (et-assert-success (et-root-resolve 'Cons:RR<Integer~List:R<String>> '(cons 1 (cons "2" nil))))

 (et-assert-success (et-root-resolve 'List:R<Integer> '(cons 1 (cons 2 nil))))
 (et-assert-error (et-root-resolve 'List:R<Integer> '(cons 1 (cons "2" nil))))
 (et-assert-error (et-root-resolve 'List:R<Integer> '(cons "1" (cons 2 nil))))
 (et-assert-error (et-root-resolve 'List:R<Integer> '(cons 1 (cons 2 t)))))


;;;;; list

(et-define-type-checker list [T] T T)

(et-test
 (et-assert-success (et-root-resolve 'Cons:RR<Integer~List:R<String>> '(list 1 "2")))
 (et-assert-error (et-root-resolve 'Cons:RR<Integer~String> '(list "1" 2)))
 (et-assert-error (et-root-resolve 'Cons:RR<Integer~String> '(list)))

 (et-assert-success (et-root-resolve 'List:R<Integer> '(list 1 2 3)))
 (et-assert-success (et-root-resolve 'List:R<Integer> '(list 1)))
 (et-assert-error (et-root-resolve 'List:R<Integer> '(list 1 "2" 3))))


;;;;; car

(et-define-type-checker car [L] (Args (:or Nil&L=Nil Cons:RR<L~Any>)) L)

(et-test
 (et-assert-success (et-root-resolve 'Integer '(car (list 1 2.2 3))))
 (et-assert-error (et-root-resolve 'Integer '(car (list 1.1 2 3))))
 (et-assert-success (et-root-resolve 'Integer '(car (cons 1 "3"))))
 (et-assert-success (et-root-resolve 'List:R<Integer> '(car (cons (list 1) "3"))))
 (et-assert-success (et-root-resolve 'Cons:RR<Integer~Any> '(car (cons (list 1) "3"))))
 (et-assert-success (et-root-resolve 'Integer '(car (car (cons (list 1) "3")))))
 (et-assert-error (et-root-resolve 'Integer '(car (car (cons (list 1.1) "3")))))

 (et-assert-success (et-root-check-call cdr :never))
 (et-assert-success (et-root-check-call cdr Nil))
 (et-assert-success (et-root-check-call cdr Nil|Cons:RR<Integer~String>))
 (et-assert-error (et-root-check-call cdr Nil|Cons:RR<Integer~String>|String))
 (et-assert-error (et-root-check-call cdr :any))

 (et-assert-success (et-root-check-call car :never))
 (et-assert-success (et-root-check-call car Nil))
 (et-assert-success (et-root-check-call car Nil|Cons:RR<Integer~String>))
 (et-assert-error (et-root-check-call car Nil|Cons:RR<Integer~String>|String))
 (et-assert-error (et-root-check-call car Any))

 (et-assert-equal (et Nil|Integer)
   (et-root-check-call car List:R<Integer>))

 (et-assert-equal (et Nil|Integer|String)
   (et-root-check-call car List:R<Integer>|Cons:RR<String~Nil>))

 (et-assert-error
     (et-root-check-call car List:R<Integer>|Cons:RR<String~Nil>|String)))


;;;;; cdr

(et-define-type-checker cdr [R] (Args (:or Nil&R=Nil Cons:RR<Any~R>)) R)

(et-test
 (et-assert-success (et-root-resolve 'List:R<Number> '(cdr (list 1 2.2 3))))
 (et-assert-success (et-root-resolve 'List:R<Integer> '(cdr (list 1.1 2 3))))
 (et-assert-error (et-root-resolve 'List:R<Integer> '(car (list 1 2.2 3))))

 (et-assert-success (et-root-resolve 'Integer '(cdr (cons "1" 2))))
 (et-assert-error (et-root-resolve 'Integer '(cdr (cons 1 "2"))))

 (et-assert-success (et-root-resolve 'List:R<Integer> '(cdr (cons "1" (list 2)))))
 (et-assert-success (et-root-resolve 'Cons:RR<Integer~Any> '(cdr (cons "1" (list 2)))))
 (et-assert-success (et-root-resolve 'Cons:RR<Integer~Boolean> '(cdr (cons "1" (list 2)))))
 (et-assert-error (et-root-resolve 'Cons:RR<Integer~Boolean> '(cdr (cons "1" (list 2 3)))))
 (et-assert-success (et-root-resolve 'Integer '(car (cdr (cons "1" (list 2))))))

 (et-assert-success (et-root-resolve 'Boolean '(cdr (cdr (cdr (list 1 2 3))))))
 (et-assert-error (et-root-resolve 'Boolean '(cdr (cdr (list 1 2 3)))))

 (et-assert-equal (et List:R<Integer>) (et-root-check-call cdr List:R<Integer>))
 (et-assert-equal (et List<Integer>|String) (et-root-check-call cdr List<Integer>|Cons<Nil~String>))
 (et-assert-error (et-root-check-call car List<Integer>|Cons<String~nil>|String)))


;;;;; nth/nthcdr

(et-define-type-checker nth [T] (Args Integer List:R<T>) T|Nil)

(et-define-type-checker nthcdr [T] (Args Integer List:R<T>) List<T>)

(et-test
 (et-assert-equal (et Number|String|Nil) (et-root-check-call nth Integer Cons<Number~List<String>>))

 (et-assert-equal (et List<Number|String>) (et-root-check-call nthcdr Integer Cons<Number~List<String>>))
 (et-assert-equal (et List<:never>) (et-root-check-call nthcdr Integer Nil)))


;;;;; length

(et-define-type-checker length [] (Args String|List:R<Any>|Vector:R<Any>) Integer)

(et-test
 (et-assert-equal (et Integer) (et-root-check-call length Vector<Number>|List<String>))
 (et-assert-error (et-root-check-call length Vector<Number>|List<String>|Number)))


;;;;; aref

(et-define-type-checker aref [T] (Args Vector:R<T>|{String&T=Integer} Integer) T)

(et-test
 (et-assert-equal (et Integer) (et-root-check-call aref String Integer))
 (et-assert-equal (et Symbol|Integer) (et-root-check-call aref (:or Vector<Symbol> String) Integer))
 (et-assert-error (et-root-check-call aref (:or Vector<Symbol> String List:R<Any>) Integer)))


;;;; Predicates

(defmacro et-define-predicate (name type)
  `(et-define-type-checker ,name [T]
     (Tuple:R T)
     (:or (:and True (:bindsof (:and T ,type))) Nil)))

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
