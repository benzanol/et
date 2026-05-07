;;; tests.el --- Tests for types.el                  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Adam Tillou

;; Author: Adam Tillou <benzanol@nixos>
;; Keywords:

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

;; Tests for types.el


;;; Code:

(eval-and-compile
  (add-to-list 'load-path "~/.emacs.d/my-packages/typesystem")
  (require 'et)
  (require 'et-types))


;;; ============================================================
;;; Parsing

(et-assert-equal (et Cons:RR<1~@abc>)
  (et-dt 'Cons:RR (et-literal 1) (et-literal 'abc)))


;;; ============================================================
;;; Typesystem
;;;; Subtype

(et-assert-true (et-subtype? (et Integer) (et Number)))
(et-assert-true (et-subtype? (et Integer) (et Any)))

(et-assert-true (et-subtype? (et Cons:RR<Integer~Integer>) (et Cons:RR<Number~Number>)))
(et-assert-nil (et-subtype? (et Cons:RR<Number~Number>) (et Cons:RR<Integer~Integer>)))

(et-assert-true (et-subtype? (et Cons:WW<Number~Number>) (et Cons:WW<Integer~Integer>)))
(et-assert-nil (et-subtype? (et Cons:WW<Integer~Integer>) (et Cons:WW<Number~Number>)))

(et-assert-true (et-subtype? (et Cons:WR<Number~Integer>) (et Cons:WR<Integer~Number>)))
(et-assert-true (et-subtype? (et Cons:RW<Integer~Number>) (et Cons:RW<Number~Integer>)))

(et-assert-true
 (et-subtype? (et List:R<Integer>)
              (et Nil|Cons:RR<Number~List:R<Integer>>)))

;; (et-assert-true
;;  (et-subtype? (et Nil|Cons:RR<Number~List:R<Integer>>)
;;               (et List:R<Number>)))


;;; ============================================================
;;; Expressions
;;;; Primitives

(et-assert-success (et-root-resolve 'Number 1))
(et-assert-success (et-root-resolve 'Number 1.1))
(et-assert-error (et-root-resolve 'Number "1"))

(et-assert-success (et-root-resolve 'Integer 1))
(et-assert-error (et-root-resolve 'Integer 1.1))
(et-assert-error (et-root-resolve 'Integer "1"))

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
(et-assert-error (et-root-resolve 'Boolean "1"))


;;;; Quoted

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
(et-assert-success (et-root-resolve 'Cons<Symbol~List<String>> ''(a "2" "3")))


;;;; Arith

(defmacro et-repeat (var repls &rest body)
  (declare (indent 2))
  (cl-assert (vectorp repls))
  (cl-loop for repl across repls
           nconc (cl-subst repl var body) into all
           finally return (cons #'progn all)))

(et-assert-equal (et Integer) (et-root-check-call + Integer Integer 1 2 3))

;; (et-repeat op [+ *]
;;   (et-assert-equal (et Integer) (et-root-check-call op Integer Integer 1 2 3))
;;   (et-assert-equal (et Number) (et-root-check-call op Integer Integer 1 2.1 3))
;;   (et-assert-equal (et Integer) (et-root-check-call op 1))
;;   (et-assert-equal (et 0) (et-root-check-call op)))


;;;; and/or

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
(et-assert-error   (et-root-resolve 'Boolean&{Symbol|Integer} 1))


;;;; cons

(et-assert-success (et-root-resolve 'Cons:RR<Integer~String> '(cons 1 "2")))
(et-assert-error (et-root-resolve 'Cons:RR<Integer~String> '(cons "1" 2)))
(et-assert-success (et-root-resolve 'Cons:RR<Integer~List:R<String>> '(cons 1 nil)))
(et-assert-success (et-root-resolve 'Cons:RR<Integer~List:R<String>> '(cons 1 (cons "2" nil))))

(et-assert-success (et-root-resolve 'List:R<Integer> '(cons 1 (cons 2 nil))))
(et-assert-error (et-root-resolve 'List:R<Integer> '(cons 1 (cons "2" nil))))
(et-assert-error (et-root-resolve 'List:R<Integer> '(cons "1" (cons 2 nil))))
(et-assert-error (et-root-resolve 'List:R<Integer> '(cons 1 (cons 2 t))))


;;;; list

(et-assert-success (et-root-resolve 'Cons:RR<Integer~List:R<String>> '(list 1 "2")))
(et-assert-error (et-root-resolve 'Cons:RR<Integer~String> '(list "1" 2)))
(et-assert-error (et-root-resolve 'Cons:RR<Integer~String> '(list)))

(et-assert-success (et-root-resolve 'List:R<Integer> '(list 1 2 3)))
(et-assert-success (et-root-resolve 'List:R<Integer> '(list 1)))
(et-assert-error (et-root-resolve 'List:R<Integer> '(list 1 "2" 3)))


;;;; car

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
 (et-root-check-call car List:R<Integer>|Cons:RR<String~Nil>|String))


;;;; cdr

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

(et-assert-equal (et List:R<Integer>)
  (et-root-check-call cdr List:R<Integer>))

(et-assert-equal (et List<Integer>|String)
  (et-root-check-call cdr List<Integer>|Cons<Nil~String>))

(et-assert-error
 (et-root-check-call car List<Integer>|Cons<String~nil>|String))


;;;; List ops

(et-assert-equal (et Number|String|Nil)
  (et-root-check-call nth Integer Cons<Number~List<String>>))

(et-assert-equal (et List<Number|String>)
  (et-root-check-call nthcdr Integer Cons<Number~List<String>>))

(et-assert-equal (et List<:never>)
  (et-root-check-call nthcdr Integer Nil))

(et-assert-equal (et Integer)
  (et-root-check-call length Vector<Number>|List<String>))

(et-assert-error
 (et-root-check-call length Vector<Number>|List<String>|Number))

(et-assert-equal (et Integer)
  (et-root-check-call aref String Integer))

(et-assert-equal (et Symbol|Integer)
  (et-root-check-call aref (:or Vector<Symbol> String) Integer))

(et-assert-error
 (et-root-check-call aref (:or Vector<Symbol> String List:R<Any>) Integer))


;;; ============================================================
;;; Blocks

;; Setting type binds to an incompatible type returns never
(cl-assert
 (equal
  (let ((vs (cons 'a (et--or (et-dt 'Integer) (et-dt 'String)))))
    (et-with-binds (list vs)
      (et-with-narrow-binds (list (cons vs (et-dt 'Integer)))
        (et--replace-type-binds (et-literal t) (list (cons vs (et-dt 'String)))))))
  (et-never)))

;; A few hard type narrowing cases
;; (et-root-block
;;  (let* ((b String|Integer|Nil 4))
;;    (if (and b (or (null b) (stringp b)))
;;        (:assert-subtype b String)
;;      (:assert-subtype b (et-or (et-nil) (et-dt 'Integer)))
;;      (:assert-error (:assert-subtype b (et-or (et-nil) (et-dt 'String)))))
;;    (if (not b)
;;        (:assert-subtype b (et-nil))
;;      (:assert-subtype b (et-or (et-dt String) (et-dt 'Integer))))
;;    (if (not (not b))
;;        (:assert-subtype b (et-or (et-dt String) (et-dt 'Integer)))
;;      (:assert-subtype b (et-nil)))
;;    (if (not (not (not (not b))))
;;        (:assert-subtype b (et-or (et-dt String) (et-dt 'Integer)))
;;      (:assert-subtype b (et-nil)))
;;    (if (not (not (not (stringp b))))
;;        (:assert-subtype b (et-or (et-nil) (et-dt 'Integer)))
;;      (:assert-subtype b (et-dt String)))))

;; Test narrowing across variables
;; (et-root-block
;;  (let* ((a String|Integer|Nil 4)
;;         (b a))
;;    (when (stringp b)
;;      (:assert-subtype a (et-dt String)))
;;    (:assert-error (:assert-subtype a (et-dt String)))))


;; ============================================================
;; Provide

(provide 'tests)
;;; tests.el ends here
