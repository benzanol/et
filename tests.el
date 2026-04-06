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

(eval-and-compile
  (add-to-list 'load-path "~/.emacs.d/my-packages/typesystem")
  (require 'types))


;;; Code:
;;; ============================================================
;;; Parse

(cl-assert (equal (types-parse :Boolean) '(:Boolean)))
(cl-assert (equal (types-parse :And<Boolean>) '(:And (:Boolean))))
(cl-assert (equal (types-parse :And<Boolean~Integer>) '(:And (:Boolean) (:Integer))))
(cl-assert (equal (types-parse :Or<Boolean~Integer>) '(:Or (:Boolean) (:Integer))))
(cl-assert (equal (types-parse :Or<Boolean~Integer~String>) '(:Or (:Boolean) (:Integer) (:String))))
(cl-assert (equal (types-parse :And<Or<Boolean~Integer>~String>)
                  '(:And (:Or (:Boolean) (:Integer)) (:String))))
(cl-assert (equal (types-parse :Or<And<Boolean~Integer>~And<String~Boolean>>)
                  '(:Or (:And (:Boolean) (:Integer)) (:And (:String) (:Boolean)))))
(cl-assert (equal (types-parse :Or<And<Or<Boolean~Integer>~String>~Boolean>)
                  '(:Or (:And (:Or (:Boolean) (:Integer)) (:String)) (:Boolean))))


;;; ============================================================
;;; Types
;;;; Primitives

(types-assert-success (types-root-resolve :Number 1))
(types-assert-success (types-root-resolve :Number 1.1))
(types-assert-error (types-root-resolve :Number "1"))

(types-assert-success (types-root-resolve :Integer 1))
(types-assert-error (types-root-resolve :Integer 1.1))
(types-assert-error (types-root-resolve :Integer "1"))

(types-assert-success (types-root-resolve :String "1"))
(types-assert-error (types-root-resolve :String 1))

(types-assert-success (types-root-resolve :Symbol nil))
(types-assert-success (types-root-resolve :Symbol t))
(types-assert-error (types-root-resolve :Symbol 'a)) ; Not self-quoting
(types-assert-error (types-root-resolve :Symbol 1))
(types-assert-error (types-root-resolve :Symbol "1"))

(types-assert-success (types-root-resolve :Boolean t))
(types-assert-success (types-root-resolve :Boolean nil))
(types-assert-error (types-root-resolve :Boolean 'a))
(types-assert-error (types-root-resolve :Boolean 1))
(types-assert-error (types-root-resolve :Boolean "1"))


;;;; Quoted

(types-assert-success (types-root-resolve :Integer ''1))
(types-assert-success (types-root-resolve :Number ''1.1))
(types-assert-success (types-root-resolve :String ''"hi"))
(types-assert-success (types-root-resolve :Symbol ''a))
(types-assert-error (types-root-resolve :Integer ''1.1))
(types-assert-error (types-root-resolve :Integer '''1))
(types-assert-error (types-root-resolve :Number '''1.1))
(types-assert-error (types-root-resolve :String '''"hi"))
(types-assert-error (types-root-resolve :Symbol '''a))

(types-assert-success (types-root-resolve :List<Integer> ''(1 2 3)))
(types-assert-success (types-root-resolve :List<Symbol> ''(a b c)))
(types-assert-success (types-root-resolve :List<Integer> ''()))
(types-assert-error (types-root-resolve :List<Integer> ''(1 2 '3)))
(types-assert-error (types-root-resolve :List<Integer> ''(1 2 3.3)))
(types-assert-error (types-root-resolve :List<Integer> '''(1 2 3)))
(types-assert-error (types-root-resolve :List<Integer> '''()))

(types-assert-success (types-root-resolve :Cons<Integer~Integer> ''(1 . 2)))
(types-assert-error (types-root-resolve :Cons<Integer~Integer> ''(1 . 2.2)))
(types-assert-error (types-root-resolve :Cons<Integer~Integer> ''(1.1 . 2)))
(types-assert-success (types-root-resolve :Cons<Symbol~List<String>> ''(a "2" "3")))


;;;; And/or

;; And - value must satisfy all constituent types
(types-assert-success (types-root-resolve :Boolean&Boolean t))
(types-assert-success (types-root-resolve :Boolean&Symbol&Boolean t))
(types-assert-error   (types-root-resolve :Boolean&Integer t))
(types-assert-error   (types-root-resolve :Boolean&Integer 1))
(types-assert-error   (types-root-resolve :Boolean&Integer nil))

;; Two Or types
(types-assert-success (types-root-resolve :Boolean|Integer t))
(types-assert-success (types-root-resolve :Boolean|Integer nil))
(types-assert-success (types-root-resolve :Boolean|Integer 1))
(types-assert-error   (types-root-resolve :Boolean|Integer "1"))
(types-assert-error   (types-root-resolve :Boolean|Integer 'a))

;; Three Or types
(types-assert-success (types-root-resolve :Boolean|Integer|String t))
(types-assert-success (types-root-resolve :Boolean|Integer|String 1))
(types-assert-success (types-root-resolve :Boolean|Integer|String "1"))
(types-assert-error   (types-root-resolve :Boolean|Integer|String 'a))

;; Nested - And inside Or
(types-assert-success (types-root-resolve :Integer|Boolean&Symbol t))
(types-assert-success (types-root-resolve :Integer|Boolean&Symbol 1))
(types-assert-error   (types-root-resolve :Integer|Boolean&Symbol 'a))

;; Nested - Or inside And
(types-assert-success (types-root-resolve :Boolean&{Symbol|Integer} t))
(types-assert-success (types-root-resolve :Boolean&{Symbol|Integer} nil))
(types-assert-error   (types-root-resolve :Boolean&{Symbol|Integer} 1))


;;;; cons

(types-assert-success (types-root-resolve :Cons<Integer~String> '(cons 1 "2")))
(types-assert-error (types-root-resolve :Cons<Integer~String> '(cons "1" 2)))
(types-assert-success (types-root-resolve :Cons<Integer~List<String>> '(cons 1 nil)))
(types-assert-success (types-root-resolve :Cons<Integer~List<String>> '(cons 1 (cons "2" nil))))

(types-assert-success (types-root-resolve :List<Integer> '(cons 1 (cons 2 nil))))
(types-assert-error (types-root-resolve :List<Integer> '(cons 1 (cons "2" nil))))
(types-assert-error (types-root-resolve :List<Integer> '(cons "1" (cons 2 nil))))
(types-assert-error (types-root-resolve :List<Integer> '(cons 1 (cons 2 t))))


;;;; list

(types-assert-success (types-root-resolve :Cons<Integer~List<String>> '(list 1 "2")))
(types-assert-error (types-root-resolve :Cons<Integer~String> '(list "1" 2)))
(types-assert-error (types-root-resolve :Cons<Integer~String> '(list)))

(types-assert-success (types-root-resolve :List<Integer> '(list 1 2 3)))
(types-assert-success (types-root-resolve :List<Integer> '(list 1)))
(types-assert-error (types-root-resolve :List<Integer> '(list 1 "2" 3)))


;;;; car

(types-assert-success (types-root-resolve :Integer '(car (list 1 2.2 3))))
(types-assert-error (types-root-resolve :Integer '(car (list 1.1 2 3))))
(types-assert-success (types-root-resolve :Integer '(car (cons 1 "3"))))
(types-assert-success (types-root-resolve :List<Integer> '(car (cons (list 1) "3"))))
(types-assert-success (types-root-resolve :Cons<Integer~Any> '(car (cons (list 1) "3"))))
(types-assert-success (types-root-resolve :Integer '(car (car (cons (list 1) "3")))))
(types-assert-error (types-root-resolve :Integer '(car (car (cons (list 1.1) "3")))))


;;;; cdr

(types-assert-success (types-root-resolve :List<Number> '(cdr (list 1 2.2 3))))
(types-assert-success (types-root-resolve :List<Integer> '(cdr (list 1.1 2 3))))
(types-assert-error (types-root-resolve :List<Integer> '(car (list 1 2.2 3))))

(types-assert-success (types-root-resolve :Integer '(cdr (cons "1" 2))))
(types-assert-error (types-root-resolve :Integer '(cdr (cons 1 "2"))))

(types-assert-success (types-root-resolve :List<Integer> '(cdr (cons "1" (list 2)))))
(types-assert-success (types-root-resolve :Cons<Integer~Any> '(cdr (cons "1" (list 2)))))
(types-assert-success (types-root-resolve :Cons<Integer~Boolean> '(cdr (cons "1" (list 2)))))
(types-assert-error (types-root-resolve :Cons<Integer~Boolean> '(cdr (cons "1" (list 2 3)))))
(types-assert-success (types-root-resolve :Integer '(car (cdr (cons "1" (list 2))))))

(types-assert-success (types-root-resolve :Boolean '(cdr (cdr (cdr (list 1 2 3))))))
(types-assert-error (types-root-resolve :Boolean '(cdr (cdr (list 1 2 3)))))


;;; ============================================================
;;; Blocks

(types-root-block
 (let* ((a :Any t)
        (b :String|Number 4)
        (c (and (numberp a) (stringp b))))
   (if (and (numberp a) (stringp b))
       (+ 1 a)
     (let* ((test a))
       test))))

(types-and '(:Logical ((:Literal nil) (:Bind a (:Integer)))
                      ((:Literal t) (:Bind a (:String)))
                      )
           '(:Literal nil))
(types-exclude '(:Logical ((:Literal nil) (:Bind a (:Integer)))
                          ((:Literal t) (:Bind a (:String)))
                          )
               '(:Literal nil))
(types--incompatible?)


;; ============================================================
;; Provide

(provide 'tests)
;;; tests.el ends here
