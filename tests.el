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

(types-assert-success (types-resolve :Number 1))
(types-assert-success (types-resolve :Number 1.1))
(types-assert-error (types-resolve :Number "1"))

(types-assert-success (types-resolve :Integer 1))
(types-assert-error (types-resolve :Integer 1.1))
(types-assert-error (types-resolve :Integer "1"))

(types-assert-success (types-resolve :String "1"))
(types-assert-error (types-resolve :String 1))

(types-assert-success (types-resolve :Symbol nil))
(types-assert-success (types-resolve :Symbol t))
(types-assert-error (types-resolve :Symbol 'a)) ; Not self-quoting
(types-assert-error (types-resolve :Symbol 1))
(types-assert-error (types-resolve :Symbol "1"))

(types-assert-success (types-resolve :Boolean t))
(types-assert-success (types-resolve :Boolean nil))
(types-assert-error (types-resolve :Boolean 'a))
(types-assert-error (types-resolve :Boolean 1))
(types-assert-error (types-resolve :Boolean "1"))


;;;; Quoted

(types-assert-success (types-resolve :Integer ''1))
(types-assert-success (types-resolve :Number ''1.1))
(types-assert-success (types-resolve :String ''"hi"))
(types-assert-success (types-resolve :Symbol ''a))
(types-assert-error (types-resolve :Integer ''1.1))
(types-assert-error (types-resolve :Integer '''1))
(types-assert-error (types-resolve :Number '''1.1))
(types-assert-error (types-resolve :String '''"hi"))
(types-assert-error (types-resolve :Symbol '''a))

(types-assert-success (types-resolve :List<Integer> ''(1 2 3)))
(types-assert-success (types-resolve :List<Symbol> ''(a b c)))
(types-assert-success (types-resolve :List<Integer> ''()))
(types-assert-error (types-resolve :List<Integer> ''(1 2 '3)))
(types-assert-error (types-resolve :List<Integer> ''(1 2 3.3)))
(types-assert-error (types-resolve :List<Integer> '''(1 2 3)))
(types-assert-error (types-resolve :List<Integer> '''()))

(types-assert-success (types-resolve :Cons<Integer~Integer> ''(1 . 2)))
(types-assert-error (types-resolve :Cons<Integer~Integer> ''(1 . 2.2)))
(types-assert-error (types-resolve :Cons<Integer~Integer> ''(1.1 . 2)))
(types-assert-success (types-resolve :Cons<Symbol~List<String>> ''(a "2" "3")))


;;;; And/or

;; And - value must satisfy all constituent types
(types-assert-success (types-resolve :And<Boolean~Boolean> t))
(types-assert-success (types-resolve :And<Boolean~Symbol~Boolean> t))
(types-assert-error   (types-resolve :And<Boolean~Integer> t))
(types-assert-error   (types-resolve :And<Boolean~Integer> 1))
(types-assert-error   (types-resolve :And<Boolean~Integer> nil))

;; Two Or types
(types-assert-success (types-resolve :Or<Boolean~Integer> t))
(types-assert-success (types-resolve :Or<Boolean~Integer> nil))
(types-assert-success (types-resolve :Or<Boolean~Integer> 1))
(types-assert-error   (types-resolve :Or<Boolean~Integer> "1"))
(types-assert-error   (types-resolve :Or<Boolean~Integer> 'a))

;; Three Or types
(types-assert-success (types-resolve :Or<Boolean~Integer~String> t))
(types-assert-success (types-resolve :Or<Boolean~Integer~String> 1))
(types-assert-success (types-resolve :Or<Boolean~Integer~String> "1"))
(types-assert-error   (types-resolve :Or<Boolean~Integer~String> 'a))

;; Nested - And inside Or
(types-assert-success (types-resolve :Or<Integer~And<Boolean~Symbol>> t))
(types-assert-success (types-resolve :Or<Integer~And<Boolean~Symbol>> 1))
(types-assert-error   (types-resolve :Or<Integer~And<Boolean~Symbol>> 'a))

;; Nested - Or inside And
(types-assert-success (types-resolve :And<Boolean~Or<Symbol~Integer>> t))
(types-assert-success (types-resolve :And<Boolean~Or<Symbol~Integer>> nil))
(types-assert-error   (types-resolve :And<Boolean~Or<Symbol~Integer>> 1))


;;;; cons

(types-assert-success (types-resolve :Cons<Integer~String> '(cons 1 "2")))
(types-assert-error (types-resolve :Cons<Integer~String> '(cons "1" 2)))
(types-assert-success (types-resolve :Cons<Integer~List<String>> '(cons 1 nil)))
(types-assert-success (types-resolve :Cons<Integer~List<String>> '(cons 1 (cons "2" nil))))

(types-assert-success (types-resolve :List<Integer> '(cons 1 (cons 2 nil))))
(types-assert-error (types-resolve :List<Integer> '(cons 1 (cons "2" nil))))
(types-assert-error (types-resolve :List<Integer> '(cons "1" (cons 2 nil))))
(types-assert-error (types-resolve :List<Integer> '(cons 1 (cons 2 t))))


;;;; list

(types-assert-success (types-resolve :Cons<Integer~List<String>> '(list 1 "2")))
(types-assert-error (types-resolve :Cons<Integer~String> '(list "1" 2)))
(types-assert-error (types-resolve :Cons<Integer~String> '(list)))

(types-assert-success (types-resolve :List<Integer> '(list 1 2 3)))
(types-assert-success (types-resolve :List<Integer> '(list 1)))
(types-assert-error (types-resolve :List<Integer> '(list 1 "2" 3)))


;;;; car

(types-assert-success (types-resolve :Integer '(car (list 1 2.2 3))))
(types-assert-error (types-resolve :Integer '(car (list 1.1 2 3))))
(types-assert-success (types-resolve :Integer '(car (cons 1 "3"))))
(types-assert-success (types-resolve :List<Integer> '(car (cons (list 1) "3"))))
(types-assert-success (types-resolve :Cons<Integer~Any> '(car (cons (list 1) "3"))))
(types-assert-success (types-resolve :Integer '(car (car (cons (list 1) "3")))))
(types-assert-error (types-resolve :Integer '(car (car (cons (list 1.1) "3")))))


;;;; cdr

(types-assert-success (types-resolve :List<Number> '(cdr (list 1 2.2 3))))
(types-assert-success (types-resolve :List<Integer> '(cdr (list 1.1 2 3))))
(types-assert-error (types-resolve :List<Integer> '(car (list 1 2.2 3))))

(types-assert-success (types-resolve :Integer '(cdr (cons "1" 2))))
(types-assert-error (types-resolve :Integer '(cdr (cons 1 "2"))))

(types-assert-success (types-resolve :List<Integer> '(cdr (cons "1" (list 2)))))
(types-assert-success (types-resolve :Cons<Integer~Any> '(cdr (cons "1" (list 2)))))
(types-assert-success (types-resolve :Cons<Integer~Boolean> '(cdr (cons "1" (list 2)))))
(types-assert-error (types-resolve :Cons<Integer~Boolean> '(cdr (cons "1" (list 2 3)))))
(types-assert-success (types-resolve :Integer '(car (cdr (cons "1" (list 2))))))

(types-assert-success (types-resolve :Boolean '(cdr (cdr (cdr (list 1 2 3))))))
(types-assert-error (types-resolve :Boolean '(cdr (cdr (list 1 2 3)))))


;;; ============================================================
;;; Blocks

(types-block
 (let* ((a :Integer 4)
        (b :List<Integer> (list (+ 1 2))))
   ;; (setq a 3.3)
   (let* ((a :List<Integer> (cons (1+ a) nil)))
     (dolist (n :Integer a)
       (setq b (cons (+ n 1) b)))
     (+ 2 1))))

;; (my-macro 1 (2 3) 3)


;; ============================================================
;; Provide

(provide 'tests)
;;; tests.el ends here
