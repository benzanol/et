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
  (require 'et))


;;; Code:
;;; ============================================================
;;; Parse

(cl-assert (equal (et-parse :Boolean) '(:Boolean)))
(cl-assert (equal (et-parse :And<Boolean>) '(:And (:Boolean))))
(cl-assert (equal (et-parse :And<Boolean~Integer>) '(:And (:Boolean) (:Integer))))
(cl-assert (equal (et-parse :Or<Boolean~Integer>) '(:Or (:Boolean) (:Integer))))
(cl-assert (equal (et-parse :Or<Boolean~Integer~String>) '(:Or (:Boolean) (:Integer) (:String))))
(cl-assert (equal (et-parse :And<Or<Boolean~Integer>~String>)
                  '(:And (:Or (:Boolean) (:Integer)) (:String))))
(cl-assert (equal (et-parse :Or<And<Boolean~Integer>~And<String~Boolean>>)
                  '(:Or (:And (:Boolean) (:Integer)) (:And (:String) (:Boolean)))))
(cl-assert (equal (et-parse :Or<And<Or<Boolean~Integer>~String>~Boolean>)
                  '(:Or (:And (:Or (:Boolean) (:Integer)) (:String)) (:Boolean))))


;;; ============================================================
;;; Types
;;;; Primitives

(et-assert-success (et-root-resolve :Number 1))
(et-assert-success (et-root-resolve :Number 1.1))
(et-assert-error (et-root-resolve :Number "1"))

(et-assert-success (et-root-resolve :Integer 1))
(et-assert-error (et-root-resolve :Integer 1.1))
(et-assert-error (et-root-resolve :Integer "1"))

(et-assert-success (et-root-resolve :String "1"))
(et-assert-error (et-root-resolve :String 1))

(et-assert-success (et-root-resolve :Symbol nil))
(et-assert-success (et-root-resolve :Symbol t))
(et-assert-error (et-root-resolve :Symbol 'a)) ; Not self-quoting
(et-assert-error (et-root-resolve :Symbol 1))
(et-assert-error (et-root-resolve :Symbol "1"))

(et-assert-success (et-root-resolve :Boolean t))
(et-assert-success (et-root-resolve :Boolean nil))
(et-assert-error (et-root-resolve :Boolean 'a))
(et-assert-error (et-root-resolve :Boolean 1))
(et-assert-error (et-root-resolve :Boolean "1"))


;;;; Quoted

(et-assert-success (et-root-resolve :Integer ''1))
(et-assert-success (et-root-resolve :Number ''1.1))
(et-assert-success (et-root-resolve :String ''"hi"))
(et-assert-success (et-root-resolve :Symbol ''a))
(et-assert-error (et-root-resolve :Integer ''1.1))
(et-assert-error (et-root-resolve :Integer '''1))
(et-assert-error (et-root-resolve :Number '''1.1))
(et-assert-error (et-root-resolve :String '''"hi"))
(et-assert-error (et-root-resolve :Symbol '''a))

(et-assert-success (et-root-resolve :List<Integer> ''(1 2 3)))
(et-assert-success (et-root-resolve :List<Symbol> ''(a b c)))
(et-assert-success (et-root-resolve :List<Integer> ''()))
(et-assert-error (et-root-resolve :List<Integer> ''(1 2 '3)))
(et-assert-error (et-root-resolve :List<Integer> ''(1 2 3.3)))
(et-assert-error (et-root-resolve :List<Integer> '''(1 2 3)))
(et-assert-error (et-root-resolve :List<Integer> '''()))

(et-assert-success (et-root-resolve :Cons<Integer~Integer> ''(1 . 2)))
(et-assert-error (et-root-resolve :Cons<Integer~Integer> ''(1 . 2.2)))
(et-assert-error (et-root-resolve :Cons<Integer~Integer> ''(1.1 . 2)))
(et-assert-success (et-root-resolve :Cons<Symbol~List<String>> ''(a "2" "3")))


;;;; And/or

;; And - value must satisfy all constituent types
(et-assert-success (et-root-resolve :Boolean&Boolean t))
(et-assert-success (et-root-resolve :Boolean&Symbol&Boolean t))
(et-assert-error   (et-root-resolve :Boolean&Integer t))
(et-assert-error   (et-root-resolve :Boolean&Integer 1))
(et-assert-error   (et-root-resolve :Boolean&Integer nil))

;; Two Or types
(et-assert-success (et-root-resolve :Boolean|Integer t))
(et-assert-success (et-root-resolve :Boolean|Integer nil))
(et-assert-success (et-root-resolve :Boolean|Integer 1))
(et-assert-error   (et-root-resolve :Boolean|Integer "1"))
(et-assert-error   (et-root-resolve :Boolean|Integer 'a))

;; Three Or types
(et-assert-success (et-root-resolve :Boolean|Integer|String t))
(et-assert-success (et-root-resolve :Boolean|Integer|String 1))
(et-assert-success (et-root-resolve :Boolean|Integer|String "1"))
(et-assert-error   (et-root-resolve :Boolean|Integer|String 'a))

;; Nested - And inside Or
(et-assert-success (et-root-resolve :Integer|Boolean&Symbol t))
(et-assert-success (et-root-resolve :Integer|Boolean&Symbol 1))
(et-assert-error   (et-root-resolve :Integer|Boolean&Symbol 'a))

;; Nested - Or inside And
(et-assert-success (et-root-resolve :Boolean&{Symbol|Integer} t))
(et-assert-success (et-root-resolve :Boolean&{Symbol|Integer} nil))
(et-assert-error   (et-root-resolve :Boolean&{Symbol|Integer} 1))


;;;; cons

(et-assert-success (et-root-resolve :Cons<Integer~String> '(cons 1 "2")))
(et-assert-error (et-root-resolve :Cons<Integer~String> '(cons "1" 2)))
(et-assert-success (et-root-resolve :Cons<Integer~List<String>> '(cons 1 nil)))
(et-assert-success (et-root-resolve :Cons<Integer~List<String>> '(cons 1 (cons "2" nil))))

(et-assert-success (et-root-resolve :List<Integer> '(cons 1 (cons 2 nil))))
(et-assert-error (et-root-resolve :List<Integer> '(cons 1 (cons "2" nil))))
(et-assert-error (et-root-resolve :List<Integer> '(cons "1" (cons 2 nil))))
(et-assert-error (et-root-resolve :List<Integer> '(cons 1 (cons 2 t))))


;;;; list

(et-assert-success (et-root-resolve :Cons<Integer~List<String>> '(list 1 "2")))
(et-assert-error (et-root-resolve :Cons<Integer~String> '(list "1" 2)))
(et-assert-error (et-root-resolve :Cons<Integer~String> '(list)))

(et-assert-success (et-root-resolve :List<Integer> '(list 1 2 3)))
(et-assert-success (et-root-resolve :List<Integer> '(list 1)))
(et-assert-error (et-root-resolve :List<Integer> '(list 1 "2" 3)))


;;;; car

(et-assert-success (et-root-resolve :Integer '(car (list 1 2.2 3))))
(et-assert-error (et-root-resolve :Integer '(car (list 1.1 2 3))))
(et-assert-success (et-root-resolve :Integer '(car (cons 1 "3"))))
(et-assert-success (et-root-resolve :List<Integer> '(car (cons (list 1) "3"))))
(et-assert-success (et-root-resolve :Cons<Integer~Any> '(car (cons (list 1) "3"))))
(et-assert-success (et-root-resolve :Integer '(car (car (cons (list 1) "3")))))
(et-assert-error (et-root-resolve :Integer '(car (car (cons (list 1.1) "3")))))


;;;; cdr

(et-assert-success (et-root-resolve :List<Number> '(cdr (list 1 2.2 3))))
(et-assert-success (et-root-resolve :List<Integer> '(cdr (list 1.1 2 3))))
(et-assert-error (et-root-resolve :List<Integer> '(car (list 1 2.2 3))))

(et-assert-success (et-root-resolve :Integer '(cdr (cons "1" 2))))
(et-assert-error (et-root-resolve :Integer '(cdr (cons 1 "2"))))

(et-assert-success (et-root-resolve :List<Integer> '(cdr (cons "1" (list 2)))))
(et-assert-success (et-root-resolve :Cons<Integer~Any> '(cdr (cons "1" (list 2)))))
(et-assert-success (et-root-resolve :Cons<Integer~Boolean> '(cdr (cons "1" (list 2)))))
(et-assert-error (et-root-resolve :Cons<Integer~Boolean> '(cdr (cons "1" (list 2 3)))))
(et-assert-success (et-root-resolve :Integer '(car (cdr (cons "1" (list 2))))))

(et-assert-success (et-root-resolve :Boolean '(cdr (cdr (cdr (list 1 2 3))))))
(et-assert-error (et-root-resolve :Boolean '(cdr (cdr (list 1 2 3)))))


;;; ============================================================
;;; Blocks

;; Setting type binds to an incompatible type returns never
(cl-assert
 (equal
  (let ((vs (cons 'a (et-or (et-dt :Integer) (et-dt :String)))))
    (et-with-binds (list vs)
      (et-with-narrow-binds (list (cons vs (et-dt :Integer)))
        (et--replace-type-binds (et-literal t) (list (cons vs (et-dt :String)))))))
  (et-never)))

;; A few hard type narrowing cases
(et-root-block
 (let* ((b :String|Integer|nil 4))
   (if (and b (or (null b) (stringp b)))
       (:assert-subtype b (et-dt :String))
     (:assert-subtype b (et-or (et-nil) (et-dt :Integer)))
     (:assert-error (:assert-subtype b (et-or (et-nil) (et-dt :String)))))
   (if (not b)
       (:assert-subtype b (et-nil))
     (:assert-subtype b (et-or (et-dt :String) (et-dt :Integer))))
   (if (not (not b))
       (:assert-subtype b (et-or (et-dt :String) (et-dt :Integer)))
     (:assert-subtype b (et-nil)))
   (if (not (not (not (not b))))
       (:assert-subtype b (et-or (et-dt :String) (et-dt :Integer)))
     (:assert-subtype b (et-nil)))
   (if (not (not (not (stringp b))))
       (:assert-subtype b (et-or (et-nil) (et-dt :Integer)))
     (:assert-subtype b (et-dt :String)))))

;; Test narrowing across variables
(et-root-block
 (let* ((a :String|Integer|nil 4)
        (b a))
   (when (stringp b)
     (:assert-subtype a (et-dt :String)))
   (:assert-error (:assert-subtype a (et-dt :String)))))


;; ============================================================
;; Provide

(provide 'tests)
;;; tests.el ends here
