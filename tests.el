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
  (require 'et)
  (require 'et-types))


;;; Code:
;;; ============================================================
;;; Typesystem
;;;; Subtype

;; This tests a very particular line in `et-subtype?'. Specifically,
;; at each subtype factor, before iterating through the supertype
;; factors, it checks whether the subtype datatype wants to check if
;; it is a subtype of the entire supertype, instead of checking
;; whether it is a subtype of each individual factor of the supertype.
;; The datatype List<integer> is a subtype of the provided nil|cons,
;; but it is a subtype of neither case individually, so this test only
;; passes when the described code path is working correctly.
(et-assert-true
 (et-subtype? (et-parse :List<integer>)
              (et-parse :nil|cons<number~List<integer>>)))

;; This one works without the special path described above, but check
;; it too for good measure
(et-assert-true
 (et-subtype? (et-parse :nil|cons<number~List<integer>>)
              (et-parse :List<number>)))


;;;; Inferring subtypes

;; Inferring outside of an infer boundary should error
(et-assert-error (et-subtype? (et-dt :number) (et-dt :infer-subtype :A)))
(et-assert-error (et-subtype? (et-dt :number) (et-dt :infer-supertype :A)))
;; Inferring in the wrong direction should error
(et-assert-error (et-infer-subtype [:A] :number :A))
(et-assert-error (et-infer-supertype [:A] :number :A))

(et-assert-equal (list (et-dt :number))
  (et-infer-subtype [:A] :A :number))

;; Cons cell matching against list

(et-assert-equal (list (et-dt :integer))
  (et-infer-subtype [:A]
    (:cons :A (:List :integer))
    (:List :integer)))

;; Change the tail type to :number from the above example. Inferring
;; should fail if the rest of the infer type doesn't match the
;; concrete type, even if the variable matches something.
(et-assert-nil
 (et-infer-subtype [:A]
   (:cons :A (:List :number))
   :List<integer>))

;; List cell matching against cons cell

(et-assert-equal (list (et-dt :integer))
  (et-infer-subtype [:A]
    (:List :A)
    (:or :nil (:cons :number (:List :integer)))))

(et-assert-nil
 (et-infer-subtype [:A]
   (:List :A)
   (:cons :number (:List :integer))))

;; Matching two variables

(et-assert-equal (list (et-dt :integer) (et-dt :integer))
  (et-infer-subtype [:A :B]
    (:List (:or :A :B))
    (:or :nil (:cons :number (:List :integer)))))

;; Came across accidentally, and thought it was a bug, but b=any
;; technically does satisfy the subtype.
(et-assert-equal (list (et-dt :integer) (et-any))
  (et-infer-subtype [:A :B]
    (:List (:and :A :B))
    (:or :nil (:cons :number (:List :integer)))))

(et-assert-equal (list (et-dt :integer) (et-alias :List (et-dt :integer)))
  (et-infer-subtype [:A :B]
    (:cons :A (:cons :integer :B))
    (:List :integer)))


;;;; Supertypes vs subtypes

(et-assert-equal (list (et-or (et-dt :integer) (et-dt :string)))
  (et-infer-supertype [:Elem]
    (:cons :Elem :Elem)
    (:cons :integer :string)))
(et-assert-nil
 (et-infer-subtype [:Elem]
   (:cons :Elem :Elem)
   (:cons :integer :string)))

(et-assert-equal (list (et-parse '(:or (:cons :integer :nil) :integer)))
  (et-infer-supertype [:Elem]
    (:and :Elem (:cons :Elem :nil))
    (:cons :integer :nil)))
(et-assert-nil
 (et-infer-subtype [:Elem]
   (:or :Elem (:cons :Elem :nil))
   (:cons :integer :nil)))


;;;; Typing car with infer

;; Say we want to infer the return type of `nth'. This determines the
;; elem type to be integer, implying that subtype inference is
;; invalid.
(et-assert-equal (list (et-dt :integer))
  (et-infer-subtype [:Elem]
    (:List :Elem)
    (:or (:num 4) (:List :integer))))
;; Whereas supertype inference correctly determines that it is invalid
(et-assert-nil
 (et-infer-supertype [:Elem]
   (:List :Elem)
   (:or (:num 4) (:List :integer))))

;; Contrast these two
(et-assert-equal (list (et-dt :number))
  (et-infer-supertype [:Elem]
    (:cons :Elem :Elem)
    (:cons :integer :number)))
(et-assert-equal (list (et-dt :integer))
  (et-infer-subtype [:Elem]
    (:cons :Elem :Elem)
    (:cons :integer :number)))

;; Finally, the car type we desired
(et-assert-equal (list (et-dt :integer))
  (et-infer-supertype [:Elem]
    (:or (:cons :Elem :any)
         (:infer :Elem :nil)) ; This case isn't used here
    (:cons :integer (:List :number))))
;; This seems like it works
(et-assert-equal (list (et-raw-or (et-nil) (et-dt :number)))
  (et-infer-supertype [:Elem]
    (:or (:cons :Elem :any) :Elem)
    (:List :number)))
;; However, elem can match to things other than nil
(et-assert-equal (list (et-raw-or (et-dt :string) (et-nil) (et-dt :number)))
  (et-infer-supertype [:Elem]
    (:or (:cons :Elem :any) :Elem)
    (:or :string (:List :number))))
;; So we need to restrict it -- this is our final `car' strategy
(et-assert-equal (list (et-raw-or (et-nil) (et-dt :number)))
  (et-infer-supertype [:Elem]
    (:or (:cons :Elem :any)
         (:infer :Elem :nil))
    (:List :number)))
(et-assert-equal (list (et-dt :string))
  (et-infer-supertype [:Elem]
    (:or (:cons :Elem :any)
         (:infer :Elem :nil))
    (:cons :string :integer)))
(et-assert-nil
 (et-infer-supertype [:Elem]
   (:or (:cons :Elem :any)
        (:infer :Elem :nil))
   (:or :string (:List :number))))


;;; ============================================================
;;; Expressions
;;;; Primitives

(et-assert-success (et-root-resolve :number 1))
(et-assert-success (et-root-resolve :number 1.1))
(et-assert-error (et-root-resolve :number "1"))

(et-assert-success (et-root-resolve :integer 1))
(et-assert-error (et-root-resolve :integer 1.1))
(et-assert-error (et-root-resolve :integer "1"))

(et-assert-success (et-root-resolve :string "1"))
(et-assert-error (et-root-resolve :string 1))

(et-assert-success (et-root-resolve :symbol nil))
(et-assert-success (et-root-resolve :symbol t))
(et-assert-error (et-root-resolve :symbol 'a)) ; Not self-quoting
(et-assert-error (et-root-resolve :symbol 1))
(et-assert-error (et-root-resolve :symbol "1"))

(et-assert-success (et-root-resolve :Boolean t))
(et-assert-success (et-root-resolve :Boolean nil))
(et-assert-error (et-root-resolve :Boolean 'a))
(et-assert-error (et-root-resolve :Boolean 1))
(et-assert-error (et-root-resolve :Boolean "1"))


;;;; Quoted

(et-assert-success (et-root-resolve :integer ''1))
(et-assert-success (et-root-resolve :number ''1.1))
(et-assert-success (et-root-resolve :string ''"hi"))
(et-assert-success (et-root-resolve :symbol ''a))
(et-assert-error (et-root-resolve :integer ''1.1))
(et-assert-error (et-root-resolve :integer '''1))
(et-assert-error (et-root-resolve :number '''1.1))
(et-assert-error (et-root-resolve :string '''"hi"))
(et-assert-error (et-root-resolve :symbol '''a))

(et-assert-success (et-root-resolve :cons<any~any> ''(1 2 3)))
(et-assert-success (et-root-resolve :List<symbol> ''(a b c)))
(et-assert-success (et-root-resolve :List<integer> ''()))
(et-assert-error (et-root-resolve :List<integer> ''(1 2 '3)))
(et-assert-error (et-root-resolve :List<integer> ''(1 2 3.3)))
(et-assert-error (et-root-resolve :List<integer> '''(1 2 3)))
(et-assert-error (et-root-resolve :List<integer> '''()))

(et-assert-success (et-root-resolve :cons<integer~integer> ''(1 . 2)))
(et-assert-error (et-root-resolve :cons<integer~integer> ''(1 . 2.2)))
(et-assert-error (et-root-resolve :cons<integer~integer> ''(1.1 . 2)))
(et-assert-success (et-root-resolve :cons<symbol~List<string>> ''(a "2" "3")))


;;;; and/or

;; and - value must satisfy all constituent types
(et-assert-success (et-root-resolve :Boolean&Boolean t))
(et-assert-success (et-root-resolve :Boolean&symbol&Boolean t))
(et-assert-error   (et-root-resolve :Boolean&integer t))
(et-assert-error   (et-root-resolve :Boolean&integer 1))
(et-assert-error   (et-root-resolve :Boolean&integer nil))

;; Two or types
(et-assert-success (et-root-resolve :Boolean|integer t))
(et-assert-success (et-root-resolve :Boolean|integer nil))
(et-assert-success (et-root-resolve :Boolean|integer 1))
(et-assert-error   (et-root-resolve :Boolean|integer "1"))
(et-assert-error   (et-root-resolve :Boolean|integer 'a))

;; Three or types
(et-assert-success (et-root-resolve :Boolean|integer|string t))
(et-assert-success (et-root-resolve :Boolean|integer|string 1))
(et-assert-success (et-root-resolve :Boolean|integer|string "1"))
(et-assert-error   (et-root-resolve :Boolean|integer|string 'a))

;; Nested - and inside or
(et-assert-success (et-root-resolve :integer|Boolean&symbol t))
(et-assert-success (et-root-resolve :integer|Boolean&symbol 1))
(et-assert-error   (et-root-resolve :integer|Boolean&symbol 'a))

;; Nested - or inside and
(et-assert-success (et-root-resolve :Boolean&{symbol|integer} t))
(et-assert-success (et-root-resolve :Boolean&{symbol|integer} nil))
(et-assert-error   (et-root-resolve :Boolean&{symbol|integer} 1))


;;;; cons

(et-assert-success (et-root-resolve :cons<integer~string> '(cons 1 "2")))
(et-assert-error (et-root-resolve :cons<integer~string> '(cons "1" 2)))
(et-assert-success (et-root-resolve :cons<integer~List<string>> '(cons 1 nil)))
(et-assert-success (et-root-resolve :cons<integer~List<string>> '(cons 1 (cons "2" nil))))

(et-assert-success (et-root-resolve :List<integer> '(cons 1 (cons 2 nil))))
(et-assert-error (et-root-resolve :List<integer> '(cons 1 (cons "2" nil))))
(et-assert-error (et-root-resolve :List<integer> '(cons "1" (cons 2 nil))))
(et-assert-error (et-root-resolve :List<integer> '(cons 1 (cons 2 t))))


;;;; list

(et-assert-success (et-root-resolve :cons<integer~List<string>> '(list 1 "2")))
(et-assert-error (et-root-resolve :cons<integer~string> '(list "1" 2)))
(et-assert-error (et-root-resolve :cons<integer~string> '(list)))

(et-assert-success (et-root-resolve :List<integer> '(list 1 2 3)))
(et-assert-success (et-root-resolve :List<integer> '(list 1)))
(et-assert-error (et-root-resolve :List<integer> '(list 1 "2" 3)))


;;;; car

(et-assert-success (et-root-resolve :integer '(car (list 1 2.2 3))))
(et-assert-error (et-root-resolve :integer '(car (list 1.1 2 3))))
(et-assert-success (et-root-resolve :integer '(car (cons 1 "3"))))
(et-assert-success (et-root-resolve :List<integer> '(car (cons (list 1) "3"))))
(et-assert-success (et-root-resolve :cons<integer~any> '(car (cons (list 1) "3"))))
(et-assert-success (et-root-resolve :integer '(car (car (cons (list 1) "3")))))
(et-assert-error (et-root-resolve :integer '(car (car (cons (list 1.1) "3")))))


;;;; cdr

(et-assert-success (et-root-resolve :List<number> '(cdr (list 1 2.2 3))))
(et-assert-success (et-root-resolve :List<integer> '(cdr (list 1.1 2 3))))
(et-assert-error (et-root-resolve :List<integer> '(car (list 1 2.2 3))))

(et-assert-success (et-root-resolve :integer '(cdr (cons "1" 2))))
(et-assert-error (et-root-resolve :integer '(cdr (cons 1 "2"))))

(et-assert-success (et-root-resolve :List<integer> '(cdr (cons "1" (list 2)))))
(et-assert-success (et-root-resolve :cons<integer~any> '(cdr (cons "1" (list 2)))))
(et-assert-success (et-root-resolve :cons<integer~Boolean> '(cdr (cons "1" (list 2)))))
(et-assert-error (et-root-resolve :cons<integer~Boolean> '(cdr (cons "1" (list 2 3)))))
(et-assert-success (et-root-resolve :integer '(car (cdr (cons "1" (list 2))))))

(et-assert-success (et-root-resolve :Boolean '(cdr (cdr (cdr (list 1 2 3))))))
(et-assert-error (et-root-resolve :Boolean '(cdr (cdr (list 1 2 3)))))


;;; ============================================================
;;; Blocks

;; Setting type binds to an incompatible type returns never
(cl-assert
 (equal
  (let ((vs (cons 'a (et-or (et-dt :integer) (et-dt :string)))))
    (et-with-binds (list vs)
      (et-with-narrow-binds (list (cons vs (et-dt :integer)))
        (et--replace-type-binds (et-literal t) (list (cons vs (et-dt :string)))))))
  (et-never)))

;; A few hard type narrowing cases
(et-root-block
 (let* ((b :string|integer|nil 4))
   (if (and b (or (null b) (stringp b)))
       (:assert-subtype b (et-dt :string))
     (:assert-subtype b (et-or (et-nil) (et-dt :integer)))
     (:assert-error (:assert-subtype b (et-or (et-nil) (et-dt :string)))))
   (if (not b)
       (:assert-subtype b (et-nil))
     (:assert-subtype b (et-or (et-dt :string) (et-dt :integer))))
   (if (not (not b))
       (:assert-subtype b (et-or (et-dt :string) (et-dt :integer)))
     (:assert-subtype b (et-nil)))
   (if (not (not (not (not b))))
       (:assert-subtype b (et-or (et-dt :string) (et-dt :integer)))
     (:assert-subtype b (et-nil)))
   (if (not (not (not (stringp b))))
       (:assert-subtype b (et-or (et-nil) (et-dt :integer)))
     (:assert-subtype b (et-dt :string)))))

;; Test narrowing across variables
(et-root-block
 (let* ((a :string|integer|nil 4)
        (b a))
   (when (stringp b)
     (:assert-subtype a (et-dt :string)))
   (:assert-error (:assert-subtype a (et-dt :string)))))


;; ============================================================
;; Provide

(provide 'tests)
;;; tests.el ends here
