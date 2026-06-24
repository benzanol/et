;;; data.c.el --- Type definitions for src/data.c -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Adam Tillou

;; Author: Adam Tillou <adam.tillou@gmail.com>
;; Keywords: tools


;;; Commentary:

;; Type definitions for builtins defined in Emacs' src/data.c.


;;; Code:

(require 'et-check)


;;; ============================================================
;;; Cons matching aliases

;; `MatchCar'/`MatchCdr' are matcher-only aliases used by `car'/`cdr'
;; (and the c[ad]+r family in subr.el): matching a value against
;; `MatchCar<T>' binds T to the type produced by calling `car' on it.

(et-declare
 (@alias MatchCar [T] :matcher-only t
         (or (and Nil (set T Nil))
             (ConsR T Any)))
 (@alias MatchCdr [T] :matcher-only t
         (or (and Nil (set T Nil))
             (ConsR Any T))))


;;; ============================================================
;;; Arithmetic

;; The argument list is captured whole as the generic `Nums' (constrained
;; to a list of numbers), and the return type branches on what `Nums'
;; turns out to be: an empty call yields the identity element, an
;; all-integer call yields Integer, and anything else yields Number.

(et-declare
 (@function + (&rest numbers)
            (@generics [Nums])
            (numbers Nums&ListR<Number>)
            (@return (extends? Nums Nil 0 (extends? Nums ListR<Integer> Integer Number))))
 (@function - (&rest numbers)
            (@generics [Nums])
            (numbers Nums&ListR<Number>)
            (@return (extends? Nums Nil 0 (extends? Nums ListR<Integer> Integer Number))))
 (@function * (&rest numbers)
            (@generics [Nums])
            (numbers Nums&ListR<Number>)
            (@return (extends? Nums Nil 1 (extends? Nums ListR<Integer> Integer Number))))
 (@function / (&rest numbers)
            (@generics [Nums])
            (numbers Nums&NonNilListR<Number>)
            (@return (extends? Nums ListR<Integer> Integer Number)))

 (@function 1+ (number)
            (@generics [Num])
            (number Num&Number)
            (@return (extends? Num Integer Integer Number)))
 (@function 1- (number)
            (@generics [Num])
            (number Num&Number)
            (@return (extends? Num Integer Integer Number)))

 (@function max (&rest numbers)
            (@generics [Nums])
            (numbers Nums&NonNilListR<Number>)
            (@return (extends? Nums ListR<Integer> Integer Number)))
 (@function min (&rest numbers)
            (@generics [Nums])
            (numbers Nums&NonNilListR<Number>)
            (@return (extends? Nums ListR<Integer> Integer Number)))

 (@function mod (x y)
            (@generics [X Y])
            (x X&Number) (y Y&Number)
            (@return (extends? X Integer (extends? Y Integer Integer Number) Number))))

(et-test
 ;; All-integer arguments -> Integer
 (et-assert-call Integer + Integer Integer 1 2 3)
 (et-assert-call Integer - Integer Integer)
 (et-assert-call Integer * 1 2 3)
 (et-assert-call Integer / Integer Integer)
 (et-assert-call Integer + 1)
 (et-assert-call Integer / 1)
 ;; Any non-integer number -> Number
 (et-assert-call Number + Integer 2.1 3)
 (et-assert-call Number * Number Integer)
 ;; Identity element for an empty call
 (et-assert-call 0 +)
 (et-assert-call 0 -)
 (et-assert-call 1 *)
 ;; `/' requires at least one argument
 (et-assert-call-errors /)
 ;; Non-numbers are rejected
 (et-assert-call-errors + String)
 (et-assert-call-errors + Integer String))

(et-test
 (et-assert-resolve Integer (1+ 1))
 (et-assert-resolve Number (1- 1.5))
 (et-assert-call-errors 1+ String)
 (et-assert-call Integer max Integer Integer)
 (et-assert-call Number min Integer Number)
 (et-assert-call-errors max)
 (et-assert-resolve Integer (mod 7 3))
 (et-assert-resolve Number (mod 7.5 3)))


;;; ============================================================
;;; Comparison / equality

(et-declare
 (@function eq (a b)
            (@generics [A B])
            (a A) (b B)
            (@return (or Nil (and True (bindsof (and A B))))))

 (@function = (a b)
            (@generics [(<= A Number) (<= B Number)])
            (a A) (b B)
            (@return (or Nil (and True (bindsof (and A B))))))

 (@function < (a b) (a Number) (b Number) (@return Boolean))
 (@function <= (a b) (a Number) (b Number) (@return Boolean))
 (@function > (a b) (a Number) (b Number) (@return Boolean))
 (@function >= (a b) (a Number) (b Number) (@return Boolean)))

(et-test
 (et-assert-resolve Boolean (< 1 2))
 (et-assert-resolve Boolean (= 1 2))
 (et-assert-resolve-errors (< 1 "2")))


;;; ============================================================
;;; Predicates

(et-declare
 (@function stringp (object)
            (@generics [T]) (object T)
            (@return (or (and True (bindsof (and T String)))
                         (and Nil (bindsof (subtract T String))))))
 (@function symbolp (object)
            (@generics [T]) (object T)
            (@return (or (and True (bindsof (and T Symbol)))
                         (and Nil (bindsof (subtract T Symbol))))))
 (@function numberp (object)
            (@generics [T]) (object T)
            (@return (or (and True (bindsof (and T Number)))
                         (and Nil (bindsof (subtract T Number))))))
 (@function integerp (object)
            (@generics [T]) (object T)
            (@return (or (and True (bindsof (and T Integer)))
                         (and Nil (bindsof (subtract T Integer))))))
 (@function consp (object)
            (@generics [T]) (object T)
            (@return (or (and True (bindsof (and T Cons)))
                         (and Nil (bindsof (subtract T Cons))))))
 (@function listp (object)
            (@generics [T]) (object T)
            (@return (or (and True (bindsof (and T Nil|Cons)))
                         (and Nil (bindsof (subtract T Nil|Cons))))))
 (@function vectorp (object)
            (@generics [T]) (object T)
            (@return (or (and True (bindsof (and T Vector)))
                         (and Nil (bindsof (subtract T Vector))))))
 (@function null (object)
            (@generics [T]) (object T)
            (@return (or (and True (bindsof (and T Nil)))
                         (and Nil (bindsof (subtract T Nil))))))
 (@function not (object)
            (@generics [T]) (object T)
            (@return (or (and True (bindsof (and T Nil)))
                         (and Nil (bindsof (subtract T Nil))))))
 (@function atom (object)
            (@generics [T]) (object T)
            (@return (or (and True (bindsof (subtract T Cons)))
                         (and Nil (bindsof (and T Cons)))))))

(et-test
 (et-assert-call True&{$a::Cons}
                 consp Cons&{::$a})
 (et-assert-call (or True&{$a::String} Nil&{$a::Number})
                 stringp {::$a}&{String|Number})
 ;; There is no defined intersection of Positive and Integer, so the
 ;; checker must approximate to a SUPERSET (never to Never).
 (not (et-subtype? (et-result-value (et-typecheck-call integerp Positive&{::$a}))
                   (et Nil))))

(et-test
 ;; Predicates narrow a typeof-bound argument (`$a') in each branch.
 (et-assert-call True&{$a::Vector<Any>} vectorp Vector<Any>&{::$a})
 (et-assert-call (or True&{$a::Vector<Any>} Nil&{$a::Number})
                 vectorp {::$a}&{Vector<Any>|Number})
 (et-assert-call True&{$a::Integer} atom Integer&{::$a})
 (et-assert-call (or True&{$a::Integer} Nil&{$a::Cons})
                 atom {::$a}&{Integer|Cons}))


;;; ============================================================
;;; Cons cells

(et-declare
 (@function car (list)
            (@generics [T]) (list (MatchCar T)) (@return T))
 (@function cdr (list)
            (@generics [T]) (list (MatchCdr T)) (@return T))
 (@function car-safe (object)
            (@generics [T]) (object Any|ConsR<T~Any>) (@return T))
 (@function cdr-safe (object)
            (@generics [T]) (object Any|ConsR<Any~T>) (@return T))
 ;; NOTE: the parameter order here is preserved verbatim from the
 ;; original `setcar' checker.
 (@function setcar (a b)
            (@generics [A]) (a A) (b Nil|ConsW<A~Never>) (@return A)))

(et-test
 (et-assert-resolve Integer (car (list 1 2.2 3)))
 (et-assert-no-resolve Integer (car (list 1.1 2 3)))
 (et-assert-resolve Integer (car (cons 1 "3")))
 (et-assert-resolve ListR<Integer> (car (cons (list 1) "3")))
 (et-assert-resolve Integer (car (car (cons (list 1) "3"))))

 (et-assert-call Never cdr Never)
 (et-assert-call Nil cdr Nil)
 (et-assert-call Nil|String cdr Nil|ConsR<Integer~String>)
 (et-assert-call-errors cdr Nil|ConsR<Integer~String>|String)
 (et-assert-call-errors cdr :any)

 (et-assert-call Never car Never)
 (et-assert-call Nil car Nil)
 (et-assert-call Nil|Integer car Nil|ConsR<Integer~String>)
 (et-assert-call-errors car Nil|ConsR<Integer~String>|String)
 (et-assert-call-errors car Any)

 (et-assert-call Nil|Integer car ListR<Integer>)
 (et-assert-call Nil|Integer|String car ListR<Integer>|ConsR<String~Nil>))

(et-test
 (et-assert-resolve ListR<Number> (cdr (list 1 2.2 3)))
 (et-assert-resolve ListR<Integer> (cdr (list 1.1 2 3)))
 (et-assert-resolve Integer (cdr (cons "1" 2)))
 (et-assert-no-resolve Integer (cdr (cons 1 "2")))
 (et-assert-resolve Boolean (cdr (cdr (cdr (list 1 2 3))))))

(et-test
 (et-typecheck-call setcar Number ConsW<Number~Number>))


;;; ============================================================
;;; Array access

;; `aref' indexes a vector (yielding its element type) or a string
;; (yielding an Integer character code). Kept as an `et-define-type-checker'
;; because its argument type carries a binding constraint
;; (`{String&T=Integer}') that the `@function' declaration form does not
;; express.
(et-define-type-checker aref [T] (Args VectorR<T>|{String&T=Integer} Integer) T)

(et-test
 (et-assert-call Integer aref String Integer)
 (et-assert-call Symbol|Integer aref (or VectorR<Symbol> String) Integer)
 (et-assert-call-errors aref (or VectorR<Symbol> String ListR<Any>) Integer))


;;; ============================================================
;;; Symbols and conversions

(et-declare
 (@function symbol-name (symbol) (symbol Symbol) (@return String))
 (@function number-to-string (number) (number Number) (@return String))
 (@function string-to-number (string &optional base)
            (string String) (base Integer) (@return Number)))

(et-test
 (et-assert-resolve String (symbol-name 'foo))
 (et-assert-resolve String (number-to-string 5))
 (et-assert-resolve Number (string-to-number "5")))


(provide 'data.c)
;;; data.c.el ends here
