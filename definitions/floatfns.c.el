;; Type definitions for builtins defined in Emacs' src/floatfns.c.
;;
;; There is no `Float' datatype, so a float-producing function returns
;; `Number', and a float-only parameter (`copysign', `isnan', the f*
;; rounding functions) is declared as `Number'. Everything else is exact.


;;; Transcendental functions

;; These always return a float, whatever they are given.

(et-declare
 (@function acos (arg) (arg Number) (@return Number))
 (@function asin (arg) (arg Number) (@return Number))
 (@function atan (y &optional x) (y Number) (x Number|Nil) (@return Number))
 (@function cos (arg) (arg Number) (@return Number))
 (@function sin (arg) (arg Number) (@return Number))
 (@function tan (arg) (arg Number) (@return Number))
 (@function exp (arg) (arg Number) (@return Number))
 (@function log (arg &optional base) (arg Number) (base Number|Nil) (@return Number))
 (@function sqrt (arg) (arg Number) (@return Number)))

(et-test
 (et-assert-resolve Number (cos 0))
 (et-assert-resolve Number (atan 1 2))
 (et-assert-resolve Number (log 8 2))
 (et-assert-no-resolve Integer (sqrt 4))
 (et-assert-resolve-errors (sin "x")))


;;; Powers and magnitude

;; `expt' follows the Common Lisp rule: it stays in the integers only
;; when the base is an Integer and the exponent is a natural number.
;;
;; `abs' preserves the exactness of its argument, and is always
;; non-negative -- but 0 is neither `Positive' nor `Negative', so the
;; sign cannot be expressed any more precisely than `Number'/`Integer'.

(et-declare
 (@alias Natnum (or (and Integer Positive) 0))

 (@function expt (arg1 arg2)
            (@generics [X Y])
            (arg1 X&Number) (arg2 Y&Number)
            (@return (extends? X Integer (extends? Y Natnum Integer Number) Number)))

 (@function abs (arg)
            (@generics [X])
            (arg X&Number)
            (@return (extends? X Integer Integer Number)))

 (@function float (arg) (arg Number) (@return Number))

 ;; The exponent of a float, i.e. an Integer even though ARG is a float.
 (@function logb (arg) (arg Number) (@return Integer))
 (@function isnan (x) (x Number) (@return Boolean))
 (@function copysign (x1 x2) (x1 Number) (x2 Number) (@return Number))

 ;; SGNFCAND is a float in [0.5, 1.0), EXPONENT an Integer.
 (@function frexp (x) (x Number) (@return ConsR<Number~Integer>))
 (@function ldexp (sgnfcand exponent)
            (sgnfcand Number) (exponent Integer) (@return Number)))

(et-test
 (et-assert-resolve Integer (expt 2 3))
 (et-assert-resolve Integer (abs -1))
 (et-assert-resolve Number (abs -1.5))
 (et-assert-call Number expt Integer Negative)
 (et-assert-call Number expt Number Integer)
 (et-assert-call-errors expt String Integer)
 (et-assert-resolve Number (float 1))
 (et-assert-resolve Integer (logb 10.0))
 (et-assert-resolve Boolean (isnan 0.0))
 (et-assert-resolve ConsR<Number~Integer> (frexp 3.5))
 (et-assert-resolve Number (ldexp 0.5 3)))


;;; Rounding

;; The plain rounding functions convert to an Integer; the `f' variants
;; round in place, and so return a float.

(et-declare
 (@function ceiling (arg &optional divisor) (arg Number) (divisor Number|Nil) (@return Integer))
 (@function floor (arg &optional divisor) (arg Number) (divisor Number|Nil) (@return Integer))
 (@function round (arg &optional divisor) (arg Number) (divisor Number|Nil) (@return Integer))
 (@function truncate (arg &optional divisor) (arg Number) (divisor Number|Nil) (@return Integer))

 (@function fceiling (arg) (arg Number) (@return Number))
 (@function ffloor (arg) (arg Number) (@return Number))
 (@function fround (arg) (arg Number) (@return Number))
 (@function ftruncate (arg) (arg Number) (@return Number)))

(et-test
 (et-assert-resolve Integer (floor 1.7))
 (et-assert-resolve Integer (round 7 2))
 (et-assert-resolve Number (ffloor 1.7))
 (et-assert-resolve-errors (floor "1.7")))
