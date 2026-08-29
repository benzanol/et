;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Trigonometric functions

(et-declare
 (@def acos (arg: Number) Number)
 (@def asin (arg: Number) Number)
 (@def atan (y: Number &optional x: Number?) Number)
 (@def cos (arg: Number) Number)
 (@def sin (arg: Number) Number)
 (@def tan (arg: Number) Number))

;;; ============================================================
;;; Floating-point operations

(et-declare
 (@def isnan (x: Number) Boolean)
 (@def copysign (x1: Number x2: Number) Number)
 (@def frexp (x: Number) ConsFresh<Number~Integer>)
 (@def ldexp (sgnfcand: Number exponent: Integer) Number)
 (@def exp (arg: Number) Number)
 (@def expt (arg1: Number arg2: Number) Number)
 (@def log (arg: Number &optional base: Number?) Number)
 (@def sqrt (arg: Number) Number))

;;; ============================================================
;;; Number conversion and magnitude

(et-declare
 (@def abs (arg: [<= N Number]) (extends? N Integer Integer Number))
 (@def float (arg: Number) Number)
 (@def logb (arg: Number) Number))

;;; ============================================================
;;; Rounding functions

(et-declare
 (@def ceiling (arg: Number &optional divisor: Number?) Integer)
 (@def floor (arg: Number &optional divisor: Number?) Integer)
 (@def round (arg: Number &optional divisor: Number?) Integer)
 (@def truncate (arg: Number &optional divisor: Number?) Integer)
 (@def fceiling (arg: Number) Number)
 (@def ffloor (arg: Number) Number)
 (@def fround (arg: Number) Number)
 (@def ftruncate (arg: Number) Number))

;;; ============================================================
