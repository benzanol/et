;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Charset definition and mapping

(et-declare
 (@def charsetp (object: Any) Boolean)
 (@def map-charset-chars
       ([A] function: fn2<Cons<Integer~Integer>~A> charset: Symbol
        &optional arg: A from-code: Integer? to-code: Integer?)
       Nil)
 (@def define-charset-internal (&rest args: &List) Nil)
 (@def define-charset-alias (alias: Symbol charset: Symbol) Nil)
 (@def charset-plist (charset: Symbol) &PlistOf<Symbol~Any>)
 (@def set-charset-plist ([P] charset: Symbol plist: P) P)
 (@def unify-charset (charset: Symbol &optional unify-map: String|Vector? deunify: Bool) Nil)
 (@def get-unused-iso-final-char (dimension: Integer chars: Integer) Integer?)
 (@def declare-equiv-charset
       (dimension: Integer chars: Integer final-char: Integer charset: Symbol)
       Nil))

;;; ============================================================
;;; Charset discovery and conversion

(et-declare
 (@def find-charset-region (beg: IntOrMarker end: IntOrMarker &optional table: CharTable?) List<Symbol>)
 (@def find-charset-string (str: String &optional table: CharTable?) List<Symbol>)
 (@def decode-char (charset: Symbol code-point: Integer|Cons<Integer~Integer>) Integer?)
 (@def encode-char (ch: Integer charset: Symbol) Integer?)
 (@def make-char
       ([] charset: Symbol &optional code1: Integer? code2: Integer? code3: Integer? code4: Integer?)
       Integer)
 (@def split-char (ch: Integer) Cons<Symbol~List<Integer>>)
 (@def char-charset (ch: Integer &optional restriction: &List<Symbol>|Symbol?) Symbol?)
 (@def charset-after (&optional pos: IntOrMarker?) Symbol?)
 (@def iso-charset (dimension: Integer chars: Integer final-char: Integer) Symbol?))

;;; ============================================================
;;; Charset maps and priority

(et-declare
 (@def clear-charset-maps () Nil)
 (@def charset-priority-list (&optional highestp: Bool) Symbol|List<Symbol>)
 (@def set-charset-priority (&rest charsets: &List<Symbol>) Nil)
 (@def charset-id-internal (&optional charset: Symbol?) Integer)
 (@def sort-charsets (charsets: List<Symbol>) List<Symbol>))

;;; ============================================================
