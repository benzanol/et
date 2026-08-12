;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Char-table creation

(et-declare
 (@def make-char-table (purpose: Symbol &optional init: Any) CharTable))

;;; ============================================================
;;; Char-table access and modification

(et-declare
 (@def char-table-subtype (char-table: CharTable) Symbol)
 (@def char-table-parent (char-table: CharTable) CharTable?)
 (@def set-char-table-parent (char-table: CharTable parent: CharTable?) CharTable?)
 (@def char-table-extra-slot (char-table: CharTable n: Integer) Any)
 (@def set-char-table-extra-slot ([T] char-table: CharTable n: Integer value: T) T)
 (@def char-table-range (char-table: CharTable range: Integer|Cons<Integer~Integer>?) Any)
 (@def set-char-table-range
       ([T] char-table: CharTable range: True|Integer|Cons<Integer~Integer>? value: T)
       T)
 (@def optimize-char-table (char-table: CharTable &optional test: (or Nil (fn (Args Any Any) Any))) Nil)
 (@def map-char-table
       (function: (fn (Args (or Integer Cons<Integer~Integer>) Any) Any) char-table: CharTable)
       Nil))

;;; ============================================================
;;; Unicode character property tables

(et-declare
 (@def unicode-property-table-internal (prop: Symbol) Any)
 (@def get-unicode-property-internal (char-table: CharTable ch: Integer) Any)
 (@def put-unicode-property-internal (char-table: CharTable ch: Integer value: Any) Nil))

;;; ============================================================
