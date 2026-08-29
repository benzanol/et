;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Category sets

(et-declare
 (@def make-category-set (categories: String) BoolVector))

;;; ============================================================
;;; Category definitions

(et-declare
 (@def define-category (category: Integer docstring: String &optional table: CharTable?) Nil)
 (@def category-docstring (category: Integer &optional table: CharTable?) String?)
 (@def get-unused-category (&optional table: CharTable?) Integer?))

;;; ============================================================
;;; Category tables

(et-declare
 (@def category-table-p (arg: Any) Boolean)
 (@def category-table () CharTable)
 (@def standard-category-table () CharTable)
 (@def copy-category-table (&optional table: CharTable?) CharTable)
 (@def make-category-table () CharTable)
 (@def set-category-table (table: CharTable?) CharTable))

;;; ============================================================
;;; Category membership

(et-declare
 (@def char-category-set (ch: Integer) BoolVector)
 (@def category-set-mnemonics (category-set: BoolVector) String)
 (@def modify-category-entry
       (character: Integer|Cons<Integer~Integer> category: Integer
        &optional table: CharTable? reset: Bool)
       Nil))

;;; ============================================================
