;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Category sets

(et-declare
;; AUTHORING STUB: not yet classified.
(@def make-category-set (categories: Todo) Todo))

;;; ============================================================
;;; Category definitions

(et-declare
;; AUTHORING STUB: not yet classified.
(@def define-category (category: Todo docstring: Todo &optional table: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def category-docstring (category: Todo &optional table: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def get-unused-category (&optional table: Todo) Todo))

;;; ============================================================
;;; Category tables

(et-declare
;; AUTHORING STUB: not yet classified.
(@def category-table-p (arg: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def category-table () Todo)
;; AUTHORING STUB: not yet classified.
(@def standard-category-table () Todo)
;; AUTHORING STUB: not yet classified.
(@def copy-category-table (&optional table: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def make-category-table () Todo)
;; AUTHORING STUB: not yet classified.
(@def set-category-table (table: Todo) Todo))

;;; ============================================================
;;; Category membership

(et-declare
;; AUTHORING STUB: not yet classified.
(@def char-category-set (ch: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def category-set-mnemonics (category_set: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def modify-category-entry (character: Todo category: Todo &optional table: Todo reset: Todo) Todo))

;;; ============================================================
