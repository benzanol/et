;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Char-table creation

(et-declare
;; AUTHORING STUB: not yet classified.
(@def make-char-table (purpose: Todo &optional init: Todo) Todo))

;;; ============================================================
;;; Char-table access and modification

(et-declare
;; AUTHORING STUB: not yet classified.
(@def char-table-subtype (char_table: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def char-table-parent (char_table: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def set-char-table-parent (char_table: Todo parent: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def char-table-extra-slot (char_table: Todo n: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def set-char-table-extra-slot (char_table: Todo n: Todo value: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def char-table-range (char_table: Todo range: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def set-char-table-range (char_table: Todo range: Todo value: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def optimize-char-table (char_table: Todo &optional test: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def map-char-table (function: Todo char_table: Todo) Todo))

;;; ============================================================
;;; Unicode character property tables

(et-declare
;; AUTHORING STUB: not yet classified.
(@def unicode-property-table-internal (prop: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def get-unicode-property-internal (char_table: Todo ch: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def put-unicode-property-internal (char_table: Todo ch: Todo value: Todo) Todo))

;;; ============================================================
