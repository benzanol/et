;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Composition cache

(et-declare
;; AUTHORING STUB: not yet classified.
(@def clear-composition-cache () Todo))

;;; ============================================================
;;; Glyph strings

(et-declare
;; AUTHORING STUB: not yet classified.
(@def composition-get-gstring (from: Todo to: Todo font-object: Todo string: Todo) Todo))

;;; ============================================================
;;; Composition operations

(et-declare
;; AUTHORING STUB: not yet classified.
(@def compose-region-internal (start: Todo end: Todo &optional components: Todo modification-func: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def compose-string-internal (string: Todo start: Todo end: Todo &optional components: Todo modification-func: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def find-composition-internal (pos: Todo limit: Todo string: Todo detail-p: Todo) Todo))

;;; ============================================================
;;; Composition rules

(et-declare
;; AUTHORING STUB: not yet classified.
(@def composition-sort-rules (rules: Todo) Todo))

;;; ============================================================
