;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Case conversion

(et-declare
;; AUTHORING STUB: not yet classified.
(@def upcase (obj: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def downcase (obj: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def capitalize (obj: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def upcase-initials (obj: Todo) Todo))

;;; ============================================================
;;; Region case conversion

(et-declare
;; AUTHORING STUB: not yet classified.
(@def upcase-region (beg: Todo end: Todo &optional region_noncontiguous_p: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def downcase-region (beg: Todo end: Todo &optional region_noncontiguous_p: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def capitalize-region (beg: Todo end: Todo &optional region_noncontiguous_p: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def upcase-initials-region (beg: Todo end: Todo &optional region_noncontiguous_p: Todo) Todo))

;;; ============================================================
;;; Word case conversion

(et-declare
;; AUTHORING STUB: not yet classified.
(@def upcase-word (arg: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def downcase-word (arg: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def capitalize-word (arg: Todo) Todo))

;;; ============================================================
