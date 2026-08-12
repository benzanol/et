;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Character predicates and conversion

(et-declare
;; AUTHORING STUB: not yet classified.
(@def characterp (object: Todo &optional ignore: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def max-char (&optional unicode: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def unibyte-char-to-multibyte (ch: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def multibyte-char-to-unibyte (ch: Todo) Todo))

;;; ============================================================
;;; Display width

(et-declare
;; AUTHORING STUB: not yet classified.
(@def char-width (char: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def string-width (string: Todo &optional from: Todo to: Todo) Todo))

;;; ============================================================
;;; String construction

(et-declare
;; AUTHORING STUB: not yet classified.
(@def string (&rest characters: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def unibyte-string (&rest bytes: Todo) Todo))

;;; ============================================================
;;; Character modifiers and byte access

(et-declare
;; AUTHORING STUB: not yet classified.
(@def char-resolve-modifiers (char: Todo) Todo)
;; AUTHORING STUB: not yet classified.
(@def get-byte (&optional position: Todo string: Todo) Todo))

;;; ============================================================
