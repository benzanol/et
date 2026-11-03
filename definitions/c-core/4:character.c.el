;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Character predicates and conversion

(et-declare
 (@def characterp (object: Any &optional ignore: Any) Boolean)
 (@def max-char (&optional unicode: Bool) Integer)
 (@def unibyte-char-to-multibyte (ch: Integer) Integer)
 (@def multibyte-char-to-unibyte (ch: Integer) Integer))

;;; ============================================================
;;; Display width

(et-declare
 (@def char-width (char: Integer) Integer)
 (@def string-width (string: String &optional from: Integer? to: Integer?) Integer))

;;; ============================================================
;;; String construction

(et-declare
 (@def string (&rest characters: &List<Integer>) String)
 (@def unibyte-string (&rest bytes: &List<Integer>) String))

;;; ============================================================
;;; Character modifiers and byte access

(et-declare
 (@def char-resolve-modifiers (char: Integer) Integer)
 (@def get-byte (&optional position: IntOrMarker? string: String?) Integer))

;;; ============================================================
