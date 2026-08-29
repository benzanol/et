;; -*- lexical-binding: t; -*-


;;; ============================================================
;;; Syntax tables

(et-declare
 (@def syntax-table-p (object: Any) Boolean)
 (@def syntax-table () CharTable)
 (@def standard-syntax-table () CharTable)
 (@def copy-syntax-table (&optional table: CharTable?) CharTable)
 (@def set-syntax-table (table: CharTable) CharTable)
 (@def char-syntax (character: Integer) Integer)
 (@def syntax-class-to-char (syntax: Integer) Integer)
 (@def matching-paren (character: Integer) Integer?)
 (@def string-to-syntax (string: String) Cons<Integer~Integer?>?)
 (@def modify-syntax-entry
       (char: Integer|Cons<Integer~Integer> newentry: String
              &optional syntax-table: CharTable?)
       Nil)
 ;; This function always returns SYNTAX unchanged, regardless of its shape.
 (@def internal-describe-syntax-value (syntax: [T]) T))


;;; ============================================================
;;; Word and character motion

(et-declare
 (@def forward-word (&optional arg: Integer?) Boolean)
 (@def skip-chars-forward (string: String &optional lim: IntOrMarker?) Integer)
 (@def skip-chars-backward (string: String &optional lim: IntOrMarker?) Integer)
 (@def skip-syntax-forward (syntax: String &optional lim: IntOrMarker?) Integer)
 (@def skip-syntax-backward (syntax: String &optional lim: IntOrMarker?) Integer))


;;; ============================================================
;;; Comment motion

(et-declare
 (@def forward-comment (count: Integer) Boolean))


;;; ============================================================
;;; Sexp scanning

(et-declare
 (@def scan-lists (from: Integer count: Integer depth: Integer) Integer?)
 (@def scan-sexps (from: Integer count: Integer) Integer?)
 (@def backward-prefix-chars () Nil)
 (@def parse-partial-sexp
       (from: IntOrMarker to: IntOrMarker
              &optional targetdepth: Integer? stopbefore: Bool
              oldstate: (or Nil
                            (Tuple Integer Integer? Integer? Boolean|Integer Boolean|Integer
                                   Boolean Integer @syntax-table|Integer? Integer?
                                   List<Integer> Integer?))
              commentstop: Any)
       (Tuple Integer Integer? Integer? Boolean|Integer Boolean|Integer
              Boolean Integer @syntax-table|Integer? Integer?
              List<Integer> Integer?)))


;;; ============================================================
