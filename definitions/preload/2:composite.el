;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Composition rule encoding

(et-declare
 (@def encode-composition-rule (rule: Integer|Cons<Symbol|Integer~Symbol|Integer>|Cons<Symbol|Integer~Cons<Symbol|Integer~Cons<Integer~Cons<Integer~Nil>>>>) Integer)
 (@def decode-composition-rule (rule-code: Integer) Cons<Symbol~Symbol>|Cons<Symbol~Cons<Integer~Cons<Integer~Cons<Symbol~Nil>>>>)
 (@def encode-composition-components (components: [(<= T Vector<Any>|List<Any>)] &optional nocopy: Bool) T)
 (@def decode-composition-components (components: [(<= T Vector<Any>)] &optional nocopy: Bool) T))

;;; ============================================================
;;; Composing and decomposing text

(et-declare
 (@def compose-region (start: IntOrMarker end: IntOrMarker &optional components: Integer|String|&Vector<Any>|&List? modification-func: Any) Any)
 (@def decompose-region (start: IntOrMarker end: IntOrMarker) Any)
 (@def compose-string (string: String &optional start: Integer? end: Integer? components: Integer|String|&Vector<Any>|&List? modification-func: Any) String)
 (@def decompose-string (string: String) String)
 (@def compose-chars (&rest args: &List<Integer|Cons<Symbol|Integer~Symbol|Integer>|Cons<Symbol|Integer~Cons<Symbol|Integer~Cons<Integer~Cons<Integer~Nil>>>>>) String)
 ;; The result is a complex value-dependent list whose structure depends
 ;; on DETAIL-P and whether a valid composition was found. The type
 ;; language cannot yet express value-dependent result structure.
 (@def find-composition (pos: IntOrMarker &optional limit: IntOrMarker? string: String? detail-p: Bool) Todo)
 (@def compose-chars-after (pos: IntOrMarker &optional limit: IntOrMarker? object: String?) Any)
 (@def compose-last-chars (args: &Cons<Symbol~&Cons<Integer~&Cons<Integer|String|&Vector<Any>|&List?~Nil>>>) Any))

;;; ============================================================
;;; Glyph-string accessors

(et-declare
 (@def lgstring-header (gstring: &Vector<Any>) Any)
 (@def lgstring-set-header (gstring: Vector<Any> header: [T]) T)
 (@def lgstring-font (gstring: &Vector<Any>) Font)
 (@def lgstring-char (gstring: &Vector<Any> i: Integer) Integer)
 (@def lgstring-char-len (gstring: &Vector<Any>) Integer)
 (@def lgstring-shaped-p (gstring: &Vector<Any>) Integer?)
 (@def lgstring-set-id (gstring: Vector<Any> id: [T]) T)
 (@def lgstring-glyph (gstring: &Vector<Any> i: Integer) Vector<Any>?)
 (@def lgstring-glyph-len (gstring: &Vector<Any>) Integer)
 (@def lgstring-set-glyph (gstring: Vector<Any> i: Integer glyph: [T]) T))

;;; ============================================================
;;; Glyph accessors

(et-declare
 (@def lglyph-from (glyph: &Vector<Any>) Integer)
 (@def lglyph-to (glyph: &Vector<Any>) Integer)
 (@def lglyph-char (glyph: &Vector<Any>) Integer)
 (@def lglyph-code (glyph: &Vector<Any>) Integer)
 (@def lglyph-width (glyph: &Vector<Any>) Integer)
 (@def lglyph-lbearing (glyph: &Vector<Any>) Integer)
 (@def lglyph-rbearing (glyph: &Vector<Any>) Integer)
 (@def lglyph-ascent (glyph: &Vector<Any>) Integer)
 (@def lglyph-descent (glyph: &Vector<Any>) Integer)
 (@def lglyph-adjustment (glyph: &Vector<Any>) Vector<Integer>?)
 (@def lglyph-set-from-to (glyph: Vector<Any> from: Integer to: [T]) T)
 (@def lglyph-set-char (glyph: Vector<Any> char: Integer) Integer)
 (@def lglyph-set-code (glyph: Vector<Any> code: Integer) Integer)
 (@def lglyph-set-width (glyph: Vector<Any> width: Integer) Integer)
 (@def lglyph-set-adjustment (glyph: Vector<Any> &optional xoff: Integer? yoff: Integer? wadjust: Integer?) Vector<Integer>)
 (@def lglyph-copy (glyph: [T]) (freshen-shallow T)))

;;; ============================================================
;;; Glyph-string composition

(et-declare
 (@def lgstring-insert-glyph (gstring: Vector<Any> idx: Integer glyph: Vector<Any>) Vector<Any>)
 (@def lgstring-remove-glyph (gstring: Vector<Any> idx: Integer) Vector<Any>)
 (@def lgstring-glyph-boundary (gstring: &Vector<Any> startpos: Integer endpos: Integer) Integer)
 (@def compose-glyph-string (gstring: [T] from: Integer to: Integer) T)
 (@def compose-glyph-string-relative (gstring: [T] from: Integer to: Integer &optional gap: Number?) T)
 (@def compose-gstring-for-graphic (gstring: Vector<Any> direction: Symbol?) Vector<Any>?)
 (@def compose-gstring-for-dotted-circle (gstring: [T] direction: Symbol?) T)
 (@def compose-gstring-for-terminal (gstring: Vector<Any> _direction: Symbol?) Vector<Any>)
 (@def compose-gstring-for-variation-glyph (gstring: Vector<Any> _direction: Symbol?) Vector<Any>?)
 (@def auto-compose-chars (func: Any from: IntOrMarker to: IntOrMarker font-object: FontObject? string: String? direction: Symbol?) Any))

;;; ============================================================
;;; Automatic composition mode

(et-declare
 (@def global-auto-composition-mode (&optional arg: Any) Any)
 (@def toggle-auto-composition (&optional arg: Any) Any))

;;; ============================================================
