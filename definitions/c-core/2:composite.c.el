;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Composition cache

(et-declare
 (@def clear-composition-cache () Nil))

;;; ============================================================
;;; Glyph strings

(et-declare
 (@def composition-get-gstring
       (from: IntOrMarker to: IntOrMarker font-object: FontObject|Terminal|Frame?
        &optional string: String?)
       Vector<Any>))

;;; ============================================================
;;; Composition operations

(et-declare
 (@def compose-region-internal
       (start: IntOrMarker end: IntOrMarker
        &optional components: Nil|Integer|&Cons|String|&Vector
        modification-func: Nil|AnyFn|Symbol)
       Nil)
 (@def compose-string-internal
       ([(<= S String)] string: S start: IntOrMarker end: IntOrMarker
        &optional components: Nil|Integer|&Cons|String|&Vector
        modification-func: Nil|AnyFn|Symbol)
       S)
 ;; The result has 5 mutually exclusive shapes depending on whether a
 ;; composition is found, whether it was found automatically, whether it
 ;; is valid, and whether DETAIL-P is set. The type language cannot yet
 ;; express result structure that branches this way on runtime values.
 (@def find-composition-internal
       (pos: IntOrMarker limit: IntOrMarker? string: String? detail-p: Bool)
       Todo))

;;; ============================================================
;;; Composition rules

(et-declare
 (@def composition-sort-rules (rules: List<Vector<Any>>) List<Vector<Any>>))

;;; ============================================================
