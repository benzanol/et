;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Text property access

(et-declare
 (@def text-properties-at (position: IntOrMarker &optional object: String|Buffer?) &PlistOf<Symbol~Any>)
 (@def get-text-property (position: IntOrMarker prop: Symbol &optional object: String|Buffer?) Any)
 (@def get-char-property (position: IntOrMarker prop: Symbol &optional object: String|Buffer|Window?) Any)
 (@def get-char-property-and-overlay
       (position: IntOrMarker prop: Symbol &optional object: String|Buffer|Window?)
       ConsFresh<Any~Overlay?>))

;;; ============================================================
;;; Property changes

(et-declare
 (@def next-char-property-change (position: IntOrMarker &optional limit: IntOrMarker?) Integer)
 (@def previous-char-property-change (position: IntOrMarker &optional limit: IntOrMarker?) Integer)
 (@def next-single-char-property-change
       (position: IntOrMarker prop: Symbol &optional object: String|Buffer? limit: IntOrMarker?)
       Integer)
 (@def previous-single-char-property-change
       (position: IntOrMarker prop: Symbol &optional object: String|Buffer? limit: IntOrMarker?)
       Integer)
 (@def next-property-change
       (position: IntOrMarker &optional object: String|Buffer? limit: IntOrMarker|True?)
       Integer?)
 (@def next-single-property-change
       (position: IntOrMarker prop: Symbol &optional object: String|Buffer? limit: IntOrMarker?)
       Integer?)
 (@def previous-property-change
       (position: IntOrMarker &optional object: String|Buffer? limit: IntOrMarker?)
       Integer?)
 (@def previous-single-property-change
       (position: IntOrMarker prop: Symbol &optional object: String|Buffer? limit: IntOrMarker?)
       Integer?))

;;; ============================================================
;;; Text property modification

(et-declare
 (@def add-text-properties
       (start: IntOrMarker end: IntOrMarker properties: &PlistOf<Symbol~Any> &optional object: String|Buffer?)
       Boolean)
 (@def put-text-property
       (start: IntOrMarker end: IntOrMarker property: Symbol value: Any &optional object: String|Buffer?)
       Nil)
 (@def set-text-properties
       (start: IntOrMarker end: IntOrMarker properties: &PlistOf<Symbol~Any> &optional object: String|Buffer?)
       Boolean)
 (@def add-face-text-property
       (start: IntOrMarker end: IntOrMarker face: Any &optional append: Bool object: String|Buffer?)
       Nil)
 (@def remove-text-properties
       (start: IntOrMarker end: IntOrMarker properties: &PlistOf<Symbol~Any> &optional object: String|Buffer?)
       Boolean)
 (@def remove-list-of-text-properties
       (start: IntOrMarker end: IntOrMarker list-of-properties: &List<Symbol> &optional object: String|Buffer?)
       Boolean))

;;; ============================================================
;;; Text property search

(et-declare
 (@def text-property-any
       (start: IntOrMarker end: IntOrMarker property: Symbol value: Any &optional object: String|Buffer?)
       Integer?)
 (@def text-property-not-all
       (start: IntOrMarker end: IntOrMarker property: Symbol value: Any &optional object: String|Buffer?)
       Integer?))

;;; ============================================================
