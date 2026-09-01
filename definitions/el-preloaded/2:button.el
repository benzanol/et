;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Globals

(et-declare
 (@def button-mode (&optional arg: Any) Boolean))

;;; ============================================================
;;; Button types

(et-declare
 (@def button-category-symbol (type: Symbol) Symbol)
 (@def define-button-type ([N] name: N &rest properties: &List<Any>) N)
 (@def button-type-put ([V] type: Symbol prop: Symbol val: V) V)
 (@def button-type-get (type: Symbol prop: Symbol) Any)
 (@def button-type-subtype-p (type: Symbol? supertype: Symbol) Boolean))

;;; ============================================================
;;; Button properties and other attributes

(et-declare
 (@def button-start (button: Overlay|IntOrMarker) Integer)
 (@def button-end (button: Overlay|IntOrMarker) Integer)
 (@def button-get (button: Overlay|IntOrMarker|Cons<String~Integer> prop: Symbol) Any)
 (@def button-put (button: Overlay|IntOrMarker|Cons<String~Integer> prop: Symbol val: Any) Any)
 (@def button-activate
       (button: Overlay|IntOrMarker|Cons<String~Integer> &optional use-mouse-action: Bool)
       Any)
 (@def button-label (button: Overlay|IntOrMarker|Cons<String~Integer>) String)
 (@def button-type (button: Overlay|IntOrMarker|Cons<String~Integer>) Symbol)
 (@def button-has-type-p (button: Overlay|IntOrMarker|Cons<String~Integer> type: Symbol) Boolean))

;;; ============================================================
;;; Creating overlay buttons

(et-declare
 (@def make-button (beg: IntOrMarker end: IntOrMarker &rest properties: &List<Any>) Overlay)
 (@def insert-button (label: String &rest properties: &List<Any>) Overlay))

;;; ============================================================
;;; Creating text-property buttons

(et-declare
 (@def make-text-button
       ([(<= B String|IntOrMarker)] beg: B end: IntOrMarker? &rest properties: &List<Any>)
       (extends? B String String B))
 (@def insert-text-button (label: String &rest properties: &List<Any>) Integer))

;;; ============================================================
;;; Finding buttons in a buffer

(et-declare
 (@def button-at (pos: IntOrMarker) Overlay|Marker?)
 (@def next-button (pos: IntOrMarker &optional count-current: Bool) Overlay|Marker?)
 (@def previous-button (pos: IntOrMarker &optional count-current: Bool) Overlay|Marker?))

;;; ============================================================
;;; User commands

(et-declare
 ;; POS may also be a mouse-event or touchscreen-down event, an object
 ;; shape the type system does not yet model.
 (@def push-button (&optional pos: (or Nil IntOrMarker Todo) use-mouse-action: Bool) Any)
 (@def forward-button
       (n: Integer &optional wrap: Bool display-message: Bool no-error: Bool)
       Overlay|Marker?)
 (@def backward-button
       (n: Integer &optional wrap: Bool display-message: Bool no-error: Bool)
       Overlay|Marker?)
 (@def button-describe (&optional button-or-pos: IntOrMarker|Overlay?) Boolean)
 (@def [buttonize button-buttonize]
       (string: String callback: fn1<Any> &optional data: Any help-echo: Any)
       String)
 (@def buttonize-region
       (start: IntOrMarker end: IntOrMarker callback: fn1<Any> &optional data: Any help-echo: Any)
       Nil))

;;; ============================================================
