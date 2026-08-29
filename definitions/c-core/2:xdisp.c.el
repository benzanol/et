;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Redisplay

(et-declare
 (@def set-buffer-redisplay (symbol: Symbol newval: Any op: VariableEvent where: Buffer?) Nil)
 (@def line-pixel-height () Integer)
 (@def remember-mouse-glyph (frame: Frame? x: Integer y: Integer)
       Tuple<Integer~Integer~Integer~Integer>))

;;; ============================================================
;;; Display properties and measurements

(et-declare
 (@def get-display-property
       ([] position: IntOrMarker prop: Symbol &optional object: String|Buffer? properties: List?)
       Any)
 (@def window-text-pixel-size
       (&optional window: Window? from: IntOrMarker|True|Cons<IntOrMarker~Integer>? to: IntOrMarker|True?
        x-limit: Integer|True? y-limit: Integer?
        mode-lines: @mode-line|@tab-line|@header-line|True? ignore-line-at-end: Bool)
       Cons<Integer~Integer>|Tuple<Integer~Integer~Integer>)
 (@def buffer-text-pixel-size
       (&optional buffer-or-name: Buffer|String? window: Window? x-limit: Integer|True? y-limit: Integer?)
       Cons<Integer~Integer>))

;;; ============================================================
;;; Tab and tool bars

(et-declare
 (@def tab-bar-height (&optional frame: Frame? pixelwise: Bool) Integer)
 (@def tool-bar-height (&optional frame: Frame? pixelwise: Bool) Integer))

;;; ============================================================
;;; Long-line optimizations

(et-declare
 (@def long-line-optimizations-p () Boolean))

;;; ============================================================
;;; Bidirectional display

(et-declare
 (@def current-bidi-paragraph-direction (&optional buffer: Buffer?) @left-to-right|@right-to-left)
 (@def bidi-find-overridden-directionality
       (from: IntOrMarker to: IntOrMarker object: String|Buffer|Window?
        &optional base-dir: @left-to-right|@right-to-left?)
       Integer?)
 (@def move-point-visually (direction: Integer) Integer)
 (@def bidi-resolved-levels (&optional vpos: Integer?) Vector<Integer>?))

;;; ============================================================
;;; Mode line formatting

(et-declare
 (@def format-mode-line
       (format: Any
        &optional face: True|Integer|@default|@mode-line-active|@mode-line-inactive|@header-line-active|@header-line-inactive|@tab-line-active|@tab-line-inactive|@tab-bar|@tool-bar?
        window: Window? buffer: Buffer?)
       String))

;;; ============================================================
;;; Text visibility

(et-declare
 (@def invisible-p (pos: Any) True|Integer?))

;;; ============================================================
;;; Image maps

(et-declare
 (@def lookup-image-map (map: &List? x: Integer y: Integer) Any))

;;; ============================================================
