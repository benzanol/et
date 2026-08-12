;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Frame basics and terminal creation

(et-declare
 (@def framep (object: Any) True|@x|@w32|@ns|@pc|@pgtk|@haiku|@android?)
 (@def frame-live-p (object: Any) True|@x|@w32|@ns|@pc|@pgtk|@haiku|@android?)
 (@def frame-id (&optional frame: Frame?) Integer?)
 (@def window-system (&optional frame: Frame?) @x|@w32|@ns|@pc|@pgtk|@haiku|@android?)
 (@def frame-windows-min-size
       (frame: Frame horizontal: Bool ignore: Any pixelwise: Bool)
       Integer)
 (@def make-terminal-frame (parms: Alist<Symbol~Any>) Frame))


;;; ============================================================
;;; Frame selection and traversal

(et-declare
 (@def select-frame (frame: Frame &optional norecord: Bool) Frame?)
 (@def handle-switch-frame (event: Frame|Cons<@switch-frame~Cons<Frame~Any>>) Frame?)
 (@def selected-frame () Frame)
 (@def old-selected-frame () Frame)
 (@def frame-list () List<Frame>)
 (@def frame-parent (&optional frame: Frame?) Frame?)
 (@def frame-ancestor-p (ancestor: Frame? descendant: Frame?) Boolean)
 (@def frame-root-frame (&optional frame: Frame?) Frame)
 (@def next-frame (&optional frame: Frame? miniframe: Any) Frame)
 (@def previous-frame (&optional frame: Frame? miniframe: Any) Frame)
 (@def last-nonminibuffer-frame () Frame?))


;;; ============================================================
;;; Frame lifecycle, mouse, and focus

(et-declare
 (@def delete-frame (&optional frame: Frame? force: Bool) Nil)
 ;; When `mouse-position-function' is non-nil, the result is replaced
 ;; by whatever that function returns. The type system cannot express
 ;; a return type produced by invoking an arbitrary function stored in
 ;; a mutable global variable.
 (@def mouse-position () Todo)
 ;; See `mouse-position': the result can be replaced by an arbitrary
 ;; call to `mouse-position-function'.
 (@def mouse-pixel-position () Todo)
 (@def set-mouse-position (frame: Frame x: Integer y: Integer) Nil)
 (@def set-mouse-pixel-position (frame: Frame x: Integer y: Integer) Nil)
 (@def make-frame-visible (&optional frame: Frame?) Frame)
 (@def make-frame-invisible (&optional frame: Frame? force: Bool) Nil)
 (@def iconify-frame (&optional frame: Frame?) Nil)
 (@def frame-visible-p (frame: Frame) True|@icon?)
 (@def visible-frame-list () List<Frame>)
 (@def raise-frame (&optional frame: Frame?) Nil)
 (@def lower-frame (&optional frame: Frame?) Nil)
 (@def redirect-frame-focus (frame: Frame &optional focus-frame: Frame?) Nil)
 (@def frame-focus (&optional frame: Frame?) Frame?)
 (@def x-focus-frame (frame: Frame? &optional noactivate: Bool) Nil)
 (@def frame-after-make-frame ([T] frame: Frame? made: T) T))


;;; ============================================================
;;; Frame parameters and geometry

(et-declare
 (@def frame-parameters (&optional frame: Frame?) Alist<Symbol~Any>?)
 (@def frame-parameter (frame: Frame? parameter: Symbol) Any)
 (@def modify-frame-parameters (frame: Frame? alist: Alist<Symbol~Any>) Nil)
 (@def frame-char-height (&optional frame: Frame?) Integer)
 (@def frame-char-width (&optional frame: Frame?) Integer)
 (@def frame-native-width (&optional frame: Frame?) Integer)
 (@def frame-native-height (&optional frame: Frame?) Integer)
 (@def tool-bar-pixel-width (&optional frame: Frame?) Integer)
 (@def frame-text-cols (&optional frame: Frame?) Integer)
 (@def frame-text-lines (&optional frame: Frame?) Integer)
 (@def frame-total-cols (&optional frame: Frame?) Integer)
 (@def frame-total-lines (&optional frame: Frame?) Integer)
 (@def frame-text-width (&optional frame: Frame?) Integer)
 (@def frame-text-height (&optional frame: Frame?) Integer)
 (@def frame-scroll-bar-width (&optional frame: Frame?) Integer)
 (@def frame-scroll-bar-height (&optional frame: Frame?) Integer)
 (@def frame-fringe-width (&optional frame: Frame?) Integer)
 (@def frame-child-frame-border-width (&optional frame: Frame?) Integer)
 (@def frame-internal-border-width (&optional frame: Frame?) Integer)
 (@def frame-right-divider-width (&optional frame: Frame?) Integer)
 (@def frame-bottom-divider-width (&optional frame: Frame?) Integer)
 (@def set-frame-height
       (frame: Frame? height: Integer &optional pretend: Bool pixelwise: Bool)
       Nil)
 (@def set-frame-width
       (frame: Frame? width: Integer &optional pretend: Bool pixelwise: Bool)
       Nil)
 (@def set-frame-size
       (frame: Frame? width: Integer height: Integer &optional pixelwise: Bool)
       Nil)
 (@def frame-position (&optional frame: Frame?) Cons<Integer~Integer>)
 (@def set-frame-position (frame: Frame? x: Integer y: Integer) True)
 (@def set-frame-size-and-position-pixelwise
       (frame: Frame? width: Integer height: Integer x: Integer y: Integer
        &optional gravity: Integer?)
       Nil)
 (@def frame-window-state-change (&optional frame: Frame?) Boolean)
 (@def set-frame-window-state-change (&optional frame: Frame? arg: Bool) Boolean)
 (@def frame-scale-factor (&optional frame: Frame?) Number))


;;; ============================================================
;;; X resources

(et-declare
 (@def x-get-resource
       (attribute: String class: String &optional component: String? subclass: String?)
       String?)
 (@def x-parse-geometry (string: String)
       Alist<@left|@top|@width|@height~Integer|Tuple<@plus|@minus~Integer>>))


;;; ============================================================
;;; Pointer visibility and fonts

(et-declare
 (@def frame-pointer-visible-p (&optional frame: Frame?) Boolean)
 (@def mouse-position-in-root-frame () Cons<Integer~Integer>?)
 (@def reconsider-frame-fonts (frame: Frame?) Nil))

;;; ============================================================
