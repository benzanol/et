;; -*- lexical-binding: t; -*-


;;; ============================================================
;;; Utilities

(et-declare
 (@def dump-colors () Nil))


;;; ============================================================
;;; Frames and faces

(et-declare
 (@def clear-face-cache (&optional thoroughly: Bool) Nil))


;;; ============================================================
;;; X pixmaps

(et-declare
 (@def bitmap-spec-p (object: Any) Boolean))


;;; ============================================================
;;; Color handling

(et-declare
 (@def color-values-from-color-spec (spec: String) (or Nil (Tuple Integer Integer Integer)))
 (@def color-gray-p (color: String &optional frame: Frame?) Boolean)
 (@def color-supported-p (color: String &optional frame: Frame? background-p: Bool) Boolean))


;;; ============================================================
;;; XLFD font names

(et-declare
 ;; Each element is a fixed-format 8-slot vector with a distinct type at
 ;; each position (family: String, width/weight/slant: Symbol,
 ;; point-size: Integer, fixed-p: Boolean, full: String, registry: String).
 ;; Vector<E> only expresses one homogeneous element type; the type
 ;; language has no per-position vector-tuple facility (unlike Tuple for
 ;; Cons chains).
 (@def x-family-fonts (&optional family: String? frame: Frame?) List<Vector<Any>>)
 (@def x-list-fonts
       (pattern: String &optional face: Symbol? frame: Frame?
                 maximum: Integer? width: Integer?)
       List<String>))


;;; ============================================================
;;; Lisp faces

(et-declare
 (@def internal-make-lisp-face (face: Symbol &optional frame: Frame?) Vector)
 (@def internal-lisp-face-p (face: Symbol|String &optional frame: Frame?) Vector?)
 (@def internal-copy-lisp-face
       ([T] from: Symbol to: T frame: True|Frame &optional new-frame: Frame?)
       T)
 (@def internal-set-lisp-face-attribute
       (face: Symbol attr: Symbol value: Any &optional frame: True|Integer|Frame?)
       Symbol)
 (@def internal-face-x-get-resource
       (resource: String class: String &optional frame: Frame?)
       String?)
 (@def internal-set-lisp-face-attribute-from-resource
       (face: Symbol attr: Symbol value: String &optional frame: True|Integer|Frame?)
       Symbol)
 (@def face-attribute-relative-p (attribute: Any value: Any) Boolean)
 (@def merge-face-attribute (attribute: Any value1: Any value2: Any) Any)
 (@def internal-get-lisp-face-attribute
       (symbol: Symbol keyword: Symbol &optional frame: True|Frame?)
       Any)
 (@def internal-lisp-face-attribute-values (attr: Symbol) (or Nil (Tuple True Nil)))
 (@def internal-merge-in-global-face (face: Symbol frame: Frame) Nil)
 (@def face-font
       (face: Symbol &optional frame: True|Frame? character: Integer?)
       Nil|String|List<Symbol>)
 (@def internal-lisp-face-equal-p
       (face1: Symbol face2: Symbol &optional frame: True|Frame?)
       Boolean)
 (@def internal-lisp-face-empty-p (face: Symbol &optional frame: True|Frame?) Boolean))


;;; ============================================================
;;; Realized faces

(et-declare
 (@def color-distance
       ([R] color1: (or String &ColorTriple)
        color2: (or String &ColorTriple)
        &optional frame: Frame?
        metric: (or Nil (fn (Args &ColorTriple &ColorTriple) R)))
       Integer|R))


;;; ============================================================
;;; Face cache

(et-declare
 (@def face-attributes-as-vector (plist: PlistOf<Symbol~Any>) Vector))


;;; ============================================================
;;; Face capability testing

(et-declare
 (@def display-supports-face-attributes-p
       (attributes: PlistOf<Symbol~Any> &optional display: Any)
       Boolean))


;;; ============================================================
;;; Font selection

(et-declare
 (@def internal-set-font-selection-order (order: &List<Symbol>) Nil)
 (@def internal-set-alternative-font-family-alist (alist: &List<&List<String>>) List<List<Symbol>>)
 (@def internal-set-alternative-font-registry-alist (alist: &List<&List<String>>) List<List<String>>))


;;; ============================================================
;;; Face realization

(et-declare
 (@def tty-suppress-bold-inverse-default-colors ([T] suppress: T) T))


;;; ============================================================
;;; Computing faces

(et-declare
 (@def x-load-color-file (filename: String) List<Cons<String~Integer>>))


;;; ============================================================
;;; Tests

(et-declare
 (@def dump-face (&optional n: Integer?) Nil)
 (@def show-face-resources () Nil))


;;; ============================================================
