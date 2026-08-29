;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Keymap construction

(et-declare
 (@def make-keymap (&optional string: String?) ConsFresh<@keymap~Any>)
 (@def make-sparse-keymap (&optional string: String?) ConsFresh<@keymap~Any>)
 (@def keymapp ([T] object: T) (is-a? T Cons<@keymap~Any>|Symbol))
 (@def keymap-prompt (map: &Cons<@keymap~Any>|Symbol) String?))

;;; ============================================================
;;; Keymap inheritance

(et-declare
 (@def keymap-parent (keymap: &Cons<@keymap~Any>|Symbol) &Cons<@keymap~Any>|Symbol?)
 (@def set-keymap-parent
       (keymap: Cons<@keymap~Any>|Symbol parent: [(<= P Cons<@keymap~Any>|Symbol?)])
       P))

;;; ============================================================
;;; Keymap traversal and copying

(et-declare
 (@def map-keymap-internal
       (function: (fn (Args Any Any) Any) keymap: &Cons<@keymap~Any>|Symbol)
       &Cons<@keymap~Any>|Symbol?)
 (@def map-keymap
       (function: (fn (Args Any Any) Any) keymap: &Cons<@keymap~Any>|Symbol
                  &optional sort_first: Bool)
       Any)
 (@def copy-keymap (keymap: &Cons<@keymap~Any>|Symbol) ConsFresh<@keymap~Any>))

;;; ============================================================
;;; Key definitions and lookup

(et-declare
 (@def define-key
       (keymap: Cons<@keymap~Any>|Symbol key: String|Vector<Any> def: [D]
        &optional remove: Bool)
       D|Nil)
 (@def command-remapping
       (command: Any
        &optional position: Any
        keymaps: &Cons<@keymap~Any>|Symbol|&List<&Cons<@keymap~Any>|Symbol>?)
       Symbol?)
 (@def lookup-key
       (keymap: &Cons<@keymap~Any>|Symbol|&List<&Cons<@keymap~Any>|Symbol>?
                key: String|&Vector<Any> &optional accept_default: Bool)
       Any))

;;; ============================================================
;;; Current keymaps and bindings

(et-declare
 (@def current-active-maps (&optional olp: Bool position: Any)
       ListFresh<Cons<@keymap~Any>|Symbol>)
 (@def key-binding
       (key: String|Vector<Any>
        &optional accept_default: Bool no_remap: Bool position: Any)
       Any)
 (@def minor-mode-key-binding (key: String|Vector<Any> &optional accept_default: Bool)
       ListFresh<ConsFresh<Symbol~Any>>)
 (@def use-global-map (keymap: Cons<@keymap~Any>|Symbol) Nil)
 (@def use-local-map (keymap: Cons<@keymap~Any>|Symbol?) Nil)
 (@def current-local-map () Cons<@keymap~Any>|Symbol?)
 (@def current-global-map () Cons<@keymap~Any>|Symbol)
 (@def current-minor-mode-maps () ListFresh<Cons<@keymap~Any>|Symbol>))

;;; ============================================================
;;; Keymap discovery and descriptions

(et-declare
 (@def accessible-keymaps
       (keymap: &Cons<@keymap~Any>|Symbol &optional prefix: String|&Vector<Any>?)
       ListFresh<ConsFresh<Vector<Any>~Cons<@keymap~Any>|Symbol>>)
 (@def key-description
       (keys: String|Vector<Any>|List<Any>? &optional prefix: String|Vector<Any>|List<Any>?)
       String)
 (@def single-key-description (key: Integer|Symbol|String|Cons &optional no_angles: Bool)
       String)
 (@def text-char-description (character: Integer) String)
 (@def where-is-internal
       (definition: Any
        &optional keymap: &Cons<@keymap~Any>|Symbol|&List<&Cons<@keymap~Any>|Symbol>?
        firstonly: [F] noindirect: Bool no_remap: Bool)
       (if-nil? F ListFresh<Vector<Any>> Vector<Any>?))
 (@def describe-buffer-bindings
       (buffer: Buffer &optional prefix: String|Vector<Any>|List<Any>? menus: Bool)
       Nil)
 (@def describe-vector (vector: &Vector<Any>|CharTable &optional describer: AnyFn?) Nil))

;;; ============================================================
