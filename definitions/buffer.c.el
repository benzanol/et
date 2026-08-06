;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Buffer discovery and creation

(et-declare
 (@def buffer-live-p (object: Any) Boolean)

 (@def buffer-list (&optional frame: Frame) ListFresh<Buffer>)

 (@def get-buffer (buffer-or-name: Buffer|String) Buffer|Nil)

 (@def get-file-buffer (filename: String) Buffer|Nil)

 (@def get-truename-buffer (filename: String) Buffer|Nil)

 (@def find-buffer (variable: Symbol value: Any) Buffer|Nil)

 (@def get-buffer-create
       (buffer-or-name: Buffer|String &optional inhibit-buffer-hooks: Any)
       Buffer)

 (@def make-indirect-buffer
       (base-buffer: Buffer|String name: String
        &optional clone: Any inhibit-buffer-hooks: Any)
       Buffer))

;;; ============================================================
;;; Buffer properties and modification state

(et-declare
 (@def generate-new-buffer-name (name: String &optional ignore: String) String)

 (@def buffer-name (&optional buffer: Buffer) String|Nil)

 (@def buffer-last-name (&optional buffer: Buffer) String)

 (@def buffer-file-name (&optional buffer: Buffer) String|Nil)

 (@def buffer-base-buffer (&optional buffer: Buffer) Buffer|Nil)

 ;; The result type is the type of the variable named by VARIABLE. The type
 ;; language cannot yet select a result type from a symbol argument's identity.
 (@def buffer-local-value (variable: Symbol buffer: Buffer) Todo)

 ;; Each association value has the type of its corresponding variable symbol.
 ;; The type language cannot express symbol-indexed types within a heterogeneous
 ;; collection.
 (@def buffer-local-variables (&optional buffer: Buffer)
       ListFresh<(or Symbol ConsFresh<Symbol~Todo>)>)

 (@def buffer-modified-p (&optional buffer: Buffer) Boolean|@autosaved)

 (@def force-mode-line-update ([T] &optional all: T) T)

 (@def set-buffer-modified-p (flag: Any) Nil)

 (@def restore-buffer-modified-p ([T] flag: T) T)

 (@def buffer-modified-tick (&optional buffer: Buffer) Integer)

 (@def internal--set-buffer-modified-tick
       (tick: Integer &optional buffer: Buffer)
       Nil)

 (@def buffer-chars-modified-tick (&optional buffer: Buffer) Integer))

;;; ============================================================
;;; Buffer lifecycle and editing state

(et-declare
 (@def rename-buffer (newname: String &optional unique: Any) String)

 (@def other-buffer
       (&optional buffer: Buffer visible-ok: Any frame: Frame)
       Buffer|Nil)

 (@def buffer-enable-undo (&optional buffer: Buffer|String) Nil)

 (@def kill-buffer (&optional buffer-or-name: Buffer|String) Boolean)

 (@def bury-buffer-internal (buffer: Buffer) Nil)

 (@def set-buffer-major-mode (buffer: Buffer) Nil)

 (@def current-buffer () Buffer)

 (@def set-buffer (buffer-or-name: Buffer|String) Buffer)

 (@def barf-if-buffer-read-only (&optional position: Integer) Nil)

 (@def erase-buffer () Nil)

 (@def buffer-swap-text (buffer: Buffer) Nil)

 (@def set-buffer-multibyte ([T] flag: T) T)

 (@def kill-all-local-variables (&optional kill-permanent: Any) Nil))

;;; ============================================================
;;; Overlays

(et-declare
 (@def overlayp (object: Any) Boolean)

 (@def make-overlay
       (beg: IntOrMarker end: IntOrMarker
        &optional buffer: Buffer front-advance: Any rear-advance: Any)
       Overlay)

 (@def move-overlay
       (overlay: Overlay beg: IntOrMarker end: IntOrMarker
        &optional buffer: Buffer)
       Overlay)

 (@def delete-overlay (overlay: Overlay) Nil)

 (@def delete-all-overlays (&optional buffer: Buffer) Nil)

 (@def overlay-start (overlay: Overlay) Integer|Nil)

 (@def overlay-end (overlay: Overlay) Integer|Nil)

 (@def overlay-buffer (overlay: Overlay) Buffer|Nil)

 (@def overlay-properties (overlay: Overlay)
       (and ListFresh PlistOf<Any~Any>))

 (@def overlays-at (pos: IntOrMarker &optional sorted: Any)
       ListFresh<Overlay>)

 (@def overlays-in (beg: IntOrMarker end: IntOrMarker) ListFresh<Overlay>)

 (@def next-overlay-change (pos: IntOrMarker) Integer)

 (@def previous-overlay-change (pos: IntOrMarker) Integer)

 (@def overlay-lists ()
       (and ListFresh<ListFresh<Overlay>> (Tuple ListFresh<Overlay>)))

 (@def overlay-recenter (pos: IntOrMarker) Nil)

 (@def overlay-get (overlay: Overlay prop: Any) Any)

 (@def overlay-put ([T] overlay: Overlay prop: Any value: T) T)

 ;; The result is a recursively nested three-element tree whose nodes are
 ;; fixed property lists. The type language cannot express an anonymous
 ;; recursive result type without introducing a supporting alias.
 (@def overlay-tree (&optional buffer: Buffer) Todo))

;;; ============================================================
