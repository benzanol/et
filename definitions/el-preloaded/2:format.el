;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Basic functions

(et-declare
 (@def format-encode-run-method
       (method: (or String (fn (Args IntOrMarker IntOrMarker Buffer) (or Integer List<Cons<IntOrMarker~String>>)))
        from: IntOrMarker to: IntOrMarker &optional buffer: Buffer?)
       (or Nil Integer List<Cons<IntOrMarker~String>>))
 (@def format-decode-run-method
       (method: (or String (fn (Args IntOrMarker IntOrMarker) Integer))
        from: IntOrMarker to: IntOrMarker &optional _buffer: Buffer?)
       Integer)
 ;; The return value is whatever TO-FN produces, and TO-FN is looked up at
 ;; runtime from `format-alist' (an untyped, per-FORMAT data table), not
 ;; passed in as a typed parameter. The type language cannot yet track
 ;; values retrieved dynamically from untyped data structures.
 (@def format-annotate-function
       (format: Symbol from: IntOrMarker to: IntOrMarker orig-buf: Buffer format-count: Integer)
       Todo)
 (@def format-decode (format: Symbol|List<Symbol> length: Integer &optional visit-flag: Bool) Integer))

;;; ============================================================
;;; Interactive functions and entry points

(et-declare
 (@def format-decode-buffer (&optional format: Symbol|List<Symbol>) Integer)
 (@def format-decode-region (from: IntOrMarker to: IntOrMarker &optional format: Symbol|List<Symbol>) Integer)
 (@def format-encode-buffer (&optional format: Symbol|List<Symbol>) Nil)
 (@def format-encode-region (beg: IntOrMarker end: IntOrMarker &optional format: Symbol|List<Symbol>) Nil)
 (@def format-write-file (filename: String format: Symbol|List<Symbol> &optional confirm: Bool) Nil)
 (@def format-find-file (filename: String format: Symbol|List<Symbol>) Nil|Integer)
 (@def format-insert-file (filename: String format: Symbol|List<Symbol> &optional beg: Integer? end: Integer?) (Tuple String Integer))
 (@def format-read (&optional prompt: String?) List<Symbol>))

;;; ============================================================
;;; Encoding and decoding helpers

(et-declare
 (@def format-replace-strings
       (alist: &List<&Cons<String~String>> &optional reverse: Bool beg: IntOrMarker? end: IntOrMarker?)
       Nil))

;;; ============================================================
;;; List-manipulation functions

(et-declare
 (@def format-delq-cons ([E] cons: List<E> list: List<E>) List<E>)
 (@def format-make-relatively-unique ([E] a: &List<E> b: &List<E>) Cons<List<E>~List<E>>)
 (@def format-reorder ([E] items: List<E> order: &List<E>) List<E>))

;;; ============================================================
;;; Decoding

(et-declare
 ;; TRANSLATIONS is an alist keyed by property name, but the value
 ;; attached to each key has a shape that varies by case (a list of
 ;; annotation-name symbols, a numeric-increment entry, or a pseudo-property
 ;; default-function marker). The type language cannot yet express this
 ;; value-dependent nested alist shape.
 (@def format-deannotate-region
       (from: IntOrMarker to: IntOrMarker translations: &List<&Cons<Symbol~Todo>>
        next-fn: (fn Nil (or (Tuple IntOrMarker IntOrMarker Symbol Bool) Nil)))
       Nil|String)
 (@def format-subtract-regions
       (minu: &List<&Cons<Integer~Integer?>> subtra: &List<&Cons<Integer~Integer>>)
       List<Cons<Integer~Integer?>>)
 (@def format-property-increment-region
       (from: IntOrMarker to: IntOrMarker prop: Symbol delta: Number default: Number)
       Nil))

;;; ============================================================
;;; Encoding

(et-declare
 (@def format-insert-annotations
       (list: &List<&Cons<IntOrMarker~String>> &optional offset: Integer?)
       Nil)
 (@def format-annotate-value
       ([T R] old: T new: R)
       (Cons (or Nil (Tuple T)) (or Nil (Tuple R))))
 ;; TRANSLATIONS has the same value-dependent nested alist shape described
 ;; above `format-deannotate-region'; the type language cannot yet express
 ;; it. FORMAT-FN's first argument is an annotation name drawn from that
 ;; same unexpressable structure, and its return value is caller-defined
 ;; (a string or an arbitrary annotation representation).
 (@def format-annotate-region
       (from: IntOrMarker to: IntOrMarker translations: &List<&Cons<Symbol~Todo>>
        format-fn: (fn (Args Todo Boolean) Any) ignore: &List<Symbol>)
       List<Cons<IntOrMarker~Any>>))

;;; ============================================================
;;; Internal functions for format-annotate-region

(et-declare
 ;; TRANSLATIONS has the same value-dependent nested alist shape described
 ;; above `format-deannotate-region'; the type language cannot yet express
 ;; it.
 (@def format-annotate-location
       (loc: IntOrMarker all: Bool ignore: &List<Symbol> translations: &List<&Cons<Symbol~Todo>>)
       Vector<Any>)
 ;; TRANSLATIONS has the same value-dependent nested alist shape described
 ;; above `format-deannotate-region'; the type language cannot yet express
 ;; it.
 (@def format-annotate-single-property-change
       (prop: Symbol old: Any new: Any translations: &List<&Cons<Symbol~Todo>>)
       Nil|Cons<List<Any>~List<Any>>)
 ;; PROP-ALIST is the per-value entry drawn from the TRANSLATIONS structure
 ;; described above `format-deannotate-region'; its shape cannot be
 ;; expressed by the current type language. The default-function branch may
 ;; also return an arbitrary caller-supplied value.
 (@def format-annotate-atomic-property-change
       (prop-alist: Todo old: Any new: Any)
       Any))

;;; ============================================================
