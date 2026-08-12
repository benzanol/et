;; -*- lexical-binding: t; -*-


;;; ============================================================
;;; Strings and bool vectors

(et-declare
 (@def make-string (length: Integer init: Integer &optional multibyte: Any) String)
 (@def make-bool-vector (length: Integer init: Any) BoolVector)
 (@def bool-vector (&rest objects: &List) BoolVector))


;;; ============================================================
;;; Conses and lists

(et-declare
 (@def cons ([L R] car: L cdr: R) ConsFresh<L~R>)
 (@def list (&rest objects: [T]) (freshen-shallow T))
 (@def make-list ([T] length: Integer init: T) ListFresh<T>))


;;; ============================================================
;;; Records, vectors, and byte code

(et-declare
 ;; nontrivial
 (@def make-record (type: Any slots: Integer init: Any) (Emacs record))
 (@def record (type: Any &rest slots: &List) (Emacs record))
 (@def make-vector ([T] length: Integer init: T) VectorFresh<T>)
 (@def vector ([T] &rest objects: &List<T>) VectorFresh<T>)
 (@def make-byte-code
       ([] arglist: Integer|&List<Sexp> byte-code: String constants: &Vector
        depth: Integer &optional docstring: Any interactive-spec: Any
        &rest elements: &List)
       ByteCodeFunction)
 (@def make-closure (prototype: ByteCodeFunction &rest closure-vars: &List)
       ByteCodeFunction))


;;; ============================================================
;;; Symbols and miscellaneous objects

(et-declare
 (@def make-symbol (name: String) Var)
 (@def make-marker () Marker)
 (@def make-finalizer (function: fn<Nil~Any>) Finalizer))


;;; ============================================================
;;; Pure storage

(et-declare
 ;; nontrivial
 (@def purecopy (obj: [T]) (freshen-deep T)))


;;; ============================================================
;;; Garbage collection

(et-declare
 (@def garbage-collect ()
       (or Nil (ListFresh (or (Tuple Symbol Integer Integer)
                              (Tuple Symbol Integer Integer Integer)))))
 (@def garbage-collect-maybe (factor: Integer) Boolean))


;;; ============================================================
;;; Memory information

(et-declare
 (@def memory-info () (or Nil (Tuple Integer Integer Integer Integer)))
 (@def memory-use-counts () (Tuple Integer Integer Integer Integer Integer Integer Integer))
 (@def malloc-info () Nil)
 (@def malloc-trim (&optional leave-padding: Integer) Boolean))


;;; ============================================================
