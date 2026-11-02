;; -*- lexical-binding: t; -*-

(et-declare
 (@check thread-first)
 (@check thread-last)
 (@def hash-table-empty-p (hash-table: &HashTable) Boolean)
 (@def hash-table-keys
       ([K V] hash-table: &HashTable<K_V>)
       ListFresh<K>)
 (@def hash-table-values
       ([K V] hash-table: &HashTable<K_V>)
       ListFresh<V>)
 (@def string-join
       (strings: &MapSeq<String> &optional separator: &OrdSeq<Integer>?)
       String)
 (@def string-reverse
       (seq: [S <: &OrdSeq])
       (freshen-shallow S))
 (@def string-truncate-left (string: String length: Integer) String)
 (@def string-blank-p (string: String) Integer?)
 (@def string-remove-prefix (prefix: String string: String) String)
 (@def string-remove-suffix (suffix: String string: String) String)
 (@def string-clean-whitespace (string: String) String)
 (@def string-fill (string: String width: Integer) String)
 (@def string-limit
       (string: String length: Integer
        &optional end: Bool coding-system: Symbol?)
       String)
 (@def string-pad
       (string: String length: Integer
        &optional padding: Integer? start: Bool)
       String)
 (@def string-chop-newline (string: String) String)
 (@def replace-region-contents
       (beg: IntOrMarker end: IntOrMarker
        replace-fn: (fn Nil String|Buffer)
        &optional max-secs: Number? max-costs: Integer?)
       Boolean)
 (@check named-let)
 (@def string-pixel-width (string: String) Integer)
 (@def string-glyph-split (string: String) ListFresh<String>)
 (@def add-display-text-property
       ([(Idx <: IntOrMarker)] start: Idx end: Idx prop: Symbol value: Any
        &optional object: StringOrBuffer<Idx>?)
       Nil)
 (@def read-process-name (prompt: String) Process)
 (@check with-buffer-unmodified-if-unchanged ($body)))
