;; -*- lexical-binding: t; -*-


;;; ============================================================
;;; Basic utilities

(et-declare
 (@def identity ([T] argument: T) T)
 (@def random (&optional limit: (or Nil True String (and Integer Positive))) Integer))


;;; ============================================================
;;; Lengths and string comparison

(et-declare
 (@def length (sequence: (or &Seq ByteCodeFunction (Emacs record))) Integer)
 (@def safe-length (list: Any) Integer)
 (@def length< (sequence: (or &Seq ByteCodeFunction (Emacs record)) length: Integer) Boolean)
 (@def length> (sequence: (or &Seq ByteCodeFunction (Emacs record)) length: Integer) Boolean)
 (@def length= (sequence: (or &Seq ByteCodeFunction (Emacs record)) length: Integer) Boolean)

 (@def proper-list-p (object: Any) Integer|Nil)
 (@def string-bytes (string: String) Integer)
 (@def string-distance
       (string1: String string2: String &optional bytecompare: Boolean)
       Integer)
 (@def string-equal (s1: Symbol|String s2: Symbol|String) Boolean)
 (@def compare-strings
       (str1: String start1: Integer|Nil end1: Integer|Nil
              str2: String start2: Integer|Nil end2: Integer|Nil
              &optional ignore-case: Boolean)
       Integer|True)
 (@def string-lessp
       (string1: Symbol|String string2: Symbol|String)
       Boolean)
 (@def string-version-lessp
       (string1: Symbol|String string2: Symbol|String)
       Boolean)
 (@def string-collate-lessp
       (s1: Symbol|String s2: Symbol|String
            &optional locale: String|Nil ignore-case: Boolean)
       Boolean)
 (@def string-collate-equalp
       (s1: Symbol|String s2: Symbol|String
            &optional locale: String|Nil ignore-case: Boolean)
       Boolean))


;;; ============================================================
;;; Sequence construction and copying

(et-declare
 ;; nontrivial
 (@def append ([E] &rest sequences: &ListWithLast<&List<E>~List<E>>) List<E>)
 (@def concat
       (&rest sequences: &List<(or String &List<Integer> &Vector<Integer>)>)
       String)
 (@def vconcat ([E] &rest sequences: &List<&OrdSeq<E>>) VectorFresh<E>)
 (@def copy-sequence (arg: [<= S (or &Seq (Emacs record))]) (freshen-shallow S)))


;;; ============================================================
;;; String representation conversion

(et-declare
 (@def string-make-multibyte (string: String) String)
 (@def string-make-unibyte (string: String) String)
 (@def string-as-unibyte (string: String) String)
 (@def string-as-multibyte (string: String) String)
 (@def string-to-multibyte (string: String) String)
 (@def string-to-unibyte (string: String) String))


;;; ============================================================
;;; Subsequences and list access

(et-declare
 ;; nontrivial
 (@def copy-alist ([L R] alist: &List<&Cons<L~R>>) ListFresh<ConsFresh<L~R>>)
 (@def substring
       (string: [<= T (or String &Vector)] &optional from: Integer|Nil to: Integer|Nil)
       (freshen-shallow T))
 (@def substring-no-properties
       (string: String &optional from: Integer|Nil to: Integer|Nil)
       String)

 ;; nontrivial
 (@def take ([E] n: Integer list: &List<E>) ListFresh<E>)
 (@def ntake ([E] n: Integer list: List<E>) List<E>)
 (@def nthcdr ([E] n: Integer list: &List<E>) &List<E>)
 (@def nth ([E] n: Integer list: &List<E>) E|Nil)
 (@def elt ([E] sequence: &Seq<E> n: Integer) E|Nil))


;;; ============================================================
;;; Membership, association, and deletion

(et-declare
 ;; nontrivial
 (@def member ([E] elt: Any list: &List<E>) &List<E>)
 (@def memq   ([E] elt: Any list: &List<E>) &List<E>)
 (@def memql  ([E] elt: Any list: &List<E>) &List<E>)
 (@def assq ([K V] key: K alist: &List<&Cons<K~V>>) Nil|&Cons<K~V>)
 (@def assoc ([K V] key: K alist: &List<&Cons<K~V>>
              &optional testfn: (or Nil (fn (Args V V) Any)))
       Nil|&Cons<K~V>)
 (@def rassq ([K V] key: V alist: &List<&Cons<K~V>>) Nil|&Cons<K~V>)
 (@def rassoc ([K V] key: V alist: &List<&Cons<K~V>>
               &optional testfn: (or Nil (fn (Args V V) Any)))
       Nil|&Cons<K~V>)
 (@def delq ([E] elt: Any list: List<E>) List<E>)
 (@def delete ([E] elt: E seq: List<E>) List<E>))


;;; ============================================================
;;; Reversing and sorting

(et-declare
 ;; nontrivial
 (@def nreverse (seq: [<= S OrdSeq]) S)
 (@def reverse (seq: [<= S &OrdSeq]) (freshen-shallow S))
 (@def sort
       ([P E K] seq: [<= S (if-non-nil? P List<E>|Vector<E> &List<E>|&Vector<E>)]
        &key
        key: (or (fn (Args E) K) Nil&{K:=:E})
        lessp: (fn (Args K K) Any)
        reverse: Boolean
        in-place: P)
       S))


;;; ============================================================
;;; Property lists and equality

(et-declare
 (@def plist-get
       ([K V] plist: &PlistOf<K~V> prop: K
        &optional predicate: (or Nil (fn (Args K K) Any)))
       V|Nil)
 (@def get (symbol: Symbol propname: Any) Any)
 ;; The result widens the plist's key and value types for a new property and may
 ;; be either the original plist after mutation or a fresh head containing the
 ;; new pair. Type widening together with conditional return identity and mixed
 ;; mutation and freshening is not yet expressible.
 (@def plist-put
       ([K V P W] plist: PlistOf<K~V> prop: P val: W
        &optional predicate: (or Nil (fn (Args K P) Any)))
       Todo)
 (@def put ([V] symbol: Symbol propname: Any value: V) V)
 ;; The result is nil or the particular existing tail beginning at PROP.
 ;; Existing-substructure identity is not yet expressible.
 (@def plist-member
       ([K V] plist: &PlistOf<K~V> prop: K
        &optional predicate: (or Nil (fn (Args K K) Any)))
       Todo)
 (@def eql (obj1: Any obj2: Any) Boolean)
 (@def equal (o1: Any o2: Any) Boolean)
 (@def equal-including-properties (o1: Any o2: Any) Boolean)
 (@def value< (a: Any b: Any) Boolean))


;;; ============================================================
;;; Mutable sequences and list concatenation

(et-declare
 ;; nontrivial
 (@def fillarray
       ([<= A (or Vector String CharTable BoolVector)]
        array: A
        item: (oneof A
                     [[E] WriteVector<E> E]
                     [String Integer]
                     [CharTable Any]
                     [BoolVector Any]))
       A)
 (@def clear-string (string: String) Nil)
 (@def nconc ([E] &rest lists: List<List<E>>) List<E>))


;;; ============================================================
;;; Sequence mapping

(et-declare
 ;; FUNCTION's input depends on SEQUENCE's element type across four container
 ;; kinds. The type language has no common element-typed sequence abstraction
 ;; for that callback relationship.
 (@def mapconcat
       ([E] function: (fn (Args E) &OrdSeq<Integer>)
        sequence: (or &OrdSeq<E>)
        &optional separator: (or Nil &OrdSeq<Integer>))
       String)
 ;; FUNCTION's input depends on SEQUENCE's element type across four container
 ;; kinds, and each result-list element depends on FUNCTION's output. Those
 ;; callback and container element relationships are not yet expressible.
 (@def mapcar
       (function: (fn (Args Todo) Todo)
                  sequence: (or &List &Vector BoolVector String ByteCodeFunction))
       ListFresh<Todo>)
 ;; FUNCTION's input depends on SEQUENCE's element type, and the return value is
 ;; the identical SEQUENCE object. Cross-kind element relationships and return
 ;; identity are not yet expressible.
 (@def mapc
       (function: (fn (Args Todo) Any)
                  sequence: (or &List &Vector BoolVector String ByteCodeFunction))
       Todo)
 ;; FUNCTION's input depends on SEQUENCE's element type, and its list results
 ;; are destructively concatenated with nconc; the final result may also be an
 ;; arbitrary shared tail. Callback element relationships, destructive linking,
 ;; and mixed result sharing are not yet expressible.
 (@def mapcan
       (function: (fn (Args Todo) Todo)
                  sequence: (or &List &Vector BoolVector String ByteCodeFunction))
       Todo))


;;; ============================================================
;;; User prompts and system load

(et-declare
 (@def yes-or-no-p (prompt: String) Boolean)
 (@def load-average (&optional use-floats: Boolean) List<Number>))


;;; ============================================================
;;; Features

(et-declare
 (@def featurep (feature: Symbol &optional subfeature: Any) Boolean)
 (@def provide
       ([(<= F Symbol)] feature: F &optional subfeatures: &List<Symbol>)
       F)
 (@def require
       ([(<= F Symbol)] feature: F
        &optional filename: String|Nil noerror: Boolean)
       F|Nil))


;;; ============================================================
;;; Widgets and locale

(et-declare
 (@def widget-put ([V] widget: Cons property: Any value: V) V)
 (@def widget-get (widget: Nil|&Cons property: Any) Any)
 ;; The callable and its result are obtained dynamically from PROPERTY in
 ;; WIDGET, then ARGS are appended to WIDGET as the effective argument list.
 ;; Property-dependent function application is not yet expressible.
 (@def widget-apply
       (widget: &Cons property: Any &rest args: &List)
       Todo)
 (@def locale-info
       (item: Symbol)
       (or Nil String &Vector<String> (Tuple Integer Integer))))


;;; ============================================================
;;; Base64

(et-declare
 (@def base64-encode-region
       (beg: IntOrMarker end: IntOrMarker &optional no-line-break: Boolean)
       Integer)
 (@def base64url-encode-region
       (beg: IntOrMarker end: IntOrMarker &optional no-pad: Boolean)
       Integer)
 (@def base64-encode-string
       (string: String &optional no-line-break: Boolean)
       String)
 (@def base64url-encode-string
       (string: String &optional no-pad: Boolean)
       String)
 (@def base64-decode-region
       (beg: IntOrMarker end: IntOrMarker
             &optional base64url: Boolean ignore-invalid: Boolean)
       Integer)
 (@def base64-decode-string
       (string: String &optional base64url: Boolean ignore-invalid: Boolean)
       String))


;;; ============================================================
;;; Structural hashing

(et-declare
 (@def sxhash-eq (obj: Any) Integer)
 (@def sxhash-eql (obj: Any) Integer)
 (@def sxhash-equal (obj: Any) Integer)
 (@def sxhash-equal-including-properties (obj: Any) Integer))


;;; ============================================================
;;; Hash tables

(et-declare
 ;; A newly created table's key and value types must remain fresh and become
 ;; constrained by later gethash and puthash calls. Fresh container generics
 ;; for hash tables are not yet represented by the type language.
 (@def make-hash-table (&rest keyword-args: &List) Todo)
 ;; The result is a fresh table preserving the source table's test, weakness,
 ;; key type, value type, and shared entries. Hash-table parameterization and
 ;; shallow freshening are not yet expressible.
 (@def copy-hash-table (table: (Emacs hash-table)) (Emacs hash-table))
 (@def hash-table-count (table: (Emacs hash-table)) Integer)
 (@def hash-table-rehash-size (table: (Emacs hash-table)) Number)
 (@def hash-table-rehash-threshold (table: (Emacs hash-table)) Number)
 (@def hash-table-size (table: (Emacs hash-table)) Integer)
 (@def hash-table-test (table: (Emacs hash-table)) Symbol)
 (@def hash-table-weakness
       (table: (Emacs hash-table))
       (or Nil @key @value @key-or-value @key-and-value))
 (@def hash-table-p ([T] obj: T) (is? T (Emacs hash-table)))
 (@def clrhash (table: (Emacs hash-table)) (Emacs hash-table))
 ;; The found value depends on TABLE's unrepresented value type, while a miss
 ;; returns DFLT. Hash-table key/value parameterization and lookup-dependent
 ;; result selection are not yet expressible.
 (@def gethash
       (key: Any table: (Emacs hash-table) &optional dflt: Any)
       Todo)
 (@def puthash ([V] key: Any value: V table: (Emacs hash-table)) V)
 (@def remhash (key: Any table: (Emacs hash-table)) Nil)
 (@def maphash
       (function: (fn (Args Any Any) Any) table: (Emacs hash-table))
       Nil)
 (@def define-hash-table-test
       (name: Symbol test: (fn (Args Any Any) Any)
              hash: (fn (Args Any) Any))
       (Tuple AnyFn AnyFn))
 (@def internal--hash-table-histogram
       (hash-table: (Emacs hash-table))
       List<Cons<Integer~Integer>>)
 (@def internal--hash-table-buckets
       (hash-table: (Emacs hash-table))
       List<List<Cons<Any~Integer>>>)
 (@def internal--hash-table-index-size
       (hash-table: (Emacs hash-table))
       Integer))


;;; ============================================================
;;; Cryptographic hashing

(et-declare
 (@def secure-hash-algorithms () List<Symbol>)
 ;; START and END are integer indices for strings but buffer positions, which
 ;; can also be markers, for buffers. Argument types conditional on another
 ;; argument's container kind are not yet expressible.
 (@def md5
       (object: Buffer|String
                &optional start: Todo end: Todo
                coding-system: Symbol|Nil noerror: Boolean)
       String)
 ;; START and END are integer indices for strings but buffer positions, which
 ;; can also be markers, for buffers. Argument types conditional on another
 ;; argument's container kind are not yet expressible.
 (@def secure-hash
       (algorithm: Symbol object: Buffer|String
                   &optional start: Todo end: Todo binary: Boolean)
       String)
 (@def buffer-hash (&optional buffer-or-name: Buffer|String|Nil) String))


;;; ============================================================
;;; Search and buffer information

(et-declare
 (@def buffer-line-statistics
       (&optional buffer-or-name: Buffer|String|Nil)
       (Tuple Integer Integer Number))
 (@def string-search
       (needle: String haystack: String &optional start-pos: Integer|Nil)
       Integer|Nil)
 (@def object-intervals
       (object: Buffer|String)
       List<Tuple<Integer Integer &List>>)
 (@def line-number-at-pos
       (&optional position: IntOrMarker|Nil absolute: Boolean)
       Integer))


;;; ============================================================
