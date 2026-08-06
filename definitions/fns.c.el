;; -*- lexical-binding: t; -*-


;;; ============================================================
;;; Basic utilities

(et-declare
 (@def identity ([T] argument: T) T)
 (@def random (&optional limit: (or Nil True String (and Integer Positive))) Integer))


;;; ============================================================
;;; Lengths and string comparison

(et-declare
 (@def length
       (sequence: (or &List &Vector String CharTable BoolVector
                      ByteCodeFunction (Emacs record)))
       Integer)
 (@def safe-length (list: Any) Integer)
 (@def length<
       (sequence: (or &List &Vector String CharTable BoolVector
                      ByteCodeFunction (Emacs record))
                  length: Integer)
       Boolean)
 (@def length>
       (sequence: (or &List &Vector String CharTable BoolVector
                      ByteCodeFunction (Emacs record))
                  length: Integer)
       Boolean)
 (@def length=
       (sequence: (or &List &Vector String CharTable BoolVector
                      ByteCodeFunction (Emacs record))
                  length: Integer)
       Boolean)
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
 ;; The final argument is an arbitrary shared tail while preceding arguments
 ;; are copied sequences. Rest-position distinctions and mixed sharing and
 ;; freshening are not yet expressible.
 (@def append (&rest sequences: Todo) Todo)
 (@def concat
       (&rest sequences: &List<(or String &List<Integer> &Vector<Integer>)>)
       String)
 ;; The result element type is the union of the element types of heterogeneous
 ;; input sequences. The type language cannot yet derive an element type across
 ;; a rest list of differently shaped containers.
 (@def vconcat
       (&rest sequences: &List<(or &List &Vector String BoolVector ByteCodeFunction)>)
       VectorFresh<Todo>)
 ;; The result preserves the input container kind and element types while
 ;; shallowly freshening its outer structure. That kind-preserving shallow-copy
 ;; relationship is not yet expressible.
 (@def copy-sequence
       (arg: (or &List &Vector String CharTable BoolVector (Emacs record)))
       Todo))


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
 ;; The result freshens both the alist spine and each association pair while
 ;; sharing the keys and values. Selective shallow freshening is not yet
 ;; expressible.
 (@def copy-alist (alist: &List) Todo)
 (@def substring
       (string: [<= T (or String &Vector)] &optional from: Integer|Nil to: Integer|Nil)
       (freshen-shallow T))
 (@def substring-no-properties
       (string: String &optional from: Integer|Nil to: Integer|Nil)
       String)
 (@def take ([E] n: Integer list: &List<E>) ListFresh<E>)
 (@def ntake ([E] n: Integer list: List<E>) List<E>)
 ;; The result is a particular existing tail of LIST and must preserve the
 ;; input spine's writeability. Substructure identity and mutability
 ;; preservation are not yet expressible.
 (@def nthcdr ([E] n: Integer list: &List<E>) Todo)
 (@def nth ([E] n: Integer list: &List<E>) E|Nil)

 (@alias EltSeq [] (or &List &Vector String BoolVector CharTable))
 (@alias EltOf [<= T EltSeq]
         (infer T [E] &List<E> E
                (infer T [E] &Vector<E> E
                       (extends? T String Integer
                                 (extends? T BoolVector Boolean
                                           (extends? T CharTable Integer
                                                     Never))))))
 (@def elt (sequence: [<= S EltSeq] n: Integer) EltOf<S>))


;;; ============================================================
;;; Membership, association, and deletion

(et-declare
 ;; The result is nil or a particular existing tail of LIST, preserving the
 ;; tail's writeability. Existing-substructure identity is not yet expressible.
 (@def member (elt: Any list: &List) Todo)
 ;; The result is nil or a particular existing tail of LIST, preserving the
 ;; tail's writeability. Existing-substructure identity is not yet expressible.
 (@def memq (elt: Any list: &List) Todo)
 ;; The result is nil or a particular existing tail of LIST, preserving the
 ;; tail's writeability. Existing-substructure identity is not yet expressible.
 (@def memql (elt: Any list: &List) Todo)
 ;; The result is an existing association cell selected from ALIST. The type
 ;; language cannot preserve association-cell identity and writeability.
 (@def assq (key: Any alist: &List) Todo)
 ;; The result is an existing association cell selected from ALIST. The type
 ;; language cannot preserve association-cell identity and writeability.
 (@def assoc
       (key: Any alist: &List
             &optional testfn: (or Nil (fn (Args Any Any) Any)))
       Todo)
 ;; The result is an existing association cell selected from ALIST. The type
 ;; language cannot preserve association-cell identity and writeability.
 (@def rassq (key: Any alist: &List) Todo)
 ;; The result is an existing association cell selected from ALIST. The type
 ;; language cannot preserve association-cell identity and writeability.
 (@def rassoc (key: Any alist: &List) Todo)
 ;; The function destructively splices LIST and may return an existing tail.
 ;; Destructive spine updates and returned-substructure identity are not yet
 ;; expressible.
 (@def delq (elt: Any list: List) Todo)
 ;; Lists are destructively spliced, whereas vectors and strings are copied,
 ;; and the result preserves the input container kind. This conditional
 ;; mutation, freshness, and kind relationship is not yet expressible.
 (@def delete (elt: Any seq: (or List &Vector String)) Todo))


;;; ============================================================
;;; Reversing and sorting

(et-declare
 ;; Lists, vectors, and bool-vectors are modified in place while strings are
 ;; copied, and the result preserves the input container kind and element type.
 ;; This conditional mutation and kind-preserving relationship is not yet
 ;; expressible.
 (@def nreverse (seq: (or List Vector String BoolVector)) Todo)
 ;; The result is a fresh container of the same kind and element type as SEQ.
 ;; Kind-preserving freshening across sequence variants is not yet expressible.
 (@def reverse (seq: (or &List &Vector String BoolVector)) Todo)
 ;; KEY's input is SEQ's element type and LESSP compares KEY's output, while a
 ;; nil KEY makes LESSP compare the elements directly. The result preserves
 ;; SEQ's kind and element type, but its identity depends on :in-place and on
 ;; the legacy calling convention. These callback dependencies and
 ;; value-dependent freshness and mutation are not yet expressible.
 (@def sort
       (seq: (or List Vector)
             &key key: Todo lessp: Todo
             reverse: Boolean in-place: Boolean)
       Todo))


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
 ;; ITEM's type depends on ARRAY's writable element type, and the result is the
 ;; same mutated array. Cross-kind writable-element relationships and return
 ;; identity are not yet expressible.
 (@def fillarray
       (array: (or WriteVector String CharTable BoolVector) item: Todo)
       Todo)
 (@def clear-string (string: String) Nil)
 ;; Every non-final argument must be a writable list and is destructively
 ;; linked to the following value, while the final argument is an arbitrary
 ;; shared tail. Rest-position mutation and returned-argument identity are not
 ;; yet expressible.
 (@def nconc (&rest lists: Todo) Todo))


;;; ============================================================
;;; Sequence mapping

(et-declare
 ;; FUNCTION's input depends on SEQUENCE's element type across four container
 ;; kinds. The type language has no common element-typed sequence abstraction
 ;; for that callback relationship.
 (@def mapconcat
       (function: (fn (Args Todo) (or String &List<Integer> &Vector<Integer>))
                  sequence: (or &List &Vector BoolVector String ByteCodeFunction)
                  &optional separator: (or Nil String &List<Integer> &Vector<Integer>))
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
 (@def copy-hash-table (table: (Emacs hash-table)) Todo)
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
