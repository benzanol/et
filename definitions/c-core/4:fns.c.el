;; -*- lexical-binding: t; -*-


;;; ============================================================
;;; Basic utilities

(et-declare
 (@def identity (argument: [T]) T)
 (@def random (&optional limit: (or Nil True String (and Integer Positive))) Integer))


;;; ============================================================
;;; Lengths and string comparison

(et-declare
 (@def length (sequence: &LenSeq) Integer)
 (@def safe-length (list: Any) Integer)
 (@def length< (sequence: &LenSeq length: Integer) Boolean)
 (@def length> (sequence: &LenSeq length: Integer) Boolean)
 (@def length= (sequence: &LenSeq length: Integer) Boolean)
 (@def proper-list-p (object: Any) Integer?)
 (@def string-bytes (string: String) Integer)
 (@def string-distance
       (string1: String string2: String &optional bytecompare: Bool)
       Integer)
 (@def string-equal (s1: Symbol|String s2: Symbol|String) Boolean)
 (@def compare-strings
       (str1: String start1: Integer? end1: Integer?
              str2: String start2: Integer? end2: Integer?
              &optional ignore-case: Bool)
       Integer|True)
 (@def string-lessp (string1: Symbol|String string2: Symbol|String) Boolean)
 (@def string-version-lessp (string1: Symbol|String string2: Symbol|String) Boolean)
 (@def string-collate-lessp
       (s1: Symbol|String s2: Symbol|String
            &optional locale: String? ignore-case: Bool)
       Boolean)
 (@def string-collate-equalp
       (s1: Symbol|String s2: Symbol|String
            &optional locale: String? ignore-case: Bool)
       Boolean))


;;; ============================================================
;;; Sequence construction and copying

(et-declare
 ;; nontrivial
 (@def append ([E] &rest sequences: &ListWithLast<&List<E>_List<E>>) List<E>)
 (@def concat (&rest sequences: &List<&ConcatSeq<Integer>>) String)
 (@def vconcat ([E] &rest sequences: &List<&OrdSeq<E>>) VectorFresh<E>)
 (@def copy-sequence (arg: [S <: &LenSeq]) (freshen-shallow S)))


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
 (@def copy-alist ([L R] alist: &List<&Cons<L_R>>) ListFresh<ConsFresh<L_R>>)
 (@def substring
       (string: [T <: (or String &Vector)] &optional from: Integer? to: Integer?)
       (freshen-shallow T))
 (@def substring-no-properties (string: String &optional from: Integer? to: Integer?) String)
 ;; nontrivial
 (@def take ([E] n: Integer list: &List<E>) ListFresh<E>)
 (@def ntake ([E] n: Integer list: List<E>) List<E>)
 (@def nthcdr ([E] n: Integer list: &List<E>) &List<E>)
 (@def nth ([E] n: Integer list: &List<E>) E?)
 (@def elt ([E] sequence: &EltSeq<E> n: Integer) E?))


;;; ============================================================
;;; Membership, association, and deletion

(et-declare
 ;; nontrivial
 (@def member ([E] elt: Any list: &List<E>) &List<E>)
 (@def memq   ([E] elt: Any list: &List<E>) &List<E>)
 (@def memql  ([E] elt: Any list: &List<E>) &List<E>)
 (@def assq ([K V] key: K alist: &List<&Cons<K_V>>) &Cons<K_V>?)
 (@def assoc ([K V] key: K alist: &List<&Cons<K_V>>
              &optional testfn: (or Nil (fn (&tuple V V) Any)))
       &Cons<K_V>?)
 (@def rassq ([K V] key: V alist: &List<&Cons<K_V>>) &Cons<K_V>?)
 (@def rassoc ([K V] key: V alist: &List<&Cons<K_V>>
               &optional testfn: (or Nil (fn (&tuple V V) Any)))
       &Cons<K_V>?)
 (@def delq ([E] elt: Any list: List<E>) List<E>)
 (@def delete ([E] elt: E seq: List<E>) List<E>))


;;; ============================================================
;;; Reversing and sorting

(et-declare
 ;; nontrivial
 (@def nreverse (seq: [S <: OrdSeq]) S)
 (@def reverse (seq: [S <: &OrdSeq]) (freshen-shallow S))
 (@def sort
       ([P E K] seq: [S <: (if-non-nil? P List<E>|Vector<E> &List<E>|&Vector<E>)]
        &key
        key: (or (fn (&tuple E) K) Nil&{K:=:E})
        lessp: (fn (&tuple K K) Any)
        reverse: Bool
        in-place: P)
       S))


;;; ============================================================
;;; Property lists and equality

(et-declare
 ;; nontrivial
 (@def plist-get
       ([K A] plist: A prop: K
        &optional predicate: fn2<K_K>?)
       (expand-1 PlistGet<A_K>))
 (@def get
       ([(S <: Symbol)] symbol: Symbol propname: S)
       (symbol-property S))
 (@def plist-put
       ([P K] plist: P prop: K val: PlistPut<P_K>
        &optional predicate: fn2<K_K>?)
       P)
 (@check put
         ($pcase
          `(,_ ,_ ,_)
          ($eval
           (et-chk ($expect ($type Symbol) ($at 1)))
           (let* ((prop-type (et-chk ($expect ($type Symbol) ($at 2))))
                  (value-type
                   (et-parse-type `(symbol-property (type ,prop-type)))))
             (et-chk ($expect ($eval value-type) ($at 3)))))))
 (@def plist-member
       ([K V] plist: &PlistOf<K_V> prop: K
        &optional predicate: (or Nil (fn (&tuple K K) Any)))
       Any)
 (@def eql (obj1: Any obj2: Any) Boolean)
 (@def equal (o1: Any o2: Any) Boolean)
 (@def equal-including-properties (o1: Any o2: Any) Boolean)
 (@def value< (a: Any b: Any) Boolean))


;;; ============================================================
;;; Mutable sequences and list concatenation

(et-declare
 ;; nontrivial
 (@def fillarray ([E (A <: FillableArray<E>)] array: A item: E) A)
 (@def clear-string (string: String) Nil)
 (@def nconc ([E] &rest lists: List<List<E>>) List<E>))


;;; ============================================================
;;; Sequence mapping

(et-declare
 ;; nontrivial
 (@def mapconcat
       ([E] function: (fn (&tuple E) &OrdSeq<Integer>)
        sequence: &MapSeq<E>
        &optional separator: &OrdSeq<Integer>?)
       String)
 (@def mapcar ([E O] function: (fn (&tuple E) O) sequence: &MapSeq<E>) ListFresh<E>)
 (@def mapc ([E O] function: (fn (&tuple E) O) sequence: &MapSeq<E>) Nil)
 (@def mapcan ([E T (L <: List<T>)] function: (fn (&tuple E) L) sequence: &MapSeq<E>) L))


;;; ============================================================
;;; User prompts and system load

(et-declare
 (@def yes-or-no-p (prompt: String) Boolean)
 (@def load-average (&optional use-floats: Bool) List<Number>))


;;; ============================================================
;;; Features

(et-declare
 (@def featurep (feature: Symbol &optional subfeature: Any) Boolean)
 (@def provide (feature: [(F <: Symbol)] &optional subfeatures: &List<Symbol>) F)
 (@def require (feature: [(F <: Symbol)] &optional filename: String? noerror: Bool) F?))


;;; ============================================================
;;; Widgets and locale

(et-declare
 (@def widget-put ([V] widget: Cons property: Any value: V) V)
 (@def widget-get (widget: &Cons? property: Any) Any)
 (@def widget-apply (widget: &Cons property: Any &rest args: &List) Any)
 (@def locale-info (item: Symbol) (or Nil String &Vector<String> (tuple Integer Integer))))


;;; ============================================================
;;; Base64

(et-declare
 (@def base64-encode-region
       (beg: IntOrMarker end: IntOrMarker &optional no-line-break: Bool)
       Integer)
 (@def base64url-encode-region
       (beg: IntOrMarker end: IntOrMarker &optional no-pad: Bool)
       Integer)
 (@def base64-encode-string
       (string: String &optional no-line-break: Bool)
       String)
 (@def base64url-encode-string
       (string: String &optional no-pad: Bool)
       String)
 (@def base64-decode-region
       (beg: IntOrMarker end: IntOrMarker
             &optional base64url: Bool ignore-invalid: Bool)
       Integer)
 (@def base64-decode-string
       (string: String &optional base64url: Bool ignore-invalid: Bool)
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
 ;; nontrivial
 (@def make-hash-table (&rest keyword-args: &List) HashTableFresh<Any_Any>) ; todo
 (@def copy-hash-table (table: [H <: &HashTable]) (freshen-shallow H))
 (@def hash-table-count (table: &HashTable) Integer)
 (@def hash-table-rehash-size (table: &HashTable) Number)
 (@def hash-table-rehash-threshold (table: &HashTable) Number)
 (@def hash-table-size (table: &HashTable) Integer)
 (@def hash-table-test (table: &HashTable) Symbol)
 (@def hash-table-weakness (table: &HashTable) (or Nil @key @value @key-or-value @key-and-value))
 (@def hash-table-p ([T] obj: T) (is? T &HashTable))
 (@def clrhash (table: [H <: &HashTable]) H)
 (@def gethash ([V] key: Any table: &HashTable<Any_V> &optional dflt: V?) V)
 (@def puthash ([K V] key: K value: V table: HashTableW<K_V>) V)
 (@def remhash ([K] key: K table: HashTableW<K_Never>) Nil)
 (@def maphash ([K V] function: (fn (&tuple K V) Any) table: &HashTable<K_V>) Nil)
 ;; todo
 (@def define-hash-table-test
       (name: Symbol test: (fn (&tuple Any Any) Any)
              hash: (fn (&tuple Any) Any))
       (tuple AnyFn AnyFn))
 (@def internal--hash-table-histogram
       (hash-table: (Emacs hash-table))
       List<Cons<Integer_Integer>>)
 (@def internal--hash-table-buckets
       (hash-table: (Emacs hash-table))
       List<List<Cons<Any_Integer>>>)
 (@def internal--hash-table-index-size
       (hash-table: (Emacs hash-table))
       Integer))


;;; ============================================================
;;; Cryptographic hashing

(et-declare
 (@def secure-hash-algorithms () List<Symbol>)
 (@def md5 (object: StringOrBuffer<I> &optional start: [I] end: I
                    coding-system: Symbol? noerror: Bool)
       String)
 (@def secure-hash
       (algorithm: Symbol object: StringOrBuffer<I>
                   &optional start: [I] end: I binary: Bool)
       String)
 (@def buffer-hash (&optional buffer-or-name: StringOrBuffer?) String))


;;; ============================================================
;;; Search and buffer information

(et-declare
 (@def buffer-line-statistics
       (&optional buffer-or-name: StringOrBuffer?)
       (tuple Integer Integer Number))
 (@def string-search
       (needle: String haystack: String &optional start-pos: Integer?)
       Integer?)
 (@def object-intervals
       (object: StringOrBuffer)
       List<tuple<Integer_Integer_&List>>)
 (@def line-number-at-pos (&optional position: IntOrMarker? absolute: Bool) Integer))


;;; ============================================================
