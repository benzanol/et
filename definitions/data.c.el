;; -*- lexical-binding: t; -*-


;;; ============================================================
;;; Data type predicates

(et-declare
 (@def eq ([A B] obj1: A obj2: B)
       (or Nil (and True (bindsof (and A B)))))
 (@def null ([T] object: T) (is? T Nil))
 (@def type-of (object: Any) NonNilSymbol)
 (@def cl-type-of (object: Any) NonNilSymbol)
 (@def consp ([T] object: T) (is? T Cons))
 (@def atom ([T] object: T)
       (or (and True (bindsof (subtract T Cons)))
           (and Nil (bindsof (and T Cons)))))
 (@def listp ([T] object: T) (is? T Nil|Cons))
 (@def nlistp ([T] object: T)
       (or (and True (bindsof (subtract T Nil|Cons)))
           (and Nil (bindsof (and T Nil|Cons)))))
 (@def bare-symbol-p ([T] object: T) (is-a? T Symbol))
 (@def symbol-with-pos-p ([T] object: T) (is? T (Emacs symbol-with-pos)))
 (@def symbolp ([T] object: T) (is? T Symbol))
 (@def keywordp ([T] object: T) (is-a? T Symbol))
 (@def vectorp ([T] object: T) (is? T Vector))
 (@def recordp ([T] object: T) Boolean)
 (@def stringp ([T] object: T) (is? T String))
 (@def multibyte-string-p ([T] object: T) (is-a? T String))
 (@def char-table-p ([T] object: T) (is? T CharTable))
 (@def vector-or-char-table-p ([T] object: T) (is? T Vector|CharTable))
 (@def bool-vector-p ([T] object: T) (is? T BoolVector))
 (@def arrayp ([T] object: T)
       (is? T String|Vector|CharTable|BoolVector))
 (@def sequencep ([T] object: T)
       (is? T Nil|Cons|String|Vector|CharTable|BoolVector))
 (@def bufferp ([T] object: T) (is? T Buffer))
 (@def markerp ([T] object: T) (is? T Marker))
 (@def user-ptrp ([T] object: T) (is? T (Emacs user-ptr)))
 (@def subrp ([T] object: T) (is? T Subr))
 (@def closurep ([T] object: T) (is? T Closure))
 (@def byte-code-function-p ([T] object: T) (is? T ByteCodeFunction))
 (@def interpreted-function-p ([T] object: T) (is? T InterpretedFunction))
 (@def module-function-p ([T] object: T) (is? T (Emacs module-function)))
 (@def char-or-string-p ([T] object: T) (is-a? T String|Integer))
 (@def integerp ([T] object: T) (is? T Integer))
 (@def integer-or-marker-p ([T] object: T) (is? T IntOrMarker))
 (@def natnump ([T] object: T)
       (is? T (or 0 (and Integer Positive))))
 (@def numberp ([T] object: T) (is? T Number))
 (@def number-or-marker-p ([T] object: T) (is? T NumOrMarker))
 (@def floatp ([T] object: T) (is-a? T Number))
 (@def threadp ([T] object: T) (is? T Thread))
 (@def mutexp ([T] object: T) (is? T Mutex))
 (@def condition-variable-p ([T] object: T) (is? T ConditionVariable)))


;;; ============================================================
;;; List components

(et-declare
 (@def car ([T] list: (or (and Nil (set T Nil)) (&Cons T Any))) T)
 (@def car-safe ([O] object: O) (infer O [T] (&Cons T Any) T Nil))
 (@def cdr ([T] list: (or (and Nil (set T Nil)) (&Cons Any T))) T)
 (@def cdr-safe ([O] object: O) (infer O [T] (&Cons Any T) T Nil))
 (@def setcar ([T] cell: WriteCons<T~Never> newcar: T) T)
 (@def setcdr ([T] cell: WriteCons<Never~T> newcdr: T) T))


;;; ============================================================
;;; Symbol components

(et-declare
 (@def boundp (symbol: Symbol) Boolean)
 (@def fboundp (symbol: Symbol) Boolean)
 (@def makunbound ([(<= S Var)] symbol: S) S)
 (@def fmakunbound ([(<= S Var)] symbol: S) S)
 (@def symbol-function (symbol: Symbol) Any)
 (@def symbol-plist (symbol: Symbol) Any)
 (@def symbol-name (symbol: Symbol) String)
 (@def bare-symbol (sym: (or Symbol (Emacs symbol-with-pos))) Symbol)
 (@def symbol-with-pos-pos (sympos: (Emacs symbol-with-pos)) Integer)
 (@def remove-pos-from-symbol (arg: [T]) (replace-in T (Emacs symbol-with-pos) Symbol))
 (@def position-symbol
       (sym: (or Symbol (Emacs symbol-with-pos))
             pos: (or Integer (Emacs symbol-with-pos)))
       (Emacs symbol-with-pos))
 (@def fset ([T] symbol: Var definition: T) T)
 (@def defalias
       ([(<= S Var)] symbol: S definition: Any &optional docstring: String|Nil)
       S)
 (@def setplist ([P] symbol: Var newplist: P) P)
 (@def subr-arity (subr: Subr)
       Cons<Integer~Integer|@many|@unevalled>)
 (@def subr-name (subr: Subr) String)
 (@def native-comp-function-p ([T] object: T) (is-a? T AnyFn))
 (@def subr-native-lambda-list (subr: Subr) True|&List)
 (@def subr-type (subr: Subr) Nil|Tuple<@function~List<Sexp>~Sexp>)
 (@def subr-native-comp-unit (subr: Subr)
       (or Nil (Emacs native-comp-unit)))
 ;; The unit's file slot is mutable and the setter accepts any Lisp value.
 ;; Field-dependent result types are needed to recover its current type.
 (@def native-comp-unit-file (comp-unit: (Emacs native-comp-unit)) Todo)
 (@def native-comp-unit-set-file (comp-unit: (Emacs native-comp-unit) new-file: Any)
       (Emacs native-comp-unit))
 (@def interactive-form (cmd: Any)
       (or Nil (&Tuple @interactive Any)))
 (@def command-modes (command: Any) &List<Symbol>))


;;; ============================================================
;;; Symbol values and watchers

(et-declare
 ;; Symbols are replaced by a different symbol at the end of an alias chain,
 ;; while non-symbols are returned unchanged. A conditional replacement
 ;; operation over union members is needed to express that result.
 (@def indirect-variable (object: Any) Todo)
 ;; A variable's value depends on the identity of SYMBOL. Symbol-property
 ;; dependent result types are needed to replace this return approximation.
 (@def symbol-value (symbol: Symbol) Todo)
 (@def set ([T] symbol: Var newval: T) T)
 (@def add-variable-watcher
       (symbol: Var
                watch-function: (fn (Args Symbol Any
                                          (or @set @let @unlet @makunbound @defvaralias)
                                          Buffer|Nil)
                                    Any))
       Nil)
 (@def remove-variable-watcher
       (symbol: Var
                watch-function: (fn (Args Symbol Any
                                          (or @set @let @unlet @makunbound @defvaralias)
                                          Buffer|Nil)
                                    Any))
       Nil)
 (@def get-variable-watchers
       (symbol: Symbol)
       (List (fn (Args Symbol Any
                       (or @set @let @unlet @makunbound @defvaralias)
                       Buffer|Nil)
                 Any))))


;;; ============================================================
;;; Default and buffer-local values

(et-declare
 (@def default-boundp (symbol: Symbol) Boolean)
 ;; A default value depends on the identity of SYMBOL. Symbol-property
 ;; dependent result types are needed to replace this return approximation.
 (@def default-value (symbol: Symbol) Todo)
 (@def set-default ([T] symbol: Var value: T) T)
 (@def make-variable-buffer-local ([(<= V Var)] variable: V) V)
 (@def make-local-variable ([(<= V Var)] variable: V) V)
 (@def kill-local-variable ([(<= V Var)] variable: V) V)
 (@def local-variable-p
       (variable: Symbol &optional buffer: Buffer|Nil)
       Boolean)
 (@def local-variable-if-set-p
       (variable: Symbol &optional buffer: Buffer|Nil)
       Boolean)
 (@def variable-binding-locus (variable: Symbol) Buffer|Terminal|Nil))


;;; ============================================================
;;; Function indirection

(et-declare
 ;; Symbols are replaced by arbitrarily typed function-cell values while
 ;; non-symbols are returned unchanged. A conditional replacement operation
 ;; over unions is needed to express that result.
 (@def indirect-function (object: Any &optional noerror: Any) Todo))


;;; ============================================================
;;; Array elements

(et-declare
 ;; Char tables lack an element parameter, and closures and records have
 ;; index-dependent layouts. Element lookup needs generic char tables,
 ;; existential records, and index-dependent result types.
 (@def aref
       ([T]
        array: (or &Vector<T>
                   (and String (set T Integer))
                   (and BoolVector (set T Boolean))
                   (and CharTable (set T Todo))
                   (and Closure (set T Todo))
                   (and Todo (set T Todo)))
        idx: Integer)
       T)
 ;; Records include Struct values with arbitrary names and generic arguments.
 ;; Existential Struct types are needed for the record branch of ARRAY.
 (@def aset
       ([T]
        array: (or WriteVector<T>
                   (and String (set T Integer))
                   BoolVector CharTable Todo)
        idx: Integer newelt: T)
       T))


;;; ============================================================
;;; Arithmetic comparisons

(et-declare
 (@def = (number-or-marker: NumOrMarker &rest numbers-or-markers: &List<NumOrMarker>) Boolean)
 (@def < (number-or-marker: NumOrMarker &rest numbers-or-markers: &List<NumOrMarker>) Boolean)
 (@def > (number-or-marker: NumOrMarker &rest numbers-or-markers: &List<NumOrMarker>) Boolean)
 (@def <= (number-or-marker: NumOrMarker &rest numbers-or-markers: &List<NumOrMarker>) Boolean)
 (@def >= (number-or-marker: NumOrMarker &rest numbers-or-markers: &List<NumOrMarker>) Boolean)
 (@def /= (num1: NumOrMarker num2: NumOrMarker) Boolean))


;;; ============================================================
;;; Number conversion

(et-declare
 (@def number-to-string (number: Number) String)
 (@def string-to-number (string: String &optional base: Integer|Nil) Number))


;;; ============================================================
;;; Arithmetic operations

(et-declare
 (@def +
       ([(<= Nums &List<NumOrMarker>)] &rest numbers-or-markers: Nums)
       (extends? Nums Nil 0
                 (extends? Nums &List<IntOrMarker> Integer Number)))
 ;; The result is Integer exactly when every supplied argument is an integer
 ;; or marker, including the optional first argument. The declaration language
 ;; cannot capture the complete optional-and-rest argument tuple as one type.
 (@def -
       (&optional number-or-marker: NumOrMarker
                  &rest more-numbers-or-markers: &List<NumOrMarker>)
       Todo)
 (@def *
       ([(<= Nums &List<NumOrMarker>)] &rest numbers-or-markers: Nums)
       (extends? Nums Nil 1
                 (extends? Nums &List<IntOrMarker> Integer Number)))
 (@def /
       ([(<= N NumOrMarker) (<= Ds &List<NumOrMarker>)]
        number: N &rest divisors: Ds)
       (extends? N IntOrMarker
                 (extends? Ds &List<IntOrMarker> Integer Number)
                 Number))
 (@def % (x: IntOrMarker y: IntOrMarker) Integer)
 (@def mod
       ([(<= X NumOrMarker) (<= Y NumOrMarker)] x: X y: Y)
       (extends? X IntOrMarker
                 (extends? Y IntOrMarker Integer Number)
                 Number))
 (@def max
       ([(<= N NumOrMarker) (<= Ns &List<NumOrMarker>)]
        number-or-marker: N &rest numbers-or-markers: Ns)
       (extends? N IntOrMarker
                 (extends? Ns &List<IntOrMarker> Integer Number)
                 Number))
 (@def min
       ([(<= N NumOrMarker) (<= Ns &List<NumOrMarker>)]
        number-or-marker: N &rest numbers-or-markers: Ns)
       (extends? N IntOrMarker
                 (extends? Ns &List<IntOrMarker> Integer Number)
                 Number))
 (@def logand (&rest ints-or-markers: &List<IntOrMarker>) Integer)
 (@def logior (&rest ints-or-markers: &List<IntOrMarker>) Integer)
 (@def logxor (&rest ints-or-markers: &List<IntOrMarker>) Integer)
 (@def logcount (value: Integer) Integer)
 (@def ash (value: Integer count: Integer) Integer)
 (@def 1+
       ([(<= N NumOrMarker)] number: N)
       (extends? N IntOrMarker Integer Number))
 (@def 1-
       ([(<= N NumOrMarker)] number: N)
       (extends? N IntOrMarker Integer Number))
 (@def lognot (number: Integer) Integer)
 (@def byteorder () 66|108))


;;; ============================================================
;;; Bool vector operations

(et-declare
 (@def bool-vector-exclusive-or
       ([(<= C BoolVector|Nil)] a: BoolVector b: BoolVector &optional c: C)
       (if-nil? C BoolVector BoolVector|Nil))
 (@def bool-vector-union
       ([(<= C BoolVector|Nil)] a: BoolVector b: BoolVector &optional c: C)
       (if-nil? C BoolVector BoolVector|Nil))
 (@def bool-vector-intersection
       ([(<= C BoolVector|Nil)] a: BoolVector b: BoolVector &optional c: C)
       (if-nil? C BoolVector BoolVector|Nil))
 (@def bool-vector-set-difference
       ([(<= C BoolVector|Nil)] a: BoolVector b: BoolVector &optional c: C)
       (if-nil? C BoolVector BoolVector|Nil))
 (@def bool-vector-subsetp (a: BoolVector b: BoolVector) Boolean)
 (@def bool-vector-not
       (a: BoolVector &optional b: BoolVector|Nil)
       BoolVector)
 (@def bool-vector-count-population (a: BoolVector) Integer)
 (@def bool-vector-count-consecutive
       (a: BoolVector b: Boolean i: Integer)
       Integer))


;;; ============================================================
