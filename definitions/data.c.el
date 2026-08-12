;; -*- lexical-binding: t; -*-


;;; ============================================================
;;; Data type predicates

(et-declare
 (@def eq ([A B] obj1: A obj2: B) (or Nil (when (and A B) True)))
 (@def null ([T] object: T) (is? T Nil))
 (@def type-of (object: Any) NonNilSymbol)
 (@def cl-type-of (object: Any) NonNilSymbol)
 (@def consp ([T] object: T) (is? T Cons))
 (@def atom ([T] object: T) (isnt? T Cons))
 (@def listp ([T] object: T) (is? T Cons?))
 (@def nlistp ([T] object: T) (isnt? T Cons?))
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
 (@def arrayp ([T] object: T) (is? T String|Vector|CharTable|BoolVector))
 (@def sequencep ([T] object: T) (is? T Cons|String|Vector|CharTable|BoolVector?))
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
 (@def natnump ([T] object: T) (is-a? T Integer))
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
 (@def car-safe ([O] object: O) (infer O [T] &Cons<T~Any> T Nil))
 (@def cdr ([T] list: (or (and Nil (set T Nil)) &Cons<Any~T>)) T)
 (@def cdr-safe ([O] object: O) (infer O [T] &Cons<Any~T> T Nil))
 (@def setcar ([A B] cell: Cons<A~B> newcar: A) A)
 (@def setcdr ([A B] cell: Cons<A~B> newcdr: B) B))


;;; ============================================================
;;; Symbol components

(et-declare
 (@def boundp (symbol: Symbol) Boolean)
 (@def fboundp (symbol: Symbol) Boolean)
 (@def makunbound (symbol: [<= S Var]) S)
 (@def fmakunbound (symbol: [<= S Var]) S)
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
       ([(<= S Var)] symbol: S definition: Any &optional docstring: String?)
       S)
 (@def setplist ([P] symbol: Var newplist: P) P)
 (@def subr-arity (subr: Subr)
       Cons<Integer~Integer|@many|@unevalled>)
 (@def subr-name (subr: Subr) String)
 (@def native-comp-function-p ([T] object: T) (is-a? T AnyFn))
 (@def subr-native-lambda-list (subr: Subr) True|&List)
 (@def subr-type (subr: Subr) Tuple<@function~List<Sexp>~Sexp>?)
 (@def subr-native-comp-unit (subr: Subr)
       (or Nil (Emacs native-comp-unit)))
 (@def native-comp-unit-file (comp-unit: (Emacs native-comp-unit)) Any)
 (@def native-comp-unit-set-file (comp-unit: (Emacs native-comp-unit) new-file: Any)
       (Emacs native-comp-unit))
 (@def interactive-form (cmd: Any) &Tuple<@interactive~Any>?)
 (@def command-modes (command: Any) &List<Symbol>))


;;; ============================================================
;;; Symbol values and watchers

(et-declare
 ;; nontrivial
 (@def indirect-variable (object: [T]) (replace-in T Var Symbol))
 (@def symbol-value (symbol: Symbol) Any)
 (@def set ([T] symbol: Var newval: T) T)
 (@def add-variable-watcher ([] symbol: Var watch-function: VariableWatcher) Nil)
 (@def remove-variable-watcher ([] symbol: Var watch-function: VariableWatcher) Nil)
 (@def get-variable-watchers (symbol: Symbol) List<VariableWatcher>))


;;; ============================================================
;;; Default and buffer-local values

(et-declare
 (@def default-boundp (symbol: Symbol) Boolean)
 (@def default-value (symbol: Symbol) Any)
 (@def set-default ([T] symbol: Var value: T) T)
 (@def make-variable-buffer-local (variable: [<= V Var]) V)
 (@def make-local-variable (variable: [<= V Var]) V)
 (@def kill-local-variable (variable: [<= V Var]) V)
 (@def local-variable-p (variable: Symbol &optional buffer: Buffer?) Boolean)
 (@def local-variable-if-set-p (variable: Symbol &optional buffer: Buffer?) Boolean)
 (@def variable-binding-locus (variable: Symbol) Buffer|Terminal?))


;;; ============================================================
;;; Function indirection

(et-declare
 ;; nontrivial
 (@def indirect-function (object: Any &optional noerror: Any) Any))


;;; ============================================================
;;; Array elements

(et-declare
 ;; nontrivial
 (@def aref ([T] array: ArefSeq<T> idx: Integer) T)
 (@def aset ([T] array: AsetSeq<T> idx: Integer newelt: T) T))


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
 (@def string-to-number (string: String &optional base: Integer?) Number))


;;; ============================================================
;;; Arithmetic operations

(et-declare
 (@def + (&rest numbers-or-markers: [(<= Nums &List<NumOrMarker>)])
       (switch Nums [Nil 0] [&List<IntOrMarker> Integer] Number))
 (@def - (&optional number-or-marker: [<= Num NumOrMarker]
                    &rest more-numbers-or-markers: [<= Nums &List<NumOrMarker>])
       (switch Nums [Nil 0] [&List<IntOrMarker> Integer] Number))
 (@def * ([(<= Nums &List<NumOrMarker>)] &rest numbers-or-markers: Nums)
       (switch Nums [Nil 1] [&List<IntOrMarker> Integer] Number))
 (@def / ([(<= N NumOrMarker) (<= Ds &List<NumOrMarker>)] number: N &rest divisors: Ds)
       (extends? N IntOrMarker (extends? Ds &List<IntOrMarker> Integer Number) Number))
 (@def % (x: IntOrMarker y: IntOrMarker) Integer)
 (@def mod ([(<= X NumOrMarker) (<= Y NumOrMarker)] x: X y: Y)
       (extends? X IntOrMarker (extends? Y IntOrMarker Integer Number) Number))
 (@def max
       ([(<= N NumOrMarker) (<= Ns &List<NumOrMarker>)] number-or-marker: N
        &rest numbers-or-markers: Ns)
       (extends? &Cons<N~Ns> &List<IntOrMarker> Integer Number))
 (@def min ([(<= N NumOrMarker) (<= Ns &List<NumOrMarker>)]
            number-or-marker: N &rest numbers-or-markers: Ns)
       (extends? &Cons<N~Ns> &List<IntOrMarker> Integer Number))
 (@def logand (&rest ints-or-markers: &List<IntOrMarker>) Integer)
 (@def logior (&rest ints-or-markers: &List<IntOrMarker>) Integer)
 (@def logxor (&rest ints-or-markers: &List<IntOrMarker>) Integer)
 (@def logcount (value: Integer) Integer)
 (@def ash (value: Integer count: Integer) Integer)
 (@def 1+ (number: [<= N NumOrMarker]) (extends? N IntOrMarker Integer Number))
 (@def 1- (number: [<= N NumOrMarker]) (extends? N IntOrMarker Integer Number))
 (@def lognot (number: Integer) Integer)
 (@def byteorder () 66|108))


;;; ============================================================
;;; Bool vector operations

(et-declare
 ;; If c is nil, always return the vector.
 ;; If c is a vector, return the vector only if it changed.
 (@def bool-vector-exclusive-or
       ([(<= C BoolVector?)] a: BoolVector b: BoolVector &optional c: C)
       (if-nil? C BoolVector BoolVector?))
 (@def bool-vector-union
       ([(<= C BoolVector?)] a: BoolVector b: BoolVector &optional c: C)
       (if-nil? C BoolVector BoolVector?))
 (@def bool-vector-intersection
       ([(<= C BoolVector?)] a: BoolVector b: BoolVector &optional c: C)
       (if-nil? C BoolVector BoolVector?))
 (@def bool-vector-set-difference
       ([(<= C BoolVector?)] a: BoolVector b: BoolVector &optional c: C)
       (if-nil? C BoolVector BoolVector?))
 (@def bool-vector-subsetp (a: BoolVector b: BoolVector) Boolean)
 (@def bool-vector-not (a: BoolVector &optional b: BoolVector?) BoolVector)
 (@def bool-vector-count-population (a: BoolVector) Integer)
 (@def bool-vector-count-consecutive (a: BoolVector b: Bool i: Integer) Integer))


;;; ============================================================
