;;; Arithmetic

;; Everywhere a number is accepted, a marker is too: it is converted to
;; its integer position.  So the arguments are `NumOrMarker's, and a
;; marker counts as an integer when deciding the return type.
;;
;; The argument list is captured whole as the generic `Nums', and the
;; return type branches on what `Nums' turns out to be: an empty call
;; yields the identity element, an all-integer (or marker) call yields
;; Integer, and anything else yields Number.

(et-declare
 (@function + (&rest numbers)
            (@generics [Nums])
            (numbers Nums&ListR<NumOrMarker>)
            (@return (extends? Nums Nil 0 (extends? Nums ListR<IntOrMarker> Integer Number))))
 (@function - (&rest numbers)
            (@generics [Nums])
            (numbers Nums&ListR<NumOrMarker>)
            (@return (extends? Nums Nil 0 (extends? Nums ListR<IntOrMarker> Integer Number))))
 (@function * (&rest numbers)
            (@generics [Nums])
            (numbers Nums&ListR<NumOrMarker>)
            (@return (extends? Nums Nil 1 (extends? Nums ListR<IntOrMarker> Integer Number))))
 (@function / (&rest numbers)
            (@generics [Nums])
            (numbers Nums&NonNilListR<NumOrMarker>)
            (@return (extends? Nums ListR<IntOrMarker> Integer Number)))

 (@function 1+ (number)
            (@generics [Num])
            (number Num&NumOrMarker)
            (@return (extends? Num IntOrMarker Integer Number)))
 (@function 1- (number)
            (@generics [Num])
            (number Num&NumOrMarker)
            (@return (extends? Num IntOrMarker Integer Number)))

 (@function max (&rest numbers)
            (@generics [Nums])
            (numbers Nums&NonNilListR<NumOrMarker>)
            (@return (extends? Nums ListR<IntOrMarker> Integer Number)))
 (@function min (&rest numbers)
            (@generics [Nums])
            (numbers Nums&NonNilListR<NumOrMarker>)
            (@return (extends? Nums ListR<IntOrMarker> Integer Number)))

 (@function mod (x y)
            (@generics [X Y])
            (x X&NumOrMarker) (y Y&NumOrMarker)
            (@return (extends? X IntOrMarker (extends? Y IntOrMarker Integer Number) Number)))

 ;; `%' is integer-only and always yields an Integer.
 (@function % (x y) (x IntOrMarker) (y IntOrMarker) (@return Integer)))

(et-test
 ;; Markers count as integers
 (et-assert-resolve Integer
   (+ (:type Integer) (:type Marker)))
 (et-assert-resolve Number
   (+ (:type Marker) (:type 1.5)))
 (et-assert-resolve Integer
   (1+ (:type Marker)))
 (et-assert-resolve Integer
   (max (:type Marker) (:type Integer)))
 (et-assert-resolve Integer
   (mod (:type Marker) (:type Integer)))
 (et-assert-resolve Integer
   (% (:type Marker) (:type Marker)))
 (et-assert-resolve-errors
  (+ (:type Marker) (:type String))))

(et-test
 ;; All-integer arguments -> Integer
 (et-assert-resolve Integer
   (+ (:type Integer) (:type Integer) (:type 1) (:type 2) (:type 3)))
 (et-assert-resolve Integer
   (- (:type Integer) (:type Integer)))
 (et-assert-resolve Integer
   (* (:type 1) (:type 2) (:type 3)))
 (et-assert-resolve Integer
   (/ (:type Integer) (:type Integer)))
 (et-assert-resolve Integer
   (+ (:type 1)))
 (et-assert-resolve Integer
   (/ (:type 1)))
 ;; Any non-integer number -> Number
 (et-assert-resolve Number
   (+ (:type Integer) (:type 2.1) (:type 3)))
 (et-assert-resolve Number
   (* (:type Number) (:type Integer)))
 ;; Identity element for an empty call
 (et-assert-resolve 0
   (+))
 (et-assert-resolve 0
   (-))
 (et-assert-resolve 1
   (*))
 ;; `/' requires at least one argument
 (et-assert-resolve-errors
  (/))
 ;; Non-numbers are rejected
 (et-assert-resolve-errors
  (+ (:type String)))
 (et-assert-resolve-errors
  (+ (:type Integer) (:type String))))

(et-test
 (et-assert-resolve Integer (1+ 1))
 (et-assert-resolve Number (1- 1.5))
 (et-assert-resolve-errors
  (1+ (:type String)))
 (et-assert-resolve Integer
   (max (:type Integer) (:type Integer)))
 (et-assert-resolve Number
   (min (:type Integer) (:type Number)))
 (et-assert-resolve-errors
  (max))
 (et-assert-resolve Integer (mod 7 3))
 (et-assert-resolve Number (mod 7.5 3)))


;;; Bitwise operations

;; All of these operate on (and produce) integers.  `logand'/`logior'/
;; `logxor' also accept markers, which they convert to integers; the
;; others are integer-only.

(et-declare
 (@function logand (&rest ints) (ints ListR<IntOrMarker>) (@return Integer))
 (@function logior (&rest ints) (ints ListR<IntOrMarker>) (@return Integer))
 (@function logxor (&rest ints) (ints ListR<IntOrMarker>) (@return Integer))
 (@function lognot (number) (number Integer) (@return Integer))
 (@function ash (value count) (value Integer) (count Integer) (@return Integer))
 (@function logcount (value) (value Integer) (@return Integer)))

(et-test
 (et-assert-resolve Integer
   (logand (:type Integer) (:type Marker)))
 (et-assert-resolve-errors
  (ash (:type Marker) (:type Integer))))


;;; Bool vector operations

;; The set operations write into the optional third argument when it is
;; given, and allocate a fresh bool vector otherwise; either way the
;; result is the bool vector they produce.

(et-declare
 (@function bool-vector-exclusive-or (a b &optional c)
            (a BoolVector) (b BoolVector) (c BoolVector|Nil) (@return BoolVector))
 (@function bool-vector-union (a b &optional c)
            (a BoolVector) (b BoolVector) (c BoolVector|Nil) (@return BoolVector))
 (@function bool-vector-intersection (a b &optional c)
            (a BoolVector) (b BoolVector) (c BoolVector|Nil) (@return BoolVector))
 (@function bool-vector-set-difference (a b &optional c)
            (a BoolVector) (b BoolVector) (c BoolVector|Nil) (@return BoolVector))
 (@function bool-vector-not (a &optional b)
            (a BoolVector) (b BoolVector|Nil) (@return BoolVector))
 (@function bool-vector-subsetp (a b)
            (a BoolVector) (b BoolVector) (@return Boolean))
 (@function bool-vector-count-population (a)
            (a BoolVector) (@return Integer))
 ;; Counts the elements of A equal to B (as a truth value) from index I on.
 (@function bool-vector-count-consecutive (a b i)
            (a BoolVector) (b Any) (i Integer) (@return Integer)))

(et-test
 (et-assert-resolve BoolVector
   (bool-vector-union (:type BoolVector) (:type BoolVector)))
 (et-assert-resolve BoolVector
   (bool-vector-not (:type BoolVector)))
 (et-assert-resolve Boolean
   (bool-vector-subsetp (:type BoolVector) (:type BoolVector)))
 (et-assert-resolve Integer
   (bool-vector-count-population (:type BoolVector)))
 (et-assert-resolve-errors
  (bool-vector-union (:type BoolVector) (:type VectorR<Any>))))


;;; Comparison / equality

(et-declare
 (@function eq (a b)
            (@generics [A B])
            (a A) (b B)
            (@return (or Nil (and True (bindsof (and A B))))))

 ;; `=' compares by numeric value, not by identity: a marker is `=' to its
 ;; position without being that integer.  So a true result does not tell
 ;; us the two arguments have the same type, and `=' does not narrow.
 (@function = (a b) (a NumOrMarker) (b NumOrMarker) (@return Boolean))

 (@function < (a b) (a NumOrMarker) (b NumOrMarker) (@return Boolean))
 (@function <= (a b) (a NumOrMarker) (b NumOrMarker) (@return Boolean))
 (@function > (a b) (a NumOrMarker) (b NumOrMarker) (@return Boolean))
 (@function >= (a b) (a NumOrMarker) (b NumOrMarker) (@return Boolean))
 (@function /= (num1 num2) (num1 NumOrMarker) (num2 NumOrMarker) (@return Boolean)))

(et-test
 (et-assert-resolve Boolean (< 1 2))
 (et-assert-resolve Boolean (= 1 2))
 (et-assert-resolve-errors (< 1 "2"))
 ;; Markers compare against numbers
 (et-assert-resolve Boolean
   (= (:type Marker) (:type Integer)))
 (et-assert-resolve Boolean
   (= (:type Integer) (:type Marker)))
 (et-assert-resolve Boolean
   (< (:type Marker) (:type Number))))


;;; Predicates

(et-declare
 (@function stringp (object)
            (@generics [T]) (object T)
            (@return (or (and True (bindsof (and T String)))
                         (and Nil (bindsof (subtract T String))))))
 (@function symbolp (object)
            (@generics [T]) (object T)
            (@return (or (and True (bindsof (and T Symbol)))
                         (and Nil (bindsof (subtract T Symbol))))))
 (@function numberp (object)
            (@generics [T]) (object T)
            (@return (or (and True (bindsof (and T Number)))
                         (and Nil (bindsof (subtract T Number))))))
 (@function integerp (object)
            (@generics [T]) (object T)
            (@return (or (and True (bindsof (and T Integer)))
                         (and Nil (bindsof (subtract T Integer))))))
 (@function consp (object)
            (@generics [T]) (object T)
            (@return (or (and True (bindsof (and T Cons)))
                         (and Nil (bindsof (subtract T Cons))))))
 (@function listp (object)
            (@generics [T]) (object T)
            (@return (or (and True (bindsof (and T Nil|Cons)))
                         (and Nil (bindsof (subtract T Nil|Cons))))))
 (@function nlistp (object)
            (@generics [T]) (object T)
            (@return (or (and True (bindsof (subtract T Nil|Cons)))
                         (and Nil (bindsof (and T Nil|Cons))))))
 (@function vectorp (object)
            (@generics [T]) (object T)
            (@return (or (and True (bindsof (and T Vector)))
                         (and Nil (bindsof (subtract T Vector))))))
 (@function null (object)
            (@generics [T]) (object T)
            (@return (or (and True (bindsof (and T Nil)))
                         (and Nil (bindsof (subtract T Nil))))))
 (@function not (object)
            (@generics [T]) (object T)
            (@return (or (and True (bindsof (and T Nil)))
                         (and Nil (bindsof (subtract T Nil))))))
 (@function atom (object)
            (@generics [T]) (object T)
            (@return (or (and True (bindsof (subtract T Cons)))
                         (and Nil (bindsof (and T Cons))))))

 (@function functionp (object)
            (@generics [T]) (object T)
            (@return (or (and True (bindsof (and T Function<Never~Any>)))
                         (and Nil (bindsof (subtract T Function<Never~Any>)))))))

;; Predicates for the emacs-internal datatypes.

(et-declare
 (@function bufferp (object)
            (@generics [T]) (object T)
            (@return (or (and True (bindsof (and T Buffer)))
                         (and Nil (bindsof (subtract T Buffer))))))
 (@function markerp (object)
            (@generics [T]) (object T)
            (@return (or (and True (bindsof (and T Marker)))
                         (and Nil (bindsof (subtract T Marker))))))
 (@function char-table-p (object)
            (@generics [T]) (object T)
            (@return (or (and True (bindsof (and T CharTable)))
                         (and Nil (bindsof (subtract T CharTable))))))
 (@function bool-vector-p (object)
            (@generics [T]) (object T)
            (@return (or (and True (bindsof (and T BoolVector)))
                         (and Nil (bindsof (subtract T BoolVector))))))
 (@function threadp (object)
            (@generics [T]) (object T)
            (@return (or (and True (bindsof (and T Thread)))
                         (and Nil (bindsof (subtract T Thread))))))
 (@function mutexp (object)
            (@generics [T]) (object T)
            (@return (or (and True (bindsof (and T Mutex)))
                         (and Nil (bindsof (subtract T Mutex))))))
 (@function condition-variable-p (object)
            (@generics [T]) (object T)
            (@return (or (and True (bindsof (and T ConditionVariable)))
                         (and Nil (bindsof (subtract T ConditionVariable))))))

 ;; Wherever a number is accepted, a marker usually is too, so these two
 ;; narrow to a union.
 (@function integer-or-marker-p (object)
            (@generics [T]) (object T)
            (@return (or (and True (bindsof (and T IntOrMarker)))
                         (and Nil (bindsof (subtract T IntOrMarker))))))
 (@function number-or-marker-p (object)
            (@generics [T]) (object T)
            (@return (or (and True (bindsof (and T NumOrMarker)))
                         (and Nil (bindsof (subtract T NumOrMarker)))))))

(et-test
 (et-assert-resolve True&{$a::Buffer}
   (bufferp (:type Buffer&{::$a})))
 (et-assert-resolve (or True&{$a::Marker} Nil&{$a::Integer})
   (markerp (:type {::$a}&{Marker|Integer})))
 (et-assert-resolve (or True&{$a::Marker} Nil&{$a::String})
   (integer-or-marker-p (:type {::$a}&{Marker|String})))
 (et-assert-resolve Boolean (bool-vector-p (bool-vector t))))


;;; Function objects

;; A function object is one of three leaf datatypes: `InterpretedFunction'
;; (a lambda body), `ByteCodeFunction' (bytecode), or `Subr' (machine
;; code -- a C builtin or a natively compiled Lisp function). `Closure' is
;; the alias for the first two, which is exactly what `closurep' tests.
;;
;; These sit alongside the `Function' datatype rather than under it:
;; `Function<ARGS~RET>' describes a callable's signature, while these
;; describe its runtime representation. A value can be both, which is why
;; narrowing with them intersects rather than replaces.

(et-declare
 (@function closurep (object)
            (@generics [T]) (object T)
            (@return (or (and True (bindsof (and T Closure)))
                         (and Nil (bindsof (subtract T Closure))))))
 (@function interpreted-function-p (object)
            (@generics [T]) (object T)
            (@return (or (and True (bindsof (and T InterpretedFunction)))
                         (and Nil (bindsof (subtract T InterpretedFunction))))))
 (@function byte-code-function-p (object)
            (@generics [T]) (object T)
            (@return (or (and True (bindsof (and T ByteCodeFunction)))
                         (and Nil (bindsof (subtract T ByteCodeFunction))))))
 (@function subrp (object)
            (@generics [T]) (object T)
            (@return (or (and True (bindsof (and T Subr)))
                         (and Nil (bindsof (subtract T Subr))))))

 ;; Only a subr can be natively compiled, so a true result narrows to
 ;; `Subr'. A false one narrows to nothing: plenty of subrs are not
 ;; natively compiled.
 (@function native-comp-function-p (object)
            (@generics [T]) (object T)
            (@return (or (and True (bindsof (and T Subr))) Nil))))

(et-test
 (et-assert-resolve True&{$a::Subr}
   (subrp (:type Subr&{::$a})))
 (et-assert-resolve (or True&{$a::Closure} Nil&{$a::Subr})
   (closurep (:type {::$a}&{Closure|Subr})))
 ;; `Closure' is the union of the two, so each half narrows out of it.
 (et-assert-resolve (or True&{$a::ByteCodeFunction} Nil&{$a::InterpretedFunction})
   (byte-code-function-p (:type {::$a}&Closure)))
 (et-assert-resolve (or True&{$a::InterpretedFunction} Nil&{$a::ByteCodeFunction})
   (interpreted-function-p (:type {::$a}&Closure)))
 ;; A false `native-comp-function-p' tells us nothing.
 (et-assert-resolve (or True&{$a::Subr} Nil)
   (native-comp-function-p (:type Subr&{::$a}))))


;;; Subrs

;; MAX is `many' for a `&rest' function, and `unevalled' for a special
;; form.

(et-declare
 (@function subr-name (subr) (subr Subr) (@return String))
 (@function subr-arity (subr)
            (subr Subr) (@return Cons<Integer~Integer|@many|@unevalled>)))

(et-test
 (et-assert-resolve String
   (subr-name (:type Subr)))
 (et-assert-resolve Cons<Integer~Integer|@many|@unevalled>
   (subr-arity (:type Subr)))
 (et-assert-resolve-errors
  (subr-name (:type Closure))))

;; These predicates have no exact datatype to narrow to (no Float,
;; natural-number, keyword, sequence, or array datatype exists), so they
;; only carry their Boolean return type.
(et-declare
 (@function floatp (object) (object Any) (@return Boolean))
 (@function natnump (object) (object Any) (@return Boolean))
 (@function keywordp (object) (object Any) (@return Boolean))
 (@function sequencep (object) (object Any) (@return Boolean))
 (@function arrayp (object) (object Any) (@return Boolean))
 (@function char-or-string-p (object) (object Any) (@return Boolean)))

(et-test
 (et-assert-resolve True&{$a::Cons}
   (consp (:type Cons&{::$a})))
 (et-assert-resolve (or True&{$a::String} Nil&{$a::Number})
   (stringp (:type {::$a}&{String|Number})))
 ;; There is no defined intersection of Positive and Integer, so the
 ;; checker must approximate to a SUPERSET (never to Never).
 (not (et-subtype? (et-root-check-type '(integerp (:type Positive&{::$a})))
                   (et Nil))))

(et-test
 ;; Predicates narrow a typeof-bound argument (`$a') in each branch.
 (et-assert-resolve True&{$a::Vector<Any>}
   (vectorp (:type Vector<Any>&{::$a})))
 (et-assert-resolve (or True&{$a::Vector<Any>} Nil&{$a::Number})
   (vectorp (:type {::$a}&{Vector<Any>|Number})))
 (et-assert-resolve True&{$a::Integer}
   (atom (:type Integer&{::$a})))
 (et-assert-resolve (or True&{$a::Integer} Nil&{$a::Cons})
   (atom (:type {::$a}&{Integer|Cons}))))


;;; Cons cells

(et-declare
 (@alias MatchCar [T]
         (or (and Nil (set T Nil))
             (ConsR T Any)))
 (@alias MatchCdr [T]
         (or (and Nil (set T Nil))
             (ConsR Any T))))

(et-declare
 (@function car (list)
            (@generics [T]) (list (MatchCar T)) (@return T))
 (@function cdr (list)
            (@generics [T]) (list (MatchCdr T)) (@return T))
 (@function car-safe (object)
            (@generics [O]) (object O)
            (@return (infer O [T] ConsR<T~Any> T Nil)))
 (@function cdr-safe (object)
            (@generics [O]) (object O)
            (@return (infer O [T] ConsR<Any~T> T Nil)))
 ;; NOTE: the parameter order here is preserved verbatim from the
 ;; original `setcar' checker.
 (@function setcar (cell newcar)
            (@generics [T]) (cell ConsW<T~Never>) (newcar T) (@return T))
 ;; `setcdr' mirrors `setcar' (same verbatim parameter order), writing
 ;; the cdr instead of the car.
 (@function setcdr (cell newcdr)
            (@generics [T]) (cell ConsW<Never~T>) (newcdr T) (@return T)))

(et-test
 (et-assert-resolve Integer (car (list 1 2.2 3)))
 (et-assert-no-resolve Integer (car (list 1.1 2 3)))
 (et-assert-resolve Integer (car (cons 1 "3")))
 (et-assert-resolve ListR<Integer> (car (cons (list 1) "3")))
 (et-assert-resolve Integer (car (car (cons (list 1) "3"))))

 (et-assert-resolve Never
   (cdr (:type Never)))
 (et-assert-resolve Nil
   (cdr (:type Nil)))
 (et-assert-resolve Nil|String
   (cdr (:type Nil|ConsR<Integer~String>)))
 (et-assert-resolve-errors
  (cdr (:type Nil|ConsR<Integer~String>|String)))
 (et-assert-resolve-errors
  (cdr (:type :any)))

 (et-assert-resolve Never
   (car (:type Never)))
 (et-assert-resolve Nil
   (car (:type Nil)))
 (et-assert-resolve Nil|Integer
   (car (:type Nil|ConsR<Integer~String>)))
 (et-assert-resolve-errors
  (car (:type Nil|ConsR<Integer~String>|String)))
 (et-assert-resolve-errors
  (car (:type Any)))

 (et-assert-resolve Nil|Integer
   (car (:type ListR<Integer>)))
 (et-assert-resolve Nil|Integer|String
   (car (:type ListR<Integer>|ConsR<String~Nil>))))

(et-test
 (et-assert-resolve ListR<Number> (cdr (list 1 2.2 3)))
 (et-assert-resolve ListR<Integer> (cdr (list 1.1 2 3)))
 (et-assert-resolve Integer (cdr (cons "1" 2)))
 (et-assert-no-resolve Integer (cdr (cons 1 "2")))
 (et-assert-resolve Boolean (cdr (cdr (cdr (list 1 2 3))))))

(et-test
 (et-root-check-type '(setcar (:type ConsW<Number~Number>) (:type Number)))
 (et-root-check-type '(setcdr (:type ConsW<Number~Number>) (:type Number))))


;;; Array access

;; `aref' indexes a vector (yielding its element type) or a string
;; (yielding an Integer character code). Kept as an `et-define-type-checker'
;; because its argument type carries a binding constraint
;; (`{String&T=Integer}') that the `@function' declaration form does not
;; express.
(et-define-type-checker aref [T] (Args VectorR<T>|{String&T=Integer} Integer) T)

;; `aset' is the writing counterpart: it stores a value of the array's
;; element type and returns it.  A vector must be writable with T
;; (`VectorW<T>'); a string element is an Integer character code.
(et-define-type-checker aset [T] (Args VectorW<T>|{String&T=Integer} Integer T) T)

(et-test
 (et-assert-resolve Integer
   (aref (:type String) (:type Integer)))
 (et-assert-resolve Symbol|Integer
   (aref (:type (or VectorR<Symbol> String)) (:type Integer)))
 (et-assert-resolve-errors
  (aref (:type (or VectorR<Symbol> String ListR<Any>)) (:type Integer))))

(et-test
 (et-assert-resolve Integer
   (aset (:type String) (:type Integer) (:type Integer)))
 (et-assert-resolve Integer
   (aset (:type VectorW<Integer>) (:type Integer) (:type Integer)))
 (et-assert-resolve-errors
  (aset (:type ListR<Any>) (:type Integer) (:type Integer))))


;;; Symbols and conversions

(et-declare
 (@function symbol-name (symbol) (symbol Symbol) (@return String))
 (@function number-to-string (number) (number Number) (@return String))
 (@function string-to-number (string &optional base)
            (string String) (base Integer|Nil) (@return Number))
 ;; `type-of'/`cl-type-of' always return a (non-nil) type-naming symbol.
 (@function type-of (object) (object Any) (@return NonNilSymbol))
 (@function cl-type-of (object) (object Any) (@return NonNilSymbol)))

(et-test
 (et-assert-resolve String (symbol-name 'foo))
 (et-assert-resolve String (number-to-string 5))
 (et-assert-resolve Number (string-to-number "5"))
 (et-assert-resolve NonNilSymbol (type-of 5)))


;;; Function cells

;; `fset' returns the definition it was given, unchanged. It does no
;; function-type handling: the definition is not constrained, and it is
;; not recorded as the `et-function-type' of SYMBOL.

(et-declare
 (@function fset (symbol definition)
            (@generics [T])
            (symbol Symbol) (definition T)
            (@return T)))

(et-test
 (et-assert-resolve Integer (fset 'foo 1))
 (et-assert-resolve-errors (fset "foo" 1)))


;;; Buffer-local variables

;; BUFFER defaults to the current buffer.  A variable's binding is local
;; to a buffer or to a terminal, or to neither (nil).

(et-declare
 (@function local-variable-p (variable &optional buffer)
            (variable Symbol) (buffer Buffer|Nil) (@return Boolean))
 (@function local-variable-if-set-p (variable &optional buffer)
            (variable Symbol) (buffer Buffer|Nil) (@return Boolean))
 (@function variable-binding-locus (variable)
            (variable Symbol) (@return Buffer|Terminal|Nil)))

(et-test
 (et-assert-resolve Boolean (local-variable-p 'foo))
 (et-assert-resolve Buffer|Terminal|Nil (variable-binding-locus 'foo))
 (et-assert-resolve-errors (local-variable-p "foo")))
