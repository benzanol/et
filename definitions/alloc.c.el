;;; Conses and lists

(et-declare
 (@function cons (car cdr)
            (@generics [L R])
            (car L) (cdr R)
            (@return ConsFresh<L~R>))

 ;; `list' matches its whole argument list as T, then returns a fresh
 ;; copy of that structure's spine.
 (@function list (&rest objects)
            (@generics [T])
            (objects T)
            (@return (eval et--freshen-type-shallow T)))

 (@function make-list (length init)
            (@generics [T])
            (length Integer) (init T)
            (@return ListFresh<T>)))

(et-test
 (et-assert-resolve ConsFresh<Integer~String> (cons 1 "2"))
 (et-assert-resolve Cons<Integer~String> (cons 1 "2"))
 (et-assert-no-resolve ConsFresh<Integer~String> (cons "1" 2))
 (et-assert-resolve ConsFresh<Integer~ListR<String>> (cons 1 nil))
 (et-assert-resolve Cons<Integer~List<String>> (cons 1 (cons "2" nil)))
 (et-assert-resolve List<Integer> (cons 1 (cons 2 nil)))
 (et-assert-no-resolve List<Integer> (cons 1 (cons "2" nil))))

(et-test
 ;; `list' freshens its argument-list spine into a fresh cons chain
 ;; (compared by equivalence: freshening yields an equal-up-to-subtype type).
 (let ((got (et-result-value (et-typecheck-call list Cons<1~2> Cons<3~4>)))
       (want (et ConsFresh<Cons<1~2>~ConsFresh<Cons<3~4>~Nil>>)))
   (and (et-subtype? got want) (et-subtype? want got)))

 (et-assert-resolve ConsR<Integer~ListR<String>> (list 1 "2"))
 (et-assert-no-resolve ConsR<Integer~String> (list "1" 2))
 (et-assert-no-resolve ConsR<Integer~String> (list))

 (et-assert-resolve ListR<Integer> (list 1 2 3))
 (et-assert-resolve ListR<Integer> (list 1))
 (et-assert-no-resolve ListR<Integer> (list 1 "2" 3)))

(et-test
 (et-assert-call ListFresh<Integer> make-list Integer Integer)
 (et-assert-call-errors make-list String Integer))


;;; Strings

(et-declare
 ;; LENGTH copies of the character INIT.  MULTIBYTE only affects encoding.
 (@function make-string (length init &optional multibyte)
            (length Integer) (init Integer) (multibyte Any)
            (@return String)))


;;; Symbols

(et-declare
 ;; A freshly allocated uninterned symbol; never nil.
 (@function make-symbol (name)
            (name String)
            (@return NonNilSymbol)))


;;; Vectors

(et-declare
 (@function vector (&rest objects)
            (@generics [T])
            (objects ListR<T>)
            (@return Vector<T>))

 (@function make-vector (length init)
            (@generics [T])
            (length Integer) (init T)
            (@return Vector<T>)))

(et-test
 (et-assert-call Vector<1|2> vector 1 2)
 (et-assert-call Vector<Never> vector)
 (et-assert-call Vector<Number> vector Integer Number Positive)
 (et-assert-call Vector<Integer> make-vector Integer Integer))
