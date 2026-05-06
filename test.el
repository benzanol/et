;;; Test

(et-matcher [A]
  :or (:and A Cons:R<String~Number>)
  (:and Nil A=True))

#s(et-type
   (#s(et-type-case
       #s(et-alias :List
                   (#s(et-type
                       (#s(et-type-case #s(et-datatype Number nil)
                                        nil)))))
       nil)))
(et-matcher [] :List<Number>)

(et-sub-match
 (et-matcher [:A] Cons<A~Number>)
 (et Cons<String~Number>))

(et-sub-match
 (et-matcher [:A] :List:ro<A>)
 (et :or Nil Cons:R<Number~List:ro<Number>>)
 )

(et-sub-match
 (et-matcher [:A] :or Nil&A=Nil Cons:R<A~Any>)
 (et :List:ro<Number>)
 )

(et-sub-match
 (et-matcher [D] :or Nil&D=Nil Cons:R<Any~D>)
 (et Cons:R<Number~t>)
 )

;; Example alist-get
(et-sub-match
 (et-matcher [K V] Tuple:R K Alist:R<K~V>)
 (et Tuple:R Number Alist:R<Integer~String>))

(et-sub-match
 (et-matcher [K V] Tuple:R K Alist:R<K~V>)
 (et Tuple:R Number Alist:R<Integer~String>))

(et--sub-Constraints
 (et-matcher [K V] Cons:R Number K)
 (et Cons 1 Number)
 )

(et-sub-match
 (et-matcher [K V] PList :hi K :bye String)
 (et PList :bye String :hi Integer)
 )

(et-defalias MyEnum ()
  `(:or (Tuple @dog String Integer)
        (Tuple @cat Symbol)
        (Tuple @turtle Number Number Number)))

(et--sub-constraints
 (et-matcher [K V] MyEnum)
 (et Cons:RR @turtle Any)
 )

(et--and (et :MyEnum)
         (et Cons:RR (:or @cat @turtle) Any))


(et-assert-string= "(#<Integer>)"
  (et-sub-match
   (et-matcher [A] Tuple:R A (Cons:WW A Integer))
   (et Tuple:R Integer (Cons Number Number))))

(et-sub-match
 (et-matcher [T] Any&T)
 ;; (et :or (:and Number (:typeof a)) (:and String (:bind a String)))
 (et :or (:and Any (:typeof a)))
 )

(et--and (et Any) (et :and Any (:typeof a)))

(et-assert-equal t
  (et-subtype? (et :and String (:bind b String) (:bind a Integer))
               (et :and String (:bind a Number))))
(et-assert-equal nil
  (et-subtype? (et :and String (:bind a Integer))
               (et :and String (:bind a Number))))
(et-assert-equal nil
  (et-subtype? (et :and String)
               (et :and String (:bind a Number))))



(et-expand-all-aliases
 (et Tuple:R A (Cons Number Number))
 )


(et-simplify-type
 (et--and
  (et :or (:and Number (:bind a Integer))
      (:and String (:bind a String))
      (:and True (:bind a @hi))
      (:and True (:bind a Symbol))
      )
  (et :or (:and Integer (:bind a String))
      Boolean
      )))


(et-define-type-checker + (:List:r Number)
  (lambda () Number))

(et-define-type-checker car [:L] (Tuple:r (:or Nil&L=Nil Cons:RR<L~Any>)) :L)
(et-define-type-checker cdr [:R] (Tuple:r (:or Nil&R=Nil Cons:RR<Any~R>)) :R)

(et-define-type-checker Cons [:L :R] (Tuple:r :L :R) Cons<L~R>)

(et-define-checker quote (expr)
  (et-dt :literal expr))

(et-define-checker :type (&rest args)
  (et-parse-type (if (eq (length args) 1) (car args) args)))

(et-root-check '(cdr (:type List<Number>)))
(et-root-check '(Cons 1 2))
(et-root-check '(car (Cons 1 2)))
(et-root-check '(car '(5 "hi" 3)))


(et-subtype? (et Boolean) (et Integer))

(et--sub-Constraints
 (et-matcher [] Number)
 (et Boolean))

(et :or List<Never~"hi"> True)

(et-ql List<@hi~Never>)

(et-expand-all-aliases (et Boolean))
(et-subtype? (et Boolean) (et Number))

(et-expand-all-aliases (et Boolean))











(et--and (et :MyEnum)
         (et Cons:RR Any Any)
         )

(et-subtype? (et :literal t) (et Symbol))

(et-and (et :literal 1) (et :literal 2))


#s(et-matcher (K V) (((m:datatype :literal t))))

(et--and (et Cons Number String) (et Cons:ww Integer :str<hi>))







;; Example alist-set
(et--sub-Constraints
 (et-matcher [K V] Tuple:R K :Alist:ao<K~V> V)
 (et Tuple:R Integer :Alist:ao<Number~String> String))

(et--sub-Constraints
 (et-matcher [K V] Cons:wo K V)
 (et Cons:wo String Number))

(et--sub-Constraints
 (et-matcher [K V] :List:wo K)
 (et :List:wo String))

(et--sub-Constraints
 (et-matcher [K V] :Alist:ao K V)
 (et :Alist:ao String Number))



(et-alias-expand (make-et-alias :name :Tree:ro :args (list (et Number))))




(et-alias-expand-as-matcher :Tree '((((m:datatype Number)))) nil)

(cl-assert
 (equal (et--matcher-expand-aliases
         `(((m:datatype :a) (m:datatype :b)) ((m:alias :Tree (((m:datatype Number))))))
         nil)
        '(((m:datatype :a) (m:datatype :b)) ((m:datatype Number))
          ((m:datatype :literal nil))
          ((m:datatype Cons (((m:alias :Tree (((m:datatype Number))))))
                       (((m:alias :List
                                  (((m:alias :Tree (((m:datatype Number))))))))))))))
