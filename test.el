;;; Test

(et-matcher [:a]
  :or (:and :a :cons:ro<string~number>)
  (:and :nil :a=t))

#s(et-type
   (#s(et-type-case
       #s(et-alias :List
                   (#s(et-type
                       (#s(et-type-case #s(et-datatype :number nil)
                                        nil)))))
       nil)))
(et-matcher [] :List<number>)


(et-alias-expand-as-matcher :Tree '((((m:datatype :number)))) nil)

(cl-assert
 (equal (et--matcher-expand-aliases
         `(((m:datatype :a) (m:datatype :b)) ((m:alias :Tree (((m:datatype :number))))))
         nil)
        '(((m:datatype :a) (m:datatype :b)) ((m:datatype :number))
          ((m:datatype :literal nil))
          ((m:datatype :cons (((m:alias :Tree (((m:datatype :number))))))
                       (((m:alias :List
                                  (((m:alias :Tree (((m:datatype :number))))))))))))))
