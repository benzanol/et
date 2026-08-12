;;; set/put

(defun et--symbol-type-property-type (sym-type)
  (let* ((literals nil)
         (only-literals t))
    (cl-loop for case in (et-type-cases sym-type)
             for val = (et-type-case-value case)
             if (and (et-datatype-p val) (eq (et-datatype-name val) 'Literal))
             collect (let* ((sym (car (et-datatype-args val))))
                       (or (and (symbolp sym)
                                (get sym 'et-symbol-property-type))
                           (et Any)))
             into sym-types
             else return (et Any)
             finally return (et-simplify-type (apply #'et--or sym-types)))))

(et-define-op symbol-property (sym-type)
  (et--symbol-type-property-type sym-type))

(et-declare
 (@function get (symbol propname)
            (@generics [(<= S Symbol)])
            (symbol Symbol) (propname S)
            (@return (symbol-property S)))
 (@function put (symbol propname value)
            (@generics [(<= S Symbol)])
            (symbol Symbol) (propname S)
            (@return (symbol-property S))))


;;; List access

(et-declare
 (@function nth (n list)
            (@generics [T]) (n Integer) (list ListR<T>) (@return T|Nil))
 (@function nthcdr (n list)
            (@generics [T]) (n Integer) (list ListR<T>) (@return ListR<T>))
 (@function take (n list)
            (@generics [T]) (n Integer) (list ListR<T>) (@return ListFresh<T>))
 (@function ntake (n list)
            (@generics [T]) (n Integer) (list ListR<T>) (@return ListR<T>))
 (@function elt (sequence n)
            (@generics [T]) (sequence ListR<T>|VectorR<T>) (n Integer)
            (@return T|Nil))
 (@function length (sequence)
            (sequence String|ListR<Any>|VectorR<Any>) (@return Integer))
 (@function safe-length (list) (list Any) (@return Integer))
 (@function proper-list-p (object) (object Any) (@return Integer|Nil))

 (@function length< (sequence length)
            (sequence String|ListR<Any>|VectorR<Any>) (length Integer)
            (@return Boolean))
 (@function length> (sequence length)
            (sequence String|ListR<Any>|VectorR<Any>) (length Integer)
            (@return Boolean))
 (@function length= (sequence length)
            (sequence String|ListR<Any>|VectorR<Any>) (length Integer)
            (@return Boolean)))

(et-test
 (et-assert-resolve Number|String|Nil
  (nth (:type Integer) (:type ConsR<Number~ListR<String>>)))
 (et-assert-resolve ListR<Number|String>
  (nthcdr (:type Integer) (:type ConsR<Number~ListR<String>>)))
 (et-assert-resolve ListR<Never>
  (nthcdr (:type Integer) (:type Nil)))
 (et-assert-resolve ListFresh<Integer>
  (take (:type Integer) (:type ListR<Integer>)))
 (et-assert-resolve ListR<Integer>
  (ntake (:type Integer) (:type ListR<Integer>))))

(et-test
 (et-assert-resolve Integer
  (length (:type VectorR<Number>|ListR<String>)))
 (et-assert-resolve-errors
 (length (:type VectorR<Number>|ListR<String>|Number)))
 (et-assert-resolve Integer
  (safe-length (:type ListR<Any>)))
 (et-assert-resolve Integer|Nil
  (proper-list-p (:type ListR<Any>))))

(et-test
 (et-assert-resolve Integer|Nil
  (elt (:type ListR<Integer>) (:type Integer)))
 (et-assert-resolve Symbol|Nil
  (elt (:type VectorR<Symbol>) (:type Integer)))
 (et-assert-resolve Boolean
  (length< (:type ListR<Any>) (:type Integer)))
 (et-assert-resolve Boolean
  (length= (:type String) (:type Integer))))


;;; Membership and association lists

(et-declare
 (@function memq (elt list)
            (@generics [T]) (elt Any) (list ListR<T>) (@return ListR<T>|Nil))
 (@function memql (elt list)
            (@generics [T]) (elt Any) (list ListR<T>) (@return ListR<T>|Nil))
 (@function member (elt list)
            (@generics [T]) (elt Any) (list ListR<T>) (@return ListR<T>|Nil))
 (@function delq (elt list)
            (@generics [T]) (elt Any) (list ListR<T>) (@return ListR<T>))
 (@function delete (elt list)
            (@generics [T]) (elt Any) (list ListR<T>) (@return ListR<T>))

 (@function assq (key alist)
            (@generics [C]) (key Any) (alist ListR<C&Cons>) (@return C|Nil))
 (@function assoc (key alist &optional testfn)
            (@generics [C]) (key Any) (alist ListR<C&Cons>) (testfn Any)
            (@return C|Nil))
 (@function rassq (value alist)
            (@generics [C]) (value Any) (alist ListR<C&Cons>) (@return C|Nil))
 (@function rassoc (value alist)
            (@generics [C]) (value Any) (alist ListR<C&Cons>) (@return C|Nil))

 (@function copy-alist (alist)
            (@generics [C]) (alist ListR<C&Cons>) (@return ListFresh<C>)))

(et-test
 (et-assert-resolve ListR<Integer>|Nil
  (memq (:type Any) (:type ListR<Integer>)))
 (et-assert-resolve ListR<String>|Nil
  (member (:type Any) (:type ListR<String>)))
 (et-assert-resolve ListR<Integer>
  (delq (:type Any) (:type ListR<Integer>)))
 (et-assert-resolve Cons<1~2>|Nil (assq 1 (list (cons 1 2))))
 (et-assert-resolve Cons<1~2>|Nil (rassq 2 (list (cons 1 2)))))


;;; Mapping

(et-declare
 (@function mapcar (function sequence)
            (@generics [T R])
            (function Function<Args<T>~R>)
            (sequence ListR<T>)
            (@return ListFresh<R>))
 (@function mapc (function sequence)
            (@generics [T R])
            (function Function<Args<T>~R>)
            (sequence ListR<T>)
            (@return Nil))
 (@function mapcan (function sequence)
            (@generics [T E])
            (function Function<Args<T>~List<E>>)
            (sequence ListR<T>)
            (@return List<E>))
 (@function mapconcat (function sequence &optional separator)
            (@generics [T])
            (function Function<Args<T>~String>)
            (sequence ListR<T>)
            (separator String|Nil)
            (@return String)))

(et-test
 (et-assert-resolve ListFresh<String>
  (mapcar (:type Function<Args<Integer>~String>) (:type ListR<Integer>)))
 (et-assert-resolve Nil
  (mapc (:type Function<Args<Integer>~String>) (:type ListR<Integer>)))
 (et-assert-resolve String
  (mapconcat (:type Function<Args<Integer>~String>) (:type ListR<Integer>)))
 (et-assert-resolve String
  (mapconcat (:type Function<Args<Integer>~String>) (:type ListR<Integer>) (:type String)))
 (et-assert-resolve List<String>
  (mapcan (:type Function<Args<Integer>~List<String>>) (:type ListR<Integer>)))
 (et-assert-resolve-errors
 (mapcar (:type Function<Args<String>~String>) (:type ListR<Integer>))))


;;; append / nconc

;; `append' builds a fresh spine for every argument except the last,
;; which it shares (so its element types stay covariant). `append's
;; `@return' resolves `et--append-return-type' lazily (at check time, via
;; the `(eval ...)' type form below), so the helper only needs to be
;; bound by then -- not when this declaration is parsed.
(et-define-op append (lists)
  (or (et-checker-infer lists [] Nil Nil)
      (et-checker-infer lists [S] (ConsR S Nil) S)
      (et-checker-infer lists [E R] (ConsR List<E> R)
                        (AppendFresh E (append R)))))

(et-declare
 (@alias AppendFresh [E R] (or R (ConsFresh E (AppendFresh E R))))

 (@function append (&rest sequences)
            (@generics [A])
            (sequences A)
            (@return (append A)))

 (@function nconc (&rest lists)
            (@generics [E])
            (lists ListR<List<E>>)
            (@return List<E>)))

(et-test
 (equal (list (et Number))
        (et-match-result-value
         (et-sub-match
          (et-matcher [T] List<T>)
          (et-root-check-type '(append (:type List<1>) (:type List<Integer>) (:type List<Number>))))))

 (equal (list (et Number))
        (et-match-result-value
         (et-sub-match
          (et-matcher [T] List<T>)
          (et-root-check-type '(append (:type List<1>) (:type List<Number>) (:type List<Integer>) (:type Nil))))))
 (equal (list (et Number))
        (et-match-result-value
         (et-sub-match
          (et-matcher [T] ListR<T>)
          (et-root-check-type '(append (:type List<1>) (:type List<Integer>) (:type List<Number>) (:type List<1>)))))))


;;; concat / vconcat

(et-declare
 (@function concat (&rest sequences)
            (sequences ListR<String|ListR<Any>|VectorR<Any>>)
            (@return String))
 (@function vconcat (&rest sequences)
            (sequences ListR<String|ListR<Any>|VectorR<Any>>)
            (@return Vector<Any>)))

(et-test
 (et-assert-resolve String
  (concat (:type String) (:type String)))
 (et-assert-resolve String
  (concat (:type ListR<Integer>) (:type String)))
 (et-assert-resolve Vector<Any>
  (vconcat (:type String) (:type ListR<Integer>))))


;;; reverse / nreverse / copy

(et-declare
 (@function reverse (sequence)
            (@generics [T]) (sequence ListR<T>) (@return List<T>))
 (@function nreverse (sequence)
            (@generics [T]) (sequence List<T>) (@return List<T>))
 (@function copy-sequence (sequence)
            (@generics [T]) (sequence ListR<T>) (@return ListFresh<T>)))

(et-test
 (et-assert-resolve List<Integer>
  (reverse (:type ListR<Integer>)))
 (et-assert-resolve List<Integer>
  (nreverse (:type List<Integer>)))
 (et-assert-resolve ListFresh<Integer>
  (copy-sequence (:type ListR<Integer>))))


;;; Equality

;; (`eq' lives in data.c; `eql'/`equal' are defined in fns.c.)
(et-declare
 (@function eql (a b)
            (@generics [A B]) (a A) (b B)
            (@return (or Nil (and True (bindsof (and A B))))))
 (@function equal (a b)
            (@generics [A B]) (a A) (b B)
            (@return (or Nil (and True (bindsof (and A B))))))
 (@function equal-including-properties (a b)
            (@generics [A B]) (a A) (b B)
            (@return (or Nil (and True (bindsof (and A B))))))
 (@function value< (a b)
            (a Any) (b Any) (@return Boolean)))

(et-test
 (et-assert-resolve Boolean (equal 1 "2"))
 (et-assert-resolve Boolean (eql 1 2))
 (et-assert-resolve Boolean (value< 1 2)))


;;; Strings

(et-declare
 (@function string-search (needle haystack &optional start-pos)
            (needle String) (haystack String) (start-pos Integer|Nil)
            (@return Integer|Nil))
 (@function string-distance (string1 string2 &optional bytecompare)
            (string1 String) (string2 String) (bytecompare Any)
            (@return Integer))
 (@function string-bytes (string) (string String) (@return Integer))
 (@function string-lessp (string1 string2)
            (string1 String) (string2 String) (@return Boolean))
 (@function string-version-lessp (string1 string2)
            (string1 String) (string2 String) (@return Boolean))
 (@function string-equal (string1 string2)
            (string1 String) (string2 String) (@return Boolean))

 (@function substring (string &optional from to)
            (@generics [S T])
            (string S&{String|VectorR<T>}) (from Integer|Nil) (to Integer|Nil)
            (@return (extends? S String String Vector<T>)))
 (@function substring-no-properties (string &optional from to)
            (string String) (from Integer|Nil) (to Integer|Nil)
            (@return String))

 ;; Multibyte/unibyte conversions: all String -> String.
 (@function string-to-multibyte (string) (string String) (@return String))
 (@function string-to-unibyte (string) (string String) (@return String))
 (@function string-as-multibyte (string) (string String) (@return String))
 (@function string-as-unibyte (string) (string String) (@return String))
 (@function string-make-multibyte (string) (string String) (@return String))
 (@function string-make-unibyte (string) (string String) (@return String)))

(et-test
 (et-assert-resolve Integer|Nil (string-search "a" "abc"))
 (et-assert-resolve Boolean (string-lessp "a" "b"))
 (et-assert-resolve Boolean (string-equal "a" "a"))
 (et-assert-resolve Integer (string-distance "a" "b"))
 (et-assert-resolve-errors (string-search "a" 5)))

(et-test
 (et-assert-resolve String
  (substring (:type String) (:type Integer)))
 (et-assert-resolve String
  (substring (:type String)))
 (et-assert-resolve Vector<Symbol>
  (substring (:type VectorR<Symbol>) (:type Integer) (:type Integer)))
 (et-assert-resolve String
  (substring-no-properties (:type String)))
 (et-assert-resolve String
  (string-to-multibyte (:type String)))
 (et-assert-resolve-errors
 (substring-no-properties (:type VectorR<Symbol>))))


;;; Symbol properties

(et-declare
 (@function put (symbol propname value)
            (@generics [V]) (symbol Symbol) (propname Symbol) (value V)
            (@return V)))

(et-test
 (et-assert-resolve String
  (put (:type NonNilSymbol) (:type NonNilSymbol) (:type String))))


;;; Hashing

;; These digest either a string or (a portion of) a buffer. START and END
;; are indices into the string, or positions in the buffer.

(et-declare
 (@function md5 (object &optional start end coding-system noerror)
            (object Buffer|String) (start IntOrMarker|Nil) (end IntOrMarker|Nil)
            (coding-system Symbol|Nil) (noerror Any)
            (@return String))
 (@function secure-hash (algorithm object &optional start end binary)
            (algorithm Symbol) (object Buffer|String)
            (start IntOrMarker|Nil) (end IntOrMarker|Nil) (binary Any)
            (@return String))
 ;; BUFFER-OR-NAME defaults to the current buffer.
 (@function buffer-hash (&optional buffer-or-name)
            (buffer-or-name Buffer|String|Nil) (@return String)))

(et-test
 (et-assert-resolve String (md5 "hi"))
 (et-assert-resolve String (secure-hash 'sha256 "hi"))
 (et-assert-resolve String (buffer-hash))
 (et-assert-resolve-errors (md5 5)))


;;; Misc

(et-declare
 (@function identity (argument)
            (@generics [T]) (argument T) (@return T))
 (@function random (&optional limit)
            (limit Any) (@return Integer))
 (@function featurep (feature &optional subfeature)
            (feature Symbol) (subfeature Any) (@return Boolean)))

(et-test
 (et-assert-resolve Integer
  (identity (:type Integer)))
 (et-assert-resolve String
  (identity (:type String)))
 (et-assert-resolve Integer
  (random (:type Integer)))
 (et-assert-resolve Boolean
  (featurep (:type NonNilSymbol))))


;;; Property lists

;; `plist-get'/`plist-put' need to look a literal key up in a `PList'
;; type, so they are checkers (with runtime logic) rather than static
;; `@function' declarations.

(defun et--plist-lookup (plist-type key-type error-fn)
  "Look up KEY-TYPE in PLIST-TYPE, returning the value type or nil.

PLIST-TYPE and KEY-TYPE should already be expanded.  KEY-TYPE must be a
literal or union of literals.  PLIST-TYPE must be a single PList case.
ERROR-FN is called with a format string and args on failure, and the
function returns nil."
  (when-let*
      ((plist-type (et-expand-all-aliases (et--remove-type-binds plist-type)))
       (key-type (et-expand-all-aliases (et--remove-type-binds key-type)))
       (keys
        (cl-loop for case in (et-type-cases key-type)
                 for val = (et-type-case-value case)
                 if (and (et-datatype-p val)
                         (eq (et-datatype-name val) 'Literal))
                 collect (car (et-datatype-args val))
                 else do (funcall error-fn "Key must be a literal, found %s" (et-pp key-type))
                 and return nil))
       (plist-args
        (pcase (et-type-cases plist-type)
          (`(,(cl-struct et-type-case
                         (value (cl-struct et-datatype (name 'PList) (args args)))))
           args)
          (_ (funcall error-fn "Expected a PList type, found %s" (et-pp plist-type))
             nil)))
       (val-types
        (cl-loop for k in keys
                 for v = (plist-get plist-args k)
                 if v collect v
                 else do (funcall error-fn "Key %s not found in %s"
                                  k (et-pp plist-type))
                 and return nil)))
    (apply #'et--or val-types)))


(et-define-pcase-checker plist-get `(,_plist ,_key)
  (let* ((plist-type (et-checker-sub 1))
         (key-type (et-checker-sub 2)))
    (et--plist-lookup plist-type key-type
                      (lambda (fmt &rest args) (apply #'et-err 0 fmt args)))))


(et-define-pcase-checker plist-put `(,_plist ,_key ,_val)
  (let* ((plist-type (et-checker-sub 1))
         (key-type (et-checker-sub 2))
         (val-type (et-checker-sub 3))
         (existing-val-type
          (et--plist-lookup plist-type key-type
                            (lambda (fmt &rest args) (apply #'et-err 0 fmt args)))))
    (when existing-val-type
      (if (et-subtype? val-type existing-val-type)
          plist-type
        (et-err 3 "Expected %s, found %s" (et-pp existing-val-type) (et-pp val-type))))))
