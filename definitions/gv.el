;;; Place machinery

;; Type checking `setf' revolves around assignments to places. Reading
;; a place is never this file's concern -- a place expression appearing
;; as a value is checked by the ordinary function definitions in the
;; other files.
;;
;; Every settable function is given a "place checker" here, so that all
;; of the setf behavior lives in this one file. A place checker runs
;; exactly like an ordinary checker -- with `et--checker-expr' bound to
;; the place expression -- and receives the type being assigned. It
;; validates that assignment and returns the place's write type. Inside
;; a place checker, `et-checker-sub' checks an argument as a plain value,
;; while `et--check-place-sub' checks assigning a type to an argument
;; which is itself a place (a container the setter stores a new value
;; back into, like the plist argument of `plist-get').

(defmacro et--define-place-checker (func &rest body)
  "Define BODY as the place checker of FUNC.
BODY runs with ASSIGN-TYPE bound to the assigned type and should validate
it, then return the write type of the place."
  (declare (indent 1))
  (cl-assert (symbolp func))
  `(setf (get ',func 'et-place-checker) (lambda (assign-type) ,@body)))


;;;; Check places

(defun et--check-place-assignment (assign-type write-type &optional path)
  "Check that ASSIGN-TYPE can be stored in WRITE-TYPE, then return WRITE-TYPE."
  (when (and write-type (not (et-subtype? assign-type write-type)))
    (et-err path "Expected %s, found %s" write-type assign-type))
  write-type)

(defun et--check-place-sub (assign-type &rest path)
  "Check assigning ASSIGN-TYPE to the place at PATH and return its write type.
Returns nil after reporting an error if the expression is not a valid place."
  (let* ((flat (flatten-tree path))
         (expr (et--traverse-tree et--checker-expr flat)))
    (et-at flat
      (pcase expr
        ;; A variable place accepts its original declared/inferred type,
        ;; regardless of its current control-flow narrowing.
        ((and sym (pred symbolp) (pred (not keywordp)) (guard sym) (guard (not (eq sym t))))
         (let* ((var (or (et-get-symbol-var sym) (get sym 'et-variable-var)))
                (write-type (or (when var (et-var-type var))
                                (get sym 'et-variable-type))))
           (if (not write-type)
               (et-err nil "Assignment to free variable")
             (et--check-place-assignment assign-type write-type)
             ;; Record the assignment in the flow: the variable now has
             ;; exactly the assigned type.
             (when var (et-checker-on-set-var var assign-type))
             write-type)))
        (`(,(and head (pred symbolp)) . ,_)
         (if-let* ((checker (get head 'et-place-checker)))
             (let* ((et--checker-expr expr))
               (funcall checker assign-type))
           (et-err nil "Not a settable place: `%s'" head)))
        (_ (et-err nil "Invalid place: %s" expr))))))

(defun et--setf-checker ()
  "Check the place/value pairs in `et--checker-expr' and return the last value type."
  (cl-loop with val-type = (et Nil)
           for (place _val) on (cdr et--checker-expr) by #'cddr
           for place-pos upfrom 1 by 2
           do (setq val-type (et-checker-sub (1+ place-pos)))
           do (et--check-place-sub val-type place-pos)
           finally return
           (if-let* ((var (when (symbolp place) (et-get-symbol-var place))))
               (et-add-typeof val-type var)
             val-type)))


;;;; setq tests

(et-declare
 (@variable et--eval-test-variable Integer)
 (@variable et--eval-test-list List<Integer>))

(et-test
 ;; `setq' returns the assigned value's type, just like `setf'.
 (et-assert-resolve 5 (setq et--eval-test-variable 5))
 (et-assert-resolve-errors (setq et--eval-test-variable "s"))
 (et-assert-resolve-errors (setq et--eval-free-variable 5))
 ;; Assignment invalidates an incompatible branch narrowing.
 (et-assert-no-resolve Nil
   (if et--eval-test-list
       nil
     (setq et--eval-test-list (list 1))
     (when et--eval-test-list (car et--eval-test-list)))))


;;;; setf

;; (setf PLACE VAL ...) checks each VAL, then checks assigning it to
;; PLACE, and returns the last VAL.

(et-define-pcase-checker setf (and args (guard (eq 0 (mod (length args) 2))))
  (et--setf-checker))

(et-test
 ;; A variable place, like `setq'
 (et-assert-resolve 2 (let* ((a (et: Integer 1))) (setf a 2)))
 (et-assert-resolve-errors (let* ((a (et: Integer 1))) (setf a "s")))
 ;; Multiple pairs: the last value is returned
 (et-assert-resolve String
   (let* ((a (et: Integer 1))
          (b (et: String "x")))
     (setf a 2 b "y")))
 ;; A global variable place (see the `@variable' declarations below)
 (et-assert-resolve 5 (setf et--gv-test-variable 5))
 (et-assert-resolve-errors (setf et--gv-test-variable "s"))
 ;; Free variables and non-places
 (et-assert-resolve-errors (setf et--gv-free-variable 5))
 (et-assert-resolve-errors (setf (+ 1 2) 5)))


;;;; Slot facets

;; Helpers for walking a container type down to the slot being written.
;; They work on the raw structure of the type (`ConsFull'/`VectorFull'
;; arguments) rather than on the ordinary function definitions, so a
;; place's write type never depends on how the reading function is
;; typed.
;;
;; A `Nil' case is skipped everywhere: writing through nil is a runtime
;; error rather than a type error, mirroring how `setcar' accepts
;; `Nil|ConsW'. Any other case which lacks the requested slot aborts by
;; throwing (INVALID . TYPE) to `et--place-invalid'.

(defun et--place-invalid (case)
  (throw 'et--place-invalid (cons 'INVALID (et-type case))))

(defun et--place-nil-case? (val)
  (and (eq 'Literal (et-datatype-name val)) (null (car (et-datatype-args val)))))

(defun et--place-read-slot-1 (type slot)
  "Union of the SLOT (`car'/`cdr') read facets of TYPE's cons cases."
  (apply #'et--or
         (cl-loop for case in (et-type-cases (et-expand-all-aliases type))
                  for val = (et-type-case-value case)
                  for args = (et-datatype-args val)
                  unless (et--place-nil-case? val)
                  collect (pcase (list slot (et-datatype-name val))
                            (`(car ConsFull) (nth 0 args))
                            (`(cdr ConsFull) (nth 2 args))
                            (`(car ConsFresh) (nth 0 args))
                            (`(cdr ConsFresh) (nth 1 args))
                            (_ (et--place-invalid case))))))

(defvar et--place-elem-stack nil
  "Recursion stack for `et--place-write-slot-1' walking list spines.")

(defun et--place-write-slot-1 (type slot)
  "Collect the SLOT write facets of TYPE's non-nil cases.
SLOT is `car', `cdr', or one of the element slots `list-elem' (conses
only), `array-elem' (vectors and strings only), or `elem' (either)."
  (et--stop-recursion et--place-elem-stack type nil
    (cl-loop for case in (et-type-cases (et-expand-all-aliases type))
             for val = (et-type-case-value case)
             for args = (et-datatype-args val)
             unless (et--place-nil-case? val)
             append (pcase (list slot (et-datatype-name val))
                      (`(car ConsFull) (list (nth 1 args)))
                      (`(cdr ConsFull) (list (nth 3 args)))
                      ;; An element of a list is the car of any cons
                      ;; along its spine.
                      (`(,(or 'elem 'list-elem) ConsFull)
                       (cons (nth 1 args) (et--place-write-slot-1 (nth 2 args) slot)))
                      (`(,(or 'elem 'array-elem) VectorFull) (list (nth 1 args)))
                      ;; A string element is a character code.
                      (`(,(or 'elem 'array-elem) String) (list (et Integer)))
                      (_ (et--place-invalid case))))))

(defun et--place-write-slot (type slot rel)
  "Write type of the SLOT of TYPE, or nil after reporting an error at REL.
The facets of all cases are intersected, since a value must be storable
whichever case the container turns out to be. A nil-only TYPE yields
`Any', as writing through nil is a runtime error, not a type error."
  (let* ((et--place-elem-stack nil)
         (facets (catch 'et--place-invalid (et--place-write-slot-1 type slot))))
    (pcase facets
      (`(INVALID . ,bad) (et-err rel "Cannot write to the `%s' of %s" slot bad))
      ('nil (et-any))
      (_ (apply #'et--supersect facets)))))


;;; Cons cell places

;; (setf (car X) v) does (setcar X v): the write type comes directly
;; from X's type. The compound accessors read their way down to the
;; final cons first -- the trailing a/d letters of the accessor are
;; reads applied innermost-first, and the first letter is the slot
;; written, so `cadr' reads the cdr and then writes the car.

(defun et--place-cons-slots (accessor)
  "Write type for the place (ACCESSOR EXPR), ACCESSOR being a c[ad]+r symbol."
  (let* ((letters (append (substring (symbol-name accessor) 1 -1) nil))
         (slots (mapcar (lambda (l) (if (eq l ?a) 'car 'cdr)) letters))
         (type (et-checker-sub 1))
         (et--place-elem-stack nil)
         (facets (catch 'et--place-invalid
                   (cl-loop for slot in (reverse (cdr slots))
                            do (setq type (et--place-read-slot-1 type slot))
                            finally return (et--place-write-slot-1 type (car slots))))))
    (pcase facets
      (`(INVALID . ,bad) (et-err 1 "Cannot write to the `%s' of %s" accessor bad))
      ('nil (et-any))
      (_ (apply #'et--supersect facets)))))

(et--define-place-checker car
  (et--check-place-assignment assign-type (et--place-cons-slots 'car)))
(et--define-place-checker cdr
  (et--check-place-assignment assign-type (et--place-cons-slots 'cdr)))
(et--define-place-checker caar
  (et--check-place-assignment assign-type (et--place-cons-slots 'caar)))
(et--define-place-checker cadr
  (et--check-place-assignment assign-type (et--place-cons-slots 'cadr)))
(et--define-place-checker cdar
  (et--check-place-assignment assign-type (et--place-cons-slots 'cdar)))
(et--define-place-checker cddr
  (et--check-place-assignment assign-type (et--place-cons-slots 'cddr)))

(et-test
 (et-assert-resolve 2
   (let* ((x (et: Cons<Integer~String> (cons 1 "a")))) (setf (car x) 2)))
 (et-assert-resolve String
   (let* ((x (et: Cons<Integer~String> (cons 1 "a")))) (setf (cdr x) "b")))
 (et-assert-resolve-errors
  (let* ((x (et: Cons<Integer~String> (cons 1 "a")))) (setf (car x) "b")))
 ;; Read-only conses cannot be written
 (et-assert-resolve-errors
  (let* ((x (et: ListR<Integer> (list 1)))) (setf (car x) 2)))
 ;; A fresh cons has not committed to a write type
 (et-assert-resolve-errors (setf (car (list 1 2)) 5))
 ;; Compound accessors read down to the written cons
 (et-assert-resolve 3
   (let* ((x (et: List<Integer> (list 1 2)))) (setf (cadr x) 3)))
 (et-assert-resolve List<Integer>
   (let* ((x (et: List<Integer> (list 1 2)))) (setf (cddr x) (list 3))))
 (et-assert-resolve-errors
  (let* ((x (et: List<Integer> (list 1 2)))) (setf (cadr x) "s")))
 (et-assert-resolve 5
   (let* ((x (et: Cons<Cons<Integer~Integer>~Nil> (cons (cons 1 2) nil))))
     (setf (caar x) 5))))


;;; Sequence element places

;; (setf (nth N LIST) v) does (setcar (nthcdr N LIST) v), (setf (aref
;; ARRAY I) v) does (aset ARRAY I v), and `elt' dispatches to one or
;; the other. Either way the index is a plain value and the write type
;; is the container's element write facet.

(et--define-place-checker nth
  (et-checker-resolve 'Integer 1)
  (et--check-place-assignment
   assign-type (et--place-write-slot (et-checker-sub 2) 'list-elem 2)))

(et--define-place-checker aref
  (et-checker-resolve 'Integer 2)
  (et--check-place-assignment
   assign-type (et--place-write-slot (et-checker-sub 1) 'array-elem 1)))

(et--define-place-checker elt
  (et-checker-resolve 'Integer 2)
  (et--check-place-assignment
   assign-type (et--place-write-slot (et-checker-sub 1) 'elem 1)))

(et-test
 (et-assert-resolve 5
   (let* ((l (et: List<Integer> (list 1 2)))) (setf (nth 1 l) 5)))
 (et-assert-resolve-errors
  (let* ((l (et: List<Integer> (list 1 2)))) (setf (nth 1 l) "s")))
 (et-assert-resolve-errors
  (let* ((l (et: List<Integer> (list 1 2)))) (setf (nth "1" l) 5)))
 ;; nth is a list place, aref an array place, elt either
 (et-assert-resolve-errors
  (let* ((v (et: Vector<Integer> (vector 1)))) (setf (nth 0 v) 5)))
 (et-assert-resolve 5
   (let* ((v (et: Vector<Integer> (vector 1)))) (setf (aref v 0) 5)))
 (et-assert-resolve Integer
   (let* ((s (et: String "ab"))) (setf (aref s 0) ?c)))
 (et-assert-resolve 5
   (let* ((l (et: List<Integer> (list 1)))) (setf (elt l 0) 5)))
 (et-assert-resolve 5
   (let* ((v (et: Vector<Integer> (vector 1)))) (setf (elt v 0) 5)))
 (et-assert-resolve-errors
  (let* ((l (et: ListR<Integer> (list 1)))) (setf (elt l 0) 5))))


;;; Symbol places

;; (setf (symbol-value SYM) v) does (set SYM v) and (setf (get SYM
;; PROP) v) does (put SYM PROP v): the write is constrained only when
;; the symbol has a declared `@variable' (respectively PROP a declared
;; `@symbol-property') type. (setf (symbol-function SYM) v) does
;; (fset SYM v), which accepts any definition (see `fset' in data.c.el).

(defun et--place-symbol-write (sym-type prop)
  "Intersect the types declared under PROP across SYM-TYPE's literal symbols.
A symbol with no declared type -- or a non-literal symbol type --
contributes `Any', since storing into it is unconstrained."
  (cl-loop for case in (et-type-cases (et-expand-all-aliases (et--remove-type-binds sym-type)))
           for val = (et-type-case-value case)
           collect (or (pcase val
                         ((cl-struct et-datatype (name 'Literal) (args `(,(and sym (pred symbolp)))))
                          (get sym prop)))
                       (et-any))
           into types
           finally return (if types (apply #'et--supersect types) (et-any))))

(et--define-place-checker symbol-value
  (et-checker-resolve 'Symbol 1)
  (et--check-place-assignment
   assign-type (et--place-symbol-write (et-checker-sub 1) 'et-variable-type)))

(et--define-place-checker get
  (et-checker-resolve 'Symbol 1)
  (et--check-place-assignment
   assign-type (et--place-symbol-write (et-checker-sub 2) 'et-symbol-property-type)))

(et--define-place-checker symbol-function
  (et-checker-resolve 'Symbol 1)
  (et--check-place-assignment assign-type (et-any)))

(et-declare
 (@variable et--gv-test-variable Integer)
 (@symbol-property et--gv-test-property String))

(et-test
 (et-assert-resolve 5 (setf (symbol-value 'et--gv-test-variable) 5))
 (et-assert-resolve-errors (setf (symbol-value 'et--gv-test-variable) "s"))
 ;; An unknown symbol is unconstrained
 (et-assert-resolve String (setf (symbol-value 'et--gv-undeclared-variable) "s"))
 (et-assert-resolve-errors (setf (symbol-value 5) 5))
 (et-assert-resolve String (setf (get 'foo 'et--gv-test-property) "s"))
 (et-assert-resolve-errors (setf (get 'foo 'et--gv-test-property) 5))
 (et-assert-resolve 1 (setf (get 'foo 'et--gv-undeclared-property) 1))
 (et-assert-resolve 1 (setf (symbol-function 'foo) 1)))


;;; Property list places

;; (setf (plist-get PLIST KEY) v) stores (plist-put PLIST KEY v) back
;; into PLIST, so PLIST is itself a place. When the plist is non-nil,
;; `plist-put' mutates the entry (KEY must be declared in the place's
;; type) and returns the same plist, so the plist's read type must fit
;; back into its own place. When the plist is nil, it returns the fresh
;; plist (KEY v), which must fit the place on its own.

(et--define-place-checker plist-get
  (let* ((key-type (et--remove-type-binds (et-checker-sub 2)))
         (read-type (et--remove-type-binds (et-checker-sub 1)))
         (non-nil-read (et--non-nil read-type))
         (nil-possible (not (et-never-p (et--supersect read-type (et Nil)))))
         (fresh-plist (et-alias 'Cons key-type (et-alias 'Cons assign-type (et-literal nil))))
         (stored-type (if nil-possible (et--or non-nil-read fresh-plist) non-nil-read)))
    (when-let* ((write-type (et--check-place-sub stored-type 1))
                (val-write (et--plist-lookup
                            write-type key-type
                            (lambda (fmt &rest args) (apply #'et-err 0 fmt args)))))
      (et--check-place-assignment assign-type val-write))))

(et-test
 (et-assert-resolve List<Integer>
   (let* ((pl (et: (PList :nums List<Integer> :name String) '(:nums (1) :name "x"))))
     (setf (plist-get pl :nums) (list 2))))
 (et-assert-resolve-errors
  (let* ((pl (et: (PList :nums List<Integer>) '(:nums (1)))))
    (setf (plist-get pl :nums) 5)))
 (et-assert-resolve-errors
  (let* ((pl (et: (PList :nums List<Integer>) '(:nums (1)))))
    (setf (plist-get pl :missing) 5)))
 ;; The plist must itself be a place
 (et-assert-resolve-errors (setf (plist-get '(:k 1) :k) 2)))


;;; Association list places

;; (setf (alist-get KEY ALIST) v) does (setcdr (assq KEY ALIST) v) when
;; the key is present, and otherwise conses the fresh entry (KEY . v)
;; onto the front and stores the result back -- so ALIST is itself a
;; place. The write type is the cdr write facet of the entries, and the
;; fresh entry must fit back into the alist's place.

(et--define-place-checker alist-get
  (let* ((key-type (et--remove-type-binds (et-checker-sub 1)))
         (al-read (et--remove-type-binds (et-checker-sub 2)))
         (new-alist (et-dt 'ConsFresh (et-dt 'ConsFresh key-type assign-type) al-read)))
    (when-let* ((al-write (et--check-place-sub new-alist 2))
                (entry-type (or (et-checker-infer al-write [C] ListR<C> C)
                                (et-err 2 "Expected an alist place, found %s" al-write)))
                (val-write (et--place-write-slot entry-type 'cdr 2)))
      (et--check-place-assignment assign-type val-write))))

(et-test
 (et-assert-resolve 5
   (let* ((al (et: (AList Symbol Integer) (list (cons 'a 1)))))
     (setf (alist-get 'b al) 5)))
 (et-assert-resolve-errors
  (let* ((al (et: (AList Symbol Integer) (list (cons 'a 1)))))
    (setf (alist-get 'b al) "s")))
 ;; The new entry's key must fit the alist's key type
 (et-assert-resolve-errors
  (let* ((al (et: (AList Symbol Integer) (list (cons 'a 1)))))
    (setf (alist-get "k" al) 5)))
 ;; Read-only entries cannot be written
 (et-assert-resolve-errors
  (let* ((al (et: (AListR Symbol Integer) (list (cons 'a 1)))))
    (setf (alist-get 'b al) 5)))
 ;; Nested places compose
 (et-assert-resolve 2
   (let* ((als (et: (AList Symbol (PList :k Integer)) (list (cons 'x '(:k 1))))))
     (setf (plist-get (alist-get 'x als) :k) 2))))


;;; Derived place macros

;; These macros expand through `setf', so they are checked with the
;; same machinery: read the place normally (through the ordinary
;; function definitions), compute the type of the value being stored,
;; and check it against the place's write type.


;;;; push / cl-pushnew

(defun et--place-push-type (include-old)
  "Check storing (cons VAL PLACE) into PLACE for a push-style macro.
When INCLUDE-OLD, the place may also keep its old value (`cl-pushnew').
Returns the stored type, which is also the macro's return type."
  (let* ((val-type (et--remove-type-binds (et-checker-sub 1)))
         (read-type (et--remove-type-binds (et-checker-sub 2)))
         (new-type (et-dt 'ConsFresh val-type read-type))
         (new-type (if include-old (et--or new-type read-type) new-type)))
    (et--check-place-sub new-type 2)
    new-type))

(et-define-pcase-checker push `(,_val ,_place)
  (et--place-push-type nil))

(et-define-pcase-checker cl-pushnew `(,_val ,_place . ,_keys)
  (et-checker-remaining 3)
  (et--place-push-type t))

(et-test
 (et-assert-resolve Cons<Integer~List<Integer>>
   (let* ((l (et: List<Integer> (list 1)))) (push 2 l)))
 (et-assert-resolve-errors
  (let* ((l (et: List<Integer> (list 1)))) (push "s" l)))
 ;; Pushing onto a nested place
 (et-assert-resolve Cons<Integer~List<Integer>>
   (let* ((pl (et: (PList :nums List<Integer>) '(:nums (2)))))
     (push 1 (plist-get pl :nums))))
 (et-assert-resolve-errors
  (let* ((pl (et: (PList :nums List<Integer>) '(:nums (2)))))
    (push "s" (plist-get pl :nums))))
 ;; cl-pushnew may also keep the old value
 (et-assert-resolve List<Integer>
   (let* ((l (et: List<Integer> (list 1)))) (cl-pushnew 2 l)))
 (et-assert-resolve List<Integer>
   (let* ((l (et: List<Integer> (list 1)))) (cl-pushnew 2 l :test #'eq)))
 (et-assert-resolve-errors
  (let* ((l (et: List<Integer> (list 1)))) (cl-pushnew "s" l))))


;;;; pop

;; (pop PLACE) returns (car PLACE) and stores (cdr PLACE) back. The
;; car/cdr types are read through the same `MatchCar'/`MatchCdr'
;; matchers that type `car' and `cdr' themselves (see data.c.el).

(et-define-pcase-checker pop `(,_place)
  (let* ((read-type (et--remove-type-binds (et-checker-sub 1)))
         (car-type (et-checker-infer read-type [T] (MatchCar T) T))
         (cdr-type (et-checker-infer read-type [T] (MatchCdr T) T)))
    (if (not (and car-type cdr-type))
        (et-err 1 "Cannot pop from %s" read-type)
      (et--check-place-sub cdr-type 1))
    car-type))

(et-test
 (et-assert-resolve Integer|Nil
   (let* ((l (et: List<Integer> (list 1)))) (pop l)))
 (et-assert-resolve-errors
  (let* ((n (et: Integer 1))) (pop n)))
 ;; Popping a variable never mutates a cons, so read-only lists are fine
 (et-assert-resolve Integer|Nil
   (let* ((l (et: ListR<Integer> (list 1)))) (pop l)))
 ;; Popping through a place
 (et-assert-resolve Integer|Nil
   (let* ((x (et: Cons<String~List<Integer>> (cons "s" (list 1)))))
     (pop (cdr x))))
 ;; The tail must be storable back into the place
 (et-assert-resolve-errors
  (let* ((x (et: ListR<List<Integer>> (list (list 1))))) (pop (car x)))))


;;;; cl-incf / cl-decf

;; (cl-incf PLACE &optional X) stores (+ PLACE X) and returns it. As
;; with `+' (see data.c.el), the result is an Integer when everything
;; is an integer or marker, and a Number otherwise.

(defun et--place-arith-type (name)
  (let* ((x? (cddr et--checker-expr))
         (read-type (et--remove-type-binds (et-checker-sub 1)))
         (x-type (if x? (et--remove-type-binds (et-checker-sub 2)) (et-literal 1)))
         (int (et IntOrMarker))
         (new-type (if (and (et-subtype? read-type int) (et-subtype? x-type int))
                       (et Integer) (et Number))))
    (unless (et-subtype? read-type (et NumOrMarker))
      (et-err 1 "Cannot %s a place of type %s" name read-type))
    (when (and x? (not (et-subtype? x-type (et NumOrMarker))))
      (et-err 2 "Expected NumOrMarker, found %s" x-type))
    (et--check-place-sub new-type 1)
    new-type))

(et-define-pcase-checker cl-incf (or `(,_place) `(,_place ,_x))
  (et--place-arith-type 'cl-incf))

(et-define-pcase-checker cl-decf (or `(,_place) `(,_place ,_x))
  (et--place-arith-type 'cl-decf))

(et-test
 (et-assert-resolve Integer (let* ((n (et: Integer 1))) (cl-incf n)))
 (et-assert-resolve Integer (let* ((n (et: Integer 1))) (cl-incf n 5)))
 (et-assert-resolve Number (let* ((n (et: Number 1))) (cl-incf n)))
 (et-assert-resolve Integer (let* ((n (et: Integer 1))) (cl-decf n)))
 ;; A float increment makes the result a Number, which no longer fits
 (et-assert-resolve-errors (let* ((n (et: Integer 1))) (cl-incf n 1.5)))
 (et-assert-resolve-errors (let* ((s (et: String "a"))) (cl-incf s)))
 ;; Incrementing a nested place
 (et-assert-resolve Integer
   (let* ((pl (et: (PList :n Integer) '(:n 1))))
     (cl-incf (plist-get pl :n)))))
