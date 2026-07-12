;;; c[ad]+r accessors

;; Each composes `MatchCar'/`MatchCdr' to peel the right car/cdr path.

(et-declare
 (@function caar (x) (@generics [T]) (x (MatchCar (MatchCar T))) (@return T))
 (@function cadr (x) (@generics [T]) (x (MatchCdr (MatchCar T))) (@return T))
 (@function cdar (x) (@generics [T]) (x (MatchCar (MatchCdr T))) (@return T))
 (@function cddr (x) (@generics [T]) (x (MatchCdr (MatchCdr T))) (@return T))

 (@function caaar (x) (@generics [T]) (x (MatchCar (MatchCar (MatchCar T)))) (@return T))
 (@function caadr (x) (@generics [T]) (x (MatchCdr (MatchCar (MatchCar T)))) (@return T))
 (@function cadar (x) (@generics [T]) (x (MatchCar (MatchCdr (MatchCar T)))) (@return T))
 (@function caddr (x) (@generics [T]) (x (MatchCdr (MatchCdr (MatchCar T)))) (@return T))
 (@function cdaar (x) (@generics [T]) (x (MatchCar (MatchCar (MatchCdr T)))) (@return T))
 (@function cdadr (x) (@generics [T]) (x (MatchCdr (MatchCar (MatchCdr T)))) (@return T))
 (@function cddar (x) (@generics [T]) (x (MatchCar (MatchCdr (MatchCdr T)))) (@return T))
 (@function cdddr (x) (@generics [T]) (x (MatchCdr (MatchCdr (MatchCdr T)))) (@return T)))

(et-test
 (et-assert-resolve 2 (cadr '(1 2 3)))
 (et-assert-resolve 3 (caddr '(1 2 3)))
 (et-assert-resolve 0 (cadar '((-1 0) 1 2 3)))
 (et-assert-resolve List<3> (cdddr '((-1 0) 1 2 3))))


;;; Association lists

(et-declare
 (@function alist-get (key alist)
            (@generics [V])
            (key Any) (alist ListR<ConsR<Any~V>>)
            (@return V|Nil)))

(et-test
 (et-assert-call-errors alist-get Integer ConsR<ConsR<1~2>~ConsR<3~Nil>>)
 (et-assert-call 2|Nil alist-get Integer AList<1~2>)
 (et-assert-resolve 2|4|Nil (alist-get 4 (list (cons 1 2) (cons 3 4)))))


;;; List utilities
;;;; Leaf type

(defun et--leaf-type-1 (type stack)
  (et--stop-recursion stack type (et-never)
    (apply #'et--or
           (cl-loop
            for case in (et-type-cases type)
            for val = (et-type-case-value case)
            for cons-args =
            (pcase (when (et-datatype-p val) (et-datatype-name val))
              ('ConsFull (list (nth 0 (et-datatype-args val)) (nth 2 (et-datatype-args val))))
              ('ConsFresh (et-datatype-args val)))
            append
            (cond
             ((et-alias-p val) (list (et--leaf-type-1 (et-alias-expand val) stack)))
             (cons-args (mapcar (lambda (arg) (et--leaf-type-1 arg stack)) cons-args))
             ((equal val (make-et-datatype :name 'Literal :args (list nil))) nil)
             (t (list (et-type case))))))))

(et-define-op leaves (type)
  (et--leaf-type-1 (et--remove-type-binds type) nil))


;;;; Functions

(et-declare
 (@function delete-dups (list)
            (@generics [T]) (list ListR<T>) (@return List<T>))
 (@function last (list &optional n)
            (@generics [T]) (list ListR<T>) (n Integer) (@return ListR<T>))
 (@function butlast (list &optional n)
            (@generics [T]) (list ListR<T>) (n Integer) (@return ListFresh<T>))
 (@function nbutlast (list &optional n)
            (@generics [T]) (list ListR<T>) (n Integer) (@return ListR<T>))
 ;; `remove' copies its argument; `remq' may share the input tail.
 (@function remove (elt seq)
            (@generics [T]) (elt Any) (seq ListR<T>) (@return ListFresh<T>))
 (@function remq (elt list)
            (@generics [T]) (elt Any) (list ListR<T>) (@return ListR<T>))
 (@function flatten-tree (tree)
            (@generics [T]) (tree T) (@return (ListFresh (leaves T))))
 (@function flatten-list (tree)
            (@generics [T]) (tree T) (@return (ListFresh (leaves T))))
 (@function number-sequence (from &optional to inc)
            (from Number) (to Number) (inc Number) (@return ListFresh<Number>))

 ;; `copy-tree' deeply freshens its argument's structure.
 (@function copy-tree (tree &optional vecp)
            (@generics [T]) (tree T) (vecp Any)
            (@return (eval et--freshen-type T))))

(et-test
 (et-assert-call List<Integer> delete-dups ListR<Integer>)
 (et-assert-call ListR<Integer> last ListR<Integer>)
 (et-assert-call ListFresh<Integer> butlast ListR<Integer>)
 (et-assert-call ListFresh<Integer> flatten-tree TreeR<Integer>)
 (et-assert-call ListFresh<Integer> flatten-list TreeR<Integer>)
 (et-assert-call ListFresh<Integer|String> flatten-tree ConsFresh<Integer~ConsFresh<ConsFresh<String~Nil>~Nil>>)
 (et-assert-call ListFresh<Number> number-sequence Integer Integer)
 (et-assert-call ListFresh<Integer> remove Any ListR<Integer>)
 (et-assert-call ListR<Integer> remq Any ListR<Integer>))

(et-test
 ;; copy-tree freshens deeply (compared by equivalence, not raw `equal').
 (let ((got (et-result-value (et-typecheck-call copy-tree (TupleR Cons<1~2> Cons<3~4>))))
       (want (et ConsFresh<ConsFresh<1~2>~ConsFresh<ConsFresh<3~4>~Nil>>)))
   (and (et-subtype? got want) (et-subtype? want got))))


;;; Symbols and errors

(et-declare
 (@function gensym (&optional prefix)
            (prefix String) (@return Var))
 (@function error (string &rest args)
            (string String) (args ListR<Any>) (@return Never)))

(et-test
 (et-assert-resolve Var (gensym))
 (et-assert-resolve Var (gensym "pre"))
 (et-assert-resolve Never (error "boom %d" 1))
 (et-assert-resolve-errors (gensym 5)))


;;; Strings

(et-declare
 (@function string-prefix-p (prefix string &optional ignore-case)
            (prefix String) (string String) (ignore-case Any) (@return Boolean))
 (@function string-suffix-p (suffix string &optional ignore-case)
            (suffix String) (string String) (ignore-case Any) (@return Boolean))
 (@function string-trim (string &optional trim-left trim-right)
            (string String) (trim-left String) (trim-right String) (@return String))
 (@function string-trim-left (string &optional regexp)
            (string String) (regexp String) (@return String))
 (@function string-trim-right (string &optional regexp)
            (string String) (regexp String) (@return String))
 (@function string-greaterp (string1 string2)
            (string1 String) (string2 String) (@return Boolean))
 (@function string-replace (from-string to-string in-string)
            (from-string String) (to-string String) (in-string String) (@return String))
 (@function split-string (string &optional separators omit-nulls trim)
            (string String) (separators String) (omit-nulls Any) (trim String)
            (@return ListFresh<String>)))

(et-test
 (et-assert-resolve Boolean (string-prefix-p "a" "abc"))
 (et-assert-resolve Boolean (string-suffix-p "c" "abc"))
 (et-assert-resolve String (string-trim "  hi  "))
 (et-assert-resolve String (string-replace "a" "b" "abc"))
 (et-assert-resolve ListFresh<String> (split-string "a b c"))
 (et-assert-resolve String (string-trim-left "  hi"))
 (et-assert-resolve String (string-trim-right "hi  "))
 (et-assert-resolve Boolean (string-greaterp "b" "a"))
 (et-assert-resolve-errors (string-trim 5)))


;;; Misc utilities

;; `xor' returns the lone non-nil argument (or nil when both/neither are).
;; `always' always returns t; `ignore' always returns nil.
(et-declare
 (@function xor (cond1 cond2)
            (@generics [A B]) (cond1 A) (cond2 B) (@return (or A B Nil)))
 (@function always (&rest arguments)
            (arguments ListR<Any>) (@return True))
 (@function ignore (&rest arguments)
            (arguments ListR<Any>) (@return Nil)))

(et-test
 (et-assert-resolve True (always 1 2 3))
 (et-assert-resolve Nil (ignore 1 2 3))
 (et-assert-resolve String|Integer|Nil (xor "a" 5)))


;;; Macros

;; `if-let' and the `when-let' family all macroexpand into `if-let*', so
;; expanding them is enough. `if-let*' itself is checked directly (see
;; below): its expansion is a `let*' whose bindings are each guarded by
;; the previous one, and the type system cannot recover from that
;; expansion the fact that the last binding being non-nil implies that
;; all of the earlier ones are too.
(et-declare
 (@macro if-let :expand t)
 (@macro when-let :expand t)
 (@macro when-let* :expand t))


;;; Control-flow macros

;; `dolist'/`when'/`unless' are macros defined in lisp/subr.el. They
;; carry runtime logic (binding a loop variable, narrowing the
;; condition), so they are written as checkers rather than `@function'
;; declarations. `when'/`unless' reuse the narrowing helpers
;; `et--and-return-type'/`et--or-return-type' that live with `and'/`or'
;; in eval.c.el.

(et-define-pcase-checker dolist
    `(,(or (and form `(,name ,elem-spec ,_lst)
                (let elem-type (et-parse-type elem-spec))
                (let _1 (setcdr form (cddr form)))
                (let _2 (et-checker-resolve (et-alias 'ListR elem-type) 1 1)))
           (and `(,name ,_lst)
                (let elem-type (et-checker-infer (et-checker-sub 1 1) [T] ListR<T> T))))
      . ,_body)
  (et-with-vars (list (et-new-var name elem-type))
    (et-checker-sub 2)))

(et-define-pcase-checker when `(,_cond . ,then)
  (let* ((cond-type (et-checker-sub 1)))
    (et-checker-hint-narrows 0 "WHEN:\\n%s" (et--non-nil cond-type))
    ;; Special case for empty then block because (when cond) always returns nil
    (if (null then) (et Nil)
      (et--and-return-type cond-type (lambda () (et-checker-tail 2))))))

(et-define-pcase-checker unless `(,_cond . ,_else)
  (let* ((cond-type (et-checker-sub 1)))
    (et-checker-hint-narrows 0 "UNLESS:\\n%s" (et--supersect cond-type (et Nil)))
    ;; Special case for empty then block because (when cond) always returns nil
    (et--or-return-type cond-type (lambda () (et-checker-tail 2)))))

(et-test
 (equal (et String|Nil)
        (et--remove-type-binds
         (et-typecheck
          (let* ((a String|Number 4))
            (when (stringp a) a))))))


;;;; if-let*

;; Like `let*', except that every binding is a condition: the THEN branch
;; runs only when all of them are non-nil. So each binding is checked
;; knowing that all the previous ones were non-nil, and is bound to the
;; non-nil part of its value's type. The same knowledge narrows the THEN
;; branch, which is what `let*' checking of the expansion cannot express.
;;
;; A binding is (NAME VALUE), (VALUE) — checked but not bound — or a bare
;; symbol, whose own value must be non-nil.
;;
;; The ELSE branch only knows that *some* binding was nil, which narrows
;; nothing, and none of the bindings are in scope there.

(et-define-pcase-checker if-let*
    `(,(and bindings (pred listp)) ,_then . ,_else)
  (cl-loop
   with vars = nil
   with narrows = nil
   for binding in bindings
   for idx upfrom 0
   for (name . rel) =
   (pcase binding
     (`(,(and name (pred symbolp)) ,_value) (cons name (list 1 idx 1)))
     (`(,_value) (cons nil (list 1 idx 0)))
     ((pred symbolp) (cons nil (list 1 idx)))
     (_ (et-fatal (list 1 idx) "Expected (NAME VALUE), (VALUE), or SYMBOL")))

   for value-type = (et-with-vars vars
                      (et-with-narrow-binds narrows (et-checker-sub rel)))
   for non-nil = (et--non-nil value-type)
   do (setq narrows (append (et--type-binds non-nil) narrows))
   when name do (push (et-new-var name (et--unfreshen-type non-nil)) vars)

   finally return
   (let* ((then-type (et-with-vars vars
                       (et-with-narrow-binds narrows (et-checker-sub 2))))
          (else-type (et-checker-tail 3)))
     (et-simplify-type (et--or then-type else-type)))))

(et-test
 ;; Each binding is narrowed to its non-nil part inside THEN
 (et-assert-resolve Integer|Nil
   (if-let* ((a (car (list 1 2)))) a))
 (et-subtype? (et-typecheck
               (let* ((a String|Nil "s"))
                 (if-let* ((b a)) b "fallback")))
              (et String))
 ;; A later binding sees the earlier ones, already non-nil
 (et-subtype? (et-typecheck
               (let* ((a String|Nil "s"))
                 (if-let* ((b a)
                           (c (concat b "!")))
                   c
                   "fallback")))
              (et String))
 ;; Bindings are not in scope in the ELSE branch
 (et-assert-resolve-errors
  (if-let* ((a 1)) a a))
 ;; The condition narrows outer variables in THEN
 (et-subtype? (et-typecheck
               (let* ((a String|Number 4))
                 (if-let* ((b (stringp a))) a "fallback")))
              (et String)))


;;; Body macros
;;;; Progn-like

;; These all evaluate their body like a `progn' -- rebinding some piece
;; of state around it -- so the value of the last body form is the value
;; of the whole form. Any leading arguments (a buffer, a window, a
;; syntax table, ...) are ordinary evaluated expressions, so the `:progn'
;; checker types them correctly too: it checks every subform and returns
;; the type of the last one.

(et-declare
 (@macro with-current-buffer :progn t)
 (@macro with-selected-window :progn t)
 (@macro with-selected-frame :progn t)
 (@macro save-window-excursion :progn t)
 (@macro with-temp-buffer :progn t)
 (@macro with-temp-file :progn t)
 (@macro with-temp-message :progn t)
 (@macro with-output-to-temp-buffer :progn t)
 (@macro with-silent-modifications :progn t)
 (@macro with-undo-amalgamate :progn t)
 (@macro with-restriction :progn t)
 (@macro without-restriction :progn t)
 (@macro combine-after-change-calls :progn t)
 (@macro combine-change-calls :progn t)
 (@macro with-syntax-table :progn t)
 (@macro with-case-table :progn t)
 (@macro with-file-modes :progn t)
 (@macro with-existing-directory :progn t)
 (@macro save-match-data :progn t)
 (@macro with-mutex :progn t))

(et-test
 (et-assert-resolve String (with-temp-buffer (insert "x") "done"))
 (et-assert-resolve Integer (with-current-buffer nil 1))
 (et-assert-resolve String (with-syntax-table nil "done"))
 (et-assert-resolve Nil (save-match-data))
 (et-assert-resolve-errors (with-temp-buffer (+ 1 "x"))))


;;;; Escaping to nil

;; `with-local-quit' returns nil when a quit terminates the body, and
;; `with-demoted-errors' returns nil when the body signals an error, so
;; nil is always a possible value on top of the body's own type.

(et-define-pcase-checker with-local-quit _body
  (et--or (et-checker-tail 1) (et Nil)))

(et-define-pcase-checker with-demoted-errors `(,_format . ,_body)
  (et-checker-sub 1)
  (et--or (et-checker-tail 2) (et Nil)))

(et-test
 (et-assert-resolve String|Nil (with-local-quit "done"))
 (et-assert-resolve String|Nil (with-demoted-errors "Error: %S" "done"))
 (et-assert-resolve-errors (with-demoted-errors "Error: %S" (+ 1 "x"))))


;;;; Other

;; `with-output-to-string' discards the body's value: it returns whatever
;; the body printed to `standard-output'.
(et-define-pcase-checker with-output-to-string _body
  (et-checker-remaining 1)
  (et String))

;; `with-memoization' returns PLACE when it is already non-nil, and
;; otherwise the value of CODE (which it stores in PLACE).
(et-define-pcase-checker with-memoization `(,_place . ,_code)
  (et--or (et--non-nil (et-checker-sub 1)) (et-checker-tail 2)))

(et-test
 (et-assert-resolve String (with-output-to-string (princ "x")))
 (et-assert-resolve String|Integer
   (with-memoization (car (list "cached")) 1)))
