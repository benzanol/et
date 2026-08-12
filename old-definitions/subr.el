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
 (et-assert-resolve-errors
  (alist-get (:type Integer) (:type ConsR<ConsR<1~2>~ConsR<3~Nil>>)))
 (et-assert-resolve 2|Nil
   (alist-get (:type Integer) (:type AList<1~2>)))
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
            (@generics [T]) (list ListR<T>) (n Integer|Nil) (@return ListR<T>))
 (@function butlast (list &optional n)
            (@generics [T]) (list ListR<T>) (n Integer|Nil) (@return ListFresh<T>))
 (@function nbutlast (list &optional n)
            (@generics [T]) (list ListR<T>) (n Integer|Nil) (@return ListR<T>))
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
            (from Number) (to Number|Nil) (inc Number|Nil) (@return ListFresh<Number>))

 ;; `copy-tree' deeply freshens its argument's structure.
 (@function copy-tree (tree &optional vecp)
            (@generics [T]) (tree T) (vecp Any)
            (@return (eval et--freshen-type T))))

(et-test
 (et-assert-resolve List<Integer>
   (delete-dups (:type ListR<Integer>)))
 (et-assert-resolve ListR<Integer>
   (last (:type ListR<Integer>)))
 (et-assert-resolve ListFresh<Integer>
   (butlast (:type ListR<Integer>)))
 (et-assert-resolve ListFresh<Integer>
   (flatten-tree (:type TreeR<Integer>)))
 (et-assert-resolve ListFresh<Integer>
   (flatten-list (:type TreeR<Integer>)))
 (et-assert-resolve ListFresh<Integer|String>
   (flatten-tree (:type ConsFresh<Integer~ConsFresh<ConsFresh<String~Nil>~Nil>>)))
 (et-assert-resolve ListFresh<Number>
   (number-sequence (:type Integer) (:type Integer)))
 (et-assert-resolve ListFresh<Integer>
   (remove (:type Any) (:type ListR<Integer>)))
 (et-assert-resolve ListR<Integer>
   (remq (:type Any) (:type ListR<Integer>))))

(et-define-pcase-checker add-to-list `(,list-var ,_elt . ,_rest)
  (let* ((var (pcase list-var
                (`(quote ,(and sym (pred symbolp))) sym)
                (_ (et-err 1 "Expected quoted list variable")))))
    (et-checker-remaining 3)
    (if (not var)
        (et-never)
      (let* ((list-type (et-get-symbol-type var))
             (elem-type (and list-type
                             (et-checker-infer list-type [T] List<T> T)))
             (elt-type (et-checker-sub 2)))
        (cond
         ((not list-type)
          (et-err 1 "No type for list variable `%s'" var))
         ((not elem-type)
          (et-err 1 "Expected quoted list variable, found %s of type %s" var list-type))
         ((not (et-subtype? elt-type elem-type))
          (et-err 2 "Expected %s, found %s" elem-type elt-type))
         (t
          (when-let* ((var-obj (et-get-symbol-var var)))
            (et-checker-on-set-var var-obj list-type))))
        list-type))))

(et-test
 (et-assert-resolve List<Integer>
   (let* ((l (et: List<Integer> (list 1))))
     (add-to-list 'l 2)))
 (et-assert-resolve-errors
  (let* ((l (et: List<Integer> (list 1))))
    (add-to-list l 2)))
 (et-assert-resolve-errors
  (let* ((l (et: List<Integer> (list 1))))
    (add-to-list 'l "s")))
 (et-assert-resolve-errors
  (let* ((n (et: Integer 1)))
    (add-to-list 'n 2))))

(et-test
 ;; copy-tree freshens deeply (compared by equivalence, not raw `equal').
 (let ((got (et-root-check-type '(copy-tree (:type (TupleR Cons<1~2> Cons<3~4>)))))
       (want (et ConsFresh<ConsFresh<1~2>~ConsFresh<ConsFresh<3~4>~Nil>>)))
   (and (et-subtype? got want) (et-subtype? want got))))


;;; Symbols and errors

(et-declare
 (@function gensym (&optional prefix)
            (prefix String|Nil) (@return Var))
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
            (string String) (trim-left String|Nil) (trim-right String|Nil) (@return String))
 (@function string-trim-left (string &optional regexp)
            (string String) (regexp String|Nil) (@return String))
 (@function string-trim-right (string &optional regexp)
            (string String) (regexp String|Nil) (@return String))
 (@function string-greaterp (string1 string2)
            (string1 String) (string2 String) (@return Boolean))
 (@function string-replace (from-string to-string in-string)
            (from-string String) (to-string String) (in-string String) (@return String))
 (@function kbd (keys)
            (keys String) (@return KeySequence))
 (@function split-string (string &optional separators omit-nulls trim)
            (string String) (separators String|Nil) (omit-nulls Any) (trim String|Nil)
            (@return ListFresh<String>)))

(et-test
 (et-assert-resolve Boolean (string-prefix-p "a" "abc"))
 (et-assert-resolve Boolean (string-suffix-p "c" "abc"))
 (et-assert-resolve String (string-trim "  hi  "))
 (et-assert-resolve String (string-replace "a" "b" "abc"))
 (et-assert-resolve KeySequence (kbd "C-c C-c"))
 (et-assert-resolve ListFresh<String> (split-string "a b c"))
 (et-assert-resolve String (string-trim-left "  hi"))
 (et-assert-resolve String (string-trim-right "hi  "))
 (et-assert-resolve Boolean (string-greaterp "b" "a"))
 (et-assert-resolve-errors (string-trim 5))
 (et-assert-resolve-errors (kbd 5)))


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


;;; Hooks

;; These deliberately do not inspect the hook variable's type: the hook
;; is just a symbol, and the function is any function.
(et-declare
 (@function add-hook (hook function &optional depth local)
            (hook Symbol) (function Function<Never~Any>) (depth Integer|Boolean) (local Any)
            (@return Any))
 (@function remove-hook (hook function &optional local)
            (hook Symbol) (function Function<Never~Any>) (local Any)
            (@return Any)))

(et-test
 (et-assert-resolve Any (add-hook 'my-hook #'ignore))
 (et-assert-resolve Any (remove-hook 'my-hook #'ignore))
 (et-assert-resolve-errors (add-hook "my-hook" #'ignore))
 (et-assert-resolve-errors (add-hook 'my-hook 5)))


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
;; declarations. `when'/`unless' are single-sided conditionals, so they
;; use `et-checker-sub-cond' with the skipped branch typed as nil or
;; never depending on branch feasibility.

(et-define-pcase-checker dolist
    `(,(or (and form `(,name ,elem-spec ,_lst)
                (let elem-type (et-parse-type elem-spec))
                (let _1 (setcdr form (cddr form)))
                (let _2 (et-checker-resolve (et-alias 'ListR elem-type) 1 1)))
           (and `(,name ,_lst)
                (let elem-type (et-checker-infer (et-checker-sub '(1 1)) [T] ListR<T> T))))
      . ,_body)
  (et-with-vars (list (et-new-var name elem-type))
    (et-checker-loop-body (lambda () (et-checker-sub 2)))))

(et-define-pcase-checker when `(,_cond . ,_then)
  (let* ((cond-type (et-checker-sub 1)))
    (et-checker-hint-narrows 0 "WHEN:\\n%s" (et--non-nil cond-type))
    (et-checker-sub-cond cond-type
                         (lambda ()
                           (if (et-never-p (et--non-nil cond-type))
                               (et Never)
                             (et-checker-tail 2)))
                         (lambda () (et--supersect cond-type (et Nil))))))

(et-define-pcase-checker unless `(,_cond . ,_else)
  (let* ((cond-type (et-checker-sub 1)))
    (et-checker-hint-narrows 0 "UNLESS:\\n%s" (et--supersect cond-type (et Nil)))
    (et-checker-sub-cond cond-type
                         (lambda ()
                           (if (et-never-p (et--non-nil cond-type))
                               (et Never)
                             (et Nil)))
                         (lambda ()
                           (if (et-never-p (et--supersect cond-type (et Nil)))
                               (et Never)
                             (et-checker-tail 2))))))

(et-test
 (et-subtype? (et--remove-type-binds
               (et-root-check-type '(let* ((a (et: String|Number 4)))
                                      (when (stringp a) a))))
              (et String|Nil))
 (let ((got (et--remove-type-binds
             (et-root-check-type '(let* ((a (et: String|Nil "s")))
                                    (unless a 1)))))
       (want (et Integer|Nil)))
   (et-subtype? got want))
 (let ((got (et--remove-type-binds
             (et-root-check-type '(let* ((a (et: String|Number|Nil 4)))
                                    (unless (stringp a) a)))))
       (want (et Number|Nil)))
   (and (et-subtype? got want) (et-subtype? want got))))


;;;; if-let*

(defun et--check-let-condition-bindings (bindings bindings-path)
  "Check conditional let BINDINGS and return (VARS . NARROWS).

BINDINGS-PATH is the path to BINDINGS in `et--checker-expr'.

Each binding's value is checked knowing all previous bindings were
non-nil: their non-nil narrows are merged into `et--checker-narrows' as
the bindings are walked. Since the bindings only all run on the success
path, the caller must scope those narrows to that path (via
`et-checker-branches' or `et-checker-loop-body')."
  (cl-loop
   with vars = nil
   with narrows = nil
   for binding in bindings
   for idx upfrom 0
   for (name . rel) =
   (pcase binding
     (`(,(and name (pred symbolp)) ,_value)
      (cons name (append bindings-path (list idx 1))))
     (`(,_value) (cons nil (append bindings-path (list idx 0))))
     ((pred symbolp) (cons nil (append bindings-path (list idx))))
     (_ (et-fatal (append bindings-path (list idx))
                  "Expected (NAME VALUE), (VALUE), or SYMBOL")))

   for value-type = (et-with-vars vars (et-checker-sub rel))
   for non-nil = (et--non-nil value-type)
   for binds = (et--type-binds non-nil)
   do (setq narrows (append binds narrows))
   do (cl-callf et--narrows-and et--checker-narrows binds)
   when name do (push (et-new-var name (et--unfreshen-type non-nil)) vars)
   finally return (cons vars narrows)))

;; `if-let*' is like `let*', except that every binding is a condition: the
;; THEN branch runs only when all of them are non-nil. So each binding is
;; checked knowing that all the previous ones were non-nil, and is bound to
;; the non-nil part of its value's type. The same knowledge narrows the THEN
;; branch, which is what `let*' checking of the expansion cannot express.
;;
;; `while-let' uses the same binding rules. It then behaves like `while':
;; all bindings are re-evaluated before every iteration, the body sees their
;; successful non-nil narrows, and the whole form returns nil.
;;
;; A binding is (NAME VALUE), (VALUE) - checked but not bound - or a bare
;; symbol, whose own value must be non-nil.
;;
;; The `if-let*' ELSE branch only knows that *some* binding was nil, which
;; narrows nothing, and none of the bindings are in scope there.

(et-define-pcase-checker if-let*
    `(,(and bindings (pred listp)) ,_then . ,_else)
  ;; The bindings and THEN only run on the all-non-nil path, so they form
  ;; one branch; ELSE (where some binding was nil, narrowing nothing) is
  ;; the other.
  (et-simplify-type
   (et-checker-branches
    (lambda ()
      (pcase-let ((`(,vars . ,_narrows)
                   (et--check-let-condition-bindings bindings '(1))))
        (et-with-vars vars (et-checker-sub 2))))
    (lambda () (et-checker-tail 3)))))

(et-define-pcase-checker while-let
    `(,(and bindings (pred listp)) . ,_body)
  ;; The bindings re-run before every iteration, so they are checked
  ;; inside the loop body: `et-checker-loop-body' drops the narrows the
  ;; loop can invalidate before the real pass, exactly like `while'.
  (et-checker-loop-body
   (lambda ()
     (pcase-let ((`(,vars . ,narrows)
                  (et--check-let-condition-bindings bindings '(1))))
       (et-checker-hint-narrows 0 "WHILE-LET:\\n%s"
                                (if narrows
                                    (apply #'et--supersect (mapcar #'cdr narrows))
                                  (et True)))
       (et-with-vars vars
         (et-checker-remaining 2)))))

  (et Nil))

(et-test
 ;; Each binding is narrowed to its non-nil part inside THEN
 (et-assert-resolve Integer|Nil
   (if-let* ((a (car (list 1 2)))) a))
 (et-subtype? (et-root-check-type '(let* ((a (et: String|Nil "s")))
                                     (if-let* ((b a)) b "fallback")))
              (et String))
 ;; A later binding sees the earlier ones, already non-nil
 (et-subtype? (et-root-check-type '(let* ((a (et: String|Nil "s")))
                                     (if-let* ((b a)
                                               (c (concat b "!")))
                                         c
                                       "fallback")))
              (et String))
 ;; Bindings are not in scope in the ELSE branch
 (et-assert-resolve-errors
  (if-let* ((a 1)) a a))
 ;; The condition narrows outer variables in THEN
 (et-subtype? (et-root-check-type '(let* ((a (et: String|Number 4)))
                                     (if-let* ((b (stringp a))) a "fallback")))
              (et String)))

(et-test
 (et-assert-resolve Nil
   (while-let ((a (car (list 1 2))))
     (:assert-subtype a Integer)))
 ;; A later binding sees the earlier ones, already non-nil.
 (et-assert-resolve Nil
   (let* ((a (et: String|Nil "s")))
     (while-let ((b a)
                 (c (concat b "!")))
       (:assert-subtype c String))))
 ;; The condition narrows outer variables in the loop body.
 (et-assert-resolve Nil
   (let* ((a (et: String|Number 4)))
     (while-let ((b (stringp a)))
       (:assert-subtype a String))))
 ;; Like `while', narrows from above the loop do not survive assignment in
 ;; the body on the next iteration.
 (et-assert-resolve-errors
  (let* ((a (et: String|Number 4)))
    (when (stringp a)
      (while-let ((b t))
        (:assert-subtype a String)
        (setq a 5))))))


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

;; Both bodies can be aborted at any point (by a quit or an error), so no
;; narrow they establish is guaranteed afterwards.

(et-define-pcase-checker with-local-quit _body
  (et--or (et-checker-escapable (lambda () (et-checker-tail 1))) (et Nil)))

(et-define-pcase-checker with-demoted-errors `(,_format . ,_body)
  (et-checker-sub 1)
  (et--or (et-checker-escapable (lambda () (et-checker-tail 2))) (et Nil)))

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

(et-declare
 (@function princ (object &optional printcharfun)
            (@generics [T]) (object T) (printcharfun Any) (@return T)))

;; `with-memoization' returns PLACE when it is already non-nil, and
;; otherwise the value of CODE (which it stores in PLACE), so CODE only
;; runs on the nil branch.
(et-define-pcase-checker with-memoization `(,_place . ,_code)
  (let* ((place-type (et-checker-sub 1)))
    (et-checker-branches
     (lambda () (et--non-nil place-type))
     (lambda () (et-checker-tail 2)))))

(et-test
 (et-assert-resolve String (with-output-to-string (princ "x")))
 (et-assert-resolve String|Integer
   (with-memoization (car (list "cached")) 1)))
