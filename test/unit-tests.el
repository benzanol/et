;; -*- lexical-binding: t -*-

(require 'ert)
(require 'et)


;;; Parsing and printing

(ert-deftest et:test-parsing ()
  ;; Unions/intersections
  (should (equal (et Integer?) (et (or Nil Integer))))
  (should (equal (et Integer|String) (et (or Integer String))))
  (should (equal (et {Integer|String}) (et (or Integer String))))
  (should (equal (et Number{Integer|String}) (et Integer)))
  (should (equal (et {Number|Integer}String) (et Never)))
  (should (equal (et Integer{Positive}) (et (and Integer Positive))))

  ;; Literals
  (should (equal (et 12.5) (et (Literal 12.5))))
  (should (equal (et :example) (et (Literal :example))))
  (should (equal (et @example) (et (Literal example))))
  (should (equal (et %example) (et (Literal "example"))))

  ;; Angle brackets
  (should (equal (et List<Integer>) (et (List Integer))))
  (should (equal (et Literal<:example>) (et (Literal :example))))
  (should (equal (et Literal<@example>) (et (Literal example))))
  (should (equal (et Literal<%example>) (et (Literal "example"))))
  (should (equal (et Literal<12.5>) (et (Literal 12.5))))

  ;; Binds and typeofs
  (let ((var (alist-get '$a et:repr--test-variables)))
    (should (equal (et $a::Integer) (et (bind ,var Integer))))
    (should (equal (et ::$a) (et (typeof ,var)))))

  ;; Variable assignments
  (should (equal (et String:>:Number) (et (set String Number SUB))))
  (should (equal (et String:<:Number) (et (set String Number SUPER))))
  (should (equal (et String:=:Number) (et (set String Number EQ)))))


;;; Datatype subtyping

(ert-deftest et:test-datatype-subtyping ()
  ;; Basic
  (should (et-subtype? (et Integer) (et Integer)))
  (should (et-subtype? (et String) (et Any)))

  ;; Literals
  (should-not (et-subtype? (et 1) (et String)))
  (should (et-subtype? (et @example) (et Symbol)))
  (should (et-subtype? (et (Literal [1 2]))
                       (et (VectorFresh Number))))

  ;; Numbers
  (should (et-subtype? (et Integer) (et Number)))
  (should (et-subtype? (et Positive) (et Number)))
  (should (et-subtype? (et Negative) (et Number)))

  ;; Fresh datatypes
  (should (et-subtype? (et (ConsFresh Integer String))
                       (et (ConsRW Number Never String Never))))
  (should (et-subtype? (et (VectorFresh Integer))
                       (et (VectorRW Number Never))))
  (should (et-subtype? (et (HashTableFresh Integer String))
                       (et (HashTableRW Number Never String Never))))

  ;; Polyfunctions
  (should (et-subtype? (et (fn [T] T T))
                       (et (fn Number Number))))
  (should-not (et-subtype? (et (fn [T] T T))
                           (et (fn Number String))))

  ;; NonNil
  (should (et-subtype? (et Integer) (et NonNil)))
  (should-not (et-subtype? (et Symbol) (et NonNil)))
  (should-not (et-subtype? (et Any) (et NonNil)))

  ;; Symbols
  (should (et-subtype? (et NonNilSymbol) (et Symbol)))
  (should (et-subtype? (et Var) (et Symbol)))
  (should-not (et-subtype? (et Integer) (et Symbol)))
  (should (et-subtype? (et Var) (et NonNilSymbol)))
  (should-not (et-subtype? (et Symbol) (et NonNilSymbol)))

  ;; Plist
  (should (et-subtype? (et (&Cons :key (&Cons Integer Nil)))
                       (et (&Plist :key Number))))
  (should-not (et-subtype? (et (&Cons :key (&Cons String Nil)))
                           (et (&Plist :key Number))))
  (should (et-subtype? (et (&Plist :a Integer :b String))
                       (et (&Plist :b String :a Number))))
  (should-not (et-subtype? (et (&Plist :a String))
                           (et (&Plist :a Number))))

  ;; Datatype arguments
  (should-not (et-subtype? (et (Emacs buffer)) (et (Emacs marker))))
  (should (et-subtype? (et (VectorFresh Integer)) (et (VectorFresh Number))))
  (should (et-subtype? (et (Function Number Integer)) (et (Function Integer Number))))

  ;; Unrelated datatypes
  (should-not (et-subtype? (et String) (et Number))))


;;; Datatype overlapping

(ert-deftest et:test-datatype-overlapping ()
  ;; Basic datatypes
  (should (et-might-overlap? (et NonNil) (et Symbol)))
  (should-not (et-might-overlap? (et String) (et Integer)))

  ;; Numeric
  (should (et-might-overlap? (et Positive) (et Integer)))
  (should (et-might-overlap? (et Negative) (et Integer)))
  (should-not (et-might-overlap? (et Positive) (et Negative)))

  ;; Cons/function
  (should (et-might-overlap? (et (&Cons @lambda Any)) (et AnyFn)))
  (should (et-might-overlap? (et (&Cons Any Any)) (et AnyFn)))
  (should-not (et-might-overlap? (et (&Cons @foo Any)) (et AnyFn)))
  (should-not (et-might-overlap? (et (&Cons Integer Any)) (et AnyFn)))

  ;; Function/Emacs
  (should (et-might-overlap? (et AnyFn) (et (Emacs interpreted-function))))
  (should-not (et-might-overlap? (et AnyFn) (et (Emacs buffer)))))


;;; Simplification

(ert-deftest et:test-simplification ()
  ;; Duplicate union
  (should (equal (et-union (et Integer) (et Integer))
                 (et Integer)))
  ;; Subtypes
  (should (equal (et-union (et Integer) (et Number)) (et Number)))
  (should (equal (et-union (et Number) (et Integer)) (et Number)))
  (should (equal (et-union (et Nil|True) (et List<Integer>)) (et True|List<Integer>)))
  ;; Never
  (should (equal (et-union (et Never) (et Integer)) (et Integer)))
  ;; Unrelated types
  (should (equal (et-union (et Integer) (et String))
                 (et (or Integer String)))))


;;; Intersection and subtraction

(ert-deftest et:test-intersection-and-subtraction ()
  ;; Subtype intersection
  (should (et-type=? (et-subsect (et Number) (et Integer))
                     (et Integer)))
  (should (et-type=? (et-supersect (et Number) (et Integer))
                     (et Integer)))

  ;; Disjoint intersection
  (should (et-type=? (et-subsect (et String) (et Integer))
                     (et Never)))
  (should (et-type=? (et-supersect (et String) (et Integer))
                     (et Never)))

  ;; Nontrivial intersection
  (should (et-type=? (et-subsect (et Integer) (et Positive))
                     (et Never)))
  (should (et-type=? (et-supersect (et Integer) (et Positive))
                     (et Integer)))

  ;; Datatype intersection
  (should (et-type=?
           (et-subsect (et (VectorFresh (or Integer String)))
                       (et (VectorFresh (or Integer Symbol))))
           (et (VectorFresh Integer))))
  (should (et-type=?
           (et-supersect (et (VectorFresh (or Integer String)))
                         (et (VectorFresh (or Integer Symbol))))
           (et (VectorFresh Integer))))

  ;; Subtype subtraction
  (should (et-type=? (et-subtract (et Integer) (et Number))
                     (et Never)))

  ;; Union subtraction
  (should (et-type=? (et-subtract (et (or String Integer)) (et String))
                     (et Integer)))

  ;; Symbol subtraction
  (should (et-type=? (et-subtract (et Symbol) (et Nil))
                     (et NonNilSymbol)))

  ;; Nontrivial subtraction
  (should (et-type=? (et-subtract (et Number) (et Integer))
                     (et Number))))


;;; Matching

(ert-deftest et:test-matching ()
  ;; Datatypes
  (should (et:match-result->success
           (et-sub-match (et-matcher [] Number) (et Integer))))
  (should-not (et:match-result->success
               (et-sub-match (et-matcher [] Number) (et String))))

  ;; Generics
  (let ((result (et-sub-match (et-matcher [E] List<E>)
                              (et List<Integer>))))
    (should (et:match-result->success result))
    (should (et-type=? (car (et:match-result->value result))
                       (et Integer))))

  ;; Generic upper bounds
  (should (et:match-result->success
           (et-sub-match (et-matcher [(E <: Number)] List<E>)
                         (et List<Integer>))))
  (should-not (et:match-result->success
               (et-sub-match (et-matcher [(E <: Number)] List<E>)
                             (et List<String>))))

  ;; Generic lower bounds
  (should (et:match-result->success
           (et-sub-match (et-matcher [(E :> Integer)] List<E>)
                         (et List<Number>))))
  (should-not (et:match-result->success
               (et-sub-match (et-matcher [(E :> Integer)] List<E>)
                             (et List<String>))))

  ;; Type unions
  (let ((result (et-sub-match (et-matcher [E] E)
                              (et (or Integer String)))))
    (should (et:match-result->success result))
    (should (et-type=? (car (et:match-result->value result))
                       (et (or Integer String)))))

  ;; Matcher unions/intersections
  (should (et:match-result->success
           (et-sub-match (et-matcher [] (or String Number))
                         (et Integer))))
  (should-not (et:match-result->success
               (et-sub-match (et-matcher [] (and Number NonNilSymbol))
                             (et Integer))))

  ;; Aliases
  (should (et:match-result->success
           (et-sub-match (et-matcher [E] &List<E>)
                         (et &List<Integer>))))
  (should (et:match-result->success
           (et-sub-match (et-matcher [] (or Nil (&Cons Integer Any)))
                         (et List<Integer>))))

  ;; Sub/iso matching
  (should (et:match-result->success
           (et-sub-match (et-matcher [] Number) (et Integer))))
  (should-not (et:match-result->success
               (et-iso-match (et-matcher [] Number) (et Integer))))

  ;; Iso generics
  (let ((result (et-iso-match (et-matcher [E] List<E>)
                              (et List<Integer>))))
    (should (et:match-result->success result))
    (should (et-type=? (car (et:match-result->value result))
                       (et Integer))))

  ;; Variable assignments
  (should (et:match-result->success
           (et-sub-match (et-matcher [] Integer{Number:>:Integer})
                         (et Integer))))
  (should (et:match-result->success
           (et-sub-match (et-matcher [] Integer{Integer:<:Number})
                         (et Integer))))
  (should-not (et:match-result->success
               (et-sub-match (et-matcher [] Integer{Number:=:Integer})
                             (et Integer))))

  ;; Noinfer
  (should (et:match-result->success
           (et-sub-match (et-matcher [] (noinfer Number))
                         (et Integer))))

  ;; Type operations
  (should (et:match-result->success
           (et-sub-match (et-matcher [] (minus Number String))
                         (et Integer))))

  ;; Polymorphs
  (et-with-type-variables [(P :> Integer)]
    (should (et:match-result->success
             (et-sub-match (et-matcher [] P) (et Integer))))))


;;; Loop aliases

(ert-deftest et:test-loop-aliases ()
  ;; List -> ListFresh
  (should
   (et-type=? (et:algebra-freshen-type (et List<Number>))
              (et ListFresh<Number>)))

  ;; ListFresh -> &List
  (should
   (et-type=? (et:algebra-unfreshen-type (et ListFresh<Number>))
              (et &List<Number>)))

  ;; Non-standard loops
  (let* ((type (et:algebra-freshen-type (et Tree<Number>)))
         (match (et-iso-match
                 (et-matcher [Recursive] (or Number ListFresh<Recursive>))
                 type)))
    (should (et:match-result->success match))
    (should (et-type=? (car (et:match-result->value match)) type))))
