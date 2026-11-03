---
title: "Provided type operations"
---

# Provided type operations

## extends?

`(extends? CHILD PARENT IF ELSE)`  

Check whether `CHILD` is a subtype of `PARENT`.  

```emacs-lisp
(et (extends? Integer Number 1 0)) ; => 1
(et (extends? String Number 1 0)) ; => 0
;; The type variable COULD represent a subtype of Number, or it might not
(et-with-type-variables [T] (et (extends? T Number 1 0))) ; => 1 | 0
```


## infer

`(infer TYPE [GENERICS] PATTERN IF ELSE)`  

Match `type` against `pattern` and determine values for each of the `generics`. If it matches, then evaluate `if` with the inferred generics. Otherwise, return `else`.  

```emacs-lisp
(et (infer List<Integer> [T] List<T> T Never)) ; => Integer
(et (infer String [T] List<T> T Never)) ; => Never
```


## minus

`(minus A B)`  

Subtract `B` from `A`.  

```emacs-lisp
(et (minus Number|String Number)) ;; => String
(et (minus List<Integer> Nil)) ;; => (Integer . List<Integer>)
```

There is not always a perfect answer for type-subtraction. In such cases, `minus` will always err on the side of keeping the type *larger* than it should be. See [Why upper-bound type-intersection?](/documentation/types/narrowing-types#intersection-bounds).  

```emacs-lisp
(et (minus Number Integer)) ;; => Number
```


## subsect/supersect

`(subsect A B)` / `(supersect A B)`  

Perform lower/upper bound intersection.  

```emacs-lisp
(et (subsect Integer Positive)) ; => Never
(et (supersect Integer Positive)) ; => Integer

(et (supersect (fn1 Integer Integer) (fn1 Number Number)))
;; => Function<&(Number), Integer>

(et (supersect AnyFn Cons<Any_Any>)) ; => Function<Never, Any>
(et (supersect AnyFn Cons<Integer_Any>)) ; => Never
(et (supersect AnyFn Cons<@foo_Any>)) ; => Never
(et (supersect AnyFn Cons<@lambda_Any>)) ; => Function<Never, Any>
```


## eval

`(eval FUNCTION TYPES...)`  

The `eval` op allows calling an arbitrary function on the type arguments. The arguments will be passed as type objects.  

```emacs-lisp
(et (eval et:std-leaves-of (tuple Boolean List<String> Cons<1_2>)))
;; => `t' | String | `1' | `2'
```


## switch

`(switch TYPE CASES...)`  

```emacs-lisp
(et-defalias SumOf [(Nums <: &List<Number>)]
  (switch Nums
          [Nil 0]
          [[One] &tuple<One> One]
          [&List<Integer> Integer]
          Number))

(et-expand-aliases (et SumOf Nil)) ;; => 0
(et-expand-aliases (et SumOf (Cons 123 Nil))) ;; => 123
(et-expand-aliases (et SumOf (tuple 2 3))) ;; => Integer
(et-expand-aliases (et SumOf (tuple 1.5 2))) ;; => Number
```

Each case is either  

-   `[type output]`
-   `[[generics] pattern output]`

If the pattern and output happen to be the same, then you can use the shortcut  

-   `[[generics] pattern-and-output]`

The final case can be a single default type, which will be returned if no cases match. If a default type is not provided, `Never` will be used as the default.  


## map-cases

`(map-cases TYPE CASES...)`  

One limitation of `switch` is that it is limited when the input is a union. `map-cases` is like `switch`, except cases can match *partially.* If a case matches only partially, the rest of the type will continue through the remaining cases, until the entire type has been matched.  

If the cases collectively match the entire type, then the result will be the union of the outputs of the matching cases. If part of the type is *not* matched by any type, the result will be `Never`.  

```emacs-lisp
(et-defalias ExampleElementOf [Seq]
  (map-cases Seq
             [[T] &List<T> T]
             [[T] Vector<T> T]
             [BoolVector Boolean]))

(et-expand-aliases (et ExampleElementOf List<Integer>|Vector<Integer>))
```


## freshen-deep/freshen-shallow

`(freshen-deep TYPE)` / `(freshen-shallow TYPE)`  

Convert a type to its freshened version.  
`freshen` will recursively freshen every level of the type, like `(copy-tree value)`.  
`freshen-shallow` will only freshen the top-level list, like `(apply #'list value)`  

```emacs-lisp
(et (freshen-deep List<List<Integer>>))
;; => ListFresh<ListFresh<Integer>>
(et (freshen-shallow List<List<Integer>>))
;; => ListFresh<List<Integer>>
(et (freshen-deep (Cons (Cons 1 2) (Cons 3 4))))
;; => (ConsFresh (ConsFresh 1 2) (ConsFresh 3 4))
(et (freshen-shallow (Cons (Cons 1 2) (Cons 3 4))))
;; => (ConsFresh (Cons 1 2) (ConsFresh 3 4))
```


## overlapping

`(overlapping? A B YES NO?)`  

If `A` and `B` are overlapping, return `YES`, otherwise return `NO`.  
If `NO` is omitted, it defaults to `Never`.  

```emacs-lisp
(et (overlapping? Integer|String Boolean|String True Nil)) ; => True
(et (overlapping? Integer|String Boolean True Nil)) ; => Nil
(et (overlapping? Integer|String Boolean True)) ; => Never
```
