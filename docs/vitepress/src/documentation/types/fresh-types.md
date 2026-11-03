---
title: "Fresh types"
---

# Fresh types

One unique aspect of lisp is that the same value can represent different types in different contexts. Consider the following (all valid) variable definitions:  

```emacs-lisp
(et-defvar my-numbers List<Integer> (list 1 2))
(et-defvar my-tuple tuple<Integer_Integer> (list 1 2))
(et-defvar my-cons Cons<Integer_Any> (list 1 2))
```

For maximum unobtrusiveness, all of these should be accepted, as they are all perfectly valid lisp code. This begs the question, what type could `(list 1 2)` possibly return to satisfy all 3 of these cases? To answer this, we introduce *fresh types* for representing newly created values.  

```emacs-lisp
(et-typecheck (list 1 2)) ; ConsFresh<1, ConsFresh<2, Nil>> 
(et-is? (list 1 2) List<Integer>) ; t
(et-is? (list 1 2) tuple<Integer_Integer>) ; t
(et-is? (list 1 2) Cons<Integer_Any>) ; t
```

Unlike other container types, which care about read-write permissions, fresh types only care about structure: a fresh type can be assigned to any type which it is *structurally compatible* with.  


## Automatic unfreshening

Fresh types are extremely powerful, but also extremely dangerous. Imagine that we allowed fresh types to propagate through function calls:  

```emacs-lisp
(et-defun cons-same (value: [T]) ConsFresh<T_T>
  (cons value value))

(et-defvar my-var (Cons Cons<Integer_Integer> Cons<Number_Number>)
  (cons-same (cons 1 2))) ; => (ConsFresh ConsFresh<1_2> ConsFresh<1_2>)

(setcar (cdr my-var) 1.5)
(car (car my-var)) ; Type-checks as Integer, but is actually 1.5
```

The newly-created cons cell is simultaneously assigned to a `Cons<Integer_Integer>` and to a `Cons<Number_Number>`, which results in a correctness bug.  
 To remedy this, fresh types can exist only temporarily. A fresh type must resolve to a concrete type as soon as a value is used (passed to another function, assigned to a variable, etc). In the example above, we instead have  

```emacs-lisp
(cons-same (cons 1 2)) ; => (ConsFresh &Cons<1_2> &Cons<1_2>)
```

because `ConsFresh<1_2>` is automatically unfreshened to `&Cons<1_2>` before passing it to `cons-same`.  


## freshen and freshen-shallow

The `freshen` and `freshen-shallow` type-utilitys can be used to turn a concrete type into an equivalent fresh type:  

```emacs-lisp
(et-defun list (&rest objects: [T]) (freshen-shallow T))
(et-defun copy-tree (x: [T]) (freshen T))

(et-defvar my-nested-list List<List<Integer>> nil)
(apply #'list my-nested-list) ; => ListFresh<List<Integer>>
(copy-tree my-nested-list) ; => ListFresh<ListFresh<Integer>>
```
