---
title: "Type-variable assignment"
---

# Type-variable assignment

Consider an attempt to define a generic array type consisting of both vectors and bool vectors:  

```emacs-lisp
(et-defalias MyNaiveArray [E]
  (or Vector<E>
      BoolVector))

(et-expand-aliases (et MyNaiveArray<Number>)) ; => Vector<Number> | BoolVector
```

However, `BoolVector` is clearly not an array of numbers. The solution is to attach a *variable assignment* to the `BoolVector` case:  

```emacs-lisp
(et-defalias MyArray [E]
  (or Vector<E>
      BoolVector{E:=:Boolean}))

(et-expand-aliases (et MyArray<Number>)) ; => Vector<Number>
(et-expand-aliases (et MyArray<Boolean>)) ; => Vector<Boolean> | BoolVector
(et-expand-aliases (et MyArray<Any>)) ; => Vector<Any>
```

As shown, `:=:` only succeeds when the type variable matches exactly (it is an *invariant* assignment). Alternatively, `:>:` (*covariant* assignment) succeeds when the type variable contains its assignment, and the less common `:<:` (*contravariant* assignment) succeeds when the type variable is contained within its assignment.  

Since the read-only version of an alias is designed to turn the alias from invariant to covariant, it will automatically convert all *invariant* assignment to *covariant* ones.  

```emacs-lisp
&MyArray<E> := &Vector<E> | BoolVector{E:>:Boolean} ; auto-generated

(et-expand-aliases (et &MyArray<Number>)) ; => &Vector<Number>
(et-expand-aliases (et &MyArray<Boolean>)) ; => &Vector<Boolean> | BoolVector
(et-expand-aliases (et &MyArray<Any>)) ; => &Vector<Any> | BoolVector
```

Variable assignments can also be used to detect the appropriate type for a type variable:  

```emacs-lisp
(et-defun my-array-get ([T] array: &MyArray<T>) T
  ...)

(my-array-get {&Vector<Number>}) ; => Number
(my-array-get {BoolVector}) ; => Boolean
(my-array-get {&Vector<Number> | BoolVector}) ; => Number | Boolean
```
