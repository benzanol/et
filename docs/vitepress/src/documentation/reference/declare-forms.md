---
title: "Declare forms"
---

# Declare forms

This page refers to *top-level declare forms*, which are defined at the top level of the file. For function-level declare forms, see [1. Annotating code](/documentation/overview/introduction/1-annotating-code).  

Top-level declare forms can be defined in one of two ways.  
Without any dependencies:  

```emacs-lisp
(declare
 (et (@declare-form-1 ...)
     (@declare-form-2 ...)))
```

With `et-annotate` as a dependency:  

```emacs-lisp
(et-declare
 (@declare-form-1 ...)
 (@declare-form-2 ...))
```


## @def

Declare a function's type.  

```emacs-lisp
(et-declare
 (@def my-fn (param1: Type1 param2: Type2) ReturnType)
 ;; With generics
 (@def my-generic-fn ([T (N <: Number)] obj: T nums: List<N>) Alist<N_T>)
 ;; With inline generics
 (@def my-inline-generic-fn (obj: [T] nums: [N <: Number]) Alist<N_T>))
```


## @check

Declare a checker for a macro.  

```emacs-lisp
(et-declare
  ;; A macro which can be type-checked by simply expanding it
  (@check my-simple-macro ($expand))
  ;; A macro which takes a string argument, and the rest is the body
  (@check my-with-default-directory ($body String))
  ;; Type check this macro by checking some other equivalent expression
  (@check my-increment ($arglist (p) ($exp `(setf ,p (1+ ,p))))))
```


## @variable

Declare a variable's type.  

```emacs-lisp
(et-declare
 (@variable my-variable Type))
```


## @alias

Define an alias.  

```emacs-lisp
(et-declare
 (@alias MyIntList List<Integer>)
 ;; With a type parameter
 (@alias MySymbolAlist [V] Alist<Symbol_V>)
 ;; With a default value for a type parameter
 (@alias MyOtherAlist [K (V = Any)] Alist<K_V>)
 ;; With a constraint and a default value on the same type parameter
 (@alias MyNumberAlist [K (V <: Number) (V = Integer)] Alist<K_V>))
```


## @spec

Define a custom spec macro.  

```emacs-lisp
(et-declare
 (@spec my-cons (&optional arg1 arg2)
        `(Cons ,(or arg1 'Any) ,(or arg2 arg1 'Any))))
```


## @symbol-property

To get accurate types for `put` and `get`, you can declare a particular symbol-property to have a particular type.  

```emacs-lisp
(et-declare
 (@symbol-property my-documentation String))

(get SYMBOL 'foo) ; => Any
(get SYMBOL 'my-documentation) ; => String?
```


## @eval

Run some elisp code within the checker process.  

```emacs-lisp
(et-declare
 (@eval (some-code ...)))
```
