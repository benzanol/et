---
title: "Type polymorphism"
---

# Type polymorphism

`et` also supports polymorphic function types. Polymorphic function types allow the output of a function to depend on the input of the function.  

```emacs-lisp
(defun my-cons (a b)
  (declare (et (@generics [A B])
               (a A) (b B)
               (@return ConsFresh<A_B>)))
  (cons a b))
```

Equivalently,  

```emacs-lisp
(et-defun my-cons ([A B] a: A b: B) ConsFresh<A_B>
  (cons a b))
```

Similarly to alias arguments, constraints can also be applied to function generics.  

```emacs-lisp
(defun my-sort (nums)
  (declare (et (@generics [(N <: Number)]) (nums &List<N>)
               (@return ListFresh<N>)))
  (sort (apply #'list nums)))
```

Equivalently,  

```emacs-lisp
(et-defun my-sort ([(N <: Number)] nums: &List<N>) ListFresh<N>
  (sort (apply #'list nums)))
```

The function body must be type-checked without knowing what these type-variables will eventually resolve to. A type-variable is not a real type, so type-variables in the parameters are each replaced with their upper bound, and then annotated with the corresponding type variable:  

```emacs-lisp
(et-defun my-num-cons ([A (B <: Number)] a: A b: B) ConsFresh<A_B>
  a ; => Any{$A}
  b ; => Number{$B}
  (cons a b))
```


## Inline generics syntax

It is extremely common that a parameter's type is exactly a type variable (`left` and `right` in `cons` for example, but not `nums` in `sort`). For these cases, we introduce a shorthand syntax for defining the generic at the same time as assigning it to the parameter:  

```emacs-lisp
(et-defun my-cons (a: [A] b: [B]) ConsFresh<A_B>
  (cons a b))
```

This syntax also allows constraints, as follows:  

```emacs-lisp
(et-defun my-num-cons (a: [A <: Number] b: [B <: Number]) ConsFresh<A_B>
  (cons a b))
```

The shorthand cannot express multiple constraints on a single generic; for this, the standard syntax must be used.  


## Polymorphic function types

We saw the `Function` type earlier, which is a type for a function with a fixed input and return type. The `PolyFunction` type is similar to `Function` except that its input and output are *type-expressions* rather than single types. In addition to standard types, type-expressions can also contain type-variables, type-operations, and [Type-variable assignment](/documentation/types/type-polymorphism/type-variable-assignment).  

To define a `PolyFunction` type, add a vector of generics before the arglist type of `fn`.  

```emacs-lisp
(et (fn [T] (args T T) ConsFresh<T_T>))
;; => PolyFunction<[T] &(T T) -> ConsFresh<T, T>>
```
