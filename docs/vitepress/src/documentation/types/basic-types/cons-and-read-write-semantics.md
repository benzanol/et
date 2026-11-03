---
title: "Cons (and read-write semantics)"
---

# Cons (and read-write semantics)

The type `(Cons A B)` represents a cons cell with car `A` and cdr `B`. By default, the `Cons` type is *invariant*, meaning that cons types don't extend each other just because their argument types do. For example, a `Cons<Integer_Integer>` is not a valid `Cons<Number_Number>`. Consider the following situation:  

```emacs-lisp
(et-defun multiply-car (cons: Cons<Number_Any>) Nil
  (setcar cons (* 1.5 (car cons))))

(et-defvar integer-cons Cons<Integer_Integer> (cons 1 1))
(multiply-car integer-cons)
integer-cons ; (1.5 . 1)
```

As shown, accepting a `Cons<Integer_Integer>` as a `Cons<Number_Any>` violates correctness, and must be rejected. In order to allow for *covariance*, we must define a read-only version of a cons cell, `&Cons`. A `&Cons` cannot be written to, so `&Cons<Integer_Integer>` is a subtype of `&Cons<Number_Number>` without violating correctness.  
Furthermore, a standard cons can be a subtype of a read-only cons, but not the other way around. To understand why, think about each cons type as "supporting" certain operations:  

|  | car | cdr |
| --- | --- | --- |
| `&Cons<A_B>` | Read `A` | Read `B` |
| `Cons<X_Y>` | Read `X`, Write `X` | Read `Y`, Write `Y` |

If `X` is a subtype of `A`, and `Y` is a subtype of `B`, then `Cons<X_Y>` supports all of the operations that `&Cons<A_B>` does, so we can conclude that `Cons<X_Y>` should be a subtype of `&Cons<A_B>`.  

For readability, types containing `Cons` and `&Cons` types will be printed in standard lisp:  

```emacs-lisp
(et Cons<1_Nil>) ; (1)
(et &Cons<1_2>) ; &(1 . 2)
(et &Cons<1_&Cons<2_&Cons<3_Nil>>>) ; &(1 2 3)
```

This is nothing more than a compact way of printing the underlying cons types.
