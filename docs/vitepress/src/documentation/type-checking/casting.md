---
title: "Casting"
---

# Casting

Sometimes, type-recommendations will not be enough to coerce an expression to the desired type. In this case, there are several macros that allow explicitly overriding the recommendation system.  
The `et:` macro, or *safe cast*, can be used to safely cast an expression to a particular type. Its safety comes from the fact that the type of the expression must be compatible with the type it gets cast to, guaranteeing that correctness will not be compromised.  

```emacs-lisp
nil ; => Nil
(et: List<Integer> nil) ; => List<Integer>
(et: Vector<Integer> nil) ; => error - expected Vector<Integer>, found Nil
```

The `et!` macro, or *unsafe cast*, can be used to cast an expression to any arbitrary type, regardless of the type actually returned by the expression. For obvious reasons, the use of this macro compromises correctness.  

```emacs-lisp
(et! Vector<Integer> nil) ; => Vector<Integer>
```
