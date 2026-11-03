---
title: "Recommendations (top-down)"
---

# Recommendations (top-down)

The correctness and types of the program are all determined bottom-up as described previously, but inner expressions sometimes need to be *nudged in the right direction*. Consider the following example:  

```emacs-lisp
(et-defvar my-list List<Cons<String_Positive>>
  (cons (cons "Alice" 1.5) nil))
```

`(cons "Alice" 1.5)` evaluates to the type `ConsFresh<"Alice"_1.5>`; however, as described in [Fresh types](/documentation/types/fresh-types), this must be resolved to a concrete cons type before proceeding. Of the many options it has to resolve to, we can clearly see that it *should* choose `Cons<String_Positive>`, but it needs some way to know that. To make this possible, a *type-recommendation* must be passed down when traversing an expression.  
`et-defvar` type-checks the top-level `cons` with the recommendation `List<Cons<String_Positive>>`. Based on this, `cons` calculates that it should give its first child the recommendation `Cons<String_Positive>`. Then, when `(cons "Alice" 1.5)` is converted to a concrete type, it uses the recommendation as a hint, and returns the correct type.
