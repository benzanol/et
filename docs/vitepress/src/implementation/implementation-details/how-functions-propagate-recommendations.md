---
title: "How functions propagate recommendations"
---

# How functions propagate recommendations

For brevity, let's define `Pair := Cons<String_Positive>`.  

An important piece of this puzzle is determining how the top-level `cons` knows what recommendations to pass to its children. The trick is to construct a function type representing the current invocation of `cons`, and compare it to the actual type of the `cons` function.  
Since the invocation of `cons` is called with 2 arguments, and is recommended to return `List<Pair>`, we construct the type `&(ARG1 ARG2) -> List<Pair>`. Compare this to the real type-signature of `cons`:  

```typescript
invocation: &(ARG1 ARG2) -> List<Pair>
cons: &(A B) -> ConsFresh<A_B>
```

Attempting to match `ConsFresh<A_B>` against `List<Pair>` gets us the following:  

```emacs-lisp
ConsFresh<A_B> <: List<Pair>
A <: Pair
B <: List<Pair>
```

Then, we attempt to match the invocation arglist to the function arglist:  

```emacs-lisp
&(ARG1 ARG2) = &(A=Pair B=List<Pair>)
ARG1 = Pair
ARG2 = List<Pair>
```

Thus, we determine argument 1 — `(cons "Alice" 1.5)` — should get the recommendation `Pair`, and that argument 2 — `nil` — should get the recommendation `List<Pair>`.
