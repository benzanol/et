---
title: "Type aliases"
---

# Type aliases

Type-aliases allow for defining new types by mapping names to type expansions. Aliases expand *lazily*, meaning that they will expand to their definition only when forced to. This feature of aliases makes defining recursive types like `List` possible.  

```emacs-lisp
(et-defalias Boolean [] (or True Nil))
(et-defalias List [T] (or Nil (Cons T List<T>)))
```

Defining an alias will automatically define a read-only version of that alias. This conversion is done by simply walking the definition, and replacing every read-write type with its read-only equivalent.  

```emacs-lisp
(et-subtype? (et List<Integer>) (et List<Number>)) ; nil
(et-subtype? (et &List<Integer>) (et &List<Number>)) ; t
(et-expand-aliases (et &List<Integer>)) ; Nil | (&Cons Integer &List<Integer>)
```

Using a read-only version of an alias does *not* convert the arguments to read only. For example, `&List<List<Integer>>` is a read-only list whose elements are read-write lists.  

```emacs-lisp
(et-subtype? (et &List<List<Integer>>) (et &List<List<Number>>)) ; nil
(et-subtype? (et &List<&List<Integer>>) (et &List<&List<Number>>)) ; t
```

Alias arguments can contain constraints on what types are valid:  

```emacs-lisp
(et-defalias NumList [(N <: Number)]
  List<N>)
```

For multiple constraints on the same argument, use multiple forms in the vector. The parser will know that these refer to the same type-argument.  

```emacs-lisp
(et-defalias AtleastIntList [(N <: Number) (N :> Integer)]
  List<N>)
```
