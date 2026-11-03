---
title: "Union types"
---

# Union types

Any list of types can be unioned to create a new type using the `or` utility.  

```emacs-lisp
(et-is? 4 (or Number String)) ; t
(et-is? "foo" (or Number String)) ; t
```

A pipe can be used as shorthand syntax for a union type.  

```emacs-lisp
(et-is? 4 Number|String) ; t
```

The `Never` type is shorthand for an empty union, and matches no values.  

```emacs-lisp
(equal (et (or)) (et Never)) ; t
```

Placing a question mark at the end of a type name is shorthand for unioning that type with `Nil`.  

```emacs-lisp
(equal (et Integer?) (et Nil|Integer)) ; t
```
