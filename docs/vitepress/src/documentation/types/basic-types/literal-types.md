---
title: "Literal types"
---

# Literal types

Any lisp value can be turned into a `Literal` type, which is a type that matches only that value.  

```emacs-lisp
(et-is? 4 (Literal 4)) ; t
(et-is? "foo" (Literal "foo")) ; t
```

There is also shorthand syntax for defining literal numbers, strings, and symbols:  

```emacs-lisp
(et 4) ; (Literal 4)
(et @foo) ; (Literal 'foo)
(et %bar) ; (Literal "bar")
```

The types `True` and `Nil` are shorthands for `(Literal t)` and `(Literal nil)` respectively.  

```emacs-lisp
(et True) ; (Literal t)
(et Nil) ; (Literal nil)
```
