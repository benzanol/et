---
title: "Custom spec macros"
---

# Custom spec macros

A "spec" is the raw human-writable list format which eventually gets parsed into an actual type object. `et-defspec` can be used to define a custom spec-macro, which expands one spec to another spec before it is parsed into a type object.  

```emacs-lisp
(et-defspec tuple (&rest args)
  (if (null args) '(Nil)
    `(Cons ,(car args) (tuple ,@(cdr args)))))

(et tuple 1 2 3) ; => (Cons 1 (Cons 2 (Cons 3 Nil)))
```

Keep in mind that the spec returned by a spec-macro cannot use the **symbol syntax** for types, it must use **list syntax**. This includes wrapping even zero-argument types in a list (as shown above with `Nil`).  
Many types you have seen so far, such as `args`, and `fn`, are actually spec macros which expand to real types. It is generally convention that spec-macros and type-operations are *lowercase*, while core datatypes and aliases are uppercase.
