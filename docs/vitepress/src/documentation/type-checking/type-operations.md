---
title: "Type operations"
---

# Type operations

> [!WARNING]  
> Defining custom type operations should be considered unstable, as defining type operations requires direct manipulation of type objects, which does not currently have a stable API.  

While defspecs operate on its arguments in unparsed form, defops allow operating on its arguments after they have already been converted to types. This makes operations much more powerful, but also more difficult to define.  

```emacs-lisp
(et-defop NAME (ARGS...)
  [:to-string TO-STRING-CODE] ; always optional
  [:parse PARSE-CODE] ; only required for very advanced defops
  EVAL-CODE)
```

In the simplest case, where all of the arguments to the defop are types, `:parse` can be ignored. `EVAL-CODE` uses each type-object in `ARGS` and returns a new type object.  

```emacs-lisp
(et-defop minus (a b)
  (et-subtract a b)) ; both a and b are type objects
```

Sometimes, not all arguments to an operation are types. For example, consider the `infer` operation. `infer` matches `TYPE` against `PATTERN`. If it matches, it evaluates `IF`, and if not, it evaluates `ELSE`.  

```emacs-lisp
;; (infer TYPE [GENERICS...] PATTERN IF ELSE)
(et (infer List<Integer> [T] List<T> T : Never)) ; => Integer
(et (infer Boolean [T] List<T> T : Never)) ; => Never
```

The definition of `infer` looks like this:  

```emacs-lisp
(et-defop infer (type [genvec :generics] [matcher :matcher] [yes :repr] [no :repr])
  ...)
```

Each argument (other than `type`) is paired with a keyword. The keyword specifies how the operation argument is parsed. Here, `:generics` refers to a vector of generics, `:matcher` refers to a pattern which types can be matched against, and `:repr` refers to an unevaluated type-expression. For more information on all of these, check the implementation details documentation.  
For more information on defining advanced defops, reading through the examples defined in `;;;; Op definitions` in `et.el` will be far more useful than this meager docs page.
