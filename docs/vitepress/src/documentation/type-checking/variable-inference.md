---
title: "Variable inference"
---

# Variable inference

When defining a variable in a let-binding, it is often unclear just from the initial value what the type should be. There are an infinite number of types that might be intended when initializing a variable as `nil` in a let binding.  

```emacs-lisp
(let ((stack nil)
      (finished nil)
      (i 0))
  (while (not finished)
    (setq stack (cons i stack))
    (setq i (1+ i))
    (when (eq i count) (setq finished t)))
  stack)
```

One solution to this is to explicitly widen the type of each value:  

```emacs-lisp
(let ((stack (et: List<Integer> nil))
      (finished (et: Boolean nil))
      (i (et: Integer 0)))
  ...)
```

This works, but requires significantly changing the way that the developer writes code.  

Instead, when checking a `let` expression, `et` will perform a first pass over the body and determine each place that a variable is used. Then, it will make a *best guess* for what the variable type should be, and proceed with type-checking using the best-guess type. This will eliminate the need for explicitly casting the value to the correct type.  
In this example, the first pass yields the following requirements:  

```emacs-lisp
typeof stack :> nil, ConsFresh<typeof i, typeof stack>
typeof i :> 0, Integer
typeof finished :> True, Nil
```

Solving these equations yields a type of `Integer` for `i`, `List<Integer>` for `stack`, and `True|Nil` for `finished`.
