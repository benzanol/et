---
title: "Narrowing types"
---

# Narrowing types

A common pattern in lisp is using predicates to handle a value differently based on its runtime type.  

```emacs-lisp
(et-defun my-repeat (repeat: Integer obj: Integer|String) String
  (if (stringp obj)
      (s-repeat repeat obj)
    (make-string repeat obj)))
```

Within the `then` branch of the if-statement, we want the type of `obj` to be narrowed to just `String`, and likewise to just `Integer` in the else branch.  
To solve this, we introduce narrow-binding. Narrow-binds allow a type to contain internal information about variables in scope.  

```emacs-lisp
(stringp obj) ; :: True{obj : String} | Nil{obj : Integer}
```

This type describes that when it is true, `obj` has type `String`, and when it is nil, `obj` has type `Integer`. Utilizing this, conditional macros such as `if` can type-check each branch with the correct narrowed variable types.  
In order for this to work, the type returned by a variable must not only contain the type of the value, but also encode that the type represents the value currently stored in `obj`.  

```emacs-lisp
obj ; :: Integer|String {= obj}
```

This type describes that it is an `Integer` or `String`, and also that whatever value it holds is equal to the value stored in the variable `obj`. This allows for narrowing not just on predicate outputs, but on variables themselves:  

```emacs-lisp
(et-defun my-negative (val: Number?) Number
  (if val
      (- val) ; val : Number
    0)) ; val : Nil
```


## Defining predicate functions

To define a function which narrows an argument, the value being narrowed must be assigned to a generic type parameter. Then, the `is?` type operation can be used in the return:  

```emacs-lisp
(et-defun my-stringp (s: [S]) (is? S String)
  (stringp s))
```

Then, suppose you type-check the expression  

```emacs-lisp
(stringp obj)
```

As discussed, `obj` will evaluate to the type `String|Integer {= obj}`. When type-checking the call to `my-stringp`, this type will get assigned to `S`. The `is?` type-utility then does the following conversion:  

```emacs-lisp
(is? String|Integer{=obj} String)
=> True{obj : String} | Nil{obj : Integer}
```

Now, consider the behavior of a `not`-like function:  

```emacs-lisp
(et-defun my-not (bool: [T]) (is? T Nil)
  (not bool))
```

If we evaluate `(my-not (my-stringp obj))`, then we will get the conversion  

```emacs-lisp
(is? {True{obj : String} | Nil{obj : Integer}} Nil)
=> True{obj : Integer} | Nil{obj: String}
```

This is exactly the behavior we would want out of this expression, and we get it nearly for free.  


## Non-exhaustive predicates

Not all predicates exhaustively classify an entire type. For example, `evenp` returns `t` only for integers, but not for *all* integers. For predicates like this, use the `is-a?` operation.  

```emacs-lisp
(et-defun my-evenp (val: [T]) (is-a? T Integer)
  (and (integerp val) (evenp val)))
```

Then, if we evaluate `(my-evenp obj)`, we get the following conversion  

```emacs-lisp
(is-a? String|Integer{= obj} Nil)
=> True{obj : Integer} | Nil{obj: String|Integer}
```

If `(my-evenp obj)` returns `t`, the typesystem can conclude that its input was definitely an integer, but if it returns `nil`, it cannot make a similar conclusion in the reverse direction.  


## Advanced narrowing

Advanced narrowing will likely be useless for 99% of users, but understanding the narrowing internals can be a fun exercise if you are up for a challenge.  

The `is?` and `is-a?` type utilities are very useful, but each serves a very specific purpose (although those purposes are likely sufficient for most people.) `is?` and `is-a?` are each built out of more fundamental building blocks, which could in theory be used to define more custom narrowing behaviors. These building blocks are:  

-   `when` - Extract the "effects" from a particular type and put it onto another type
-   `supersect` - Perform an *upper-bound* type intersection
-   `minus` - Perform an *upper-bound* type subtraction

For example, `is?` is defined as  

```emacs-lisp
(or (when (supersect TYPE PRED) True)
    (when (minus TYPE PRED) Nil))
```

In other words, return `True` in the cases where `TYPE` overlaps with `PRED`, and `Nil` in the cases where `TYPE` differs from `PRED`.  
To see how narrows propagate, watch what happens when we type-check `(stringp obj)`:  

```emacs-lisp
(supersect String|Number{= obj} String) => String{= obj}
(when String{= obj} True) => True{obj : String}

(minus String|Number{= obj} String) => Number{= obj}
(when Number{= obj} Nil) => Nil{obj : Number}

(is? String|Number{= obj} String) => True{obj : String} | Nil{obj : Number}
```

Similarly, `is-a?` is defined by the following:  

```emacs-lisp
(or (when (supersect TYPE PRED) True)
    Nil)
```

The `isnt?` type utility, the opposite of `is?`, is defined by simply flipping the order of `is?`:  

```emacs-lisp
(or (when (supersect TYPE PRED) Nil)
    (when (minus TYPE PRED) True))
```


### Why upper-bound type-intersection?

When intersecting/subtracting two-types, a perfect answer is not always possible (e.g. intersecting `Integer` with `Positive`).  
A *lower-bound intersection*, as is performed by the `subsect` utility, is guaranteed to be entirely contained within the true intersection. Namely, the `Never` type is a (trivially) valid lower-bound intersection for *any* pair of types; For `Integer` and `Positive`, it is the *only* valid lower-bound.  
An *upper-bound intersection*, as is performed by the `supersect` utility, is guaranteed to entirely *contain* the true intersection. For our example, both `Integer` and `Positive` would be valid upper-bound intersections, as a type itself is always guaranteed to be at least as large as its intersection with any other type.  

Consider a `positivep` function, with type `(is? T Positive)`. If `is?` used lower-bound intersection, `(positivep Integer)` would incorrectly report to never return `True`, since the lower-bound intersection of `Integer` and `Positive` would be `Never`. For this reason, upper-bound intersection (and for the same reason, subtraction) is necessary to guarantee that type-narrowing does not lie to us.
