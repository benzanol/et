---
title: "Macro checkers"
---

# Macro checkers

Type-checking macros is substantially different than type-checking functions. While a function type models a *type transformation*, a macro checker must instead model *control-flow*.  
For this, we introduce "checker expressions". A checker expression is built out of building blocks, each representing a common control-flow pattern.  


## $expand

Many simple macros can get away with just using the `$expand` checker. This tells the type-checker to expand the macro, and then type-check the resulting expression, instead of trying to type-check the original macro invocation.  

```emacs-lisp
(defmacro my-increment (place)
  (et ($expand))
  `(setf ,place (1+ ,place)))
```


## $body

Many macros take a fixed number of positional arguments (zero or more), and the remaining arguments are expressions to be evaluated. The `$body` checker can be used in this case:  

```emacs-lisp
(defmacro save-excursion (&rest body)
  (et ($body))
  ...)
(defmacro with-current-buffer (buf &rest body)
  (et ($body Buffer))
  ...)
```


## Some utility checkers

The following can be used as building blocks for defining more complex checkers:  

-   `($at PATH...)` traverse the current expression using `PATH`, and type-check that sub-expression.
-   `($tail PATH...)` traverse the current expression using `PATH`, and type-check that sub-expression and all remaining sub-expressions of its parent. Return the last type.
-   `($tail-tuple PATH...)` like `$tail`, but return a tuple type containing the types of all of the tail entries.
-   `($exp EXPR)` type-check the expression `EXPR`.
-   `($exps EXPRS)` type-check the expressions `EXPRS`, returning the type of the last one.
-   `($type TYPE-SPEC)` do nothing and return the type `TYPE-SPEC`.
-   `($nil)` do nothing and return the type `Nil`.
-   `($progn CHECKERS...)` perform the checks specified by `CHECKERS`, and return the last one.
-   `($prog1 CHECKERS...)` perform the checks specified by `CHECKERS`, and return the first one.
-   `($ignore CHECKERS...)` perform the checks specified by `CHECKERS`, and return `Nil`.
-   `($with-rec REC-CHECKER CHECKER)` perform the check specified by `REC-CHECKER`, and set the current type-recommendation to its return type. Then, perform the checks specified by `CHECKER`.
-   `($expect EXPECTED-CHECKER CHECKER)` similar to `$with-rec`, but throw an error if the type returned by `CHECKER` is not a subtype of the expected type.

These checkers all implement fairly linear control-flow. There are numerous other checkers for checking non-linear control flow, such as `$branches`, `$loop`, `$if`, and many others. This documentation needs to be expanded to contain in-depth explanations of each of these checkers.  


## Pattern matching

The `$pcase` checker can be used to match the macro's arguments, and route to different checkers based on the matched branch. The following is the real checker for `let*`:  

```emacs-lisp
($pcase
 `(() . ,_body) ($tail 2)
 `((,first . ,rest) . ,body)
 ($exp `(let (,first) (let* ,rest ,@body))))
```

Here, `($tail 2)` type-checks the expression's tail, starting at index 2. For `let*`, this is the body. `($exp (let (,first) ...))` type-checks a `let` expression containing just the first binding, whose body recursively references `let*`. This will recurse until each binding of the original `let*` has been processed individually within its own `let`.  

Alternatively, the `$arglist` checker destructures the arguments like a function arglist.  

```emacs-lisp
(defmacro my-increment (place)
  (declare
   (et ($arglist (p) ($exp `(setf ,p (1+ ,p))))))
  ...)
```

This checker tells the type-checker that when it encounters `(my-increment PLACE)`, it should instead type-check the expression `(setf PLACE (1+ PLACE))`. It is often easier to describe how to check a macro in terms of an equivalent expression rather than trying to describe its functionality from the ground up.
