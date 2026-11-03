---
title: "Function types"
---

# Function types

Functions in emacs support required, optional, rest parameters, and (with cl-lib) keyed parameters. Rather than building a function type which explicitly supports all of these features, it is much simpler to recognize that in lisp, arguments are passed to functions as a list, and that this list can be represented using the types that we already have.  

For example, consider the function  

```emacs-lisp
(et-defun foo (a: A &optional b: B c: C &rest rest: Rest) Return)
```

Note that for this function to even make sense, the type `Rest` must accept the value `nil`. A valid argument-list for this function is one of the following:  

```emacs-lisp
(&Cons A Nil)
(&Cons A (&Cons B Nil))
(&Cons A (&Cons B (&Cons C Rest)))
```

We can simplify this to get the following type for the argument-list.  

```emacs-lisp
(&Cons A (or Nil (&Cons B (or Nil (&Cons C Rest)))))
```

Now, consider a function using cl-style keyed parameters.  

```emacs-lisp
(cl-defun bar (a: A &key x: X y: Y) Return)
```

The argument-list for this kind of function can be represented using the `Plist` type.  

```emacs-lisp
(&Cons A (&Plist :x X :y Y))
```

Luckily, you will never need to write these out in practice. For defuns, the function type will be generated automatically. For explicitly defining function types, you can use the `args` helper to generate the arglist type.  

```emacs-lisp
(et (fn (args Integer &optional String &rest Any) Symbol))
;; (Function (&Cons Integer (or Nil (&Cons String Any))) Symbol)
(et (fn (args Integer &key :a String?) Symbol))
;; (Function (&Cons Integer (&Plist :a String?)) Symbol)
```

Then, function types can be expressed as follows:  

```emacs-lisp
(et-is? #'foo (fn (args A &optional B C &rest Rest) Return)) ; t
(et-is? #'bar (fn (args A &key :x X :y Y) Return)) ; t
```

Additionally, there are several helpers for compactly defining an `N`-argument function. If any arguments or the return value is omitted, they default to `Any`.  

```emacs-lisp
(et fn0) ; (Function Nil Any)
(et fn0<Integer>) ; (Function Nil Integer)
(et fn3) ; (Function (&Cons Any (&Cons Any (&Cons Any Nil))) Any)
(et fn3<1_2_3>) ; (Function (&Cons 1 (&Cons 2 (&Cons 3 Nil))) Any)
```
