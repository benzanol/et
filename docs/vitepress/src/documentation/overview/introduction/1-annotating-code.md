---
title: "1. Annotating code"
---

# 1. Annotating code

There are two ways to add type-annotations to your code: declare forms, and `et-annotate`.  


## Declare annotations

Type annotations can be defined using standard elisp `declare` forms, without the need for any additional dependencies.  

```emacs-lisp
(defun add-numbers (n m)
  (declare (et (n Number) (m Number) (@return Number)))
  (+ n m))
```

-   The only requirement for a function annotation is the `@return` type.
-   Any parameter types which are ommitted will default to the `Any` type.
-   Adding `(@skip)` will tell the type-checker not to type-check the body of this function.

A top-level declare form can be used to define types for global variables.  

```emacs-lisp
(declare
 (et (@variable my-number Number)
     (@variable my-local-number Number)))

(defvar my-number 4)
(defvar-local my-local-number 4)
```


## et-annotate

`et-annotate.el` is a tiny package which defines several useful code-annotation macros.  

```emacs-lisp
(et-defun add-numbers (n: Number m: Number) Number
  (+ n m))

;; vvv  expands to  vvv

(cl-defun add-numbers (n m)
  (declare (et (n Number) (m Number) (@return Number)))
  (+ n m))
```

For global variables:  

```emacs-lisp
(et-defvar my-number Number 4)
(et-defvar-local my-local-number Number 4)

;; vvv  expands to  vvv

(defvar my-number 4)
(defvar-local my-local-number 4)
```

I will use the `et-annotate` syntax throught this documentation, however it is completely optional, and all of `et`'s features are available without the extra dependency.
