---
title: "Provided spec macros"
---

# Provided spec macros

## tuple/tuple\*

`(tuple TYPES...)` / `(tuple* TYPES... TAIL)`  

```emacs-lisp
(et (tuple 1 2 3)) ; => (Cons 1 (Cons 2 (Cons 3 Nil)))
(et (&tuple 1 2 3)) ; => (&Cons 1 (&Cons 2 (&Cons 3 Nil)))

(et (tuple* 1 2 3)) ; => (Cons 1 (Cons 2 3))
(et (&tuple* 1 2 3)) ; => (&Cons 1 (&Cons 2 3))
```


## args

`(args SPECS...)`  

```emacs-lisp
(et (args A B C)) ; => &(A B C)

(et (args Integer &optional String &rest Any))
;; => &(Integer . Nil | &(String . Any))

(et (args String &key :flag Boolean :count Integer))
;; => &(String . &Plist<:flag Boolean, :count Integer>)
```


## fn

`(fn [GENERICS?] ARGS RETURN)`  

```emacs-lisp
(et (fn (args Integer String) Boolean))
;; => (Function &(Integer String) Boolean)
(et (fn [T] (args T T T) List<T>))
;; => (PolyFunction &(T T T) List<T>)
```


## fnN

`(fnN ARGS... RETURN?)`  

The explicit `return` type can be omitted, in which case the return type will default to `Any`.  

```emacs-lisp
(et (fn0 Integer)) ; => (Function Nil Integer)
(et (fn1 4 Integer)) ; => (Function &(4 . Nil) Integer)
(et (fn2 4 3 Integer)) ; => (Function &(4 3) Integer)
(et (fn1 4)) ; => (Function &(4) Any)
```
