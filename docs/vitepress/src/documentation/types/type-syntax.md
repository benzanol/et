---
title: "Type syntax"
---

# Type syntax

There are two mutually compatible syntaxes for defining types:  
**List syntax:** `(Cons (Vector Integer) String)`  
**Symbol syntax:** `Cons<Vector<Integer>_String>`  
Symbol syntax can be nested inside of list syntax:  

```emacs-lisp
(Cons Vector<Integer> String)
```

But not the other way around:  

```emacs-lisp
Cons<(Vector Integer)_String> ; INVALID
```
