---
title: "Other container types"
---

# Other container types

The `Vector` and `HashTable` types behave very similarly to `Cons`, with read-only variants.  

```emacs-lisp
(et-subtype? (et HashTable<Integer_Integer>) (et HashTable<Integer_Number>)) ; nil
(et-subtype? (et &HashTable<Integer_Integer>) (et &HashTable<Integer_Number>)) ; t
(et-subtype? (et Vector<Integer>) (et Vector<Number>)) ; nil
(et-subtype? (et &Vector<Integer>) (et &Vector<Number>)) ; t
```
