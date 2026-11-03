---
title: "Plist type"
---

# Plist type

The `Plist` type represents an unordered plist, with a different type for each possible key. Similarly to `Cons`, the `Plist` type is read-write, and `&Plist` is the read-only version.  

```emacs-lisp
(et-subtype? (et (Plist :num Integer)) (et (Plist :num Number))) ; nil
(et-subtype? (et (&Plist :num Integer)) (et (&Plist :num Number))) ; t
```

If `nil` is a valid value for a particular key's type, then that key is optional.  

```emacs-lisp
(et-is? '(:a 4) (&Plist :a Integer :b Integer)) ; nil
(et-is? '(:a 4) (&Plist :a Integer :b Integer?)) ; t
(et-is? nil (&Plist :a Integer? :b Integer?)) ; t
```
