---
title: "Scalar types"
---

# Scalar types

The typical scalar types `String`, `Number`, and `Symbol` behave as expected for describing simple datatypes.  

```emacs-lisp
(et-is? 4 Number) ; t
(et-is? "foo" String) ; t
(et-is? 'bar Symbol) ; t
```

The numeric types `Integer`, `Positive`, and `Negative` exist as subtypes of `Number`.  

```emacs-lisp
(et-is? 4 Integer) ; t
(et-is? 4.0 Integer) ; nil
(et-is? 4.5 Integer) ; nil
(et-is? 1 Positive) ; t
(et-is? -1 Negative) ; t
(et-is? 0 Positive) ; t
(et-is? 0 Negative) ; t
```

The types `NonNilSymbol` (all symbols except `nil`) and `Var` (all symbols except `nil` and `t`) exist as subtypes of `Symbol`.  

```emacs-lisp
(et-is? nil NonNilSymbol) ; nil
(et-is? nil Var) ; nil
(et-is? t Var) ; nil
```

There are also a host of types representing internal emacs objects, such as `Buffer`, `Window`, `Frame`, `Overlay`, etc.  

```emacs-lisp
(et-is? (current-buffer) Buffer) ; t
(et-is? (selected-window) Window) ; t
(et-is? (selected-frame) Frame) ; t
```

The `Any` type matches any value, and the `NonNil` type matches any value *except* for `nil`.  

```emacs-lisp
(et-is? 123 NonNil) ; t
(et-is? 'foo NonNil) ; t
(et-is? (current-buffer) NonNil) ; t
(et-is? nil NonNil) ; nil
(et-is? nil Any) ; t
```
