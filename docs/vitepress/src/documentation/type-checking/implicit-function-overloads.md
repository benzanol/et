---
title: "Implicit function overloads"
---

# Implicit function overloads

Many languages allow for function overloading, where the same function has multiple signatures. Elisp lacks overloads feature, but many functions still exhibit overload-like behavior: their return types vary based on optional flags, argument types, etc.  
For example, a (somewhat) common pattern in elisp is for a function to provide a `noerror` flag. If this flag is non-nil, then the function will return the value `nil` instead of throwing an error if it fails. This can be expressed by the typesystem as follows:  

```emacs-lisp
(et-defun my-string-search (pattern: String &optional noerror: [N])
          (is-nil? N String String?)
  (let* ((result (search-for pattern))) ; => result : String?
    (if noerror
        result ; => String?
      (or result (error "No result found!"))))) ; => String

(my-string-search "foo") ; => String
(my-string-search "foo" nil) ; => String
(my-string-search "foo" t) ; => String?
(my-string-search "foo" {Any}) ; => String?
```

This says that if `noerror` is *definitely* nil, then the function definitely returns a non-nil `String`. If there is even a slight chance that `noerror` is non-nil, then it returns `String?`.  

How can the typesystem possibly verify that the body of the function conforms to this complex set of rules? The answer is *implicit overloads*.  
Upon seeing `(is-nil? N String String?)`, the typesystem implicitly generates two overloads for `my-string-search`:  

```emacs-lisp
1. (pattern: String &optional noerror: [N <: Nil]) -> String
2. (pattern: String &optional noerror: [N !<: Nil]) -> String?
```

In the first overload, `N` is constrained to be a subtype of `Nil`. With this constraint, the return type of the function collapses to just `String`. In the second overload, `N` is constrained to NOT be a subtype of `Nil`, in which case the return type collapses to `String?`.  
The function is then type-checked twice, once for each overload. For overload 1, the type-checker determines that `noerror: Nil`, so only the *else* branch can ever be reached. Thus, the body correctly returns `String`. On the other hand, the condition `[N !<: Nil]` cannot make any guarantees about `noerror`. For example `Any !<: Nil`, in which case `noerror` could be any value. Thus, overload 2 evaluates both branches of the if statement, and finds that the body correctly returns `String?`.  

Another common example of overloaded functions are *arithmetic operations*. These can be represented without overloads as follows:  

```emacs-lisp
(et-defun + (&rest nums: [Ns <: &List<Number>])
          (switch Ns [Nil 0] [&List<Integer> Integer] Number)
  ...)

(+) ; => 0
(+ 1 2) ; => Integer
(+ 1 2.5) ; => Number
```

The `switch` type operation checks `Ns` against each case. If `Ns <: Nil`, it returns `0`. If `Ns <: &List<Integer>`, it returns `Integer`. Otherwise, it returns `Number`.
