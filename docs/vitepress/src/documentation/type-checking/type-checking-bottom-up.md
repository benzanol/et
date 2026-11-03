---
title: "Type-checking (bottom-up)"
---

# Type-checking (bottom-up)

Expression checking is primarily done bottom-up. That is, the most deeply nested expressions are evaluated first, and their types are propagated upward in order to determine correctness.  

```emacs-lisp
(string-width (concat "a" "b"))
```

In this example, the first expressions evaluated are `"a"` and `"b"` , which each return literal types corresponding to their exact values.  
The next expression evaluated is `(concat "a" "b")`. The arglist type `&("a" "b")` is matched against the function type `&List<String> -> String`, determining that the expression returns `String`.  
Finally, the top-level expression is evaluated. The arglist type `&(String)` is matched against the function type `&(String) -> Integer`, determining that the expression returns `Integer`.
