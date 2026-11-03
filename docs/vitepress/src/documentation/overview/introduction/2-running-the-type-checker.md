---
title: "2. Running the type-checker"
---

# 2. Running the type-checker

The type-checker is built in four main layers:  

1.  `et.el` defines the typesystem
2.  `et-check.el` defines how to type-check elisp code
3.  `et-cache.el` caches results of expensive computations to improve the checker's performance
4.  `et-flymake.el` uses flymake to run the type-checker in a background emacs process

To run the checker, clone the `et` repo, then add the following to your config:  

```emacs-lisp
(add-to-list 'load-path "/path/to/et")
(require 'et-flymake)
```

Then, navigate to the buffer you want to type-check and run  

```
M-x et-mode
```

Keep in mind that the checker will only analyze annotated code.
