---
title: "Pre-processing"
---

# Pre-processing

As with all type-checkers, multiple passes must be performed to ensure that all types are defined, and all functions/variables are given their types, before type-checking actually begins. `et` performs 3 distinct phases:  
**1. Identification** - Identify all of the expressions that need to be pre-processed  
**2. Pre-processing** - Pre-process all of the identified expressions  
**3. Checking** - Perform type-checking  

**Identification** is performed by looping through all sexps in the current buffer (and all required buffers), checking the `car` of the expression for an *identifier function*, and calling that identifier function. An identifier function returns a plist which describes how to pre-process the expression.  
To add an identifier to a symbol, use `et-set-identifier`:  

```emacs-lisp
(et-set-identifier #'defun #'et:identify-defun)
```
