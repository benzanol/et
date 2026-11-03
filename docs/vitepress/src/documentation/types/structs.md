---
title: "Structs"
---

# Structs

When using `cl-defstruct`, the `:et` slot-property can be used to indicate the type of a struct slots. Slots without an `:et` property are given the `Any` type.  

```emacs-lisp
(cl-defstruct my-person
  (name "" :et String)
  (age 0 :et Integer))
```

The type `(Struct my-person)` refers to instances of the `my-person` struct. The shorthand `*my-person` is equivalent, however the longer form may still need to be used for structs whose names contain characters which break et's parsing.  

From a struct definition, the type-checker will generate definitions for the struct's constructor, predicate, copier, and accessors.  

```emacs-lisp
(make-my-person :name "John" :age 35) ; => *my-person
(my-person-name {*my-person}) ; => String
(copy-my-person {*my-person}) ; => *my-person
(my-person-p {x : *my-person|Other}) ; => t{x : *my-person} | nil{x : Other}
```

For more information on the predicate return type, see [Narrowing types](/documentation/types/narrowing-types).  


## Generic structs

To make a struct type generic, add an `:et-generics` property to the first slot.  

```emacs-lisp
(cl-defstruct my-pair
  (first nil :et-generics [T] :et T)
  (second nil :et T))
```

The type `(Struct my-pair Integer)`, or equivalently `*my-pair<Integer>`, represents a `my-pair` struct with `T=Integer`.  
Generic constraints use the same syntax as aliases and polymorphic functions:  

```emacs-lisp
(cl-defstruct my-numeric-pair
  (first 0 :et-generics [(T <: Number)] :et T)
  (second 0 :et T))
```

Struct type arguments are invariant, since struct slots can be mutated. Thus, `*box<Integer>` is not a subtype of `*box<Number>`.
