---
title: "Goals of the typesystem"
---

# Goals of the typesystem

The typesystem's design is driven by the following 3 goals:  

1.  **Correctness:** A successful type-check should be considered a proof of correctness, except for when the user explicitly opts out of this behavior
2.  **Unobtrusiveness:** The typesystem should stay out of the user's way as much as possible. It should *not*:  
    -   Force the user to change how they write code
    -   Fill the user's code with unnecessary type-annotations
    -   Require excessive effort to get valid code to type-check
3.  **Lisp-centrism:** The typesystem should be designed around Lisp's design and conventions  
    -   Types like lists, trees, alists, etc are all built out of a generalized `Cons` type
    -   Function types are single-argument, where the argument is the entire arglist
    -   Predicates and type-narrowing are first-class citizens


## The invariance problem

A primary goal of the typesystem is *correctness*, which means that if the typesystem tells you a value will have a particular type, it will. This may seem like an obvious property for a typesystem to have, but consider the following snippet in Typescript, which is arguably the most comparable project to this one:  

```typescript
function addAnimals(animals: Animal[]) {
    animals.push(new Cat());
    animals.push(new Elephant());
}

const dogs: Dog[] = [];
addAnimals(dogs); // Dog[] is considered to be a valid Animal[]
// 'dogs' has the type Dog[], but now contains non-Dogs
```

This code will type-check in every version of Typescript. The user has done nothing wrong here, but the typesystem has failed them. The design of the typesystem is heavily motivated by the goal of addressing this problem.
