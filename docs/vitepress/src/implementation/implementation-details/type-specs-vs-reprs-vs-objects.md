---
title: "Type specs vs reprs vs objects"
---

# Type specs vs reprs vs objects

There are 3 different forms that a type can take.  
A **type-spec** is the human-write/readable format that you will actually use in code.  
A **type-expression** (`et:repr`) is a structural representation of a type-spec after being parsed.  
A **type-object** (`et:type`) is a less-expressive, normalized representation of a type. Type-checking an expression evaluates to an `et:type`.  

|  | spec | et:repr | et:type |
| --- | --- | --- | --- |
| Human readable | [X] | [ ] | [ ] |
| Contains datatypes | [X] | [X] | [X] |
| Contains aliases | [X] | [X] | [X] |
| Contains unions | [X] | [X] | [X] |
| Contains intersections | [X] | [X] | [ ] |
| Contains type-ops | [X] | [X] | [ ] |
| Contains spec-macro calls | [X] | [ ] | [ ] |
| Contains type-variables | [X] | [X] | [ ] |
| Contains variable narrows | [ ] | [ ] | [X] |
