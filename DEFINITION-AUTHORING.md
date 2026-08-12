# Definition Authoring Contract

## Purpose

Use this contract when creating type definitions for an Emacs source file.
Each Emacs source file has one corresponding definition file, regardless of
the size of either file. Large definition files are organized into semantic
sections within that single file.

Generation has two distinct passes:

1. A file-skeleton pass enumerates the source file without judging which
   declarations can be completed.
2. A file-authoring pass replaces every stub in the file with a precise
   declaration or documented approximation.

Do not combine these passes.

## Required context

Before making changes, read all of:

- `et.el`
- `et-check.el`
- The assigned Emacs source file
- This contract

Use `et.el` and `et-check.el` as the authority for the available type language,
aliases, declaration syntax, checker behavior, and current implementation
capabilities.

The task must provide both the Emacs source path and the target definition
path. Do not infer a missing path.

## Scope and ownership

- Edit only the assigned target definition file.
- Do not add aliases, checker shortcuts, helper functions, or type-system
  features while authoring definitions.
- Do not fix unrelated problems found in the source or definition files.
- Preserve the order of Lisp-visible definitions from the Emacs source.
- Keep declarations for one Emacs source file in one definition file.

One authoring agent owns the entire target definition file. Agents working on
different target files may run concurrently.

## Target file format

The first line of every target definition file must be exactly:

```elisp
;; -*- lexical-binding: t; -*-
```

When the source has no useful semantic divisions, do not create artificial
sections. Put every declaration in one `et-declare` form spanning the file.

When the source has distinct semantic sections:

- Give every section its own `et-declare` form.
- Write each section heading as `;;; Section name`.
- Use sentence case for section headings: capitalize only the first letter of
  the heading. Do not use title case or all caps.
- Write `;;; ============================================================`
  before the first section, between consecutive sections, and after the last
  section.

The structure in the target definition file is:

```elisp
;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; First section

(et-declare
 ...)

;;; ============================================================
;;; Second section

(et-declare
 ...)

;;; ============================================================
```

Within one `et-declare` form, place definitions directly next to each other.
Do not put blank lines between definitions in the same `et-declare` form.
Keep a definition's explanatory comment directly above that definition, also
without a blank separator line.

## Type syntax

Do not nest list-based type syntax inside arrow-bracket type syntax. Once a
type expression uses arrow brackets, write nested type constructors with arrow
brackets too:

```elisp
List<Cons<Integer~Integer>>
```

Do not write `List<(Cons Integer Integer)>`.

For an inline union with `Nil`, use the optional `?` suffix. Write `String?`
or `&Cons<K~V>?`, not `String|Nil`, `Nil|String`, or
`Nil|&Cons<K~V>`. If `Nil` is part of a larger inline union, put `?` at the
end of the complete union: write `Cons|String|Vector?`, not
`Cons?|String|Vector`. This rule applies only to inline syntax; list-based
syntax may still use forms such as `(or Nil String)`.

Use `Bool` for a parameter that controls behavior through Lisp truthiness,
where `nil` means false and any non-nil value means true. `Bool` is an alias
for `Any`; its name documents that the unrestricted value is used as a
truthiness flag. Do not use `Boolean` for such parameters. Reserve `Boolean`
for values that must actually be `nil` or `t`, including boolean return types.

## Pass 1: File skeleton

The skeleton agent owns the entire new target definition file. Its task is
mechanical enumeration and organization, not type authoring.

### Skeleton procedure

1. Read the complete assigned source file.
2. Identify every Lisp-visible function, macro, and special form that requires
   an `@def` or `@check` declaration.
3. Determine whether the source has useful semantic sections. Follow
   meaningful groupings in the source when they exist; do not divide a small
   or conceptually unified file merely because sections are supported.
4. Format the target according to `Target file format`, using either one
   file-wide `et-declare` form or one `et-declare` form per section.
5. Add an unclassified stub for every identified definition.
6. Compare the completed skeleton against the source so that nothing was
   omitted or duplicated.

Do not classify any entry as authorable or unauthorable during this pass, even
when the answer appears obvious.

### Function stubs

In the target definition file, preserve the source parameter names and arity,
including `&optional` and `&rest`, but use `Todo` for every parameter and return
type:

```elisp
;; AUTHORING STUB: not yet classified.
(@def FUNCTION (ARG: Todo &optional OTHER: Todo) Todo)
```

Arity and argument structure are source facts, not authoring decisions. Do not
add generics or infer type relationships during the skeleton pass.

### Checker stubs

In the target definition file, use:

```elisp
;; AUTHORING STUB: not yet classified.
(@check FUNCTION ($todo))
```

The skeleton agent must not replace `$todo` with `$body`, `$fn`, or a custom
checker.

### Skeleton completion conditions

- Every relevant Lisp-visible definition has exactly one stub.
- Every stub has the exact `AUTHORING STUB` comment immediately above it.
- Parameter names, optional arguments, rest arguments, and source order agree
  with the Emacs source.
- No declaration contains an authored type.
- No authorability or blocker claims have been made.

## Pass 2: File authoring

A file-authoring agent owns the entire existing skeleton file. It must read the
relevant source declarations, documentation, and implementations before
authoring their types.

The agent must preserve the skeleton's headings, section boundaries, and
declaration order. It must not add supporting implementation outside the
target definition file.

For every stub in the file, the agent must choose one of these outcomes:

1. Replace it with a complete `@def` declaration.
2. Replace it with the most precise available `@def` approximation containing
   `Todo` only where the current type language is insufficient.
3. Replace an authorable checker with `$body` or `$fn`.
4. Leave an unauthorable checker as exactly `(@check FUNCTION ($todo))` and
   replace the neutral stub comment with a concrete blocker explanation.

No `AUTHORING STUB` comment may remain when the file is complete.

## Authoring `@def` declarations

A function is authorable when its useful parameter types, return type, and
relationships can be expressed declaratively using the facilities already
implemented in `et.el` and `et-check.el`.

Generics, constraints, unions, function types, existing aliases, and existing
type-spec operations are allowed. Complexity in the source implementation is
not itself a reason to approximate a declaration. The deciding question is
whether the function's type semantics can be expressed accurately with the
existing declaration language and without new supporting code.

An `@def` requires approximation when an accurate declaration depends on a
relationship that the current type language cannot express, including:

- Freshening or preservation of object identity
- Shallow, deep, or selective copying relationships
- Mixed sharing and freshness within one result
- Mutation or writeability relationships not represented by existing types
- Returning a particular existing substructure or argument
- Container-kind or element-type relationships not represented by existing
  types
- Value-dependent arity or value-dependent result structure
- Callback application relationships not expressible by existing function
  types

This list describes common blockers, not an automatic rejection rule. Use an
existing facility when it expresses the behavior accurately.

### Format-string functions

Do not encode or verify how many data arguments are required by directives in
a format string. Declare variadic data arguments to format-string functions as
an unconstrained rest list (`&List`). A format-dependent argument count is not
an authoring blocker and must not cause the declaration to contain `Todo`.

### Approximation rules

- Preserve every part of the declaration that can be expressed accurately.
- Put `Todo` at the narrowest type position requiring a future capability.
- Do not replace an accurately known type with `Any` merely to make the
  declaration loadable.
- Do not define `Todo`. It is intentionally nonexistent and marks unfinished
  type semantics.
- Do not invent a new alias or helper to avoid using `Todo`.
- Do not claim unsupported precision.

In the target definition file, every approximated `@def` must have a comment
immediately above it that states:

1. The semantic relationship that cannot currently be represented.
2. The missing type-system capability needed to replace `Todo`.

For example:

```elisp
;; The result preserves the input elements but requires a fresh outer
;; structure. The type language cannot yet express shallow freshening.
(@def FUNCTION (input: ExistingType) Todo)
```

Do not use comments such as "too complex", "needs custom handling", or
"improve later" without describing the actual missing relationship.

## Authoring `@check` declarations

This phase permits only the existing `$body` and `$fn` shortcuts. Do not write
a custom checker, even when one can be designed from the current primitives.

Use `$body` only when the complete checker behavior is:

- Check a fixed sequence of leading operands against the supplied types.
- Check every remaining operand as a body.
- Return the type of the body's final expression.

Use `$fn` only when the complete checker behavior is:

- Check fixed positional operands against the supplied types.
- Return a fixed declared type independent of operand values and types.
- Require no binding, narrowing, branching, looping, assignment, place
  handling, nonlocal control flow, or custom evaluation behavior.

If neither shortcut fully models the form, use exactly:

```elisp
;; Explain the form-specific behavior that requires a future checker.
(@check FUNCTION ($todo))
```

The comment immediately above `$todo` must identify the unsupported checker
behavior and the capability a future checker must provide. Do not implement
that capability during definition authoring.

Do not define `$todo` or replace it with a superficially similar shortcut.

## Ambiguity

Read the source implementation when the declaration or documentation does not
determine the type semantics. Do not guess. If the source still does not
resolve a material ambiguity, leave the existing stub unchanged, stop work on
the file, and request clarification.

Unsupported semantics and unclear semantics are different:

- Unsupported but understood semantics receive a documented `Todo` or
  `$todo` approximation.
- Semantics that are not understood remain unclassified until clarified.

## Static verification

Generated files containing `Todo` are intentionally not loadable as complete
definition files. Do not run or load them for verification.

For a completed file, verify by inspection that:

- No `AUTHORING STUB` comment remains.
- Every remaining `Todo` has a concrete blocker comment immediately above its
  `@def`.
- Every remaining `$todo` has a concrete blocker comment immediately above its
  `@check`.
- Every fully authored declaration agrees with the source arity and behavior.
- Only the assigned target file changed.
- Formatting introduces no whitespace errors.

The file-authoring agent must also verify that:

- Every relevant source definition appears exactly once.
- No definition was lost while replacing stubs.
- Sections and declarations remain in source order.
- No custom implementation code was added.
- Neither existing definition file was modified.

## Handoff

The skeleton agent reports the sections it created and the number of stubs in
each section.

Each file-authoring agent reports:

- The declarations completed precisely
- The declarations left with `Todo`
- The checkers implemented with `$body` or `$fn`
- The checkers left with `$todo`
- Any unresolved ambiguity that prevented file completion

Do not perform work outside the assigned pass or target file during handoff.
## Directories
### Definition directories
Within `./definitions`, files are separated into one of the following sub-directories:

`c-core/` — C primitives that implement the Lisp runtime and basic editor model, such as evaluation,
objects, buffers, markers, and text properties.
`c-subsystems/` — C primitives for specialized facilities, such as JSON, XML, SQLite, tree-sitter,
networking, images, and sound.
`bundled/` — Lisp files loaded at startup that do not provide a corresponding feature, such
as subr.el.
`preload/` — Lisp libraries whose features are already loaded in a clean emacs -Q.
`included-core/` — Non-preloaded features providing general-purpose Lisp programming utilities.
`included-libraries/` — Non-preloaded features serving a specific format, service, editor facility, or application domain.
`packages/` - Packages not included with emacs, but that can be downloaded from package repositories such as `elpa` or `melpa`

Empty files should be prefixed with `0:`.
Files that have had pass-1 done but not pass-2 should be prefixed with `1:`
Files that have had pass-2 done but still contain todos should be prefixed with `2:`
