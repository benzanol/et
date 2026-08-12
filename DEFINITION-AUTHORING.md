# Agent spawner instructions

## Purpose

The agent spawner divides definition-authoring work into assignments and gives
each authoring agent complete, non-overlapping source/target pairs.

Authoring uses one workflow. Do not schedule separate skeleton and authoring
passes. The authoring agent inventories and authors each file during the same
assignment.

## Assignment requirements

Every assignment must provide:

- The path to `et.el`.
- The path to `et-check.el`.
- The path to this contract.
- The exact Emacs source path for every assigned file.
- The exact target definition path for every assigned file.

Do not require an authoring agent to infer a missing source or target path.

## Batching

Batch small files together so that agents do not repeatedly read the shared
type-system and authoring context.

Build batches by expected work rather than file count:

- Prefer batches containing approximately 30 to 60 eligible declarations in
  total.
- Keep a file with more than approximately 30 eligible declarations in its own
  assignment.
- Keep files with complex macros, special forms, or unusual conditional
  compilation in their own assignment when they require concentrated review.
- Prefer grouping related source files.
- A single source file must **NEVER** be split across multiple batches or
  agents. One agent owns the complete source file and its complete target file.

These are workload guidelines, not correctness requirements. Every file must
still receive its own inventory, verification, rename, and handoff report.

## Ownership and concurrency

- No two active agents may own the same target file.
- One agent may own several complete target files in one batch.
- Each source file has exactly one corresponding definition file.
- Do not split declarations from one source file across definition files.
- Agents working on non-overlapping batches may run concurrently.

## Spawner handoff

Collect a separate completion report for every source/target pair, even when
one agent handled several files. Do not treat a batch-level success statement
as verification that every file was completed.

# Authoring agent instructions

## Purpose

Use this contract when creating type definitions for one or more assigned
Emacs source files.

For each source file, first create an internal inventory of its Lisp-visible
definitions, then author and verify its target definition file. The inventory
is a working checklist, not a skeleton file or a separate pass.

## Required context

Before making changes, read all of:

- `et.el`
- `et-check.el`
- This contract
- Every assigned Emacs source file
- Every assigned target definition file

Read `et.el`, `et-check.el`, and this contract once per assignment. Read each
assigned source file completely before completing its corresponding target.

Use `et.el` and `et-check.el` as the authority for the available type language,
aliases, declaration syntax, checker behavior, and current implementation
capabilities.

## Scope and ownership

- Edit only the assigned target definition files.
- Treat every source/target pair as an independent unit of work.
- Do not move declarations between target files.
- Do not add aliases, checker shortcuts, helper functions, or type-system
  features while authoring definitions.
- Do not fix unrelated problems found in source or definition files.
- Preserve the order of Lisp-visible definitions from each Emacs source.
- Keep declarations for one Emacs source file in one definition file.

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
or `&Cons<K~V>?`, not `String|Nil`, `Nil|String`, or `Nil|&Cons<K~V>`. If
`Nil` is part of a larger inline union, put `?` at the end of the complete
union: write `Cons|String|Vector?`, not `Cons?|String|Vector`. This rule
applies only to inline syntax; list-based syntax may still use forms such as
`(or Nil String)`.

Use `Bool` for a parameter that controls behavior through Lisp truthiness,
where `nil` means false and any non-nil value means true. `Bool` is an alias
for `Any`; its name documents that the unrestricted value is used as a
truthiness flag. Do not use `Boolean` for such parameters. Reserve `Boolean`
for values that must actually be `nil` or `t`, including boolean return types.

## Unified authoring workflow

Complete these steps independently for every assigned source/target pair:

1. Read the complete source file.
2. Create an internal, ordered inventory of every Lisp-visible function,
   macro, and special form that requires an `@def` or `@check` declaration.
3. Exclude definitions with `--` in their name.
4. Record the source parameter names and arity, including `&optional` and
   `&rest` structure.
5. Determine whether the source has useful semantic sections. Follow
   meaningful source groupings and do not invent divisions for small or
   conceptually unified files.
6. Read the relevant declarations, documentation, and implementations before
   authoring their types.
7. Write the target file in the required format and source order.
8. Choose one supported authoring outcome for every inventory entry.
9. Compare the completed target against the inventory so that nothing was
   omitted or duplicated.
10. Perform the static verification for that file.
11. Rename the target according to the file-state rules.

Do not create neutral `AUTHORING STUB` declarations. Do not hand an inventory
to another agent as a separate authoring pass.

For every inventory entry, choose one of these outcomes:

1. Write a complete `@def` declaration.
2. Write the most precise available `@def` approximation containing `Todo`
   only where the current type language is insufficient.
3. Write an authorable checker with `$body` or `$fn`.
4. Write an unauthorable checker as exactly `(@check FUNCTION ($todo))` with
   a concrete blocker explanation.

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

If there are complex relationships between multiple types in a definition,
and other functions have a similar relationship, find that definition and
follow the established precedent. For example, for indexing a string or
buffer, where a marker is a valid index only for a buffer, the precedent is to
use `StringOrBuffer<Idx>` for the string-or-buffer type and `Idx` for the
index type.

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

Every approximated `@def` must have a comment immediately above it that
states:

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

Authoring permits only the existing `$body` and `$fn` shortcuts. Do not write
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
resolve a material ambiguity, leave that target unchanged, stop work on that
file, and request clarification. Continue other files in the batch that are
not affected by the ambiguity.

Unsupported semantics and unclear semantics are different:

- Unsupported but understood semantics receive a documented `Todo` or
  `$todo` approximation.
- Semantics that are not understood remain unchanged until clarified.

## Static verification

Generated files containing `Todo` or `$todo` are intentionally not loadable as
complete definition files. Do not run or load them for verification.

Verify every target independently by inspection:

- Every relevant source definition appears exactly once.
- No ineligible or disabled definition was added.
- No definition was lost while authoring the target.
- Definitions remain in source order.
- Parameter names, optional arguments, rest arguments, and arity agree with
  the source.
- No neutral `AUTHORING STUB` comment remains.
- Every `Todo` has a concrete blocker comment immediately above its `@def`.
- Every `$todo` has a concrete blocker comment immediately above its `@check`.
- Every fully authored declaration agrees with the source behavior.
- Sections and declarations follow the source organization.
- No custom implementation code was added.
- Only assigned target files changed.
- Formatting introduces no whitespace errors.

Failure or ambiguity in one target does not invalidate completed targets in
the same batch.

## Handoff

Report every target file separately, even when the assignment contained a
batch of files.

For each file, report:

- The sections and declaration count
- The declarations completed precisely
- The declarations left with `Todo`
- The checkers implemented with `$body` or `$fn`
- The checkers left with `$todo`
- Definitions excluded because of `--` or disabled conditional compilation
- Any unresolved ambiguity that prevented completion
- The final target filename

Do not perform work outside the assigned targets during handoff.

## Repository structure

### Definition directories

Within `./definitions`, files are separated into these subdirectories:

`c-core/` — C primitives that implement the Lisp runtime and basic editor
model, such as evaluation, objects, buffers, markers, and text properties.

`c-subsystems/` — C primitives for specialized facilities, such as JSON, XML,
SQLite, tree-sitter, networking, images, and sound.

`bundled/` — Lisp files loaded at startup that do not provide a corresponding
feature, such as `subr.el`.

`preload/` — Lisp libraries whose features are already loaded in a clean
`emacs -Q`.

`included-core/` — Non-preloaded features providing general-purpose Lisp
programming utilities.

`included-libraries/` — Non-preloaded features serving a specific format,
service, editor facility, or application domain.

`packages/` — Packages not included with Emacs that can be downloaded from
package repositories such as ELPA or MELPA.

### File names

Files awaiting unified authoring are prefixed with `0:`.

After authoring:

- Rename directly from `0:NAME` to `2:NAME` when authoring new files
- Do not create new `1:` files.

Existing `1:` skeleton files are legacy inputs. Complete their authoring using
the unified workflow, then rename them to `2:`
Never rename a file to contain no prefix. The user must do that.
