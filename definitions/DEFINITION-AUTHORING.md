# Authoring definitions

Definition files describe APIs implemented elsewhere; they must not change the
API or the type system. `docs/documentation.org` is the authoritative guide to
et's type language, declaration forms, and checker language. Read it before
authoring definitions. This file contains only repository-specific policy.

## File lifecycle

The numeric filename prefix records review state:

- `0_`: not inventoried
- `1_`: inventoried
- `2_`: authored, awaiting human review
- `3_`: human review in progress
- `4_`: human-approved

Authors may advance a file only from `0_` to `1_` or from `1_` to `2_`.
Prefixes `3_` and `4_` are reserved for the human reviewer.

## Inventory: `0_` to `1_`

Scan the corresponding source file and create a stub for every public
function, macro, special form, and global variable, in source order:

```elisp
(@def function-name)
(@check macro-name)
(@variable variable-name)
```

Exclude names containing `--` and definitions disabled by conditional
compilation. Preserve useful source sections, but do not investigate types or
implementations during inventory. Check that every eligible name occurs
exactly once, then rename the file to `1_NAME`.

## Authoring: `1_` to `2_`

Read the complete source file, its documentation, the inventory, and
`docs/documentation.org`. Use the implementation to resolve anything the
documentation leaves ambiguous. Do not guess.

Replace every stub with the appropriate `@def`, `@check`, or `@variable`:

- Match the source's parameter names, arity, optional/rest/keyword structure,
  evaluation rules, bindings, side effects, and return behavior.
- Write the most precise sound type the current language can express. Preserve
  relationships with generics and type operations rather than widening to
  `Any`; use `Any` only when the value is genuinely unrestricted.
- Use read-only container types for inputs that are only read, mutable types
  for inputs that may be mutated, and fresh types for newly allocated results.
- Use `Bool` for truthiness flags and `Boolean` only for values restricted to
  `nil` and `t`.
- Type macros and special forms with a checker that faithfully models their
  evaluation and binding semantics. Use `$expand` only when expansion itself
  is a faithful model.
- Follow established approved definitions when they express the same semantic
  relationship, but verify them against the source.

If understood behavior cannot be expressed, keep all expressible precision and
put `Todo` only at the blocked type position. If a checker cannot yet be
implemented, use exactly `(@check NAME ($todo))`. In either case, place a
comment immediately above the declaration explaining the unsupported behavior
and the missing capability. `Todo` and `$todo` are deliberate, undefined
review markers—not aliases to define. If the behavior itself remains unclear,
leave the file at `1_` and ask for clarification.

After checking the finished file against the source, rename it to `2_NAME`.

## File structure

Every file begins with:

```elisp
;; -*- lexical-binding: t; -*-
```

Use one `et-declare` form when the source has no useful divisions. Otherwise,
preserve its semantic sections with one `et-declare` per section, sentence-case
`;;;` headings, and `;;; ============================================================`
separators. Keep declarations in source order and comments directly above the
declarations they explain. Do not add unrelated aliases, helpers, or fixes.

## Final check

Before advancing a file, verify that:

- every eligible source definition appears exactly once and no ineligible one
  was added;
- all signatures and behavior agree with the source;
- every `Todo` or `$todo` has a specific blocker comment;
- declarations are ordered, grouped, and formatted consistently; and
- only the intended definition file changed.

Load the file when it contains no review markers. Files containing `Todo` or
`$todo` are intentionally incomplete and should be verified by inspection.
