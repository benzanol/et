# Agent spawner instructions

## Purpose

The agent spawner divides definition-authoring work into assignments and gives
each authoring agent complete, non-overlapping source/target pairs.

## Assignment requirements

Every assignment must provide:

- The path to this contract.
- The exact Emacs source path for every assigned file.
- The exact target definition path for every assigned file.

Do not require an authoring agent to infer a missing source or target path.

## Batching

Batch small files together so that agents do not repeatedly read the shared
type-system and authoring context.

Build batches by expected work rather than file count:

- **Inventory phase:** batch liberally. Each file takes very little work.
  50–100 source files per agent is fine.
- **Authoring phase:** prefer batches containing approximately 20–40 eligible
  declarations in total. Prefer grouping related source files.
  A single source file should be split across multiple batches or
  agents if it is large enough.

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


# Phase 1: Inventory (creating `1:` files)

## Purpose

Quickly scan each assigned source file and produce a skeleton `1:` file that
lists every Lisp-visible function, macro, and special form. The skeleton
contains only names and section structure — no parameter lists, no types, no
return types.

This phase is meant to be fast and shallow. Do not read implementations. Do
not determine types. Just identify what exists and where it belongs.

## Required context

- This contract (the inventory section only — skip the type reference).
- Each assigned source file.

## What to produce

For each source file, create a `1:` target file containing:

- An `@def` stub for every Lisp-visible function (just the name, nothing
  else).
- An `@check` stub for every Lisp-visible macro or special form (just the
  name, nothing else).
- Exclude definitions with `--` in their name.
- Preserve the order of definitions from the source.

### Stub format

```elisp
(@def function-name)
(@check macro-name)
```

No parenthesized parameter list. No return type. Just the declaration keyword
and the name.

### Section structure

Emacs source files often have `;;; Section name` headers that divide the
file into semantic groups. Look for these headers while scanning. When the
source has distinct semantic sections, reproduce them in the target using
the section format described in the Phase 2 target file format section:
separate `et-declare` forms, `;;; Section name` headings, and
`;;; ============================================================` separators.

When the source has no section headers, use a single `et-declare` form.

Do not skip this step. A flat list of 100+ stubs in one `et-declare` form
is wrong when the source is organized into sections.

### Example — unsectioned source

For a source file with no section headers, containing `make-marker`,
`set-marker`, `marker-position`, `marker-buffer`, `marker-insertion-type`,
`set-marker-insertion-type`, and the internal `marker--make`:

```elisp
;; -*- lexical-binding: t; -*-

(et-declare
 (@def make-marker)
 (@def set-marker)
 (@def marker-position)
 (@def marker-buffer)
 (@def marker-insertion-type)
 (@def set-marker-insertion-type))
```

(`marker--make` is excluded because it contains `--`.)

### Example — sectioned source

For a source file with `;;; Creation` and `;;; Queries` section headers:

```elisp
;; -*- lexical-binding: t; -*-

;;; ============================================================
;;; Creation

(et-declare
 (@def make-marker)
 (@def set-marker))

;;; ============================================================
;;; Queries

(et-declare
 (@def marker-position)
 (@def marker-buffer)
 (@def marker-insertion-type))

;;; ============================================================
```

## File naming

Name the output file with the `1:` prefix: `1:NAME`.

## Verification

Skim the source once more to confirm every eligible name appears and none
were duplicated. This should take seconds, not minutes.

## Handoff

For each file, report:

- The declaration count.
- The final target filename.


# Phase 2: Authoring (converting `1:` files to `2:` files)

## Purpose

Take an existing `1:` skeleton file and fill in the complete type declarations
for every stub, producing a `2:` file.

## Required context

Before making changes, read all of:

- This contract (especially the type reference below).
- Every assigned Emacs source file.
- Every assigned `1:` target file.

Read this contract once per assignment. Read each assigned source file
completely before completing its corresponding target.

Do **not** read `et.el` or `et-check.el`. Everything you need to know about
the type language is documented in this contract.

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


# Type language reference

This section is the complete reference for the type language used in
definition files. Do not read `et.el` or `et-check.el` for type information;
use this section instead.

## Primitive datatypes

These are the built-in datatypes. Each has fixed variance for its arguments.

| Datatype | Arguments | Description |
|----------|-----------|-------------|
| `Any` | none | Top type, matches everything |
| `Nil` | none | The value `nil` only (spec macro, expands to `Literal nil`) |
| `True` | none | The value `t` only (spec macro, expands to `Literal t`) |
| `Never` | none | Bottom type, matches nothing |
| `Literal` | `VALUE` (const) | Exactly the value VALUE |
| `NonNil` | none | Any non-nil value |
| `Symbol` | none | Any symbol (including nil) |
| `NonNilSymbol` | none | Any symbol except nil |
| `Var` | none | Any symbol except nil and t |
| `Number` | none | Any number |
| `Integer` | none | Any integer |
| `Positive` | none | Any positive number |
| `Negative` | none | Any negative number |
| `String` | none | Any string |
| `Boolean` | none | Alias for `Nil\|True` (exactly nil or t) |
| `Bool` | none | Alias for `Any` (documents truthiness-flag usage) |

### Cons cells

| Datatype | Arguments (variance) | Description |
|----------|---------------------|-------------|
| `ConsRW` | `CAR-R(co) CAR-W(contra) CDR-R(co) CDR-W(contra)` | Mutable cons cell with separate read/write types |
| `ConsFresh` | `CAR(co) CDR(co)` | Freshly created cons cell (can be widened to ConsRW) |

In practice, use the `Cons` alias instead of `ConsRW` directly:

- `Cons<L~R>` — a cons cell readable and writable as `L` and `R`
  (expands to `ConsRW<L~L~R~R>`)

### Vectors

| Datatype | Arguments (variance) | Description |
|----------|---------------------|-------------|
| `VectorRW` | `ELEM-R(co) ELEM-W(contra)` | Mutable vector |
| `VectorFresh` | `ELEM(co)` | Freshly created vector |

Use the `Vector` alias: `Vector<E>` expands to `VectorRW<E~E>`.

### Hash tables

| Datatype | Arguments (variance) | Description |
|----------|---------------------|-------------|
| `HashTableRW` | `KEY-R(co) KEY-W(contra) VAL-R(co) VAL-W(contra)` | Mutable hash table |
| `HashTableFresh` | `KEY(co) VAL(co)` | Freshly created hash table |

Use the `HashTable` alias: `HashTable<K~V>` expands to `HashTableRW<K~K~V~V>`.

### Functions

| Datatype | Arguments (variance) | Description |
|----------|---------------------|-------------|
| `Function` | `INPUT(contra) OUTPUT(co)` | Fixed-signature function |
| `DynFunction` | `MATCHER(const) OUTPUT-REPR(const)` | Generic/dynamic function |

Use the `fn` spec macro (see below) rather than writing these directly.

### Plists

| Datatype | Arguments | Description |
|----------|-----------|-------------|
| `PlistRW` | `KEY1(iso) VAL1-R(co) VAL1-W(contra) ...` triples | Typed property list with known keys |

Use the `Plist` spec macro: `(Plist :key1 Type1 :key2 Type2)`.

### Structs

| Datatype | Arguments | Description |
|----------|-----------|-------------|
| `Struct` | `NAME(const) GENERIC-ARGS(iso)...` | A cl-defstruct instance |

Written as `*struct-name` or `*struct-name<Args...>` in inline syntax.

### Emacs internal types

| Datatype | Arguments | Description |
|----------|-----------|-------------|
| `Emacs` | `TAG(const)` | Non-readable Emacs internal objects |

The following aliases exist for `(Emacs tag)`:

`Buffer`, `Marker`, `Window`, `Frame`, `WindowConfiguration`, `Overlay`,
`Terminal`, `Process`, `Thread`, `Mutex`, `ConditionVariable`,
`FontSpec`, `FontEntity`, `FontObject`, `Xwidget`, `XwidgetView`,
`CharTable`, `BoolVector`, `Obarray`, `Finalizer`,
`InterpretedFunction`, `ByteCodeFunction`, `Subr`

Additional compound aliases:
- `Closure` = `InterpretedFunction | ByteCodeFunction`
- `Font` = `FontSpec | FontEntity | FontObject`
- `IntOrMarker` = `Integer | Marker`
- `NumOrMarker` = `Number | Marker`


## Type alias definitions

When determining the type of a function parameter or return value, first
identify the concrete values the function accepts or returns by reading the
source implementation. Then check whether an existing alias already describes
that concrete type. Do not select aliases by guessing from the alias name;
match the alias definition against the concrete types you identified.

### Foundational aliases

These aliases wrap internal primitive datatypes. Use them as building blocks
without expanding them further.

| Alias | Description |
|-------|-------------|
| `Cons<L~R>` | Mutable cons cell with car type `L` and cdr type `R`. Defaults: `L=Any`, `R=Any` |
| `ListFresh<E>` | Freshly allocated list of `E` (can widen to `List<E>`). Default: `E=Any` |
| `Vector<E>` | Mutable vector with element type `E`. Default: `E=Any` |
| `VectorW<E>` | Write-constraining vector: reads as `Any`, writes require `E` |
| `HashTable<K~V>` | Mutable hash table. Defaults: `K=Any`, `V=Any` |
| `HashTableW<K~V>` | Write-constraining hash table: reads as `Any`, writes require `K`/`V` |

### Compound alias definitions

In the definitions below, `[(= E Any)]` in a generic vector means the
generic `E` has a default value of `Any`. So `List` with no arguments is
`List<Any>`.

#### List types

```elisp
(et-defalias List [(= E Any)] (or Nil (Cons E (List E))))
(et-defalias Alist [K V] (List (Cons K V)))
(et-defalias Tree [(= E Any)] (or (List (Tree E)) E))
(et-defalias ConsTree [(= E Any)] (or (Cons (ConsTree E) (ConsTree E)) E))
(et-defalias ListWithLast [E Last]
  (or (Cons Last Nil) (Cons E ListWithLast<E~Last>)))
(et-defalias PlistOf [K V] (or Nil (Cons K (Cons V (PlistOf K V)))))
```

`(Plist :key1 Type1 :key2 Type2)` is a spec macro for typed property lists
with known fixed keys.

#### Tuple types (spec macros)

| Macro | Meaning |
|-------|---------|
| `(Tuple A B C)` | `Cons<A~Cons<B~Cons<C~Nil>>>` — fixed-length list |
| `(Tuple* A B C)` | `Cons<A~Cons<B~C>>` — dotted-pair chain |
| `(Args A B C)` | Same as Tuple but uses read-only cons (`&Cons`) — for function arglists |
| `(Args* A B C)` | Same as Tuple* but with `&Cons` |

#### Function types

```elisp
(et-defalias AnyFn [] (Function Never Any))
(et-defalias IdFn [T] (Function T T))
(et-defalias Sink [T] (Function (Args T) Nil))
```

Additional function syntax:

| Syntax | Meaning |
|--------|---------|
| `(fn ARGLIST RET)` | A function with a specific arglist and return type |
| `(fn [GENVEC] ARGLIST RET)` | A generic (dynamic) function |
| `fn` | `(fn Nil Any)` — no args, returns anything |
| `fn<ARGLIST~RET>` | Inline syntax for `(fn ARGLIST RET)` |
| `fn1<ARG>` | One-arg function returning Any |
| `fn1<ARG~RET>` | One-arg function with return type |
| `fn2<ARG1~ARG2>` | Two-arg function returning Any |
| `fn2<ARG1~ARG2~RET>` | Two-arg function with return type |

Arglist types use `(Args ...)` for positional parameters. A `&rest` parameter
is encoded by ending the arglist with `&List<T>` (for a rest of type T) or
just `&List` (for an untyped rest). For example:

- `(fn (Args Integer String) Boolean)` — takes an integer and a string
- `(fn (Args Integer &List<String>) Nil)` — one integer then any number of strings
- `(fn &List Any)` — variadic function (any args, returns anything)

#### Booleans

```elisp
(et-defalias Boolean [] (or Nil True))
(et-defalias Bool [] Any)
```

Use `Bool` for truthiness flags (any non-nil means true, nil means false).
Use `Boolean` for values that must actually be `nil` or `t`.

#### Emacs compound types

```elisp
(et-defalias Closure [] (or InterpretedFunction ByteCodeFunction))
(et-defalias Font [] (or FontSpec FontEntity FontObject))
(et-defalias IntOrMarker [] (or Integer Marker))
(et-defalias NumOrMarker [] (or Number Marker))
(et-defalias ColorTriple [] (Tuple Integer Integer Integer))
```

#### S-expressions

```elisp
(et-defalias Sexp [] (or Symbol String Number (Cons Sexp Sexp) (Vector Sexp)))
(et-defalias Sexps [] List<Sexp>)
```

#### Time types

```elisp
(et-defalias Timestamp []
  (or Nil Number
      (&Cons Integer Integer)
      (&Cons Integer (&Cons Integer Integer))
      (&Tuple Integer Integer)
      (&Tuple Integer Integer Integer)
      (&Tuple Integer Integer Integer Integer)))

(et-defalias TimeOutput []
  (or Integer (Cons Integer Integer)
      (Tuple Integer Integer Integer Integer)))

(et-defalias Timezone []
  (or Nil True @wall String Integer (&Tuple Integer String)))
```

#### Sequence types

These form a hierarchy. Each level includes all types from the level below
plus additional container types. Conditional members are included only when
the element type `E` is compatible.

| Alias | Includes | Condition on extra members |
|-------|----------|---------------------------|
| `ConcatSeq<E>` | `List<E>`, `Vector<E>`, `String` | String only when `E` includes Integer |
| `OrdSeq<E>` | `ConcatSeq<E>`, `BoolVector` | BoolVector only when `E` includes Boolean |
| `EltSeq<E>` | `OrdSeq<E>`, `CharTable` | — |
| `LenSeq<E>` | `EltSeq<E>`, records | — |
| `MapSeq<E>` | `OrdSeq<E>`, `Closure` | — |

All default `E=Any` except `MapSeq` which requires `E`.

Additional array-operation aliases:

| Alias | Description |
|-------|-------------|
| `ArefSeq<E>` | Readable with `aref`, returning `E`: `&Vector<E>`, String, BoolVector, CharTable, Closure, records |
| `AsetSeq<E>` | Writable with `aset`, accepting `E`: `VectorW<E>`, String, BoolVector, CharTable, records |
| `FillableArray<E>` | Fillable with `fillarray`, accepting `E`: `VectorW<E>`, String, CharTable, BoolVector |

#### String and buffer types

`StringOrBuffer<Idx>` accepts `String` or `Buffer`, parameterized by the
index type `Idx` (bounded by `IntOrMarker`). A String accepts only Integer
indices. A Buffer accepts both Integer and Marker indices. Use this for
functions that operate on either a string or buffer with a position argument.

#### Completion types

```elisp
(et-defalias CompletionPredicate [] fn1<String|Symbol>)

(et-defalias CompletionFunction []
  (fn [<= A Boolean|@lambda|@metadata|Cons<@boundaries~String>]
      (Args String CompletionPredicate? A)
      (switch A
              [Nil Boolean|String]
              [True &List<String>]
              [@lambda Bool]
              [@metadata Cons<@metadata~Alist<Symbol~Any>>]
              [Cons<@boundaries~String> Cons<@boundaries~Cons<Integer~Integer>>]
              Any)))

(et-defalias CompletionTable []
  (or List<String> Obarray HashTable<String|Symbol~Any> CompletionFunction))
```

#### Other aliases

```elisp
(et-defalias VariableEvent [] (or @set @let @unlet @makunbound @defvaralias))
(et-defalias VariableWatcher [] (fn (Args Symbol Any VariableEvent Buffer?)))
```


## Inline (arrow-bracket) type syntax

Inline syntax is the string-based syntax used for type expressions. It
supports:

### Basic types

- `Integer`, `String`, `Any`, etc. — type names
- `@symbol-name` — literal symbol (e.g., `@keymap` → `'keymap`)
- `:keyword` — literal keyword symbol (e.g., `:type` → `':type`)
- `%string-value` — literal string
- `42`, `3.14` — literal numbers

### Parameterized types

- `Name<Arg1~Arg2>` — type with arguments, separated by `~`
- `*struct-name` — struct type (e.g., `*et:result`)
- `*struct-name<Arg>` — generic struct

### Unions and intersections

- `A|B|C` — union type (A or B or C)
- `A^B` — intersection type (A and B) — rarely needed

### Optional (nullable)

- `Type?` — shorthand for `Type|Nil`
- Place `?` at the end of the complete union: `A|B|C?` means `A|B|C|Nil`
- You CANNOT add `?` to the end of list syntax: `(List Integer)?` is INVALID.

### Read-only prefix

- `&Type` — read-only version of Type
- `&Cons<A~B>` — read-only cons (cannot be written to)
- `&List<E>` — read-only list
- `&Vector<E>` — read-only vector

### Nesting rule

Do not nest list-based syntax inside arrow-bracket syntax. Once using angle
brackets, stay with angle brackets:

```elisp
;; CORRECT:
List<Cons<Integer~Integer>>

;; WRONG:
List<(Cons Integer Integer)>
```

### Generics in inline syntax

Generic type variables appear as uppercase identifiers. They are declared
in the generic vector `[T]` or with constraints `[(<= T SomeType)]`.


## List-based type syntax

List-based syntax is used for more complex type expressions:

- `(or A B C)` — union
- `(and A B)` — intersection
- `(Cons A B)` — parameterized type
- `(Literal value)` — literal value
- `(Tuple A B C)` — fixed-length list type
- `(Tuple* A B C)` — cons chain ending with C
- `(Args A B C)` — read-only arglist tuple
- `(Plist :key1 Type1 :key2 Type2)` — typed property list
- `(fn ARGLIST RETURN)` — function type
- `(fn [GENVEC] ARGLIST RETURN)` — generic function type
- `(read-only TYPE)` — read-only wrapper


## Type operators (for return types)

These are used in return-type positions to express type-level computation:

| Operator | Syntax | Meaning |
|----------|--------|---------|
| `extends?` | `(extends? SUB SUPER YES NO)` | If SUB <: SUPER then YES else NO |
| `if-nil?` | `(if-nil? T YES NO)` | If T is always nil then YES else NO |
| `is?` | `(is? T TYPE)` | Returns `True` if T is TYPE, `Nil` if not, `Boolean` if maybe |
| `is-a?` | `(is-a? T TYPE)` | Like `is?` but nil case doesn't guarantee value ISN'T TYPE |
| `isnt?` | `(isnt? T TYPE)` | Returns `True` if T is not TYPE |
| `freshen-shallow` | `(freshen-shallow T)` | Make the outermost container fresh |
| `freshen-deep` | `(freshen-deep T)` | Make all containers fresh recursively |
| `is-non-nil?` | `(is-non-nil? B T1 T2)` | If B is non-nil, T1; else T2 |
| `switch` | `(switch T [PAT1 OUT1] [PAT2 OUT2] ... DEFAULT)` | Pattern-match on type |


## Declaration forms

### `@def` — Function type declaration

```elisp
(@def FUNCTION-NAME (PARAMS...) RETURN-TYPE)
(@def FUNCTION-NAME (PARAMS...) RETURN-TYPE EXTRA-DECLARES...)
```

Parameters use colon syntax: `name: Type`. Special parameter markers:

| Marker | Meaning |
|--------|---------|
| `name: Type` | Positional parameter with type |
| `&optional` | Following parameters are optional |
| `&rest name: Type` | Rest parameter |
| `&key` | Following parameters are keyword arguments (converted to plist) |
| `[T]` or `[(<= T Bound)]` | Generic vector (before params) |

When a parameter IS the generic itself, use the shorthand:

```elisp
;; Short form — arg IS the generic:
(@def set-marker-insertion-type (marker: Marker type: [T]) T)

;; Equivalent long form:
(@def set-marker-insertion-type ([T] marker: Marker type: T) T)
```

This shorthand only works when the type of the parameter is exactly `T`. It
does not work if the type is a more complex expression containing `T`.

### Generic vectors

The generic vector declares type variables and optional constraints:

| Form | Meaning |
|------|---------|
| `[T]` | Unconstrained generic T |
| `[T R]` | Two unconstrained generics |
| `[(<= T Number)]` | T is a subtype of Number |
| `[(>= T Integer)]` | T is a supertype of Integer |
| `[(= T Any)]` | T with default value Any (alias definitions only, never in `@def`) |
| `[A R]` | Multiple generics |

### `@check` — Checker declaration

```elisp
(@check FUNCTION-NAME (CHECKER-EXPR))
```

Permitted checker shortcuts:

- `($body TYPE1 TYPE2 ...)` — Check leading operands against types, then
  check remaining as a body (implicit progn). Returns the type of the last
  body expression.

- `($fn TYPE1 TYPE2 ... RETURN-TYPE)` — Check positional operands against
  types (all but last), return the last type. No body evaluation.

- `($todo)` — Placeholder for checkers that need custom implementation. Must
  have a comment above explaining what behavior is needed.

### `@alias` — Type alias declaration

```elisp
(@alias NAME TYPE)
(@alias NAME [GENVEC] TYPE)
```

You will not normally write `@alias` in definition files. It is included here
for completeness.

### `@variable` — Variable type declaration

```elisp
(@variable NAME TYPE)
```

### Vector of names

Both `@def` and `@check` accept a vector of names to declare the same
signature for multiple functions:

```elisp
(@def [forward-char backward-char] (&optional n: Integer?) Nil)
```


## Concrete examples

### Simple function

```elisp
(@def characterp (object: Any &optional ignore: Any) Boolean)
```

### Optional and rest parameters

```elisp
(@def string (&rest characters: &List<Integer>) String)
(@def sleep-for (seconds: Number &optional milliseconds: Integer?) Nil)
```

### Generic function

```elisp
(@def prin1
      ([T] object: T
       &optional printcharfun: True|Buffer|Marker|fn1<Integer>?
       overrides: True|List<Cons<Symbol~Any>>?)
      T)
```

### Generic with constraints

```elisp
(@def set-marker
      ([(<= M Marker)] marker: M position: IntOrMarker? &optional buffer: Buffer?)
      M)
```

### Conditional return type

```elisp
(@def abs (arg: [<= N Number]) (extends? N Integer Integer Number))
```

### Complex union parameters

```elisp
(@def call-process
      (program: String &optional infile: String?
       destination: (or Buffer String Boolean Integer
                        (Tuple @file String)
                        (Tuple Buffer|String|Boolean String|Boolean))
       display: Bool &rest args: &List<String>)
      Nil|Integer|String)
```

### Checker with $todo

```elisp
;; `interactive' is a declarative special form: its arguments are never
;; evaluated, and it only tells `call-interactively' how to read arguments
;; for the enclosing command. There is no checker shortcut for a
;; declaration-only form whose operands are never evaluated or checked.
(@check interactive ($todo))
```

### Todo in return type with blocker comment

```elisp
;; The result is a complex value-dependent list whose structure depends
;; on DETAIL-P. The type language cannot yet express value-dependent
;; result structure.
(@def find-composition-internal
      (pos: IntOrMarker limit: IntOrMarker? string: String? detail-p: Bool)
      Todo)
```

### Using Bool for truthiness flags

```elisp
(@def redisplay (&optional force: Any) Boolean)
(@def ding (&optional arg: Bool) Nil)
```


## `@def` rules

### Return type defaults

- For functions that return `Any`, you may omit the return type from `fn`
  (it defaults to `Any`).
- For functions that take `Nil` as arglist and return `Any`, you may omit
  both arglist and return from `fn` (it defaults to `(fn Nil Any)`).

### Bool vs Boolean

- Use `Bool` for a parameter that controls behavior through truthiness
  (nil = false, any non-nil = true). `Bool` is an alias for `Any`.
- Use `Boolean` for values that must actually be `nil` or `t`, including
  boolean return types.

### Read-only types

Read-only (`&`) applies only to container types: `Cons`, `List`, `Vector`,
`HashTable`, and aliases built on them (like `Tuple`, `Alist`, `Tree`).
It does not apply to atomic types like `Integer`, `String`, or `Symbol` —
those have no write interface, so `&Integer` is meaningless.

**Parameters:**

- Use `&List<E>`, `&Cons<A~B>`, `&Vector<E>` when the function only reads
  the container (traverses, searches, copies elements out).
- Use `List<E>`, `Cons<A~B>`, `Vector<E>` when the function may mutate the
  container (e.g. `setcar`, `nconc`, `sort`, `aset`, `nreverse`).
- Rest parameters always use `&List<T>`.

**Return types:**

- Use `List<E>`, `Cons<A~B>` when the function returns a freshly allocated
  container the caller is free to mutate.
- Use `&List<E>`, `&Cons<A~B>` when the function returns internal structure
  the caller should not mutate (e.g. returning a stored list without copying).

**Default:** most Emacs functions read containers without mutating them.
Default to `&List` / `&Cons` / `&Vector` for parameters, and use the mutable
version only when the source shows mutation.

### Format-string functions

Do not encode how many data arguments are required by format directives.
Declare variadic data arguments as an unconstrained rest list (`&List`).
A format-dependent argument count is not an authoring blocker.

### Plist tails with `&key`

If the tail of a function's arguments is a plist, use `&key` in the argument
list (cl-defun convention) to indicate the start of keyword arguments. You
can then give types to the remaining arguments as normal. The processor
automatically converts this to a function whose argument list ends with a
plist.

### Following established precedent

If there are complex relationships between types, look for existing
definitions with a similar relationship and follow the precedent. For
example, for indexing a string or buffer, where a marker is only valid for
a buffer, the precedent is `StringOrBuffer<Idx>` for the string-or-buffer
type and `Idx` for the index type.


## Authoring `@def` declarations

A function is authorable when its useful parameter types, return type, and
relationships can be expressed declaratively using the type language documented
in this contract.

Generics, constraints, unions, function types, existing aliases, and type
operators are allowed. Complexity in the source implementation is not itself a
reason to approximate. The deciding question is whether the function's type
semantics can be expressed accurately without new supporting code.

If an argument is EXACTLY a generic, you don't have to BOTH declare the
generic in the genvec AND THEN use it in the parameter. Instead, declare it
directly in the parameter:
`(arg: [T])` is short for `([T] arg: T)`

An `@def` requires approximation when an accurate declaration depends on a
relationship that the current type language cannot express, including:

- Freshening or preservation of object identity
- Shallow, deep, or selective copying relationships
- Mixed sharing and freshness within one result
- Mutation or writeability relationships not represented by existing types
- Returning a particular existing substructure of an argument (e.g. the cdr
  of an internal list element). Note: when a function returns one of its
  arguments unchanged, a generic captures this — that is not a blocker
- Container-kind or element-type relationships not represented by existing
  types
- Value-dependent arity or value-dependent result structure
- Callback application relationships not expressible by existing function
  types

This list describes common blockers, not an automatic rejection rule. Use an
existing facility when it expresses the behavior accurately.

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


## Unified authoring workflow

Complete these steps independently for every assigned source/target pair:

1. Read the complete source file.
2. Compare the source against the existing `1:` skeleton to confirm nothing
   was missed or duplicated in the inventory.
3. Read the relevant declarations, documentation, and implementations before
   authoring their types.
4. Fill in every stub with a complete declaration.
5. Choose one supported authoring outcome for every entry.
6. Perform the static verification for that file.
7. Rename the target from `1:` to `2:`.

For every entry, choose one of these outcomes:

1. Write a complete `@def` declaration.
2. Write the most precise available `@def` approximation containing `Todo`
   only where the current type language is insufficient.
3. Write an authorable checker with `$body` or `$fn`.
4. Write an unauthorable checker as exactly `(@check FUNCTION ($todo))` with
   a concrete blocker explanation.


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

- `0:NAME` — unprocessed source file, awaiting inventory.
- `1:NAME` — inventory complete (skeleton of names only), awaiting authoring.
- `2:NAME` — authoring complete.

Never rename a file to contain no prefix. The user must do that.
