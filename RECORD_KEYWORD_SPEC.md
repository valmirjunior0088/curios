# Implementation specification: `struct`/`record` keyword for representation visibility

## Summary

Replace the struct declaration's *inner* `pub` (the one before the `{`) with a
choice of declaration keyword: `struct` (representation private) vs `record`
(representation public). The outer `pub` continues to export the type name.

```
struct Foo { ... }      -- private:     type and representation module-local
pub struct Foo { ... }  -- abstract:    type exported, representation hidden
pub record Foo { ... }  -- transparent: type and representation exported
```

`record` must be `pub`. A non-`pub` `record` (private type, public
representation) is rejected — its only meaning was "representation shared with
submodules but not exported", which nothing uses.

This is a **surface-only** change. The core carries a single `rep_public: bool`
exactly as today; only the way it is *spelled and parsed* changes. There are no
changes below `text/` (no elaboration, erasure, or codegen changes).

## Background

Today a struct carries two independent `pub` markers:

```
pub struct Foo pub { ... }
```

- outer `pub` → exports the type name (`TopStruct.is_pub`)
- inner `pub`, before the brace → exports the representation: the brace literal
  and field projection (`TopStruct.rep_pub` → `core::Structure.rep_public`)

The representation boundary is the **exact** declaring module, *excluding* its
submodules (SYNTAX.md). The grammar therefore allows four combinations, but only
three are wanted. The fourth — `is_pub = false, rep_pub = true` (type private,
representation public) — only grants submodule access to the representation of an
otherwise-private type. It is currently expressible (`std/Task.crs` spells it for
`Job`/`Parked`, `src/tests/erasure.rs` for `Wrap`) but no code relies on it: in
every case the struct is used only within its own module, so it compiles
identically as a plain `struct`. That state is dropped.

`island == module` is confirmed by `struct_destructure_private_field_rejected`
(`src/tests/structs.rs`): a rep-private struct is usable across separate top-level
`let`s of the same module; only out-of-module access is rejected. So migrating
the old `struct … pub { }` declarations to plain `struct { }` is safe wherever
all use is in-module (verified true for `Job`/`Parked`/`Wrap`).

## Target semantics

| spelling                | `is_pub` | `rep_pub` | meaning      | type visible | representation usable |
|-------------------------|----------|-----------|--------------|--------------|-----------------------|
| `struct Foo { ... }`    | false    | false     | private      | declaring module | exact declaring module |
| `pub struct Foo { ... }`| true     | false     | abstract     | exported     | exact declaring module |
| `pub record Foo { ... }`| true     | true      | transparent  | exported     | wherever the type is nameable |
| `record Foo { ... }`    | false    | true      | **rejected** | —            | — |

`rep_pub` implies `is_pub`. Violation is a compile-time error
(`text::Error::PrivateRecord`).

## Code changes

### 1. Parser — `src/text/parse.rs`

**Keyword list** (the `KEYWORDS` constant, ~line 26): add `"record"`.

```rust
const KEYWORDS: &[&str] = &[
    "let", "match", "rec", "mod", "use", "pub", "end", "false", "true", "induct", "struct", "record",
];
```

(No `.crs` source or term uses `record` as an identifier; the only `record` in
the codebase is a Rust helper `fn record` in `src/text/prelude.rs`, which a
Curios keyword does not affect.)

**`parse_top_struct`** (~lines 1545–1576): derive `rep_pub` from the kind
keyword and remove the inner `parse_pub()` before `{`. The first alternative of
the keyword choice needs `catch` so `.or` can backtrack the consumed identifier
(mirroring how `parse_pub` wraps `parse_keyword("pub")`).

```rust
fn parse_top_struct<'a>() -> Parser<'a, TopItem> {
    // `pub`? then the kind keyword: `struct` (rep private) or `record` (rep public).
    let kind = catch(parse_keyword("struct"))
        .map(|()| false)
        .or(parse_keyword("record").map(|()| true));
    catch(parse_pub().and(kind)).flat_map(|(is_pub, rep_pub)| {
        parse_identifier()
            .and(
                catch(
                    parse_literal("(")
                        .and_keep(sep_by0(parse_inductive_param, || parse_literal(",")))
                        .and_drop(parse_literal(")")),
                )
                .or(pure(vec![])),
            )
            .and(
                catch(parse_literal(":").and_keep(lazy(parse_term))).or(pure(Subterm::Type.into())),
            )
            // inner `pub` removed — representation visibility now comes from the keyword
            .and_drop(parse_literal("{"))
            .and(sep_by0(parse_tuple_type_field, || parse_literal(",")))
            .and_drop(parse_literal("}"))
            .map(move |(((label, params), result_sort), fields)| {
                TopItem::Struct(TopStruct {
                    is_pub,
                    rep_pub,
                    label: label.to_string(),
                    params,
                    result_sort,
                    fields,
                })
            })
    })
}
```

The map closure loses one tuple-nesting level (`rep_pub` is now captured from the
`flat_map`, not threaded through a trailing `.and`).

The grammar still *accepts* the orphan `record Foo { ... }` (parses to
`is_pub = false, rep_pub = true`); it is rejected in `to_core` so the error is
located and helpful rather than a generic parse failure.

### 2. AST — `src/text/module.rs`

`TopStruct` is unchanged (both `is_pub` and `rep_pub` remain). Update only the
doc comment (~lines 100–104):

```rust
/// A `struct`/`record` declaration: a nominal record. `is_pub` is the outer
/// `pub` (the type-former's visibility). `rep_pub` is the kind keyword —
/// `record` (representation exported) vs `struct` (representation
/// module-private). `rep_pub` implies `is_pub`; a non-`pub` `record` is
/// rejected in `to_core`. `params` are written exactly like an inductive's;
/// `fields` reuse the Σ-type field grammar (label optional, like tuple-type
/// fields).
```

### 3. Orphan rejection — `src/text/to_core.rs` and `src/text/error.rs`

In the struct lowering arm (`to_core.rs`, the `TopItem::Struct(s) =>` at ~line
575), reject the orphan before any lowering work:

```rust
TopItem::Struct(s) => {
    if s.rep_pub && !s.is_pub {
        return Err(Error::PrivateRecord { label: s.label.clone() });
    }
    // ... existing lowering unchanged; `rep_public: s.rep_pub` still flows
    //     into the core Structure (~line 631).
}
```

Add the variant to `src/text/error.rs` (alongside the other simple variants) and
a `Display` arm:

```rust
/// A `record` (representation-public struct) was declared without `pub`. It
/// would hide the type yet publish its representation — reachable only from
/// submodules — which is disallowed. Write `pub record` to export the type, or
/// `struct` for a module-private record.
PrivateRecord { label: String },
```

```rust
Error::PrivateRecord { label } => write!(
    f,
    "the record '{label}' must be `pub`: write `pub record {label}` to export \
     the type, or `struct {label}` for a module-private one"
),
```

### 4. Printer — `src/text/print.rs`

`print_top_struct` (~line 868): emit the keyword from `rep_pub`; remove the inner
`pub` emission. The existing `pure(" ")` already provides the space before `{`.

```rust
fn print_top_struct(item: TopStruct) -> Printer<'static> {
    flat([
        print_pub(item.is_pub),
        pure(if item.rep_pub { "record " } else { "struct " }),
        pure(item.label),
        print_top_inductive_params(item.params),
        pure(" "),
        pure("{ "),
        sep_flat(item.fields.into_iter().map(print_field), || pure(", ")),
        pure(" }"),
    ])
}
```

### 5. No changes below `text/`

`core::Structure.rep_public`, the representation-privacy checks in
`src/core/elaborate.rs` (~lines 477 and 718), `src/core/zonk.rs`,
`src/core/elaborate_module.rs`, and newtype erasure in `src/core/erase.rs` are
all spelling-agnostic and remain untouched.

Optional follow-up (not required): the `PrivateRepresentation` message in
`src/core/error.rs` could gain a hint pointing at `record`.

## Source migrations

### Standard library (`std/*.crs`)

Transparent (exported representation) → `pub record`:

- `std/Tcp.crs:3`   `pub struct Settings pub {` → `pub record Settings {`
- `std/Http.crs:19` `pub struct Request pub {`  → `pub record Request {`
- `std/Http.crs:103``pub struct Status pub {`   → `pub record Status {`
- `std/Http.crs:124``pub struct Response pub {` → `pub record Response {`
- `std/BigNat.crs:3``pub struct BigNat pub {`   → `pub record BigNat {`

Old type-private + rep-public (`struct … pub {`) → plain `struct` (verified
in-module only; `Task` has no submodules):

- `std/Task.crs:159` `struct Job pub {`    → `struct Job {`
- `std/Task.crs:168` `struct Parked pub {` → `struct Parked {`

Unchanged: `std/File.crs:3` `pub struct File { Io }` (already abstract).

Embedding is via `include_str!` (`src/text/prelude.rs`, ~line 730 onward), so
edits are picked up on rebuild. The in-memory std-elaboration memo is recomputed
per test run from that embedded source — there is no committed snapshot to
regenerate.

### Tests

- `src/tests/structs.rs` — every transparent source (the `… pub {` forms at
  ~lines 29, 45, 61, 77, 164, 178, 192, 225, 241, 257, 273–274, 291–292, 308,
  324) → `pub record …`. The abstract `pub struct Celsius { … }` cases (~lines
  100, 120, 142, 347) stay as `pub struct`. **Add** a negative test
  (`record_without_pub_rejected`) asserting that `record Foo { … }` produces the
  `PrivateRecord` message. This must run through `to_core`/`run_text` (it parses
  successfully and fails during lowering).
- `src/tests/erasure.rs:105` `struct Wrap pub { … }` → `struct Wrap { … }`
  (verified used only in-module, not exported).
- `src/text/parse_tests.rs` — `parse_struct_visibility_spellings` (~line 1147):
  replace the table and comment with the three legal spellings:
  ```rust
  ("struct Foo { x : Type } u", false, false),
  ("pub struct Foo { x : Type } u", true, false),
  ("pub record Foo { x : Type } u", true, true),
  ```
- `src/tests/inference.rs` — no migration. Line ~211 is only a section comment;
  its test is an `Eq` test with no struct declaration. (Optionally tidy the
  stale "transparent record" comment; out of scope.)

### Examples

- `examples/crs_struct.rs:17–18` — `pub struct Pair(…) pub {` and
  `pub struct Meters pub {` → `pub record …`. The abstract
  `pub struct Token { Bin }` (line 22) and the ill-typed `Token/Token { … }`
  construction (line 77) stay unchanged; that case still exercises
  `PrivateRepresentation`.

## Documentation

### `SYNTAX.md` — §Struct (~lines 140–179)

Lines 149 (`struct Sized pub {`) and 155 (`struct Meters pub {`) are the
now-illegal orphan combo and **must** change. Replace the Visibility subsection
and fix the inline examples.

Proposed Visibility text:

> **Visibility.** A struct sits on a private → abstract → transparent scale, set
> by the outer `pub` and the kind keyword:
>
> ```
> struct Foo { ... }      -- private:     type and representation module-local
> pub struct Foo { ... }  -- abstract:    type exported, representation hidden
> pub record Foo { ... }  -- transparent: type and representation exported
> ```
>
> - The outer `pub` exports the **type name**, exactly as on an inductive.
> - `struct` keeps the **representation** — the brace literal and field
>   projection — private to the declaring module; `record` exports it. A
>   `record` must be `pub`: an unexported `record` would hide the type yet
>   publish its representation, so write `struct` for a module-private record.
>
> `struct` is the default kind, as `induct` is for sums — reach for it first and
> opt into `record` only when callers genuinely need the representation. The
> abstract form is the motivating case: outside the declaring module the type is
> namable but opaque, reachable only through the smart constructors and accessors
> that module exports. The representation boundary is exact — a `struct`'s
> representation may be constructed or projected only in the very module that
> declares it, not in its submodules — and a violation is a compile-time error.

Inline example fixes:

- line 143 → `pub record Pair(A : Type, B : Type) { fst : A, snd : B }`
- line 149 → `pub record Sized { n : Nat, v : Vec(Nat, n) }`
- line 155 → `pub record Meters { Nat }`

Construction examples (~lines 174–176) are brace literals and stay unchanged.

### `STD.md`

Struct renderings at ~lines 344, 378, 389, 512 → `record` / `pub record` to
match the migrated sources.

### Others

`CRASH_COURSE.md`, `PROOFS_101.md`, `README.md` — a grep finds no
transparent-struct spellings; re-scan to be safe. (`ARCHITECTURE.md`'s
`pub struct` reference is a Rust `name!` macro, unrelated.)

## Verification

- `cargo build` — keyword + parser compile.
- `cargo test` — full suite; pay attention to `structs`, `parse_tests`,
  `erasure`, and the std-dependent suites (`network`, `scheduler`, `bignat`),
  which exercise the migrated `std/*.crs`.
- Run `examples/crs_struct.rs` — still prints `hi12`, and the ill-typed case
  still fails with `PrivateRepresentation`.
- The new `record_without_pub_rejected` test covers the orphan rejection.

## Out of scope / explicitly dropped

- The "private type, submodule-shared representation" state (old
  `struct … pub {` with no outer `pub`) is removed. Confirmed unused.
- No changes to the representation-privacy *semantics* (the boundary stays the
  exact declaring module); only the spelling of the visibility bit changes.
