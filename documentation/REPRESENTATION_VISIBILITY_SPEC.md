# Representation visibility specification

Working implementation specification for making representation visibility orthogonal to nominal-name visibility on both `struct` and `induct`, and for removing the `record` declaration keyword in the same change.

This is a source-breaking surface-language change. There is no compatibility parser or deprecation interval: the implementation migrates the repository atomically, removes `record` from the grammar, and updates every embedded library source, test, printer expectation, and durable document in the same patch.

When the implementation lands, fold the durable language rules into `SYNTAX.md`, update `ROADMAP.md` and `AGENTS.md`, and retain or delete this working specification according to whether it still adds implementation history not appropriate to those references.

## Motivation

Curios currently has two independent notions of visibility for nominal product types but encodes them asymmetrically: an outer `pub` exports the type name, while the declaration keyword chooses representation visibility (`struct` hides fields and `record` exposes them). Inductives have only the first axis: a public `induct` necessarily exposes all of its constructors and elimination structure.

The language needs the same abstraction boundary for sums that it already has for products. A module should be able to export an inductive type while retaining exclusive construction and elimination rights, allowing smart constructors, invariant preservation, and representation changes without an otherwise artificial opaque-struct wrapper.

Rather than add another declaration modifier or a second inductive keyword, the result-sort position carries representation visibility. The two written `pub`s then qualify different boundaries: the outer `pub` exports the nominal name, and the inner `pub` exports the representation.

## Surface grammar

The outer visibility marker remains optional and continues to occupy the beginning of an item. Rename its implementation terminology from `is_pub` to `vis_pub` wherever it means name or interface visibility.

The terminal sort of a `struct` or `induct` declaration accepts its own optional `pub`:

```ebnf
visibility          := "pub"?
sort                := "Type" | "Prop"
representation-sort := "pub"? sort

struct-declaration := visibility "struct" identifier parameters? ":" representation-sort "{" fields "}"

inductive-arity := representation-sort
                 | "(" indices ")" "->" representation-sort

induct-declaration := visibility "induct" identifier parameters? ":" inductive-arity cases (visibility "and" ...)* "end"
```

`representation-sort` is declaration-only syntax. This feature does not add a general `pub Type` or `pub Prop` term, does not change the universes, and does not permit `pub` inside arbitrary type expressions.

Plain and indexed examples:

```crs
pub induct Option(A : Type) : pub Type
| some(A)
| none()
end

pub induct Vec(T : Type) : (n : Nat) -> pub Type
| nil() : (0)
| cons(@m : Nat, x : T, xs : Vec(T, m)) : (m + 1)
end

pub struct Pair(A : Type, B : Type) : pub Type {
    fst : A,
    snd : B
}
```

The same syntax applies to `Prop`:

```crs
pub induct Eq(@A : Type) : (x : A, y : A) -> pub Prop
| refl(@z : A) : (z, z)
end
```

## The two visibility axes

The outer and inner markers are independent. All four combinations are legal; there is no validation rule requiring a representation-public declaration to have a directly public name.

| Declaration form | Nominal name | Representation |
| --- | --- | --- |
| `struct S : Type` / `induct I : Type` | private | private |
| `pub struct S : Type` / `pub induct I : Type` | public | private |
| `struct S : pub Type` / `induct I : pub Type` | private | public |
| `pub struct S : pub Type` / `pub induct I : pub Type` | public | public |

Name visibility answers whether ordinary module lookup may reach the type-former binding. Representation visibility answers what a module may do once it can reach the type: build and project a struct, or construct and eliminate an inductive.

A representation-public declaration with a private name does not publish that name by itself. The combination remains meaningful for code reached through a facade, re-export, transparent alias, generated reference, or another route that makes the nominal type available without making the original declaration directly public. The implementation must preserve the two bits rather than normalize this combination away.

Representation privacy remains exact-module privacy. The declaring module may always use its own fields or constructors; a descendant module does not inherit access merely by nesting under the declaration's qualifier.

## Hard removal of `record`

Remove `record` as a declaration keyword in the same implementation. `struct` becomes the sole nominal product declaration, and the inner `pub` expresses the representation-public form.

The migration is mechanical and behavior-preserving:

```text
record S : Type      -> struct S : pub Type
pub record S : Type  -> pub struct S : pub Type
record P : Prop      -> struct P : pub Prop
pub record P : Prop  -> pub struct P : pub Prop
```

There is no deprecated spelling and no targeted compatibility diagnostic. Remove `record` from the reserved-keyword table, so it becomes available as an ordinary identifier after the change. A source file that still uses declaration-shaped `record X ...` fails under the ordinary grammar.

Do not mechanically remove the English noun “record” where it describes a genuine domain concept rather than the deleted keyword. Anonymous records, ABI result records, erased flat records, and record-shaped concept dictionaries remain valid terminology. Documentation that describes surface declarations must use `struct` and the inner-`pub` rule.

## Migration of existing inductives

Every currently public inductive is representation-public. Preserve that behavior by adding the inner `pub` throughout the repository:

```text
pub induct Option(A : Type) : Type
    -> pub induct Option(A : Type) : pub Type
```

Private inductives remain private on both axes unless the source intentionally chooses `: pub Type` or `: pub Prop`.

The repository migration includes `curios-text/std/`, `curios-text/syn/`, parser and lowering fixtures, cross-stage tests, syntax examples, and any source strings embedded in Rust tests. Important public families include `Option`, `Result`, `Vec`, `BigInt`, `Eq`, `Order`, JSON, I/O, task, HTTP, and `/syn/Str` support families.

## Surface AST

Rename every surface-AST field whose old name is `is_pub` and whose meaning is outer declaration or interface visibility to `vis_pub`.

This applies to `TopMod`, `TopUse`, `TopLet`, `TopForeign`, `TopInduct`, `TopStruct`, and `TopConcept`, plus associated parser locals, printer helpers, prelude constructors, tests, and comments. For `TopUse`, `vis_pub` means that the import also contributes to the declaring module's public interface.

`TopInduct` gains `rep_pub`; `TopStruct` retains `rep_pub` but obtains it from the result-sort marker instead of from the `record`/`struct` keyword choice:

```rust
pub struct TopInduct {
    pub vis_pub: bool,
    pub rep_pub: bool,
    // existing label, parameters, indices, result sort, and cases
}

pub struct TopStruct {
    pub vis_pub: bool,
    pub rep_pub: bool,
    // existing label, parameters, result sort, and fields
}
```

Keep the token-level helper named `parse_pub` if useful; bind its result to `vis_pub` or `rep_pub` at the semantic destination. Add a declaration-specific parser such as `parse_representation_sort` that returns `(rep_pub, result_sort)`. Do not change the plain `parse_sort` used by concepts and other closed-sort positions.

The printer emits both flags independently. Printer round trips must preserve every one of the four combinations, including each member of a mutually recursive inductive group.

## Text-stage module interfaces

`vis_pub` controls direct public-interface reachability exactly where `is_pub` does today.

For a struct, `vis_pub` controls the type-former binding. There is no generated child namespace.

For an inductive, the type-former binding and constructor child namespace must no longer share one visibility bit:

- the type-former binding is directly public iff `vis_pub`;
- the constructor namespace is directly public iff both `vis_pub` and `rep_pub`;
- the constructor namespace and its direct bindings still exist in the internal module table regardless of either flag, so the declaring module can resolve its constructors;
- an opaque inductive's constructor namespace must not enter a public interface through a glob or `pub use` and thereby raise `rep_pub`.

The existing interface phase already materializes an inductive's constructor namespace separately from the type-former binding. Split the parent binding/child visibility there instead of inventing a second set of constructor definitions.

Public-interface dependency checks follow the externally exposed portion of the declaration:

- if `vis_pub`, check the type-former interface: parameter types, index types, and result sort;
- if both `vis_pub` and `rep_pub`, also check struct field types or inductive constructor signatures, including payload types and indexed targets;
- if `vis_pub` and not `rep_pub`, private field or constructor-payload types are legal because they remain behind the abstraction boundary.

A `pub use` must never upgrade a declaration whose `rep_pub` is false. The interface resolver must retain enough provenance to distinguish a public type binding from a public representation rather than treating the constructor namespace as an unrelated ordinary child module.

### Private name with public representation

The `vis_pub = false, rep_pub = true` combination has no direct public-interface dependency obligation merely because it is declared: its name is not in the direct interface. If a facade or re-export later makes the representation reachable, that exposure must be checked at the exposure point so private field or constructor-signature dependencies cannot be laundered.

The current interface checker validates direct public declarations during lowering and does not carry full type signatures through the `pub use` fixed point. The implementation must choose and document a concrete enforcement mechanism before treating this corner as complete. Acceptable implementations include attaching representation provenance and signatures to interface entries, or adding a post-resolution audit over every representation that the resolved public interface exposes. Eagerly checking every `rep_pub` declaration is simpler but intentionally more restrictive than the stated semantics and should not be substituted silently.

## Core registry metadata

`Structure` already carries the representation metadata needed by the elaborator:

```rust
pub module: Qualifier,
pub rep_public: bool,
```

Add the same fields to `Inductive`:

```rust
pub struct Inductive {
    // existing parameter, index, constructor, and result-sort metadata
    pub module: Qualifier,
    pub root: RootId,
    pub rep_public: bool,
}
```

The core keeps the existing longer `rep_public` spelling. `vis_pub` is a surface/module-interface concern and does not need to survive in the core module after name resolution and interface construction.

Populate `Inductive::module` with the exact source module that owns the declaration, not with the generated constructor namespace. Propagate both fields through registry rebuilding, zonking, cloning, test fixtures, module storage, and the fresh contexts used independently by elaboration and erasure.

Update the `Inductive` and `Structure` rustdoc to describe the common representation boundary and the distinction between the fine-grained declaring module and the compilation `RootId`.

## Struct construction and projection

Existing struct behavior remains unchanged after the syntax migration:

- `elaborate_struct` permits construction when `rep_public` or when the current island equals the declaring module;
- `elaborate_proj` permits projection under the same condition;
- struct spreads and destructuring ultimately use construction/projection and inherit the same boundary;
- erasure and runtime layout do not depend on `rep_public`.

The only functional struct change is how `rep_public` reaches the AST and registry: inner `pub` replaces the `record` keyword.

## Inductive construction

An opaque inductive's constructor namespace is absent from external name resolution, which blocks ordinary source construction. Core elaboration should nevertheless enforce the same invariant defensively when checking a `Variant`, so representation privacy does not rely solely on one text-stage namespace path.

Generated constructor functions are qualified under `Type/constructor`, but their bodies semantically belong to the source module that declared the inductive. Today `FlatLet::into_core` derives a definition's island from its qualified binding name, which would incorrectly make a generated constructor body belong to the synthetic constructor namespace. Make the declaring island explicit on flat definitions, or otherwise stamp generated constructor definitions with the source module, before adding the defensive `elaborate_variant` check.

The check is then the same as struct construction: permit the variant when `rep_public` or when the current island equals `Inductive::module`; otherwise report a private-representation error.

## Inductive elimination

`elaborate_inductive_match` is the authoritative representation-privacy boundary for elimination. After inferring the scrutinee's inductive head and retrieving its registry entry, reject the match when `rep_public` is false and the current island differs from `Inductive::module`.

The check applies to every inductive eliminator, not only matches that name a constructor:

- complete constructor enumeration;
- sparse matches with a final `_` default;
- a catch-all/default path that happens not to inspect payloads;
- empty or vacuous elimination;
- indexed elimination whose omitted arms are discharged by inversion;
- dependent motives and synthesized convoy motives.

This prevents clients from observing constructor count, emptiness, indexed impossibility, or payload structure through an eliminator while constructor names themselves are hidden.

The declaring module retains the full existing behavior, including dependent elimination, coverage checking, index inversion, large-elimination checking, and constructor refinement.

## Meaning of opacity

Representation opacity is an elaboration and module-interface boundary, matching the existing meaning of an opaque `struct`. It is not computational opacity or sealing of definitions.

Do not change reduction, conversion, proof irrelevance, constructor injectivity, runtime tags, erased layout, or Wasm emission. A public function whose body privately constructs or matches an opaque value continues to elaborate in its declaring island, and its already-elaborated body may reduce normally during later compiler passes.

Full representation independence under definitional equality would be a separate feature involving opaque definitions or module sealing and is outside this specification.

## Concepts

Concepts remain inherently representation-public record-shaped interfaces. Their syntax continues to use a plain result sort:

```crs
pub concept Show(A : Type) : Type {
    show(A) -> Str
}
```

Do not add inner-`pub` concept syntax in this change. Concept lowering continues to create a `Structure` with `rep_public = true`, because concept literals build dictionaries and method wrappers consume their fields. Replace documentation that says a concept “lowers to a `record`” with wording such as “lowers to a representation-public nominal structure.”

## Diagnostics

Generalize the existing struct-specific `PrivateRepresentation` rendering so it can describe either declaration kind:

```text
the representation of type 'T' is private to its declaring module
```

Use it for direct struct construction, defensive inductive construction, and inductive elimination. Retain the more specific `PrivateField` diagnostic for struct projection if its field name remains useful:

```text
field 'x' of struct 'T' is private to its declaring module
```

External constructor lookup may fail earlier in text-stage name resolution because an opaque constructor namespace is not public. Tests should pin the appropriate layer's diagnostic without requiring every forbidden operation to converge on one error variant.

Update `PrivateItemInPublicInterface` examples and documentation to cover representation-public constructor signatures as well as struct fields.

## Erasure and downstream stages

There is no intended erased-IR, continuation-IR, Wasm-model, ABI, runtime, or code-generation change.

Erasure creates its own core `Context` and re-infers terms, so it must receive the new `Inductive` metadata and the correct definition islands. Once seeded correctly, it reproduces the same privacy decisions as elaboration and emits exactly the same variant tags and payload layouts as before.

Constructor ordering remains derived from `Inductive::constructor_order`. Representation visibility must not participate in tag assignment, optimization, or reachability except through ordinary source accessibility.

## Implementation map

The principal source locations are:

- `curios-text/src/parse.rs`: remove `record` from `KEYWORDS`;
- `curios-text/src/parse/top_level.rs`: parse inner `pub`, remove the `record` branch, and return both visibility bits;
- `curios-text/src/module.rs`: rename `is_pub` to `vis_pub` and add `TopInduct::rep_pub`;
- `curios-text/src/print.rs`: print outer and inner `pub` independently;
- `curios-text/src/into_core.rs`: split type/constructor interface checks, lower both registry flags, migrate concept comments, and stamp generated constructor islands correctly;
- `curios-text/src/into_core/context.rs`: rename visibility terminology and make flat-definition islands explicit if used for constructor enforcement;
- `curios-text/src/into_core/interface.rs`: separate inductive binding visibility from constructor-namespace visibility and prevent representation laundering through `pub use`;
- `curios-core/src/inductive.rs`: add and document `module` and `rep_public`;
- `curios-core/src/elaborate/aggregate.rs`: enforce defensive variant construction privacy;
- `curios-core/src/elaborate/match_.rs`: enforce elimination privacy before coverage and inversion expose representation facts;
- `curios-core/src/elaborate/module.rs` and `curios-core/src/zonk.rs`: preserve the new inductive metadata;
- `curios-core/src/error.rs`: generalize representation diagnostics;
- `curios-text/std/`, `curios-text/syn/`, and `curios/src/tests/`: perform the atomic source migration;
- `documentation/SYNTAX.md`, `documentation/ROADMAP.md`, and `AGENTS.md`: install the durable post-implementation description.

Search for `record`, `rep_pub`, `rep_public`, `is_pub`, `TopInduct`, `TopStruct`, constructor-interface comments, and struct-specific private-representation wording before declaring the migration complete. Do not blindly replace generic uses of the English word “record.”

## Tests

### Parser and printer

- Parse and round-trip all four visibility combinations for both `struct` and `induct`.
- Cover `Type`, `Prop`, indexed inductive arities, parameterized declarations, and mutually recursive groups.
- Prove that inner `pub` is rejected outside a struct/inductive terminal-sort position.
- Remove every `record` parser expectation, reject declaration-shaped legacy syntax, and accept `record` as an ordinary identifier.
- Pin `vis_pub` and `rep_pub` independently in AST assertions.

### Struct behavior

- Preserve opaque external construction and projection failures.
- Preserve representation-public construction, projection, patterns, spread/update, dependent fields, and newtype erasure under the new spelling.
- Cover private-name/public-representation declarations.
- Preserve the rule that hidden fields may mention private helpers while exposed fields in a public interface may not.

### Inductive behavior

- A public opaque inductive may appear in public signatures.
- Its constructors cannot be resolved or re-exported externally.
- External direct construction is rejected.
- External named constructor matching is rejected.
- External `_` default matching is rejected.
- External empty or vacuous elimination is rejected.
- Indexed inversion and omitted impossible arms cannot bypass opacity.
- The exact declaring module can construct, match, invert indices, and use dependent motives.
- A representation-public inductive preserves all current constructor and match behavior.
- Constructor payload and target types participate in public-interface checking only when the representation is exposed through that interface.
- `pub use` cannot upgrade an opaque constructor namespace.

### Core and downstream regression

- Update every direct `Inductive` fixture with `module` and `rep_public`.
- Preserve constructor order and erased tags.
- Preserve large-elimination and proof-erasure behavior for public-representation propositions.
- Run existing codegen parity tests after syntax migration; their emitted behavior must not change.
- Preserve concept construction and witness resolution.

## Documentation migration

Update `SYNTAX.md` to describe one `struct` keyword, the two independent `pub` positions, the four legal combinations, and opaque inductive elimination. Remove `record` from the reserved keyword list and every surface grammar/example.

Update `ROADMAP.md` only when the implementation lands: replace the completed `struct`/`record` item with the unified `struct` representation-visibility syntax, and record opaque/public inductive representation control under inductive types.

Update `AGENTS.md` architecture and Curios-writing guidance in the same patch. In particular, replace the current `record`/`struct` distinction and the statement that every inductive's constructors share its visibility.

Any other document containing legacy `.crs` snippets must migrate those snippets even if the document describes unfinished future work, so the repository contains no examples in syntax the parser no longer accepts.

## Verification

Run the repository's full done bar in order:

```sh
make curios/runtime
cargo fmt --all -- --check
cargo check --workspace --all-targets --all-features
cargo clippy --workspace --all-targets --all-features
cargo test --workspace --all-targets --all-features
```

Because the change touches `curios-text` and `curios-core`, which are dependencies of `curios-js`, also run the browser build performed by CI with the exact `wasm-bindgen-cli` version matching `Cargo.lock`:

```sh
cargo build --release --target wasm32-unknown-unknown --package curios-js
wasm-bindgen --target web --out-dir /tmp/curios-js-bindgen target/wasm32-unknown-unknown/release/curios_js.wasm
```

No generated `.wasm` output belongs in the repository.

## Completion criteria

The work is complete when the `record` keyword and every `record` declaration are absent from the surface grammar and repository sources, `struct` and `induct` both preserve independent `vis_pub` and `rep_pub` semantics, opaque inductives cannot be constructed or eliminated outside their declaring module, representation-public declarations preserve current runtime behavior, public interfaces cannot launder private representations, durable documentation describes the landed language rather than this migration, and every verification gate passes.
