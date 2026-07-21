# Element-indexed packed binaries

Working implementation specification for replacing the surface names `/std/Bits` and `/std/Bytes` with one element-indexed family `/std/Bin`: `Bin(Bool)` is today's `Bits` and `Bin(Byte)` is today's `Bytes`, with the index dispatched through a sealed concept. The change deletes the old names without compatibility aliases and folds in the compiler work it was de-risked against: completing dependent-field method wrappers, minting the wrapper witness binder unspellably, probing erasure of `Type`-valued structure fields, and renaming `WireType::Bin` to `WireType::Bytes`.

## Objective

Name the two packed binary sequence types by what they contain. The element type is the honest index — `get` on a `Bin(Byte)` returns exactly a `Byte` — and a sealed concept keeps the family closed while giving the whole surface one module, one op set, and no `Byte`/`Bytes` near-collision. The compiler's internal `Bin(Grain)` family vocabulary, the packed literal grammar, and the erased representation are all unchanged; the surface adopts the family meaning the compiler already uses.

## Decision record

Chosen: an element-type index resolved by a sealed concept. It is honest (the index is the element the sequence contains), closed (sealing forbids external witnesses and external dictionary literals), aligned with the design law that syntax forms are closed and dispatch is concept-based, and its grain-to-carrier seam is two `satisfy` blocks in the prelude rather than Rust.

Rejected alternatives, recorded so they are not relitigated:

- Renaming only the byte type (`Bin`, `Blob`, `Buf`): surface `Bin` meaning bytes-only forks the compiler-internal family meaning of `Bin(Grain)` used across `curios-text` and `curios-core`; `Blob` connotes an opaque lump while these values decompose element-wise through `cons`-style matches; `Buf` connotes a mutable I/O buffer. The best pure-rename outcome was keeping `Bits`/`Bytes`, which preserves the adjacency this spec eliminates.
- `Bin(1)`/`Bin(8)` over a `Nat` index: the type former must answer for every natural. An empty-type default makes total generic ops unwritable, because a catch-all match arm never refines the index (only literal arms do), so the impossible arm can neither produce a result nor be discharged; a unit default mints junk inhabitants instead. Either way it is a partial generalization — a `(g : Nat) -> Type` surface over a two-point reality.
- An inductive `Grain` index (`Bin(bit)`/`Bin(byte)`): fully feasible on existing machinery with exhaustive two-arm matches and total ops, and it required no compiler changes, making it the de-risked fallback. Rejected because the constructors name the encoding rather than the element, and every op signature drags a computed `Elem(g)` that the element index provides for free.
- A two-concept split (a carrier-only `Grain` plus a `GrainOps` reaching it through a superclass edge): dodges the wrapper defect below instead of fixing it, doubles the concept surface, and rests on unexercised resolution behavior inside field-type elaboration.

## Surface design

`/std/Bin.crs` owns the family: the sealed concept, its two witnesses, the `Bin` type former, and the derived functions. The concept's fields mirror the six `/sys` primitive ops verbatim, with the grain-specific carrier and atom types replaced by the `Carrier` field and the `A` parameter:

```crs
pub concept Grain(A : Type) : Type {
    Carrier : Type,
    len(Carrier) -> Nat,
    eql(Carrier, Carrier) -> Bool,
    get(Carrier, Nat) -> A,
    slice(Carrier, Nat, Nat) -> Carrier,
    append(Carrier, A) -> Carrier,
    concat(Carrier, Carrier) -> Carrier,
}
```

The unmarked `: Type` seals the concept: witness declarations and dictionary literals are permitted only in `/std/Bin.crs`, so the family is closed there — `Bin(Str)` fails at elaboration with a missing-witness error, and no third party can register or hand-build a `Grain` dictionary. The two witnesses bind the index types to the generated `/sys` carriers, and the orphan rule is satisfied by `/std` owning the concept:

```crs
satisfy Grain(Bool) { Carrier = /sys/Bits/Bits, len(b) = /sys/Bits/len(b), … }
satisfy Grain(Byte) { Carrier = /sys/Bytes/Bytes, len(b) = /sys/Bytes/len(b), … }
```

The type former projects the carrier, and the derived functions of both old modules generalize over the witness — `at` (proof-carrying), `get` (the `Option` form), `cons`, `flatten`, `fold`, and the formerly byte-only `find_index` and `join`, which generalize without change:

```crs
pub let Bin(A : Type, use Grain(A)) -> Type = Grain/Carrier(A);
```

The `satisfy` blocks must precede every in-module consumer: witnesses land in flat order after their body's dependencies and have no topological edge from consumers. Downstream modules are ordered safely by importing `/std/Bin`. The `Eql` witnesses in `/std/Eql.crs` become `satisfy Eql(Bin(Bool))` and `satisfy Eql(Bin(Byte))`, keyed on the reduced primitive heads exactly as the current `Eql(Bits)`/`Eql(Bytes)` already are.

## What `Bin(Byte)` means to the compiler

Finding: `Bin` is an ordinary name resolved by module resolution; nothing in the parser or lowering knows it. Elaborating the application puts `Byte` in the explicit queue, and the `use Grain(A)` parameter triggers witness resolution: the goal keys as the concept name plus `HeadKey::Byte`, the program-wide table returns the `/std/Bin` witness, and its backing definition is spliced in as the argument.

Reduction: unfold the `let Bin`, unfold the `Grain/Carrier` wrapper, project `Carrier` from the witness definition's structure literal, unfold the `/sys` carrier alias, arriving at `Prim::BinType(Grain)`. Every step is ordinary delta and iota reduction; sealing gates what source may be written where, never conversion, consistent with the law that conversion incompleteness comes only from the deadline.

Seams: the lexer's literal prefixes still select the carrier (`b\`/`x\` produce grain-tagged packed literals that never consult `Bin`), `/sys` generation in `curios-text/src/prelude.rs` is untouched, and the match compiler keeps discovering grain by reducing the scrutinee's type, which now reduces through the witness chain. The only new seam is the pair of `satisfy` blocks. The IR is untouched end to end: the `Prim` ops keep static `Grain`, generic-element code is dictionary passing in Curios source that the `curios-ersd` partial evaluator specializes at concrete elements, and the distinct-shapes and numeric-carrier laws are unaffected.

## Compiler changes

### Dependent-field method wrappers (`curios-text`)

Defect: wrapper synthesis in `into_core.rs` reuses each field's surface type verbatim as the wrapper's output in a scope binding only the concept parameters and the witness, so a reference to an earlier field — `len(Carrier) -> Nat` — lowers to an unbound free variable and fails core elaboration. No existing concept has inter-field references, which is why it has never fired; the structure-telescope side of the contract is production-proven by `BigNat`'s dependent record, and SYNTAX.md already promises that a field's label binds later fields.

Fix: while lowering a wrapper's signature, bind each earlier ordinary field label to the raw projection from the witness binder (substitute-on-lookup through the `Lowerer`'s existing binder tracking, so shadowing is handled by the same machinery that handles it everywhere else). Raw projections are already the generated-wrapper idiom — the body is `w.f` — and are legal because wrappers are minted inside the declaring module, so sealing is indifferent. This reproduces exactly the substitution the core projection rule applies to the body, so the declared type and the body's type coincide; because a wrapper is an ordinary `let` checked at prelude build, any future drift fails loudly and unsoundness is structurally unreachable. Superclass fields need no treatment: they are anonymous, their minted labels are unspellable from surface syntax, and they receive no wrappers.

Rejected shapes: rewriting labels to wrapper names routes the signature through witness resolution and is correct only because the wrapper scope happens to hold a single candidate; generating wrappers in core from the elaborated telescope is drift-proof by construction but requires core to mint top-level definitions during elaboration, cutting across the flat-item pipeline and the prelude replay contract for a guarantee the type-check backstop already provides.

### Unspellable witness binder (`curios-text`)

The generator hardcodes the spellable binder `"w"`, so a concept parameter named `w` — legal today — would be captured by the witness binder in the wrapper's output type. Mint the binder through the compiler naming scheme (clash-free against surface identifiers) in the same change. The wrapper fix and this rename are identity for every existing concept, which has neither inter-field references nor a `w` parameter.

### Erasure of `Type`-valued fields (probe, contingent fix in `curios-core`)

`Grain` witnesses are the first structure values in the codebase carrying a type (`Carrier = …`) in value position; no existing struct or concept has a `Type`-sorted field. The general erase-what-is-type-sorted rule should drop the field as it drops implicit type arguments, but this exact shape is unexercised and the concept-erasure seam has regression history. Probe value-driven before implementation; if broken, the fix lives in `erase_ir`: drop type-sorted fields consistently at literal and projection sites.

### `WireType::Bin` → `WireType::Bytes`

The wire type is bytes-only, so the family name misdescribes it. Inventory: the enum and rows in `curios-abi/src/host.rs`; the wire mapping in `curios-core/src/prim.rs`; in `curios-text` the surface FFI keyword (`parse/top_level.rs` parses the literal `"Bin"`, `print.rs` prints it) plus `prelude.rs`'s wire lowering and the parser and lowering tests; `curios-cont/src/into_wasm/module_emitter.rs` and `context.rs`; `curios-runtime/src/engine.rs`; and SYNTAX.md's wire-type sentence, which becomes "Wire `Bytes` maps to object-language `Bin(Byte)`". No in-repository `.crs` file uses the wire keyword. The JavaScript harness's `decodeBin`/`encodeBin` are local helper names, not contract; renaming them is optional.

## Probes before implementation

1. A toy concept with a `Carrier : Type` field and a dependent method field, confirming the wrapper defect as diagnosed and then validating the wrapper fix, the sealed gating, and cross-module reduction of a `Bin`-style alias in type position (a packed literal checked against it, and a bin match whose scrutinee type reduces through the witness chain).
2. A value-driven erasure probe: a program whose runtime path constructs a witness with a `Type`-valued field and calls a projected op, with runtime-tainted inputs so the emitted code is actually exercised rather than const-folded.

## Migration

No aliases. `/std/Bits.crs` and `/std/Bytes.crs` are deleted, `/std/Bin.crs` is added and registered in `std.crs`, and every consumer moves in the same change:

- Roughly 300 mentions across about 20 prelude modules (`Toml`, `Str`, `Map`, `Json`, `http`, `Parse`, `Fmt`, `Io`, `Char`, `Task`, `Flt`, `File`, `BigNat`, `NonZero`, `Eql`, the `tcp` tree, among others) plus the Curios programs under `curios/src/tests/`. `BigNat` and `NonZero` index by `Bool`.
- Printer vocabulary: the primitive type formers print as `Bin(Bool)`/`Bin(Byte)`; audit the `"Bits"`/`"Bytes"` strings in `curios-core/src/print.rs` and `curios-text/src/print.rs` and update the affected test expectations.
- The generated `/sys/Bits` and `/sys/Bytes` modules remain as internal carrier namespaces, referenced only by the two `satisfy` blocks; they are not re-exported by `/std`.
- SYNTAX.md: the packed-literal, match, and FFI sections adopt the `Bin(…)` vocabulary; the literal grammar itself (`b\`/`x\` prefixes, tight spread rules) is unchanged.
- Compiler-emitted literals are unaffected: string and character lowering construct `/syn` values over primitive packed carriers directly, so `curios-prelude/src/syntax.rs` is untouched.

## Validation

The standard gate: `make curios/runtime`, `cargo fmt --all -- --check`, `cargo check --workspace --all-targets --all-features`, Clippy with warnings denied, and the full test suite run once to a log. The prelude archive rebuild is itself a gate: it elaborates every witness and generated wrapper at compiler build, so a wrapper-substitution error cannot land silently. The wire rename additionally exercises the FFI parser and printer round-trip tests and the runtime engine tests; the bundle format is unchanged.
