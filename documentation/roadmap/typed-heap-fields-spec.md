# A monomorphic field carries its own type

## The issue

Every constructor and product field in an emitted module is `(ref null any)`, and every function signature is uniformly `anyref`. The costs that remain, each with its measurement beside the probe that retakes it:

- **Every scalar field read unboxes and every store boxes.** One always-boxed `Nat` field prices at 4.13 ns per element — 17% of a dispatch-heavy fold's per-element budget — and the share holds as the loop around it gets faster, which is what makes it a representation tax rather than a site cost (`boxed_field_read_measurements`, `curios/src/tests/codegen/shapes.rs`). It is the largest static population in every corpus program: 76–109 unbox casts and 143–205 box/unbox instructions each (`field_shape_census`, same file).
- **Every load of a rope from a uniform position is a host call.** The `Bytes`/`Bits` box helpers end in `ref.cast (ref $rope/bin)` and a `List` load casts to `$rope/list` directly; both bases are non-final and the object is always a leaf, node, or view subtype, so Wasmtime's exactness fast path never hits and the check is the `is_subtype` libcall — the mechanism [a tuple is read at its own final type](../design/toolchain/a-tuple-is-read-at-its-own-final-type.md) records, whose deletion for tuples measured −61% on `chain`. The census counts 51–72 such sites per program.
- **Binaryen's refining passes are starved.** The optimizer already runs closed-world, and `TypeRefining`/`GlobalTypeOptimization` narrow nothing, because one arity-keyed type serves every constructor of an arity module-wide and the join over any field's stores is the top type by construction. Binaryen's own guidance to WasmGC toolchains is to emit the most refined field types possible and distinct types per semantic class.

None of this reflects missing knowledge — the knowledge is now *recorded*. Erasure writes every kept field's erased shape onto the constructor and product schemas (`FieldShape`: immediate, flt, packed grain, list, closure arity, product width, family, opaque), the recorder's end-to-end pin holds (`a_recorded_shape_survives_to_the_program_schema`), and `cps/fields.rs` already travels a variant region at its family's width behind a tag. The emitted types are the one place still saying `anyref`.

External precedent, briefly: Lean 4's runtime stores the tag in the object header and packs scalar fields unboxed, recording per-field layout in its IR; Kotlin/Wasm, J2CL, and dart2wasm emit nominal per-class structs with field types narrowed specifically to avoid casts; wasm_of_ocaml and Wasocaml keep the uniform representation only because untyped `Obj.field` demands prefix access — an obligation Curios does not have.

## The census, and what it gates

The census this campaign was gated on is taken, and its figures live beside the probes in `curios/src/tests/codegen/shapes.rs`. What they say: **116 of 149 recorded fields — 78% — are monomorphic at erasure** (34 immediate, 19 bytes, 18 product, 15 family, 13 list, 11 closure, 4 bits, 2 flt, against 33 opaque); the box/unbox class is the largest static population in every program; and family keying replaces the 4–5 arity-keyed tuple types with the roster's 58 nominal types, a growth the closed-world passes are built to consume. Beside it, the collector's share of the `spines` insert measured nil (`spines_collection_decomposition`), so the representation tax is not sharing that workload's remainder with collection work. The go/no-go on the approach below reads those numbers.

## The approach

One mechanism: **key heap types by family, and type the slots.** One **final** struct per variant family, replacing the arity-keyed tuples for family values:

- The tag is an `i8` field, read by `struct.get_u` — no unbox, and the engine packs it at one byte (Wasmtime 47 layout, verified in source: `i8`/`i16` fields genuinely pack; references including `anyref` are 4 bytes, so a typed `i32` field saves instructions rather than bytes; an inline `f32` field deletes the 16-byte `$flt` box outright).
- Constructors take disjoint slot ranges, each slot typed by its recorded shape: `i32` and `f32` scalars raw, monomorphic references concrete — including self-references, so a fork's children are `(ref null $node)` and the descent loop is two `struct.get`s and a compare — and polymorphic slots `(ref null any)`. Disjoint-first is the decided starting point: `GlobalTypeOptimization` removes slots nothing reads, and the width probe below decides whether class-partitioned overlap (scalars over scalars, references over references — Lean's layout) is needed.
- Finality makes every cast exact: a family value re-entering from an `anyref` position is one exact `ref.cast`, a family read from a typed field is none, and the roster cascade survives only for the products that remain structural. Products get the same treatment by schema.
- `Collapsed` and `Immediate` encodings are unchanged — nullary and immediate constructors keep riding the i31.
- The parameter-split seam composes instead of fighting: a region's travel width is the family width by construction, so splitting a family parameter yields the raw `i32` tag plus typed payload parameters, and a filler materialises at its slot's own carrier.

## What it costs

Three structural costs, priced by the census where it could and by probes where it must:

- `find_tuple_type` keys by arity; family keying adds the roster's 58 nominal types in the closed world — counted, and not a scaling concern.
- `cps/fields.rs`'s split and rebuild machinery and the return protocol must thread shapes through parameter splitting — the deepest seam, though the width rule now agrees by construction.
- Every projection site in the emitter and the host boundary's uniform lifts must agree about which side owns each coercion, and narrow constructors allocate at family width — a bounded growth the width probe holds against the trees precedent that live bytes convert to time under the all-live collector.

## Boundaries

- Rope payloads stay uniform: typing a list's element storage per element type is the minted-parallel-carrier shape already priced and declined.
- Sometimes-immediate packed carriers stay untyped in fields — a `Bytes` inside its envelope rides the i31, so no single heap type names its population; the grain still travels on the load. The rope-base libcall class therefore shrinks to `anyref` re-entries rather than vanishing; typing the *helpers'* entries at the base type is part of this campaign, making the bases' remaining casts exact is not ruled on here.
- Function signatures stay uniformly `anyref`. Typed calling conventions are a successor this specification does not schedule.
- The `$envr/N` cast at dispatch has no free fix — the per-closure subtypes are what type the captures — and stays as the priced residue, its candidate shapes (a code-index-beside-environment pair, captures behind one indirection, or the cached libcall as-is) left to their own probe. The `call_indirect` signature check beside it is already deleted: the per-arity tables are typed, and `closure_index_dispatch_measurements` holds what that was worth.
