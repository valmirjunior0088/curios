# A monomorphic field carries its own type

## The issue

Every constructor and product field in an emitted module is `(ref null any)`, and every function signature is uniformly `anyref`. Four costs follow, with one engine mechanism under three of them:

- **Every scalar field read unboxes and every store boxes.** A `Nat` held in a heap field costs `ref.cast (ref i31)` plus `i31.get_u` per read and `ref.i31` per store — several hundred such sites in each optimized workload, with the hot-loop share priced by the census's paired-fold probe below.
- **Every load of a rope from a uniform position is a host call.** The `Bytes`/`Bits` box helpers end in `ref.cast (ref $rope/bin)` and a `List` load casts to `$rope/list` directly; both bases are non-final and the object is always a leaf, node, or view subtype, so Wasmtime's exactness fast path never hits and the check is the `is_subtype` libcall — the mechanism [a tuple is read at its own final type](../design/toolchain/a-tuple-is-read-at-its-own-final-type.md) records, whose deletion for tuples measured −61% on `chain` (`a_tuple_is_read_at_its_own_final_type`, in `curios`'s codegen tests).
- **Every closure dispatch pays that class twice.** The apply site casts to the non-final `$envr/N`, and `call_indirect` against the plain `funcref` table runs the full runtime signature check — the callee's named final func type never equals the expected non-final `$clsr/N`, so both checks are libcalls. Wasmtime deletes the signature check at compile time when the table's element type equals the call site's expected type, and reduces it to an inline compare when the two type indices are equal; neither exit is taken today.
- **Binaryen's refining passes are starved.** The optimizer already runs closed-world, and `TypeRefining`/`GlobalTypeOptimization` narrow nothing, because one arity-keyed type serves every constructor module-wide and the join over any field's stores is the top type by construction. Binaryen's own guidance to WasmGC toolchains is to emit the most refined field types possible and distinct types per semantic class.

None of this reflects missing knowledge. Erasure still holds every field's Core type and records only immediate-or-not; `cps/fields.rs` already travels a variant region at its family's width behind a tag; `FamilyEncoding` already decides layout per family. The emitted types throw all of it away — the lowering records nothing of what erasure knows, and the emitter types nothing of what the IR does.

External precedent, briefly. Lean 4's runtime stores the constructor tag in the object header and packs scalar fields unboxed, recording per-field layout in its IR; Kotlin/Wasm, J2CL, and dart2wasm emit nominal per-class structs with field types narrowed specifically to avoid casts; wasm_of_ocaml and Wasocaml keep OCaml's uniform representation only because untyped `Obj.field` demands prefix access — an obligation Curios does not have. V8's own measurement of statically-unnecessary casts found "easily 2×" in tight loops even with inlined checks; Wasmtime does not inline the non-exact half at all.

## The approach

Three mechanisms, cheapest first. The first needs no gate; the second *is* the census; the third is the campaign the census gates.

**1. Say the truth about closures.** Declare each closure body at its arity's `$clsr/N` type — the per-closure named func types exist only to type the body declarations, so they fold away — and give each arity its own dispatch table typed `(ref null $clsr/N)`. The body declaration alone turns the signature libcall into an inline always-true compare; the typed table deletes the check at compile time. No IR change and no representation change; the probes are the monadic slopes and the `chain`/`spines` anchors.

**2. Record shapes at erasure.** Generalize `FieldShape` from immediate-or-opaque to the full erased shape of a kept field — a scalar carrier (`Nat`/`Bool`/`Byte`/`Int`, or `Flt`), a packed grain, a list, a closure of known arity, a product of known schema, a variant family, or opaque — by extending the `reduce_with` walk `classify.rs` already runs over every field's declared type. Opaque stays the conservative point that only ever misses an optimization. Carry the row through the door on constructor and product schemas. Running the recorder over `/std` and the corpus is the census: the population of each shape class, the cast and box sites each would delete, and the type-count growth under family keying — plus one dynamic probe, a paired fold over two families differing only in one carried `Nat` field, which prices the scalar class per element. The go/no-go for mechanism 3 reads those numbers. This mechanism also subsumes carrier-aware immediate payloads: an arm binding an always-immediate payload can bind it at its raw carrier once the recorded shape names one.

**3. Key heap types by family, and type the slots.** One **final** struct per variant family, replacing the arity-keyed tuples for family values:

- The tag is an `i8` field read by `struct.get_u` — no unbox, and the engine packs it at one byte.
- Constructors take disjoint slot ranges, each slot typed by its recorded shape: `i32` and `f32` scalars raw, monomorphic references concrete — including self-references, so a fork's children are `(ref null $node)` and the descent loop is two `struct.get`s and a compare — and polymorphic slots `(ref null any)`. Disjoint-first is the decided starting point: `GlobalTypeOptimization` removes slots nothing reads, and the width probe below decides whether class-partitioned overlap (scalars over scalars, references over references — Lean's layout) is needed.
- Finality makes every cast exact. A family value re-entering from an `anyref` position is one exact `ref.cast`; a family read from a typed field is none; the roster cascade survives only for the products that remain structural.
- Products get the same treatment by schema. `Collapsed` and `Immediate` encodings are unchanged — nullary and immediate constructors keep riding the i31.
- The parameter-split seam composes instead of fighting: a region's travel width is the family width by construction, so splitting a family parameter yields the raw `i32` tag plus typed payload parameters, and a filler materialises at its slot's own carrier.

## What it costs

Three structural costs, named so the census prices them rather than discovers them:

- `find_tuple_type` keys by arity; family keying adds one type per family and product schema in the closed world, and the census reports the count.
- `cps/fields.rs`'s split and rebuild machinery and the return protocol must thread shapes through parameter splitting — the deepest seam, though the width rule now agrees by construction.
- Every projection site in the emitter and the host boundary's uniform lifts must agree about which side owns each coercion, and narrow constructors allocate at family width — a bounded growth the width probe holds against the trees precedent that live bytes convert to time under the all-live collector.

## Boundaries

- Rope payloads stay uniform: typing a list's element storage per element type is the minted-parallel-carrier shape already priced and declined.
- Sometimes-immediate packed carriers stay untyped in fields — a `Bytes` inside its envelope rides the i31, so no single heap type names its population; the grain still travels on the load.
- Function signatures stay uniformly `anyref`. Typed calling conventions are a successor this specification does not schedule.
- The `$envr/N` cast at dispatch has no free fix — the per-closure subtypes are what type the captures — and stays as the priced residue, its candidate shapes (a code-index-beside-environment pair, captures behind one indirection, or the cached libcall as-is) left to their own probe.

Engine facts this rests on, verified in Wasmtime 47's sources: references including `anyref` are 4 bytes, so a typed `i32` field saves instructions rather than bytes; `i8` and `i16` struct fields genuinely pack; an inline `f32` field deletes the 16-byte `$flt` box outright; casts are checked inline only when the target is final or the type indices are equal, and `call_indirect`'s check is deleted when the table's element type matches statically.
