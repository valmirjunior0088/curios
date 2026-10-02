# A variant collapses when nothing needs to distinguish it

**Decision.** A variant family's runtime encoding is decided per family at the one-way door, `curios-ersd`'s `lower_to_cont`, as a pure function of its registered schema, and a family pays for discrimination only where discrimination can happen:

- a **single-constructor family** collapses as the struct with the same relevant row erases: one payload is the bare value, several an untagged row, none the interned `Nat` zero, and its matches do not dispatch;
- a **multi-constructor family with exactly one immediate-unary constructor** — one carrying one always-immediate payload, or one `Nat` or `Int` — rides that constructor as its bare payload while the siblings keep their rows, and a match opens with an `IsImmediate` test (`ref.test (ref i31)`, and `ref.test (ref $big)` behind it) instead of a tag read, which is never read at all when one boxed constructor remains;
- **every other family** is a row whose slot zero is its tag ([A field is declared at the carrier its shape names](a-field-is-declared-at-the-carrier-its-shape-names.md)).

Eligibility is recorded at erasure, the last walk holding the types: each relevant payload field of a constructor carries a `FieldShape`, `Immediate` iff every runtime value of its type is an immediate — `Bool`, `Byte`, or a chain of single-field collapses landing on one — and `Number` iff every one is a `Nat` or `Int`. A cycle through a self-referential struct and a polymorphic payload classify `Opaque`. Every construction of a row family is padded to the row's width, so a family value has one width at rest and in flight: a parameter every flow reaches as constructions of one family travels as its row's fields ([A value costs when it is kept, not when it is named](a-value-costs-when-it-is-kept-not-when-it-is-named.md)), and a constant discriminant is found by the passes that already fold one, projection forwarding and jump threading.

**Rationale.**

- **The door decides because the tag is born there.** Ersd keeps `Construct` and `MatchVariant` semantic, so there is nothing to erase earlier, only a dispatch encoding to decline; deciding per family from the schema alone makes every construction and match agree with no shared state, over a whole-program lowering.
- **`IsImmediate` is asked only where its answers are disjoint by construction**: the bare constructor's payload is an i31 or a `$big`, every sibling a row, and `$big` a final type no row can be. A packed payload is not admitted, since its boxed rope is not cheaply told from a row, and two immediate-unary constructors would collide on the same immediates.
- **A wrapper adding no information adds no representation**, which removes the cost asymmetry between `struct Meters { Nat }` and `induct Meters | m(Nat) end` for values at rest, where call-pattern specialization cannot reach. The immediate encoding, OCaml's int-versus-block split drawn per family, removes the leaf allocations of tree-shaped data — half the objects of the binary-trees workload (`trees_leaf_rides_its_payload`).
- **Padding at rest is the price of one heap type per family**, which is what lets a slot's carrier be read downstream, and grouping slots by carrier limits the filler to the slots constructors disagree on. One width also lets a variant in flight split as a product does: the per-character path of an idiomatic UTF-8 walk allocates nothing, `/std/Str/step` taking its scan as field parameters (`curios`'s `tests::codegen` ladder).
- **A wrongly `Opaque` field misses an encoding and a wrongly `Immediate` one corrupts it**, so eligibility errs toward `Opaque`.

**Rejected.**

- **Relying on the optimizer**, which a heap field, an escape or an unknown merge defeats; a representation guarantee must hold where the analyses cannot see.
- **Nullary constructors as bare immediates**, which collide with an immediate constructor's payloads, where nullary rows are interned constants already.
- **Dropping the boxed side's tag when the test discriminates**, which shifts every payload index for no byte the collector's 16-byte alignment does not absorb.
- **Constructions left at their own widths and merged per width in flight**: a site declines a source whose flows report several widths, a surviving narrow use materializes at its widest sibling anyway, and the discriminant cannot be recorded where a resume rebuilds a constructor with its tag in a parameter, as constructions in the UTF-8 scan's region do.
- **Variant-width classes in the return protocol**: an immediate family's return edges differ in shape, a bare payload on one and a row on another, and the corpus's variant-width returns either escape or store their results in a parent, where splitting relocates the allocation rather than removing it.
