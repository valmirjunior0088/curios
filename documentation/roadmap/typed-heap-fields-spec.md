# A monomorphic field carries its own type

## Status

Unrefined, not started, deliberately. This file records what the map-wall campaign learned about the shape of the work — enough that the campaign can be priced and scheduled without re-deriving it — and no more; the refinement pass that would harden its claims into a schedule has not run. The one commitment already made is sequencing: **a census session precedes any go/no-go**, because the campaign's size demands numbers before representation moves — the "replace, don't measure the wrong structure" lesson applied in advance rather than in hindsight.

## Why it exists

Every constructor and product field in emitted code is `(ref null any)`, so every projection pays a `ref.cast` to the field's actual shape. The variant-collapse decision names field representation as its successor's subject, and the map-wall campaign priced one instance concretely: the declined qp reshape's child-array access kept "a tag test and a `ref.cast` that a raw wasm array would not pay" — a residual this campaign would delete for every consumer at once, instead of minting a per-consumer carrier (the shape the reshape's rejection recorded). The other measured beneficiary class is monad-heavy code, where every `TupleGet` behind the dictionary machinery casts what erasure already knew the shape of.

## The shape of the work

Erasure — the one walk still holding Core field types — records each field's concrete erased heap shape where it is monomorphic (a tuple of known arity, a packed rope, a list rope, a closure environment, an always-immediate scalar), following the `FieldShape` precedent exactly: a fact recorded above, spent below, with `Opaque` the conservative point that only ever misses an optimization. The door carries the fact onto Cont tuple types; the emitter declares per-shape struct fields and drops the cast at every projection whose field is shaped.

Boundaries already drawn, from the map-wall campaign's evidence:

- **Constructor and product fields only.** Rope *payloads* stay uniform: typing a list's element storage per element type is exactly the minted-parallel-carrier shape the qp reshape's rejection priced and declined.
- **Always-immediate fields narrow to `(ref i31)` for free** — the admission data already exists as `FieldShape::Immediate`, and after the small-canonical landings it is settled that sometimes-immediate packed carriers stay `Opaque` there, so no new analysis is needed for this slice.
- **Subtyping stays one-directional.** A shaped field read into a uniform context is free; the reverse direction must be unrepresentable, not audited — the `Repr::Bin(Grain)` carrier split is the in-tree precedent for making a confusion structurally impossible rather than checked.

## What makes it the largest of its cohort

Three structural costs, named so the census prices them rather than discovers them:

- `find_tuple_type` keys tuple types by arity alone; shaped fields re-key by shape vector, with the type-count management and canonical ordering that implies.
- `cps/fields.rs`'s split/rebuild machinery and the return protocol must thread shapes through parameter splitting — the seam the value-lifetime campaign took a full campaign to land.
- Every projection site in the emitter, and the host boundary's uniform lifts, must agree about which side owns the coercion.

## The census that gates it

A read-only session over `/std` and the corpus, producing: the population of monomorphic-at-erasure fields by shape class; the count of emitted `ref.cast`s per class that shaping would delete; and the tuple-type population growth under shape-vector keying. The go/no-go reads those three numbers against the campaign's cost above. Until that session runs, this specification stays unrefined and makes no schedule claims.
