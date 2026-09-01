# An operation whose meaning needs a width belongs to the carrier that has one

## Status

Not refined yet. This file records the decision that created the gap and the surface as it stands today; the design space is untouched and none of the questions below has been answered. Nothing is started.

## Why it exists

[Numeric carriers narrow by refusing, never by changing a value](../design/toolchain/numeric-carriers-narrow-by-refusing-never-by-changing-a-value.md) removed rotation and leading-zero counts from the surface, on the ground that they "narrowed through `to_u32` to answer, which made them 32-bit notions wearing an unbounded type and left them *stuck* above `2³²` — partial in the theory." That decision named the destination in the same sentence: "a width-carrying `/std/Bits` is where they belong." This specification is the location that sentence was pointing at, and the operations it deleted have had nowhere to live since.

## What is certain

Read from source.

- **The packed roster carries no bit arithmetic at all.** `curios-core/src/intrinsic.rs` gives `Bin` a type former, a literal, `BinLen`, `BinEql`, `BinGet`, `BinSlice`, `BinAppend` and `BinConcat`, each parameterized by a `Grain` so one roster serves `Bits` and `Bytes`. There is no conjunction, disjunction, exclusive or, complement, shift, rotation or population count on the roster, at either grain.
- **`/std/Bits` adds no arithmetic either.** It re-exports `get`, `len`, `eql` and `slice` from `/sys`, and adds `try_get`, `flatten` and `fold`. So the only way to combine two words today is a fold that walks one bit at a time, which is linear in the width for an operation the machine does in one instruction.
- **`Nat` and `Int` have the family and are not the wide path.** `NatAnd`, `NatOr`, `NatXor`, `NatShl` and `NatShr` exist with their signed twins, and a result leaving the i31 envelope traps at the backend boundary rather than answering — `curios`' `tests::numeric::envelope_tests::overflowing_computations_trap_at_the_backend_boundary` is what pins it. Values past the envelope belong to `/std/BigNat`, which is itself built over `Bits`.
- **The width the decision asked for already exists.** A packed value's width is its length, and `BinLen` reads it. Nothing has to be added to carry a width; what is missing is operations that relate two values through it.
- **Adding a row is adding to the trusted base.** `Intrinsic::signature` is the source of truth both checkers walk rather than restate, and the reduction rules a new operation would need are graded in [Intrinsic fold laws and the free-monoid peel](../soundness/per-term-rules/intrinsic-fold-laws-and-the-free-monoid-peel.md). A library function over `fold` costs the trusted base nothing.
- **A narrow packed value has exactly one runtime form.** [The map wall falls by classes, not by symptom](../design/toolchain/the-map-wall-falls-by-classes-not-by-symptom.md) states the small-canonical invariant: a `Bits` of at most 26 bits is an i31 and every producer normalizes on the way out. Any new operation is a producer and owes that normalization.

## What has to be decided

- **Which operations.** Rotation and leading-zero counts are the two the numeric decision named, and they are the reason this file exists. Conjunction, disjunction, exclusive or, complement and population count are the obvious neighbours; shifts are the ones whose meaning is least obvious here, below.
- **Intrinsic or library.** This is the central question and it decides everything else. An intrinsic buys a constant folder, a real instruction and a signature both checkers read; it also enlarges the trusted base at exactly the place the perimeter grades weakest. A library function over `fold` is free of that and pays the width in time at every call.
- **What a binary operation does with two widths.** Refusing an unequal pair is the answer the carriers decision would give, since a narrowing may refuse a value and may never change one; zero-extending the shorter operand is the alternative and it silently invents bits. Whichever is chosen is a precondition in the `/sys` declaration if it refuses.
- **What a shift means when the width is the value's own length.** Growing the value and dropping bits off the top are different operations, and `Nat/shl`'s answer — trap when the result leaves the carrier — has no analogue on a carrier with no fixed width.
- **Whether `Bytes` gets the same family.** One `Grain`-parameterized roster serves both today, so a row added for one is nearly a row added for both, and a byte-grain rotation is a different notion from a bit-grain one.
- **If intrinsic: the fold law per operation, and whether it is stated over values or over the packed representation.** Both checkers must decide the same way, and `curios-num`'s `scalar` is where the erased carriers' semantics are stated.

## Deliberately not specified

The surface spellings. Whether any of this dispatches through a concept rather than being named directly. Whether `/std/BigNat` is rewritten to use whatever lands here — it is a consumer, and a consumer's migration is its own change.
