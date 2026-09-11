# A fact is stated once, or the copies are checked

## Status

**Not refined yet.** An umbrella over four places where one fact is written down more than once with nothing holding the writings together. Each is separately landable and none blocks another; what they share is a failure mode, not a dependency.

## Why it exists

The repository already has this principle and already names it where it has been applied. `Intrinsic::signature` opens by saying so — "This is the one statement. The kernel walks it to check, elaboration walks it to elaborate" — and admits the third copy, `/sys`'s declarations, on the grounds that the prelude build *checks* it rather than that it does not exist. `peel_nat_terms` is "spelled once, in `curios-core`, which both congruences and the inverter call, so the two checkers remain incapable of disagreeing about it". `Nat::cancel_common` says "doing it here rather than at each consumer is what keeps the two spellings from drifting."

So the rule is not new: **one statement, or a copy something checks.** These are the places that have neither, collected because they were found together while a bound was being made to reach the kernel, and because each one cost that work time.

Drift is not hypothetical here. It has already happened in two of the four, and in neither case did anything report it — the copies were found by diffing them on purpose.

## What is stated twice

**The segment decomposition, three times.** Reading a value as measured pieces is implemented by `curios-core`'s `spine::Atom` for conversion, by `free_monoid`'s `bin_segments` for a literal run at a concrete index, and by `reduce::intrinsic::free_monoid`'s `concatenated` for a symbolic position. They differ in what a measure *is* — a `Nat` term, a `usize`, a `Nat` term again — and in whether a segment can be split, which is a real capability difference rather than an accident: `bin_segments` takes no `Reducer` and its own documentation calls the signature the audit. `Atom` is the richest, carrying a `Window` whose length is known where its contents are not, and it is the one that would decide `len(slice(b, s, l)) = l`.

The lift is a view parameterized by its measure. `measure`, `locate` and `window` in `curios-core`'s `free_monoid` are *already* generic over the carrier, with `bin_*` and `list_*` as thin wrappers, so the carrier axis is solved and the measure axis is the one nobody parameterized. A `Measure` with `zero`, a `consume` that reports overshoot rather than underflowing, and a `within` that answers `Option<bool>` — because a `usize` measure can always say whether an index lands inside a segment and a symbolic one generally cannot — covers both readers. `seam_window`, `concatenated` and `single_generator` fold into `locate` and `window` at the symbolic measure and stop existing. Two frictions to expect: segments are borrowed from the value today and a flattening producer must *construct* a singleton, so the pair becomes owned; and the symbolic producer needs a reducer where `bin_segments` must not have one, so the producer stays a parameter.

Folding `Atom` in is the larger half and changes what *conversion* decides, so it wants its own evidence rather than riding along.

**The fold arms, twice per grain and a third time per carrier.** `BinLen`, `BinEql`, `BinGet`, `BinSlice` and `BinAppend` each have a grain-X arm and a grain-B twin in `curios-core`'s `reduce::intrinsic`, about 290 lines on each side, and `ListLen`/`ListGet`/`ListSlice`/`ListAppend`/`ListConcat` are the same shapes again over the unpacked carrier. Grain-erasing the two `Bin` blocks and diffing behaviour puts `BinLen` at 100% identical, `BinEql` at 73%, `BinGet` at 70%, `BinSlice` at 71% and `BinAppend` at 28%.

The twins have drifted, and nothing caught it. `BinAppend` at the bit grain charges `Cost::buffer(width)` and at the byte grain does not — two prices for one operation, against the budget whose whole job is to bound memory. The byte arm also carries an `.unwrap()` the bit arm has no need for. `BinEql`'s bit copy lost both of the explanatory comments its byte original carries and builds its neutral term directly where the other uses the `bin_eql` builder.

`BinConcat` is already one grain-generic arm, which is the proof that sharing is available and that someone took it once. Three tiers, each landable alone: bind the grain instead of matching it, which collapses `BinType`, `Bin`, `BinLen` and `BinEql` with no semantic change at all; push the remaining branch to the element seam for `BinGet` and `BinSlice`, where `bin_piece` already shows the shape; and `BinAppend` last, because unifying it means *deciding* which of its two cost formulas is right, which is a question rather than a refactor.

What this buys is a grade. `intrinsic-fold-laws-and-the-free-monoid-peel.md` and `open-fold-laws-and-the-sum-normal-form.md` both carry one **argued** claim and it is this one — the bit-grain copies, whose only fixture is `bit_get_of_a_symbolic_cons_head_is_the_bit`. Collapse the twins and the grade becomes **probed**, because there is no second arm left to be wrong.

**A key's encoding, in the type and in every witness.** `/std/Map`'s `Key` declares `to_bytes(K) -> Bytes` and every witness assigns it `Hash/hash`, with the invariant — a type has one identity as bytes — stated in the concept's header and maintained by convention. A witness that assigned something else would compile, and the header would quietly become false.

`use Hash(K)` as a superclass edge makes it structural: resolution fills the slot, no witness writes the line, and the encoding cannot disagree with the hash because there is only one. Two things to probe before committing to it, neither of them an argument. Whether `Hash/hash(a)` resolves through the edge from *inside* the concept's own field telescope, the edge being anonymous and `injective`'s type needing to name what it compares. And whether `Eq(Hash/hash(a), Hash/hash(b))` reduces to `Eq(a, b)` at `Bytes`, since `Key(Bytes)`'s `injective(_, _, same) = same` depends on it; if it does not, that witness needs a real proof and the change costs more than it saves.

A named field is the trap worth naming. `hash: Hash(K)` reads better at `injective` but forces every witness to supply a dictionary explicitly, and `satisfy` registers an *anonymous* witness — so there is no registered `Hash(K)` to assign, and each `Key` would build a fresh one by hand. That is the same fact written twice again, one layer down.

## Where a copy is checked instead

Removing a copy is not always the answer, and the repository says when it is not. `/sys`'s declarations restate every intrinsic signature deliberately, because a reader calls the declaration rather than the table, and what makes that safe is that elaborating a `/sys` body checks its operands against `Intrinsic::signature` — a disagreement does not compile. `curios`'s `tests::laws` is the same move for the fold laws: the taken and declined halves are both written down, and each row is read back through the compiler's own refl-fit report, so a row cannot sit on the wrong side quietly.

So a second statement earns its place by being checked against the first, mechanically, on every build. What this specification is about is the statements that are not.

## Rejected

**One `Word` carrier parameterized by `Grain`, mirroring `Bin`.** It looks like the same unification and it is not: `Bin`'s grain parameter carries weight because the operations are shared, and the element types share no behaviour at all. The argument is recorded where the carriers are, in [numeric carriers narrow by refusing](../design/toolchain/numeric-carriers-narrow-by-refusing-never-by-changing-a-value.md).

**Folding the elaboration cost in.** A function type elaborates in its size times its binder count rather than its size, which is its own roadmap row and its own kind of defect — a cost, not a duplication. It is listed here only to say it does not belong here.
