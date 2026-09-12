# A fact is stated once, or the copies are checked

## Status

**Not refined yet.** An umbrella over two places where one fact is written down more than once with nothing holding the writings together. They are separately landable and neither blocks the other; what they share is a failure mode, not a dependency. A third place — the fold arms, a grain-X arm beside a grain-B twin for each of six operations — has been collapsed, and what that work established is kept below rather than lost with the section it came from.

## Why it exists

The repository already has this principle and already names it where it has been applied. `Intrinsic::signature` opens by saying so — "This is the one statement. The kernel walks it to check, elaboration walks it to elaborate" — and admits the third copy, `/sys`'s declarations, on the grounds that the prelude build *checks* it rather than that it does not exist. `peel_nat_terms` is "spelled once, in `curios-core`, which both congruences and the inverter call, so the two checkers remain incapable of disagreeing about it". `Nat::cancel_common` says "doing it here rather than at each consumer is what keeps the two spellings from drifting."

So the rule is not new: **one statement, or a copy something checks.** These are the places that have neither, collected because they were found together while a bound was being made to reach the kernel, and because each one cost that work time.

Drift is not hypothetical here. It happened in the fold arms and nothing reported it — the copies were found by diffing them on purpose, and the diff showed drift running in *both* directions: the bit copies had lost comments their byte originals carried, and one byte arm had lost a comment only its bit copy still held. Neither side was the master copy, which is what a copy with nothing checking it eventually looks like.

## What is stated twice

**The segment decomposition, three times.** Reading a value as measured pieces is implemented by `curios-core`'s `spine::Atom` for conversion, by `free_monoid`'s `bin_segments` for a literal run at a concrete index, and by `reduce::intrinsic::free_monoid`'s `concatenated` for a symbolic position. They differ in what a measure *is* — a `Nat` term, a `usize`, a `Nat` term again — and in whether a segment can be split, which is a real capability difference rather than an accident: `bin_segments` takes no `Reducer` and its own documentation calls the signature the audit. `Atom` is the richest, carrying a `Window` whose length is known where its contents are not, and it is the one that would decide `len(slice(b, s, l)) = l`.

The lift is a view parameterized by its measure. The carrier axis is now genuinely solved — one walk per seam on `FreeMonoid` — so the measure axis is what is left, and it is the smaller of the two: `locate`/`window` over a `usize` measure against `seam_window` over a `Term` one, each already generic over its carrier. A `Measure` with `zero`, a `consume` that reports overshoot rather than underflowing, and a `within` that answers `Option<bool>` — because a `usize` measure can always say whether an index lands inside a segment and a symbolic one generally cannot — covers both readers. `seam_window`, `concatenated` and `single_generator` fold into `locate` and `window` at the symbolic measure and stop existing. Two frictions to expect: segments are borrowed from the value today and a flattening producer must *construct* a singleton, so the pair becomes owned; and the symbolic producer needs a reducer where `bin_segments` must not have one, so the producer stays a parameter.

Folding `Atom` in is the larger half and changes what *conversion* decides, so it wants its own evidence rather than riding along.

**The carrier axis was the larger half, and it is done.** The spec used to say the carrier axis was solved because `measure`, `locate` and `window` are generic over it. That was true of the shared *walk* and false of the eight producers and readers feeding it — `bin_segments` beside `list_segments`, `concatenated` beside `list_concatenated`, and six thin wrappers, about 126 lines whose only difference was which nodes they matched. They are now methods on `FreeMonoid`, the closed carrier enum `uncons` already belonged to, over two seams: `spine` for a *measurable* node and `joined` for a juxtaposition, which differ in what they admit rather than in how they recurse. Two readers stayed one per carrier and the reasons are recorded beside them: `bin_shape` and `list_shape` answer different types and only one is fallible, and `bin_piece` and `list_piece` differ in what they need from *outside* the value — a grain is a tag the carrier holds, a `List`'s element type is a term it does not. That is the line: a reader folded into the carrier exactly when it needed nothing the carrier did not already carry.

**What the carrier axis turned up on the way.** `ListGet` had no seam rule where `ListSlice` had one, so a window at a symbolic seam was located and an index at the same seam was not — the asymmetry `Bin` had already closed, still open one carrier over, and a live incompleteness rather than a tidiness point. Closing it needed `list_concatenated` to flatten nested spellings as its `Bin` twin did; it had been the shallow one, so `ListSlice`'s own seam windows were weaker than `BinSlice`'s in the same way. Both are fixed and the rows are stated at `List` in `tests::laws`. Neither was visible from reading the two functions side by side — the grid found them, which is the argument for stating a law at every carrier before unifying anything.

**A key's encoding, in the type and in every witness.** `/std/Map`'s `Key` declares `to_bytes(K) -> Bytes` and every witness assigns it `Hash/hash`, with the invariant — a type has one identity as bytes — stated in the concept's header and maintained by convention. A witness that assigned something else would compile, and the header would quietly become false.

`use Hash(K)` as a superclass edge makes it structural: resolution fills the slot, no witness writes the line, and the encoding cannot disagree with the hash because there is only one. Two things to probe before committing to it, neither of them an argument. Whether `Hash/hash(a)` resolves through the edge from *inside* the concept's own field telescope, the edge being anonymous and `injective`'s type needing to name what it compares. And whether `Eq(Hash/hash(a), Hash/hash(b))` reduces to `Eq(a, b)` at `Bytes`, since `Key(Bytes)`'s `injective(_, _, same) = same` depends on it; if it does not, that witness needs a real proof and the change costs more than it saves.

A named field is the trap worth naming. `hash: Hash(K)` reads better at `injective` but forces every witness to supply a dictionary explicitly, and `satisfy` registers an *anonymous* witness — so there is no registered `Hash(K)` to assign, and each `Key` would build a fresh one by hand. That is the same fact written twice again, one layer down.

## What the fold arms established

Two things to carry into the work above, both earned by doing it rather than assumed before it.

**A one-sided check reads exactly like a passing one.** The bit-grain arms were not unchecked. `tests::laws` stated the free monoid's rows and every row it stated passed — but it stated most of them at the byte grain alone, and `eql` at neither, so the grade stayed **argued** while a green suite said nothing was wrong. The rows went in before any arm moved, which is what turned every later step into a refactor with an oracle instead of a change with a hope; state the segment and window rows at `List` as well as at both `Bin` grains, and state them first.

**An asymmetry can be earned.** `BinAppend` charged `Cost::buffer(width)` at the bit grain and not at the byte grain, which reads as drift and was not: each formula was right for its own implementation, because `append_bit` rebuilt the value through `from_bits` and materialized a `bool` per bit where `append_byte` copied the packed payload. The fix was the implementation, not the formula, and one formula priced both appends afterwards. Before unifying any copy above, check whether the versions differ for a reason — where they do, the reason is the thing to remove, and unifying over it would have written the accident into the budget permanently.

## Where a copy is checked instead

Removing a copy is not always the answer, and the repository says when it is not. `/sys`'s declarations restate every intrinsic signature deliberately, because a reader calls the declaration rather than the table, and what makes that safe is that elaborating a `/sys` body checks its operands against `Intrinsic::signature` — a disagreement does not compile. `curios`'s `tests::laws` is the same move for the fold laws: the taken and declined halves are both written down, and each row is read back through the compiler's own refl-fit report, so a row cannot sit on the wrong side quietly.

So a second statement earns its place by being checked against the first, mechanically, on every build. What this specification is about is the statements that are not.

## Rejected

**One `Word` carrier parameterized by `Grain`, mirroring `Bin`.** It looks like the same unification and it is not: `Bin`'s grain parameter carries weight because the operations are shared, and the element types share no behaviour at all. The argument is recorded where the carriers are, in [numeric carriers narrow by refusing](../design/toolchain/numeric-carriers-narrow-by-refusing-never-by-changing-a-value.md).

**Folding the elaboration cost in.** A function type elaborates in its size times its binder count rather than its size, which is its own roadmap row and its own kind of defect — a cost, not a duplication. It is listed here only to say it does not belong here.
