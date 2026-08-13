# You should pay for a value when you keep it, not when you name it

**This specification is not worked out.** It states an idea and the evidence that produced it, and stops there deliberately. It has no milestones, no acceptance criteria and no design, because inventing them now would repeat the mistake its predecessor was rewritten to fix: naming a mechanism before a measurement justified one. What follows is a thesis and a body of evidence, and the next person to pick it up owes it a survey before a line of code.

## The idea

A value that is created and consumed inside one loop iteration, and never escapes it, should cost nothing to allocate. Today it costs a heap allocation and a runtime allocation call, and the source gives no sign of the difference — naming a value and keeping a value read identically.

That is the whole thesis. Everything below is why it is worth believing.

## What the string-walk campaign found

It set out to remove a closure chain from `/std/Str/fold` and ended up somewhere else. Three changes landed, and the ladder in `curios/src/tests/codegen/ladder.rs` carries every figure:

| Change | What it removed |
| --- | --- |
| The lowering stopped materializing a fold suffix nothing reads | one allocation per element, in every sequence fold in the language |
| `Str/fold` threaded its accumulator through the recursion | a closure environment and an indirect call per character, and a whole traversal |
| Emptiness stopped being decided by `count_scalars` | a second walk of the string per parse |

`programs/parse_digits.crs` went from 2.31 s to 0.90 s at N = 1 000 000; the gap against the hand-written control went from fourteenfold to under sixfold.

**Every one of those was an allocation or a traversal that the source did not admit to.** None was the mechanism the predecessor specification proposed.

## What one character still costs, and why two of the five belong here

Counted in the emitted body of `/std/Str/fold`, one arm per character: a byte read, a rope-view allocation for the tail, the UTF-8 scan, an indirect call through `f`, and an accumulator-tuple allocation.

The scan is work the abstraction performs. The indirect call is its own roadmap item. The two allocations are this document's subject, and **neither escapes its iteration**:

- **The suffix view.** `match b | x[h, ..t]` binds a tail, and the walk passes it to the recursive call. It is read and dropped in the same step.
- **The accumulator tuple.** `Str/fold` threads `{A, Nat}` — the result and the codepoint under construction — so every step allocates a fresh pair. Nobody wrote it; it exists because of the fold's signature.

The second is the better example precisely because no one chose it.

## Three tiers, only one of which is settled

**Unused: do not materialize.** Landed, in `curios-ersd/src/into_cont.rs`. It has to live in the lowering: a slice may trap, so no later pass may drop one for having a dead result, and the reason *that* slice cannot trap is a property of the loop the lowering itself emits. Of five fold sites in the prelude, exactly one reads its tail at runtime — and it does so only to ask whether it is at the last element.

**Used but not escaping: materialize the fields, not the object.** Unbuilt, and the substance of this document. A rope view is `(base, offset, length)`; every read-through consumer — `len`, `get`, a further slice — reads those fields. Holding them in locals would make the tail free *however the use is spelled*, which is the property peepholes cannot deliver: they fix spellings, this fixes the thing being spelled.

**Escaping: allocate.** Correct, and then the cost is earned.

Two obligations anyone building the middle tier inherits. It belongs in `curios-cont` by the thin-arena rule, where the only escape notion today is one conservative test in `represent.rs` — "is this referenced from another function's body" — used to withdraw an unboxing offer. And the emitted `slice` helper does three things: bounds-check, force an uncached base, and allocate the view. Only the third may go.

## What it inherits

**Does Curios promise that both spellings of a dependently typed left fold cost the same?** The predecessor left this open and it does not dissolve because `/std/Str/fold` was rewritten: a user writing the induction form still pays what the library stopped paying. It is a cost-model question of exactly this document's kind — a promise about when a program pays — and it is recorded here rather than in `documentation/DESIGN.md` so that it is answered together with the rest, or refused in writing.

## Evidence that would change the thesis

A survey finding that non-escaping allocations are rare outside `/std`, or that the two per-character allocations above are a small share of what remains. The share is **unmeasured**: dividing the five costs above needs one instrument each, not another control program, and the honest order is to measure before building — which is the one habit the predecessor campaign was worth having.
