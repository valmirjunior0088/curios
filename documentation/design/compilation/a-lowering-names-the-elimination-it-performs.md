# A lowering names the elimination it performs

**Decision.** Where a level below Core eliminates a sequence, it names the elimination as a form of its own rather than open-coding it out of indexed reads. `curios-ersd` has two, `FoldSequence` for a cons arm that uses its induction hypothesis and `UnconsSequence` for one that ignores it, and both reach `into_cont::emit_peel`, the one implementation of taking a sequence apart, which emits a read at an index and a suffix — `curios_cont::Intrinsic::BinRest`/`ListRest`, `(seq, start) -> seq`, naming no count. No compiler pass derives a window's extent from a length it was handed. A new elimination form is added to `curios-ersd`'s `optimize/rebase.rs` tail-position dispatch forms and to `curios-cont`'s `WindowFamily::of`, whose pattern lists no exhaustiveness check reaches.

**Rationale.**

- **The partiality is lowering's, so the form must be too.** Core's free-monoid eliminator is total and the kernel checks a match as a match; implementing it from `get` and `slice` introduces bounds nothing below Core can state, since erasure deletes propositions and `curios-ersd`'s verifier checks arity rather than values.
- **Open-coding twice is two conventions.** A peel open-coded in erasure and again in the fold lowering can disagree about a convention neither states, over operands that are `Nat` either way, and then the workspace builds, the kernel certifies the library, and every program touching a string traps — the case of a window respelled as a start and a count under two such copies.
- **A suffix that takes no count has nothing to derive**, so the derivation happens where the length lives — the rope helper's `len` field and the virtualized region's `length` — as a fact about a value in hand.
- **A pattern list is not checked.** A form missing from `rebase.rs` leaves a sequence recursion non-tail, overflowing at sixty thousand elements, silently; `tests::runtime::loop_tests::arena_deferred_context_recursion_is_stack_safe_at_depth` holds the `UnconsSequence` case.
- **What remains is off the soundness board**: the fold loop's `i <= len` is a runtime fact trapped rather than checked, detected only by the cross-stage corpus in `curios`.

**Rejected.**

- **A suffix operation without the control form**, which leaves each lowering open-coding a peel out of two operations.
- **Lifting the case split above erasure**, so its reads become Core terms the kernel checks: the peel needs `1 + (len - 1) <= len`, a `/std` lemma the compiler would reach through a registry slot to prove — a compiler that constructs proofs, the defect a reducer that does is.
- **Checking below erasure**, where a window's bound is a runtime length and the trap is the check.
- **Emitting window fields directly from the fold**: regions are found by a whole-module walk after the module is built, so the lowering must emit something concrete for the pass to recognize.
