# A type-level concatenation should not copy what it joins

This is the implementation specification for making a sequence value cost the same to build at the type level as it costs to build at runtime.

## Status

A performance capability over the free-monoid carriers — `Bin` at both grains, and `List` — where the type level and the runtime currently disagree about what a concatenation costs.

It rests on [A reduction step costs what it builds](01_PRICED_REDUCTION_SPEC.md) for the bound. Pricing decides that a runaway reduction refuses; this decides that the ordinary program never approaches the limit. Both are wanted, and neither substitutes for the other: a limit that the obvious spelling reaches is a limit that reads as a language defect, and a cheaper representation still needs something to stop a genuinely unbounded computation.

Scoped to reduction and conversion. The surface language, the erased IR, the host ABI and the runtime are unchanged.

## The evidence, and where it already lives in the tree

`curios/src/tests/runtime.rs`'s `accumulation_loops_are_linear_by_construction` records why the runtime uses a rope: the representation it replaced "copied the accumulator per step (Θ(n²), tens of minutes at this size)". The type level still uses that representation.

The spec-13 reproducer measures both sides of the resulting asymmetry. The same accumulate-then-slice loop is linear when the program runs it and quadratic when a decided bound makes the compiler evaluate it, and compile-time evaluation of a small fraction of the runtime measurement's size already costs gigabytes. The figures live beside that probe; what they decide is recorded here.

The user-facing consequence is the reason this is a roadmap item rather than a tuning note. `Bytes/slice` states `10 <= Bytes/len(b)`, so writing the obvious thing — accumulate a value, then take its head — is what triggers the evaluation. The workaround is to route the subject through a parameter so reduction stops at it, and `accumulation_loops_are_linear_by_construction` carries exactly that helper, labelled in place as a workaround.

## The cause is one policy, not a representation limit

`normalize_concat` in `curios-core/src/free_monoid.rs` fuses an all-literal operand set into a single value, and `PackedBin::concat` allocates an exact-sized buffer to do it — twice, since the filled `Vec<u8>` is then converted into an `Arc<[u8]>`. Each step of an accumulation therefore copies everything accumulated so far, and the elaborator's reduction cache retains every distinct copy.

The concatenation node that would have cost one allocation is destroyed on the way in, before anything downstream can benefit from it.

## The machinery for the unfused form already exists and is unreachable

This is what makes the capability small, and it is the reason to take it rather than to accept the cost.

`Bin/len` and `List/len` are already monoid homomorphisms. `reduce_homomorphism` distributes them over a `Concat`, summing the operands' lengths without reading a byte. A length is exactly what a bound needs, and the homomorphism never fires because fusion has already replaced the concatenation with a literal.

Conversion already decides the two spellings equal, independently and in both checkers. `bin_atoms` and `list_atoms` flatten nested concatenations and merge adjacent literal runs, so `Concat(literals)` and the fused literal decompose to the same segment list; `curios-elab`'s and `curios-cert`'s intrinsic conversions both route through `peel_bin`/`peel_list` before any structural comparison. Fusion is therefore not what makes those values definitionally equal.

Erasure already lowers `BinConcat` and `ListConcat` n-ary into the runtime's rope operations, and it lowers from the elaborated term rather than from a reduct, so a value that stops fusing at the type level changes nothing about what is emitted.

## Decision

Fusion becomes an optimization for small runs rather than a normalization obligation.

Literal operands fuse while each is below a documented size cap; a concatenation with an operand above the cap keeps its `Concat` node with the operands as they are.

Capping by *operand* size rather than by result size is the load-bearing choice. Under a result cap an accumulator is re-copied within every chunk, so the cost is quadratic in the chunk size and merely divided by the number of chunks. Under an operand cap the accumulator stops being a fusible literal after a handful of steps, every subsequent step is one node, and the leaves of a loop that appends the same run share one payload. Small concatenations keep the normal form they have today, which is what keeps the corpus still.

Second, `Bin/get`, `Bin/slice` and their `List` twins gain a length-directed distribution rule over `Concat`: the operand lengths locate the window, the operands it spans are sliced, and the rest are dropped. Without it a window over a deep concatenation is reached by peeling one atom at a time, which is linear in the depth for every atom read.

## Design boundaries

**The cap must be proven invisible to conversion, not assumed.** The premise the whole capability rests on is that `peel_bin` and `peel_list` decide a capped spelling against an uncapped one. It holds by reading, per grain and per carrier, and it is what focused tests establish first — before either rewrite lands, and for both checkers, since the same shared function serves both and no disagreement between them is structurally possible.

**Declining to fuse adds no equation.** It removes a normalization step, so the risk is completeness rather than soundness: a proof that closed by literal equality must still close through the peel. That fails toward refusal, and the fixed prelude is the detector — a workspace check already elaborates, erases and certifies every `/std` and `/syn` module, so one build says whether anything lost its derivation.

**The distribution rule is a new fold law and is probed rather than argued.** It lands on `documentation/SOUNDNESS.md`'s *Intrinsic fold laws and the free-monoid peel*, which that file names as the weakest position on the perimeter and grades as argued in code comments only. A wrong fold there is a false definitional equation, and congruence carries one to `False`. Landing a new law on that row without a probe would make the weakest row weaker.

**Depth becomes data-shaped, and the shared walks meet it first.** `peel_front`, `bin_collect_intrinsic` and their list twins recurse into a concatenation's leading operand, so an accumulation as long as the loop becomes a walk as deep as the loop. `documentation/DESIGN.md`'s *Depth is bought with stack, not with hand-rolled frames* protects the two reduction and conversion strategies that are deliberately implemented twice, and it does not reach these: they are single-implementation shared helpers in `curios-core`, so an explicit worklist costs nothing that entry was defending. The precedent is already in the tree — the backend's `$<carrier>/force` walks a hundred-thousand-deep concatenation chain on an explicit doubling worklist for exactly this reason. Under spec 13 a recursion level is priced, so leaving these recursive costs budget as well as memory.

## The fixtures this returns to their natural spelling

`curios/src/tests/runtime.rs`'s `accumulation_loops_are_linear_by_construction` routes its bound through `head_of`, a helper whose only purpose is to put an opaque parameter between `Bytes/slice` and a computed subject. Its comment says what removing it does today, and says it was measured the hard way once. Acceptance is that `head_of` is deleted and the test writes `Bytes/slice(built, 0, 10)` directly. The direct spelling is the point of the measurement: the workaround is precisely the thing a user would not think to write, so a runtime measurement that depends on it is measuring a program nobody would have authored.

Its pinned twin, `tests::numeric`'s `a_bound_on_a_computed_subject_evaluates_it`, states a 50 000-step budget so that the program refuses before it consumes the machine — the budget is containment, not a claim about the program. Under this capability that program succeeds, so the fixture is rewritten to assert success at an ordinary budget, and its parameter control stays, restated as what it always meant: that opacity costs nothing, rather than that opacity is how a computed subject survives.

Sequence matters. Spec 13 reprices that budget first and updates the fixture's figure to what the corrected pricing says; this specification then changes what the fixture asserts. The same fixture therefore moves twice, for two different reasons, and this document owns the second move.

`documentation/DESIGN.md`'s decided-bound entry closes with *What still constrains these obligations is evaluation, not provability*, which names both fixtures and states the opaque-subject discipline as the answer. It is amended in the same landing change.

## Measurement gate

Spec 13's accumulate-then-slice probe is the regression. Its growing-accumulator arm must fall from quadratic to linear in the iteration count, and its fixed-payload arm must not move — a change that improves the first by slowing the second has moved cost rather than removed it.

Beside it, the prelude's own cost, following `curios-prelude-archive`'s `stored_prelude_measurements` pattern: build time and peak memory before and after, recorded with the command, the date and the profile that produced them.

The cap's value is chosen from a census of operand sizes actually reached in `/std` and `/syn`, not picked. A cap below the corpus's ordinary operand sizes changes normal forms that have no reason to change; a cap far above them leaves the accumulation quadratic for longer than it needs to be.

**Evidence that would stop the work.** A cap high enough to leave the prelude's normal forms untouched that still fails to make the accumulation loop linear, which would mean the operand-cap shape is wrong and a real rope is the only remaining option. Or a corpus in which declining to fuse costs proofs that the peel was expected to carry, which would mean fusion is load-bearing for definitional equality in a way reading the peel did not reveal.

## Milestones

### M0 — Evidence and the census

- Extend the spec-13 reproducer, or add its sibling, so the `List` carrier is measured alongside `Bin` at both grains.
- Census the operand sizes reached by every `BinConcat` and `ListConcat` that reduction fuses over a fixed-prelude build, and choose the cap from it.
- Record the prelude baseline the gate compares against.
- Prove the conversion premise: focused tests, per grain and per carrier and in both checkers, that a capped spelling and a fused literal are definitionally equal.

### M1 — Cap the fusion, linearise the shared walks

- Apply the operand cap in `normalize_concat`, with the constant named and documented where it is defined.
- Make `peel_front`, `bin_collect_intrinsic` and their list twins iterative, following the backend force walk's shape.
- Measure: the reproducer's growing arm is linear, the fixed-payload arm is unmoved, and the prelude has not regressed.

### M2 — Length-directed windows

- Add the distribution rule for `Bin/get` and `Bin/slice` over `Concat`, with perimeter probes for the law itself rather than only for the results it produces.
- Show that a window over a deep concatenation is located by operand lengths rather than by peeling atoms.

### M3 — The list carrier

- Extend both the cap and the distribution rules to `ListConcat`, `List/get` and `List/slice`, whose fusion flattens element vectors on the same schedule.

### M4 — Revert the workarounds and retire

- Delete `head_of` from `accumulation_loops_are_linear_by_construction` and write the bound directly.
- Rewrite `a_bound_on_a_computed_subject_evaluates_it` to assert success at an ordinary budget, keeping its parameter control with its meaning restated.
- Amend `documentation/DESIGN.md`'s decided-bound entry, move the durable cost statement into permanent documentation, check off the roadmap item, and delete this working specification in the same landing change.

## Acceptance

- The accumulate-then-slice reproducer's growing arm is linear in the iteration count, and its fixed-payload arm is unchanged.
- A capped concatenation and the literal it would have fused to are definitionally equal, proven per grain, per carrier, and in both checkers.
- The fixed prelude elaborates, erases and certifies with no proof losing its derivation, and its build time and peak memory do not regress.
- A window over a deep concatenation is located from operand lengths, and the distribution law carries its own perimeter probe.
- The shared free-monoid walks handle a hundred-thousand-deep concatenation without native recursion proportional to its depth.
- `accumulation_loops_are_linear_by_construction` writes `Bytes/slice(built, 0, 10)` directly, with no helper standing between the bound and its computed subject, and remains a runtime measurement.
- `a_bound_on_a_computed_subject_evaluates_it` asserts success at an ordinary budget, and its parameter control still elaborates without materializing anything.
- Emitted modules are unchanged: erasure lowers from the written term, so a value that stops fusing at the type level produces the same WebAssembly.
- The cap's value is recorded beside the census that chose it.

## Refused alternatives

**A balanced rope with rebalancing.** Depth under the operand cap is bounded by how many capped operands an accumulation produces, and the length-directed window rule reaches an operand without descending a spine, so rebalancing buys an asymptotic guarantee for a case the cap already handles and adds an invariant that every construction site must maintain.

**Uniqueness-based in-place extension.** Extending a uniquely owned buffer rather than copying it is the mechanism Lean's reset and reuse uses, and it does not survive contact here: the reduction cache and the kernel memos retain every intermediate by construction, so nothing reached by reduction is ever uniquely owned, and the packed carrier holds an exactly sized allocation with no capacity to extend into. It also reaches only `Bin`.

**Teaching `len` to skip evaluating its subject.** A rule that reads a length off a recursive producer without running it is a fusion law over `rec` — deforestation — which must hold for every recursive producer rather than for the one that motivated it, and which is a much larger capability than a representation cost.

**Sharing the backend's rope type.** `curios-core` sits below `curios-cont`, so the dependency direction forbids it, and the type level wants terms its checkers can peel rather than heap objects its emitter can force.

## Precedent

Lean converts a string literal into a cons list lazily — only when definitional equality or a recursor actually demands it — and represents `Nat` literals with a GMP-backed carrier whose basic operations are kernel primitives rather than structural reductions. Both are the same instinct one level down: keep the compact form until something needs the expanded one, and give the operations a rule that reads the compact form directly. See [Lean's natural numbers](https://lean-lang.org/doc/reference/latest/Basic-Types/Natural-Numbers/) and [strings](https://lean-lang.org/doc/reference/latest/Basic-Types/Strings/).

Rocq's [primitive objects](https://rocq-prover.org/doc/V9.0.0/refman/language/core/primitive.html) carry it further for sequences: `PrimString` gives strings primitive `length`, `get` and `sub` that the reduction machines reduce with dedicated rules, and `PArray` makes a persistent array cheap on its current version by keeping one native array and representing older versions as modifications.

No proof assistant in that family uses a rope inside its kernel, so the honest statement is that the precedent for the *problem* is Lean and Rocq, and the precedent for the *answer* is Curios's own backend — `curios-cont`'s `rope_emitter`, which already services `len`, `get`, `slice` and concatenation over an unflattened representation and forces it once on demand — together with the general technique in [Boehm, Atkinson and Plass](https://www.cs.tufts.edu/comp/150FP/archive/hans-boehm/ropes.pdf).

## Verification and retirement

Shared reduction code feeds both checkers and the browser target, so this change requires the exact repository gate from `CLAUDE.md`:

```text
make curios/runtime
cargo fmt --all -- --check
cargo clippy --workspace --all-targets --all-features -- -Dwarnings
cargo test --workspace --all-targets --all-features
make curios/web
```

The handoff also includes the documentation, invariant, and repository-hygiene review required by `CLAUDE.md`.

Measurement probes remain ignored and bounded, and ordinary tests are deterministic and do not observe resident-set size.

Once all acceptance criteria pass, move the durable cost statement into permanent documentation, check off the roadmap item, and delete this working specification in the same landing change.
