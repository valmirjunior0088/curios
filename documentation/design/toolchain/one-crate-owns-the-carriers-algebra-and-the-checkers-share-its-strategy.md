# One crate owns the carriers' algebra, and the checkers share its strategy

**Decision.** The carriers' algebra is divided among crates by what each part reads.

| Owner | Responsibility |
| --- | --- |
| `curios-num` | The concrete numeric and packed carriers and their scalar semantics. |
| `curios-algebra` | The mathematics over abstract atoms. It covers the arithmetic: combinations and their cancellation, distribution, Euclid's recombination, comparison facts, bounds and domination. It covers the Boolean laws and the truth table's evaluation. It covers the words' normal form, measures and prefix strip, the round trips between carriers, and the canonical linear form of a comparison. It also holds the vocabulary of operations and the law table. It owns what its results mean and how strong they are. |
| `curios-core` | Which operation each intrinsic is, through `Intrinsic::algebra`, an exhaustive match, and which terms are one atom, in the `atoms` module. It reads terms into the algebra's views and rebuilds terms from the algebra's results. It also owns reduction demand and charging, proof and element-type provenance, and the folds that apply the results. |
| `curios-analysis` | The chain both checkers run when two intrinsics meet, `convert_intrinsics`. It returns an outcome or obligations typed by `Intrinsic::signature`, and calls no judgment. It also owns inversion, which reads the algebra only through Core's `peel_intrinsic`, whose `Deduction` cannot carry a residual that is only sufficient. |
| `curios-elab` | Solved-metavariable substitution before the chain, the packed-literal view and the proposals it makes, the worklist, validation, rollback and commitment. |
| `curios-cert` | Discharging the chain's obligations in order under its active conversion history and its budget. |

**The dependencies point one way.**

- `curios-algebra` depends directly on `curios-num` alone. It names no `Term`, no `Intrinsic`, no elaborator context and no kernel type.
- Core depends on Algebra, Analysis on Core, and both checkers on Analysis.
- A procedure that searches — [algebra part 2](../../roadmap/algebra/02-bounds-from-facts-spec.md)'s — lives on the elaborator's side of the certifier's dependency closure. It reads the views Core publishes and hands the checkers ordinary terms.
- Algebra holds no search, so the certifier depends on none.

The crate's own decisions — what it names, how results carry their strength, whose atom identity it uses, and which operations it declares — are [its README](../../../curios-algebra/README.md)'s. Why the laws are decided in conversion at all is [The carriers' algebra stays in conversion](../language/the-carriers-algebra-stays-in-conversion.md).

**Rationale.** The algebra used to sit inside `curios-core`, interleaved with term reading, reduction demand and reconstruction, and each converter repeated its comparison strategy. That cost four things:

- A rule's meaning was readable only from the folds that applied it.
- Each carrier's copy of a rule could drift. The `<`/`<=` seam met at `Int` and not at `Nat` because two procedures decided one relation.
- Every change to the strategy was two edits. A normalization added to the elaborator's chain alone made the elaborator close a law the kernel refused, and only the law grid noticed.
- Inversion could not deduce a residual that was only sufficient, but nothing in the result said so. The rule was enforced by leaving the product-factor peel out of inversion's entry.

With the parts separated:

- The mathematics is tested over bare atoms and concrete values, with no checker involved.
- One chain serves both checkers, so a demand added to it is asked by both.
- A result's strength is in its type.
- A law declared once is generated at every carrier that declares it, so a law true at two carriers and decided at one fails at the other.

**What is kept where it was.** The demand boundaries are Core's, and the extraction preserved each one:

- A symbolic product is distributed only at the existing normalization gates.
- Reading a Boolean formula stops at the eight-atom cap without forcing the rest of the input.
- A stuck connective keeps its right operand as written.
- Numeric atom arguments are forced once, in the existing retry, and fall back to the original spelling.
- A window measures its operands only as far as the seam walk reaches.

The algebra reports the work it would do before doing it — `distribution_size` and `evaluation_size` — and Core charges it. So exhaustion takes the existing refusing path and never manufactures a verdict.

Refinement keys keep their written spellings and the dual and successor probes. The canonical view reads a comparison where one is asked for and rebuilds no key.

**What the carriers still decide differently.** Having one owner does not make every law hold at every carrier, but it makes each difference visible. The extraction preserved behavior, so none of these was in its scope:

- **Bitwise operations.** `Int`'s `and`, `or` and `xor` are opaque. The identities and the commutativity `Nat`'s carry are not decided at `Int`, and the law table declares none there.
- **Literal divisors.** The literal-divisor laws and the Euclidean split fold only `Nat`'s division.
- **Bounds and domination.** Both are `Nat`'s. An `Int` comparison reaches them only through the preimages of widened naturals.

Each is a law true at both carriers and decided at one. Declaring the law at `Int` would generate its rows there.

**What it cost.** The prelude's elaboration and certification were measured before the extraction and after each of its stages. Call counts and allocated bytes are the stable figures of those instrumented debug builds, and durations stayed within their noise.

| Measure | Before | After |
| --- | --- | --- |
| Elaboration, allocated | 36 724 MB | 36 684 MB |
| Certification, allocated | 8 034 MB | 8 009 MB |
| `nat::cancel_common` in elaboration, allocated | 1 271 MB | 135 MB |
| `nat::linear` calls in elaboration | 150 697 | 2 615 |

- **Cancellation allocates a tenth of what it did.** It keys a summand once, as a handle, instead of scanning projected terms.
- **`nat::linear` has almost no callers left.** Recombination and cancellation collect through the algebra directly, instead of re-reading a sum through it.
- **Other call counts moved only where a stage changed behavior on purpose.** Two numbers of one operation now meet up to universe instances, which added ten cancellations in elaboration and six in certification.
- **A sum is still flattened afresh on every `Nat::summands` call.** Keeping each sum's flattened form was in scope and was not built. The flattening measured 206 137 calls in elaboration — about 1.0 s of 105 s and 43 MB of 36 684 MB — and 118 066 calls in certification, 0.45 s and 25 MB. It went back to [invariants part 3](../../roadmap/invariants/03-shared-term-costs-spec.md), which raised it.

**Rejected — consolidating inside `curios-core` first, and extracting later.** It would leave the mathematics tied to `Term`, which defers the risk the extraction exists to take. It could not be tested over independent atoms either.

**Rejected — strategies apart in the two checkers, for a second opinion.** Both copies ran rules of `curios-core` and `curios-algebra` already. A second copy of a pure function is a second run, not a second opinion, which [`curios-analysis`'s README](../../../curios-analysis/README.md) states for every rule that crate shares. The copies bought drift, and the law grid caught one.

**Rejected — every result an equality, a clash, or a bare residual.** Inversion must not deduce what is only sufficient. A result vocabulary that could not tell the two apart would leave the restriction to whoever registers a rule.

**Rejected — eager canonicalization.** Rewriting sums and comparisons into a canonical form as they are built changes demand, spelling and cost at once. A guard's refinement is keyed on its written spelling ([A comparison is spelled one way when it is stuck](a-comparison-is-spelled-one-way-when-it-is-stuck.md)). The canonical linear view is not canonicalization: it is read where a comparison is asked for, and it rebuilds no term and keys nothing.

**Rejected — a compatibility engine kept beside the new one.** Two production implementations would keep the ownership problem this decision ends. Each old body ran beside its replacement only as a debug-build oracle while its family moved, and was deleted when the family's migration closed.

**Rejected — solvers and representations with no consumer.** Canonical refinement keys, polynomial unification proposals, Boolean normal forms and one internal sequence carrier are each deferred on the roadmap until a consumer asks. The interfaces here are the ones current consumers exercise.

**Rejected — a carrier's rows stated by hand for a declared law.** The rule to state a row at every carrier was a convention, and the `<`/`<=` seam slipped past it. Generating the rows from the declarations makes the row at every carrier a consequence.

**Rejected — an alignment per carrier.** Two procedures deciding one relation drift. One alignment over the linear view decides the relation once, at every carrier.
