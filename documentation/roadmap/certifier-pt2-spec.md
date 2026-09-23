# Certifier, part 2: checked evidence and trusted reasoning

**Not refined yet.** This specification preserves the broader certifier design: checked evidence for reasoning performed outside the kernel, explicit requirements on trusted implementations, and the corresponding soundness account. [Part 1](certifier-pt1-spec.md) owns profiling, independent totality records and kernel call-site discovery. Those changes can retire independently; they do not establish the stronger implementation restrictions proposed here.

The requirements below retain the earlier design direction. Their precise grade definitions, certificate format, dependency transitions and delivery order need refinement with [algebra part 2](algebra-pt2-spec.md). In particular, algebra part 1 preserves today's comparison retries and does not satisfy a blanket prohibition on retrying or first-answer chains.

## What this builds on

- **The crate boundary.** `curios-cert` reaches nothing of `curios-elab`, so it cannot consult a metavariable store, a refinement layer or a parked goal ([An independent kernel re-checks what the elaborator accepts](../design/language/an-independent-kernel-re-checks-what-the-elaborator-accepts.md), and [`curios-cert`'s README](../../curios-cert/README.md)).
- **What the certifier already owns.** Typing, its weak-head strategy, the conversion driver, level entailment, and the erased positions it records during its own typing walk (`kernel/positions.rs`), from which obligations (T) and (V) are seeded.
- **What it shares.** The representation and the literal folds in `curios-core`; the symbolic algebra and the closed machine beside them; and `curios-analysis`'s inversion, positivity and size-change totality.
- **The evidence discipline** of [the soundness perimeter](../design/language/the-soundness-perimeter.md), the two-checker fixtures in `curios/src/tests/perimeter.rs`, and the differentials that already hold an optimization to its slow path: `curios-prelude-archive`'s `kernel_memo_parity` and `curios-cert`'s `the_closed_machine_agrees_with_the_strategy`.
- **Coq Modulo Theory and its successor CCIC**: a decidable first-order theory in conversion, its metatheory established for strong elimination (Jouannaud and Strub, 2017), and the move from a trusted decision procedure to decision procedures outside the kernel returning certificates the kernel checks (Blanqui, Jouannaud and Strub, 2008).

## The gap

**The certifier depends on code written for reasons other than being checked.** Its closure includes the symbolic algebra — normal forms kept lazy for cost, peels chained first-answer-wins, a truth table capped for cost — a 945-line closed-term evaluator that exists to make evaluation cheaper, and shared analyses whose trusted contracts need review. Sharing avoids a second copy but does not establish which implementations the certifier should depend on. Part 1 separately replaces shared call-site discovery for the kernel.

**Several statements about the boundary are not what the code does.**

- `curios-cert`'s module documentation calls the roster "and its folds" representation. Literal folds are arithmetic, but the symbolic laws are a theory in Coq Modulo Theory's sense, and deciding a theory is part of the conversion rule.
- [Evaluating a closed term is representation, not judgment](../design/toolchain/evaluating-a-closed-term-is-representation-not-judgment.md) argues from confluence that a closed term's result is unique. That makes the value well defined, not an evaluator correct: it is a trusted evaluator, as Rocq's virtual machine is.
- The perimeter's trusted base names the elaborator's checker beside the certifier, and it holds rules only the elaborator runs. [Witness coherence and the orphan rule](../soundness/per-term-rules/witness-coherence-and-the-orphan-rule.md) is one: in Core a witness is an ordinary `Global::Witness` definition the certifier types like any other, so incoherence cannot make it accept a false proposition — the argument the perimeter already makes for privacy.

**The algebra is pervasive and cheap.** Elaborating `/std` runs `nat::cancel_common` 16,230 times in 0.19 s, `nat::linear` 51,062 times and `nat::summands` 59,657 times, and the closed machine 78,276 times in 1.54 s, out of about 42 s — the prelude build's `profile.tsv`. These are elaboration measurements from the earlier survey, not a bound on the cost of a replacement inside the kernel. Part 1's certifier profile must inform any evaluator or algorithm replacement.

## Design direction and contracts to refine

**The certifier believes no input.** A verdict is derived from the term, from the certifier's own record, or from evidence the certifier checks. Elaborator metadata — totality stamps, polarity vectors, witness and concept lists, universe seeds, the binder floor — is never an input to a verdict. The binder floor is already derived rather than read, and the polarity vectors already re-derived.

The certifier-owned record and its reuse are specified by part 1; this work adds checked evidence without reintroducing elaborator metadata as authority.

**Trust flows from simple code to complex code, never the reverse.** Code is *certifier-grade* when:

- it decides by a total function its module documentation states;
- it performs no search: no first-answer chains, no retry on failure, no cache whose hit changes an answer, no verdict that depends on a budget except a refusal;
- it fails closed on a shape it does not anticipate;
- a grid or a differential holds it to a statement independent of it.

The certifier depends on certifier-grade code alone. Such code may be shared through `curios-core`, `curios-algebra` and `curios-analysis`, and the elaborator may use it. Search, heuristics and optimization are *elaborator-grade* and stay outside the certifier's closure. A copy of an implementation is not independence and is not a goal; what the certifier depends on being simple is.

**Search outside, checking inside.** Where a verdict is expensive to find and cheap to check, the elaborator finds it and the certifier checks it. Evidence travels with the module, keyed by what it is about — a certificate by its canonical formula — so the certifier's relation is a function of the question and never of where it was asked. Missing evidence is a refusal, never an acceptance.

**The certifier decides the carriers' theory itself**, with the reference implementation whose broader theory and evidence contracts require refinement in [algebra part 2](algebra-pt2-spec.md#arithmetic-reasoning-and-evidence). Inversion reads the theory's freeness — constructors distinct and injective modulo the theory — through that implementation, never through the elaborator's search.

**An optimization inside the certifier keeps its slow path.** A memo, an evaluator, a recurrence key: each is held by a differential against the unoptimized rule it replaces, as `kernel_memo_parity` and `the_closed_machine_agrees_with_the_strategy` already hold two.

## The components

These are the recorded target treatments, subject to the grade and evidence refinement below; they are not completed classifications of today's code.

| Component | Treatment |
| --- | --- |
| The representation, the signature and wire tables, literal arithmetic over `curios-num`, `pinned_by_targets`, `Erased` | Certifier-grade, shared as they are |
| The carriers' theory | Decided in both checkers by `curios-algebra`'s reference implementation; the search moves to the elaborator |
| Linear integer arithmetic | Farkas certificates as evidence, checked by a certifier-grade checker in `curios-algebra` |
| Inversion | Shared and certifier-grade; its intrinsic cases read freeness from the reference implementation |
| Size-change totality | Part 1 supplies kernel-derived call sites; review the shared graph closure against the refined trusted-code requirements |
| Positivity | Shared, reviewed to certifier grade |
| The closed machine | A trusted evaluator, named as one and held to the strategy by its differential. Whether the certifier gets an evaluator of its own is decided once it is profiled |

## Work to refine before implementation

- **Evidence.** Specify the module evidence table, the formula identity it uses, certificate transport and validation, and missing-evidence refusal. Coordinate the certificate language and checker with algebra part 2; Farkas certificates are the recorded proposal, not a settled complete integer decision procedure.
- **Trusted-code requirements.** Reconcile the stated ban on search, retries and first-answer chains with existing shared algorithms, the permitted budget refusal, and the memo and evaluator differentials. Classify actual procedures before claiming that whole crates satisfy the grade.
- **Dependency enforcement.** Identify what moves out of the normal certifier closure, including arithmetic search and the elaborator's universe solver housed in Core's `universe` module, and where it belongs. The intended `cargo tree -p curios-cert --edges normal` invariant must correspond to the refined grade definition.
- **Evaluator and analyses.** Use part 1's measurements to decide whether the kernel needs a separate evaluator. Review positivity, inversion and graph closure against their stated contracts; copying code alone establishes no independence.
- **Delivery and evidence.** Turn these topics into stages with dependencies, supported fragments, refusal cases and completion evidence once their interfaces are known. Preserve the existing optimization differentials throughout.

Each implementation updates the affected soundness and design claims when it lands; there is no final documentation stage that postpones corrections already justified by earlier work.

## Design decisions this overturns or corrects

Each is revised in the change that makes it true.

- [An independent kernel re-checks what the elaborator accepts](../design/language/an-independent-kernel-re-checks-what-the-elaborator-accepts.md): its sharing paragraphs are restated against the refined trusted-code requirements; part 1 owns the totality correction.
- [Evaluating a closed term is representation, not judgment](../design/toolchain/evaluating-a-closed-term-is-representation-not-judgment.md): relabelled as a trusted evaluator, held by its differential.
- [The soundness perimeter](../design/language/the-soundness-perimeter.md): the trusted base becomes the certifier's closure, and the elaborator's checker leaves it, with the rules only it runs.
- [`curios-analysis`'s README](../../curios-analysis/README.md) decision *These rules are shared rather than duplicated* is restated: they are shared because they are certifier-grade.
- `curios-cert`'s module documentation: the representation claim; part 1 owns the elaborator dev-dependency correction.
- Perimeter entries: [The closed machine](../soundness/per-term-rules/the-closed-machine.md) is re-graded as a trusted evaluator; [Witness coherence and the orphan rule](../soundness/per-term-rules/witness-coherence-and-the-orphan-rule.md) leaves the perimeter for a decision on concepts; an entry is added for the certificate checker. Part 1 owns the verdict-record entry and retirement of the carried-totality account.

## Rejected

- **Duplicating the shared code.** A copy fails where its original does — the correlation independent implementations are known for (Knight and Leveson, 1986). What the certifier needs is simple code, not a second one.
- **Compiling matches to eliminators and recursion to recursors**, as Lean's elaborator does, which would take inversion and termination out of the certifier. `Match` is an intrinsic of Core and partial recursion is allowed at run time: both are the language's, not its elaboration's.
- **Taking the algebra out of conversion by casts.** The recorded rejection is retained; its missing rationale is an explicit reconciliation item in [algebra part 2](algebra-pt2-spec.md#foundation-and-boundaries).
- **A fresh explicit kernel IR**, for the reasons [An independent kernel re-checks what the elaborator accepts](../design/language/an-independent-kernel-re-checks-what-the-elaborator-accepts.md) records.
- **Forbidding all sharing**, which buys independence only from code whose simplicity was the point.

## Non-goals

Certifying anything below Core; a verified certifier; a certifier runnable apart from the compiler over stored units; exporting proofs to another checker.

## Verification

- The two-checker fixtures and `kernel_disagreements` stay clean at every stage, and the goal-level differential for conversion, which [An independent kernel re-checks what the elaborator accepts](../design/language/an-independent-kernel-re-checks-what-the-elaborator-accepts.md) lists as missing, is written.
- The certificate checker is held against certificates corrupted one step at a time, each corruption refused.
- The certifier's time is measured before and after each stage, and reported by the stage.

## Completion targets and retirement

- No verdict of the certifier reads an input it did not derive or check.
- `cargo tree -p curios-cert --edges normal` reaches only certifier-grade crates, and `CLAUDE.md` states it.
- The carriers' theory is decided in the certifier by the reference implementation.
- The perimeter's trusted base is the certifier's closure.
- Refinement must give the targets above concrete contracts and acceptance evidence. Before this specification is deleted, the certifier's contracts are recorded in `curios-cert`'s README and rustdoc, the decision and its rejected alternatives are a design decision replacing the ones corrected above, the roadmap entry is a checked summary, and no reference to this filename remains.

When part 1 retires, link to its permanent record and call-site contracts. Retire this document only after the retained evidence and trusted-reasoning work is complete or explicitly rescoped, with all remaining filename references updated.
