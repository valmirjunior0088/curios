# Checked evidence and trusted reasoning

Working specification for what the certifier may depend on. [An independent kernel re-checks what the elaborator accepts](../../design/soundness/an-independent-kernel-re-checks-what-the-elaborator-accepts.md) states the grade the certifier's closure is held to, and that the gate holds the closure; here every procedure in the closure is classified against the grade, a step of the gate holds the closure, and reasoning found outside the kernel gets evidence the certifier checks. The first four stages need nothing that does not exist; the fifth, the evidence itself, opens with [the relational layer](../04-arithmetic/08-relational-layer.md), whose certificates it would carry.

The certifier's profile is [recorded](../../../curios-cert/README.md#measuring-the-certifier), and so are [its own verdict record](../../../curios-cert/README.md#a-later-walk-reads-the-certifiers-own-totality-record-never-elaborations-stamp) and [the calls it takes from its own typing](../../../curios-cert/README.md#a-groups-calls-are-the-ones-the-kernel-types). [The procedure that proves a bound from the facts in scope](../../design/arithmetic/a-bound-that-follows-from-the-facts-in-scope-is-proved-by-the-elaborator.md) needs no evidence at all: it writes ordinary proofs, which the certifier checks as it checks any term.

## What this builds on

- **The crate boundary.** `curios-cert` reaches nothing of `curios-elab`, so it cannot consult a metavariable store, a refinement layer or a parked goal ([An independent kernel re-checks what the elaborator accepts](../../design/soundness/an-independent-kernel-re-checks-what-the-elaborator-accepts.md)). Its closure is [enumerated in prose](../../../curios-cert/README.md#the-trusted-base-is-a-crate-boundary): the shared layer, its foundation crates and three third-party roots.
- **What the certifier already owns.** Typing, its weak-head strategy, the conversion driver, level entailment, the erased positions and the calls of a group it records during its own typing walk, and its record of each definition's totality and reads.
- **What it shares.** The representation, the literal folds and the closed machine in `curios-core`; the carriers' algebra in `curios-algebra`, read through `curios-core`'s `spine`, `nat`, `int`, `linear` and `atoms`; and `curios-analysis`'s inversion, positivity, size-change grading and closure, and the chain conversion runs when two intrinsics meet.
- **No solver in the closure.** The universe solver is `curios-elab`'s `universe_solver`, the search that proves a bound is `curios-elab`'s `entailment`, and `curios-algebra` names no term (`.claude/rules/checking.md`).
- **The evidence discipline** of [the soundness board](../../design/soundness/the-soundness-board.md), the programs `curios/src/tests/board/` puts to both checkers, and the differentials that hold an optimization to its slow path: `kernel_memo_parity`, `the_closed_machine_agrees_with_the_strategy`, and `entail`'s `the_structural_fast_path_never_outruns_the_oracle`.

## The gap

**The certifier depends on code written for reasons other than being checked.** Its closure includes the symbolic algebra — normal forms kept lazy for cost, peels chained so the first that answers wins (`curios-core`'s `spine`), a truth table capped for cost — a closed-term evaluator that exists to make evaluation cheaper, and shared analyses whose trusted contracts are stated per rule and nowhere as one grade. Sharing avoids a second copy; it does not say which implementations the certifier should depend on.

**The closure is held by a sentence.** `cargo tree -p curios-cert --edges normal` is named in `curios-cert`'s README, its `lib.rs` and `.claude/rules/checking.md`, and run by nothing: a dependency added to any crate in the closure enters the trusted base with no step of the gate noticing.

**One statement says less than the code does.** `curios-cert`'s `lib.rs` calls "the intrinsic roster and its folds" part of what a term *is*. Literal folds are arithmetic; the symbolic laws are a theory in Coq Modulo Theory's sense, and deciding a theory is part of the conversion rule. [The closed machine](../../design/soundness/conversion/the-closed-machine.md)'s entry argues the machine beside the folds in the same breath, where the kernel's design decision names it a trusted evaluator.

## Prior art

- **The Calculus of Congruent Inductive Constructions and Coq Modulo Theory.** The first sends the context's hypotheses to a decision procedure inside conversion, its metatheory established for the weak recursor, and moves from a trusted procedure to procedures outside the kernel returning certificates the kernel checks (Blanqui, Jouannaud and Strub, 2007 and 2008). The second keeps a decidable first-order theory in conversion without the context's hypotheses (Strub, 2010), with metatheory for strong elimination (Jouannaud and Strub, 2017).
- **Where trusted kernels have been wrong.** Rocq's list of critical bugs keeps one of its longest sections for conversion machines: the virtual machine, native compilation, and the primitive integers, floats and arrays, with entries such as "the invariant justifying some optimization was wrong for some combination of sharing side effects" and "conversion of Prod / Prod values was comparing the wrong components" ([`dev/doc/critical-bugs.md`](https://github.com/rocq-prover/rocq/blob/master/dev/doc/critical-bugs.md)). An optimization is where a kernel's defects gather, which is the case for keeping each one's slow path beside it.
- **Search outside, checking inside, in practice.** Rocq's `lra` and `lia` ask an untrusted oracle for a refutation and check it. Over the rationals a positive linear combination suffices; over the integers it does not — "2 × x = 1 → False which is a theorem of ℤ but not a theorem of ℝ" — so `lia`'s certificates add cutting planes, `p ≥ c → p ≥ ⌈c⌉`, and case splits over a bounded range ([*Micromega*](https://rocq-prover.org/doc/master/refman/addendum/micromega.html)).
- **A kernel held by a second one.** Lean4Lean is a checker for Lean written in Lean, "between 20% and 50% slower" than the C++ kernel, able to check mathlib, and "one soundness bug has been spotted and fixed as a result of this work" (Carneiro, [arXiv:2403.14064](https://arxiv.org/abs/2403.14064)).
- **Independent copies fail together** (Knight and Leveson, 1986).

## Decisions

**The certifier believes no input.** A verdict is derived from the term, from the certifier's own record, or from evidence the certifier checks. Elaborator metadata — totality stamps, polarity vectors, witness and concept lists, universe seeds, the binder floor — is never an input to a verdict.

**Trust flows from simple code to complex code, never the reverse.** [The kernel's design decision](../../design/soundness/an-independent-kernel-re-checks-what-the-elaborator-accepts.md) states the grade in six clauses, cited below by its numbers: the certifier depends on certifier-grade code alone, and search stays outside its closure. A copy of an implementation is not independence and is not a goal; what the certifier depends on being simple is.

**Clause 2 replaces a ban on first-answer chains.** [The carriers' algebra](../../design/arithmetic/the-carriers-algebra-stays-in-conversion.md) chains its peels and retries a comparison through its normal forms, and a ban would exclude it. What a chain can get wrong is acceptance by a rule that is unsound alone, which the grid attacks rule by rule, and a verdict that depends on order, which clause 2 forbids and a permuted-chain differential holds.

**Search outside, checking inside.** Where a verdict is expensive to find and cheap to check, the elaborator finds it and the certifier checks it. Evidence travels with the module, keyed by what it is about — a certificate by its canonical formula — so the certifier's relation is a function of the question and never of where it was asked. Missing evidence is a refusal, never an acceptance.

**The certifier decides the carriers' theory itself**, by `curios-algebra`'s reference implementation; the theory grows by [the declared operations](../04-arithmetic/02-declared-operations.md). Inversion reads the theory's freeness — constructors distinct and injective modulo the theory — through that implementation.

**An optimization inside the certifier keeps its slow path**, held by a differential against the rule it replaces.

**The closure is held by the gate.** One step fails when `curios-cert`'s normal closure differs from the crates its README names, and the README's list is what the step reads.

## The components

The target treatment of each; stage 2 turns this into a row per procedure.

| Component | Treatment |
| --- | --- |
| The representation, the signature and wire tables, literal arithmetic over `curios-num`, `pinned_by_targets`, `Erased` | Certifier-grade, shared as they are |
| The carriers' theory | Decided in both checkers by `curios-algebra`'s reference implementation; its chains held by clause 2's differential |
| The truth table | Certifier-grade under clause 4: past its cap it refuses |
| Linear integer arithmetic | In conversion, if [the relational layer](../04-arithmetic/08-relational-layer.md) is opened: certificates as evidence, checked by a certifier-grade checker in `curios-algebra`. In elaboration, [the proofs the elaborator writes](../../design/arithmetic/a-bound-that-follows-from-the-facts-in-scope-is-proved-by-the-elaborator.md), checked as ordinary terms |
| Inversion | Shared and certifier-grade; its intrinsic cases read freeness from the reference implementation |
| Size-change totality | The kernel derives its own call sites; the shared grading and closure are classified in stage 2 |
| Positivity | Shared, classified in stage 2; the kernel recomputes every vector |
| The closed machine | A trusted evaluator, named as one and held to the strategy by its differential |
| The kernel's memos and the entailment fast path | Optimizations, each with its slow path and its differential |

## Stages

1. **The statement.** `curios-cert`'s `lib.rs` separates literal folds from the symbolic theory, and [The closed machine](../../design/soundness/conversion/the-closed-machine.md)'s entry names the machine a trusted evaluator, as the design decision does. Documentation alone; its check is that every path and test it names resolves.
2. **The classification.** Every procedure a verdict of the certifier reaches in the shared layer gets a row against the grade's six clauses: the procedure, its crate, the clauses it meets, what holds it, and for a clause it fails, what is done — restated, moved to the elaborator, or given the differential it lacks. The rows live in `curios-cert`'s README. A procedure that fails a clause with no remedy is a finding or, where it admits a term at `False`, a ticket.
3. **The chains' differential.** Each chain of sufficient rules the certifier reaches is run in its written order and in every rotation over the law grid's instances and asserted to answer alike. Mutation-checked: a rule made unsound for one instance fails the grid, and a rule made to shadow another fails the differential.
4. **The closure in the gate.** A step of `cargo xtask` computes `curios-cert`'s normal dependency closure and compares it with the list in its README, in both directions. `.claude/rules/checking.md` names the step where it names the command.
5. **Evidence.** With the relational layer's trigger: the module's evidence table, the formula identity it is keyed by, transport and validation, and refusal where evidence is missing. That specification owns the certificate language and its checker; this one owns the transport and the grade the checker must meet. Rocq's `lia` shows what the language must say about the integers: a rational refutation alone does not decide them.

Each stage updates the soundness and design claims it makes true; none is postponed to a last documentation stage.

## Open

- **Whether the certifier gets an evaluator of its own.** Decided from [the recorded profile](../../../curios-cert/README.md#measuring-the-certifier): the `reduce` and `reduce_forced` rows of the prelude's certification with the machine on, against the same stream with the machine gated off. If the strategy alone certifies `/std` within the budget every item has, the machine leaves the kernel's path and stays the elaborator's; if not, it stays a trusted evaluator.
- **Where the classification's rows are held to the code.** A row names a procedure by path; nothing yet fails when a new procedure enters a verdict's reach without a row. Stage 2 states whether a lint can hold it or the rows are reviewed with each kernel change.
- **The evaluator's unprobed route.** [The closed machine](../../design/soundness/conversion/the-closed-machine.md) records that whether a run-dependent rigid head could reach the elaborator's witness keying is unprobed.

## Design decisions this overturns or corrects

Each is revised in the stage that makes it true.

- [An independent kernel re-checks what the elaborator accepts](../../design/soundness/an-independent-kernel-re-checks-what-the-elaborator-accepts.md): its sharing paragraphs restated against the classification in stage 2.
- [`curios-analysis`'s README](../../../curios-analysis/README.md) decision *These rules are shared rather than duplicated*: they are shared because they are certifier-grade, in stage 2.
- `curios-cert`'s module documentation: the representation claim, in stage 1.
- Board entries: [The closed machine](../../design/soundness/conversion/the-closed-machine.md) in stage 1, and an entry for the certificate checker in stage 5.

## Rejected

- **Duplicating the shared code.** A copy fails where its original does. What the certifier needs is simple code, not a second one.
- **A ban on chains and retries**, which states a means: it excludes the algebra as it stands and admits a single unsound rule.
- **Compiling matches to eliminators and recursion to recursors**, as Lean's elaborator does, which would take inversion and termination out of the certifier. `Match` is an intrinsic of Core and partial recursion is allowed at run time: both are the language's, not its elaboration's.
- **Taking the algebra out of conversion by casts**, for the reasons [The carriers' algebra stays in conversion](../../design/arithmetic/the-carriers-algebra-stays-in-conversion.md) records.
- **A fresh explicit kernel IR**, for the reasons the kernel's design decision records.
- **Forbidding all sharing**, which buys independence only from code whose simplicity was the point.
- **A trusted decision procedure in the kernel.** The certificate is checked; the search that found it is not trusted.

## Non-goals

Certifying anything below Core; a verified certifier; a certifier runnable apart from the compiler over stored units; exporting proofs to another checker.

## Verification

- `curios/src/tests/board/` and `kernel_disagreements` stay clean at every stage, and [the audit of conversion's laws](../../design/soundness/conversion-is-one-relation-in-both-checkers.md) holds the two checkers' conversion to each other.
- Stage 3's differential and stage 4's step, each failing on the mutation its stage names.
- The certificate checker is held against certificates corrupted one step at a time, each corruption refused.
- The certifier's time is measured before and after stages 3 and 5, and reported by the stage.

## Completion and retirement

- No verdict of the certifier reads an input it did not derive or check.
- `curios-cert`'s closure reaches only certifier-grade code, every procedure in it has its row, and the gate holds the closure.
- The carriers' theory is decided in the certifier by the reference implementation.

Stages 1 to 4 complete without the relational layer. If that layer is ruled out, stage 5 is deleted with it and this specification retires on the first four. Before it is deleted, the certifier's contracts are recorded in `curios-cert`'s README and rustdoc, the decision and its rejected alternatives are a design decision replacing the ones corrected above, the roadmap entry is a checked summary, and no reference to this filename remains.
