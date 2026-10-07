# The trusted base is held by the tree

Working specification for holding what the certifier depends on to the tree. [An independent kernel re-checks what the elaborator accepts](../../design/soundness/an-independent-kernel-re-checks-what-the-elaborator-accepts.md) states the grade the certifier's closure is held to, in six clauses cited below by their numbers. Today what the kernel reaches of the shared layer is spread over its modules, and the one set of alternatives conversion asks is listed twice with nothing holding its order. Here the kernel names what it reaches in one module, a set of alternatives is one list whose answers agree, and no table of procedures is kept.

Evidence for reasoning found outside the kernel is [the relational layer](../04-arithmetic/08-relational-layer.md)'s, which reserves its transport beside the certificates it would carry. [The procedure that proves a bound from the facts in scope](../../design/arithmetic/a-bound-that-follows-from-the-facts-in-scope-is-proved-by-the-elaborator.md) needs none: it writes ordinary proofs, which the certifier checks as it checks any term.

## What this builds on

- **The grade.** The certifier depends on code that decides by a documented total function (1), accepts only by rules sound alone, so order decides cost and never a verdict (2), leaves nothing behind where a rule does not apply (3), lets a budget, a cap or a cache change a verdict only into a refusal, a hit being what recomputation answers (4), fails closed on a shape it does not anticipate (5), and is held by a grid or a differential to a statement independent of it (6).
- **What holds a clause today.** [The audit of conversion's laws](../../design/soundness/conversion-is-one-relation-in-both-checkers.md) puts every rule of conversion to each checker by itself, near misses among the rows, which is clause 2's first half and clause 6 for the carriers' theory. `curios-prelude-archive`'s `kernel_memo_parity` holds the kernel's memos to a walk without them, `curios-cert`'s `the_closed_machine_agrees_with_the_strategy` the closed machine to each checker's own strategy, and `entail`'s `the_structural_fast_path_never_outruns_the_oracle` the entailment fast path to its oracle. `decide_bool` (`curios-core/src/reduce/intrinsic/truth.rs`) answers a definitional equality or *undecided*, never a disequality.
- **The crate boundary.** `curios-cert` reaches nothing of `curios-elab`, and its closure is [enumerated in prose](../../../curios-cert/README.md#the-trusted-base-is-a-crate-boundary) by root and family: the shared layer, its foundation crates and three third-party roots.
- **The carriers' theory has one owner.** `curios-algebra` states it over abstract atoms and names no term, `curios-core` maps terms onto it, and `curios-analysis`'s `convert_intrinsics` runs it in one order for both checkers; what inversion may read of a comparison is a `Deduction` and what only conversion may is a `Conclusion`.
- **The chain is a pipeline around one set of alternatives.** `convert_intrinsics` prepares each side, distributes a stuck product, asks the truth table, flattens two connective trees and respells two comparisons, each step rewriting the pair the next one reads. Then it asks the peels, the first that answers winning: `peel_nat_pair`, `peel_int_pair`, `peel_bin`, `peel_list`, `peel_bool`, `peel_symmetric` and `peel_position`, between `peel_monomial` and `peel_comparison` before them and `peel_commutative` after.
- **A rule has no row.** [The soundness board](../../design/soundness/the-soundness-board.md) rejects an entry per rule grading its evidence, "an inventory of tests nothing held to the tree": a flaw is a ticket the board runs, an attack that found nothing a test, and a rule its decision's.

## The gap

- **The peels are listed twice, and nothing holds their order.** `curios-core/src/spine.rs` and `read` in `curios-analysis/src/conversion.rs` each spell the seven in the same order. Two that both answer, one concluding an equality where the other concludes a clash, or a later one concluding more than the first, would make a verdict turn on the order, which clause 2 forbids and no test asks.
- **What the kernel reaches has no one place.** Twenty-two of `curios-cert`'s modules import from the four shared crates at `cd4920105`, each for itself, functions and types alike, so a shared procedure entering a verdict's reach is a line in whichever module wanted it.
- **Two statements say other than the code.** `curios-cert`'s `lib.rs` files "the intrinsic roster and its folds" and the closed machine under what a term *is*, where the kernel's design decision names the machine a trusted evaluator and the carriers' theory `curios-algebra`'s. And a record of elaboration's can turn a verdict, into a refusal only: a `Total` stamp the walk's own classification denies (`curios-cert/src/obligation.rs`), and a concept's name under which a module declares something else (`Globals::in_scope`).

## Prior art

- **Where trusted kernels have been wrong.** Rocq's list of critical bugs keeps one of its longest sections for conversion machines, with entries such as "the invariant justifying some optimization was wrong for some combination of sharing side effects" and "conversion of Prod / Prod values was comparing the wrong components", and records its checker accepting a proof of `False` by trusting a serialized bytecode segment it did not derive ([`dev/doc/critical-bugs.md`](https://github.com/rocq-prover/rocq/blob/master/dev/doc/critical-bugs.md)). Lean 4.34.0 closes three routes to `False`: a cache of successful comparisons kept as a union-find, whose closure made an answer depend on the queries before it; `is_prop` answering no for a type that did not reduce to a sort, where it now requires one; and a reference count that wrapped. It also refuses a `Nat` literal past a size limit ([release notes](https://lean-lang.org/doc/reference/4.34.0/releases/v4.34.0/)).
- **Search outside, checking inside.** Rocq's `lra` and `lia` ask an untrusted oracle for a refutation and check it ([*Micromega*](https://rocq-prover.org/doc/master/refman/addendum/micromega.html)), and Lean's `bv_decide` hands a goal to a SAT solver and checks the LRAT certificate it returns with a checker proved sound ([`Std.Tactic.BVDecide.LRAT.Checker`](https://lean-lang.org/doc/api/Std/Tactic/BVDecide/LRAT/Checker.html)).
- **A kernel held by a second one.** Lean4Lean is a checker for Lean written in Lean, "between 20% and 50% slower" than the C++ kernel, and "one soundness bug has been spotted and fixed as a result of this work" (Carneiro, [arXiv:2403.14064](https://arxiv.org/abs/2403.14064)).
- **Independent copies fail together** (Knight and Leveson, 1986).

## Decisions

1. **The certifier believes no input for an acceptance.** A verdict that accepts is derived from the term, from the certifier's own record, or from evidence the certifier checks. A record of elaboration's — a totality stamp, a polarity vector, a witness or concept list — may make it refuse, and never accept.
2. **Trust flows from simple code to complex code, never the reverse.** The certifier depends on certifier-grade code alone, and search stays outside its closure. A copy of an implementation is not independence and is not a goal; what the certifier depends on being simple is.
3. **What the kernel reaches is named in one module.** Every function of the shared layer a judgment calls is imported there and nowhere else in `curios-cert`, so one entering a verdict's reach is a line added to one file. The methods of the representation's own types are the representation, and are not listed.
4. **A set of alternatives is one list, and its answers agree.** The peels are stated once, and a debug build asks every one of them: an equality never meets a clash, and none after the first concludes more than it. The whole suite and both halves of the prelude's build run it over every pair conversion meets; a release build asks until one answers.
5. **An optimization keeps its slow path, and a cap refuses.** Each is held by a differential against the rule it replaces, or by a test that the cap answers undecided.
6. **No table of procedures.** What can hold a clause holds it where the procedure is; a clause nothing mechanises is the design decision's prose; a flaw is a ticket.

## Stages

1. **The peels.** Decision 4: one function in `curios-core`'s `spine`, called from both sites, with the three peels ordered around the list stated beside it with their reason.
2. **The roster.** Decision 3: the module, the kernel's imports moved to it, and a test over the crate's own sources that fails where another module imports a shared function.
3. **The statement, and one pass.** `lib.rs` separates literal folds from the carriers' theory and names the machine a trusted evaluator; the design decision says decision 1 as it is and names what holds clause 2. Each function on the roster is then read against clauses 4 to 6: a cap with no test that it refuses and an optimization with no differential gets one where that is small, and is a finding otherwise.

Each stage updates the claims it makes true; none is left to a last documentation stage.

## Open

- **Whether the certifier gets an evaluator of its own.** Decided from [the recorded profile](../../../curios-cert/README.md#measuring-the-certifier): the `reduce` and `reduce_forced` rows of the prelude's certification with the closed machine on, against the same stream with it off, which needs a walk that can leave it out as `Kernel::uncached` leaves the memos out. If the strategy alone certifies `/std` within the budget every item has, the machine leaves the kernel's path; if not, it stays a trusted evaluator.
- **The evaluator's unprobed route.** Whether a run-dependent rigid head could reach the elaborator's witness keying is unprobed.

## Rejected

- **A row per procedure in a README**, graded against the clauses. Nothing fails when a procedure enters a verdict's reach without its row, which is the inventory the soundness board rejects for rules.
- **Running the chain in every rotation.** Its steps rewrite the pair the next one reads, so most of it has no other order; and for the alternatives that do, every rotation answering alike is every pair of answers agreeing, which one pass over the list asks.
- **Asking every alternative in a release build**, the strongest answer winning. It would hold the order by construction and charge every comparison for it, where the debug build asks the same question of every pair the suite and the standard library produce.
- **A lint against wildcard arms in the trusted crates**, to hold clause 5 by exhaustive matches. It is a lint configuration with a design decision of its own to make.
- **Duplicating the shared code.** A copy fails where its original does. What the certifier needs is simple code, not a second one.
- **A ban on chains and retries**, which states a means: it excludes the algebra as it stands and admits a single unsound rule.
- **Compiling matches to eliminators and recursion to recursors**, as Lean's elaborator does, which would take inversion and termination out of the certifier. `Match` is an intrinsic of Core and partial recursion is allowed at run time: both are the language's, not its elaboration's.
- **Taking the algebra out of conversion by casts**, for the reasons [The carriers' algebra stays in conversion](../../design/arithmetic/the-carriers-algebra-stays-in-conversion.md) records.
- **A trusted decision procedure in the kernel.** A certificate is checked; the search that found it is not trusted.

## Non-goals

Certifying anything below Core; a verified certifier; a certifier runnable apart from the compiler over stored units; exporting proofs to another checker.

## Verification

- Stage 1: `cargo xtask clippy`, whose two prelude build scripts elaborate and certify `/std` under the assertion; `curios-core`'s, `curios-analysis`'s and `curios-cert`'s tests and `curios`'s `laws`; and a peel moved behind a weaker one fails the assertion.
- Stage 2: `curios-cert`'s tests, with a shared function imported outside the module failing the roster's.
- `curios/src/tests/board/` and `kernel_disagreements` stay clean at every stage.

## Completion and retirement

Done when the peels are stated once with their agreement asserted, every shared function the kernel reaches is named in one module, and the two statements say what the code does. The kernel's design decision carries the rule and what is rejected here, `curios-cert`'s rustdoc the roster, and the two open questions go where they belong: the first to `curios-cert`'s README beside the profile it is decided from, the second to [the findings](00-findings.md). Replace the roadmap entry with a checked summary, verify that nothing references this filename, and delete it.
