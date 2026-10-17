# A reduct states what it rests on

Working specification for keeping every answer either checker remembers — a reduct, a closed value, a sort, a verdict, a settled spelling — for exactly as long as what it read stands, and no longer. A table's lifetime is then a cost, and never what a declaration elaborates to.

## What this builds on

- **Every memo lives one budget** ([A reduction step costs what it builds](../../design/soundness/a-reduction-step-costs-what-it-builds.md)): both checkers clear every table where the budget is restored, so a hit is free and what a judgment spends is a fact about it alone.
- **The kernel gives an answer one of two lives** (`curios-cert/src/kernel/memos.rs`): one about local-free terms is the declaration's, and one read off the scope lives while the equations in force stand, filed with the binders it read under (`Prefix`).
- **No equation answers a term that names no local** ([Case equations and their key](../../design/soundness/case-equations-and-their-key.md)): both checkers record one only under a spelling that names a local and probe one only at a form that does, which is what lets the closed machine run inside an arm (`curios-core/src/machine.rs`).
- **The elaborator's reducts and closed values have the kernel's two lives** (`Caches`, `curios-elab/src/context/caches.rs`; `ClosedMemo`, `curios-core/src/machine.rs`): a `lasting` term's answer stands for the declaration and goes only at a redefinition or a rollback, and every other goes at each boundary of the scope.
- **A settlement is bounded by the declaration's budget alone**, as the kernel's is (`reduce`'s `settle`, `curios-elab/src/reduce.rs`), so no ceiling below the budget turns what a table holds into whether an entry settles.
- **A settled spelling already states what it rests on** (`Spelling`, `curios-elab/src/context/frame.rs`): the solutions committed and the universe state where either could move it, and a count of the times every spelling was forgotten at once.
- **Both checkers cost alike**, which the parity measurements hold (`str_literal_cost_measurements`, `kernel_memo_charge_measurements` and `combinator_web_cost_measurements`, in `curios/src/tests/reduction.rs`).

## The gap

**A scoped answer is dropped by the kind of boundary, not by what it read.** `Caches` clears every scoped reduct, sort and verdict wherever a guard is registered, a frame is left, a settlement withholds the frames inside its entry or restores them, a judgment stands on held frames or leaves them, levels are rewritten or answers withdrawn; a rollback and a redefinition clear the lasting ones too. Over `/std`'s elaboration at `d930af78e`, 59,828,912 units, 7,581,678 reduce a term again to the answer a clear had just dropped, or compare a pair again to the verdict it had just dropped:

| Boundary | Reducts | Verdicts |
| --- | --- | --- |
| A settlement's two sides | 3,837,058 | 1,016,610 |
| A guard registered | 824,291 | 407 |
| A rollback | 740,601 | 440 |
| A stand on held frames, either side | 421,666 | — |
| A frame left | 402,812 | 11,660 |
| A rewrite of levels | 239,602 | — |
| A redefinition | 56,474 | — |
| A stamped write | — | 29,020 |
| A fresh definition | — | 1,037 |

The reducts that name a local are 5,934,014 of them, those holding a metavariable and no local 12,844, those holding a universe metavariable and neither 393,989, and lasting ones dropped by a rollback or a redefinition 181,657. An answer that came back different was rightly dropped: 928 reducts and 2 verdicts. Counted by the census protocol below.

**A settlement is made again where its spelling was forgotten, and settles as it did.** Every spelling is forgotten at once at a redefinition, a rollback, withdrawn answers and every rewrite of levels, the closing of a witness's universe instance among them, once for each operator over a universe-polymorphic concept (`Context::close_universe_instance`). At `d930af78e`, 2,669 first settlements cost `/std` 10,703,712 units; 3,528 settlements of an entry already settled in the declaration cost 1,524,357 and came back as they were, and 3 came back otherwise.

**What an answer rests on is not handed on through a hit.** The questions a reduction declined and asked are counters a computation moves (`Asking`), and a hit moves none of them, so an answer computed over a remembered one does not know what that one rested on. While every scoped answer dies at every boundary this costs nothing; once lifetimes differ by what was read, it is a hole.

## Prior art

- **Lean 4's elaborator caches what no scope can move**: weak-head forms of terms with no free variable and no expression metavariable (`useWHNFCache`, [`Lean.Meta.WHNF`](https://github.com/leanprover/lean4/blob/master/src/Lean/Meta/WHNF.lean)), and definitional-equality verdicts in a permanent table where neither side holds a metavariable and a transient one, "frequently reset", where one does ([`Lean.Meta.Basic`](https://github.com/leanprover/lean4/blob/master/src/Lean/Meta/Basic.lean)). A transient key is instantiated before it is stored, since "the key is invalid after the assignment is backtracked", and a verdict is stored only where the call postponed nothing new ([`Lean.Meta.ExprDefEq`](https://github.com/leanprover/lean4/blob/master/src/Lean/Meta/ExprDefEq.lean)).
- **Lean's kernel invalidates nothing**: its `whnf` and `infer_type` caches are keyed by the expression alone ([`type_checker.cpp`](https://github.com/leanprover/lean4/blob/master/src/kernel/type_checker.cpp)), since every local has an identity of its own and no equation is local.
- **Salsa hands a memo's dependencies on to the queries that read it**, and summarizes them as durability, so a change validates only what read it and a recomputation that comes back equal is kept ([the algorithm](https://salsa-rs.github.io/salsa/reference/algorithm.html)); [Build systems à la carte](https://www.microsoft.com/en-us/research/uploads/prod/2018/03/build-systems-a-la-carte.pdf) names the two verifying traces and early cutoff.
- **Agda's reduction states what a stuck term waits on** ([`Blocker`](https://hackage-origin.haskell.org/package/Agda-2.6.3/docs/Agda-Syntax-Internal-Blockers.html)): a metavariable, a problem or a definition.
- **A SAT solver keeps an implied literal at the level of its reasons**, and chronological backtracking keeps it across a backtrack to any level at or above that ([Möhle and Biere, *Backing Backtracking*](https://link.springer.com/doi/10.1007/978-3-030-24258-9_18)): where facts are a stack, how far into it an answer read is one number.
- **GHC kicks out of its inert set only what a new equality can rewrite** ([*Make kick-out more selective*](https://mail.haskell.org/pipermail/ghc-commits/2024-August/063438.html)), the selective form of what registering a guard does here.

## Decisions

1. **A remembered answer is the answer asking again would give, for as long as what it read stands.** In both checkers and in every table. So keeping an answer longer never moves what a declaration elaborates to; where it does, what the answer rests on was stated short, and the stage that kept it is wrong.
2. **What an answer rests on is a footing**, a small value filed with it and joined through every hit into the computation that asked: the window of frames it was taken under, how far into the solution log it read, and the heads of the stuck forms it met. A boundary drops the answers whose footing it reaches, and no other.
3. **One rule for both checkers.** Where a boundary exists in both — a settlement, an equation in force — both land together, and the parity measurements are taken again.
4. **The audit.** The elaborator keeps today's protocol as a mode, every boundary clearing every table, set as the kernel's memos are switched off (`Kernel::uncached`). `curios-pipeline`'s `standard` tests elaborate `/std` from its tree both ways and require one module, item by item, naming the items that differ (`std_identity_census` is the model).
5. **A stage is built where its boundary is worth it.** One whose census share is under 1% of `/std`'s elaboration units is not built: its boundary keeps clearing wholesale, which is sound, and *Rejected* records it with its figure.

## The components

| Component | Treatment | Stage |
| --- | --- | --- |
| The audit | Wholesale mode; `/std` compared both ways | Audit |
| Settlements and stands | A table per window, the live one handed back | Windows |
| Settled spellings | Forgotten by what reaches each one's footing | Spellings |
| A guard's registration | Drops what met a stuck form its key can answer | Guards |
| Rollbacks | The solution log's watermark | Solutions |

## Stages

1. **Audit.** The wholesale mode and the test, which holds at `d930af78e` before anything else lands; where it does not, what it names is a finding fixed first.
2. **Windows.** A settlement and a stand are brackets: what the live window remembered before one is what asking again gives once it closes, since a settlement commits nothing and a stuck form is filed only once every entry it could match has been settled, and a stand writes nothing to the frames it sets aside. So each sets the live window's scoped tables aside at its entry and hands them back at its exit, sorts and verdicts with them, and what it reduces is filed for its own window and kept for the next bracket made under that window while the frames outside it stand unwritten. In both checkers: the kernel's settlement (`Kernel::settle_refinement`) narrows and restores the equations in force the same way (`Memos::begin_equations`, `Memos::begin_plain`). Figure: 3,837,058 and 421,666 units of reducts and 1,016,610 of verdicts. Check: the audit, `/std`'s units, the parity measurements, `runaway_measurements`.
3. **Spellings.** A spelling is forgotten only by what reaches its footing: a rewrite of levels forgets the ones holding a level it rewrites, a rollback the ones settled past its mark, withdrawn answers the ones a question was asked for, and a redefinition the ones that read the name; `Spelling`'s `forgotten` count retires. Figure: 1,524,357 units. Check: the audit, `/std`'s units, and `curios`'s `infix_under_a_guard_takes_its_witness_where_it_stands`, 79 operators under two guards.
4. **Guards.** A guard's registration drops the answers that met a stuck form its key could answer, and keeps the rest: every head the reduction met where an equation can key one, read under the spellings the probe and the shared rule read it under (`probe_spellings`, `curios_analysis::answers`), joined through hits. In both checkers, where an equation is assumed. Figure: 824,291 units. Check: the audit, `curios-cert`'s and `curios-elab`'s equation tests, the parity measurements.
5. **Solutions.** An answer is filed with the solution log's length when it was taken; a rollback to a mark, and a restamp of solutions past one, drop the answers filed past it, a lasting one included. Elaborator only. Figure: 740,601 units. Check: the audit and `/std`'s units.
6. **Retirement**, below.

## Verification

- **One module.** The audit, at every stage.
- **What it saves.** `/std`'s elaboration units over `cargo xtask profile-prelude elaboration <PATH>`, the `budget::consumed` row's total. Counted.
- **The census.** In a scratch copy, each clear moves what it drops into a side table tagged with its kind. Where a term so dropped is reduced again — the outermost such reduction only — the units it takes are credited to that kind and to the term's class (naming a local, holding a metavariable, holding a universe metavariable, or none) where the reduct comes back identical, and counted apart where it does not; a verdict asked again in `same_uncommitted_inside` is credited the same way, the stamp-driven clears filed as a write. A settlement of an entry already settled in the declaration, at the same frame and by the same reduction, is credited where its spelling comes back identical. Read off the `census::*` samples of the same profile. Counted.
- **Parity.** `str_literal_cost_measurements`, `kernel_memo_charge_measurements`, `combinator_web_cost_measurements` and `runaway_measurements`, at a stage that touches a boundary both checkers have. Counted.

## Design decisions this overturns or corrects

- [A reduction step costs what it builds](../../design/soundness/a-reduction-step-costs-what-it-builds.md): the memo rule gains the lives within a declaration and the footing. Retirement.
- `curios-elab/src/context/caches.rs`'s and `curios-cert/src/kernel/memos.rs`'s module documentation: the protocol stated per boundary becomes the footing's. Every stage.

## Rejected

- **Naming the declarations each step moves**, as the earlier statement of this specification planned: an answer kept past a boundary that moves the image rests on something its footing does not say, and naming the declarations leaves it so.
- **A footing in the elaborator alone**: the elaborator would spend less than the kernel at the boundaries both have, so a declaration it accepts could exhaust the kernel's budget.
- **A frame's exit dropping only what read the frame**, the deepest frame an answer read positively: 402,812 units at `d930af78e`, 0.67% of `/std`'s elaboration.
- **A rewrite of levels dropping only what holds a level it rewrites**: 239,602 units at `d930af78e`, 0.40%, and 393,989 for every reduct holding a universe metavariable and no local at every boundary, 0.66%, most of which the windows keep.
- **Sorts and verdicts outside a bracket on the kernel's lives**, past the stamped writes: 42,564 units at `d930af78e`, 0.07%. The `classes` row of `runaway_measurements` is the shape this would have answered; `/std` holds little of it.
- **A redefinition dropping only what read the name**: 56,474 units, 0.09%.
- **An assumption-based label per answer**, the minimal sets of assumptions it holds under (de Kleer's ATMS): frames are a stack, so a window is one value, and labels grow combinatorially.
- **Evaluation by environment, as smalltt's**, which needs no table to invalidate: a case equation is a fact of the scope no closed value carries, and it would replace both checkers' strategies, and the cost model with them.
- **Bounding the tables by eviction**, which [A reduction step costs what it builds](../../design/soundness/a-reduction-step-costs-what-it-builds.md) already rejects.

## Completion and retirement

The rule goes to [A reduction step costs what it builds](../../design/soundness/a-reduction-step-costs-what-it-builds.md), the protocol to the module documentation of `Caches` and of the kernel's `memos`, and this specification is deleted once its last stage lands.
