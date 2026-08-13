# Restoring recursion to the defunctionalized walks

Nine walks in this workspace were rewritten from recursion into explicit frame machines, each to stop a *data*-shaped depth from reaching the native stack: a string literal's scan-state chain, a `Str` literal's per-byte UTF-8 derivation, a spine built by a loop. The motivation was real and is not in dispute. What it cost is the thing a checker can least afford to spend.

`curios-base`'s `recurse` bracket now buys the same depth safety without it, and the kernel's three walks have been restored — `convert.rs`, `infer.rs`, `whnf.rs`, at −242/+131 lines. This specification covers the six that remain, and closes the one question that should *not* be answered by migrating.

## Why this is a correctness argument and not a style one

`curios-elab` and `curios-cert` implement reduction and conversion twice, deliberately, so that a bug in one is caught by disagreement with the other. `whnf.rs`'s own module documentation states the terms: "a bug shared by both checkers is a bug neither can catch," and the crate boundary exists so the duplication cannot collapse back into a call. That bet is only paid out by a person reading the two side by side. Two hand-rolled state machines are far harder to diff than two recursive strategies, so defunctionalization erodes the value of the duplication it sits inside.

The kernel work produced the concrete instance. `infer.rs`'s worklist replaced `check(kernel, param, &domain)?` with an obligation pushed onto a stack and later discharged by the node rule plus `subsumes` — which **skips the three checked rules `check` dispatches first**: let-descent, Π-introduction, Σ-introduction. Deferred positions are exactly arguments, constructor payloads and record fields, so `f((x) => e)` and `f((a, b))` are the shapes that lost them. Nothing in the prelude or the corpus reached the one shape that would show it, so every gate passed for two weeks. The commit that introduced it was about stack depth; its comment argues only that *ordering* was preserved, which it was. Nobody decided to change the rule set, and nothing about the change looked like it touched the rule set.

That is the class this work exists to close, and it is why the acceptance criterion below is not "the tests still pass."

## The remaining walks

| Walk | Site | Depth it absorbs |
| --- | --- | --- |
| `curios-elab` reduction | `reduce.rs`, `PendingMatch` | Match towers — the direct twin of the kernel's `Pending`, already restored |
| `curios-elab` elaboration | `elaborate.rs`, `Vec<ElabFrame>` + `work_term`/`work_mode` | The largest and most interleaved; a mode threaded beside the term |
| `curios-elab` universe solving | `universe_solver.rs` at three sites | Two worklists and a `Vec<Frame>` |
| `curios-text` lowering | `into_core/lowerer.rs` at two sites | Surface-tree depth |
| `curios-core` printing | `print.rs` | Document construction over a deep term |

## What must not be migrated

`Term::walk` (`curios-core/src/walk.rs`) stays. It is not a bespoke machine — it is one shared Enter/Exit driver behind which six call sites write two hooks each, and its value is not depth safety at all:

> Children are enumerated exclusively through `Subterm::any_child_term`, so a new term former flows into every analysis by extending that one fold.

Dissolving it into six hand-written recursions trades one child-enumeration seam for six and loses automatic propagation of a new term former. The same criterion that condemns the frame machines protects this one: a driver with hooks is more legible than the recursion it replaced, not less. **This question is closed, not deferred.**

## Milestones

Ordered by payoff per unit of risk, and by what each leaves behind for the next.

- **M1 — `curios-elab`'s reducer.** The kernel's twin is already done, so this is the one migration with a worked reference to be written against, and the pair is what the duplication argument is *about*. *Acceptance:* `PendingMatch` gone; the two reducers readable side by side; the two refinement probe points still distinct and still commented as such.
- **M2 — the lowerer and the printer.** Neither is a judgment, so a drift here refuses or misprints rather than admitting. Cheap, and they establish the pattern outside the checkers. *Acceptance:* both migrated; a deep-literal fixture that would have overflowed pre-`recurse` still passes.
- **M3 — the universe solver's three sites.** Its walks decide satisfiability, which `SOUNDNESS.md` already carries an open entry about. *Acceptance:* all three migrated, with the satisfiability entry re-read and amended if the walk's shape is part of what it argues.
- **M4 — elaboration.** Last, because it is the largest, the most interleaved, and the only one carrying a mode beside the term. *Acceptance:* the frame stack and both work variables gone.

## Acceptance, for every milestone

**Diff the restored walk against the pre-defunctionalization original**, recoverable via `git show <commit>^:<path>` — the ten commits are recorded on the task list. The question is not whether the new code passes; it is whether the *machine* had silently changed a rule, as `infer.rs`'s had. Where it did, the restoration is a behavior change and says so in its commit message and in `SOUNDNESS.md`, rather than riding along inside a refactor.

**A gate that passes is not evidence of equivalence here.** `infer.rs`'s drift survived every gate for two weeks, because the corpus does not reach the shape. State what the tests would have to contain to detect the difference, and record its absence when they do not contain it.

**Keep the trampolines.** `Step` and `Reduce::Continue`/`Break` are not part of the frame machines: they make the reduction *sequence* iterative, so a hundred-thousand-step fold costs zero native frames. Removing them would put one frame per reduction step, which is a different order of magnitude from one per structural link.

## Prerequisites still unmet

Both were recorded when the migration was first evaluated and neither is discharged. Neither blocks the milestones, and both bound what may be *claimed* about them.

- **Every performance figure so far is debug.** The one measurement taken — defunctionalization costing up to 1.73× on `Term::warm_frees` — was a debug build over one synthetic string literal, with correctness unverified. Do not quote it as a reason to migrate; the reason to migrate is in the first section.
- **Memory is unbounded by design and unmeasured in practice.** `recurse` grows rather than aborting, so a runaway type-level computation now runs until the *budget* stops it — a deep accumulator reached 233 MiB where it previously died. That trade is deliberate and stated at `curios-elab`'s `reduce`. What is not stated is a ceiling, because the budget bounds steps and not memory.
