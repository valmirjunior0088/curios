# Restoring recursion to the defunctionalized walks

Nine walks in this workspace were rewritten from recursion into explicit frame machines, each to stop a *data*-shaped depth from reaching the native stack: a string literal's scan-state chain, a `Str` literal's per-byte UTF-8 derivation, a spine built by a loop. The motivation was real and is not in dispute. What it cost is the thing a checker can least afford to spend.

`curios-base`'s `recurse` bracket now buys the same depth safety without it. The kernel's three walks were restored first — `convert.rs`, `infer.rs`, `whnf.rs`, at −242/+131 — and eight more have followed. This specification tracks what remains, records what the work has corrected about its own premises, and closes the one question that should *not* be answered by migrating.

## Why this is a correctness argument and not a style one

`curios-elab` and `curios-cert` implement reduction and conversion twice, deliberately, so that a bug in one is caught by disagreement with the other. `whnf.rs`'s own module documentation states the terms: "a bug shared by both checkers is a bug neither can catch," and the crate boundary exists so the duplication cannot collapse back into a call. That bet is only paid out by a person reading the two side by side. Two hand-rolled state machines are far harder to diff than two recursive strategies, so defunctionalization erodes the value of the duplication it sits inside.

The kernel work produced the concrete instance. `infer.rs`'s worklist replaced `check(kernel, param, &domain)?` with an obligation pushed onto a stack and later discharged by the node rule plus `subsumes` — which **skips the three checked rules `check` dispatches first**: let-descent, Π-introduction, Σ-introduction. Deferred positions are exactly arguments, constructor payloads and record fields, so `f((x) => e)` and `f((a, b))` are the shapes that lost them. Nothing in the prelude or the corpus reached the one shape that would show it, so every gate passed for two weeks. The commit that introduced it was about stack depth; its comment argues only that *ordering* was preserved, which it was. Nobody decided to change the rule set, and nothing about the change looked like it touched the rule set.

That is the class this work exists to close, and it is why the acceptance criterion below is not "the tests still pass."

## What tells a machine from a loop

The first pass over this inventory misclassified a site, and the test that failed is worth writing down. "It iterates a `Vec`" is not the question — the lowerer's `let` sites iterate a flat `Vec<LetBinding>` and are machines anyway. The question is what the *accumulator* holds:

- A **loop** accumulates results. `elaborate_let` gathers `(label, type, body)` triples and folds them into `Term::let_` — no context effects on the way back, nothing fallible, one flat scope throughout.
- A **machine** accumulates frames. `lower_let_region`'s `PendingLet` holds `mark`, `binders`, `binds`, `binder`, `type_`, `value` — which is `build_let`'s local variables, field for field, twelve lines away in the same file. Its reverse pass runs `leave_scope(mark)` and a fallible `wrap`.

Three tells, any of which is decisive: an element that mirrors a function's locals; a reverse or pop phase carrying *effects* (scope exit, unwinding) rather than construction; and a comment arguing that ordering is preserved, which is the argument a call stack makes unnecessary.

## The walks

| Walk | Site | Status |
| --- | --- | --- |
| kernel conversion | `curios-cert/src/kernel/convert.rs` | done, `fb81e456` |
| kernel typing | `curios-cert/src/kernel/infer.rs` | done, `1fbb297c` — a rule set had drifted; see `SOUNDNESS.md` |
| kernel reduction | `curios-cert/src/kernel/whnf.rs` | done, `3a3b8f0b` |
| `curios-elab` reduction | `reduce.rs`, `PendingMatch` | done, `475759c9`, −46/+42 |
| `curios-elab` universe zonk | `universe_solver.rs`, `zonk` | done, `cdd5fb34`, −58/+32 |
| `curios-elab` universe search | `universe_solver.rs`, `check_consistent_full::choose` | done, `48dcb8a8`, −95/+42 |
| `curios-core` printing | `print.rs`, `sub`/`sub_intrinsic` | done, `60bdd365`, −25/+20 |
| kernel satisfiability | `curios-analysis/src/satisfy.rs`, `choose` | done, `eec7ec6c`, −53/+51 |
| erased-module verification | `curios-ersd/src/verify.rs`, `Task` | done, `10f5ac17`, −147/+76 |
| size-change totality | `curios-analysis/src/totality.rs`, `Op` | done, −336/+170 |
| `curios-text` lowering | `into_core/lowerer.rs`, two `let` sites | done, −74/+40 |
| `curios-elab` elaboration | `elaborate.rs`, `Vec<ElabFrame>` + `work_term`/`work_mode` | **pending** — M4 |

## What this work corrected about its own premises

**The motivating input no longer exists.** `20a58e38` replaced a string literal's per-byte `Utf8` derivation with a constant-size `of_scan_eq(b, refl_scan(b))` proof, so a literal is now a packed `Bytes` plus an O(1) proof. Four consequences. The lowerer's `utf8_derivation` — plausibly one of the "two sites" this document first attributed to that crate — was deleted then, not still pending. `curios/src/tests/strings.rs:100` still describes the spine it built. `curios-elab/README.md` justifies its machinery by that shape. And nothing in the corpus now produces a deep ground all-explicit application spine, which is M4's whole input class: **its machine has no live input, and no existing test would notice its deletion.**

**The universe solver held two walks, not three sites.** `zonk` and `choose`. The other `while let Some` loops in that file — `Potential::restore_from`, `connected_metas`, the solve worklist — are genuine graph and fixpoint algorithms that were never recursion.

**The lowerer's two sites are its `let` blocks**, and the literal acceptance below could not be run on them: the loop landed in `a4b69386`, and `1dc35095` then flattened the AST, so the pre-defunctionalization original lowers a different tree. `build_let`, in the same file, was the live worked reference instead — it does the identical step by recursion through `bound`, for the first binding of every block whose remaining bindings went through the machine. Both are restored against it, and `enter_scope`/`leave_scope` folded back into `bound`, the doc justifying their separation having named the loop that needed them.

**The near-miss there is worth more than the migration.** The two sites lower a binding's halves in *opposite* orders — `lower_let_region` value-then-type, `subterm`'s arm type-then-value — and both `term` and `collect` mint metavariables and fresh binders. They read as duplicate code inviting a merge, and merging them would renumber minted identities across the corpus, surfacing as changed names in diagnostics rather than as a failed assertion. Each order is preserved and `lower_let` now says why, so the next reader who notices the near-duplication does not collapse it.

**The lowerer is otherwise unguarded.** It makes 178 recursive self-calls over surface depth and contains no `recurse` at all; exactly one node kind was defunctionalized. So the machine is a local exception inside a function that recurses freely, not a defence of a walk that is careful elsewhere. Whether `Lowerer::term`/`subterm` should be guarded is a real question this raises and does not settle.

## Machines outside the original inventory

Found by applying the test above across the workspace, after a sweep that also came back empty for `curios-wasm`, `curios-package`, `curios-runtime`, `curios-unit` and `curios-pipeline` — none of which holds an explicit work-stack loop at all. `curios-cont` holds ten, and nine pop a bare id into a visited set: reachability and call-graph fixpoints, not reified frames. So the inventory is closed, and one machine remains unscheduled:

- **`curios-cont/src/cps/cse.rs`** — the `Task::Visit`/`Task::Retract` walk, 171 lines. `Retract` is a scope exit, and its doc argues the ordering by hand: "The LIFO order makes retraction happen exactly between a `LetCont`'s sibling subtrees." The only one of these whose detector is the codegen corpus rather than the prelude build, because the prelude archive stops at Ersd and nothing in the light gate reaches the Cont stage.

Three of the four have since been restored, and all three are in the table above.

**`curios-ersd/src/verify.rs`** reified all three of what `recurse` retires, which is why it was taken first: sibling ordering (`push_sequence` extended the stack then reversed the slice, because the sequence was "given in execution order"), scope entry and exit (`BindValue`/`UnbindValues`, `UnbindFunctions`, `EnterFunction`/`ExitFunction`, `EnterInit`/`ExitInit`), and unwinding. Each collapsed into something the language already says: the two function tasks into `walk_function`'s pair of lines, the bind/unbind tasks into one `scoped_block` bracket now shared by arm binders, fold binders and function parameters, and the init tasks into a push and pop around the recursive call. The error path was deliberately preserved rather than improved — `?` propagates before the unbinding, exactly as the task stack abandoned its pending work.

**`curios-analysis/src/totality.rs`** was the largest, at 995 lines and 52 `Op::` sites from `407d99c2` (+481/−230), and the one whose machine argued hardest for itself: *"Arms expand lazily, and that is the correctness argument for the whole stack."* That argument is real — an arm's guard read, shape read and binder minting are effects on the checker (`Env::force` spends a reduction budget, `Env::fresh` mints an identity), so the order arms are reached in *is* the order those effects land in, and a differently-ordered spend against a nearly exhausted budget reads a different shape. What changed is who guarantees it. Each arm's reads now happen in the loop iteration that walks that arm, because that is where the recursion puts them; `Op::RecBodies`'s one-body-at-a-time materialization is a `for` over the group's length; the `Enter`/`Walk`/`Exit` triple is one `scoped` bracket; and the four telescope ops are two loops, a telescope being a list. The paragraph that argued the machine reproduced call order is four lines saying why the order matters.

**`curios-analysis/src/satisfy.rs::choose` was the one whose citation had gone stale**, since it named `universe_solver::choose` as "linearised for the same reason" after that walk was restored. Restoring it surfaced a distinction the frame machine had kept implicit, and it is the sharpest thing this campaign has turned up since `infer`'s: the function returns plain `bool`, conflating *refuted* with *budget exhausted*, where its elaborator twin separates them in the return type. The iterative form abandoned the entire search on exhaustion by returning straight out through every pending frame. A naive recursion instead backtracks into it — retrying alternatives and committing arcs the old form never touched — because a `false` from a descent reads as refutation. The restoration checks the budget after each descent and propagates without rolling back, which reproduces the abort exactly. Nothing observes the difference today, since `Search` is local to `satisfiable` and an exhausted decision refuses either way; it is recorded because the next reader of that `bool` deserves to know it carries two meanings.

Checked and deliberately **not** on this list: `invert.rs`'s `Step` (a three-valued verdict), `uncurry.rs`'s `Resume` (a call-site ABI concept), and `interpret.rs`'s `Frame` (an interpreter's lexical environment). `curios-wasm`, `curios-package` and the `into_wasm` emitter have not been swept.

## What must not be migrated

`Term::walk` (`curios-core/src/walk.rs`) raises two questions, and only the first is settled. Keeping them apart matters, because this section used to answer the first and read as though it had answered both.

**Dissolving the driver into hand-written recursions at its call sites is closed, not deferred.** Its value is not depth safety at all:

> Children are enumerated exclusively through `Subterm::any_child_term`, so a new term former flows into every analysis by extending that one fold.

Trading one child-enumeration seam for eight loses automatic propagation of a new term former. The same criterion that condemns the frame machines protects this one: a driver with hooks is more legible than the recursion it replaced, not less. `collect_labels` was moved *onto* it for exactly that reason.

**Whether the driver's own internals should recurse, behind an unchanged API, is deferred indefinitely** — not closed, because the finding stands: by this document's test they *are* a machine. All four tells are in those forty lines: `Frame::Exit(Term, usize)` is a recursive frame's locals written out (the node, and how many results below belong to it); `exit` runs on pop carrying `&mut S`; `results.len().checked_sub(child_count).expect("each exit frame owns its child results")` is a hand-rolled calling convention with a hand-checked invariant; and `children.into_iter().rev()` is sibling ordering by manual reversal. The seam, the single `any_child_term` fold and every call site would survive a recursive reimplementation untouched, so none of the argument above bears on it.

**What settles it is that the stack was never what this driver is for.** It exists to provide two seams: one child-enumeration fold, so a new term former reaches every analysis; and a place for each analysis to hang its own memo, which is why the driver deliberately owns none and prunes through `Enter::Skip`. `curios-elab/src/totality/reach.rs` records the two measurements that separate them — "2.5s of a 3.5s compile at 12KiB" was O(n²) *paths* through a shared DAG, cured by deduplicating on node identity in the `enter` hook, and a stack overflow above 16KiB was depth, cured by the explicit stack. The first is the one the driver was built for, and it lives in the hooks. A recursive reimplementation would address only the second, which `recurse` would handle equally well but which was never this component's reason to exist.

Against that, the cost is the widest blast radius remaining: the kernel's erasure obligations, both positivity and totality drivers, `reach`, `collect_labels`, and `warm_frees`/`warm_scalars` — the per-node memo fill behind every `free_vars()` call in the compiler. And the payoff is legibility in a component that decides nothing, since every analysis's semantics live in its two hooks; a defect here breaks eight analyses loudly rather than changing one rule silently, which is the opposite of the failure this campaign exists to close. There is also no twin to read it against, so the argument in the first section has no purchase at all.

**No work is scheduled, and the criterion alone is not a reason to reopen it.** What would be: a measurement. The one figure that bears on it names `warm_frees` directly and points toward the recursive form being faster on the hottest walk in the compiler — but it is debug, single-input and correctness-unverified, so under the prerequisite below it may not be quoted as a reason to migrate. Anyone reopening this should arrive with a probe, and should know that the recursive helper must take *named* generic parameters for the two closures rather than `impl FnMut` in argument position, or each level instantiates at `&mut &mut F` and monomorphization diverges.

`curios-base`'s `Printer` also stays data. Its explicit stacks make *running* and *freeing* a document iterative, which no builder-side guard replaces, and three fixtures hold it at 100k depth. Note that `deferred()` and `Printer::Deferred` now have no producer anywhere in the workspace, since `print.rs` was their only caller — whether to delete them is open.

## Milestones

- ~~**M1 — `curios-elab`'s reducer.**~~ Done. `PendingMatch` gone; the `Match` arm now reads as the kernel's. The surviving asymmetry — elab passes the original scrutinee beside the forced value, for the call-by-name projection rule the kernel does not need — was promoted from an implicit fact to a stated one in `reduce_match`'s doc, because it is now the only thing a reader diffing the two must account for. Budget accounting was checked rather than assumed and is unchanged in both the warm and cold cases.
- ~~**M3 — the universe solver.**~~ Done, both walks. `zonk`'s path-scoped cycle guard is now carried structurally: a metavariable is on the path exactly while its own call is open. `choose` keeps its exploration order, its budget decrement before the terminal check, and the revert-then-read ordering its old `Step::Resume` comment described as "the order the recursive form had." The `SOUNDNESS.md` satisfiability entry was re-read; the walk's shape is not part of what it argues, which concerns grounding heads in the base potential.
- ~~**M2 — the lowerer.**~~ Done, both halves. The guard sits on the two recursions the change introduced, not on `Lowerer::term`/`subterm`, so the crate's other 178 recursive self-calls over surface depth stay unguarded exactly as they were — the open question above, deliberately not widened into here.
- **M4 — elaboration.** Last, because it is the largest, the most interleaved, and the only one carrying a mode beside the term. Deleting `ElabFrame` removes a hand-specialized *copy* of `elaborate_apply` restricted to a class — the same duplication shape that produced `infer.rs`'s drift — which already falls back to the real thing whenever its gate declines. `elaborate` returns to its pre-`79063bc1` shape plus `record_checked`, which must stay outside the cache because it fires on hits too. *Acceptance:* the frame stack and both work variables gone; a deep-spine fixture written first, since the corpus no longer supplies one.

## Acceptance, for every milestone

**Diff the restored walk against the pre-defunctionalization original**, recoverable via `git show <commit>^:<path>`. The question is not whether the new code passes; it is whether the *machine* had silently changed a rule, as `infer.rs`'s had. Where it did, the restoration is a behavior change and says so in its commit message and in `SOUNDNESS.md`, rather than riding along inside a refactor.

| Walk | Introduced by | Path at that commit |
| --- | --- | --- |
| `curios-elab` reduction | `f2dcc251` | `curios-core/src/reduce.rs` |
| `curios-elab` elaboration | `79063bc1` | `curios-core/src/elaborate.rs`, `elaborate/apply.rs` |
| universe zonk | `c46571d5` | `curios-elab/src/universe_solver.rs` |
| universe search | `434f32fb` | `curios-elab/src/universe_solver.rs` |
| lowerer `let` sites | `a4b69386` | `curios-text/src/into_core/lowerer.rs` — but see the AST-flattening caveat above |
| printer document | `c8491ba4`, `9fbdf1dd`, `13cda5a1` | `curios-base/src/monads/printer.rs`, `curios-core/src/print.rs` |
| `collect_labels` | `a1ad4189` | `curios-core/src/print.rs` |
| totality walk | `407d99c2` | `curios-cert/src/totality.rs` |

Paths before `352b030c` are `curios-core/…`, which is today's `curios-elab`. This table replaces a task list that no longer exists.

**A gate that passes is not evidence of equivalence here.** `infer.rs`'s drift survived every gate for two weeks, because the corpus does not reach the shape. State what the tests would have to contain to detect the difference, and record its absence when they do not contain it.

Two fixtures were written because the answer was "nothing would detect it," and both were mutation-checked by removing the guard and confirming the abort:

- `curios-elab`'s `reduce::tests::a_match_tower_reduces_without_overflowing` — 10,000 levels, each scrutinee the level below, sized far inside the step budget so it tests the stack rather than the budget.
- `curios-core`'s `print::tests::a_deep_term_is_printed_without_overflowing` — a 100,000-link spine rendered through `Display`.

M3 needed neither: `zonk`'s depth is the metavariable-solution chain the fixed prelude drives, and `choose`'s is the branching-clause depth `/std/Async/block_on` takes four hundred deep, so `curios-prelude`'s build script — which elaborates and certifies the whole prelude, and runs during `cargo clippy` — is the fixture.

**Which walks the light gate reaches, and why it matters here.** `cargo clippy --workspace` builds `curios-prelude-archive`, whose build script elaborates `/std` and `/syn`, erases them through `erase_unit`, and hands the result to `curios-ersd`'s `verify`; then `curios-prelude`'s script certifies the whole module with the kernel, which calls `satisfiable` on every universe context before assuming it. So a walk on the Text, Core, Ersd or certification path is exercised over the entire standard library by a step the gate already runs, and needs only its crate's own tests beside it. A walk *below* Ersd is not: the archive stops there, so `curios-cont`'s passes have no light-gate coverage at all and their detector is the codegen corpus in `curios`. That line — not the size of the diff — is what decides whether a restoration can be verified cheaply.

**Keep the trampolines.** `Step` and `Reduce::Continue`/`Break` are not part of the frame machines: they make the reduction *sequence* iterative, so a hundred-thousand-step fold costs zero native frames. Removing them would put one frame per reduction step, which is a different order of magnitude from one per structural link.

## Prerequisites still unmet

Both were recorded when the migration was first evaluated and neither is discharged. Neither blocks the milestones, and both bound what may be *claimed* about them.

- **Every performance figure so far is debug.** The one measurement taken — defunctionalization costing up to 1.73× on `Term::warm_frees` — was a debug build over one synthetic string literal, with correctness unverified. Do not quote it as a reason to migrate; the reason to migrate is in the first section. A restatement of it as "75% faster for 2% more memory" has already been observed in conversation, which is the decay this repository's measurement rule exists to prevent.
- **Memory is unbounded by design and unmeasured in practice.** `recurse` grows rather than aborting, so a runaway type-level computation now runs until the *budget* stops it — a deep accumulator reached 233 MiB where it previously died. That trade is deliberate and stated at `curios-elab`'s `reduce`. What is not stated is a ceiling, because the budget bounds steps and not memory. Note that this figure is heap for *terms*, not stacker's segments, which are allocated only once a walk approaches its limit and are freed with the guard.

## Documentation this campaign falsifies

Not owned by any single milestone, so none of them will carry it. Fold it into the amendment rather than leaving it to be discovered.

- `curios-elab/README.md` records, under a rejected alternative, that "Raising the stack instead was never on the table — recursive lowering working on the default stack is a contract, not a tuning knob." `recurse` is raising the stack. `CLAUDE.md` carries a matching invariant about the default test-thread stack.
- `curios/src/tests/strings.rs:100` describes the per-byte derivation and the iterative elaboration that absorbed it. The derivation is gone and the elaboration is scheduled to be.
