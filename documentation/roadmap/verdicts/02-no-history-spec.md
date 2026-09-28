# Verdicts, part 2: no verdict depends on history

Working specification for one caching rule shared by both checkers, under which a cache can change how fast a compilation runs and never what it decides. Today a declaration's acceptance can depend on what was compiled before it, which the serial compiler hides and a parallel schedule would turn into a verdict that changes between runs.

It needs nothing, and makes the serial compiler better on its own. It touches the elaborator's caches, so it lands after [the invariants campaign](../invariants/03-shared-term-costs-spec.md)'s instruments and first fixes, and never beside another change to the elaborator's solver.

## What this builds on

- **Per-declaration budgets.** The elaborator (`elaborate_module_item`) and the kernel (`check_definition`) restore the work budget at every item, so whether an item is accepted does not depend on what earlier items spent.
- **The kernel's memo discipline** (`curios-cert/src/kernel/memos.rs`): term-keyed tables live one declaration and hit free; the one table outliving a declaration is keyed by name and charged. That is the rule under which a cache cannot change a verdict with the schedule. `kernel_memo_parity` holds the memo to the unmemoized kernel.
- **What the invariants work left in the elaborator's caches.** Within an oracle bracket each memoizable node is elaborated once, in a table that lives only as long as the bracket, and a rollback clears only what it undid, from whichever tables hold it. The rule below changes neither: the bracket's table already lives inside one declaration, and a rollback's invalidation holds whatever a table's lifetime.

## The gap

**Acceptance already depends on history.** The elaborator's closed-reduct cache outlives the declaration that filled it and a hit on it is free, so a declaration that would have hit a warm cache can exhaust its budget when compiled after different neighbours — `curios-core/src/retention.rs` calls this the elaborator's warmth-dependence. It is a defect of the serial compiler, and under a parallel schedule it would be a verdict that changes between runs.

## The decision

**One caching rule for both checkers.** A table that lives one declaration hits free. A table outliving a declaration is keyed by name and a hit on it costs what recomputing would. Under that rule the compilation-wide retention allowance can change how fast a compilation runs and never what it decides, so it stays compilation-wide.

## Stages

1. **The rule, in the elaborator.** The elaborator's closed-reduct and elaboration caches take the kernel's split: term-keyed tables cleared at the declaration boundary, one name-keyed unfold table charged. Charging recorded costs everywhere is not the rule — `curios-cert`'s README records how recorded costs compound. Measured over `/std` and the corpus in `programs/`: the heaviest declaration's consumption, as `Context::heaviest_declaration` reports it, against `DEFAULT_STEP_BUDGET`, and wall time, before and after. Headroom that shrinks is answered by raising the budget by the measured figure.

## Verification

- An item's verdict is the same compiled alone and after its neighbours, held by a fixture.
- Every verdict over `/std` and the corpus in `programs/` is compared with the compiler before the stage; a difference is a finding, never a fixture update.
- The stage reports the time it moved, and the headroom it measured.

## Design decisions this overturns or corrects

`curios-core/src/retention.rs` and `curios-cert/src/kernel/memos.rs`: the caching rule is stated once, for both checkers, and the warmth-dependence paragraph is deleted with the warmth.

## Rejected

- **Keeping the warm caches and ordering the schedule** to reproduce the serial order: it serializes exactly what [part 6](06-item-tasks-spec.md) parallelizes, and keeps a defect.

## Completion and retirement

Both checkers follow one caching rule, stated once, and no verdict depends on what was compiled before it. Record the rule in `curios-core`'s retention documentation and both checkers' memo documentation, replace the roadmap entry with a checked summary, verify that nothing references this filename, and delete it.
