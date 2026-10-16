# A bounded copy declines before it enumerates

**Not refined yet; waits for a consumer.** This specification reserves a copy plan that enumerates a binding group's bodies only as far as its allowance reaches, where it enumerates the whole group before it can decline today. It is the documented qualification of [bounded copy admission](../../../curios-cont/README.md#a-copy-is-planned-within-its-allowance), not a violation of its copied-node ceiling. It is not an implementation plan.

## What is missing

`CopyPlan::limited` (`curios-cont/src/optimize/clone.rs`) checks the distinct-node allowance when a pending body is popped, but a visited `LetFun` first reads every defined member, inserts its identity in `nested` and queues its body; `LetCont` likewise enumerates and queues its complete group. A request with an allowance of sixteen can therefore enumerate and queue thousands of members before its seventeenth distinct node refuses the copy, and repeated call sites repeat that work. Synthetic wide-group controls establish the shape; no current consumer is known to supply wide groups often enough for it to matter, and the copied-node allowance does not price group enumeration.

## Refinement

- **The consumer.** It opens when attribution names a program whose compilation pays for this.
- **The direction.** Group bodies enumerated incrementally in the shared planner, keeping complete successful extents, deduplication and fresh-plan execution; no second fuel limit, and no change to which copies fit.
- **Check**: counted group-member enumeration and pending work over doubling groups, identical exact-limit plans and nested captures, and unchanged consumer artifacts and scheduling.
