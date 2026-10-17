# Pending work is owned by its specification

**Decision.** Pending work lives under [`roadmap/`](../roadmap/), with its status in the finding or specification that describes it. Implemented contracts live in the owning reference, rustdoc, crate `README.md` and tests; rationale and rejected alternatives live in a design decision or the owning crate's `README.md`. Neither pending work nor landed capabilities have a second inventory.

## Filing and order

The areas follow this directory's subjects, one directory per area under `roadmap/`. Work belonging to the workspace as a whole is filed loose at its root. A file's name opens with the order its work is taken in:

- `000-findings.md` comes first: the small fixes and possible bugs.
- A specification numbered from `100` describes code that breaks a rule a document states, and opens by linking that rule. A design decision states the intended rule; a gap is a specification here, never a caveat there.
- A specification numbered from `200` describes a removable cost for which no document states a rule.
- A specification numbered from `300` describes a missing capability for which no document states a rule.

Within each hundred a lower number is taken first. A number is a place in that order, never a permanent identifier: a retired specification leaves its number free, and one filed ahead of others takes a free number at its place or moves the later specifications up with every path that cites them. A filename spells its subject out, and references cite the file directly. Directory listings provide discovery without a maintained index.

## Findings and specifications

A finding is a bullet led by the finding in bold and naming where, what is wrong, the fix, its check and its size. Findings are worked as one pass. An entry marked uncertain states what is not yet known and is investigated before its fix is taken. An entry stays a finding while its fix is stated and takes one pass, one commit under one check; it becomes a specification where the fix takes stages that land apart, rests on a question an answer on the spot cannot settle, or falls short of one rule beside others of its size. The commit that fixes a finding deletes its entry.

A specification states what is missing or wrong today and where, its context from the code, its goal, decisions already settled and questions still open, stages each with its own check, its verification and its retirement. One not yet refined is marked **Not refined yet** and states what is known and what it waits on. A specification that waits for a consumer says so. A measurement it depends on states a protocol precise enough to rebuild its scripts; the scripts are not checked in.

## Landing and retirement

When a stage lands, rewrite the specification it extends to describe the remaining work; do not stack a new specification beside it or leave completed stages as pending. Contracts go to their authoritative owners and rationale to the owning decision as the work lands. Once all the work has landed, update references to those owners, verify that nothing references the specification and delete it. No checked summary replaces it. Open new work only for a limitation named from the code: what is missing, refused or wrong, and where.

**Rationale.** A finding or specification already holds its own status and the context a contributor starts from. A second summary has to change whenever the first changes, and its existence cannot ensure agreement. A landed capability's contract is held by its documentation and tests, so maintaining a second account of what exists adds another place for it to drift. The directory structure exposes pending work and its order without making a reader synchronize an index.

**Rejected.** A maintained checklist beside the specifications, which duplicates status; a replacement inventory of landed capabilities, which duplicates their owners; filing gaps as caveats in design decisions, which makes an intended rule depend on the current implementation; and putting the filing rules only in agent instructions, which leaves contributors without their workspace policy.
