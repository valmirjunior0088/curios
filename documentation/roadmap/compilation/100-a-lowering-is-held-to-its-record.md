# A lowering is held to its record

Adopted: a census holds every declaration's lowering to its record and to its answers, so that what a recompile keeps is what a whole compile would make. [A lowering is kept where its text and the names it resolved stand](../../design/compilation/a-lowering-is-kept-where-its-text-and-the-names-it-resolved-stand.md) keeps a declaration's lowering over a baseline where its text stands and every name it resolved is answered as it was, resting on the claim `curios-module/src/into_core/lowered.rs` opens with: a declaration reads its scope through two doors alone, what they answer is everything its lowering reads of a name, and what they read is what its record names. The claim is held by reading alone, and a read past it goes unseen until a recompile keeps a stale lowering, as a concept's method read past the answer did: nothing lowers a declaration with no more than its record, or its answers, in scope. Settled with the user on 2026-11-18.

The next step is stage 6, a delivery.

## What is missing

- **A scope made of a record or of its answers.** A declaration is lowered by `lower_declaration` (`curios-module/src/into_core/declaration.rs`) in the context the walk builds over the whole of its unit and the units in scope; nothing builds one holding only the entries a record names, or only what its trace answered, and so nothing lowers or asks a declaration in one.

## What is known

- **A refusal's reads need no record.** `unresolved_head`, `guarded`'s refusal and the refusals a step raises (`unresolved`, `carriers`, `out_of_reach`) run only where a name resolves to nothing, which `Context::unbound` marks, and a bare name's in the same call that marks it (`Context::unbound_bare`), and such a declaration's record holds no key (`LoweredDeclaration::made`, `lowered.rs`): no later lowering keeps it, asked or not.
- **A body's scope at a declaration is what its unit holds.** A body's own labels are its table's (`ModuleInfo`), its imports are `Imports::body`, in order, and how many stand before each definition is the `InScope` it records (`Imports::scope`, `curios-core/src/names.rs`); the root binds each mount's name besides. A label is bound once in a body, so a lookup a kept declaration made found the binding its body holds.
- **What already holds a recompile.** `std_replay_census` (`curios-pipeline/src/standard/tests.rs`) holds each of `/std`'s elaborations to its trace; `std_recompile_reproduction` holds two edits, a leaf's and a hub's, to the bytes of a whole compile; `curios-module`'s revision tests (`same_unit`, `curios-module/src/into_core/revision/tests.rs`) hold a unit lowered over its baseline to its whole lowering, case by case.

## Decided

- **The census holds both halves of the keep**, behind `curios-module`'s `test_support`, from production's pieces, over every declaration of `/std`:
  - **The record names everything the doors read**, which holds a declaration kept unasked: each lookup of a declaration's trace, asked again through `Context::answers` in a scope holding only what its record names — the unit's entries for those labels, and its body's scope at its place — answers as recorded.
  - **The lowering reads nothing of its scope but the doors' answers**, which holds a declaration kept by asking: the declaration, lowered alone in a scope made of its trace's answers and nothing more — each lookup reaching its answer through export entries alone, a method through its namespace's entry — lowers to the items and mints the unit holds, a witness's minted name read as the item's own.
  - Rejected: the declaration lowered in its record's filtered scope, this specification's earlier census: it holds the unasked keep alone, and passes `/m/C/meth`, whose walk reads the namespace's entry, while the recompile keeps it stale.
  - Rejected: the doors answered from the trace with the tables whole, which leaves unchecked every read past the doors; in a scope of the answers alone, every such read meets nothing.
  - Rejected: a door answering from a trace by a mode of its own, which production would carry for the census alone.
  - Rejected: the walk's setup and body scope composed a second time to rebuild a declaration's scope, which its unit holds.
- **The census asserts.** A declaration it names is a defect of the lowering, fixed where the census lands or filed as one finding a cause.
- **The walk stays the one statement of a body's scope**, and production carries nothing only the census reads.

## Open

- **A standing test, or an ignored measurement beside `std_replay_census`.** What the census holds moves with every change to the lowering, which a measurement no task runs does not hold; a standing test costs every run of `curios-pipeline`'s suite. Decided at stage 6 by the census's time over `/std`, timed at its commit: standing where it costs what one of the suite's tests over `/std` does.

## Stages

6. **The census.** Behind `curios-module`'s `test_support`, a scope of given entries — a record's, valued from its unit and its body's scope at its place, or a trace's answers alone — and the two checks over every declaration of `/std`, beside `std_replay_census`; fixtures where an entry taken from a record answers otherwise and a changed answer lowers otherwise, so each check refuses what it is for. Its time over `/std`, timed at its commit, settles the open question. One commit.

## Verification

`cargo xtask workspace lint` and the stage's tests by name at each stage; `curios-module`'s suites at every stage, `curios-pipeline`'s at stage 6; the census at stage 6; the whole workspace's tests once stage 6 has landed.

## Retirement

Once stage 6 lands, `lowered.rs`'s module documentation names the census that holds its claim, [A compilation is a graph of item tasks](300-item-tasks.md) cites the census in place of this specification, and this file is deleted.
