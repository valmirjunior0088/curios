# A lowering is held to its record

Broken rule: [A lowering is kept where its text and the names it resolved stand](../../design/compilation/a-lowering-is-kept-where-its-text-and-the-names-it-resolved-stand.md) keeps a declaration's lowering over a baseline where its text stands and every name it resolved is answered as it was, resting on the claim `curios-module/src/into_core/lowered.rs` opens with: a declaration reads its scope through two doors alone, and what they answer is everything its lowering reads of a name. The lowering reads more of the compilation than its record names — which prefixes it may write — so a lowering over a baseline keeps one a whole lowering would refuse, and nothing holds a lowering to its record to tell where else it does. The repair is the record, not a guard: a door reads only through the reading an edit compares, the reach is read as the rest is, and a census holds every declaration of `/std` to its record and to its answers.

The next step is stage 3, a delivery.

## What is wrong today, and where

- **The reach is read and not recorded.** `Context::guarded` asks `Reach::refused` which prefixes the unit may write, and `note_spelled` asks `Mount::owning` of every prefix the compilation mounts; no `Consulted` names either, and `Consulted::Mount` never moves (`moved_between`, `curios-module/src/into_core/interface.rs`, makes `Member` and `Members` alone). `into_core_unit_over`, `settled` over a baseline lowered under other mounts or other declared dependencies, keeps a declaration that writes a prefix the unit may no longer name. Nothing reaches it from the command line today: a store offers a baseline only at the slot its declared dependencies name (`Verdicts::slot`, `curios-verdict/src/verdicts.rs`), and a dependency dropped, still mounted through another or gone, is refused over the stored unit as it is whole (probed at `61bee406e`). The lowering's keep rests on a key of another crate's addressing.
- **What a step reads is stated twice.** A door reads a module's label through `visible_child` or `visible_binding`, each paired by hand with the `consult` recording it, eight times in `context.rs`, and a ninth reads the namespace a name resolved into (`Context::found`); what an edit compares of the label is `moved_between`'s own reading of its entry. The comparison holds everything a door reads today, and nothing keeps it so.
- **Nothing lowers one declaration alone.** `process_items` (`curios-module/src/into_core.rs`) lowers a declaration in its arms over `TopItem`, inline in the walk, beside a struct, `Lowering`, whose documentation opens with a line about a test's list that describes nothing it holds.

## What is known

- **A refusal's reads need no record.** `unresolved_head`, `guarded`'s refusal and the refusals a step raises (`unresolved`, `carriers`, `out_of_reach`) run only where a name resolves to nothing, which `Context::unbound` marks, and a bare name's in the same call that marks it (`Context::unbound_bare`), and such a declaration's record holds no key (`LoweredDeclaration::made`, `lowered.rs`): no later lowering keeps it, asked or not.
- **A body's scope at a declaration is what its unit holds.** A body's own labels are its table's (`ModuleInfo`), its imports are `Imports::body`, in order, and how many stand before each definition is the `InScope` it records (`Imports::scope`, `curios-core/src/names.rs`); the root binds each mount's name besides. A label is bound once in a body, so a lookup a kept declaration made found the binding its body holds.
- **What already holds a recompile.** `std_replay_census` (`curios-pipeline/src/standard/tests.rs`) holds each of `/std`'s elaborations to its trace; `std_recompile_reproduction` holds two edits, a leaf's and a hub's, to the bytes of a whole compile; `curios-module`'s revision tests (`same_unit`, `curios-module/src/into_core/revision/tests.rs`) hold a unit lowered over its baseline to its whole lowering, case by case.

## Decided

- **A label is read through one reading**, which records the entry it reads and returns it, and which `moved_between` compares; a door's step, a body's label and a glob are each read through it, so what an edit compares is what a door read.
- **The reach is part of what a lowering read.** A unit's baseline half records the prefixes its compilation mounted and the ones the unit could write, and a unit lowered over a baseline whose reach is not its own asks every declaration, as where a unit in scope moved.
  - Rejected: the reach left to the store's slot: the keep would rest on how another crate files.
  - Rejected: a `Consulted` for each prefix's reach, moving alone: a dependency edit is rare, and asking finds the declarations writing the prefix.
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

- **A standing test, or an ignored measurement beside `std_replay_census`.** What the census holds moves with every change to the lowering, which a measurement no task runs does not hold; a standing test costs every run of `curios-pipeline`'s suite. Decided at stage 6 by the census's time over `/std`, counted at its commit: standing where it costs what one of the suite's tests over `/std` does.

## Stages

3. **The reach recorded.** The baseline half holds the reach a unit was lowered under, and `into_core_unit_over` asks every declaration where it is not the unit's own; the verdict schema tag moves. Check: revision tests — `settled`, a dependency the unit may no longer write while still mounted, and one no longer mounted, each refused as the whole lowering refuses it. One commit.
4. **One reading of a label.** `Context` reads a module's label, a body's label and a glob each through one reading that records what it reads, and `moved_between` compares the entry that reading returns. Check: `/std`'s image digest unchanged; `curios-module`'s suites. One commit.
5. **A declaration's lowering is a function of its own.** `process_items`' arms over `TopItem`, from a `let` to a `satisfy`, move into one function the walk calls with the context and the lowering state, and `Lowering`'s documentation is its own. Check: as stage 4. One commit.
6. **The census.** Behind `curios-module`'s `test_support`, a scope of given entries — a record's, valued from its unit and its body's scope at its place, or a trace's answers alone — and the two checks over every declaration of `/std`, beside `std_replay_census`; fixtures where an entry taken from a record answers otherwise and a changed answer lowers otherwise, so each check refuses what it is for. Its time over `/std`, counted at its commit, settles the open question. One commit.

## Verification

`cargo xtask workspace lint` and the stage's tests by name at each stage; `curios-module`'s suites at every stage, `curios-pipeline`'s at stages 2, 3 and 6; the census at stage 6; the whole workspace's tests once stage 6 has landed.

## Retirement

Once stage 6 lands, [the decision](../../design/compilation/a-lowering-is-kept-where-its-text-and-the-names-it-resolved-stand.md) states what a door answers and that the reach is read, `lowered.rs`'s module documentation names the census that holds its claim, [A compilation is a graph of item tasks](300-item-tasks.md) cites the census in place of this specification, and this file is deleted.
