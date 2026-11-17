# A lowering is held to its record

A unit lowered over a baseline keeps a declaration's lowering where its text stands and every name its record says it resolved resolves as it did ([A lowering is kept where its text and the names it resolved stand](../../design/compilation/a-lowering-is-kept-where-its-text-and-the-names-it-resolved-stand.md)). That rests on the claim `curios-module/src/into_core/lowered.rs` opens with: a declaration reads its scope through two doors alone, `Context::resolve_term_name` and `Context::resolve_bare`, and each binding they read is recorded as a `Consulted`. A read outside the record would keep a stale lowering unseen, and nothing holds a lowering to its record. `curios-pipeline`'s `std_replay_census` (`src/standard/tests.rs`) holds each of `/std`'s elaborations to its trace. `std_recompile_reproduction` holds two edits, a leaf's and a hub's, to the bytes of a whole compile. `into_core_unit_over` keeps a declaration by its record, but nothing lowers one declaration with only its record's names in scope. [A compilation is a graph of item tasks](300-item-tasks.md) schedules from those records and waits on this census.

## What is known

- **The scope a declaration is lowered in exists only inside the walk.** `process_items` (`curios-module/src/into_core.rs`) builds a module body's scope from its heads and from its `use` declarations as it meets them, over the tables, export views and mounts `into_core_unit_within` discovers, and lowers each declaration in the scope as it stands at the declaration, in its arms over `TopItem`. No other path builds that scope, so no test can lower one declaration in it today.
- **Four reads of `Context` record nothing.** `unresolved_bare` and `unresolved_head` read the scope's labels for the nearest-name suggestion of a refusal. `guarded` asks `Reach::refused` of a resolved module. `method_of` reads the table of the module declaring a concept, through `concept_method`. Each may be covered by a `Consulted` its resolution recorded first. That is what the census answers, along with any read the lowerer makes outside `Context`.

## Decided

- **The tables are filtered, and the lowering's own doors read them.** A declaration is lowered by the lowering's own `Context`, over the archive's tables, export views, body scope and mounts filtered to exactly what its record's `Consulted` entries name, and compared with what the archive holds. This runs behind `curios-module`'s `test_support`.
  - Rejected: answering the two doors from the record's trace with the tables whole, which leaves unchecked every read that does not pass the doors.
  - Rejected: a unit's source built from the record, which states the lowering's input a second time.
- **The walk stays the one statement of a body's scope.** The census composes the walk's pieces rather than restating them, so production carries nothing only the census reads.

## Stages

1. **A declaration's lowering is a function of its own.** `process_items`' arms over `TopItem`, from a `let` to a `satisfy`, move into one function that the walk calls with the context and the lowering state. Check: `/std`'s image digest is unchanged, and `curios-module`'s suites pass. One commit.
2. **The walk's setup and body scope compose.** The setup `into_core_unit_within` performs before the walk is factored into a function, and so is building a body's scope from its heads and resolving one of its `use` declarations, so that a second walk over the same unit composes them. Check: as stage 1. One commit.
3. **A scope its record filters.** Behind `curios-module`'s `test_support`, one declaration of a unit is lowered in a `Context` whose tables, export views, body scope and mounts hold what its record's `Consulted` names and nothing else, and is handed back with the record its lowering made. Check: a fixture declaration lowers there to what the whole scope lowers it to, and with one of its `Consulted` entries taken away, it lowers otherwise or is refused. One commit.
4. **The census.** An ignored measurement beside `std_replay_census` lowers each of `/std`'s declarations in its record's filtered scope and compares the lowered items and the record with what the archive holds. A name minted anew for a kept witness is read as the item's own. Its reading is counted and stated with the commit it was taken at. Each declaration it names is filed as a finding of its own. One commit.

## Verification

`cargo xtask workspace lint` and `curios-module`'s suites at each stage; `curios-pipeline`'s suites at stages 2 and 4; the census run at stage 4.

## Retirement

Once the census lands and every declaration it names is filed, [A compilation is a graph of item tasks](300-item-tasks.md) cites the census in place of this specification, and this file is deleted.
