# An item evaluated at compile time links what it reads of a row

[Every stage but reduction costs the graph it is handed](../../design/compilation/every-stage-but-reduction-costs-the-graph-it-is-handed.md) has a compilation end in time proportional to what was written plus what the budget counted, and [an item imports what it reads of another's row](../../../curios-ersd/README.md#an-item-imports-what-it-reads-of-anothers-row) so that no row is copied once for each declaration that reads it. A top-level value computed where its unit is compiled links each row it reaches whole, so values each built from one constructor of a family cost the square of the family.

**Not refined yet.** It waits on the choice between the two designs under *Refinement*, which differ in what an evaluation's trace says it read; neither is drawn.

## What is missing

**Each evaluation lists every row it reaches whole.** A top-level value is computed where its unit is compiled (`evaluate`, `curios-elab/src/into_ersd/evaluated.rs`): its fragment is linked last into a module of what it reaches, and `Linking::add_reached` (`curios-elab/src/into_ersd/link.rs`) adds the declaring item's rows for each row reached, every constructor of a family among them. So `w` values each built from one constructor of a family of `w` link `w` constructors each.

Probed at `25125304c` with the release compiler, timed once: a family of `w` constructors and `w` values `pub let v{i}: Big = Big/c{i}();`, compiled to the erased rung, takes 1.39, 4.28 and 12.84 s at 2 000, 4 000 and 8 000; five of six samples under gdb at 8 000 stand in `evaluate` and `Linking::add_reached`.

## What is known

- **The row's decision excepts a program.** [An item imports what it reads of another's row](../../../curios-ersd/README.md#an-item-imports-what-it-reads-of-anothers-row): a fragment imports a family at its count and each constructor it names at its position, only the declaring item lists a row whole, "as does every program, which has no import". An evaluation's module is linked as a program is, once for each item evaluated, which is where its rationale's square returns: a row copied per declaration costs the square of the family.
- **What an evaluation read is what it was linked after.** Its trace names every item linked into the module it ran in (`Question::Ran`), and the item is evaluated again where one of them has another form; the module holds those items and no other.
- **Linking resolves a row before it maps a constructor.** It asserts an imported row resolved and maps a constructor by its position, so a module that holds a row in part is one linking does not build today.

## Refinement

- **The design, one of two.** An evaluation's module imports each row as a fragment does, at its count and the constructors the evaluation reaches, with the evaluator running what it was handed and linking resolving a row it holds in part; or a unit's evaluations share one module extended an item at a time, each row linked once for the unit, with what each evaluation's trace names then stated from what it reached and not from what the module holds.
- **Check**: the probe above within a tenth of 2.0 per doubling, counted; `/std`'s erased forms the bytes they were; the incremental tests that hold an evaluation's trace (`curios-pipeline`'s `tests::incremental_tests`).
