# An item materializes only what it reads of a row

[Every stage but reduction costs the graph it is handed](../../design/compilation/every-stage-but-reduction-costs-the-graph-it-is-handed.md) has a compilation end in time proportional to what was written plus what the budget counted, and [an item imports what it reads of another's row](../../../curios-ersd/README.md#an-item-imports-what-it-reads-of-anothers-row) so that no row is copied once for each declaration that reads it. A top-level value computed where its unit is compiled links each row it reaches whole, so values each built from one constructor of a family cost the square of the family. Separately, a fragment projecting an anonymous tuple materializes its schema at its full width; the allocation is visible in the code, and its contribution to compilation cost remains to be measured.

**Not refined yet.** Evaluation linking waits on the choice between the two designs under *Refinement*, which differ in what an evaluation's trace says it read; neither is drawn. The next step for anonymous rows is the bounded counted investigation below. These are independent workstreams: neither waits on the other's measurement or implementation.

## What is missing

**Each evaluation lists every row it reaches whole.** A top-level value is computed where its unit is compiled (`evaluate`, `curios-elab/src/into_ersd/evaluated.rs`): its fragment is linked last into a module of what it reaches, and `Linking::add_reached` (`curios-elab/src/into_ersd/link.rs`) adds the declaring item's rows for each row reached, every constructor of a family among them. So `w` values each built from one constructor of a family of `w` link `w` constructors each.

Probed at `25125304c` with the release compiler, timed once: a family of `w` constructors and `w` values `pub let v{i}: Big = Big/c{i}();`, compiled to the erased rung, takes 1.39, 4.28 and 12.84 s at 2 000, 4 000 and 8 000; five of six samples under gdb at 8 000 stand in `evaluate` and `Linking::add_reached`.

**Uncertain cost: each projecting fragment materializes an anonymous schema whole.** `tuple_schema` (`curios-elab/src/into_ersd/aggregate.rs`) creates a shared `ProductSchema` with `vec![Field::opaque(None); width]`, remembered by width in that fragment's environment. `erase_proj` calls it for a multi-field tuple, so separate projecting items each create the width-sized row. Classification (`shape_walk`, `curios-elab/src/into_ersd/classify.rs`) returns `Opaque` for such a tuple and registers no row; projection is the current path to count. A fragment needs the width and selected positions, while the shared anonymous schema states no field shape.

The probe is `pub let T: Type = {Nat, …}` of width `w` and `w` functions `pub let f{i}(x: T) -> Nat = x.{i};`. Its aggregate timings at `25125304c` include repeated alias reads as well as erasure and establish no current attribution to schema creation. The alias investigation remains in [compilation's findings](000-findings.md); the independent question here is how many fields and allocations anonymous-row materialization contributes in the current tree, and whether that work is material beside those reads.

## What is known

- **The row's decision excepts a program.** [An item imports what it reads of another's row](../../../curios-ersd/README.md#an-item-imports-what-it-reads-of-anothers-row): a fragment imports a family at its count and each constructor it names at its position, only the declaring item lists a row whole, "as does every program, which has no import". An evaluation's module is linked as a program is, once for each item evaluated, which is where its rationale's square returns: a row copied per declaration costs the square of the family.
- **What an evaluation read is what it was linked after.** Its trace names every item linked into the module it ran in (`Question::Ran`), and the item is evaluated again where one of them has another form; the module holds those items and no other.
- **Linking resolves a row before it maps a constructor.** It asserts an imported row resolved and maps a constructor by its position, so a module that holds a row in part is one linking does not build today.

## Refinement

### Evaluation linking

- **The design, one of two.** An evaluation's module imports each row as a fragment does, at its count and the constructors the evaluation reaches, with the evaluator running what it was handed and linking resolving a row it holds in part; or a unit's evaluations share one module extended an item at a time, each row linked once for the unit, with what each evaluation's trace names then stated from what it reached and not from what the module holds.
- **Check**: the family-and-values probe within a tenth of 2.0 per doubling, counted; `/std`'s erased forms the bytes they were; the incremental tests that hold an evaluation's trace (`curios-pipeline`'s `tests::incremental_tests`).

### Anonymous rows

- **The investigation.** In a scratch copy add a `sample!` of `width` only where `tuple_schema` creates a schema, and a `profile!` span around that creation. Compile the probe to the erased rung at widths 1 000, 2 000, 4 000 and 8 000, first with one projecting function and then with `w`, so width and reader count vary separately. Read created schemas, materialized fields and allocated bytes under that span, apart from alias reduction. Counted; the whole-compile times from the old probe are not substituted for this attribution.
- **The decision.** If materialization is a cost worth removing, state an anonymous schema by its width and materialize fields only where a consumer needs them, preserving independent fragment verification. The representation and its readers are designed from the counted result; no field is inferred to be erased or to hold unit from its anonymous width. If the current path establishes no material cost, close this workstream with the result and its reason rather than requiring a representation change.
- **Check.** Over the same grid, row bookkeeping costs the declared widths plus the facts the readers use, rather than the product of width and reader count; both the single-reader and many-reader controls hold. `/std`'s erased results, tuple projection and construction, shared-schema resolution and executable behavior agree with the baseline. The evaluation-linking tests above remain independent of this check.

## Verification and retirement

Each workstream retains its own decision, attribution and completion check. A landed change states its row contract in the owning crate and leaves only remaining work here. Retire this specification once evaluation linking is complete, the anonymous-row investigation is resolved and any resulting materialization fix has landed, and no dependent reference names this file.
