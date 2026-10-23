# An item materializes only what it reads of a row

[Every stage but reduction costs the graph it is handed](../../design/compilation/every-stage-but-reduction-costs-the-graph-it-is-handed.md) has a declaration read a fact at a time and a compilation end in time proportional to what was written plus what the budget counted, and [an item imports what it reads of another's row](../../../curios-ersd/README.md#an-item-imports-what-it-reads-of-anothers-row) so that no row is copied once for each declaration that reads it. Three paths pay a row's width for each reader: evaluation linking, type alias reads in both checkers, and anonymous schema materialization in erasure. The alias cost is counted below; the schema allocation is visible in the code, and its contribution to compilation cost remains to be measured.

**Not refined yet.** Evaluation linking waits on the choice between the two designs under *Refinement*, which differ in what an evaluation's trace says it read; neither is drawn. Type alias reads wait on a design for reading the actual sort and selected field without materializing unrelated fields, preserving declaration budgets and evaluation on demand. The next step for anonymous schemas is the bounded counted investigation below. These are independent workstreams: each has its own decision and completion check and can progress without either of the others.

## What is missing

**Each evaluation lists every row it reaches whole.** A top-level value is computed where its unit is compiled (`evaluate`, `curios-elab/src/into_ersd/evaluated.rs`): its fragment is linked last into a module of what it reaches, and `Linking::add_reached` (`curios-elab/src/into_ersd/link.rs`) adds the declaring item's rows for each row reached, every constructor of a family among them. So `w` values each built from one constructor of a family of `w` link `w` constructors each.

Probed at `25125304c` with the release compiler, timed once: a family of `w` constructors and `w` values `pub let v{i}: Big = Big/c{i}();`, compiled to the erased rung, takes 1.39, 4.28 and 12.84 s at 2 000, 4 000 and 8 000; five of six samples under gdb at 8 000 stand in `evaluate` and `Linking::add_reached`.

**Each reader materializes a tuple alias before reading its sort or one field.** For `pub let T: Type = {Nat, …}` of width `w` and `k` functions `pub let f{i}(x: T) -> Nat = x.0;`, elaboration forces `T` to classify its parameter (`Sort::of_under`, `curios-elab/src/convert/sort.rs`) and to project the field (`elaborate_proj`, `elaborate/aggregate.rs`), and the kernel classifies it again (`Sort::of`, `curios-cert/src/kernel/sort.rs`). The closed machine prepares a memo key before returning even a tuple type already in normal form (`Machine::eval`, `ClosedMemo::key`, `curios-core/src/machine.rs`); `Sharing` rebuilds the tuple telescope to canonicalize a key, and `Term::unwrap_or_clone` clones that telescope when its node is shared. Sort classification walks the fields as well. These paths add work proportional to `w × k`, before erasure and even where the parameter is never projected.

**Uncertain cost: each projecting fragment materializes an anonymous schema whole.** `tuple_schema` (`curios-elab/src/into_ersd/aggregate.rs`) creates a shared `ProductSchema` with `vec![Field::opaque(None); width]`, remembered by width in that fragment's environment. `erase_proj` calls it for a multi-field tuple, so separate projecting items each create the width-sized row. Classification (`shape_walk`, `curios-elab/src/into_ersd/classify.rs`) returns `Opaque` for such a tuple and registers no row; projection is the current path to count. A fragment needs the width and selected positions, while the shared anonymous schema states no field shape.

The schema probe is the same tuple alias with `w` functions `pub let f{i}(x: T) -> Nat = x.{i};`. Whole-compilation readings include the checking cost above and establish no attribution to schema creation. The independent question is how many fields and allocations anonymous schema materialization contributes, and whether that work is material beside the checking cost.

## What is known

- **The row's decision excepts a program.** [An item imports what it reads of another's row](../../../curios-ersd/README.md#an-item-imports-what-it-reads-of-anothers-row): a fragment imports a family at its count and each constructor it names at its position, only the declaring item lists a row whole, "as does every program, which has no import". An evaluation's module is linked as a program is, once for each item evaluated, which is where its rationale's square returns: a row copied per declaration costs the square of the family.
- **What an evaluation read is what it was linked after.** Its trace names every item linked into the module it ran in (`Question::Ran`), and the item is evaluated again where one of them has another form; the module holds those items and no other.
- **Linking resolves a row before it maps a constructor.** It asserts an imported row resolved and maps a constructor by its position, so a module that holds a row in part is one linking does not build today.

### Type alias reading

Counted at `82bfb1fb1`, Rust `1.99.1`, release build with all features. In the unmodified compiler's trace, the `declaration` and `certify_declaration` spans of one projecting function allocate the following decimal MB. These are cumulative allocation volumes including the observer, not peak memory or timings. Parsing, prelude setup, the alias's own declaration and erasure are outside the selected spans.

| Tuple width | Reader elaboration, MB | Reader certification, MB |
| ---: | ---: | ---: |
| 1,000 | 6.022 | 1.947 |
| 2,000 | 11.929 | 3.858 |
| 4,000 | 23.743 | 8.194 |
| 8,000 | 47.377 | 16.349 |

At width 8,000, an unused parameter (`f0(x: T) -> Nat = 0`) still allocates 34.7 MB in elaboration and 15.5 MB in certification. The first-field and last-field projections have identical allocation totals in the probe. Sixteen separate first-field readers allocate 1.016 GB cumulatively across the two checkers, counting those declarations alone. Repeated projections within one function reuse much of the work; separate declarations repeat it.

Attribution in a scratch copy of the same commit counts the following for each first-field reader at every measured width, including each reader in the four- and sixteen-reader controls. Rebuilds and clones count whole tuple telescopes; field visits are the width multiplied by the number of sort walks. The operations may nest and their allocation totals are not added together.

| Operation | Elaborator | Kernel |
| --- | ---: | ---: |
| Tuple rebuilds during key canonicalization | 9 | 2 |
| Tuple clones in `Term::unwrap_or_clone` | 7 | 3 |
| Fields visited for sort classification | `2 × w` | `w` |

The selected-field helper opens one field type per checker. Returning tuple and function types already in normal form before the closed machine probes its memo removes six of the elaborator's tuple rebuilds and both of the kernel's, but leaves the sort walks and other width-dependent work. This partial change does not establish the bound the workstream needs.

**Declaration budgets remain independent.** [A reduction step costs what it builds](../../design/soundness/a-reduction-step-costs-what-it-builds.md) clears both checkers' answers at a declaration boundary, with the budget. Retaining judgment answers across declarations changes that contract. The design here concerns immutable facts a declaration publishes and a narrow reading of its body, while preserving those answer lifetimes.

**The reduced value determines the exact universe.** A type's spelling can inhabit a higher universe than its reduced value: `Pick(true)` can be `Nat` while `Pick`'s codomain includes the larger type returned by its other arm. [The existing regression](../../../curios/src/tests/universes/settlement_tests.rs) `a_type_named_through_a_higher_universe_solves_at_its_own` holds this distinction. Reading the actual sort must preserve it. The selected field must also retain its dependent substitutions, proof relevance and universe instance; a linked telescope's lookup position remains a cost even after unrelated materialization is removed.

### Reproducing the alias reading

Build the launcher once, then the release compiler with all features. Generate `use /std/{Nat};`, the alias of `w` `Nat` fields, `k` public functions projecting `x.0`, and the final `()`; pass it on standard input to `cargo run --release --all-features --package curios -- --profile <capture> wonder diagnostics -`. Take widths 1,000, 2,000, 4,000 and 8,000, with one and sixteen readers at each. Require no diagnostics for these projecting programs. Fold each stream with `curios_profile::fold`, require all spans to close, and select `declaration` and `certify_declaration` by the reader's `group` (`/f0`, and so on); their `allocated` byte differences divided by 1,000,000 give the table. Exclude `/T` and all enclosing or later-stage spans. These counts require no quiet-machine timing campaign.

For attribution, add temporary `profile!` spans around `Sharing::share`, `ClosedMemo::key`, `Term::unwrap_or_clone`'s shared-tuple clone, both tuple-sort walks and `Telescope::field_type_from` in a scratch copy of the same checkout. Sample the width where `Visit::adopt` has rebuilt a `TupleType`, at the shared-tuple clone and at each tuple-sort walk; sample the selected field's slot and telescope width separately. Trace `typecheck_with_prelude`, then `recheck_with_prelude` over the zonked program, with no erasure, and attribute samples to the enclosing reader's declaration span. Use all four widths with one reader and widths 1,000 and 8,000 with four and sixteen readers. Do not substitute allocations from this more heavily instrumented probe for the unmodified-compiler table.

Controls replace the projection with an unused parameter or identity, select the last field, repeat the first projection two, four and eight times within one function, and read through a second alias. Parameterized and heterogeneous aliases are checked at widths 1,000 and 8,000; dependent and mixed proof/data tuples are checked at width 64. The higher-universe regression is checked and executed. For the unused-parameter control, its unused-binder lint is expected and no refusal is accepted. The normal-form return experiment keeps the machine's transition charge and changes only the path before its memo probe; both checkers must still accept every control it is put to. All figures are counted, and none establishes a default-feature runtime speedup.

## Refinement

### Evaluation linking

- **The design, one of two.** An evaluation's module imports each row as a fragment does, at its count and the constructors the evaluation reaches, with the evaluator running what it was handed and linking resolving a row it holds in part; or a unit's evaluations share one module extended an item at a time, each row linked once for the unit, with what each evaluation's trace names then stated from what it reached and not from what the module holds.
- **Check**: the family-and-values probe within a tenth of 2.0 per doubling, counted; `/std`'s erased forms the bytes they were; the incremental tests that hold an evaluation's trace (`curios-pipeline`'s `tests::incremental_tests`).

### Type alias reads

- **The design.** State how a checked alias exposes its actual sort and a selected field without rebuilding, canonicalizing or classifying unrelated fields for each reader. Identify the owner and lifetime of any published facts or index, how parameters and universe instances are applied to the requested fact, and how the reader records what it depends on. The body's annotation is no substitute for its actual sort. Preserve reduction on demand: an unused alias is not normalized eagerly, computed aliases still spend the existing reduction budget, and no judgment answer outlives its declaration. Settle the representation and lookup bound, including access to the last field and the dependencies the selected field genuinely reads, before implementation.
- **Independent improvements.** A measured local reduction in copying may land without the full design, with unchanged resource and semantic contracts, but leaves this workstream open while a narrow reader still pays the whole width. Neither completing this workstream nor changing erasure's reducer establishes that anonymous schemas are cheaper.
- **The cost check.** Count alias construction separately from reader work over the reproduction grid, with width and reader count varied independently. For the stored tuple body in the probe, bookkeeping costs its declared width plus the facts the readers use, rather than `w × k`; remove the attributed whole-row rebuilds and sort walks from narrow reads. Retain first- and last-field, unused-parameter, identity and repeated-read controls. Any irreducible traversal or charged computation has its bound stated separately, rather than hidden behind a broad regression threshold.
- **The semantic check.** Both checkers agree with the baseline on dependent fields, proof-containing tuples, parameterized aliases, universe instances and the higher-universe regression. Keep declarations independent of order and budget warmth, and keep the memo census and incremental dependency checks. Preserve diagnostics, erased results and executable artifacts. If the stored layout stays the same, require byte-identical prelude images at one checkout path; if published facts or an index change it, state the expected stored-byte changes and format invalidation before implementation and compare the semantic and executable results independently of those bytes.

### Anonymous rows

- **The investigation.** In a scratch copy add a `sample!` of `width` only where `tuple_schema` creates a schema, and a `profile!` span around that creation. Compile the probe to the erased rung at widths 1 000, 2 000, 4 000 and 8 000, first with one projecting function and then with `w`, so width and reader count vary separately. Read created schemas, materialized fields and allocated bytes under that span, apart from alias reduction. Counted; the whole-compile times from the old probe are not substituted for this attribution.
- **The decision.** If materialization is a cost worth removing, state an anonymous schema by its width and materialize fields only where a consumer needs them, preserving independent fragment verification. The representation and its readers are designed from the counted result; no field is inferred to be erased or to hold unit from its anonymous width. If the current path establishes no material cost, close this workstream with the result and its reason rather than requiring a representation change.
- **Check.** Over the same grid, row bookkeeping costs the declared widths plus the facts the readers use, rather than the product of width and reader count; both the single-reader and many-reader controls hold. `/std`'s erased results, tuple projection and construction, shared-schema resolution and executable behavior agree with the baseline. The evaluation-linking tests above remain independent of this check.

## Verification and retirement

Each workstream retains its own decision, attribution and completion check. A landed change states its row contract in the owning crate and leaves only remaining work here. Retire this specification once evaluation linking is complete, type alias reads meet their cost and semantic checks, the anonymous-row investigation is resolved and any resulting materialization fix has landed, and no dependent reference names this file.
