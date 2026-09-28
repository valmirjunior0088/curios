# Verdicts, part 3: artifacts carry no minted identity

Working specification for taking position out of what a compilation stores and names: identities minted in a space private to the item that mints them, names interned once per process, and sources owned by a map rather than shared by pointer. Today a unit's binder, metavariable and universe floors resume above the previous unit's, so an artifact's identities depend on everything compiled before it, and the fold must be a line to keep them apart.

It needs nothing, and each of its stages makes the serial compiler better on its own. The universe-seed table it deletes is also an input [part 7](07-checked-evidence-spec.md) forbids the certifier to read, and [the invariants campaign's part 2](../invariants/02-unrecorded-universes-spec.md) asks whether the elaborator records levels at all; that table's deletion is taken with part 2's answer in hand.

## What this builds on

- **`validate_stored_identities`** (`curios-core/src/module.rs`): a stored unit carries no metavariable and no free local. A stored binder identity is read only by the printer — both checkers mint fresh variables when they open a scope — so the identities a unit carries are not what either checker's freshness rests on.
- **The stored-unit format** (`curios-unit`): a serialized unit already crosses processes.

## The gap

**Positions are carried along the fold.** The binder, metavariable and universe floors resume above the previous unit's; the universe-seed table is cumulative from index zero; and a stored unit is addressed by the ordered list of its predecessors because of the last two.

**Names and sources are shared by pointer.** `Qualifier` is an `Rc<Vec<String>>` interned per thread; `Span` holds an `Rc<Source>`. The interner and the parsed-file memo are `thread_local!` tables (`curios-utilities/src/qualifier.rs`, `curios-text/src/root_source.rs`).

## The decision

**Artifacts carry no minted identity.** A stored binder label is a display hint; binder, metavariable and universe identities are minted in a space private to the item task that mints them. Witness ordinals stay per module, as they are.

## The components

| Component | Treatment |
| --- | --- |
| Names and qualifiers | Interned once per process into copyable identities; the per-thread interner is deleted |
| Sources and spans | A source map owns every text; a span is a source identity and a range, copied rather than shared. Two separately loaded sources stay distinct, as `Rc` pointer identity keeps them today |
| Binder labels | Hints; the printer derives its labels from hints and depth |
| Floors and the seed table | Deleted with the positions they protected: `Module::binder_floor`, `Unit::binder_floor`, `derived_binder_floor`, and the metavariable and universe floors with the cumulative seed table |

## Stages

Each lands alone and passes the gate.

1. **Interned names and a source map.** Needs nothing.
2. **Artifacts carry no minted identity.** Needs nothing. Binder labels become hints, identities are minted per item, and the floors and the seed table are deleted. `validate_stored_identities` keeps refusing what it refuses today.

## Verification

- Every verdict over `/std` and the corpus in `programs/` is compared with the compiler before each stage; a difference is a finding, never a fixture update.
- A unit's stored bytes do not change with what was compiled before it.
- Each stage reports the time it moved.

## Design decisions this overturns or corrects

- [`curios-prelude-archive`'s README](../../../curios-prelude-archive/README.md): `/std`'s seeds and floors no longer resume above `/sys`'s.
- `curios-text/src/root_source.rs` and `curios-utilities/src/qualifier.rs`: the comments justifying a per-thread table by `Rc`.

## Rejected

- **Hash-consing as a prerequisite.** It changes what a term's identity means and needs a global interner with a reclamation story for the language server; it is a decision of its own, taken on its own merits.

## Completion and retirement

No artifact carries a minted identity, no floor survives, and names and sources are process-wide identities. Record the identity contract in `curios-unit`'s and `curios-core`'s documentation, replace the roadmap entry with a checked summary, verify that nothing references this filename, and delete it.
