# curios-pipeline

The Curios compile driver: `compile_entrypoint`, `compile_units`, `Stage`, and the fold that strings the pipeline together from a parsed `curios_surface::Entrypoint` to a `curios_wasm::Module` — plus, in `standard.rs`, the same fold with the fixed prelude supplied. It is the compiler boundary: everything a compilation needs and nothing a product decides. How the stages work belongs to each stage's crate; where judgment sits in the sequence and what `Stage` observes belong to the crate rustdoc.

## Design

### The driver is the compiler boundary, and its predecessors are not its decision

**Decision.** This crate depends on no runtime, no Binaryen, no CLI and no `curios-package`, and folds its stages over whatever predecessors it is handed. `curios-package` sits beside the boundary, and `curios-js` does not touch it.

**Rationale.** Manifests, dependency resolution and the store answer what is in a compilation and where each part came from, which is a product's question: `curios` answers it over `curios-package`, and the browser, with no filesystem, answers it differently. A driver that knew manifests would need a plausible answer from every caller that has none. The boundary is the manifest's — no `curios-package` row here — and [`curios-package`'s README](../curios-package/README.md#one-subsystem-one-owner) states it from the other side.

**Rejected.** The driver resolving its own inputs, which puts a filesystem assumption below the browser product.

### The standard predecessors are a function here, not a policy

**Decision.** `compile_entrypoint` takes a `Predecessors` and cannot tell which unit is `/std`. `standard.rs` sits above it and supplies the fixed prelude, `/sys` and `/std` first in scope, and nothing in the half that knows no particular predecessors calls it. A package named `std` that is a fold's first unit takes the archived `/std`'s place, compiled over the archived unit as its baseline and granted what that root could see, and every later unit is compiled against it; only the first unit and the last root can qualify, and both are asserted ([A stored unit is a baseline for an item-level recompile](../documentation/design/compilation/a-stored-unit-is-a-baseline-for-an-item-level-recompile.md)).

**Rationale.** The native product, the browser product and this crate's fixtures would each spell the prelude by hand, and three callers agreeing on one spelling is a missing function. The standard library is the largest Curios program and the one edited most, and a question about one of its modules would otherwise be a collision of two claims on `/std`. Reserving the name makes the answer independent of which binary answers, and a baseline is correct for any tree, since a declaration is kept only where its lowering is the baseline's and everything its elaboration asked is answered as it was.

**Rejected.** Superseding by prefix for any unit of the fold, when a later unit's predecessors are not the ones the archived unit was compiled against; identifying the tree by the record's directory, which depends on the binary's checkout; a manifest key or root kind for the standard library; recognizing the prelude in `curios-wonder` or the package layer, neither of which can reach the archive.

### A baseline crosses the cache seam as a unit, and the cache decides

**Decision.** `Cache::baseline` hands the fold a unit compiled from an earlier text of the same sources, or nothing; the fold compiles over it with `compile_unit_over` and hands the result to `put` as any unit. Whether a store offers a baseline and files what was compiled over one is the cache implementation's decision: the store's own cache offers the nearest earlier unit and files everything, and the `wonder` engine's offers the same and files only what it compiled from the text on disk ([`curios-wonder`'s README](../curios-wonder/README.md#a-question-files-the-units-it-compiled-from-disk)).

**Rationale.** A baseline crosses the seam as the one thing the fold understands, a `Unit`, never a path or a record, and which commands take one is written in one place: a build takes one because the store's cache answers, and nothing of the fold says so.

**Rejected.** A `baselines` parameter on `compile_units`, a second seam for the same fact; deciding by entry point, when `wonder --stage` and `curios test` reach the fold through the same ones.

### The puts are this crate's, behind `test-support`

**Decision.** What hands a program or a module to the checkers as an instrument, rather than building it, sits in `test_support` behind `test-support`. `examine_with_prelude` puts a program whole to both checkers at the tail it is handed — the program contract, as a suite guarding a rule puts one, or `False`, as the soundness board puts a witness — and hands back the elaborator's error, its reported obligations and the certifier's verdicts; `typecheck_with_prelude` and `recheck_with_prelude` are its two halves. `prelude_form_scope` is the scope Core text is read over in front of the prelude. The instruments beneath, `examine_entrypoint` and `typecheck_entrypoint`, take the predecessors they are handed. What the product asks of the checkers is no instrument and ships: Core text read and compiled ([below](#core-text-is-read-here-outside-the-trusted-base)).

**Rationale.** This is the lowest crate that puts a program over the prelude, so the board's puts and the rules' tests in `curios` are one function, and a ticket and a rule test cannot disagree on what refused a program and where. Behind a feature, no build that ships holds an instrument.

**Rejected.** A put per consumer, which states twice how a refusal is read. A put in the board's crate, which a test of `curios` cannot reach.

### Core text is read here, outside the trusted base

**Decision.** A text of Core text is read for the product by this crate (`read_core_text`): parsed with each item's span, read by `curios-surface`'s reader over the units in scope, its closing term put at the type its question puts it at, its foreign rows gathered and its declarations anchored where the text writes them. `compile_read` compiles what it reads as a unit, where a program's is lowered and elaborated, judged and erased as any unit is, over the baseline a store offers; `enter_core_text` enters a loose text on top of a fold as a program is entered, its closing term judged at the program contract over its unit and erased, which `Fold::check_core_text` is the check of. `wonder`, the soundness board and the board's histories read through this one door, and a refusal of what it reads is placed where the text writes the declaration it is about, as a lowered unit's is placed where its lowering wrote it.

**Rationale.** The trusted base is `curios-cert`'s dependency closure ([its README](../curios-cert/README.md#the-trusted-base-is-a-crate-boundary)), which holds no parser; this crate depends on the reader and on the certifier's production walks both, so it composes them without moving either. Nothing the certifier is trusted for rests on the reader: a term the certifier admits at `False` is a flaw whatever text it was read from, and what a refusal of a text says rests on the round trip that holds the reader to the writer. One door for the product, the board and its histories means what the board says of a ticket is what `wonder` says of the same text.

**Rejected.** The door in `curios-cert`, out of `test-support`, which makes the parser, printer and formatter part of the trusted base; and behind a feature of `curios-cert` other than `test-support`, which makes that base a function of the features a build turns on, the call-closure reasoning its decision rejects.
