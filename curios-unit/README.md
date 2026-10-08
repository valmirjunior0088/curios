# curios-unit

The compilation unit: what one unit hands its successors — one opaque artifact per stage — and the borrowed `Predecessors` each stage is compiled against. A compilation is a set of units folded over a dependency order; the intrinsic root is a unit, the standard library above it is one, a package is one, and the program asked for is the unit with no successors, which is what lets it carry the entry point. What `Unit` and `Predecessors` expose belongs to the crate rustdoc.

## Design

### Below the kernel, so a build script constructing a unit never reaches the certifier

**Decision.** This crate depends on every stage that does not judge — `curios-text`, `curios-elab`, `curios-ersd` — and deliberately not on `curios-cert`; judgment is interleaved by the driver above it. `cargo tree -p curios-unit --edges normal` must not contain `curios-cert`.

**Rationale.** `curios-prelude-archive`'s build script constructs an `Uncertified` unit, and a build script reaching the kernel re-runs on every certifier edit and re-elaborates the whole standard library, the regression `curios-analysis` stands apart to prevent.

### Predecessors are borrowed, per stage, as that stage's own type

**Decision.** `Predecessors` borrows the predecessor units and hands each stage its view as the opaque type that stage owns — one `curios-text` resolution state per unit (`Predecessors::text`), one `curios-core` module per unit (`Predecessors::cores`), and one `curios-elab` erased unit per unit (`Predecessors::erased`) — rather than one merged value or anything this crate unpacks. The kernel's environment is assembled above this crate, which stays below the kernel, from the units themselves (`Predecessors::units`), each read where it stands by the places it keeps. A unit is composed of those opaque artifacts rather than flattened into their fields.

**Rationale.** Merging would copy the standard library into every compilation. Widening the stages' internals so a struct here could hold them would export a resolver's internals for no consumer.

### The stored-unit format lives below the store

**Decision.** What a stored unit is — the `Record` of what it was compiled from, the framing that puts it ahead of the archived unit in one file, the reading of the two apart, and the two halves a unit's artifact is — is this crate's; what verifies a record and where a slot is addressed stay in `curios-verdicts` and `curios-package`.

**Rationale.** Two producers write the format and neither may depend on the other: the store files a unit from above the pipeline, and the prelude's build script images `/sys` and `/std` from below every store. One statement below both lets the prelude image carry the record a slot does, framing the unit before certification where a slot frames a certified one.

**Rejected.** A crate of its own for one struct and two functions; the archive crate depending on `curios-verdicts`, which pulls the package layer and the pipeline under the prelude's build script.

### A unit is certified by construction

**Decision.** A `Unit` carries the certifier's record of its declarations, and `Uncertified::certified` is the only way to make one: a compilation assembles an `Uncertified` from what each stage produced and certifies it with the record the kernel's walk left. The only uncertified units kept anywhere are the prelude's images, which `curios-prelude` certifies as it restores them.

**Rationale.** A later walk reads a unit in scope for its definitions' totality and its families' polarity vectors, so a unit without a record would be a case every reader handles, for the one producer below the certifier by design. The state in the type removes the case from every consumer.

**Rejected.** An optional record, a special case every reader carries; a unit generic over its record, which two named types say without the machinery; the record outside the unit, which turns the predecessors into a slice of pairs.

### A unit is handed over, never copied

**Decision.** A unit holds its lowered state, its elaborated module, its erased forms and its certification each by pointer, and each item's erased form by pointer among them, so a clone of a unit is a clone of its handles. Where its elaborated module holds each name it defines (`curios_core::Places`) is made once, the first time the unit is read by name, and kept with it for as long as it is held.

**Rationale.** A unit is handed on more often than it is made: a fold hands each to its successors, a session keeps the last one as the next question's baseline and takes it back out, a store offers the archived one, and a unit compiled over a baseline keeps every erased form it does not redo. Copied, each of those is a walk of the whole unit for a question that may redo one declaration, and a scope keyed again for each compile is the same walk made of every unit in scope.

**Rejected.** One handle around the whole unit, which leaves a recompile copying the parts it keeps into the unit it builds. A table of a unit's names made by elaboration for each compile, a walk of every unit in scope before the first declaration is read.

### A unit carries no identity another compilation could mint

**Decision.** Nothing a unit stores is a position in a counter another compilation also counts. A binder's label is its display hint; no term holds a free local or a metavariable; a witness is named by its module, its concept and the key its signature is spelled at; and every counter a lowering and an elaboration mint from starts at zero for each declaration of the unit, the lowering's count carried beside the module for the elaborator to start above (`curios_core::Minted`). An erased form's identities are places in its own item's fragment, counted from zero, and what it hands on binds the names its item declares, never a binder its erasure opened.

**Rationale.** An identity meaningful only in the compilation that assigned it has no safe direction to degrade in: restored beside a unit whose counters hand out the same index, it aliases silently, which admits rather than fails. Refusing one where a unit is stored (`validate_stored_identities`) and where a module is judged (the kernel's free-local refusal) lets every counter start at zero, so a unit's bytes depend on its own sources and its scope's interfaces, never on how much its predecessors minted; and counting from zero for each declaration, never for the unit, keeps a declaration's bytes from following the declarations written or elaborated before it ([A declaration is a function of what it reads](../documentation/design/compilation/a-declaration-is-a-function-of-what-it-reads.md)).

**Rejected.** Floors, each unit's counters resuming above every predecessor's: a floor widens safely, but it ties a unit's bytes to its place in the fold and asks every walk to trust a carried number nothing checks.

### A unit is a function of what it was compiled from

**Decision.** Two compilations of one text against one scope, by one compiler, store the same bytes. Whatever a stage walks into a term, a table or a list a unit stores is walked in an order the program states — registration, declaration or label — never a hash's.

**Rationale.** A successor's record vouches for the bytes each predecessor contained, so a unit that differs from one compilation to the next is a miss for every unit after it; two commands filing one slot write one thing only where this holds; and it is what a compilation over several workers is held to. A hash's order reaches further than the table it orders: the elaborator's frames hold one guard under every spelling it is met by, the bound prover read them in a hash's order and minted a different number of binders doing it, and every later proof moved with the count, since a term's structural hash ranks the atoms a bound is searched over and takes each local's index. `curios-elab`'s `a_frames_refinements_are_read_in_the_order_they_were_registered` and `curios-pipeline`'s `a_name_exported_along_several_paths_is_stored_the_same_every_time` hold the two orders that reached the standard library's unit, and `std_unit_reproduction` measures the library whole.

**Rejected.** Ordering a map where it is archived and nowhere else, which `curios-text`'s `OrderedMap` does: it orders the map's own entries, and not a list built by walking the map. Comparing units up to what moved, which leaves a record nothing can compare by its digest.

### A stored unit says what its items mean, not where they were written or how they were come by

**Decision.** A unit is serialized for a slot by one function, `Unit::stored`, which hash-conses its elaborated module in a table of its own first: one node for each structure as it is spelled, built over its canonical children, under no position (`curios_core::Sharing`), and whose universe constraints say nowhere where they were raised. A binary literal's buffer is written by value wherever it is held, shared with no other (`curios_num::BufferBytes`). The prelude's images are consed by the same pass where they are built. The lowered module is stored as it was built. The elaborated module's items are in the order it was made in, every item after the items it reaches and otherwise by name (`curios_core::Module::ordered`), which says nothing of where a declaration sits in its sources. In memory a unit keeps what elaboration gave it.

**Rationale.** A unit compiled over a baseline reuses items elaborated from an earlier text, so a stored item that kept its positions would hold the stamps of a text the unit was not lowered from, where a whole compile's are all the present text's; and an item reused by pointer shares nothing with the items elaborated around it, where a whole compile's share by construction. Consed and without positions, an item elaborated now and the same item reused are the same nodes, which is what lets a recompiled unit be held to the bytes of a whole one. A literal's buffer is the same case outside the terms: lowered, elaborated and erased, it is one buffer in three parts of a unit, and a reused item's is the baseline's beside the new text's, so a buffer written once for whoever shares it would say which. It is also what a restored unit costs: every compilation restores the prelude whole, and a node written once is read once. The pass runs where a unit is serialized because it builds a node for every node of the module, and a unit that is neither filed nor followed, a keystroke's, is never serialized. A binder's name is kept because it is part of the text: a report spells a signature by it, and the diff an item-level recompile makes reads it.

**Rejected.** Consing up to α, which hands a structure the node of the first one equal to it up to its binders' names, and a signature the parameter names of another declaration. Handing a rebuilt node back when its payload is unchanged, as every other traversal does: under a consing visit the payload is always unchanged, so the first node met of a structure would keep every duplicate beneath it. A table shared with the lowered module, which keeps its positions and so shares no node with a module that keeps none. Consing where a unit is assembled, which every keystroke would pay for a unit it never stores. A field adapter on the unit's module in place of a function, which runs the pass once to serialize and once to resolve. A universe constraint that keeps its position in memory and loses it in a slot, or takes the position of the use that instantiated its scheme: an inconsistency would be located by how its unit was compiled, and the solver is reached where no written occurrence is in hand, for a position the term being elaborated already gives an error on its way out. Handing a reused item's literals the new text's buffers, a walk over the text's part and the arena that nothing else makes where a unit is stored.

### A stored unit has two halves, by who reads them

**Decision.** A unit's artifact is two archives in one segment (`halved`, `halves`): the unit as what is compiled after it restores it, and its baseline half, which only a later compile of the same unit reads. The baseline half is two parts framed the same way (`Uncertified::baseline_bytes`), one for each stage that keeps a record: the text's — what each declaration lowered to beside its items, with the trace of its lookups, and what each of its files declares, with the text it was read from (`curios_text::PreparedText::baseline_bytes`) — and the elaborator's, what each declaration asked of the declarations around it as it elaborated, with each name written once (`curios_elab::Traces`). Opening a unit decodes the first half and keeps each part of the second as the bytes it is, borrowed where they are a constant of the executable and copied where they were read from a slot, and a compile over the unit reads a part when it first asks for it (`curios_archive::Deferred`). `Unit::stored` and `Uncertified::stored` write both halves, `Unit::restored` reads them, and a record digests the artifact whole, so the two are believed together and a slot stays one file.

**Rationale.** A program compiled against a unit never compiles that unit again, so what only its recompile reads is restored for no other reader, and every process restores the prelude. rustc keeps what recompiling a crate reads, its dependency graph and its query results, in a directory of its own, apart from the metadata a dependent loads ([`rustc_incremental`](https://doc.rust-lang.org/stable/nightly-rustc/src/rustc_incremental/persist/fs.rs.html)). One segment and no second file, so a slot is still one rename and one digest.

**Rejected.** The records restored with the unit, which every process pays for a reader most never are. A second file beside the slot: two renames, two digests, and a slot that can hold one without the other. The half decoded where a store hands a unit out as a baseline: a session keeps the unit a hit gave it and takes it as the baseline of the next edit, so no opening knows. The half read in place and never decoded, which is what storing by facet decides.

### An erased form is its item's

**Decision.** A unit stores what each of its items erased to as a fragment of its own (`curios_elab::ErasedItem`): a module whose identities count from zero, in which what another item declares is an import by name, be it a value, a function, or the row of a structure or an inductive, which the item declaring it registers. A program is linked from the fragments its entry reaches and the ones that compute as the program is initialized, under identities the link mints (`curios_ersd::Module::link`), and a unit stores no arena.

**Rationale.** An arena each unit's erasure resumes over makes a unit's erased bytes a function of every unit placed before it and of every item erased before each of its own, so erasure is a pass over the unit however little of it an edit reaches, and a program clones and prunes its whole scope to keep what it reaches. A fragment is a function of its item's Core, of how each name it reads is bound — dropped, a constant, a value, or a function of an arity and an interface — and of the rows it imports, by their widths and the fields they erase, so it stands wherever those do, and a program pays to copy what it links.

**Rejected.** The cumulative arena with a relocation per unit, the pass rustc pays a `cnum_map` for: it frees a unit's bytes from its predecessors' sizes and leaves erasure at the grain of a unit. A row derived by every fragment that uses it and made one by name where a program links them: what a field is would be decided once for each use, and a program would hold whichever derivation it happened to link first, where two declarations that name each other are classified by the one the derivation starts from.
