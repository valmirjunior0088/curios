# curios-unit

The compilation unit: what one unit hands its successors — one opaque artifact per stage — and the borrowed `Predecessors` each stage is compiled against. A compilation is a set of units folded over a dependency order; the intrinsic root is a unit, the standard library above it is one, a package is one, and the program asked for is the unit with no successors, which is what lets it carry the entry point. What `Unit` and `Predecessors` expose belongs to the crate rustdoc.

## Design

### Below the kernel, so a build script constructing a unit never reaches the certifier

**Decision.** This crate depends on every stage that does not judge — `curios-text`, `curios-elab`, `curios-ersd` — and deliberately not on `curios-cert`; judgment is interleaved by the driver above it. `cargo tree -p curios-unit --edges normal` must not contain `curios-cert`.

**Rationale.** `curios-prelude-archive`'s build script constructs an `Uncertified` unit, and a build script reaching the kernel re-runs on every certifier edit and re-elaborates the whole standard library, the regression `curios-analysis` stands apart to prevent.

### Predecessors are borrowed, per stage, as that stage's own type

**Decision.** `Predecessors` borrows the predecessor units and hands each stage its view as the opaque type that stage owns — one `curios-text` resolution state per unit (`Predecessors::text`), one `curios-core` module per unit (`Predecessors::cores`), and the one cumulative `curios-elab` erased arena the last unit carries, cloned because replay consumes it (`Predecessors::arena`) — rather than one merged value or anything this crate unpacks. A unit is composed of those opaque artifacts rather than flattened into their fields.

**Rationale.** Merging would copy the standard library into every compilation. Widening the stages' internals so a struct here could hold them would export a resolver's internals for no consumer.

### The stored-unit format lives below the store

**Decision.** What a stored unit is — the `Record` of what it was compiled from, the framing that puts it ahead of the archived unit in one file, and the reading of the two apart — is this crate's; what verifies a record and where a slot is addressed stay in `curios-verdicts` and `curios-package`.

**Rationale.** Two producers write the format and neither may depend on the other: the store files a unit from above the pipeline, and the prelude's build script images `/sys` and `/std` from below every store. One statement below both lets the prelude image carry the record a slot does, framing the unit before certification where a slot frames a certified one.

**Rejected.** A crate of its own for one struct and two functions; the archive crate depending on `curios-verdicts`, which pulls the package layer and the pipeline under the prelude's build script.

### A unit is certified by construction

**Decision.** A `Unit` carries the certifier's record of its definitions, and `Uncertified::certified` is the only way to make one: a compilation assembles an `Uncertified` from what each stage produced and certifies it with the record the kernel's walk left. The only uncertified units kept anywhere are the prelude's images, which `curios-prelude` certifies as it restores them.

**Rationale.** A later walk reads a unit in scope for its definitions' totality, so a unit without a record would be a case every reader handles, for the one producer below the certifier by design. The state in the type removes the case from every consumer.

**Rejected.** An optional record, a special case every reader carries; a unit generic over its record, which two named types say without the machinery; the record outside the unit, which turns the predecessors into a slice of pairs.

### A unit carries no identity another compilation could mint

**Decision.** Nothing a unit stores is a position in a counter another compilation also counts. A binder's label is its display hint; no term holds a free local or a metavariable; a witness's ordinal counts within its module; and every counter a lowering and an elaboration mint from starts at zero for each declaration of the unit, the lowering's count carried beside the module for the elaborator to start above (`curios_core::Minted`). The erased arena is the one exception, and it is the prefix's.

**Rationale.** An identity meaningful only in the compilation that assigned it has no safe direction to degrade in: restored beside a unit whose counters hand out the same index, it aliases silently, which admits rather than fails. Refusing one where a unit is stored (`validate_stored_identities`) and where a module is judged (the kernel's free-local refusal) lets every counter start at zero, so a unit's bytes depend on its own sources and its scope's interfaces, never on how much its predecessors minted; and counting from zero for each declaration, never for the unit, keeps a declaration's bytes from following the declarations written or elaborated before it ([A declaration is a function of what it reads](../documentation/design/compilation/a-declaration-is-a-function-of-what-it-reads.md)).

**Rejected.** Floors, each unit's counters resuming above every predecessor's: a floor widens safely, but it ties a unit's bytes to its place in the fold and asks every walk to trust a carried number nothing checks.

### A unit is a function of what it was compiled from

**Decision.** Two compilations of one text against one scope, by one compiler, store the same bytes. Whatever a stage walks into a term, a table or a list a unit stores is walked in an order the program states — registration, declaration or label — never a hash's.

**Rationale.** A successor's record vouches for the bytes each predecessor contained, so a unit that differs from one compilation to the next is a miss for every unit after it; two commands filing one slot write one thing only where this holds; and it is what a compilation over several workers is held to. A hash's order reaches further than the table it orders: the elaborator's frames hold one guard under every spelling it is met by, the bound prover read them in a hash's order and minted a different number of binders doing it, and every later proof moved with the count, since a term's structural hash ranks the atoms a bound is searched over and takes each local's index. `curios-elab`'s `a_frames_refinements_are_read_in_the_order_they_were_registered` and `curios-pipeline`'s `a_name_exported_along_several_paths_is_stored_the_same_every_time` hold the two orders that reached the standard library's unit, and `std_unit_reproduction` measures the library whole.

**Rejected.** Ordering a map where it is archived and nowhere else, which `curios-text`'s `OrderedMap` does: it orders the map's own entries, and not a list built by walking the map. Comparing units up to what moved, which leaves a record nothing can compare by its digest.

### The erased arena is the fold's, not the unit's

**Decision.** The arena a `Unit` carries is cumulative from the first unit forward — each unit's erasure resumes over the previous one's — never an independent arena numbered from zero.

**Rationale.** Independently erased arenas both start at zero, so per-unit artifacts would need a relocation pass, the one rustc pays a `cnum_map` for. A stored unit's key names its exact ordered predecessors, so the arena a restored unit carries always matches the predecessors it is restored after, which is what lets a unit be stored whole.
