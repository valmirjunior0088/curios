# curios-unit

The compilation unit: what one unit hands its successors — one opaque artifact per stage — and the `Prefix` of borrowed predecessors each stage is compiled against. A compilation is a set of units folded over a dependency order; the intrinsic root is a unit, the standard library above it is one, a package is one, and the program asked for is the unit with no successors, which is what lets it carry the entry point. What `Unit` and `Prefix` expose belongs to the crate rustdoc.

## Design

### Below the kernel, so a build script constructing a unit never reaches the certifier

**Decision.** This crate depends on every stage that does not judge — `curios-text`, `curios-elab`, `curios-ersd` — and deliberately not on `curios-cert`; judgment is interleaved by the driver above it. `cargo tree -p curios-unit --edges normal` must not contain `curios-cert`.

**Rationale.** `curios-prelude-archive`'s build script constructs an `Uncertified` unit, and a build script reaching the kernel re-runs on every certifier edit and re-elaborates the whole standard library, the regression `curios-analysis` stands apart to prevent.

### A scope is borrowed, per stage, as that stage's own type

**Decision.** `Prefix` borrows the predecessor units and hands each stage its view as the opaque type that stage owns — one `curios-text` resolution state per unit (`Prefix::text`), one `curios-core` module per unit (`Prefix::cores`), and the one cumulative `curios-elab` erased arena the last unit carries, cloned because replay consumes it (`Prefix::arena`) — rather than one merged value or anything this crate unpacks. A unit is composed of those opaque artifacts rather than flattened into their fields.

**Rationale.** Merging would copy the standard library into every compilation. Widening the stages' internals so a struct here could hold them would export a resolver's internals for no consumer.

### The stored-unit format lives below the store

**Decision.** What a stored unit is — the `Record` of what it was compiled from, the framing that puts it ahead of the archived unit in one file, and the reading of the two apart — is this crate's; what verifies a record and where a slot is addressed stay in `curios-verdicts` and `curios-package`.

**Rationale.** Two producers write the format and neither may depend on the other: the store files a unit from above the pipeline, and the prelude's build script images `/sys` and `/std` from below every store. One statement below both lets the prelude image carry the record a slot does, framing the unit before certification where a slot frames a certified one.

**Rejected.** A crate of its own for one struct and two functions; the archive crate depending on `curios-verdicts`, which pulls the package layer and the pipeline under the prelude's build script.

### A unit is certified by construction

**Decision.** A `Unit` carries the certifier's record of its definitions, and `Uncertified::certified` is the only way to make one: a compilation assembles an `Uncertified` from what each stage produced and certifies it with the record the kernel's walk left. The only uncertified units kept anywhere are the prelude's images, which `curios-prelude` certifies as it restores them.

**Rationale.** A later walk reads a unit in scope for its definitions' totality, so a unit without a record would be a case every reader handles, for the one producer below the certifier by design. The state in the type removes the case from every consumer.

**Rejected.** An optional record, a special case every reader carries; a unit generic over its record, which two named types say without the machinery; the record outside the unit, which turns every scope into a slice of pairs.

### A unit carries no identity another compilation could mint

**Decision.** Nothing a unit stores is a position in a counter another compilation also counts. A binder's label is its display hint; no term holds a free local or a metavariable; a witness's ordinal counts within its module; and every counter a unit's lowering and elaboration mint from starts at zero for that unit. The erased arena is the one exception, and it is the prefix's.

**Rationale.** An identity meaningful only in the compilation that assigned it has no safe direction to degrade in: restored beside a unit whose counters hand out the same index, it aliases silently, which admits rather than fails. Refusing one where a unit is stored (`validate_stored_identities`) and where a module is judged (the kernel's free-local refusal) lets every counter start at zero, so a unit's bytes depend on its own sources and its scope's interfaces, never on how much its predecessors minted.

**Rejected.** Floors, each unit's counters resuming above every predecessor's: a floor widens safely, but it ties a unit's bytes to its place in the fold and asks every walk to trust a carried number nothing checks.

### The erased arena is the prefix's, not the unit's

**Decision.** The arena a `Unit` carries is cumulative from the first unit forward — each unit's erasure resumes over the previous one's — never an independent arena numbered from zero.

**Rationale.** Independently erased arenas both start at zero, so per-unit artifacts would need a relocation pass, the one rustc pays a `cnum_map` for. A stored unit's key names its exact ordered predecessors, so the arena a restored unit carries always matches the prefix it is restored into, which is what lets a unit be stored whole.
