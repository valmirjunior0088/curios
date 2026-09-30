# curios-unit

The compilation unit: what one unit hands its successors — one opaque artifact per stage — and the `Prefix` of borrowed predecessors each stage is compiled against. A compilation is a set of units folded over a dependency order; the intrinsic root is a unit, the standard library above it is a unit, a package is a unit, and the program asked for is the unit with no successors, which is what lets it own the empty prefix and carry the entrypoint. What `Unit` and `Prefix` expose belongs to the crate rustdoc.

## Design

### Below the kernel, so a build script constructing a unit never reaches the certifier

**Decision.** This crate depends on every stage that does not judge — `curios-text`, `curios-elab`, `curios-ersd` — and deliberately not on `curios-cert`; judgment is interleaved by the driver above it. Checkable: `cargo tree -p curios-unit --edges normal` must not contain `curios-cert`.

**Rationale.** The driver depends on the kernel, and `curios-prelude-archive`'s build script has to construct a unit — an `Uncertified` one, since no certifier can reach it there. A build script reaching the kernel re-runs on every certifier edit, and re-running that one re-elaborates the whole standard library — the 469-second regression `curios-analysis` was split out to fix, arriving through a different door.

### A scope is borrowed, per stage, as that stage's own type

**Decision.** `Prefix` hands each stage every predecessor *borrowed*, as a slice of the opaque type that stage owns — `curios-text`'s resolution state, `curios-elab`'s erased arena — rather than one merged value or anything this crate unpacks. The unit itself is composed of those opaque artifacts rather than flattened into their fields.

**Rationale.** Merging would copy the standard library into every compilation, the cost retiring the splice removed. Widening the stages' internals to `pub` so a struct here could hold them directly would export a resolver's internals for no consumer; each stage builds its own view instead.

### The stored-unit format lives below the store

**Decision.** What a stored unit *is* — the `Record` of what it was compiled from, the framing that puts that record ahead of the archived unit in one file, and the reading of the two back apart — is this crate's. What verifies a record, and where a slot is addressed, stay in `curios-verdicts` and `curios-package`.

**Rationale.** Two producers write the format and neither may depend on the other: the store files a unit from above the pipeline, and `curios-prelude-archive`'s build script images the fixed prelude from below every store. Stating the format once below both is what lets the prelude image carry the same record a slot does — the compiler's own account of the tree it was built from — though what it frames is the unit before certification, where a slot frames a certified one.

**Rejected.** A crate of its own for the format: one struct and two functions do not carry a crate. The archive crate depending on `curios-verdicts`: that pulls `curios-package` and the pipeline under the build script that constructs the prelude, which is the regression the "below the kernel" decision exists to prevent, arriving through another door.

### A unit is certified by construction

**Decision.** A `Unit` carries the certifier's record of its definitions, and `Uncertified::certified` is the only way to make one: a compilation assembles an `Uncertified` from what each stage produced and certifies it with the record the kernel's walk left. The only uncertified units kept anywhere are the prelude's images, which `curios-prelude` certifies as it restores them.

**Rationale.** A unit in scope is read by a later walk for its definitions' totality, so a unit without a record is one every reader would have to handle — and it had exactly one producer, the archive's build script, which sits below the certifier by design. Stating the state in the type rather than in an `Option` removes the case from every consumer instead of documenting it at each.

**Rejected.** An optional record, which is what this replaced: a special case every reader carried for one producer. A unit generic over its record, with `()` for an image: two named types say the same with none of the machinery. The record outside the unit: every scope would become a slice of pairs and every slot would gain a third segment, for nothing the pairing inside does not already give.

### A unit carries no identity another compilation could mint

**Decision.** Nothing a unit stores is a position in a counter some other compilation also counts. A binder's label is its display hint; no term holds a free local or a metavariable; a witness's ordinal counts within the module that declares it; and every counter a unit's lowering and elaboration mint from starts at zero for that unit. The erased arena below is the one exception, until the verdicts campaign's part 6 deletes it.

**Rationale.** An identity meaningful only in the compilation that assigned it has no safe direction to degrade in: restored beside a unit whose own counters hand out the same index, it aliases silently, which admits rather than fails. Refusing one where a unit is stored (`validate_stored_identities`) and where a module is judged (the kernel's free-local refusal) is what lets every counter start at zero — so a unit's bytes depend on its own sources and its scope's interfaces, never on how much its predecessors minted.

**Rejected.** Floors, which this replaced: each unit's binder, metavariable and universe counters resumed above every predecessor's, and the universe-seed table was cumulative from the first unit. A floor is a bound and widened safely, but it tied a unit's bytes to where it sat in the fold, and asked every walk to trust a carried number nothing checked.

### The erased arena is the prefix's, not the unit's

**Decision.** The arena a `Unit` carries is cumulative from the first unit forward — each unit's erasure resumes over the previous one's — and never an independent arena numbered from zero.

**Rationale.** Two independently erased arenas both start at zero, so per-unit artifacts would need a relocation pass, which is `cnum_map` again. They are not independent, and a stored unit's key names its exact ordered predecessors, so the arena a restored unit carries always matches the prefix it is restored into. That is what lets a unit be stored whole.
