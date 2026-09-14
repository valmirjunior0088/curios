# curios-unit

The compilation unit: what one unit hands its successors — one opaque artifact per stage — and the `Prefix` of borrowed predecessors each stage is compiled against. A compilation is a set of units folded over a dependency order; the intrinsic root is a unit, the standard library above it is a unit, a package is a unit, and the program asked for is the unit with no successors, which is what lets it own the empty prefix and carry the entrypoint. What `Unit` and `Prefix` expose belongs to the crate rustdoc.

## Design

### Below the kernel, so a build script constructing a unit never reaches the certifier

**Decision.** This crate depends on every stage that does not judge — `curios-text`, `curios-elab`, `curios-ersd` — and deliberately not on `curios-cert`; judgment is interleaved by the driver above it. Checkable: `cargo tree -p curios-unit --edges normal` must not contain `curios-cert`.

**Rationale.** The driver depends on the kernel, and `curios-prelude-archive`'s build script has to construct a `Unit`. A build script reaching the kernel re-runs on every certifier edit, and re-running that one re-elaborates the whole standard library — the 469-second regression `curios-analysis` was split out to fix, arriving through a different door.

### A scope is borrowed, per stage, as that stage's own type

**Decision.** `Prefix` hands each stage every predecessor *borrowed*, as a slice of the opaque type that stage owns — `curios-text`'s resolution state, `curios-elab`'s erased arena — rather than one merged value or anything this crate unpacks. The unit itself is composed of those opaque artifacts rather than flattened into their fields.

**Rationale.** Merging would copy the standard library into every compilation, the cost retiring the splice removed. Widening the stages' internals to `pub` so a struct here could hold them directly would export a resolver's internals for no consumer; each stage builds its own view instead.

### The stored-unit format lives below the store

**Decision.** What a stored unit *is* — the `Record` of what it was compiled from, the framing that puts that record ahead of the archived unit in one file, and the reading of the two back apart — is this crate's. What verifies a record, and where a slot is addressed, stay in `curios-verdicts` and `curios-package`.

**Rationale.** Two producers write the format and neither may depend on the other: the store files a unit from above the pipeline, and `curios-prelude-archive`'s build script images the fixed prelude from below every store. Stating the format once below both is what lets the prelude image carry the same record a slot does, so a question about a standard-library module can take the archived unit as a baseline exactly as it takes a stored one, and lets `curios document` read a unit off either.

**Rejected.** A crate of its own for the format: one struct and two functions do not carry a crate. The archive crate depending on `curios-verdicts`: that pulls `curios-package` and the pipeline under the build script that constructs the prelude, which is the regression the "below the kernel" decision exists to prevent, arriving through another door.

### The erased arena is the prefix's, not the unit's

**Decision.** The arena a `Unit` carries is cumulative from the first unit forward — each unit's erasure resumes over the previous one's — and never an independent arena numbered from zero.

**Rationale.** Two independently erased arenas both start at zero, so per-unit artifacts would need a relocation pass, which is `cnum_map` again. They are not independent, and a stored unit's key names its exact ordered predecessors, so the arena a restored unit carries always matches the prefix it is restored into. That is what lets a unit be stored whole.
