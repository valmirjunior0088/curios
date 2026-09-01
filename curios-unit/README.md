# curios-unit

The compilation unit: what one unit hands its successors — one opaque artifact per stage — and the `Prefix` of borrowed predecessors each stage is compiled against. A compilation is a set of units folded over a dependency order; the standard library is a unit, a package is a unit, and the program asked for is the unit with no successors, which is what lets it own the empty prefix and carry the entrypoint. What `Unit` and `Prefix` expose belongs to the crate rustdoc.

## Design

### Below the kernel, so a build script constructing a unit never reaches the certifier

**Decision.** This crate depends on every stage that does not judge — `curios-text`, `curios-elab`, `curios-ersd` — and deliberately not on `curios-cert`; judgment is interleaved by the driver above it. Checkable: `cargo tree -p curios-unit --edges normal` must not contain `curios-cert`.

**Rationale.** The driver depends on the kernel, and `curios-prelude-archive`'s build script has to construct a `Unit`. A build script reaching the kernel re-runs on every certifier edit, and re-running that one re-elaborates the whole standard library — the 469-second regression `curios-analysis` was split out to fix, arriving through a different door.

### A scope is borrowed, per stage, as that stage's own type

**Decision.** `Prefix` hands each stage every predecessor *borrowed*, as a slice of the opaque type that stage owns — `curios-text`'s resolution state, `curios-elab`'s erased arena — rather than one merged value or anything this crate unpacks. The unit itself is composed of those opaque artifacts rather than flattened into their fields.

**Rationale.** Merging would copy the standard library into every compilation, the cost retiring the splice removed. Widening the stages' internals to `pub` so a struct here could hold them directly would export a resolver's internals for no consumer; each stage builds its own view instead.

### The erased arena is the prefix's, not the unit's

**Decision.** The arena a `Unit` carries is cumulative from the first unit forward — each unit's erasure resumes over the previous one's — and never an independent arena numbered from zero.

**Rationale.** Two independently erased arenas both start at zero, so per-unit artifacts would need a relocation pass, which is `cnum_map` again. They are not independent, and a stored unit's key names its exact ordered predecessors, so the arena a restored unit carries always matches the prefix it is restored into. That is what lets a unit be stored whole.
