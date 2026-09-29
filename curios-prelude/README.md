# curios-prelude

Curios's fixed prelude, certified by the independent kernel as a condition of this crate building: its build script restores the two images `curios-prelude-archive` produced — `/sys`, then `/std` against it — walks every item with `curios-cert` in that order, fails the build on any refusal, and files the record the walk leaves of each root's totality verdicts, which the crate attaches to the units it lends. The environment is mounted root by root as the fold goes, because `/std` names `/sys` and an empty one would refuse every carrier it wraps. Depend on this crate, never on `curios-prelude-archive` directly — that one hands out an image no kernel has seen. What the crate re-exports belongs to the crate rustdoc; what the image holds is `curios-prelude-archive/README.md`'s.

## Design

### Certification is a crate, not a check

**Decision.** The invariant *an archive that exists is one whose every item the kernel accepted* is enforced by making this the only crate that hands out the prelude, and one that does not compile unless the kernel accepted every item.

**Rationale.** Stated as a test, the invariant is a convention: an image could exist, be compiled against, and never have been walked. Stated as a crate, it is a build-time impossibility — Coq's `.vok`, reached independently.

### Split from the archive, so a certifier edit re-certifies without re-elaborating

**Decision.** Elaboration and serialization live in `curios-prelude-archive`'s build script; restoration and certification live in this one. Two crates, two scripts.

**Rationale.** Cargo re-runs a build script whenever any of its dependencies change. The single script this replaced re-elaborated the entire standard library for every `curios-cert` edit, spent re-deriving something the certifier cannot affect — `curios-analysis/README.md` carries what that measured, and `curios-prelude-archive/README.md`'s "Why this is not `curios-prelude`" states the other half of the split.

### The certifier's record is filed here, beside the images rather than inside them

**Decision.** The certifying walk returns a record of each root's definitions and their totality, closed over what each mentions (`curios_core::Certification`), and the build script files the two at `.artifacts/certification.rkyv`. The crate restores the archive's images itself, through `curios_prelude_archive::restore_archives`, attaches each root's record to its unit, and lends those through its own `with_prelude`. It re-exports the archive's items by name rather than by glob, so the archive's uncertified restoration never reaches a consumer through this crate.

**Rationale.** A later walk reads the prelude's totality from this record, not from the stamps elaboration wrote, so the record must travel with the units every compilation borrows. It cannot ride in the archive's images: those are built before certification, and a record written there would need the kernel in the archive's build script, which is the re-elaboration this split exists to avoid. **Rejected:** re-certifying on first use in each process, which re-answers a settled question at the cost of the whole walk; and filing the record in `OUT_DIR`, since a build product that outlives its build lives in `.artifacts/` beside its owner, as the images do.
