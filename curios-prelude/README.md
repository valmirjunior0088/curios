# curios-prelude

Curios's fixed prelude, certified by the independent kernel as a condition of this crate building: its build script restores the two images `curios-prelude-archive` produced — `/sys`, then `/std` against it — walks every item with `curios-cert` in that order, fails the build on any refusal, and files the record the walk leaves of each root's definitions — each one's totality and what judging it read — with which the crate certifies the units it lends. The environment is mounted root by root as the fold goes, since `/std` names `/sys`. Depend on this crate, never on `curios-prelude-archive`, which hands out an image no kernel has seen. What the crate re-exports belongs to the crate rustdoc; what the image holds is `curios-prelude-archive/README.md`'s.

## Design

### Certification is a crate, not a check

**Decision.** The invariant *an archive that exists is one whose every item the kernel accepted* is enforced by making this the only crate that hands out the prelude, and one that does not compile unless the kernel accepted every item.

**Rationale.** As a test the invariant is a convention: an image could exist, be compiled against and never have been walked. As a crate it is a build-time impossibility, Rocq's `.vok` reached independently.

### Split from the archive, so a certifier edit re-certifies without re-elaborating

**Decision.** Elaboration and serialization live in `curios-prelude-archive`'s build script; restoration and certification live in this one.

**Rationale.** Cargo re-runs a build script whenever any of its dependencies changes, so one script doing both re-elaborates the whole standard library for every `curios-cert` edit, for work the certifier cannot affect.

### The certifier's record is filed here, beside the images rather than inside them

**Decision.** The certifying walk returns a record of each root's definitions — their totality, closed over what each mentions, and what judging each read of other items (`curios_core::Certification`) — and the build script files it as `certification.rkyv` under its `OUT_DIR`, beside its one reader. The crate restores the archive's images through `curios_prelude_archive::restore_archives`, certifies each root's `Uncertified` unit with its record, which is what makes it a `Unit`, and lends those through `with_prelude`, re-exporting the archive's items by name so the uncertified restoration never reaches a consumer.

**Rationale.** A later walk reads the prelude's totality from this record rather than from elaboration's stamps, so the record travels with the units every compilation borrows. It cannot ride in the archive's images, which are built before certification by a script that must not reach the kernel.

**Rejected.** Re-certifying on first use in each process, which re-answers a settled question at the cost of the whole walk; re-imaging each root as a certified unit, which serializes the whole prelude again on every certifier edit to carry a record a small fraction of its size.
