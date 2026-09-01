# curios-prelude

Curios's fixed prelude, certified by the independent kernel as a condition of this crate building: its build script restores the image `curios-prelude-archive` produced, walks every item with `curios-cert`, and fails the build on any refusal. Depend on this crate, never on `curios-prelude-archive` directly — that one hands out an image no kernel has seen. What the crate re-exports belongs to the crate rustdoc; what the image holds is `curios-prelude-archive/README.md`'s.

## Design

### Certification is a crate, not a check

**Decision.** The invariant *an archive that exists is one whose every item the kernel accepted* is enforced by making this the only crate that hands out the prelude, and one that does not compile unless the kernel accepted every item.

**Rationale.** Stated as a test, the invariant is a convention: an image could exist, be compiled against, and never have been walked. Stated as a crate, it is a build-time impossibility — Coq's `.vok`, reached independently.

### Split from the archive, so a certifier edit re-certifies without re-elaborating

**Decision.** Elaboration and serialization live in `curios-prelude-archive`'s build script; restoration and certification live in this one. Two crates, two scripts.

**Rationale.** Cargo re-runs a build script whenever any of its dependencies change. The single script this replaced re-elaborated the entire standard library for every `curios-cert` edit, spent re-deriving something the certifier cannot affect — `curios-analysis/README.md` carries what that measured, and `curios-prelude-archive/README.md`'s "Why this is not `curios-prelude`" states the other half of the split.
