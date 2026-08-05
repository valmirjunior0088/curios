# curios-prelude

Build-scoped archived ownership of Curios's fixed `/sys`, `/syn`, and `/std` prelude: the authored `.crs` sources, the canonical registry of compiler-emitted `/syn` names, and the build script that compiles them into the rkyv image production compilation replays. The archive and replay APIs belong to the crate rustdoc; every source module must be registered in its Curios index.

## Design

### The archive is build-scoped, not an interchange format

**Decision.** The prelude ships as an rkyv image in this crate's `OUT_DIR`, scoped to one compiler build: the build script discovers and fingerprints every `.crs` input and emits the matching rebuild directives, and production compilation replays the image with no source fallback and no cache-miss branch — construction or restoration failure is a compiler invariant and fails loudly.

**Rationale.** A fallback would turn an invariant violation into a silent recompile, letting the archive drift from the sources it claims to capture; failing loudly keeps the archive's fidelity a checked property. Scoping the image to one compiler build removes any stability obligation on the format, so it can change freely with the representations it serializes.

### /syn holds compiler-emitted names only

**Decision.** `/syn` owns exactly the names Rust lowering emits — the proof-certified literal structures (`/syn/Char`, `/syn/Str`) and the concepts syntax dispatches through. A concept may be declared at `syn.crs` top level or in its own module file beside them; the choice is presentation, and what it decides is the name — `Eql` in a file is `/syn/Eql/Eql`, not `/syn/Eql` — so the Rust registry entry moves with it. Operator witnesses live in the `/std` operator facades and other witnesses beside their types; the canonical Rust registry of the hidden lowering targets is this crate's `src/syntax.rs` for the syntax-directed names and `curios-base`'s `NumOp::concept_field` for the operator concepts, and the registry contract belongs to `curios-text`.

**Rationale.** The compiler's lowering contract needs a closed, auditable surface: a name the compiler emits must exist with the shape the registry claims, and keeping `/syn` to exactly that set lets the registry be checked against it mechanically while `/std` stays free to evolve as an ordinary library.
