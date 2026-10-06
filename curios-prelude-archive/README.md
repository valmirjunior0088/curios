# curios-prelude-archive

The build step that folds Curios's fixed `/sys` and `/std` prelude into the rkyv images production compilation replays — `sys.rkyv` then `std.rkyv`, one per unit — from its two roots: `/sys`, which `curios-text` generates beside the registry of the names the compiler emits, and `/std`, which `curios-std` holds. Consumers depend on `curios-prelude`, never on this crate, because the image here has been elaborated and not judged; the archive and replay APIs belong to the crate rustdoc.

## Design

### This is not `curios-prelude`, and it never reaches the kernel

**Decision.** This crate's build script elaborates and serializes; `curios-prelude`'s restores the image and certifies it with `curios-cert`. This crate never gains a `curios-cert` dependency in any kind that propagates, which `cargo tree -p curios-prelude-archive --edges build` checks.

**Rationale.** Cargo's rebuild granularity is the build script, so one script doing both would re-elaborate the whole standard library on every kernel edit, for work no kernel rule can affect. What keeps the kernel out transitively is `curios-analysis` standing apart from `curios-cert` and `curios-elab` taking the certifier as a dev-dependency only.

### The archive is build-scoped, not an interchange format

**Decision.** The prelude ships as one rkyv image of an `Uncertified` unit per root, filed under the build script's `OUT_DIR` and scoped to one compiler build. The build script emits one rebuild directive per file the lowering read — every file a `mod` line reaches — and production compilation replays the images with no source fallback and no cache-miss branch: construction or restoration failure is a compiler invariant and fails loudly. Each image is framed as a store slot is, `curios-unit`'s record of what the unit was compiled from ahead of the unit: `/sys` records no reads and nothing before it, `/std` every file of its tree by canonical path and the `/sys` image as its one predecessor. `/sys` comes first and `/std` is folded against it, its erased arena resuming above `/sys`'s while every counter it mints starts at zero, since no term `/sys` stores carries one; the order is a constant of `restore.rs`.

**Rationale.** A fallback would turn an invariant violation into a silent recompile and let the archive drift from its sources. Scoping the image to one build frees the format to change with the representations it serializes, and `OUT_DIR` suits a product whose one reader is the crate that includes it — `curios document` reads the standard library off the prelude the compiler embeds. The record makes the archive's provenance a fact the compiler wrote down, and the rerun directives are read off the read log it is built from. One image per root keeps one framing for every file a store read meets, and rkyv's sharing by pointer address is per image, so a table spanning both would report a structure count neither has. The other order would hand a successor a scope its names were never resolved in.
