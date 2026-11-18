# curios-binaryen

WebAssembly-level optimization for the Curios native product via a statically linked Binaryen: the build script downloads, verifies and builds a pinned Binaryen source release, and the library exposes its optimizer over serialized module bytes, after `curios-wasm` encoding and knowing nothing about any Curios IR.

## Design

### Built from a pinned source, cached outside Cargo's target tree

**Decision.** Binaryen is built with CMake in the build script's `OUT_DIR`, from a source release checked against its pinned hash in memory and never stored, and only the static library it installs is kept: in a locked, target-specific cache in `.artifacts/` beside this crate, not under `target/`, which every Cargo mode and feature set reads. A cache entry is valid only against a marker naming the Binaryen version, the verified source hash, a hash of the build script itself, the target triple, and the C++ toolchain's own version string. An archive placed in the entry by hand stands in for the download, for an offline build.

**Rationale.** An `OUT_DIR` is fingerprint-scoped, so every Cargo mode and feature set would repeat a build that takes minutes; the shared cache pays it once per target, and the lock makes concurrent invocations safe. The library is all a link reads, so the unpacked source and CMake's build tree are scratch in the `OUT_DIR` of the configuration that builds, and a rebuild fetches the source again rather than keeping an archive beside its product. Beside the crate, `cargo clean` stays ordinary and no build script reconstructs Cargo's undocumented layout: `CARGO_MANIFEST_DIR` is an interface, `OUT_DIR`'s ancestry is not. Hashing the build script makes a changed CMake flag invalidate warm caches with no step to remember, and the toolchain string closes the other half: the entry's path carries the triple, so an architecture cannot be confused, but two machines of one triple on different distributions produce incompatible libraries under identical paths — beside the crate, one `rsync`, shared checkout or restored CI cache away.

**Rejected.** A hand-bumped `BUILD_SCHEMA` constant, correct and forgettable: nothing but memory connects a changed flag to the bump, so one commit could link a library built with old flags here and new flags on a cold machine. The library itself in `OUT_DIR`, built again by every configuration, after every `cargo clean` and in every worktree. The source, CMake's build tree and the archive kept in the entry, none of which a link reads.

### The last stage, over bytes

**Decision.** Binaryen runs last, over the serialized module `curios_wasm::to_bytes` writes, and this crate knows nothing about any Curios IR. Semantic optimization belongs to `curios-anf`'s and `curios-cps`'s optimizers.

**Rationale.** What a program means is held in the IRs above, so a rewrite that needs it is written where it is known; Binaryen's passes need only the module, and taking it as bytes keeps the library behind one seam that names no Curios type.
