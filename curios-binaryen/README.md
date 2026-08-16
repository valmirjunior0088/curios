# curios-binaryen

WebAssembly-level optimization for the Curios native product via a statically linked Binaryen: the build script downloads, verifies, and builds a pinned Binaryen source release, and the library exposes its optimizer over serialized module bytes, after `curios-wasm` encoding and knowing nothing about any Curios IR.

## Design

### Built from a pinned source, cached outside Cargo's target tree

**Decision.** Binaryen is built from a checksum-verified source release with CMake, and the expensive C++ build is shared through a locked, target-specific cache in `.artifacts/` beside this crate — neither a Cargo fingerprint-specific `OUT_DIR` nor anywhere under `target/`. A cache entry is valid only against a marker naming the Binaryen version, the verified source hash, a hash of the build script itself, the target triple, and the C++ toolchain's own version string.

**Rationale.** An `OUT_DIR` is fingerprint-scoped, so every Cargo mode — debug, release, clippy, each feature set — would repeat a build that takes minutes and requires a C++ toolchain; the shared cache pays it once per target. Living beside the crate rather than under `target/` is what lets `cargo clean` stay an ordinary command, and it means no build script has to reconstruct Cargo's undocumented directory layout to find the cache: `CARGO_MANIFEST_DIR` is an interface, `OUT_DIR`'s ancestry is not. The lock makes concurrent Cargo invocations safe.

**The marker describes the build, not just its inputs.** It replaced a hand-bumped `BUILD_SCHEMA` constant, which was correct and forgettable — nothing but a contributor's memory connected a changed CMake flag to the bump that would invalidate warm caches, so one commit could link a library built with old flags here and new flags on a cold machine. Hashing the build script removes the step rather than documenting it. The toolchain string closes the other half: the entry's *path* carries the target triple, so an architecture cannot be confused, but nothing carried the platform underneath it, and two machines of one triple on different distributions produce incompatible static libraries under identical paths. That was nearly unreachable while the cache sat under `target/`; beside the crate it is one `rsync`, shared checkout, or restored CI cache away.
