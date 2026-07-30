# curios-binaryen

WebAssembly-level optimization for the Curios native product via a statically linked Binaryen: the build script downloads, verifies, and builds a pinned Binaryen source release, and the library exposes its optimizer over serialized module bytes, after `curios-wasm` encoding and knowing nothing about any Curios IR.

## Design

### Built from a pinned source, cached outside Cargo's fingerprint

**Decision.** Binaryen is built from a checksum-verified source release with CMake, and the expensive C++ build is shared through a locked, target-specific cache under `target/binaryen` rather than a Cargo fingerprint-specific `OUT_DIR`. `BUILD_SCHEMA` names the cache's contract and is bumped when the CMake configuration or the installed-library layout changes.

**Rationale.** An `OUT_DIR` is fingerprint-scoped, so every Cargo mode — debug, release, clippy, each feature set — would repeat a build that takes minutes and requires a C++ toolchain; the shared cache pays it once per target. The lock makes concurrent Cargo invocations safe, and the schema number makes cache staleness an explicit contract instead of a guess.
