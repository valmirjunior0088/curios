# curios-binaryen

WebAssembly-level optimization for the Curios native product via a statically linked Binaryen: the build script downloads, verifies and builds a pinned Binaryen source release, and the library exposes its optimizer over serialized module bytes, after `curios-wasm` encoding and knowing nothing about any Curios IR.

## Design

### Built from a pinned source, cached outside Cargo's target tree

**Decision.** Binaryen is built with CMake in the build script's `OUT_DIR`, from a source release checked against its pinned hash in memory and never stored, and only the static library it installs is kept: in a locked, target-specific cache in `.artifacts/` beside this crate, not under `target/`, which every Cargo mode and feature set reads. A cache entry is valid only against a marker naming the Binaryen version, the verified source hash, a hash of the build script itself, the target triple, and the C++ toolchain's own version string. An archive placed in the entry by hand stands in for the download, for an offline build.

**Rationale.** An `OUT_DIR` is fingerprint-scoped, so every Cargo mode and feature set would repeat a build that takes minutes; the shared cache pays it once per target, and the lock makes concurrent invocations safe. The library is all a link reads, so the unpacked source and CMake's build tree are scratch in the `OUT_DIR` of the configuration that builds, and a rebuild fetches the source again rather than keeping an archive beside its product. Beside the crate, `cargo clean` stays ordinary and no build script reconstructs Cargo's undocumented layout: `CARGO_MANIFEST_DIR` is an interface, `OUT_DIR`'s ancestry is not. Hashing the build script makes a changed CMake flag invalidate warm caches with no step to remember, and the toolchain string closes the other half: the entry's path carries the triple, so an architecture cannot be confused, but two machines of one triple on different distributions produce incompatible libraries under identical paths — beside the crate, one `rsync`, shared checkout or restored CI cache away.

**Rejected.** A hand-bumped `BUILD_SCHEMA` constant, correct and forgettable: nothing but memory connects a changed flag to the bump, so one commit could link a library built with old flags here and new flags on a cold machine. The library itself in `OUT_DIR`, built again by every configuration, after every `cargo clean` and in every worktree. The source, CMake's build tree and the archive kept in the entry, none of which a link reads.

### The optimized module is observed through Binaryen's own text writer

**Decision.** `optimize` hands its observer an `Optimized` view of the module it just rewrote, which renders with `BinaryenModuleAllocateAndWriteText` when the observer formats it, and that text is the `wonder stage wasm-optm` dump. It is eyes-only: nothing parses it, and the folded s-expression dialect is Binaryen's to change.

**Rationale.** The observation exists to show what the optimizer did, and its own printer is the one renderer that cannot misrepresent it; the module is alive between `BinaryenModuleOptimize` and `BinaryenModuleDispose`, so the capture is one C call, made only for an observer that looks.

**Rejected.** A binary reader in `curios-wasm` printing the optimized bytes in the house rendering, whose bug would misrepresent the thing observed; reinstate for a consumer that must hold the optimized module as data, when the operand-less and memarg encodings can become paired tables beside `Instr` as the WAT `mnemonics!` table is. Parsing Binaryen's text, a second grammar with no other consumer. A `wasmprinter` dependency re-parsing bytes whose source module is still in memory.
