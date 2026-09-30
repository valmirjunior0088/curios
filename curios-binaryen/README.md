# curios-binaryen

WebAssembly-level optimization for the Curios native product via a statically linked Binaryen: the build script downloads, verifies and builds a pinned Binaryen source release, and the library exposes its optimizer over serialized module bytes, after `curios-wasm` encoding and knowing nothing about any Curios IR.

## Design

### Built from a pinned source, cached outside Cargo's target tree

**Decision.** Binaryen is built from a checksum-verified source release with CMake, and the C++ build is shared through a locked, target-specific cache in `.artifacts/` beside this crate — neither a fingerprint-specific `OUT_DIR` nor anywhere under `target/`. A cache entry is valid only against a marker naming the Binaryen version, the verified source hash, a hash of the build script itself, the target triple, and the C++ toolchain's own version string.

**Rationale.** An `OUT_DIR` is fingerprint-scoped, so every Cargo mode and feature set would repeat a build that takes minutes; the shared cache pays it once per target, and the lock makes concurrent invocations safe. Beside the crate, `cargo clean` stays ordinary and no build script reconstructs Cargo's undocumented layout: `CARGO_MANIFEST_DIR` is an interface, `OUT_DIR`'s ancestry is not. Hashing the build script makes a changed CMake flag invalidate warm caches with no step to remember, and the toolchain string closes the other half: the entry's path carries the triple, so an architecture cannot be confused, but two machines of one triple on different distributions produce incompatible libraries under identical paths — beside the crate, one `rsync`, shared checkout or restored CI cache away.

**Rejected.** A hand-bumped `BUILD_SCHEMA` constant, correct and forgettable: nothing but memory connects a changed flag to the bump, so one commit could link a library built with old flags here and new flags on a cold machine.

### The optimized module is observed through Binaryen's own text writer

**Decision.** `optimize_with_text` renders the optimized module with `BinaryenModuleAllocateAndWriteText`, from the in-memory module the optimizer just rewrote, and that text is the `wonder stage wasm-optm` dump. It is eyes-only: nothing parses it, and the folded s-expression dialect is Binaryen's to change.

**Rationale.** The observation exists to show what the optimizer did, and its own printer is the one renderer that cannot misrepresent it; the module is alive between `BinaryenModuleOptimize` and `BinaryenModuleDispose`, so the capture is one C call.

**Rejected.** A binary reader in `curios-wasm` printing the optimized bytes in the house rendering, whose bug would misrepresent the thing observed; reinstate for a consumer that must hold the optimized module as data, when the operand-less and memarg encodings can become paired tables beside `Instr` as the WAT `mnemonics!` table is. Parsing Binaryen's text, a second grammar with no other consumer. A `wasmprinter` dependency re-parsing bytes whose source module is still in memory.
