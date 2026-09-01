# curios-binaryen

WebAssembly-level optimization for the Curios native product via a statically linked Binaryen: the build script downloads, verifies, and builds a pinned Binaryen source release, and the library exposes its optimizer over serialized module bytes, after `curios-wasm` encoding and knowing nothing about any Curios IR.

## Design

### Built from a pinned source, cached outside Cargo's target tree

**Decision.** Binaryen is built from a checksum-verified source release with CMake, and the expensive C++ build is shared through a locked, target-specific cache in `.artifacts/` beside this crate — neither a Cargo fingerprint-specific `OUT_DIR` nor anywhere under `target/`. A cache entry is valid only against a marker naming the Binaryen version, the verified source hash, a hash of the build script itself, the target triple, and the C++ toolchain's own version string.

**Rationale.** An `OUT_DIR` is fingerprint-scoped, so every Cargo mode — debug, release, clippy, each feature set — would repeat a build that takes minutes and requires a C++ toolchain; the shared cache pays it once per target. Living beside the crate rather than under `target/` is what lets `cargo clean` stay an ordinary command, and it means no build script has to reconstruct Cargo's undocumented directory layout to find the cache: `CARGO_MANIFEST_DIR` is an interface, `OUT_DIR`'s ancestry is not. The lock makes concurrent Cargo invocations safe.

**The marker describes the build, not just its inputs.** It replaced a hand-bumped `BUILD_SCHEMA` constant, which was correct and forgettable — nothing but a contributor's memory connected a changed CMake flag to the bump that would invalidate warm caches, so one commit could link a library built with old flags here and new flags on a cold machine. Hashing the build script removes the step rather than documenting it. The toolchain string closes the other half: the entry's *path* carries the target triple, so an architecture cannot be confused, but nothing carried the platform underneath it, and two machines of one triple on different distributions produce incompatible static libraries under identical paths. That was nearly unreachable while the cache sat under `target/`; beside the crate it is one `rsync`, shared checkout, or restored CI cache away.

### The optimized module is observed through Binaryen's own text writer

**Decision.** `optimize_with_text` renders the optimized module with `BinaryenModuleAllocateAndWriteText`, from the same in-memory module the optimizer just rewrote, and that text is the `wonder stage wasm-optm` dump. It is eyes-only: nothing in the workspace parses it, and the folded s-expression dialect is Binaryen's to change.

**Rationale.** The observation's whole purpose is trust — seeing what the optimizer actually did — and the optimizer's own printer is the one renderer that cannot misrepresent it. The module is already alive inside the session between `BinaryenModuleOptimize` and `BinaryenModuleDispose`, so the capture costs one C call and no second parse of anything.

**Rejected.**

- **A binary reader in `curios-wasm`** (`from_bytes`, the encoder's inverse), which would parse the optimized bytes back into the symbolic model and print it in the house rendering. Roughly 1,500 lines plus a writer refactor, serving an observability need one C call serves — and a reader bug would misrepresent exactly the thing being observed. Reinstate condition: a consumer that must hold the optimized module as *data* rather than text — a structural survey over post-Binaryen code, or artifact introspection. If one appears, the groundwork is severable: the binary spellings of the 172 operand-less and memarg instruction encodings can become paired tables beside `Instr` the way the WAT `mnemonics!` table already is, shrinking the writer and making the reader's dispatch data.
- **Parsing Binaryen's text output**, at any point. The folded dialect is a second grammar with no other consumer, and every objection to it attaches to parsing — printing is not parsing.
- **A `wasmprinter` dependency** rendering the optimized bytes: a new dependency row and a re-parse of bytes whose source module is still in memory.
