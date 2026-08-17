# curios-wasm

The Curios WebAssembly-GC target: the symbolic module model, the WAT parser, and the binary encoder — the pipeline's final stage. `curios-cont` lowers continuation IR into a `Module`, and `to_bytes` produces the binary that wasmtime (`curios-runtime`), the browser (`curios-js`), and `wasm-opt` (`curios-binaryen`) consume. How identities are spelled is the cross-cutting decision in [One naming scheme for compiler identities](../documentation/design/toolchain/one-naming-scheme-for-compiler-identities.md); the type grammar, the instruction set, and the encoder's section order belong to the crate rustdoc.

## Design

### Everything is symbolic, and the index spaces exist only inside the encoder

**Decision.** Items and their cross-references use the `name!` newtypes in `names` — `TypeName`, `FuncName`, and the rest. The numeric index spaces the binary format is defined over are derived from declaration order at encoding time and exist nowhere else; nothing above the encoder can hold one.

**Rationale.** An index is a fact about a finished module, and the module is not finished while it is being built. Constructing one with indices means every insertion can invalidate a reference that is already written down, so the builder would owe a renumbering pass and every caller would owe it correctness — a class of bug that produces a *valid* module computing the wrong thing, which no validator catches and no test names. With names, an unresolved reference is a lookup failure at encode time, in one place, before any bytes exist.

It also keeps the emitter honest about ordering: because the encoder assigns indices from declaration order, `curios-cont` may emit items in whatever order its lowering finds natural, and the two concerns never negotiate.

**Rejected.** Carrying indices in the model and renumbering on mutation. It moves a whole-module invariant into every mutation site, and its failure mode is silent.
