# A binary reader for `curios-wasm`

**Not refined yet.** This specification reserves reading WebAssembly binaries back into `curios-wasm`'s model. It is not an implementation plan.

## What is missing

`curios-wasm` writes the binary format and parses and prints the text format, round-tripped against the binary writer, but reads no binary. So `curios/src/tests/wasm_conformance.rs` checks the binary side only by the engine's acceptance, and a module Binaryen hands back is rendered by Binaryen's own text writer rather than by the model.

## Refinement

The reader's scope — every section the writer emits, or the envelope the model already covers — and which checks it replaces.
