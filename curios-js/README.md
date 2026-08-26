# curios-js

The Curios ↔ JavaScript boundary: wasm-bindgen exports of the pure compile pipeline plus the browser run harness. Build steps belong to `xtask`.

## Design

### Plain cargo plus the bindings generator as a library

**Decision.** The browser build is `cargo xtask js`: `cargo build` for wasm32, then `--target web` bindings generation. No `wasm-pack`, and no `wasm-opt`: Binaryen optimization belongs only to the native `curios` product.

**Rationale.** The two tools the build actually needs are the compiler and the bindings generator; a packager on top adds a second build system to version, cache, and debug without adding a capability. Keeping Binaryen out preserves the crate-boundary ownership of optimization, so the browser artifact is the pure pipeline's output, reproducible from the workspace toolchain alone.
