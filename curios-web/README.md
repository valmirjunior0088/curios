# curios-web

The Curios ↔ JavaScript boundary: wasm-bindgen exports of the pure compile pipeline plus the browser run harness. Build steps and the exact-version `wasm-bindgen-cli` requirement belong to the crate rustdoc.

## Design

### Plain cargo plus wasm-bindgen-cli

**Decision.** The browser build is `cargo build` for wasm32 followed by `wasm-bindgen-cli --target web`, with the installed CLI matching the `wasm-bindgen` crate version exactly. No `wasm-pack`, and no `wasm-opt`: Binaryen optimization belongs only to the native `curios` product.

**Rationale.** The two tools the build actually needs are the compiler and the bindings generator; a packager on top adds a second build system to version, cache, and debug without adding a capability. Keeping Binaryen out preserves the crate-boundary ownership of optimization, so the browser artifact is the pure pipeline's output, reproducible from the workspace toolchain alone.
