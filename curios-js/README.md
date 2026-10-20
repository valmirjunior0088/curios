# curios-js

The Curios compiler in the browser: the wasm-bindgen export of the pure compile pipeline, `compile`, in the bundle the playground loads, beside `curios-browser`'s `run`. Build steps belong to `xtask`.

## Design

### Plain cargo plus the bindings generator as a library

**Decision.** The browser build is `cargo xtask bundle build`: `cargo build` for wasm32, then `--target web` bindings generation. No `wasm-pack`, and no `wasm-opt`: Binaryen optimization belongs only to the native `curios` product.

**Rationale.** The build needs the compiler and the bindings generator, and a packager adds a second build system to version, cache and debug for no capability. Keeping Binaryen out keeps the browser artifact the pure pipeline's output, reproducible from the workspace toolchain alone.

### Compiling is this crate's, and running is `curios-browser`'s

**Decision.** This crate is the compiler in the browser and nothing else. `compile` hands back the module and the program's `foreign` rows as `curios-browser` renders them, and the bundle exports `curios-browser`'s `run` beside it, which this crate names so the bundle links it.

**Rationale.** Running a program under a JavaScript engine is one concern whoever compiled the program — this crate in the playground, the native compiler for a page — so one crate owns it, and this one depends on it rather than holding half of it.

**Rejected.** The run harness here, which the native compiler could not reach without linking this crate's exports; a `raw_module` import of a harness another crate holds, with the bundle step copying the file in beside the bindings.
