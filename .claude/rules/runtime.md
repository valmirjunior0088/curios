---
paths:
  - "curios-runtime/**"
  - "curios-binaryen/**"
  - "curios/src/bundle.rs"
  - "curios/src/bundle/**"
  - "curios/tests/bundle.rs"
---

# The runtime, the launcher and Binaryen

- `curios` embeds the slim launcher with `include_bytes!`, built by `cargo xtask runtime` in its own Cargo invocation so feature unification keeps Cranelift and Binaryen out; a `curios-runtime` binary from a workspace build is no evidence the launcher is slim.
- `curios-runtime`'s default features are runtime-only; its `cranelift` feature exists for `curios` and never enters `default`, which `curios/src/bundle.rs` enforces on the shipped image. `curios` is the only crate combining Binaryen with Cranelift-enabled Wasmtime, names no wasmtime type, and reaches the runtime through `curios_runtime::validate` and `curios_runtime::precompile`. The Wasmtime pin lives in `curios-runtime/Cargo.toml` alone.
- A change to the runtime or the bundle format also reaches the launcher's dependency boundary and the bundle integration tests.
- Binaryen is built with CMake and a C++ toolchain in the build script's `OUT_DIR`, from a pinned source release verified in memory, and only its static library is kept, in the locked cache under `curios-binaryen/.artifacts/<triple>` that every Cargo mode shares. `build_schema` hashes `curios-binaryen/build.rs`, so any edit to it — a comment included — discards the cache.
