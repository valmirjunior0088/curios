---
paths:
  - "curios-runtime/**"
  - "curios-cranelift/**"
  - "curios-binaryen/**"
  - "curios/src/bundle.rs"
  - "curios/src/bundle/**"
  - "curios/tests/bundle.rs"
---

# The runtime, the launcher, the backend and Binaryen

- `curios` embeds the slim launcher with `include_bytes!`, built by `cargo xtask launcher build` in its own Cargo invocation so feature unification keeps Cranelift and Binaryen out; a `curios-runtime` binary from a workspace build is no evidence the launcher is slim.
- `curios-runtime` has no feature that reaches a compiler: everything that needs Cranelift is `curios-cranelift`'s, which `curios` and the suites link and the launcher never does, and `curios/src/bundle.rs` holds that on the shipped image. `curios` is the only crate combining Binaryen with Cranelift, names no wasmtime type, and reaches the engine through `curios_runtime::validate` and `curios_cranelift`'s `precompile` and `compile`. Wasmtime's version is one `[workspace.dependencies]` row the two crates take.
- A module reaches the runtime as a `MachineCode`, which only an engine makes: compiled by `curios-cranelift`, or read back through `MachineCode::deserialize`, the one `unsafe` door, called where the bytes' provenance is known — the launcher's footer and the store's read-backs. A new path for machine code goes through one of the two, never through bytes a safe function takes.
- A change to the runtime or the bundle format also reaches the launcher's dependency boundary and the bundle integration tests.
- Binaryen is built with CMake and a C++ toolchain in the build script's `OUT_DIR`, from a pinned source release verified in memory, and only its static library is kept, in the locked cache under `curios-binaryen/.artifacts/<triple>` that every Cargo mode shares. `build_schema` hashes `curios-binaryen/build.rs`, so any edit to it — a comment included — discards the cache.
