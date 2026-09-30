---
paths:
  - "**/Cargo.toml"
  - "**/build.rs"
  - "rust-toolchain.toml"
  - ".cargo/**"
---

# Manifests, build scripts and build products

- Data flows down the pipeline and Rust dependencies point up it: a lowering depends on the representation it constructs.
- Crate boundaries, not Cargo features, separate the compiler, runtime and browser products. An external dependency with a surface worth governing is named in one manifest, whose crate is its authority.
- `rust-toolchain.toml`'s floor is Wasmtime's; bump it when the pin in `curios-runtime/Cargo.toml` moves past it.
- A build product that outlives its build lives in `.artifacts/` beside its owner, never under `target/`; `cargo clean` leaves it, so delete one by hand to force a rebuild. Generated `.wasm` and other build products are never committed; `Cargo.lock` is.
- `target/debug/incremental` is rustc cache, safe to delete when no build runs.
