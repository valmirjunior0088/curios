# curios

The Curios native compiler: the compile-and-run helpers, the CLI, and the only workspace crate that links both native backends — Binaryen via `curios-binaryen` and Cranelift via Wasmtime. Runtime-only embedders stay on the slim `curios-runtime`, whose launcher this crate embeds into bundled executables. Public usage belongs to the repository [README](../README.md); local architecture belongs to the crate rustdoc.

## Design

### Distribution is ahead-of-time

**Decision.** The native product precompiles modules to `.cwasm` with Cranelift at build time and bundles them with the slim runtime launcher into standalone executables. The launcher deserializes and runs; it cannot compile. Other native distribution modes are not foreclosed; none is planned yet.

**Rationale.** User startup does no compilation work, the launcher stays slim precisely because Cranelift and Binaryen are excluded from it, and pinning the compiler and runtime to one Wasmtime version guarantees every `.cwasm` matches the engine that deserializes it.
