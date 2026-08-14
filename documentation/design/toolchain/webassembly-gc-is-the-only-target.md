# WebAssembly-GC is the only target

**Decision.** The pipeline emits Wasm-GC exclusively. Program values live in GC references, never linear memory, and the same backend serves the native and browser products.

**Rationale.** A functional dependently typed language needs a garbage collector, and targeting Wasm-GC inherits a production collector instead of hand-rolling a runtime system. One backend yields both products, and portability comes with the ecosystem. The mechanism — the symbolic module builder and the GC-only, memory-less instruction roster — belongs to `curios-wasm`'s rustdoc.

**Rejected.** Native code generation, and Wasm over linear memory with a shipped garbage collector.
