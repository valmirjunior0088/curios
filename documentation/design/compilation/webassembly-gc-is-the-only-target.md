# WebAssembly-GC is the only target

**Decision.** The pipeline emits Wasm-GC exclusively. Program values live in GC references, never linear memory, and one backend serves the native and browser products; a module `curios-emit` produces declares no memory. `curios-wasm` models the whole envelope's memory and table surface, since a representation that omits what the format has cannot encode a module that uses it, and the emitter uses part of it — a passive data segment behind every `array.new_data`, a typed table per closure arity — so nothing below `curios-emit` would refuse a value in linear memory: this decision is what enforces it.

**Rationale.** A functional, dependently typed language needs a garbage collector, and Wasm-GC inherits a production one instead of a hand-rolled runtime system. One backend yields both products, and portability comes with the ecosystem.

**Rejected.** Native code generation, and Wasm over linear memory with a shipped garbage collector.
