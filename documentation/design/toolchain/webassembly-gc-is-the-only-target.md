# WebAssembly-GC is the only target

**Decision.** The pipeline emits Wasm-GC exclusively. Program values live in GC references, never linear memory, and the same backend serves the native and browser products.

**Rationale.** A functional dependently typed language needs a garbage collector, and targeting Wasm-GC inherits a production collector instead of hand-rolling a runtime system. One backend yields both products, and portability comes with the ecosystem. The mechanism — the symbolic module builder, and the emitter that puts every program value in a GC reference — belongs to `curios-wasm`'s and `curios-cont`'s rustdoc.

**This decision is the only thing enforcing it.** It used to be enforced twice: `curios-wasm`'s instruction roster held four byte-granular memory instructions and nothing else, so an emitter physically could not put a value in linear memory. That roster now models the whole envelope's memory and table surface, because a representation that omits what the format has cannot encode a module that uses it — and the pipeline wanted the segments, the tables, and the plural memories for their own sake. So the wall is gone and the rule is a rule: `curios-cont` chooses GC references, and nothing below it would refuse the alternative.

**Rejected.** Native code generation, and Wasm over linear memory with a shipped garbage collector.
