# curios-runtime

The runtime-only Curios engine: deserialize a precompiled `.cwasm` module and run it on embedded Wasmtime with the `sys.*` host imports, plus the slim launcher the native compiler embeds into bundled executables. Host bindings and the bundle payload format belong to the crate rustdoc.

## Design

### The launcher is slim by exclusion

**Decision.** This crate never depends on Cranelift or Binaryen — it deserializes, it cannot compile or optimize — and it pins the same workspace Wasmtime version as the compiler (see `curios/README.md`, "Distribution is ahead-of-time"). The launcher is built in isolation (`make curios/runtime`), outside workspace feature unification, and that isolated build is the only evidence the dependency graph stayed slim.

**Rationale.** Bundled-executable startup should do no compilation work, and slimness is a dependency-graph property: it holds because the capability is absent, not because a code path declines to use it. A workspace build cannot witness it, since feature unification can quietly pull a compiler backend into the graph — which is why a launcher produced by a workspace build is not evidence of anything.
