# curios-runtime

The runtime-only Curios engine: deserialize a precompiled `.cwasm` module and run it on embedded Wasmtime with the `sys.*` host imports, plus the slim launcher the native compiler embeds into bundled executables. Host bindings and the bundle payload format belong to the crate rustdoc.

## Design

### The launcher is slim by exclusion

**Decision.** This crate never depends on Binaryen, and by default it does not reach Cranelift — it deserializes, and compiling is an opt-in the native product turns on. It *owns* the Wasmtime pin: the version lives in this manifest and nothing else in the workspace names wasmtime, so the compiler that precompiles a `.cwasm` and the launcher that deserializes it cannot drift apart (see `curios/README.md`, "Distribution is ahead-of-time"). The launcher is built in isolation (`make curios/runtime`), outside workspace feature unification.

**What makes the slim launcher checkable rather than merely intended.** The isolated build used to be the *only* evidence, and evidence nobody re-derives decays — a workspace build even produces a same-named binary that carries Cranelift, so the wrong artifact is easy to inspect. The guards in `curios/src/bundle.rs` now scan the embedded image itself, and they were validated by building a Cranelift-linked launcher and watching both refuse it.

**Rationale.** Bundled-executable startup should do no compilation work, and slimness is a dependency-graph property: it holds because the capability is absent, not because a code path declines to use it. A workspace build cannot witness it, since feature unification can quietly pull a compiler backend into the graph — which is why a launcher produced by a workspace build is not evidence of anything.
