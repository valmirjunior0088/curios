# Curios owns the language, Rust owns the host

**Decision.** Rust owns the native host — Binaryen optimization, Wasmtime precompilation and execution, bundling, the CLI, and operating-system services — and the self-hosting objective claims only the language-specific stages. The frozen Rust baseline compiler remains as bootstrap seed and differential oracle.

**Rationale.** Self-hosting pays off exactly where the language defines itself — parsing through Wasm generation. Reimplementing Wasmtime, Binaryen, or host integration in Curios would add risk without teaching the language anything.

**Rejected.** Making the bootstrap objective reach past the Curios toolchain. Going further stays open for later — with a more robust FFI story, host components could link directly through WebAssembly imports and exports compiled from other languages — but for now, the main toolchain in Curios itself is the objective.
