# curios-cranelift

The compiling half of the Curios engine: Cranelift behind Wasmtime, on the engine `curios-runtime` configures. It precompiles a module to the `.cwasm` a bundle carries and a store files, compiles one to machine code a process holds, writes held machine code out, and describes the engine a payload is addressed under. The native compiler links it; the launcher never does.

## Decisions

### The backend is a crate beside the runtime

**Decision.** Everything that needs a compiler behind Wasmtime lives here, and `curios-runtime` has no feature that reaches one. The two take Wasmtime from one row of the workspace manifest, each naming the features it needs: the runtime the engine, this crate `cranelift` and `parallel-compilation`. What crosses between them is the engine, `shared_engine`, and `MachineCode`, the value a compiled module is handed over as and written out from; nothing above the pair names a wasmtime type.

**Rationale.** The launcher is `curios-runtime`'s binary and must link no backend. As a feature of that crate, the backend gated code inside it, so the launcher's own build was a configuration no workspace check compiled: a workspace build turns every feature of a shared crate on at once. As a crate, the launcher's has one configuration, and a build of it alone is the build every check already made. One row keeps the version single where two manifests name the dependency, as it does for `wasm-bindgen` beside its generator.

**Rejected.** The feature, for the reason above. Bytes as what crosses, a declared module precompiled here and read back there: every module would then pass the one operation whose safety rests on its caller, where a `MachineCode` made here passes none.

### Compilation runs across threads, and only where compilation exists

**Decision.** This crate takes Wasmtime's `parallel-compilation` beside the backend, so the native product compiles a module's functions across a thread pool, while the launcher, which never links this crate, links no pool: `cargo tree -p curios-runtime -e normal` names no `rayon`, as it names no `cranelift-codegen`.

**Rationale.** Wasmtime compiles each function on its own and joins the results in order, so the artifact does not depend on the thread count — `curios compile programs/monad_async.crs` writes the same executable byte for byte either way, and a payload's store address needs no new component. On a sixteen-thread machine in a debug build, that program's `precompile` span took 1.61–1.64 s serially and 0.38–0.42 s in parallel; retake it with `curios --profile <PATH> run programs/monad_async.crs` and the fold's `precompile` row.
