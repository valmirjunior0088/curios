//! A module compiled and run in this process: what this crate's suites and `curios-prelude`'s run a program by, where a shipped build runs one from its `.cwasm` alone.

use {
    crate::compile::optimize,
    curios_abi::{ForeignStore, HostOps},
    curios_native::{Compiled, ForeignBindings, GuestMap, start_engine},
};

/// Optimize (Binaryen) and compile (Cranelift) a module to the program this process runs, on `curios-native`'s shared engine: [`to_cwasm`](crate::to_cwasm)'s machine code, held rather than written out, so running it reads nothing back. `foreigns` is the rows the compilation handed back beside `module`, which the program carries to its link.
pub fn to_compiled(
    module: &curios_wasm::Module,
    foreigns: ForeignStore,
) -> Result<Compiled, String> {
    // A suite runs no `main`, so it starts the engine as every test does, with no map for a sampler, before the optimizer validates against it.
    start_engine(GuestMap::Unwritten)?;
    let bytes = optimize(module);

    let code = curios_cranelift::compile(&bytes)
        .map_err(|error| format!("failed to compile module: {error}"))?;

    Ok(Compiled::of(code, foreigns))
}

/// Compile a module and run it in-process on the shared runtime engine. `foreigns` is the rows the compilation handed back beside it, and `bindings` answers rows ahead of the host, the program's own `foreign` declarations among them (pass [`ForeignBindings::empty`] where the host answers every row). Returns the process exit code.
pub fn run_wasm<H: HostOps + Send + Sync + 'static>(
    module: &curios_wasm::Module,
    foreigns: ForeignStore,
    host: H,
    bindings: ForeignBindings,
) -> Result<u8, String> {
    to_compiled(module, foreigns)?
        .run(host, bindings)
        .map_err(|stop| stop.to_string())
}
