//! The launcher stub. A bundled Curios executable is this binary with a program's payload, the modules answering its `foreign` declarations, and the record naming them appended to its tail (see `curios`'s `compile` subcommand). At startup it reads its own image, recovers that bundle, links the modules, and runs the program on the runtime-only engine.

use {
    curios_runtime::{
        Bundle, DeclaredModule, ModuleBytes, OsHost, extract_bundle, plugin_bindings, run_bytes,
    },
    std::{
        env,
        ffi::OsString,
        fs,
        process::{self, ExitCode},
    },
};

/// Recover the appended bundle from this executable's own tail. The tail format lives in `curios_runtime::bundle`, shared with the bundler.
fn bundle() -> Result<Bundle, String> {
    let exe =
        env::current_exe().map_err(|error| format!("cannot locate own executable: {error}"))?;

    let image =
        fs::read(&exe).map_err(|error| format!("cannot read {}: {error}", exe.display()))?;

    extract_bundle(&image)
}

fn main() -> ExitCode {
    // argv crosses to the guest via `/std/proc/args` as the bytes the OS handed over, argv[0] being this executable; `env::args` would panic on an argument that is not UTF-8, which the row promises to carry.
    let args = env::args_os().map(OsString::into_encoded_bytes).collect();

    let launch = || {
        let bundle = bundle()?;

        // Every module the bundle carries is machine code this engine emitted: nothing here compiles, which is what keeps a backend out of this binary. `plugin_bindings` still checks the coverage rule against the store, so a bundle whose modules and declarations disagree is refused here exactly as it would have been at `curios run`.
        let declared = bundle
            .plugins
            .into_iter()
            .map(|plugin| DeclaredModule {
                package: plugin.package,
                name: plugin.name,
                bytes: ModuleBytes::Precompiled(plugin.payload),
                exports: plugin.exports,
            })
            .collect();

        let bindings = plugin_bindings(bundle.foreigns, declared)?;

        // SAFETY: the payload is what this executable's own tail carries, appended by the compiler that embedded this launcher.
        unsafe { run_bytes(&bundle.program, OsHost::with_args(args), bindings) }
    };

    match launch() {
        Ok(0) => ExitCode::SUCCESS,
        Ok(code) => process::exit(code),
        Err(error) => {
            eprintln!("{error}");

            ExitCode::FAILURE
        }
    }
}
