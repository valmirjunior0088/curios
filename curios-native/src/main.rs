//! The launcher stub. A bundled Curios executable is this binary with a program's payload, the modules answering its `foreign` declarations, and the record naming them appended to its tail (see `curios`'s `compile` subcommand). At startup it reads its own image, recovers that bundle, links the modules, and runs the program on the runtime-only engine.
//!
//! **Whatever stops it is said on one line**, the tag of the stop and what there is to say of it: a built executable has no compiler beside it to draw a report, and what it says is what the compiler's own `run` says of the same stop.

use {
    curios_native::{
        Bundle, Compiled, DeclaredModule, MachineCode, OsHost, Stop, extract_bundle,
        plugin_bindings,
    },
    std::{env, ffi::OsString, fs, process::ExitCode},
};

/// Recover the appended bundle from this executable's own tail. The tail format lives in `curios_native::bundle`, shared with the bundler.
fn bundle() -> Result<Bundle, Stop> {
    let exe = env::current_exe().map_err(|error| {
        Stop::of(
            "host failure",
            format_args!("this executable could not be located: {error}"),
        )
    })?;

    let image = fs::read(&exe).map_err(|error| {
        Stop::of(
            "host failure",
            format_args!("{} could not be read: {error}", exe.display()),
        )
    })?;

    extract_bundle(&image)
}

fn main() -> ExitCode {
    // argv crosses to the guest via `/std/Proc/args` as the bytes the OS handed over, argv[0] being this executable; `env::args` would panic on an argument that is not UTF-8, which the row promises to carry.
    let args = env::args_os().map(OsString::into_encoded_bytes).collect();

    let launch = || {
        let bundle = bundle()?;

        // Every module the bundle carries is machine code this engine emitted: nothing here compiles, which is what keeps a backend out of this binary. `plugin_bindings` still checks the coverage rule against the store, so a bundle whose modules and declarations disagree is refused here exactly as it would have been at `curios run`.
        let declared = bundle
            .plugins
            .into_iter()
            .map(|plugin| {
                // SAFETY: the payload is what this executable's own tail carries, written out by the compiler that embedded this launcher from the module it had resolved, past the point any other producer could substitute it. The wasmtime pin is one row, so the engine that wrote it and this one cannot drift, and wasmtime's own stamp inside the artifact stays the backstop.
                let code = unsafe { MachineCode::deserialize(&plugin.payload) }.map_err(|error| {
                    Stop::of(
                        "corrupt bundle",
                        format_args!(
                            "the module `{}` of `/{}` is no machine code this engine accepts: {error}",
                            plugin.name, plugin.package
                        ),
                    )
                })?;

                Ok(DeclaredModule {
                    package: plugin.package,
                    name: plugin.name,
                    code,
                    answers: plugin.answers,
                })
            })
            .collect::<Result<Vec<_>, Stop>>()?;

        let bindings = plugin_bindings(&bundle.foreigns, declared)?;

        // SAFETY: the payload is what this executable's own tail carries, appended by the compiler that embedded this launcher.
        let program = unsafe { Compiled::deserialize(&bundle.program, bundle.foreigns) }
            .map_err(|error| Stop::of("corrupt bundle", error))?;

        program.run(OsHost::with_args(args), bindings)
    };

    match launch() {
        Ok(code) => ExitCode::from(code),
        Err(stop) => {
            eprintln!("{stop}");

            ExitCode::FAILURE
        }
    }
}
