//! The launcher stub. A bundled Curios executable is this binary with a `.cwasm`
//! payload appended to its tail (see `curios`'s `compile` subcommand).
//! At startup it reads its own image, slices off the trailing payload, and runs
//! it on the runtime-only engine.

use {
    curios_runtime::{ForeignBindings, OsHost, extract_payload, run_bytes},
    std::{
        env, fs,
        process::{self, ExitCode},
    },
};

/// Recover the appended `.cwasm` payload from this executable's own tail. The
/// footer format lives in `curios_runtime::bundle`, shared with the bundler.
fn payload() -> Result<Vec<u8>, String> {
    let exe =
        env::current_exe().map_err(|error| format!("cannot locate own executable: {error}"))?;

    let image =
        fs::read(&exe).map_err(|error| format!("cannot read {}: {error}", exe.display()))?;

    extract_payload(&image)
}

fn main() -> ExitCode {
    // argv crosses to the guest via `/std/proc/args`; argv[0] is this executable.
    let args = env::args().map(String::into_bytes).collect();

    match payload()
        .and_then(|payload| run_bytes(&payload, OsHost::with_args(args), ForeignBindings::empty()))
    {
        Ok(0) => ExitCode::SUCCESS,
        Ok(code) => process::exit(code),
        Err(error) => {
            eprintln!("{error}");

            ExitCode::FAILURE
        }
    }
}
