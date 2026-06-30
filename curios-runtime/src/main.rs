//! The launcher stub. A bundled curios executable is this binary with a `.cwasm`
//! payload appended to its tail (see `curios-compiler`'s `compile` subcommand).
//! At startup it reads its own image, slices off the trailing payload, and runs
//! it on the runtime-only engine.

use {
    curios_runtime::{OsHost, run_bytes},
    std::{
        env, fs,
        process::{self, ExitCode},
    },
};

/// Footer appended after the `.cwasm` payload: `payload ++ (len: u64 LE) ++ MAGIC`.
/// Kept in sync with the bundler in `curios-compiler`.
const MAGIC: &[u8; 8] = b"CRSEXEC1";
const FOOTER_LEN: usize = 16;

/// Recover the appended `.cwasm` payload from this executable's own tail.
fn payload() -> Result<Vec<u8>, String> {
    let exe =
        env::current_exe().map_err(|error| format!("cannot locate own executable: {error}"))?;

    let image =
        fs::read(&exe).map_err(|error| format!("cannot read {}: {error}", exe.display()))?;

    if image.len() < FOOTER_LEN {
        return Err("not a bundled curios executable (no payload)".into());
    }

    let (body, footer) = image.split_at(image.len() - FOOTER_LEN);
    let (len_bytes, magic) = footer.split_at(8);

    if magic != MAGIC {
        return Err("not a bundled curios executable (bad footer)".into());
    }

    let len = u64::from_le_bytes(len_bytes.try_into().unwrap()) as usize;

    if len > body.len() {
        return Err("corrupt curios executable (payload length exceeds image)".into());
    }

    Ok(body[body.len() - len..].to_vec())
}

fn main() -> ExitCode {
    // argv crosses to the guest via `/std/Proc/args`; argv[0] is this executable.
    let args = env::args().map(String::into_bytes).collect();

    match payload().and_then(|payload| run_bytes(&payload, OsHost::with_args(args))) {
        Ok(0) => ExitCode::SUCCESS,
        Ok(code) => process::exit(code),
        Err(error) => {
            eprintln!("{error}");

            ExitCode::FAILURE
        }
    }
}
