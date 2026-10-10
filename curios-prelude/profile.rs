//! The prelude's certification under a recorder: `profile-certification <PATH>` does what this crate's build script does and files the stream of it at `PATH`, then prints that stream's fold.
//!
//! **The program that is asked.** A build script takes no argument, so nothing can ask one for a stream, and one that recorded for carrying its instrumentation would record every build. This stands beside `build.rs` and shares `certify.rs` with it, so what is measured is the build's own work, over the images the build made; `cargo xtask profile-prelude certification <PATH>` runs it.
//!
//! The records it files go to a directory of its own, removed when it is done: the ones the compiler reads are the build script's.

mod certify;

use {
    certify::certify,
    curios_profile::{Destination, fold_at, trace},
    std::{
        env, fs,
        path::{Path, PathBuf},
        process::{self, ExitCode},
    },
};

fn main() -> ExitCode {
    let Some(stream) = env::args_os().nth(1).map(PathBuf::from) else {
        eprintln!("usage: profile-certification <PATH>");

        return ExitCode::FAILURE;
    };

    match recorded(&stream) {
        Ok(()) => ExitCode::SUCCESS,
        Err(error) => {
            eprintln!("{error}");

            ExitCode::FAILURE
        }
    }
}

/// Certify the prelude under a stream filed at `stream`, and print what the stream folds to.
fn recorded(stream: &Path) -> Result<(), String> {
    let out = env::temp_dir().join(format!("curios-profile-certification-{}", process::id()));
    fs::create_dir_all(&out).map_err(|error| format!("{}: {error}", out.display()))?;

    let traced = trace(Destination::File(stream.to_path_buf()), || certify(&out));
    // Removed whatever the run answered: the records were the work, and nothing reads them.
    let _ = fs::remove_dir_all(&out);
    traced.map_err(|error| format!("{}: {error}", stream.display()))?;

    let report = fold_at(stream).map_err(|error| format!("{}: {error}", stream.display()))?;
    print!("{}", report.render());
    println!("\nstream: {}", stream.display());

    Ok(())
}
