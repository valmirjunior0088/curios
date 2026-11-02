//! The prelude's build under a recorder: `profile-prelude` does what this crate's build script does and files the stream of it at `.artifacts/profile.tsv` beside this crate's manifest, then prints that stream's fold.
//!
//! **The program that is asked.** A build script takes no argument, so nothing can ask one for a stream, and one that recorded for carrying its instrumentation would record every build. This stands beside `build.rs` and shares `compile.rs` with it, so what is measured is the build's own work, every root lowered, elaborated, erased and certified; `cargo xtask profile-prelude` runs it.
//!
//! The images it builds go to a directory of its own, removed when it is done: the ones the compiler embeds are the build script's.

mod compile;
use compile::*;

// Held as the build script holds it, so `compile.rs` reads one crate root in both programs.
#[path = "src/std_module.rs"]
mod std_module;
use std_module::*;

#[path = "src/sys_module.rs"]
mod sys_module;
use sys_module::*;

use {
    curios_profile::{Destination, fold_at, trace},
    std::{
        env, fs,
        path::Path,
        process::{self, ExitCode},
    },
};

fn main() -> ExitCode {
    // Spelled here alone: this program both files the stream and folds it, so nothing else needs its place.
    let stream = Path::new(env!("CARGO_MANIFEST_DIR")).join(".artifacts/profile.tsv");

    match recorded(&stream) {
        Ok(()) => ExitCode::SUCCESS,
        Err(error) => {
            eprintln!("{error}");

            ExitCode::FAILURE
        }
    }
}

/// Compile the prelude under a stream filed at `stream`, and print what the stream folds to.
fn recorded(stream: &Path) -> Result<(), String> {
    let out = env::temp_dir().join(format!("curios-profile-prelude-{}", process::id()));
    fs::create_dir_all(&out).map_err(|error| format!("{}: {error}", out.display()))?;

    let traced = trace(Destination::File(stream.to_path_buf()), || {
        compile(&out);
    });
    // Removed whatever the run answered: the images were the work, and nothing reads them.
    let _ = fs::remove_dir_all(&out);
    traced.map_err(|error| format!("{}: {error}", stream.display()))?;

    let report = fold_at(stream).map_err(|error| format!("{}: {error}", stream.display()))?;
    print!("{}", report.render());
    println!("\nstream: {}", stream.display());

    Ok(())
}
