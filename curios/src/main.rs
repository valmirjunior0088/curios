//! The `curios` CLI. `run` obtains an entrypoint's precompiled payload and executes it in-process, forwarding the trailing arguments as the program's argv (the entry path, or `-`, as argv[0]) and its exit code as the process's; `compile` appends that same payload to the embedded launcher stub and writes a self-contained native executable. Both take the same target four ways — no argument for the governing package's default executable, an identifier for one it declares, a path for a bare `.crs` file, and `-` for the program on standard input — because what a bare invocation means inside a package should not depend on which subcommand asked.
//!
//! **Neither subcommand compiles unconditionally.** A manifest target's payload is filed in the project's store, so an invocation whose entry, whose entry's modules and whose dependencies are all unchanged is served from it — one slot for both subcommands, which is what makes `compile` after `run` a file write. Neither standalone form has a project, so neither consults anything. `pipeline` owns that decision and everything downstream of it.
//!
//! Argument parsing lives in `cli`, what each command accepts and the admission of its argument in `contract`, compilation and payload reuse in `pipeline`, executable emission in `bundle` — this file only dispatches, mapping any error to stderr and a failure exit.

mod bundle;
use bundle::*;

mod cli;
use cli::*;

mod contract;
use contract::*;

mod pipeline;
use pipeline::*;

mod report;
use report::*;

mod test_runner;
use test_runner::*;

use {
    clap::Parser,
    curios::wasm_optm,
    curios_document::write_documentation,
    curios_package::{Entry, Spelling, Store, curate, scaffold},
    curios_pipeline::CompileError,
    curios_runtime::{ForeignBindings, OsHost, run_bytes},
    curios_text::{Formatted, Overlay},
    curios_wonder::{
        Linted, archived_documentation, documentation, lint, serve, wonder_cost,
        wonder_diagnostics, wonder_stage, wonder_tests,
    },
    std::{
        ffi::OsString,
        fs, iter,
        process::{self, ExitCode},
        time::Instant,
    },
};

#[cfg(feature = "profile")]
use {
    curios_profile::{Destination, install},
    std::path::PathBuf,
};

// Only the `profile` build installs it, so the ordinary CLI keeps the system allocator untouched and pays nothing for counters no mode would read.
#[cfg(feature = "profile")]
#[global_allocator]
static ALLOCATOR: curios_profile::CountingAllocator = curios_profile::CountingAllocator;

/// The size at which the record stream rotates, keeping the current file and its predecessor. A constant rather than a second flag: it bounds what an endless run may write, which is not a thing a caller has a reason to choose, and the destination is the only part of a measurement that is the caller's.
#[cfg(feature = "profile")]
const PROFILE_CAP: u64 = 512 * 1024 * 1024;

/// File every span and event this invocation makes at `path`.
///
/// **Whatever the arguments asked for is what gets measured**, so `run`, `test`, `document` and a package build are all profileable — where a dedicated mode could only ever profile the one compilation it performed itself.
///
/// **The path is the caller's, and there is no default.** A compiler that chose one would spell a location the reader then has to spell again to read it back, and two derivations of one path are two things that can disagree. This way the reader chooses and the compiler obeys.
///
/// The stream is filed rather than piped because a hung run is the case profiling exists for, and a pipe dies with the interrupt that ends one. Standard output is also the *product* of most subcommands — a program's own output under `run`, an answer under `wonder` — so rows on it would corrupt the thing being profiled.
///
/// A failure to open is reported and the invocation proceeds. The alternative is refusing to compile because a measurement could not be filed, which inverts which of the two the caller asked for. Success says nothing: the caller named the path, so it has nothing to be told.
#[cfg(feature = "profile")]
fn install_profiling(path: PathBuf) {
    let destination = Destination::Rotating {
        path,
        cap: PROFILE_CAP,
    };

    if let Err(error) = install(destination) {
        eprintln!("profiling unavailable: {error}");
    }
}

/// The process-level failure split: a written-goal batch is incomplete development state (exit 2), everything else a hard error (exit 1). A running program's own exit code passes through untouched, so 0 always means "compiled, ran, and exited 0".
enum Failure {
    Incomplete(String),
    Error(String),
}

impl From<String> for Failure {
    fn from(message: String) -> Self {
        Failure::Error(message)
    }
}

impl From<CompileError> for Failure {
    fn from(error: CompileError) -> Self {
        match error {
            CompileError::Incomplete(_) => Failure::Incomplete(error.to_string()),
            CompileError::Failure(_) | CompileError::Mixed { .. } => {
                Failure::Error(error.to_string())
            }
        }
    }
}

fn dispatch() -> Result<(), Failure> {
    let cli = Cli::parse();

    // After parsing, because the destination is one of the arguments; before dispatch, because everything worth measuring is downstream of it.
    #[cfg(feature = "profile")]
    if let Some(path) = cli.profile.clone() {
        install_profiling(path);
    }

    let Cli {
        budget,
        manifest,
        mode,
        ..
    } = cli;
    let manifest = manifest.as_deref();

    // Read before the match takes the command apart, so every arm admits its argument through the one contract its command has.
    let contract = mode.contract();
    let target = mode.target().map(str::to_owned);
    let target = target.as_deref();

    match mode {
        Mode::Run { args, .. } => {
            let program = contract.admit_program(target, manifest, &here()?)?;
            // argv[0] is how the program was invoked, so a program on standard input passes on the `-` that invoked it rather than the name the compiler reports it by. Every argument crosses as the bytes the OS holds, since `/std/proc/args` promises opaque byte strings and a path or an argument need not be UTF-8.
            let entry = match program.entry() {
                Entry::Stdin => Spelling::STDIN.as_bytes().to_vec(),
                Entry::File(path) => path.as_os_str().as_encoded_bytes().to_vec(),
            };
            let subject = subject_of(&program);
            let store = program
                .home()
                .and_then(|home| contract.access.filed(&home.root));
            let cwasm = payload_of(budget, program, store)?;

            step(Heading::Running, &subject);

            // SAFETY: `payload_of` precompiled the payload in this process or read it back from the project's own store, where a compilation of this compiler filed it.
            let code = unsafe {
                run_bytes(
                    &cwasm,
                    OsHost::with_args(
                        iter::once(entry)
                            .chain(args.into_iter().map(OsString::into_encoded_bytes))
                            .collect(),
                    ),
                    ForeignBindings::empty(),
                )
            }?;

            if code != 0 {
                process::exit(code);
            }
        }
        // Exit 1 on any failing, trapping or exiting test, exactly as a failing compile exits 1 and a goal batch exits 2 — 0 means every selected test passed or proved.
        Mode::Test { filter } => {
            let entire = contract.admit_entire(target, manifest, &here()?)?;

            if !run_tests(budget, entire, contract.access, filter.as_deref())? {
                process::exit(1);
            }
        }
        // The tri-state `run` exits with, read off what was reported: a lint is as much a finding as an error, and a goal batch alone is the incomplete state it is everywhere else.
        Mode::Lint { .. } => match lint(budget, contract.admit_any(target, manifest, &here()?)?)? {
            Linted::Clean => {}
            Linted::Goals => process::exit(2),
            Linted::Findings => process::exit(1),
        },
        Mode::Compile { output_path, .. } => {
            let program = contract.admit_program(target, manifest, &here()?)?;

            // Admission refuses a program no package declares, since the executable is filed under the package that declares it — so this one has an entry file and a home.
            let (Entry::File(entry), Some(home)) = (program.entry(), program.home()) else {
                unreachable!("`compile` admits only a declared executable");
            };
            let entry = entry.clone();
            let output = output_path.unwrap_or_else(|| home.output.clone());
            let store = contract.access.filed(&home.root);

            // `-o` can name the entry itself. Refuse before compiling rather than destroy the source.
            if let (Ok(input), Ok(written)) = (entry.canonicalize(), output.canonicalize())
                && input == written
            {
                return Err(Failure::Error(format!(
                    "refusing to overwrite the input {}",
                    entry.display()
                )));
            }

            let started = Instant::now();
            let cwasm = payload_of(budget, program, store)?;

            emit_exe(&cwasm, &output)?;

            // Where it landed rather than what it was called: that is the one fact a finished build is read for, and the group above already named the target twice. The time is the whole invocation's, payload and emission both, so it is measured here rather than by the line.
            let mut line = Line::open(Heading::Finished, &Subject::File(output));
            line.outcome(&format!("{:.1}s", started.elapsed().as_secs_f64()));
            eprintln!();
        }
        Mode::Document {
            target: archive,
            output_path,
        } => {
            let (record, directory) = match archive {
                // A unit already archived has no package to file its pages under, so the directory is asked for rather than guessed.
                Some(path) => {
                    let Some(directory) = output_path else {
                        return Err(Failure::Error(format!(
                            "{}: an archived unit has no store to file its pages under; say where with `--output`",
                            path.display()
                        )));
                    };
                    (archived_documentation(&path)?, directory)
                }
                None => {
                    let library = contract.admit_library(target, manifest, &here()?)?;
                    let store = contract.access.consulted(&library.root);
                    let record =
                        documentation(budget, library.units, &Overlay::default(), store.as_ref())?;
                    let directory = output_path
                        .unwrap_or_else(|| Store::at(library.root).documentation(&library.package));
                    (record, directory)
                }
            };

            write_documentation(&record, &directory)
                .map_err(|error| format!("{}: {error}", directory.display()))?;
        }
        Mode::New { directory } => {
            for written in scaffold(&directory)? {
                // A trailing separator is what tells a reader the first line is the directory the other two landed in.
                match written.is_dir() {
                    true => fact(Heading::Created, format!("{}/", written.display())),
                    false => fact(Heading::Created, written.display()),
                }
            }

            fact(
                Heading::Try,
                format!("cd {} && curios run", directory.display()),
            );
        }
        Mode::Curate => {
            let entire = contract.admit_entire(target, manifest, &here()?)?;

            // Past tense because it is: every round has fetched before the acquisitions come back to be reported.
            for acquisition in curate(&entire.governing)? {
                fact(Heading::Fetched, Subject::package(&acquisition.name));
            }
        }
        Mode::Format { paths, check } => {
            // The formatter is pure and reports changedness in its result; whether a `Changed` verdict fails the run (`--check`) or rewrites the file is this loop's policy. The formatter refuses internally when its output would not reparse to the same program, so nothing corrupt is ever written.
            let mut dirty = Vec::new();
            for path in &paths {
                match Formatted::from_path(path)? {
                    Formatted::Unchanged(_) => {}
                    Formatted::Changed(text) => match check {
                        true => dirty.push(path.display().to_string()),
                        false => fs::write(path, text)
                            .map_err(|error| format!("{}: {error}", path.display()))?,
                    },
                }
            }
            if !dirty.is_empty() {
                return Err(Failure::Error(format!(
                    "would reformat: {}",
                    dirty.join(", ")
                )));
            }
        }
        Mode::Wonder { query } => match query {
            Query::Diagnostics { .. } => {
                wonder_diagnostics(budget, contract.admit_any(target, manifest, &here()?)?)?
            }
            Query::Tests { .. } => {
                wonder_tests(budget, contract.admit_any(target, manifest, &here()?)?)?
            }
            Query::Cost { .. } => {
                wonder_cost(budget, contract.admit_program(target, manifest, &here()?)?)?
            }
            // The one rung the engine hands back unrendered is Binaryen's, and this is the crate that links it.
            Query::Stage { name, .. } => wonder_stage(
                budget,
                &name,
                contract.admit_program(target, manifest, &here()?)?,
                |module| wasm_optm(&module, |stage| println!("{stage}")),
            )?,
            Query::Server => serve(budget, manifest)?,
        },
    }

    Ok(())
}

fn main() -> ExitCode {
    match dispatch() {
        Ok(()) => ExitCode::SUCCESS,
        Err(Failure::Incomplete(report)) => {
            eprintln!("{report}");

            ExitCode::from(2)
        }
        Err(Failure::Error(error)) => {
            eprintln!("{error}");

            ExitCode::FAILURE
        }
    }
}
