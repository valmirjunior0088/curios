//! The `curios` CLI. `run` obtains an entrypoint's precompiled payload and executes it in-process, forwarding the trailing arguments as the program's argv (the entry path, or `-`, as argv[0]) and its exit code as the process's; `compile` appends that same payload to the embedded launcher stub and writes a self-contained native executable. Both take the same target four ways — no argument for the governing package's default executable, an identifier for one it declares, a path for a bare `.crs` file, and `-` for the program on standard input — because what a bare invocation means inside a package should not depend on which subcommand asked.
//!
//! **Neither subcommand compiles unconditionally.** A manifest target's payload is filed in the project's store, so an invocation whose entry, whose entry's modules and whose dependencies are all unchanged is served from it — one slot for both subcommands, which is what makes `compile` after `run` a file write. Neither standalone form has a project, so neither consults anything. `pipeline` owns that decision and everything downstream of it.
//!
//! Argument parsing lives in `cli`, target resolution, compilation and payload reuse in `pipeline`, executable emission in `bundle` — this file only dispatches, mapping any error to stderr and a failure exit.

mod bundle;
use bundle::*;

mod cli;
use cli::*;

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
    curios_package::{Governing, LIBRARY, Target, curate, order, scaffold},
    curios_pipeline::CompileError,
    curios_runtime::{ForeignBindings, OsHost, run_bytes},
    curios_text::{Formatted, Overlay},
    curios_verdicts::Verdicts,
    curios_wonder::{
        Linted, archived_documentation, documentation, lint, serve, wonder_diagnostics,
        wonder_stage, wonder_tests,
    },
    std::{
        ffi::OsString,
        fs, iter,
        process::{self, ExitCode},
        time::Instant,
    },
};

#[cfg(feature = "profile")]
use curios_profile::{Destination, ProfileReport, fold, trace};

// Only the `profile` build installs it, so the ordinary CLI keeps the system allocator untouched and pays nothing for counters no mode would read.
#[cfg(feature = "profile")]
#[global_allocator]
static ALLOCATOR: curios_profile::CountingAllocator = curios_profile::CountingAllocator;

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
            CompileError::Failure(_) => Failure::Error(error.to_string()),
        }
    }
}

fn dispatch() -> Result<(), Failure> {
    let Cli {
        budget,
        units,
        manifest,
        mode,
    } = Cli::parse();

    match mode {
        Mode::Run { target, args } => {
            let target = Target::here(target.as_deref(), manifest.as_deref())?;
            // argv[0] is how the program was invoked, so a program on standard input passes on the `-` that invoked it rather than the name the compiler reports it by. Every argument crosses as the bytes the OS holds, since `/std/proc/args` promises opaque byte strings and a path or an argument need not be UTF-8.
            let entry = target.entry().map_or_else(
                || Target::STDIN.as_bytes().to_vec(),
                |path| path.as_os_str().as_encoded_bytes().to_vec(),
            );
            let subject = subject_of(&target);
            let cwasm = payload_of(budget, &units, target)?;

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
            if !run_tests(budget, &units, manifest.as_deref(), filter.as_deref())? {
                process::exit(1);
            }
        }
        // The tri-state `run` exits with, read off what was reported: a lint is as much a finding as an error, and a goal batch alone is the incomplete state it is everywhere else.
        Mode::Lint { target } => {
            match lint(budget, &units, manifest.as_deref(), target.as_deref())? {
                Linted::Clean => {}
                Linted::Goals => process::exit(2),
                Linted::Findings => process::exit(1),
            }
        }
        Mode::Compile {
            target,
            output_path,
        } => {
            let target = Target::here(target.as_deref(), manifest.as_deref())?;

            // A product written to disk needs a package to be filed under and a name to be filed as, and only a declared executable has both. A loose file or standard input is `run`'s to take: trying a theory leaves nothing behind.
            let Target::Executable {
                entry,
                output: filed,
                ..
            } = &target
            else {
                return Err(Failure::Error(
                    "`compile` builds a declared executable of the governing package; `run` is what takes a file or standard input".to_string(),
                ));
            };
            let entry = entry.clone();
            let output = output_path.unwrap_or_else(|| filed.clone());

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
            let cwasm = payload_of(budget, &units, target)?;

            emit_exe(&cwasm, &output)?;

            // Where it landed rather than what it was called: that is the one fact a finished build is read for, and the group above already named the target twice. The time is the whole invocation's, payload and emission both, so it is measured here rather than by the line.
            let mut line = Line::open(Heading::Finished, &Subject::File(output));
            line.outcome(&format!("{:.1}s", started.elapsed().as_secs_f64()));
            eprintln!();
        }
        Mode::Document {
            target,
            output_path,
        } => {
            let (record, directory) = match target {
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
                    let governing = Governing::here(manifest.as_deref())?;
                    if !governing.directory.join(LIBRARY).is_file() {
                        return Err(Failure::Error(format!(
                            "{:?} declares no library, and a library is the one thing with an interface to document",
                            governing.package.name
                        )));
                    }

                    // The same scope `test` and `wonder` assemble: the `--unit` mounts in front, then the dependency graph with the governing package's own library last.
                    let mut scope = load_units(&units)?;
                    scope.extend(order(&governing)?);
                    let store = Verdicts::at(governing.root.clone());
                    let record = documentation(budget, scope, &Overlay::default(), Some(&store))?;
                    let directory = output_path.unwrap_or_else(|| {
                        governing.store().documentation(&governing.package.name)
                    });
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
            let governing = Governing::here(manifest.as_deref())?;

            // Past tense because it is: every round has fetched before the acquisitions come back to be reported.
            for acquisition in curate(&governing)? {
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
            Query::Diagnostics { target } => {
                wonder_diagnostics(budget, &units, manifest.as_deref(), target.as_deref())?
            }
            Query::Tests { target } => {
                wonder_tests(budget, &units, manifest.as_deref(), target.as_deref())?
            }
            // The one rung the engine hands back unrendered is Binaryen's, and this is the crate that links it.
            Query::Stage { name, target } => wonder_stage(
                budget,
                &units,
                manifest.as_deref(),
                &name,
                target.as_deref(),
                |module| wasm_optm(&module, |stage| println!("{stage}")),
            )?,
            Query::Server => serve(budget, &units, manifest.as_deref())?,
        },
        #[cfg(feature = "profile")]
        Mode::Profile {
            input_path,
            out,
            cap,
        } => {
            let scope = load_units(&units)?;
            let subject = Subject::File(input_path.clone());
            // Standard output is the default because the compile itself reports on standard error, so a stream can be piped or redirected without separating the two by hand.
            let destination = match &out {
                None => Destination::Stream(Box::new(std::io::stdout())),
                Some(path) => Destination::Rotating {
                    path: path.clone(),
                    cap,
                },
            };

            let compilation = trace(destination, || {
                compile_file(budget, scope, &input_path, &subject)
            })
            .map_err(|error| Failure::Error(format!("profile: {error}")))?;

            // A run that returned can be summarized; one that did not never reaches here, which is why the rows were written as they happened.
            if let Some(path) = &out {
                print!("{}", summarize(path)?.render());
            }

            compilation.map(|_| ())?;
        }
    }

    Ok(())
}

/// Fold a rotated stream back into its summaries: the discarded file's rows first, then the current file's.
///
/// Both are handed to one fold because each restates the callsite table at its head, so their concatenation is well defined. They are chained rather than concatenated in memory: the fold reads rows, and at the default cap the pair is a gigabyte. A missing `.prev` is the ordinary case of a run that never grew past one file.
#[cfg(feature = "profile")]
fn summarize(path: &std::path::Path) -> Result<ProfileReport, Failure> {
    use std::io::Read;

    let named = |error| Failure::Error(format!("{}: {error}", path.display()));
    let mut previous = path.to_path_buf().into_os_string();
    previous.push(".prev");

    let discarded: Box<dyn Read> = match fs::File::open(&previous) {
        Ok(file) => Box::new(file),
        Err(_) => Box::new(std::io::empty()),
    };
    let current = fs::File::open(path).map_err(named)?;

    fold(std::io::BufReader::new(discarded.chain(current))).map_err(named)
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
