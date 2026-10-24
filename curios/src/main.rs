//! The `curios` CLI. `run` obtains an entrypoint's precompiled payload and executes it in-process, forwarding the trailing arguments as the program's argv (the entry path, or `-`, as argv[0]) and its exit code as the process's, or, with `--target browser`, serves the page it is built into on loopback, the arguments in the page's address; `compile` appends that same payload to the embedded launcher stub and writes a self-contained native executable, or, with `--target browser`, writes the module Binaryen produced beside the page that runs it. Both take the same subject four ways — no argument for the governing package's default executable, an identifier for one it declares, a path for a `.crs` file placed by what declares it, and `-` for the program on standard input — because what a bare invocation means inside a package should not depend on which subcommand asked.
//!
//! **Neither subcommand compiles unconditionally.** A declared executable's payload is filed in the project's store, so an invocation whose entry, whose entry's modules and whose dependencies are all unchanged is served from it — one slot for both subcommands, which is what makes `compile` after `run` a file write. A loose program has no project, so it consults nothing. `pipeline` owns that decision and everything downstream of it.
//!
//! Argument parsing lives in `cli`, what each command accepts and the admission of its argument in `contract`, compilation and payload reuse in `pipeline`, what answers a program's `foreign` rows in `link`, executable emission in `bundle`, a page's in `browser` and its serving in `serve` — this file only dispatches, mapping any error to stderr and a failure exit.

mod bundle;
use bundle::*;

mod cli;
use cli::*;

mod contract;
use contract::*;

mod link;
use link::*;

mod browser;
use browser::*;

mod pipeline;
use pipeline::*;

mod report;
use report::*;

mod serve;
use serve::*;

mod test_runner;
use test_runner::*;

use {
    curios::optimize,
    curios_cranelift::module_exports,
    curios_document::write_documentation,
    curios_native::{Bundle, Compiled, OsHost},
    curios_package::{Entry, Pinning, Repoint, Spelling, Store, Target, curate, pin, scaffold},
    curios_pipeline::CompileError,
    curios_text::Formatted,
    curios_utilities::Source,
    curios_wonder::{
        Linted, STDIN_LABEL, Staged, lint, serve, std_documentation, wonder_at, wonder_cost,
        wonder_declarations, wonder_diagnostics, wonder_stage, wonder_tests,
    },
    std::{
        collections::BTreeSet,
        ffi::OsString,
        fs, io, iter,
        path::{Path, PathBuf},
        process::{self, ExitCode},
        time::Instant,
    },
};

#[cfg(feature = "profile")]
use curios_profile::{Destination, fold_at, install};

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
    if let Err(error) = install(Destination::File(path)) {
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

/// What a `pin` subcommand asks for: which table, which row, where it should point, and whether to write it.
///
/// The exclusivity is clap's — the delivery flags are one required group, and the ones that cannot stand together say so — so what is left here is reading which of them was given. A dependency fetched with no revision to pin, which `requires` already refuses for `--url`, is `curios_package::pin`'s to refuse for a caller that reaches it another way.
fn asked(row: Pinned) -> (curios_package::Subject, String, Repoint, bool) {
    let repoint = |path: Option<PathBuf>,
                   url: Option<String>,
                   rev: Option<String>,
                   refresh: bool| match (path, url, rev, refresh) {
        (Some(path), ..) => Repoint::Carried(path),
        (None, Some(url), rev, _) => Repoint::Fetched { url, rev },
        (None, None, Some(rev), _) => Repoint::Revised(rev),
        (None, None, None, _) => Repoint::Refresh,
    };

    match row {
        Pinned::Foreign {
            name,
            target,
            path,
            url,
            refresh,
            check,
            ..
        } => (
            curios_package::Subject::Foreign(target.0),
            name,
            repoint(path, url, None, refresh),
            check.check,
        ),
        Pinned::Dependency {
            name,
            path,
            url,
            rev,
            refresh,
            check,
            ..
        } => (
            curios_package::Subject::Dependency,
            name,
            repoint(path, url, rev, refresh),
            check.check,
        ),
    }
}

fn dispatch() -> Result<(), Failure> {
    let cli = Cli::parse();

    // After parsing, because the destination is one of the arguments; before dispatch, because everything worth measuring is downstream of it.
    #[cfg(feature = "profile")]
    if let Some(path) = cli.profile.clone() {
        install_profiling(path);
    }

    let Cli { mode, .. } = cli;

    // Read before the match takes the command apart, so every arm admits its argument through the one contract its command has.
    let contract = mode.contract();
    let subject = mode.subject().map(str::to_owned);
    let subject = subject.as_deref();
    let manifest = mode.manifest().map(Path::to_path_buf);
    let manifest = manifest.as_deref();
    let destination = mode.output().map(Path::to_path_buf);
    let destination = destination.as_deref();

    match mode {
        Mode::Run {
            target: TargetValue(target),
            port,
            elaboration,
            args,
            ..
        } => {
            if target == Target::Native && port.is_some() {
                return Err(Failure::Error(
                    "`--port` is where a page is served, and `--target native` runs the program here: `--target browser` serves its page".to_string(),
                ));
            }

            let program = contract.admit_program(subject, manifest, None, &here()?)?;

            // The program's arguments reach it through the page's address, and its name is its first.
            if target == Target::Browser {
                let store = program.home().and_then(|home| {
                    let store = contract.access.filed(&home.root)?;

                    Some(store.executing(&home.package, &home.executable))
                });
                let (name, files) = built(elaboration.budget, program, store)?;
                let listener = bound(port.unwrap_or(PORT))?;
                let port = listener
                    .local_addr()
                    .map_err(|error| format!("cannot read the address served on: {error}"))?
                    .port();
                let served = Served { name, files };

                serving(&served.url(port, &args));
                serve_page(&listener, served)?;

                return Ok(());
            }

            // argv[0] is how the program was invoked, so a program on standard input passes on the `-` that invoked it rather than the name the compiler reports it by. Every argument crosses as the bytes the OS holds, since `/std/Process/args` promises opaque byte strings and a path or an argument need not be UTF-8.
            let entry = match program.entry() {
                Entry::Stdin => Spelling::STDIN.as_bytes().to_vec(),
                Entry::File(path) => path.as_os_str().as_encoded_bytes().to_vec(),
            };
            let subject = subject_of(&program);
            let store = program.home().and_then(|home| {
                let store = contract.access.filed(&home.root)?;

                Some(store.executing(&home.package, &home.executable))
            });
            // Kept before the program is consumed: binding needs the manifest that declares it, and `payload_of` takes the program by value.
            let manifest = program.home().map(|home| home.manifest.clone());
            let (cwasm, foreigns) = payload_of(elaboration.budget, program, store, Target::Native)?;
            let bindings = bindings_of(manifest.as_deref(), &foreigns)?;

            step(Heading::Running, &subject);

            // SAFETY: `payload_of` precompiled the payload in this process or read it back from the project's own store, where a compilation of this compiler filed it.
            let compiled = unsafe { Compiled::deserialize(&cwasm, foreigns) }?;
            let code = compiled.run(
                OsHost::with_args(
                    iter::once(entry)
                        .chain(args.into_iter().map(OsString::into_encoded_bytes))
                        .collect(),
                ),
                bindings,
            )?;

            if code != 0 {
                process::exit(i32::from(code));
            }
        }
        // Exit 1 on any failing, trapping or exiting test, exactly as a failing compile exits 1 and a goal batch exits 2 — 0 means every selected test passed.
        Mode::Test {
            filter,
            elaboration,
            ..
        } => {
            let selection = contract.admit_any(subject, manifest, &here()?)?;

            if !run_tests(elaboration.budget, selection, contract.access, &filter)? {
                process::exit(1);
            }
        }
        // The tri-state `run` exits with, read off what was reported: a lint is as much a finding as an error, and a goal batch alone is the incomplete state it is everywhere else.
        Mode::Lint { elaboration, .. } => match lint(
            elaboration.budget,
            contract.admit_any(subject, manifest, &here()?)?,
            unfiled,
        )? {
            Linted::Clean => {}
            Linted::Goals => process::exit(2),
            Linted::Findings => process::exit(1),
        },
        Mode::Compile {
            target: TargetValue(target),
            output_path,
            elaboration,
            ..
        } => {
            let program = contract.admit_program(subject, manifest, destination, &here()?)?;

            // Admission refuses a program with nowhere to go, so one no package declares came with `--output`.
            let owned = output_path.is_none();
            let output = match (output_path, program.home()) {
                (Some(output), _) => output,
                (None, Some(home)) => home.built(target),
                (None, None) => unreachable!(
                    "`compile` admits a program no package declares only with `--output`"
                ),
            };
            let store = program.home().and_then(|home| {
                let store = contract.access.filed(&home.root)?;

                Some(store.executing(&home.package, &home.executable))
            });
            // Kept before the program is consumed: resolving the declared modules needs the manifest that declares it, and `payload_of` takes the program by value.
            let manifest = program.home().map(|home| home.manifest.clone());

            match target {
                Target::Native => {
                    // `-o` can name the entry itself. Refuse before compiling rather than destroy the source.
                    if let Entry::File(entry) = program.entry()
                        && let (Ok(input), Ok(written)) =
                            (entry.canonicalize(), output.canonicalize())
                        && input == written
                    {
                        return Err(Failure::Error(format!(
                            "refusing to overwrite the input {}",
                            entry.display()
                        )));
                    }
                    // A directory takes no executable's place, and the write would say so only after the whole compilation.
                    if output.is_dir() {
                        return Err(Failure::Error(format!(
                            "{} is a directory; `--output` names the file to write",
                            output.display()
                        )));
                    }
                }
                // A file takes no page's place — the entry among them — and the write would say so only after the whole compilation.
                Target::Browser => {
                    if output.is_file() {
                        return Err(Failure::Error(format!(
                            "{} is a file; `--output` names the directory the page is written in",
                            output.display()
                        )));
                    }
                }
            }

            let started = Instant::now();

            match target {
                Target::Native => {
                    let (payload, foreigns) =
                        payload_of(elaboration.budget, program, store, target)?;

                    // Linked here as a run links, entering nothing: the executable's launcher has these modules and its host and no other answer, so what it would refuse at every start is refused before the executable is written.
                    let bindings = bindings_of(manifest.as_deref(), &foreigns)?;
                    bindings.check(&foreigns)?;

                    // Written out here, where Cranelift is, so the launcher only ever deserializes: that is what lets a program declaring `foreign` reach a machine with no compiler, no manifest and no sources, carrying the modules answering it inside its own image.
                    let plugins = bindings.bundled(curios_cranelift::serialize)?;

                    emit_exe(
                        &Bundle {
                            program: payload,
                            foreigns,
                            plugins,
                        },
                        &output,
                    )?;
                }
                Target::Browser => {
                    let (_, files) = built(elaboration.budget, program, store)?;

                    emit_page(&output, &files, owned)?;
                }
            }

            // Where it landed rather than what it was called: that is the one fact a finished build is read for, and the group above already named the subject twice. The time is the whole invocation's, payload and emission both, so it is measured here rather than by the line.
            let mut line = Line::open(Heading::Finished, &Subject::File(output));
            line.outcome(&format!("{:.1}s", started.elapsed().as_secs_f64()));
            eprintln!();
        }
        Mode::Document {
            std,
            output_path,
            elaboration,
            ..
        } => {
            let (record, directory) = match std {
                // The standard library has no package to file its pages under, so the flag names where they go rather than a directory being guessed.
                Some(directory) => (std_documentation()?, directory),
                None => {
                    let library = contract.admit_library(subject, manifest, &here()?)?;
                    let store = contract.access.filed(&library.root);
                    let record =
                        documentation_of(elaboration.budget, &library.units, store.as_ref())?;
                    let directory = output_path
                        .unwrap_or_else(|| Store::at(library.root).documentation(&library.package));
                    (record, directory)
                }
            };

            write_documentation(&record, &directory)
                .map_err(|error| format!("{}: {error}", directory.display()))?;

            // Where the pages landed, as `compile` ends on where its executable did: by default that is under the store, where nobody would look, and the landing page is the one file a reader opens.
            fact(
                Heading::Finished,
                Subject::File(directory.join("index.html")),
            );
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
        Mode::Curate { .. } => {
            let entire = contract.admit_entire(subject, manifest, &here()?)?;

            // Past tense because it is: every round has fetched before the acquisitions come back to be reported.
            let curated = curate(&entire.governing)?;
            for acquisition in curated.packages {
                fact(Heading::Fetched, Subject::package(&acquisition.name));
            }
            // After the packages, in the order they were brought in: a module is named by a manifest a tree had to arrive for.
            for acquisition in curated.modules {
                fact(Heading::Fetched, Subject::Module(acquisition.name));
            }
        }
        Mode::Pin { row } => {
            // Found without reading the manifest's rows, since the row a pin repairs may be one the manifest cannot be read with yet: what is written is what is checked.
            let pinning = Pinning::found(manifest, &here()?)?;
            let (subject, name, repoint, check) = asked(row);
            let pinned = pin(&pinning, subject, &name, &repoint, check)?;
            let named = Subject::Module(name);

            // Before what was written, because this is what the write is about to believe.
            if let Some(probe) = &pinned.probe {
                fact(
                    Heading::Delivered,
                    format!("{named}: {} bytes, {}", probe.bytes, probe.kind),
                );

                // What the row's `[foreign.answers]` may name as an export, read by compiling the module — which `curios-package` cannot do and this can, since the runtime is here. A module that will not compile reports why instead, and that is the more useful half of the answer. Bytes with no magic are not put to it: the line above has already answered, and wasmtime would answer again by printing both magic numbers byte by byte.
                let read = |at| module_exports(&fs::read(at).map_err(|error| error.to_string())?);
                match probe.magic.then(|| read(&probe.at)) {
                    None => {}
                    Some(Ok(exports)) if exports.is_empty() => {
                        detail("exports: none — nothing here can answer a `foreign` declaration");
                    }
                    Some(Ok(exports)) => detail(format!("exports: {}", exports.join(", "))),
                    // Onto one line, because a report is one fact per line and an engine's diagnostics are written for a terminal of their own.
                    Some(Err(error)) => detail(format!(
                        "will not compile: {}",
                        error.split_whitespace().collect::<Vec<_>>().join(" ")
                    )),
                }

                // Only for a fetch. A module the package carries arrived inside the tree its consumer pinned; one that came over the wire is vouched for by nothing until this row says what it must hash to.
                if probe.fetched {
                    detail(
                        "nothing verified this download — the hash below is derived from it, not checked against it",
                    );
                }
            }

            match pinned.fields.is_empty() {
                // A row already saying what it was asked to say is the ordinary outcome of a second `pin`, and worth saying rather than passing in silence.
                true => fact(Heading::Pinned, format!("{named}; unchanged")),
                false => {
                    let mut line = Line::open(Heading::Pinned, &named);
                    line.outcome(match pinned.wrote {
                        true => "written",
                        false => "would change",
                    });

                    eprintln!();
                    for (key, before, after) in &pinned.fields {
                        match before {
                            Some(before) => detail(format!("{key}: {before} → {after}")),
                            None => detail(format!("{key}: {after}")),
                        }
                    }
                }
            }

            // `--check` is the question, and its answer is the exit code — `format --check`'s contract, for the same reason.
            if check && pinned.would_write() {
                process::exit(1);
            }
        }
        Mode::Format {
            subjects, check, ..
        } => {
            let directory = here()?;
            // No subject is every file the governing package declares, as no subject is the package entire for every command that takes one.
            let subjects = match subjects.is_empty() {
                true => vec![None],
                false => subjects
                    .iter()
                    .map(|subject| Some(subject.as_str()))
                    .collect(),
            };

            // The formatter is pure and reports changedness in its result; whether a `Changed` verdict fails the run (`--check`) or rewrites the file is this loop's policy. The formatter refuses internally when its output would not reparse to the same program, so nothing corrupt is ever written.
            let mut dirty = Vec::new();
            let mut rewritten = BTreeSet::new();
            for subject in subjects {
                match contract.admit_files(subject, manifest, &directory)? {
                    // Standard input is answered on standard output, whole, so a pipe gets the canonical text whether or not anything moved.
                    Rewritten::Stdin => {
                        let text = io::read_to_string(io::stdin())
                            .map_err(|error| format!("failed to read standard input: {error}"))?;
                        match Formatted::from_source(&Source::labelled(STDIN_LABEL, &text))? {
                            Formatted::Changed(_) if check => {
                                dirty.push(Spelling::STDIN.to_string())
                            }
                            Formatted::Unchanged(_) if check => {}
                            Formatted::Changed(text) | Formatted::Unchanged(text) => {
                                print!("{text}")
                            }
                        }
                    }
                    Rewritten::Files(paths) => {
                        // Two subjects naming one file rewrite it once.
                        for path in paths
                            .into_iter()
                            .filter(|path| rewritten.insert(path.clone()))
                        {
                            match Formatted::from_path(&path)? {
                                Formatted::Unchanged(_) => {}
                                Formatted::Changed(text) => match check {
                                    true => dirty.push(path.display().to_string()),
                                    false => fs::write(&path, text)
                                        .map_err(|error| format!("{}: {error}", path.display()))?,
                                },
                            }
                        }
                    }
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
            Query::Diagnostics { elaboration, .. } => wonder_diagnostics(
                elaboration.budget,
                contract.admit_any(subject, manifest, &here()?)?,
                unfiled,
            )?,
            Query::Tests { elaboration, .. } => wonder_tests(
                elaboration.budget,
                contract.admit_any(subject, manifest, &here()?)?,
                unfiled,
            )?,
            Query::Cost { elaboration, .. } => wonder_cost(
                elaboration.budget,
                contract.admit_program(subject, manifest, None, &here()?)?,
                unfiled,
            )?,
            // The one rung the engine hands back unrendered is Binaryen's, and this is the crate that links it.
            Query::Stage {
                name, elaboration, ..
            } => wonder_stage(
                elaboration.budget,
                &name,
                // A rung a unit has is a library's as well as a program's, and the contract the rung selected says which.
                match contract.accepts {
                    Accepts::ProgramOrLibrary => {
                        contract.admit_staged(subject, manifest, &here()?)?
                    }
                    _ => Staged::Program(contract.admit_program(
                        subject,
                        manifest,
                        None,
                        &here()?,
                    )?),
                },
                |module| {
                    optimize(&module, true, |stage| println!("{stage}"));
                },
                unfiled,
            )?,
            Query::At {
                place, elaboration, ..
            } => wonder_at(
                elaboration.budget,
                contract.admit_staged(subject, manifest, &here()?)?,
                place,
                unfiled,
            )?,
            Query::Declarations { elaboration, .. } => wonder_declarations(
                elaboration.budget,
                contract.admit_staged(subject, manifest, &here()?)?,
                unfiled,
            )?,
            Query::Server { elaboration } => serve(elaboration.budget, manifest)?,
        },
        // The path is taken as it was given: `--profile` spells no default, so the only path that exists is the one a caller chose, and reading it back means naming that one rather than deriving a second.
        #[cfg(feature = "profile")]
        Mode::Profile { stream } => {
            let report =
                fold_at(&stream).map_err(|error| format!("{}: {error}", stream.display()))?;

            print!("{}", report.render());
        }
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
