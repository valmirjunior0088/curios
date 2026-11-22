//! The `curios` CLI. `run` obtains an entrypoint's precompiled payload and executes it in-process, forwarding the trailing arguments as the program's argv (the entry path, or `-`, as argv[0]) and its exit code as the process's, or, with `--target browser`, serves the page it is built into on loopback, the arguments in the page's address; `compile` appends that same payload to the embedded launcher stub and writes a self-contained native executable, or, with `--target browser`, writes the module Binaryen produced beside the page that runs it. Both take the same subject four ways — no argument for the governing package's default executable, an identifier for one it declares, a path for a `.crs` file placed by what declares it, and `-` for the program on standard input — because what a bare invocation means inside a package should not depend on which subcommand asked.
//!
//! **Neither subcommand compiles unconditionally.** A declared executable's payload is filed in the project's cabinet, so an invocation whose entry, whose entry's modules and whose dependencies are all unchanged is served from it — one payload for both subcommands, which is what makes `compile` after `run` a file write. A loose program has no project, so it consults nothing. `pipeline` owns that decision and everything downstream of it.
//!
//! Argument parsing lives in `cli`, what each command accepts and the admission of its argument in `contract`, compilation and payload reuse in `pipeline`, what a test run shows in `test_runner`, executable emission in `bundle`, a page's in `browser` and its serving in `serve` — this file only dispatches, mapping any error to stderr and a failure exit. What answers a program's `foreign` rows and what a test run compiles and runs are the library's (`curios::bindings_of`, `curios::test_subjects`).

mod bundle;
use bundle::*;

mod cli;
use cli::*;

mod contract;
use contract::*;

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
    curios::{CABINET, bindings_of, fault, unlinked},
    curios_common::{Code, Kind, OsError, Report, Source, SourceName, Style},
    curios_cranelift::module_exports,
    curios_document::write_documentation,
    curios_native::{Bundle, Compiled, GuestMap, OsHost, Stop, start_engine},
    curios_package::{
        Entry, Fetch, Pinning, Products, Refused, Repoint, Spelling, Target, Transport, curate,
        pin, scaffold,
    },
    curios_pipeline::CompileError,
    curios_profile::{CountingAllocator, Destination, fold_at, install},
    curios_surface::Formatted,
    curios_wonder::{
        Linted, Scope, Staged, answer, lint, serve, std_documentation, wonder_at, wonder_classes,
        wonder_diagnostics, wonder_false, wonder_footprint, wonder_name, wonder_named_stage,
        wonder_stage, wonder_tests, wonder_witnesses,
    },
    std::{
        collections::BTreeSet,
        env,
        ffi::OsString,
        fs,
        io::{self, IsTerminal},
        iter,
        path::{Path, PathBuf},
        process::ExitCode,
    },
};

// The counting allocator, which counts only while `--profile` records: this binary is the program that reads what it counts.
#[global_allocator]
static ALLOCATOR: CountingAllocator = CountingAllocator;

/// File every span and event this invocation makes at `path`.
///
/// **Whatever the arguments asked for is what gets measured**, so `run`, `test`, `document` and a package build are all profileable — where a dedicated mode could only ever profile the one compilation it performed itself.
///
/// **The path is the caller's, and there is no default.** A compiler that chose one would spell a location the reader then has to spell again to read it back, and two derivations of one path are two things that can disagree. This way the reader chooses and the compiler obeys.
///
/// The stream is filed rather than piped because a hung run is the case profiling exists for, and a pipe dies with the interrupt that ends one. Standard output is also the *product* of most subcommands — a program's own output under `run`, an answer under `wonder` — so rows on it would corrupt the thing being profiled.
///
/// A failure to open is reported and the invocation proceeds. The alternative is refusing to compile because a measurement could not be filed, which inverts which of the two the caller asked for. Success says nothing: the caller named the path, so it has nothing to be told.
fn install_profiling(path: PathBuf) {
    if let Err(error) = install(Destination::File(path)) {
        said(format!("profiling unavailable: {error}"));
    }
}

/// The process-level failure split: a written-goal batch is incomplete development state (exit 2), everything else a hard error (exit 1). A command that could not get to its end hands `main` the report of why; one that got there and found something failed has said so itself, and hands over only how the process ends ([`Concluded`]).
enum Failure {
    /// A compile that stopped, with what it reported: rendered where it is printed, for whoever reads it there.
    Stopped(CompileError),
    /// What a command refuses before anything is compiled, as its report: rendered where it is printed, for whoever reads it there.
    Refused(Box<Report>),
    /// A program that could not go on, with how it stopped: said on one line, as the program built into an executable says it of itself.
    Halted(Stop),
    Concluded(Concluded),
}

/// How a command that ran to its end ends the process where something it found failed, having reported what itself: a test run its failing tests, `lint` its findings, `pin --check` what it would write. A running program's own exit code passes through untouched, so 0 always means "compiled, ran, and exited 0".
enum Concluded {
    /// Exit 2: written goals and nothing worse, as `lint` finds them.
    Incomplete,
    /// Exit 1: a test that did not pass, a lint, a row `pin --check` would change.
    Failed,
    /// The program's own exit code.
    Exited(u8),
}

impl From<Stop> for Failure {
    fn from(stop: Stop) -> Self {
        Failure::Halted(stop)
    }
}

impl From<Box<Report>> for Failure {
    fn from(report: Box<Report>) -> Self {
        Failure::Refused(report)
    }
}

impl From<CompileError> for Failure {
    fn from(error: CompileError) -> Self {
        Failure::Stopped(error)
    }
}

/// What a `pin` subcommand asks for: which table, which row, where it should point, and whether to write it.
///
/// The exclusivity is clap's — the delivery flags are one required group, and the ones that cannot stand together say so — so what is left here is reading which of them was given. A dependency fetched with no revision to pin, which `requires` already refuses for `--git`, is `curios_package::pin`'s to refuse for a caller that reaches it another way.
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
            git,
            rev,
            refresh,
            check,
            ..
        } => (
            curios_package::Subject::Dependency,
            name,
            repoint(path, git, rev, refresh),
            check.check,
        ),
    }
}

fn dispatch() -> Result<(), Failure> {
    let cli = match Cli::parse() {
        Ok(cli) => cli,
        // Help and the version are answers, printed to standard output; anything else is a line that does not parse, printed to standard error and failed as a command that could not run is.
        Err(error) => {
            return match error.use_stderr() {
                true => Err(refused(&error, &env::args_os().collect::<Vec<_>>()).into()),
                false => {
                    let _ = error.print();

                    Ok(())
                }
            };
        }
    };

    // Chosen first, before anything is written, as colour is: asked on the command line, else named in the environment, else read off where standard error goes. A name that spells no drawing is refused as a flag's value is.
    let drawing = Drawing::chosen(
        cli.progress.map(|DrawingValue(drawing)| drawing),
        env::var_os(PROGRESS).as_deref(),
        io::stderr().is_terminal(),
        env::var_os("TERM").as_deref(),
        env::var_os("CI").is_some(),
    )
    .map_err(|named| {
        Refused::of("invalid value")
            .stating(format_args!("written: `{named}`"))
            .stating(format_args!(
                "`{PROGRESS}` takes one of: {}",
                Drawing::ALL
                    .map(|drawing| Code(drawing.spelled()).to_string())
                    .join(", ")
            ))
            .about_named(PROGRESS)
    })?;
    transcribe(drawing);

    // After parsing, because the destination is one of the arguments; before dispatch, because everything worth measuring is downstream of it.
    // The guest's map is the other half of the same ask, written where a sampler looks for it.
    let map = match cli.profile.clone() {
        Some(path) => {
            install_profiling(path);
            GuestMap::Written
        }
        None => GuestMap::Unwritten,
    };
    // The first start of the process, so none is refused; the engine itself is built where a command first runs or compiles a module.
    start_engine(map).expect("the engine is started once, here, before any use");

    // Decided once, before anything is written: asked for or refused outright, and otherwise on where standard error goes and whether the reader has said `NO_COLOR`, which a value that is not empty says.
    let seen = |terminal: bool| match cli.color {
        Color::Always => true,
        Color::Never => false,
        Color::Auto => {
            terminal && env::var_os("NO_COLOR").is_none_or(|unpainted| unpainted.is_empty())
        }
    };
    let stood = env::current_dir().and_then(|here| here.canonicalize()).ok();
    // Each stream has a reader of its own: an answer redirected to a file is written no colour while the narration beside it, on a terminal, is.
    let answered = Style {
        here: stood.clone(),
        painted: seen(io::stdout().is_terminal()),
    };
    paint(
        Style {
            here: stood,
            painted: seen(io::stderr().is_terminal()),
        },
        answered.clone(),
    );

    let Cli { mode, .. } = cli;

    // Read before the match takes the command apart, so every arm admits its argument through the one contract its command has.
    let contract = mode.contract();
    let subject = mode.subject().map(str::to_owned);
    let subject = subject.as_deref();
    let manifest = mode.manifest().map(Path::to_path_buf);
    let manifest = manifest.as_deref();
    let destination = mode.output().map(Path::to_path_buf);
    let destination = destination.as_deref();
    // Read once, here, as `NO_COLOR` is, and handed to whatever opens a governing root (`curios::CABINET`).
    let cabinet = env::var_os(CABINET).map(PathBuf::from);
    let cabinet = cabinet.as_deref();
    // Drained by admission, where `-` is the subject and nowhere else, so what it held travels with what is admitted.
    let stdin = StandardInput {
        core: mode.core(),
        read: &drained,
    };

    match mode {
        Mode::Run {
            target: TargetValue(target),
            port,
            elaboration,
            args,
            ..
        } => {
            if target == Target::Native && port.is_some() {
                return Err(Refused::of("surplus flag")
                    .stating("`--port` is where a page is served")
                    .stating("`--target native` runs the program here")
                    .stating("write `--target browser` to serve its page")
                    .about_named("--port")
                    .into());
            }

            let program =
                contract.admit_program(subject, manifest, None, &here()?, cabinet, &stdin)?;
            let root = program.home().map(|home| home.root.clone());

            // The program's arguments reach it through the page's address, and its name is its first.
            if target == Target::Browser {
                let verdicts = program.home().and_then(|home| {
                    let verdicts = contract.access.filed(&home.cabinet)?;

                    Some(verdicts.executing(&home.package, &home.executable))
                });
                let page = built(elaboration.budget, program, verdicts, cabinet);
                contract.access.collect(root.as_deref(), cabinet);
                let (files, mut tree) = page?;
                let listener = bound(port.unwrap_or(PORT))?;
                let port = port_of(&listener)?;

                // The address is what the node came to, and the one fact a reader needs from what follows, which is nothing until the server is stopped.
                tree.stood(Step::Serve, true, url(port, &args), Vec::new());
                serve_page(&listener, Served { files })?;

                return Ok(());
            }

            // argv[0] is how the program was invoked, so a program on standard input passes on the `-` that invoked it rather than the name the compiler reports it by. Every argument crosses as the bytes the OS holds, since `/std/Proc/args` promises opaque byte strings and a path or an argument need not be UTF-8.
            let entry = match program.entry() {
                Entry::Stdin(_) => Spelling::STDIN.as_bytes().to_vec(),
                Entry::File(path) => path.as_os_str().as_encoded_bytes().to_vec(),
            };
            let verdicts = program.home().and_then(|home| {
                let verdicts = contract.access.filed(&home.cabinet)?;

                Some(verdicts.executing(&home.package, &home.executable))
            });
            // Kept before the program is consumed: binding needs the manifest that declares it, and `payload_of` takes the program by value.
            let manifest = program.home().map(|home| home.manifest.clone());
            let compiled = payload_of(
                elaboration.budget,
                program,
                verdicts,
                Target::Native,
                Then::Nothing,
            );
            contract.access.collect(root.as_deref(), cabinet);
            let Built {
                payload: cwasm,
                foreigns,
                tree,
            } = compiled?;
            let bindings = bindings_of(manifest.as_deref(), &foreigns, cabinet)?;
            // Linked here as the run links, entering nothing, so what could not be linked is the compiler's to refuse, at the row that declares it, before the program has taken a step.
            bindings
                .check(&foreigns)
                .map_err(|refusal| unlinked(refusal, manifest.as_deref(), cabinet))?;

            // Nothing announces the run: what follows is the program's own output, written past the transcript.
            tree.close();
            settle();

            // SAFETY: `payload_of` precompiled the payload in this process or read it back from the project's own cabinet, where a compilation of this compiler filed it.
            let compiled = unsafe { Compiled::deserialize(&cwasm, foreigns) }.map_err(|error| {
                fault("the program's machine code could not be read back", error)
            })?;
            let code = compiled.run(
                OsHost::with_args(
                    iter::once(entry)
                        .chain(args.into_iter().map(OsString::into_encoded_bytes))
                        .collect(),
                ),
                bindings,
            )?;

            if code != 0 {
                return Err(Failure::Concluded(Concluded::Exited(code)));
            }
        }
        // Exit 1 on any failing, trapping or exiting test, exactly as a failing compile exits 1 and a goal batch exits 2 — 0 means every selected test passed, natively or in whatever browser opened the served run.
        Mode::Test {
            target: TargetValue(target),
            port,
            filter,
            elaboration,
            ..
        } => {
            if target == Target::Native && port.is_some() {
                return Err(Refused::of("surplus flag")
                    .stating("`--port` is where a page is served")
                    .stating("`--target native` runs the tests here")
                    .stating("write `--target browser` to serve their pages")
                    .about_named("--port")
                    .into());
            }

            let selection = contract.admit_any(subject, manifest, &here()?, cabinet, &stdin)?;
            let root = selection.root().map(Path::to_path_buf);
            let green = match target {
                Target::Native => {
                    run_tests(elaboration.budget, selection, contract.access, &filter)
                        .map_err(Failure::from)
                }
                Target::Browser => serve_tests(
                    elaboration.budget,
                    selection,
                    contract.access,
                    &filter,
                    port.unwrap_or(PORT),
                ),
            };
            contract.access.collect(root.as_deref(), cabinet);

            if !green? {
                return Err(Failure::Concluded(Concluded::Failed));
            }
        }
        // The tri-state `run` exits with, read off what was reported: a lint is as much a finding as an error, and a goal batch alone is the incomplete state it is everywhere else.
        Mode::Lint { elaboration, .. } => {
            let selection = contract.admit_any(subject, manifest, &here()?, cabinet, &stdin)?;
            let root = selection.root().map(Path::to_path_buf);
            let linted = lint(
                elaboration.budget,
                selection,
                &mut Asking::checking(answered.clone()),
            );
            contract.access.collect(root.as_deref(), cabinet);
            let Linted {
                lints,
                errors,
                goals,
            } = linted?;
            // The count of lints always, since it is what the command is asked for, and of what else was reported where anything was.
            let counted = |count: usize, one: &str| match count {
                1 => format!("1 {one}"),
                count => format!("{count} {one}s"),
            };
            let beside = [
                (errors > 0).then(|| troubled(counted(errors, "error"))),
                (goals > 0).then(|| Piece::from(counted(goals, "goal"))),
            ];

            total(
                Piece::joined(
                    iter::once(Piece::from(counted(lints, "lint")))
                        .chain(beside.into_iter().flatten()),
                    ", ",
                ),
                None,
            );
            match (lints + errors, goals) {
                (0, 0) => {}
                (0, _) => return Err(Failure::Concluded(Concluded::Incomplete)),
                _ => return Err(Failure::Concluded(Concluded::Failed)),
            }
        }
        Mode::Compile {
            target: TargetValue(target),
            output_path,
            elaboration,
            ..
        } => {
            let program = contract.admit_program(
                subject,
                manifest,
                destination,
                &here()?,
                cabinet,
                &stdin,
            )?;
            let root = program.home().map(|home| home.root.clone());

            // Admission refuses a program with nowhere to go, so one no package declares came with `--output`.
            let owned = output_path.is_none();
            let output = match (output_path, program.home()) {
                (Some(output), _) => output,
                (None, Some(home)) => home.built(target),
                (None, None) => unreachable!(
                    "`compile` admits a program no package declares only with `--output`"
                ),
            };
            let verdicts = program.home().and_then(|home| {
                let verdicts = contract.access.filed(&home.cabinet)?;

                Some(verdicts.executing(&home.package, &home.executable))
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
                        return Err(Refused::of("output is the input")
                            .stating(format_args!(
                                "`--output` names {}, the file being compiled",
                                entry.display()
                            ))
                            .about_path(&output)
                            .into());
                    }
                    // A directory takes no executable's place, and the write would say so only after the whole compilation.
                    if output.is_dir() {
                        return Err(Refused::of("not a file")
                            .stating("`--output` names the file to write")
                            .stating("this path is a directory")
                            .about_path(&output)
                            .into());
                    }
                }
                // A file takes no page's place — the entry among them — and the write would say so only after the whole compilation.
                Target::Browser => {
                    if output.is_file() {
                        return Err(Refused::of("not a directory")
                            .stating("`--output` names the directory the page is written in")
                            .stating("this path is a file")
                            .about_path(&output)
                            .into());
                    }
                }
            }

            let (written, mut tree) = match target {
                Target::Native => {
                    let compiled =
                        payload_of(elaboration.budget, program, verdicts, target, Then::More);
                    contract.access.collect(root.as_deref(), cabinet);
                    let Built {
                        payload,
                        foreigns,
                        mut tree,
                    } = compiled?;

                    // Linked here as a run links, entering nothing: the executable's launcher has these modules and its host and no other answer, so what it would refuse at every start is refused before the executable is written.
                    let bindings = bindings_of(manifest.as_deref(), &foreigns, cabinet)?;
                    bindings
                        .check(&foreigns)
                        .map_err(|refusal| unlinked(refusal, manifest.as_deref(), cabinet))?;

                    // Written out here, where Cranelift is, so the launcher only ever deserializes: that is what lets a program declaring `foreign` reach a machine with no compiler, no manifest and no sources, carrying the modules answering it inside its own image.
                    let plugins = bindings
                        .bundled(curios_cranelift::serialize)
                        .map_err(|refusal| unlinked(refusal, manifest.as_deref(), cabinet))?;

                    tree.open(Step::Write, true);

                    let written = emit_exe(
                        &Bundle {
                            program: payload,
                            foreigns,
                            plugins,
                        },
                        &output,
                    );

                    (written, tree)
                }
                Target::Browser => {
                    let page = built(elaboration.budget, program, verdicts, cabinet);
                    contract.access.collect(root.as_deref(), cabinet);
                    let (files, mut tree) = page?;

                    tree.open(Step::Write, true);

                    (emit_page(&output, &files, owned), tree)
                }
            };

            // Where it landed rather than what it was called: that is the one fact a finished build is read for. A directory is told from a file by its trailing separator.
            let place = match target {
                Target::Native => from_here(&output),
                Target::Browser => format!("{}/", from_here(&output)),
            };

            tree.stands(written, place)?;
        }
        Mode::Document {
            std,
            output_path,
            elaboration,
            ..
        } => {
            let (record, directory, mut tree) = match std {
                // The standard library has no package to file its pages under, so the flag names where they go rather than a directory being guessed. Nothing is compiled for it, so its tree is its subject's alone.
                Some(directory) => {
                    let record = std_documentation()?;
                    let tree = Tree::of(&Subject::Mounted(record.prefix), None);

                    (record, directory, tree)
                }
                None => {
                    let library =
                        contract.admit_library(subject, manifest, &here()?, cabinet, &stdin)?;
                    let verdicts = contract.access.filed(&library.cabinet);
                    let documented = documentation_of(
                        elaboration.budget,
                        &library.units,
                        verdicts.as_ref(),
                        &Subject::package(&library.package),
                    );
                    contract.access.collect(Some(&library.root), cabinet);
                    let (record, tree) = documented?;
                    let directory = output_path.unwrap_or_else(|| {
                        Products::at(library.root).documentation(&library.package)
                    });
                    (record, directory, tree)
                }
            };

            tree.open(Step::Write, true);

            // Where the pages landed, as `compile` ends on where its executable did: by default that is under `.curios/`, where nobody would look, and the landing page is the one file a reader opens.
            tree.stands(
                write_documentation(&record, &directory).map_err(|error| {
                    Refused::of("unwritable directory")
                        .stating(OsError(&error))
                        .about_path(&directory)
                }),
                from_here(&directory.join("index.html")),
            )?;
        }
        Mode::New { directory } => {
            let written = scaffold(&directory)?;
            // The directory is the subject, and each file a node spelled from inside it, said once it is written; the last node is the command that runs what was written.
            let mut tree = Tree::of(&Subject::Directory(directory.clone()), None);

            for file in written.iter().filter(|written| !written.is_dir()) {
                let file = file.strip_prefix(&directory).unwrap_or(file).display();
                tree.stood(Step::Write, false, file, Vec::new());
            }
            tree.stood(
                Step::Try,
                true,
                format!("cd {} && curios run", directory.display()),
                Vec::new(),
            );
        }
        Mode::Curate { .. } => {
            let entire = contract.admit_entire(subject, manifest, &here()?, cabinet, &stdin)?;

            // The fetch in hand's tree, and whether its transport has delivered: each is a subject of its own, its one node open while it arrives.
            let mut fetching: Option<(Tree, bool)> = None;
            let begun = |subject: Subject| {
                let mut tree = Tree::of(&subject, None);
                tree.open(Step::Fetch, true);

                Some((tree, false))
            };
            let curated = curate(&entire.governing, |fetch| match fetch {
                Fetch::Package(acquisition) => {
                    fetching = begun(Subject::package(&acquisition.name));
                }
                // After the packages, in the order they are brought in: a module is named by a manifest a tree had to arrive for.
                Fetch::Module(acquisition) => {
                    fetching = begun(Subject::Module(acquisition.name.clone()));
                }
                Fetch::Arrived => {
                    if let Some((_, arrived)) = &mut fetching {
                        *arrived = true;
                    }
                }
                Fetch::Placed => {
                    if let Some((mut tree, _)) = fetching.take() {
                        tree.done();
                    }
                }
            });
            // A fetch that stopped closes its node with how: its transport failed where nothing arrived, and what arrived was refused where something did.
            if let (Err(_), Some((mut tree, arrived))) = (&curated, fetching) {
                tree.ending(match arrived {
                    true => "refused",
                    false => "failed",
                });
                apart();
            }
            let curated = curated?;

            total(
                format!("{} fetched", curated.packages.len() + curated.modules.len()),
                None,
            );
        }
        Mode::Pin { row } => {
            // Found without reading the manifest's rows, since the row a pin repairs may be one the manifest cannot be read with yet: what is written is what is checked.
            let pinning = Pinning::found(manifest, &here()?, cabinet)?;
            let (subject, name, repoint, check) = asked(row);
            let foreign = matches!(subject, curios_package::Subject::Foreign(_));
            let mut tree = Tree::of(&Subject::Module(name.clone()), None);
            let mut fetched = false;
            let pinned = pin(
                &pinning,
                subject,
                &name,
                &repoint,
                check,
                |delivery| match delivery {
                    // The line stands open while a delivery arrives: a module's on a node of its own, which what arrived then closes, and a dependency's, which nothing describes, on the pin's.
                    Transport::Asked => match foreign {
                        true => tree.open(Step::Deliver, false),
                        false => tree.open(Step::Pin, true),
                    },
                    Transport::Answered => fetched = true,
                },
            );
            // A pin that stopped with a node open closes it with how: the transport failed where nothing arrived, and a rule refused what did. One refused before any delivery began has no node to close.
            if pinned.is_err() && tree.is_open() {
                tree.ending(match fetched {
                    true => "refused",
                    false => "failed",
                });
                apart();
            }
            let pinned = pinned?;

            // Before what was written, because this is what the write is about to believe.
            if let Some(probe) = &pinned.probe {
                // What the provider's `answers` table may name as an export, read by compiling the module — which `curios-package` cannot do and this can, since the runtime is here. A module that will not compile reports why instead, and that is the more useful half of the answer. Bytes with no magic are not put to it: the delivery's line answers already, and wasmtime would answer again by printing both magic numbers byte by byte.
                let read = |at| module_exports(&fs::read(at).map_err(|error| error.to_string())?);
                let mut beneath = match probe.magic.then(|| read(&probe.at)) {
                    None => Vec::new(),
                    Some(Ok(exports)) if exports.is_empty() => vec![Piece::from(
                        "exports: none — nothing here can answer a `foreign` declaration",
                    )],
                    Some(Ok(exports)) => {
                        vec![Piece::from(format!("exports: {}", exports.join(", ")))]
                    }
                    // Onto one line, because a report is one fact per line and an engine's diagnostics are written for a terminal of their own.
                    Some(Err(error)) => vec![Piece::from(format!(
                        "will not compile: {}",
                        error.split_whitespace().collect::<Vec<_>>().join(" ")
                    ))],
                };

                // Only for a fetch. A module the package carries arrived inside the tree its consumer pinned; one that came over the wire is vouched for by nothing until this row says what it must hash to.
                if probe.fetched {
                    beneath.push(Piece::from(
                        "nothing verified this download — the hash below is derived from it, not checked against it",
                    ));
                }
                let delivered = format!("{} bytes, {}", grouped(probe.bytes), probe.kind);
                // A module that arrived closes the node it arrived on; a carried one was read where it lies, with nothing to wait for, and is said at once.
                match tree.is_open() {
                    true => tree.outcome_beneath(delivered, beneath),
                    false => tree.stood(Step::Deliver, false, delivered, beneath),
                }
            }

            let outcome = match (pinned.fields.is_empty(), pinned.wrote) {
                // A row already saying what it was asked to say is the ordinary outcome of a second `pin`, and worth saying rather than passing in silence.
                (true, _) => "unchanged",
                (false, true) => "recorded",
                (false, false) => "would change",
            };
            let fields = pinned
                .fields
                .iter()
                .map(|(key, before, after)| match before {
                    Some(before) => Piece::from(format!("{key}: {before} → {after}")),
                    None => Piece::from(format!("{key}: {after}")),
                })
                .collect();
            match tree.is_open() {
                true => tree.outcome_beneath(outcome, fields),
                false => tree.stood(Step::Pin, true, outcome, fields),
            }

            // `--check` is the question, and its answer is the exit code — `format --check`'s contract, for the same reason.
            if check && pinned.would_write() {
                return Err(Failure::Concluded(Concluded::Failed));
            }
        }
        Mode::Format {
            subjects,
            check,
            settings,
            ..
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
            // Every subject is admitted, and the width of each decided, before anything is written, so a refusal leaves every file as it was.
            let admitted = subjects
                .into_iter()
                .map(|subject| {
                    contract.admit_files(subject, manifest, &directory, &settings, cabinet, &stdin)
                })
                .collect::<Result<Vec<_>, _>>()?;

            // The formatter is pure and reports changedness in its result; whether a `Changed` answer fails the run (`--check`) or rewrites the file is this loop's policy. The formatter refuses internally when its output would not reparse to the same program, so nothing corrupt is ever written.
            //
            // A file that changes is a subject, its one node saying what became of it; one that does not is counted, and the count is the last line. Standard input formatted onto standard output is an answer, of which nothing is said.
            let (mut changed, mut unchanged, mut counted) = (0usize, 0usize, false);
            let mut rewritten = BTreeSet::new();
            let becomes = match check {
                true => "would change",
                false => "rewritten",
            };
            for admitted in admitted {
                match admitted {
                    // Standard input is answered on standard output, whole, so a pipe gets the canonical text whether or not anything moved.
                    Rewritten::Stdin(format, supplied) => {
                        let formatted = Formatted::from_source(
                            &Source::named(SourceName::stdin(), supplied.text.as_str()),
                            supplied.written,
                            format,
                        )?;

                        match (check, formatted) {
                            (true, Formatted::Changed(_)) => {
                                let mut tree = Tree::of(&Subject::Stdin, None);
                                tree.stood(Step::Format, true, becomes, Vec::new());
                                changed += 1;
                                counted = true;
                            }
                            (true, Formatted::Unchanged(_)) => {
                                unchanged += 1;
                                counted = true;
                            }
                            (false, Formatted::Changed(text) | Formatted::Unchanged(text)) => {
                                settle();
                                answer(text);
                            }
                        }
                    }
                    Rewritten::Files(paths, format) => {
                        counted = true;
                        // Two subjects naming one file rewrite it once.
                        for path in paths
                            .into_iter()
                            .filter(|path| rewritten.insert(path.clone()))
                        {
                            let subject = Subject::File(PathBuf::from(from_here(&path)));
                            let formatted =
                                Formatted::from_path(&path, format).inspect_err(|_| {
                                    // A file that cannot be formatted is refused, its tree said ahead of why.
                                    let mut tree = Tree::of(&subject, None);
                                    tree.stood(Step::Format, true, troubled("refused"), Vec::new());
                                    apart();
                                })?;

                            match formatted {
                                Formatted::Unchanged(_) => unchanged += 1,
                                Formatted::Changed(text) => {
                                    let mut tree = Tree::of(&subject, None);
                                    match check {
                                        // A check writes nothing, so its node is said at once.
                                        true => tree.stood(Step::Format, true, becomes, Vec::new()),
                                        false => {
                                            tree.open(Step::Format, true);
                                            let written = fs::write(&path, text).map_err(|error| {
                                                Refused::of("unwritable file")
                                                    .stating(OsError(&error))
                                                    .about_path(&path)
                                            });
                                            tree.stands(written, becomes)?;
                                        }
                                    }
                                    changed += 1;
                                }
                            }
                        }
                    }
                }
            }
            if counted {
                total(format!("{changed} {becomes}, {unchanged} unchanged"), None);
            }
            // `--check` is the question, and its answer is the exit code.
            if check && changed > 0 {
                return Err(Failure::Concluded(Concluded::Failed));
            }
        }
        Mode::Wonder {
            query, elaboration, ..
        } => {
            match query {
                Query::Check => wonder_diagnostics(
                    elaboration.budget,
                    contract.admit_any(subject, manifest, &here()?, cabinet, &stdin)?,
                    &mut Asking::checking(answered.clone()),
                )?,
                Query::Tests => wonder_tests(
                    elaboration.budget,
                    contract.admit_any(subject, manifest, &here()?, cabinet, &stdin)?,
                    &mut Asking::checking(answered.clone()),
                )?,
                Query::Stage(name) => {
                    let beside = wonder_stage(
                        elaboration.budget,
                        &name,
                        // A rung a unit has is a library's as well as a program's, and the contract the rung selected says which.
                        match contract.accepts {
                            Accepts::ProgramOrLibrary => contract.admit_staged(
                                subject,
                                manifest,
                                &here()?,
                                cabinet,
                                &stdin,
                            )?,
                            _ => Staged::Program(contract.admit_program(
                                subject,
                                manifest,
                                None,
                                &here()?,
                                cabinet,
                                &stdin,
                            )?),
                        },
                        &mut Asking::reading(answered.clone()),
                    )?;
                    // What stopped the compilation past the rung is said beside it, on standard error, so a pipeline reading the rung is unaffected.
                    for diagnosis in &beside {
                        said(Piece::rendered(|style| diagnosis.rendered(style)));
                    }
                }
                Query::At(place) => wonder_at(
                    elaboration.budget,
                    contract.admit_staged(subject, manifest, &here()?, cabinet, &stdin)?,
                    &place,
                    &mut Asking::reading(answered.clone()),
                )?,
                Query::Classes => wonder_classes(
                    elaboration.budget,
                    contract.admit_staged(subject, manifest, &here()?, cabinet, &stdin)?,
                    &mut Asking::reading(answered.clone()),
                )?,
                Query::Name(path, None) => wonder_name(
                    elaboration.budget,
                    contract.admit_scope(subject, manifest, &here()?, cabinet, &stdin)?,
                    &path,
                    subject,
                    &mut Asking::reading(answered.clone()),
                )?,
                Query::Name(path, Some(View::Witnesses)) => wonder_witnesses(
                    elaboration.budget,
                    contract.admit_scope(subject, manifest, &here()?, cabinet, &stdin)?,
                    &path,
                    &mut Asking::reading(answered.clone()),
                )?,
                Query::Name(path, Some(View::Footprint)) => wonder_footprint(
                    elaboration.budget,
                    contract.admit_program(subject, manifest, None, &here()?, cabinet, &stdin)?,
                    &path,
                    &mut Asking::reading(answered.clone()),
                )?,
                // A unit's rung takes any scope `--name` takes, and a rung past them a program, as the contract the rung selected says.
                Query::Name(path, Some(View::Stage(name))) => wonder_named_stage(
                    elaboration.budget,
                    match contract.accepts {
                        Accepts::Program => Scope::Program(contract.admit_program(
                            subject,
                            manifest,
                            None,
                            &here()?,
                            cabinet,
                            &stdin,
                        )?),
                        _ => contract.admit_scope(subject, manifest, &here()?, cabinet, &stdin)?,
                    },
                    &path,
                    &name,
                    &mut Asking::reading(answered.clone()),
                )?,
                Query::False => wonder_false(
                    elaboration.budget,
                    contract.admit_program(subject, manifest, None, &here()?, cabinet, &stdin)?,
                    &mut Asking::reading(answered.clone()),
                )?,
                Query::Server => serve(elaboration.budget, manifest, cabinet).map_err(|words| {
                    Refused::of("server failure")
                        .stating(words)
                        .about_named("--server")
                })?,
            }
        }
        // The path is taken as it was given: `--profile` spells no default, so the only path that exists is the one a caller chose, and reading it back means naming that one rather than deriving a second.
        Mode::Profile { stream } => {
            let report = fold_at(&stream).map_err(|error| {
                Refused::of("unreadable file")
                    .stating(OsError(&error))
                    .about_path(&stream)
            })?;

            settle();
            answer(report.render());
        }
    }

    Ok(())
}

/// The one place the process exits: the report of a command that could not get to its end, every [`Failure`]'s code, and the transcript's end, whatever the command came to.
fn main() -> ExitCode {
    let code = exited(dispatch());
    finish();

    code
}

/// The report of what stopped a command, where something did, and the code the process exits with.
fn exited(dispatched: Result<(), Failure>) -> ExitCode {
    match dispatched {
        Ok(()) => ExitCode::SUCCESS,
        Err(Failure::Stopped(stopped)) => {
            said(Piece::rendered(|style| stopped.rendered(style)));

            // Written goals alone are the incomplete state; anything refused beside them is a failure.
            match stopped {
                CompileError::Incomplete(_) => ExitCode::from(2),
                CompileError::Failure(_) | CompileError::Mixed { .. } => ExitCode::FAILURE,
            }
        }
        Err(Failure::Refused(report)) => {
            said(Piece::rendered(|style| report.rendered(Kind::Error, style)));

            ExitCode::FAILURE
        }
        Err(Failure::Halted(stop)) => {
            said(stop.to_string());

            ExitCode::FAILURE
        }
        Err(Failure::Concluded(Concluded::Incomplete)) => ExitCode::from(2),
        Err(Failure::Concluded(Concluded::Failed)) => ExitCode::FAILURE,
        Err(Failure::Concluded(Concluded::Exited(code))) => ExitCode::from(code),
    }
}
