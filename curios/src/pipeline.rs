//! Driving the compile pipeline for the CLI, from an admitted program to the `.cwasm` payload both subcommands consume — including the store consultation that can skip the whole thing. Observing a stage is not here: that is a question about a program, and questions are `wonder`'s.
//!
//! **The payload, not the wasm module, is what this hands back**, and that is what lets one stored artifact serve `run` and `compile` alike: `run` executes it in-process exactly as it executes a fresh one, and `compile` appends it to the embedded launcher. Optimization and precompilation therefore happen here rather than in `main`, which is left dispatching.

use {
    crate::{Heading, Line, Subject, fact, processing},
    curios::{engine, to_cwasm},
    curios_abi::ForeignStore,
    curios_document::Documentation,
    curios_package::{Entry, Program},
    curios_pipeline::{Cache, CompileError, Fold, Progress},
    curios_text::{Entrypoint, Form, RootSource, UnitSource},
    curios_utilities::Source,
    curios_verdicts::Verdicts,
    curios_wasm::Module,
    curios_wonder::STDIN_LABEL,
    std::{fmt::Display, fs, io, path::Path, rc::Rc},
};

/// The precompiled payload for `program`, taken from `cache` when nothing it was made from has changed, and compiled — and filed there — otherwise. `cache` is the store its command opened for it to file into, and `None` compiles everything and files nothing.
///
/// The scope is the program's own dependency graph and nothing else: a manifest is the only thing that says what a unit is compiled against. The error keeps the incomplete/failure split so `main` can map a goal batch to its own exit code.
pub(crate) fn payload_of(
    budget: u64,
    program: Program,
    cache: Option<Verdicts>,
) -> Result<(Vec<u8>, ForeignStore), CompileError> {
    let subject = subject_of(&program);

    // A loose program has no project, so it has no store to consult: what a compilation may reuse is a fact about the project it is in, and a loose program is in none.
    let entry = match program.entry() {
        Entry::Stdin => None,
        Entry::File(path) => Some(path.clone()),
    };
    let declared = program
        .home()
        .map(|home| (home.package.clone(), home.executable.clone()));
    let declares = program.declares();
    let manifest = program.home().map(|home| home.manifest.clone());
    let scope = program.into_units();

    // Opened before the store is consulted, because the entry's own text is half of what a stored payload is verified against — and it has to be the text that was *parsed*, not a re-read taken afterwards.
    let (entrypoint, loader, source) = open(entry.as_deref())?;
    // What the manifest declared, onto the resolver the entry's own names go through. A loose program has no manifest, so it declares nothing and sees every open prefix — which is the whole scope it was given, since nothing mounted anything beside the prelude.
    let loader = match declares {
        Some(declares) => loader.declaring(declares),
        None => loader,
    };

    // A payload is filed only where all three exist: a store to put it in, a declared name to file it under, and an entry file to verify it against. Standard input has none of them, and reaches this as the `None` that skips both the get and the put.
    let filed = cache
        .as_ref()
        .zip(declared.as_ref())
        .zip(entry.as_deref())
        .map(|((cache, name), path)| {
            (
                cache,
                curios_verdicts::Program {
                    package: &name.0,
                    executable: &name.1,
                    entry: path,
                    text: &source.text,
                    loader: &loader,
                },
            )
        });

    // Built once and handed to both halves of the payload family, which is what keeps them agreeing about what the chain is: the probe refuses a unit the fold could not place, and the write must refuse the same one.
    let sources = scope.iter().map(UnitSource::mounted).collect::<Vec<_>>();

    if let Some((cache, program)) = &filed
        && let Some((payload, foreigns)) = cache.payload_get(program, &sources, engine())
    {
        // Announced after the store is consulted, exactly as the fold announces a reused unit: a reported operation is one that actually happened. The step names the target rather than a unit, because what came back is the whole program's machine code.
        processing(&subject, manifest.as_deref());
        let mut line = Line::nested(Heading::Compiling, &subject);
        line.outcome("reused");
        eprintln!();

        return Ok((payload, foreigns));
    }

    let compiled = compile_entry(
        budget,
        &scope,
        &entrypoint,
        &loader,
        &subject,
        manifest.as_deref(),
        cache.as_ref().map(|cache| cache as &dyn Cache),
    )
    .and_then(|(module, foreigns)| {
        to_cwasm(&module)
            .map(|payload| (payload, foreigns))
            .map_err(CompileError::failure)
    });

    if let (Ok((payload, foreigns)), Some((cache, program))) = (&compiled, &filed) {
        cache.payload_put(program, &sources, payload, foreigns, engine());
    }

    // After the fold rather than during it: one unwritable store refuses everything for the same reason, so this is one line however many units went past it. Reported even when the compilation failed, because a store nobody can write is true either way and the next run pays for it either way.
    if let Some(refusal) = cache.as_ref().and_then(Verdicts::refused) {
        fact(
            Heading::Skipped,
            format!("storing what this built; {refusal}"),
        );
    }

    compiled
}

/// The interface of the last of `units` — a package's library, compiled against everything before it — read off the compilation that builds it, and filed into `store` when there is one.
///
/// **A build, where the `wonder` engine's reading of the same record is a question.** A library documented is a library compiled, and what was compiled is worth what `run` and `test` keep of theirs; the engine's `documentation` reads the store and never writes it, which is what a question may do and a build has no reason to. The compilation runs to completion first, the kernel included, so a library that does not check is not documented and reports what stopped it exactly as `run` would.
pub(crate) fn documentation_of(
    budget: u64,
    units: &[RootSource],
    store: Option<&Verdicts>,
) -> Result<Documentation, CompileError> {
    let documented = Fold::new(budget, units, store.map(|store| store as &dyn Cache)).units(
        |_| {},
        |_, produced| {
            produced
                .last()
                .and_then(|unit| unit.text().documentation().cloned())
                .ok_or_else(|| {
                    CompileError::failure(
                        "nothing to document: the scope's last unit carries no interface"
                            .to_string(),
                    )
                })
        },
    );

    if let Some(refusal) = store.and_then(Verdicts::refused) {
        fact(
            Heading::Skipped,
            format!("storing what this built; {refusal}"),
        );
    }

    documented
}

/// What a program is reported as — the name that was asked for, never the file it resolved to.
///
/// A declared executable resolves to an absolute path somewhere under the governing root, and echoing that back fills a status line with what the reader already knew. A loose file *is* what was asked for, so it reports as written. Standard input was asked for as `-`, which reports as nothing a reader can act on, so it is the one subject named rather than echoed.
pub(crate) fn subject_of(program: &Program) -> Subject {
    match (program.entry(), program.home()) {
        (_, Some(home)) => Subject::Executable(home.executable.clone()),
        (Entry::File(path), None) => Subject::File(path.clone()),
        (Entry::Stdin, None) => Subject::Stdin,
    }
}

/// The entry program, what its own modules resolve against, and the text it was parsed from: a file when there is one, and otherwise standard input, drained to end.
///
/// Draining is why this is worth naming rather than inlining. The program's own standard input is gone once the compiler has read the source out of it, so `/std/read()` reports end-of-input — unavoidable when both want one descriptor, and the reason a program that reads its input belongs in a file.
pub(crate) fn open(
    entry: Option<&Path>,
) -> Result<(Entrypoint, RootSource, Rc<Source>), CompileError> {
    let Some(path) = entry else {
        return supplied(&drained()?);
    };

    Entrypoint::opened(path).map_err(|error| match fs::read_to_string(path) {
        // A text written as a module fails the program grammar at its end, where it was never meant to hold a term: say what it is rather than what the grammar expected there.
        Ok(text) if Form::of(path, &text) == Form::Module => written_as_a_module(path.display()),
        _ => CompileError::Failure(vec![error.report()]),
    })
}

/// Standard input, drained to end — once, since a second read finds nothing.
pub(crate) fn drained() -> Result<String, CompileError> {
    io::read_to_string(io::stdin())
        .map_err(|error| CompileError::failure(format!("failed to read standard input: {error}")))
}

/// The program `text`, arrived on standard input, parsed — and refused as [`open`] refuses a file when it is written as a module.
pub(crate) fn supplied(text: &str) -> Result<(Entrypoint, RootSource, Rc<Source>), CompileError> {
    Entrypoint::supplied(STDIN_LABEL, text).map_err(|error| {
        match Form::of(Path::new(STDIN_LABEL), text) {
            Form::Module => written_as_a_module(STDIN_LABEL),
            Form::Program => CompileError::Failure(vec![error.report()]),
        }
    })
}

/// What an entry written as a module, named `label`, is refused with.
fn written_as_a_module(label: impl Display) -> CompileError {
    CompileError::failure(format!(
        "{label} is written as a module, with no final term to compile a program from"
    ))
}

/// Compile `entrypoint` against `units` in the order given, narrating each step under a header that names `manifest` when it is not where the invocation stands.
pub(crate) fn compile_entry(
    budget: u64,
    units: &[RootSource],
    entrypoint: &Entrypoint,
    loader: &RootSource,
    subject: &Subject,
    manifest: Option<&Path>,
    cache: Option<&dyn Cache>,
) -> Result<(Module, ForeignStore), CompileError> {
    // Every target heads a group, since a compile and a handover always follow it. What the scope decides is whether the entry's own compile is a step of its own: with units to fold, those are the steps and the entry finishes among them unannounced; with none, the entry's compile is the one step there is.
    let has_units = !units.is_empty();
    processing(subject, manifest);

    // The entry is the one subject the fold cannot name — it owns the empty prefix — so it is reported under the name the caller was asked for.
    let mut line: Option<Line> = None;

    let compiled = Fold::new(budget, units, cache).compile(
        entrypoint,
        loader,
        |_| {},
        |progress| report(&mut line, subject, has_units, progress),
    );

    // A refusal is not a compiler dying mid-operation, which is the one case an unterminated line is left to mean: this one finished, and is about to say why. Closing the innermost line here is what lets the report below start at column zero, as every line of it after the first already does — and as the same `Report` renders through `wonder`.
    if compiled.is_err() && line.is_some() {
        eprintln!();
    }

    compiled
}

/// Fold one [`Progress`] event onto the open status line, opening and closing lines as subjects begin and end.
///
/// The line outlives each event, which is why it is threaded rather than owned here: `↳ Compiling /hello` and the `; 1.4s` that completes it are two separate writes to one line.
pub(crate) fn report(
    line: &mut Option<Line>,
    target: &Subject,
    has_units: bool,
    progress: Progress<'_>,
) {
    match progress {
        // A recompile over a baseline reads as a compile: what the line reports is that the unit is being worked on and how long it took, and the store this product hands the fold never offers a baseline anyway.
        Progress::Compiling(prefix) | Progress::Recompiling(prefix) => {
            *line = Some(Line::nested(
                Heading::Compiling,
                &Subject::Mounted(prefix.clone()),
            ));
        }
        // The entry program *is* the target the header named, so among unit steps its compile adds none of its own. With no units there is no other step, and this is it.
        Progress::Entry => {
            if !has_units {
                *line = Some(Line::nested(Heading::Compiling, target));
            }
        }
        Progress::Reused(prefix) => {
            Line::nested(Heading::Compiling, &Subject::Mounted(prefix.clone())).outcome("reused");
            eprintln!();
        }
        Progress::Compiled => {
            if let Some(mut open) = line.take() {
                open.done();
                eprintln!();
            }
        }
    }
}
