//! Driving the compile pipeline for the CLI, from a resolved target to the `.cwasm` payload both subcommands consume — including the store consultation that can skip the whole thing. Observing a stage is not here: that is a question about a program, and questions are `wonder`'s.
//!
//! **The payload, not the wasm module, is what this hands back**, and that is what lets one stored artifact serve `run` and `compile` alike: `run` executes it in-process exactly as it executes a fresh one, and `compile` appends it to the embedded launcher. Optimization and precompilation therefore happen here rather than in `main`, which is left dispatching.

use {
    crate::{Heading, Line, Subject, fact},
    curios::STDIN_LABEL,
    curios::{Program, Verdicts, to_cwasm},
    curios_package::Target,
    curios_pipeline::{Cache, CompileError, Progress, compile_with_units},
    curios_text::{Entrypoint, RootSource, UnitSource},
    curios_utilities::Source,
    curios_wasm::Module,
    std::{
        io,
        path::{Path, PathBuf},
        rc::Rc,
    },
};

/// The precompiled payload for `target`, taken from the store when nothing it was made from has changed and compiled otherwise.
///
/// A `--unit` package is the already-resolved form of a manifest entry, so it goes in front of the graph's own order: the order arguments arrive in *is* dependency order. The error keeps the incomplete/failure split so `main` can map a goal batch to its own exit code.
pub(crate) fn payload_of(
    budget: u64,
    units: &[PathBuf],
    target: Target,
) -> Result<Vec<u8>, CompileError> {
    let mut scope = load_units(units)?;
    let subject = subject_of(&target);

    // Neither standalone form has a project, so neither has a store to consult: what a compilation may reuse is a fact about the project it is in, and these are in none.
    let (entry, declared, cache) = match target {
        Target::Stdin => (None, None, None),
        Target::File(path) => (Some(path), None, None),
        Target::Executable {
            entry,
            units,
            root,
            package,
            name,
            ..
        } => {
            scope.extend(units);

            (Some(entry), Some((package, name)), Some(Verdicts::at(root)))
        }
    };

    // Opened before the store is consulted, because the entry's own text is half of what a stored payload is verified against — and it has to be the text that was *parsed*, not a re-read taken afterwards.
    let (entrypoint, loader, source) = open(entry.as_deref())?;

    // A payload is filed only where all three exist: a store to put it in, a declared name to file it under, and an entry file to verify it against. Standard input has none of them, and reaches this as the `None` that skips both the get and the put.
    let filed = cache
        .as_ref()
        .zip(declared.as_ref())
        .zip(entry.as_deref())
        .map(|((cache, name), path)| {
            (
                cache,
                Program {
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
        && let Some(payload) = cache.payload_get(program, &sources)
    {
        // Announced after the store is consulted, exactly as the fold announces a reused unit: a reported operation is one that actually happened. The step names the target rather than a unit, because what came back is the whole program's machine code.
        fact(Heading::Processing, &subject);
        let mut line = Line::nested(Heading::Compiling, &subject);
        line.outcome("reused");
        eprintln!();

        return Ok(payload);
    }

    let compiled = compile_entry(
        budget,
        &scope,
        &entrypoint,
        &loader,
        &subject,
        cache.as_ref().map(|cache| cache as &dyn Cache),
    )
    .and_then(|module| to_cwasm(&module).map_err(CompileError::failure));

    if let (Ok(payload), Some((cache, program))) = (&compiled, &filed) {
        cache.payload_put(program, &sources, payload);
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

/// What a target is reported as — the name that was asked for, never the file it resolved to.
///
/// A declared executable resolves to an absolute path somewhere under the governing root, and echoing that back fills a status line with what the reader already knew. A bare file *is* what was asked for, so it reports as written. Standard input was asked for as `-`, which reports as nothing a reader can act on, so it is the one subject named rather than echoed.
pub(crate) fn subject_of(target: &Target) -> Subject {
    match target {
        Target::Stdin => Subject::Stdin,
        Target::File(path) => Subject::File(path.clone()),
        Target::Executable { name, .. } => Subject::Executable(name.clone()),
    }
}

/// The entry program, what its own modules resolve against, and the text it was parsed from: a file when there is one, and otherwise standard input, drained to end.
///
/// Draining is why this is worth naming rather than inlining. The program's own standard input is gone once the compiler has read the source out of it, so `/std/read()` reports end-of-input — unavoidable when both want one descriptor, and the reason a program that reads its input belongs in a file.
fn open(entry: Option<&Path>) -> Result<(Entrypoint, RootSource, Rc<Source>), CompileError> {
    let Some(path) = entry else {
        let text = io::read_to_string(io::stdin()).map_err(|error| {
            CompileError::failure(format!("failed to read standard input: {error}"))
        })?;

        return Entrypoint::supplied(STDIN_LABEL, &text)
            .map_err(|error| CompileError::Failure(vec![error.report()]));
    };

    Entrypoint::opened(path).map_err(|error| CompileError::Failure(vec![error.report()]))
}

/// Compile `entrypoint` against `units` in the order given, narrating each step.
pub(crate) fn compile_entry(
    budget: u64,
    units: &[RootSource],
    entrypoint: &Entrypoint,
    loader: &RootSource,
    subject: &Subject,
    cache: Option<&dyn Cache>,
) -> Result<Module, CompileError> {
    // Every target heads a group, since a compile and a handover always follow it. What the scope decides is whether the entry's own compile is a step of its own: with units to fold, those are the steps and the entry finishes among them unannounced; with none, the entry's compile is the one step there is.
    let has_units = !units.is_empty();
    fact(Heading::Processing, subject);

    // The entry is the one subject the fold cannot name — it owns the empty prefix — so it is reported under the name the caller was asked for.
    let mut line: Option<Line> = None;

    // The CLI doesn't yet expose a way to supply `foreign` implementations, so its `ForeignStore` is dropped here.
    let compiled = compile_with_units(
        budget,
        units,
        entrypoint,
        loader,
        cache,
        |_| {},
        |progress| report(&mut line, subject, has_units, progress),
    );

    // A refusal is not a compiler dying mid-operation, which is the one case an unterminated line is left to mean: this one finished, and is about to say why. Closing the innermost line here is what lets the report below start at column zero, as every line of it after the first already does — and as the same `Report` renders through `wonder`.
    if compiled.is_err() && line.is_some() {
        eprintln!();
    }

    compiled.map(|(module, _foreigns)| module)
}

/// [`compile_entry`] over a target that has already been resolved to a path, for the profiling mode — which compiles one bare file and consults nothing.
#[cfg(feature = "profile")]
pub(crate) fn compile_file(
    budget: u64,
    units: Vec<RootSource>,
    entry: &Path,
    subject: &Subject,
) -> Result<Module, CompileError> {
    let (entrypoint, loader, _source) =
        Entrypoint::opened(entry).map_err(|error| CompileError::Failure(vec![error.report()]))?;

    compile_entry(budget, &units, &entrypoint, &loader, subject, None)
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
        Progress::Compiling(prefix) => {
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

/// Read every `--unit DIR`'s manifest in the order written, which is the order they are compiled in.
pub(crate) fn load_units(units: &[PathBuf]) -> Result<Vec<RootSource>, CompileError> {
    curios_package::mounted(units).map_err(CompileError::failure)
}
