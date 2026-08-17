//! Driving the compile pipeline for the CLI, from a resolved target to the `.cwasm` payload both subcommands consume — including the store consultation that can skip the whole thing. `stage_printer` is the single owner of how each IR stage is selected and rendered.
//!
//! **The payload, not the wasm module, is what this hands back**, and that is what lets one stored artifact serve `run` and `compile` alike: `run` executes it in-process exactly as it executes a fresh one, and `compile` appends it to the embedded launcher. Optimization and precompilation therefore happen here rather than in `main`, which is left dispatching.

use {
    crate::{Heading, Line, Subject, fact},
    curios::{Program, Verdicts, to_cwasm, to_cwasm_dumped},
    curios_package::Target,
    curios_pipeline::{Cache, CompileError, Progress, Stage, compile_with_units},
    curios_text::{Entrypoint, RootSource, UnitSource},
    curios_wasm::Module,
    std::path::PathBuf,
};

#[cfg(feature = "profile")]
use std::path::Path;

/// Build the observer closure that prints each requested IR stage to stderr. `print` is the comma-separated stage list from `--print`; empty segments are dropped (the flag's absence arrives as `""`), and an unknown stage name is an error rather than a silently empty selection. The stage `to_cwasm_dumped` emits downstream of the pipeline goes through a second one built the same way, so the two rendering paths cannot drift.
pub(crate) fn stage_printer(print: &str) -> Result<impl Fn(Stage<'_>) + '_, String> {
    let stages = print
        .split(',')
        .filter(|name| !name.is_empty())
        .collect::<Vec<_>>();

    if let Some(unknown) = stages.iter().find(|name| !Stage::NAMES.contains(name)) {
        return Err(format!(
            "unknown --print stage {unknown:?}; the stages are {}",
            Stage::NAMES.join(", ")
        ));
    }

    Ok(move |stage: Stage<'_>| {
        if stages.contains(&stage.name()) {
            eprintln!("\n=== {} ===\n{stage}", stage.name());
        }
    })
}

/// The precompiled payload for `target`, taken from the store when nothing it was made from has changed and compiled otherwise.
///
/// A `--unit` package is the already-resolved form of a manifest entry, so it goes in front of the graph's own order: the order arguments arrive in *is* dependency order. The error keeps the incomplete/failure split so `main` can map a goal batch to its own exit code.
pub(crate) fn payload_of(
    budget: u64,
    print: &str,
    units: &[PathBuf],
    target: Target,
) -> Result<Vec<u8>, CompileError> {
    let mut scope = load_units(units)?;
    let subject = subject_of(&target);

    // A bare file has no project, so it has no store to consult: what it may reuse is a fact about the project it is in, and it is in none.
    let (entry, declared, cache) = match target {
        Target::File(path) => (path, None, None),
        Target::Executable {
            entry,
            units,
            root,
            package,
            name,
            ..
        } => {
            scope.extend(units);

            (entry, Some((package, name)), Some(Verdicts::at(root)))
        }
    };

    // Opened before the store is consulted, because the entry's own text is half of what a stored payload is verified against — and it has to be the text that was *parsed*, not a re-read taken afterwards.
    let (entrypoint, loader, source) = Entrypoint::opened(&entry).map_err(CompileError::Failure)?;

    let filed = cache.as_ref().zip(declared.as_ref()).map(|(cache, name)| {
        (
            cache,
            Program {
                package: &name.0,
                executable: &name.1,
                entry: &entry,
                text: &source.text,
                loader: &loader,
            },
        )
    });

    // `--print` skips the get and still puts: a stage dump exists only when compilation runs, so asking for one is asking for the work — and filing what a real compilation produced is always safe.
    if print.is_empty()
        && let Some((cache, program)) = &filed
        && let Some(payload) = cache.payload_get(
            program,
            &scope.iter().map(UnitSource::mounted).collect::<Vec<_>>(),
        )
    {
        // Announced after the store is consulted, exactly as the fold announces a reused unit: a reported operation is one that actually happened.
        let mut line = Line::open(Heading::Building, &subject);
        line.outcome("reused");
        eprintln!();

        return Ok(payload);
    }

    let compiled = compile_entry(
        budget,
        print,
        &scope,
        &entrypoint,
        &loader,
        &subject,
        cache.as_ref().map(|cache| cache as &dyn Cache),
    )
    .and_then(|module| precompiled(print, &module));

    if let (Ok(payload), Some((cache, program))) = (&compiled, &filed) {
        cache.payload_put(program, payload);
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

/// Optimize and precompile `module`, dumping Binaryen's own rendering of the optimized module when `--print` asked for it.
///
/// Chosen before optimizing rather than filtered after, unlike the driver's stages: this one's payload costs — Binaryen's text capture, and the name section riding into the artifact.
fn precompiled(print: &str, module: &Module) -> Result<Vec<u8>, CompileError> {
    match print.split(',').any(|name| name == "wasm-optm") {
        true => to_cwasm_dumped(module, stage_printer(print).map_err(CompileError::Failure)?),
        false => to_cwasm(module),
    }
    .map_err(CompileError::Failure)
}

/// What a target is reported as — the name that was asked for, never the file it resolved to.
///
/// A declared executable resolves to an absolute path somewhere under the governing root, and echoing that back fills a status line with what the reader already knew. A bare file *is* what was asked for, so it reports as written.
pub(crate) fn subject_of(target: &Target) -> Subject {
    match target {
        Target::File(path) => Subject::File(path.clone()),
        Target::Executable { name, .. } => Subject::Executable(name.clone()),
    }
}

/// Compile `entrypoint` against `units` in the order given, printing any requested IR stages along the way.
pub(crate) fn compile_entry(
    budget: u64,
    print: &str,
    units: &[RootSource],
    entrypoint: &Entrypoint,
    loader: &RootSource,
    subject: &Subject,
    cache: Option<&dyn Cache>,
) -> Result<Module, CompileError> {
    let printer = stage_printer(print).map_err(CompileError::Failure)?;

    // Having a scope to show is what makes this a group: with units to nest, the target heads them and its own compile closes that header, and with none the header *is* that compile's line. A group of one line under a heading naming the same work twice is worse than the plain line it replaces.
    let grouped = !units.is_empty();

    if grouped {
        fact(Heading::Building, subject);
    }

    // The entry is the one subject the fold cannot name — it owns the empty prefix — so it is reported under the name the caller was asked for.
    let mut line: Option<Line> = None;

    // The CLI doesn't yet expose a way to supply `foreign` implementations, so its `ForeignStore` is dropped here.
    compile_with_units(
        budget,
        units,
        entrypoint,
        loader,
        cache,
        printer,
        |progress| report(&mut line, subject, grouped, progress),
    )
    .map(|(module, _foreigns)| module)
}

/// [`compile_entry`] over a target that has already been resolved to a path, for the profiling mode — which compiles one bare file and consults nothing.
#[cfg(feature = "profile")]
pub(crate) fn compile_file(
    budget: u64,
    print: &str,
    units: Vec<RootSource>,
    entry: &Path,
    subject: &Subject,
) -> Result<Module, CompileError> {
    let (entrypoint, loader, _source) = Entrypoint::opened(entry).map_err(CompileError::Failure)?;

    compile_entry(budget, print, &units, &entrypoint, &loader, subject, None)
}

/// Fold one [`Progress`] event onto the open status line, opening and closing lines as subjects begin and end.
///
/// The line outlives each event, which is why it is threaded rather than owned here: `↳ Processing /hello` and the `; compiling... done 1.4s` that completes it are three separate writes to one line.
fn report(line: &mut Option<Line>, target: &Subject, grouped: bool, progress: Progress<'_>) {
    match progress {
        Progress::Compiling(prefix) => {
            *line = Some(opened(Line::nested(
                Heading::Processing,
                &Subject::Mounted(prefix.clone()),
            )));
        }
        // The entry program *is* the target, so a group that has already named it has nothing to add: its compile finishes the header rather than standing among the units as another step. Ungrouped there is no header yet, and this is it.
        Progress::Entry => {
            if !grouped {
                *line = Some(opened(Line::open(Heading::Building, target)));
            }
        }
        Progress::Reused(prefix) => {
            Line::nested(Heading::Processing, &Subject::Mounted(prefix.clone())).outcome("reused");
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

/// `line` with its `compiling` step already announced, so the clock starts where the work does.
fn opened(mut line: Line) -> Line {
    line.step("compiling");

    line
}

/// Read every `--unit DIR`'s manifest in the order written, which is the order they are compiled in.
pub(crate) fn load_units(units: &[PathBuf]) -> Result<Vec<RootSource>, CompileError> {
    curios_package::mounted(units).map_err(CompileError::Failure)
}
