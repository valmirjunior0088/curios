//! Driving the compile pipeline for the CLI, with the `--print` stage dump wired in. `stage_printer` is the single owner of how each IR stage is selected and rendered.

use {
    crate::{Heading, Line, Subject, fact},
    curios::Verdicts,
    curios_package::Target,
    curios_pipeline::{Cache, CompileError, Progress, Stage, compile_with_units},
    curios_text::Entrypoint,
    curios_wasm::Module,
    std::path::{Path, PathBuf},
};

/// Build the observer closure that prints each requested IR stage to stderr. `print` is the comma-separated stage list from `--print`; empty segments are dropped (the flag's absence arrives as `""`), and an unknown stage name is an error rather than a silently empty selection. `main` builds a second one for the stage `to_cwasm_dumped` emits downstream of the pipeline, so the two rendering paths cannot drift.
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

/// Compile `target` to a wasm module, against the `--unit` packages and whatever its own manifest declares.
///
/// A `--unit` package is the already-resolved form of a manifest entry, so it goes in front of the graph's own order: the order arguments arrive in *is* dependency order. The error keeps the incomplete/failure split so `main` can map a goal batch to its own exit code.
pub(crate) fn compile_target(
    budget: u64,
    print: &str,
    units: &[PathBuf],
    target: Target,
) -> Result<Module, CompileError> {
    let mut scope = load_units(units)?;

    // A bare file has no project, so it has no store to consult: what it may reuse is a fact about the project it is in, and it is in none.
    let subject = subject_of(&target);
    let (entry, cache) = match target {
        Target::File(path) => (path, None),
        Target::Executable {
            entry, units, root, ..
        } => {
            scope.extend(units);

            (entry, Some(Verdicts::at(root)))
        }
    };

    let compiled = compile_entry(
        budget,
        print,
        scope,
        &entry,
        &subject,
        cache.as_ref().map(|cache| cache as &dyn Cache),
    );

    // After the fold rather than during it: one unwritable store refuses every unit for the same reason, so this is one line however many units went past it. Reported even when the compilation failed, because a store nobody can write is true either way and the next run pays for it either way.
    if let Some(refusal) = cache.as_ref().and_then(Verdicts::refused) {
        fact(Heading::Skipped, format!("storing units; {refusal}"));
    }

    compiled
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

/// Compile `entry` against `units` in the order given, printing any requested IR stages along the way.
pub(crate) fn compile_entry(
    budget: u64,
    print: &str,
    units: Vec<curios_text::RootSource>,
    entry: &Path,
    subject: &Subject,
    cache: Option<&dyn Cache>,
) -> Result<Module, CompileError> {
    let printer = stage_printer(print).map_err(CompileError::Failure)?;
    let (entrypoint, loader) = Entrypoint::opened(entry).map_err(CompileError::Failure)?;

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
        &units,
        &entrypoint,
        loader,
        cache,
        printer,
        |progress| report(&mut line, subject, grouped, progress),
    )
    .map(|(module, _foreigns)| module)
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
pub(crate) fn load_units(units: &[PathBuf]) -> Result<Vec<curios_text::RootSource>, CompileError> {
    curios_package::mounted(units).map_err(CompileError::Failure)
}
