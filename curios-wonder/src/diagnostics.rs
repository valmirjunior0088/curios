//! The `diagnostics` query: every diagnostic and goal one compilation of a program reports.

use {
    crate::{Diagnosis, Reading, STDIN_MOUNT, Severity, Written},
    curios_common::{Qualifier, Report, Said, Source, Span, is_identifier},
    curios_module::{
        Overlay, PreparedText, RootSource, UnitSource, open_entrypoint, overlay_entrypoint,
        supply_entrypoint,
    },
    curios_pipeline::{Cache, Checked, CompileError, Earlier, EntryTail, Findings, Fold, Progress},
    curios_surface::{Entrypoint, Form},
    curios_unit::Unit,
    curios_verdict::Verdicts,
    std::{
        cell::RefCell,
        collections::BTreeSet,
        fs,
        path::{Path, PathBuf},
    },
};

/// What one compilation of a subject reports, and what it reached: every diagnostic, goal and lint, and the prefix of every mount some reference of the subject was *written* under — what `curios lint` reads a package's unused dependencies off.
pub struct Diagnosed {
    pub diagnostics: Vec<Diagnosis>,
    pub reached: BTreeSet<Qualifier>,
    /// What the file a check was asked about writes, read off the subject's lowering in that check's own fold (`checked`): every text of the file the subject was lowered from, with the names each writes. Empty where no file was asked about, or the subject did not lower.
    pub written: Vec<Written>,
}

/// What a question is about.
///
/// The transport decides this and the engine compiles it: a file a package's library declares is asked about as that whole unit, one an executable declares as that executable's origin, and a file no unit declares on its own — see `curios-package`'s `Selection` for the rule. The engine never probes for a manifest of its own; the one thing it decides is the form a loose program is written in (`Subject::formed`).
pub enum Subject {
    /// A program: the entry compiled against `units`, in the order given.
    Entry {
        units: Vec<RootSource>,
        origin: Origin,
        /// The prefixes the entry may name, as its manifest declares them — `None` for a loose program, which has no manifest and so sees every open prefix in scope.
        ///
        /// An `Option` rather than a possibly-empty list, because the two differ: a program that declared *none* sees nothing but its own names, and one that declared *nothing* sees everything a program may name. A loose program is the second.
        declares: Option<Vec<Qualifier>>,
    },
    /// A unit: the last of `units`, compiled against the ones before it. Its verdicts are the answer.
    Unit { units: Vec<RootSource> },
}

impl Subject {
    /// The refusal of a loose file written as a module whose stem is no name a module can take: it can be mounted nowhere to be checked item by item, and checked as a program it would be refused at its last line for a term it was never meant to hold.
    fn unnameable(&self, overlay: &Overlay) -> Option<Diagnosis> {
        let Subject::Entry {
            units,
            origin: Origin::File(path),
            declares: None,
            ..
        } = self
        else {
            return None;
        };
        if !units.is_empty() {
            return None;
        }

        let text = match overlay.get(path) {
            Some(text) => text.to_string(),
            None => fs::read_to_string(path).ok()?,
        };
        let stem = path.file_stem()?.to_str()?;
        if Form::of(path, &text) != Form::Module || is_identifier(stem) {
            return None;
        }

        let mut said = Said::line("unnameable module");
        said.stating(format_args!(
            "{} is written as a module, with no final term",
            path.display()
        ));
        said.stating(format_args!(
            "its stem `{stem}` is no name a module can take"
        ));
        said.stating(
            "rename the file to an identifier, or end it in a term to check it as a program",
        );

        Some(Diagnosis {
            severity: Severity::Error,
            report: Report::of(Some(Span::new(Source::held(path, &text), 0, 0)), said),
        })
    }

    /// The subject a loose program is answered as, by the form it is written in: a program when a final term follows its items, and otherwise a module, checked as a unit of its own the way a library is — mounted at its stem for a file, and at [`STDIN_MOUNT`] for text on standard input — so a module no `mod` declares is answered item by item, rather than refused at its end for lacking a term it was never meant to have.
    pub fn formed(self, overlay: &Overlay) -> Self {
        match self {
            Subject::Entry {
                units,
                origin,
                declares: None,
            } if units.is_empty() => {
                let module = match &origin {
                    Origin::File(path) => RootSource::loose_module(path, overlay),
                    Origin::Text { label, text } => {
                        RootSource::labelled_module(STDIN_MOUNT, label, text)
                    }
                };

                match module {
                    Some(unit) => Subject::Unit { units: vec![unit] },
                    None => Subject::Entry {
                        units,
                        origin,
                        declares: None,
                    },
                }
            }
            subject => subject,
        }
    }
}

/// Where the program a question is about comes from: a file, or text standing in for one.
///
/// Text carries a label because a diagnostic names its source in the place it closes on, `label:line:column`, exactly as it names a path, and `<stdin>` is what a program piped in is called. A file an editor holds unsaved is not text — it is [`Origin::File`] with its path in the overlay, so its `mod` declarations still resolve where the saved file's would.
pub enum Origin {
    File(PathBuf),
    Text { label: String, text: String },
}

/// Every diagnostic and goal `subject` reports when lowered, elaborated and judged against the prelude — empty when it compiles — with what the subject reached beside the records. `overlay` is consulted before the disk for every file read, the entry included.
///
/// **A library is asked through the fold alone.** It has no written program, and none is synthesized for it: the subject is the fold's last unit, and what the fold decided about it is the answer — its refusal, or its lints. A loose file written as a module is asked the same way, as a unit of its own. An entry is asked under its written tail alone.
///
/// **The subject is asked as `curios test` compiles it**, tested, so its tests are checked and a declaration only they read is reached; every unit it stands on is built. `curios test` also puts the unit under a synthesized `Test/main([...])` over its tests, and a question does not, because that tail raises nothing of its own: a test's body is an item checked at `/std/Test` like any other, so a fault in one is the unit's refusal, and a tail over tests that were accepted is one that is accepted. Compiling it anyway would cost every question about a library the tail's elaboration, its certifier recheck, and its erasure over the unit's whole arena, which is most of what a check of a small library costs.
///
/// **Every refused declaration is reported, and a declaration reaching a refused one is not.** Elaboration recovers per item, so a file with three mistakes answers with three records in one check, each classified on its own — a refusal beside a goal batch keeps the goal records at their goals — and nothing for a dependent, whose record could only restate the refusal (the rule is `curios-elab`'s README's). A parse failure in one declaration is that declaration's record — the parser resynchronizes at the next item, and the rest of the file is read, elaborated and reported as if the broken one were a refused one — while a failure the item grammar never committed to still ends the file where it stopped, since past that point nothing says what an item is.
///
/// **A lint is reported beside whatever the verdict was**, after it. A lint is decided by the lowering, so a program that lowers has its lints whether elaboration then refused it, left a goal batch, or accepted it; only a program that does not lower — a parse failure, an unresolved name — reports its error alone, since there is nothing to have read the lints off.
///
/// `cache` is consulted for units already built, and filed into where the disk holds every text the fold has read — see the `wonder` module documentation. `progress` is told each step the fold takes.
pub fn diagnostics(
    budget: u64,
    subject: Subject,
    overlay: &Overlay,
    cache: Option<&Verdicts>,
    progress: &mut dyn FnMut(Progress<'_>),
) -> Diagnosed {
    diagnosed_reading(budget, subject, overlay, cache, None, progress)
}

/// [`diagnostics`](diagnostics()), with what `file` writes read off the subject's lowering in the same fold ([`Diagnosed::written`]): what an editor's check answers a place in the document from, the lowering being there whether or not the subject elaborates.
pub fn checked(
    budget: u64,
    subject: Subject,
    overlay: &Overlay,
    cache: Option<&Verdicts>,
    file: &Path,
    progress: &mut dyn FnMut(Progress<'_>),
) -> Diagnosed {
    diagnosed_reading(budget, subject, overlay, cache, Some(file), progress)
}

/// [`diagnostics`](diagnostics()), reading what `file` writes where there is one.
fn diagnosed_reading(
    budget: u64,
    subject: Subject,
    overlay: &Overlay,
    cache: Option<&Verdicts>,
    file: Option<&Path>,
    progress: &mut dyn FnMut(Progress<'_>),
) -> Diagnosed {
    let store = cache.map(|cache| Overlaid::over(cache, overlay));
    let through = store.as_ref().map(|store| store as &dyn Cache);

    if let Some(refusal) = subject.unnameable(overlay) {
        return Diagnosed {
            diagnostics: vec![refusal],
            reached: BTreeSet::new(),
            written: Vec::new(),
        };
    }

    // The subject's lowering, as the fold makes or restores it, read for what `file` writes.
    let reading = file.map(|file| Reading::of(file, overlay));
    let written = RefCell::new(Vec::new());
    let read = |scope: &[&Unit], text: &PreparedText, entry: Option<&curios_core::Entrypoint>| {
        if let Some(reading) = &reading {
            *written.borrow_mut() = reading.read(scope, text, entry);
        }
    };
    let fold = |units| {
        let fold = Fold::new(budget, units, through);
        match reading {
            Some(_) => fold.reading(&read),
            None => fold,
        }
    };

    let mut diagnosed = match subject.formed(overlay) {
        Subject::Unit { units } => {
            let units = tested_last(overlaid(units, overlay));
            let found = fold(&units).units(&mut *progress, |_, produced| {
                Ok(produced.last().map(Findings::of_unit).unwrap_or_default())
            });
            match found {
                Ok(findings) => Diagnosed::found(None, findings),
                Err(error) => Diagnosed::refused(error),
            }
        }
        Subject::Entry {
            units,
            origin,
            declares,
            ..
        } => match open(origin, declares, overlay) {
            Ok((entrypoint, loader)) => {
                let units = overlaid(units, overlay);
                let checked =
                    fold(&units).check(&entrypoint, &loader.tested(), EntryTail::Program, |step| {
                        progress(step)
                    });
                answered(checked)
            }
            Err(refusal) => Diagnosed {
                diagnostics: refusal,
                reached: BTreeSet::new(),
                written: Vec::new(),
            },
        },
    };
    diagnosed.written = written.take();

    diagnosed
}

/// What a program's check reports: its verdict's records, and the lints its entry's lowering found.
fn answered(checked: Result<Checked, CompileError>) -> Diagnosed {
    match checked {
        Ok(Checked { entry, verdict }) => Diagnosed::found(verdict.err(), entry),
        Err(error) => Diagnosed::refused(error),
    }
}

impl Diagnosed {
    /// A subject that lowered: whatever stopped it, then its lints, and what it reached.
    fn found(stopped: Option<CompileError>, findings: Findings) -> Self {
        let mut diagnostics = stopped.map(of_error).unwrap_or_default();
        diagnostics.extend(findings.reports().into_iter().map(Diagnosis::lint));
        let reached = findings.reached;

        Self {
            diagnostics,
            reached,
            written: Vec::new(),
        }
    }

    /// A compilation that stopped before its subject was lowered: the error, and nothing reached.
    fn refused(error: CompileError) -> Self {
        Self {
            diagnostics: of_error(error),
            reached: BTreeSet::new(),
            written: Vec::new(),
        }
    }
}

/// `program` parsed, with the loader its modules resolve through — both reading through `overlay` — or the one diagnostic a program that does not parse gets.
pub fn open(
    origin: Origin,
    declares: Option<Vec<Qualifier>>,
    overlay: &Overlay,
) -> Result<(Entrypoint, RootSource), Vec<Diagnosis>> {
    let opened = match origin {
        Origin::File(path) => match overlay.get(&path) {
            Some(text) => overlay_entrypoint(&path, text).map_err(|error| error.report()),
            None => open_entrypoint(&path).map_err(|error| error.report()),
        },
        Origin::Text { label, text } => {
            supply_entrypoint(&label, &text).map_err(|error| error.report())
        }
    };

    match opened {
        Ok((entrypoint, loader, _source)) => Ok((
            entrypoint,
            // Declared before the overlay, so the two builders compose in the order they are written rather than one dropping the other.
            match declares {
                Some(declares) => loader.declaring(declares),
                None => loader,
            }
            .with_overlay(overlay.clone()),
        )),
        Err(report) => Err(vec![Diagnosis {
            severity: Severity::Error,
            report,
        }]),
    }
}

/// A compile failure as records: the classification the compile path made, on every report it carries — for a mixed failure, the first `failures` reports as errors and the rest as goals, which is how it says which is which.
pub fn of_error(error: CompileError) -> Vec<Diagnosis> {
    let (reports, failures) = match error {
        CompileError::Incomplete(reports) => (reports, 0),
        CompileError::Failure(reports) => {
            let failures = reports.len();
            (reports, failures)
        }
        CompileError::Mixed { reports, failures } => (reports, failures),
    };

    reports
        .into_iter()
        .enumerate()
        .map(|(index, report)| Diagnosis {
            severity: match index < failures {
                true => Severity::Error,
                false => Severity::Goal,
            },
            report,
        })
        .collect()
}

/// Every unit reading through `overlay`.
pub fn overlaid(units: Vec<RootSource>, overlay: &Overlay) -> Vec<RootSource> {
    units
        .into_iter()
        .map(|unit| unit.with_overlay(overlay.clone()))
        .collect()
}

/// `units` with the last, a question's subject, compiled tested: a question is answered of the unit as `curios test` compiles it, its tests and what they read included, and of every unit it stands on as built.
pub fn tested_last(mut units: Vec<RootSource>) -> Vec<RootSource> {
    if let Some(subject) = units.pop() {
        units.push(subject.tested());
    }

    units
}

/// The store as a question reaches it: read through the editor's documents, and filed into where the disk holds what was compiled.
///
/// **A stored unit is verified through the overlay.** A unit is believed on a re-read of every file it was compiled from, and here that re-read takes an open document's text over the disk's (`Verdicts::get_overlaid`), so a unit whose source an editor holds edited is a miss and one whose source is merely open is not. The rule is exact because the record lists what the unit read: a document the unit never read — an executable beside a package's library, in the very directory the library reads from — leaves the hit standing. Refusing on containment instead would cost the language server that library on every keystroke in a program file.
///
/// **A unit is filed where the disk holds every text its fold has read** — its own and those of every unit before it, each restored on a record the disk confirms or compiled from text the disk holds, whole or over a baseline — which is [A command compiles only as far as its answer needs, and a project keeps what it compiled](../../documentation/design/tools/a-command-compiles-only-as-far-as-its-answer-needs-and-a-project-keeps-what-it-compiled.md)'s rule. How it was compiled is no part of the test: a unit compiled over a baseline is stored as the bytes a whole compile of the same text stores, so the slot is the one a build files. A fold that has read a text the disk does not hold files nothing: a record names what each unit before it contained, so a unit filed after one nothing on disk reproduces would be a miss for every later reader, in the place of a slot that may have held the disk's own.
pub struct Overlaid<'a> {
    cache: &'a Verdicts,
    overlay: &'a Overlay,
}

impl<'a> Overlaid<'a> {
    /// `cache` as one fold of a question reading through `overlay` reaches it.
    pub fn over(cache: &'a Verdicts, overlay: &'a Overlay) -> Self {
        Self { cache, overlay }
    }
}

impl Cache for Overlaid<'_> {
    fn get(&self, source: &UnitSource<'_>) -> Option<Unit> {
        self.cache.get_overlaid(source, self.overlay)
    }

    /// The store's own answer ([`Verdicts`]' `Cache::baseline`): the session's last unit, then the slot this tree filed, then what the scope offered. A build run in another terminal mid-session can leave the store nearer than the session, and then the recompile redoes more than it had to and answers the same.
    fn baseline(&self, source: &UnitSource<'_>, offered: Option<&Unit>) -> Option<Earlier> {
        Cache::baseline(self.cache, source, offered)
    }

    /// Kept, then filed where the disk holds every text the fold has read. This is why the store itself is held rather than a `dyn Cache`, which cannot keep a unit without filing it.
    ///
    /// **Every unit is kept, filed or not.** Keeping hands the session its next baseline, with the units it was compiled after, and takes the unit into the fold, so a unit after it is checked against it whether or not it was filed.
    ///
    /// **The disk is asked as the unit is handed over**, of everything the fold has read so far, which is what catches a file rewritten while its unit compiled.
    ///
    /// **A unit that is not filed has no place in the store.** No slot is addressed after another, so a unit that cannot read it is restored as though it were not there, and one that can is compiled over its baseline, each stage asking again what it read of the unit that moved. Saying what an unfiled unit's slot would hold costs a serialization of the whole unit, which on the standard library is most of what a cache would cost a keystroke, and nothing reads it.
    fn put(&self, source: &UnitSource<'_>, unit: &Unit) {
        self.cache.keep(source, unit);

        if self.cache.taken_on_disk() {
            Cache::put(self.cache, source, unit);
        }
    }
}
