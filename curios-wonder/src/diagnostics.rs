//! The `diagnostics` query: every diagnostic and goal one compilation of a program reports.

use {
    crate::{Diagnosis, Placed, Reading, Severity},
    curios_common::{About, OsError, Qualifier, Report, Said, Source, SourceName},
    curios_module::{
        Overlay, PackageRoot, PreparedText, RootSource, UnitSource, open_entrypoint,
        overlay_entrypoint, supply_entrypoint,
    },
    curios_package::STDIN_MOUNT,
    curios_pipeline::{Cache, Checked, CompileError, Earlier, EntryTail, Findings, Fold, Progress},
    curios_surface::{Entrypoint, WrittenIn},
    curios_unit::Unit,
    curios_verdict::Verdicts,
    std::{
        cell::RefCell,
        collections::BTreeSet,
        fs,
        path::{Path, PathBuf},
        sync::Arc,
    },
};

/// What one compilation of a subject reports, and what it reached: every diagnostic, goal and lint, and the prefix of every mount some reference of the subject was *written* under — what `curios lint` reads a package's unused dependencies off.
pub struct Diagnosed {
    pub diagnostics: Vec<Diagnosis>,
    pub reached: BTreeSet<Qualifier>,
    /// What the file a check was asked about writes, read off the subject's lowering in that check's own fold (`checked`): every text of the file the subject was lowered from with the names and terms each writes, and what each declaration they name is answered with. None where no file was asked about, or the subject did not lower.
    pub placed: Option<Placed>,
}

/// What a question is about.
///
/// The transport decides this and the engine compiles it: a file a package's library declares is asked about as that whole unit, one an executable declares as that executable's origin, and a file no unit declares on its own, as the program or the module it is written as — see `curios-package`'s `Selection` for the rule. The engine never probes for a manifest of its own, nor reads what form a text takes.
pub enum Subject {
    /// A program: the entry compiled against `units`, in the order given.
    Entry {
        units: Vec<RootSource>,
        origin: Origin,
        /// The prefixes the entry may name, as its manifest declares them — `None` for a loose program, which has no manifest and so sees every open prefix in scope.
        ///
        /// An `Option` rather than a possibly-empty list, because the two differ: a program that declared *none* sees nothing but its own names, and one that declared *nothing* sees everything a program may name. A loose program is the second.
        declares: Option<Vec<Qualifier>>,
        /// The package that declares the entry, which names its files — `None` for a loose program, which no package declares.
        package: Option<PackageRoot>,
    },
    /// A unit: the last of `units`, compiled against the ones before it. Its verdicts are the answer.
    Unit { units: Vec<RootSource> },
    /// A loose text of Core text, which no lowering made: read over the prelude through the pipeline's door and compiled as a unit, the term it closes with, where it closes with one, judged as a program's is ([`Fold::check_core_text`]).
    CoreText { origin: Origin },
}

impl Subject {
    /// The prelude's scope alone: a unit of nothing over it, mounted nowhere, so no name answers in it but the prelude's.
    pub fn prelude() -> Self {
        Subject::Unit {
            units: vec![RootSource::supplied()],
        }
    }
}

/// Where the program a question is about comes from: a file, or the text read from standard input, which is named [`SourceName::stdin`] where a diagnostic closes on its place.
///
/// A file an editor holds unsaved is not text — it is [`Origin::File`] with its path in the overlay, so its `mod` declarations still resolve where the saved file's would.
pub enum Origin {
    File(PathBuf),
    /// The text standard input held, written in what the command line said it is.
    Text {
        text: String,
        written: WrittenIn,
    },
}

impl Origin {
    /// What the text is written in: a file's name says it, and standard input's text was told it.
    pub fn written(&self) -> WrittenIn {
        match self {
            Origin::File(path) => WrittenIn::of(path),
            Origin::Text { written, .. } => *written,
        }
    }

    /// The text, as the source a report closes on, read through `overlay` for a file, with the prefix a unit of it claims where it is written as a module: a file's stem, and [`STDIN_MOUNT`] for standard input's text. A file that cannot be read is the one diagnosis.
    pub(crate) fn read(
        &self,
        overlay: &Overlay,
    ) -> Result<(Arc<Source>, Qualifier), Box<Diagnosis>> {
        match self {
            Origin::File(path) => {
                let text = match overlay.get(path) {
                    Some(text) => text.to_string(),
                    None => fs::read_to_string(path).map_err(|error| {
                        let mut said = Said::line("unreadable file");
                        said.stating(OsError(&error));

                        Box::new(Diagnosis {
                            severity: Severity::Error,
                            report: Report::about(About::Path(path.clone()), said),
                        })
                    })?,
                };
                let stem = path
                    .file_stem()
                    .and_then(|stem| stem.to_str())
                    .unwrap_or_default();

                Ok((Source::held(path, text), Qualifier::from([stem])))
            }
            Origin::Text { text, .. } => Ok((
                Source::named(SourceName::stdin(), text.as_str()),
                Qualifier::from([STDIN_MOUNT]),
            )),
        }
    }
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

/// [`diagnostics`](diagnostics()), with what `file` writes read off the subject's lowering in the same fold ([`Diagnosed::placed`]): what an editor's check answers a place in the document from, the lowering being there whether or not the subject elaborates.
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
    let overlaid_verdicts = cache.map(|cache| Overlaid::over(cache, overlay));
    let through = overlaid_verdicts
        .as_ref()
        .map(|overlaid_verdicts| overlaid_verdicts as &dyn Cache);

    // The subject's lowering, as the fold makes or restores it, read for what `file` writes.
    let reading = file.map(Reading::of);
    let placed = RefCell::new(None);
    let read = |scope: &[&Unit], text: &PreparedText, entry: Option<&curios_core::Entrypoint>| {
        if let Some(reading) = &reading {
            *placed.borrow_mut() = Some(reading.read(scope, text, entry));
        }
    };
    // What elaboration established of the subject's own declarations, told once it has, completing what the lowering answered of them.
    let elaborated = |scope: &[&Unit],
                      text: &PreparedText,
                      definitions: &curios_core::Definitions,
                      traces: &curios_elab::Traces,
                      certification: Option<&curios_core::Certification>| {
        if let Some(placed) = placed.borrow_mut().as_mut() {
            placed.establish(scope, text, definitions, traces, certification);
        }
    };
    let fold = |units| {
        let fold = Fold::new(budget, units, through);
        match reading {
            Some(_) => fold.reading(&read).elaborating(&elaborated),
            None => fold,
        }
    };

    let mut diagnosed = match subject {
        Subject::Unit { units } => {
            let units = tested_last(overlaid(units, overlay));
            // A refused library's findings, reported beside its refusal as a refused program's are.
            let refusal = RefCell::new(None);
            let refused = |findings: Findings| *refusal.borrow_mut() = Some(findings);
            let found = fold(&units)
                .refusing(&refused)
                .units(&mut *progress, |_, produced| {
                    Ok(produced.last().map(Findings::of_unit).unwrap_or_default())
                });
            match (found, refusal.take()) {
                (Ok(findings), _) => Diagnosed::found(None, findings),
                (Err(error), Some(findings)) => Diagnosed::found(Some(error), findings),
                (Err(error), None) => Diagnosed::refused(error),
            }
        }
        Subject::Entry {
            units,
            origin,
            declares,
            package,
        } => match open(origin, declares, package.as_ref(), overlay) {
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
                placed: None,
            },
        },
        // Core text is lowered by nothing: the check reads it through the pipeline's door.
        Subject::CoreText { origin } => match origin.read(overlay) {
            Ok((source, module)) => {
                match Fold::new(budget, &[], through).check_core_text(
                    &source,
                    module,
                    &mut *progress,
                ) {
                    Ok(()) => Diagnosed::found(None, Findings::default()),
                    Err(error) => Diagnosed::refused(error),
                }
            }
            Err(refusal) => Diagnosed::unread(*refusal),
        },
    };
    diagnosed.placed = placed.take();

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
            placed: None,
        }
    }

    /// A compilation that stopped before its subject was lowered: the error, and nothing reached.
    fn refused(error: CompileError) -> Self {
        Self {
            diagnostics: of_error(error),
            reached: BTreeSet::new(),
            placed: None,
        }
    }

    /// A subject whose text could not be read: the one diagnosis, and nothing reached.
    fn unread(refusal: Diagnosis) -> Self {
        Self {
            diagnostics: vec![refusal],
            reached: BTreeSet::new(),
            placed: None,
        }
    }
}

/// `program` parsed, with the loader its modules resolve through — both reading through `overlay`, and for `package` where a package declares it — or the one diagnostic a program that does not parse gets.
pub fn open(
    origin: Origin,
    declares: Option<Vec<Qualifier>>,
    package: Option<&PackageRoot>,
    overlay: &Overlay,
) -> Result<(Entrypoint, RootSource), Vec<Diagnosis>> {
    let opened = match origin {
        Origin::File(path) => match overlay.get(&path) {
            Some(text) => overlay_entrypoint(&path, text, package).map_err(|error| error.report()),
            None => open_entrypoint(&path, package).map_err(|error| error.report()),
        },
        Origin::Text { text, .. } => supply_entrypoint(&text).map_err(|error| error.report()),
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

/// The cabinet as a question reaches it: read through the editor's documents, and filed into where the disk holds what was compiled.
///
/// **A stored unit is verified through the overlay.** A unit is believed on a re-read of every file it was compiled from, and here that re-read takes an open document's text over the disk's (`Verdicts::get_overlaid`), so a unit whose source an editor holds edited is a miss and one whose source is merely open is not. The rule is exact because the record lists what the unit read: a document the unit never read — an executable beside a package's library, in the very directory the library reads from — leaves the hit standing. Refusing on containment instead would cost the language server that library on every keystroke in a program file.
///
/// **A unit is filed where the disk holds every text its fold has read** — its own and those of every unit before it, each restored on a record the disk confirms or compiled from text the disk holds, whole or over a baseline — which is [A command compiles only as far as its answer needs, and a project keeps what it compiled](../../documentation/design/tooling/a-command-compiles-only-as-far-as-its-answer-needs-and-a-project-keeps-what-it-compiled.md)'s rule. How it was compiled is no part of the test: a unit compiled over a baseline is stored as the bytes a whole compile of the same text stores, so the object is the one a build files. A fold that has read a text the disk does not hold files nothing: a record names what each unit before it contained, so a unit filed after one nothing on disk reproduces would be a miss for every later reader, in the place of an object that may have held the disk's own.
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

    /// The cabinet's own answer ([`Verdicts`]' `Cache::baseline`): the session's last unit, then the object this tree filed, then what the scope offered. A build run in another terminal mid-session can leave the cabinet nearer than the session, and then the recompile redoes more than it had to and answers the same.
    fn baseline(&self, source: &UnitSource<'_>, offered: Option<&Unit>) -> Option<Earlier> {
        Cache::baseline(self.cache, source, offered)
    }

    /// Kept, then filed where the disk holds every text the fold has read. This is why the cabinet itself is held rather than a `dyn Cache`, which cannot keep a unit without filing it.
    ///
    /// **Every unit is kept, filed or not.** Keeping hands the session its next baseline, with the units it was compiled after, and takes the unit into the fold, so a unit after it is checked against it whether or not it was filed.
    ///
    /// **The disk is asked as the unit is handed over**, of everything the fold has read so far, which is what catches a file rewritten while its unit compiled.
    ///
    /// **A unit that is not filed has no place in the cabinet.** No object is addressed after another, so a unit that cannot read it is restored as though it were not there, and one that can is compiled over its baseline, each stage asking again what it read of the unit that moved. Saying what an unfiled unit's object would hold costs a serialization of the whole unit, which on the standard library is most of what a cache would cost a keystroke, and nothing reads it.
    fn put(&self, source: &UnitSource<'_>, unit: &Unit) {
        self.cache.keep(source, unit);

        if self.cache.taken_on_disk() {
            Cache::put(self.cache, source, unit);
        }
    }
}
