//! The `diagnostics` query: every diagnostic and goal one compilation of a program reports.

use {
    crate::{Diagnostic, Severity},
    curios_package::Unlinked,
    curios_pipeline::{Cache, Checked, CompileError, EntryTail, Findings, check_with_units},
    curios_text::{Entrypoint, Overlay, RootSource, UnitSource},
    curios_unit::Unit,
    curios_utilities::{Qualifier, Report, Source, Span},
    curios_verdicts::Verdicts,
    std::{collections::BTreeSet, path::PathBuf},
};

/// What one compilation of a subject reports, and what it reached: every diagnostic, goal and lint, and the prefix of every mount some reference of the subject was *written* under — what `curios lint` reads a package's unused dependencies off.
pub struct Diagnosed {
    pub diagnostics: Vec<Diagnostic>,
    pub reached: BTreeSet<Qualifier>,
}

/// What a question is about.
///
/// The transport decides this and the engine compiles it: a file a package's library declares is asked about as that whole unit, one an executable declares as that executable's origin, and a file no unit declares on its own — see `curios-package`'s `Selection` for the rule. The engine never probes for a manifest of its own; the one thing it decides is the form a loose file is written in ([`Subject::formed`]).
pub enum Subject {
    /// A program: the entry compiled against `units`, in the order given.
    Entry {
        units: Vec<RootSource>,
        origin: Origin,
        /// The prefixes the entry may name, as its manifest declares them — `None` for a loose program, which has no manifest and so sees every open prefix in scope.
        ///
        /// An `Option` rather than a possibly-empty list, because the two differ: a program that declared *none* sees nothing but its own names, and one that declared *nothing* sees everything a program may name. A loose program is the second.
        declares: Option<Vec<Qualifier>>,
        /// Why the file is in none of the units of the package that holds it, when one does — the note the answer opens with.
        unlinked: Option<Unlinked>,
    },
    /// A unit: the last of `units`, compiled against the ones before it. Its verdicts are the answer.
    Unit { units: Vec<RootSource> },
}

impl Subject {
    /// The note a program selected loose from inside a package opens with, located at its file's start so an editor places it on the document asked about — the text the overlay's where it holds one, as every read is.
    fn note(&self, overlay: &Overlay) -> Option<Diagnostic> {
        let Subject::Entry {
            unlinked: Some(unlinked),
            ..
        } = self
        else {
            return None;
        };

        let source = match overlay.get(&unlinked.file) {
            Some(text) => Some(Source::held(&unlinked.file, text)),
            None => Source::read(&unlinked.file).ok(),
        };
        let report = match source {
            Some(source) => Report::at(Span::new(source, 0, 0), unlinked.message.clone()),
            None => Report::unlocated(unlinked.message.clone()),
        };

        Some(Diagnostic {
            severity: Severity::Note,
            report,
        })
    }

    /// The subject a loose file is answered as, by the form it is written in: a program when a final term follows its items, and otherwise a module, checked as a unit of its own mounted at its stem the way a library is — so a file no `mod` declares is answered item by item, rather than refused at its end for lacking a term it was never meant to have.
    pub(crate) fn formed(self, overlay: &Overlay) -> Self {
        match self {
            Subject::Entry {
                units,
                origin: Origin::File(path),
                declares: None,
                unlinked,
            } if units.is_empty() => match RootSource::loose_module(&path, overlay) {
                Some(unit) => Subject::Unit { units: vec![unit] },
                None => Subject::Entry {
                    units,
                    origin: Origin::File(path),
                    declares: None,
                    unlinked,
                },
            },
            subject => subject,
        }
    }
}

/// Where the program a question is about comes from: a file, or text standing in for one.
///
/// Text carries a label because a diagnostic names its source in the `--> label:line:column` header exactly as it names a path, and `<stdin>` is what a program piped in is called. A file an editor holds unsaved is not text — it is [`Origin::File`] with its path in the overlay, so its `mod` declarations still resolve from its stem directory.
pub enum Origin {
    File(PathBuf),
    Text { label: String, text: String },
}

/// Every diagnostic and goal `subject` reports when lowered, elaborated and judged against the prelude — empty when it compiles. `overlay` is consulted before the disk for every file read, the entry included.
///
/// **A library is asked through the tail `curios test` compiles it with.** It has no written program of its own, so the question is put to the same `()` entry under [`EntryTail::LastUnitTests`], scheduling the last unit's tests; the fold that compiles the units is one and the same, and a unit with no tests gets `Test/main([])`, which costs nothing. A loose file written as a module is asked the same way, as a unit of its own.
///
/// An entry is asked under its written tail alone. A unit's tests are ordinary items and are elaborated whatever the policy, so a fault in one is reported either way; the synthesized tail over them pairs each declaration with its path and raises nothing of its own, which is what a test taking no parameters leaves it with. A policy that checked both tails existed while that was untrue and was removed with the parameters.
///
/// **Every refused declaration is reported, and a declaration reaching a refused one is not.** Elaboration recovers per item, so a file with three mistakes answers with three records in one check, each classified on its own — a refusal beside a goal batch keeps the goal records at their goals — and nothing for a dependent, whose record could only restate the refusal (the rule is `curios-elab`'s README's). A parse failure in one declaration is that declaration's record — the parser resynchronizes at the next item, and the rest of the file is read, elaborated and reported as if the broken one were a refused one — while a failure the item grammar never committed to still ends the file where it stopped, since past that point nothing says what an item is.
///
/// **A lint is reported beside whatever the verdict was**, after it. A lint is decided by the lowering, so a program that lowers has its lints whether elaboration then refused it, left a goal batch, or accepted it; only a program that does not lower — a parse failure, an unresolved name — reports its error alone, since there is nothing to have read the lints off.
///
/// `cache` is consulted for units already built and never written — see the `wonder` module documentation.
pub fn diagnostics(
    budget: u64,
    subject: Subject,
    overlay: &Overlay,
    cache: Option<&Verdicts>,
) -> Vec<Diagnostic> {
    diagnosed(budget, subject, overlay, cache).diagnostics
}

/// [`diagnostics`], with what the subject reached beside the records.
pub fn diagnosed(
    budget: u64,
    subject: Subject,
    overlay: &Overlay,
    cache: Option<&Verdicts>,
) -> Diagnosed {
    let read_only = cache.map(|cache| ReadOnly { cache, overlay });
    let cache = read_only.as_ref().map(|cache| cache as &dyn Cache);

    // Taken before the subject is formed, since it is the one fact about the file asked that no compilation of it says.
    let note = subject.note(overlay);

    let mut diagnosed = match subject.formed(overlay) {
        Subject::Unit { units } => {
            let units = overlaid(units, overlay);
            // A unit has no written entry, so it is asked through the trivial one, which the tests tail then replaces — the subject is the scope's final unit, exactly as `curios test` compiles a library.
            let checked = check_with_units(
                budget,
                &units,
                &Entrypoint::trivial(),
                &RootSource::none(),
                cache,
                EntryTail::LastUnitTests,
                |_| {},
            );
            answered(checked, true)
        }
        Subject::Entry {
            units,
            origin,
            declares,
            ..
        } => match open(origin, declares, overlay) {
            Ok((entrypoint, loader)) => {
                let units = overlaid(units, overlay);
                let checked = check_with_units(
                    budget,
                    &units,
                    &entrypoint,
                    &loader,
                    cache,
                    EntryTail::Authored,
                    |_| {},
                );
                answered(checked, false)
            }
            Err(refusal) => Diagnosed {
                diagnostics: refusal,
                reached: BTreeSet::new(),
            },
        },
    };

    // First, because it says how everything after it was answered.
    if let Some(note) = note {
        diagnosed.diagnostics.insert(0, note);
    }

    diagnosed
}

/// What one check reports: its verdict's records and its lints, read off the unit it was asked about when `is_unit`, and off the entry otherwise.
fn answered(checked: Result<Checked, CompileError>, is_unit: bool) -> Diagnosed {
    match checked {
        Ok(Checked {
            entry,
            unit,
            verdict,
        }) => {
            let Findings { lints, reached } = match is_unit {
                true => unit.unwrap_or_default(),
                false => entry,
            };
            let mut diagnostics = verdict.err().map(of_error).unwrap_or_default();
            diagnostics.extend(lints.into_iter().map(Diagnostic::lint));
            Diagnosed {
                diagnostics,
                reached,
            }
        }
        Err(error) => Diagnosed::refused(error),
    }
}

impl Diagnosed {
    /// A compilation that stopped before its subject was lowered: the error, and nothing reached.
    fn refused(error: CompileError) -> Self {
        Self {
            diagnostics: of_error(error),
            reached: BTreeSet::new(),
        }
    }
}

/// `program` parsed, with the loader its modules resolve through — both reading through `overlay` — or the one diagnostic a program that does not parse gets.
pub(crate) fn open(
    origin: Origin,
    declares: Option<Vec<Qualifier>>,
    overlay: &Overlay,
) -> Result<(Entrypoint, RootSource), Vec<Diagnostic>> {
    let opened = match origin {
        Origin::File(path) => match overlay.get(&path) {
            Some(text) => Entrypoint::overlaid(&path, text).map_err(|error| error.report()),
            None => Entrypoint::opened(&path).map_err(|error| error.report()),
        },
        Origin::Text { label, text } => {
            Entrypoint::supplied(&label, &text).map_err(|error| error.report())
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
        Err(report) => Err(vec![Diagnostic {
            severity: Severity::Error,
            report,
        }]),
    }
}

/// A compile failure as records: the classification the compile path made, on every report it carries — for a mixed failure, the first `failures` reports as errors and the rest as goals, which is how it says which is which.
pub(crate) fn of_error(error: CompileError) -> Vec<Diagnostic> {
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
        .map(|(index, report)| Diagnostic {
            severity: match index < failures {
                true => Severity::Error,
                false => Severity::Goal,
            },
            report,
        })
        .collect()
}

/// Every unit reading through `overlay`.
pub(crate) fn overlaid(units: Vec<RootSource>, overlay: &Overlay) -> Vec<RootSource> {
    units
        .into_iter()
        .map(|unit| unit.with_overlay(overlay.clone()))
        .collect()
}

/// A cache that answers about the text the compilation would read and never records: what a query is allowed to do with the store.
///
/// **A stored unit is verified through the overlay.** A unit is believed on a re-read of every file it was compiled from, and here that re-read takes an open document's text over the disk's (`Verdicts::get_overlaid`), so a unit whose source an editor holds edited is a miss and one whose source is merely open is not. The rule is exact because the record lists what the unit read: a document the unit never read — an executable beside a package's library, in the very directory the library reads from — leaves the hit standing. Refusing on containment instead cost the language server that library on every keystroke in a program file.
pub(crate) struct ReadOnly<'a> {
    pub(crate) cache: &'a Verdicts,
    pub(crate) overlay: &'a Overlay,
}

impl Cache for ReadOnly<'_> {
    fn get(&self, source: &UnitSource<'_>) -> Option<Unit> {
        self.cache.get_overlaid(source, self.overlay)
    }

    /// The slot this tree filed for the unit, however its files have moved since, ahead of whatever the scope offered — a unit this tree filed more recently than the compiler was built is nearer the text being asked about than the archived image, and the image is what is there when nothing was filed.
    ///
    /// A question is what takes a baseline. What it compiles over one is placed and never filed, which is the reading of this cache the method above already states; the store's own cache offers none, so a build compiles a moved unit whole and files what it compiled.
    fn baseline(&self, source: &UnitSource<'_>, offered: Option<Unit>) -> Option<Unit> {
        self.cache.earlier(source).or(offered)
    }

    /// Placed, not filed — and this is why the store itself is held rather than a `dyn Cache`.
    ///
    /// Dropping the write is the whole of what read-only means. Dropping the *placement* with it is a second thing nobody asked for: a slot is addressed after the units placed before it, so a unit missing from that chain shifts every later address by one, and one declined hit becomes a miss for every unit after it. A `dyn Cache` has no way to say the first without the second.
    fn put(&self, source: &UnitSource<'_>, unit: &Unit) {
        self.cache.place(source, unit);
    }
}
