//! The `diagnostics` query: every diagnostic and goal one compilation of a program reports.

use {
    super::{Diagnostic, Severity},
    curios_pipeline::{Cache, CompileError, check_units_with_prelude, check_with_units},
    curios_text::{Entrypoint, Overlay, RootSource, UnitSource},
    curios_unit::Unit,
    std::path::PathBuf,
};

/// What a question is about.
///
/// The transport decides this and the engine only compiles it: a file declared by a package's library is asked about as that whole unit, one declared as an executable's entry is asked about as that origin, and a file no unit declares is asked about standalone — see `curios-package`'s `Membership` for the rule. The engine never probes for a manifest of its own.
pub enum Subject {
    /// A program: the entry compiled against `units`, in the order given.
    Entry {
        units: Vec<RootSource>,
        origin: Origin,
    },
    /// A unit: the last of `units`, compiled against the ones before it. Its verdicts are the answer.
    Unit { units: Vec<RootSource> },
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
/// **One failure stops the compilation, as it does on the compile path.** A parse failure yields one diagnostic and nothing after it; a refused declaration yields its own and nothing after it; only a goal batch yields several, one per `?`. Per-item recovery would be a change to three loops on the shared compile path, and until it lands this is the answer on a broken file: what stopped the compiler, and where.
///
/// `cache` is consulted for units already built and never written — see the `wonder` module documentation.
pub fn diagnostics(
    budget: u64,
    subject: Subject,
    overlay: &Overlay,
    cache: Option<&dyn Cache>,
) -> Vec<Diagnostic> {
    let read_only = cache.map(|cache| ReadOnly { cache, overlay });
    let cache = read_only.as_ref().map(|cache| cache as &dyn Cache);

    let checked = match subject {
        Subject::Unit { units } => {
            let units = overlaid(units, overlay);
            check_units_with_prelude(budget, &units, cache, |_| {})
        }
        Subject::Entry { units, origin } => {
            let (entrypoint, loader) = match open(origin, overlay) {
                Ok(opened) => opened,
                Err(refusal) => return refusal,
            };
            let units = overlaid(units, overlay);

            check_with_units(budget, &units, &entrypoint, &loader, cache, |_| {}).map(|_module| ())
        }
    };

    match checked {
        Ok(()) => Vec::new(),
        Err(error) => of_error(error),
    }
}

/// `program` parsed, with the loader its modules resolve through — both reading through `overlay` — or the one diagnostic a program that does not parse gets.
pub(crate) fn open(
    origin: Origin,
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
        Ok((entrypoint, loader, _source)) => Ok((entrypoint, loader.with_overlay(overlay.clone()))),
        Err(report) => Err(vec![Diagnostic {
            severity: Severity::Error,
            report,
        }]),
    }
}

/// A compile failure as records: the classification the compile path made, on every report it carries.
pub(crate) fn of_error(error: CompileError) -> Vec<Diagnostic> {
    let (severity, reports) = match error {
        CompileError::Incomplete(reports) => (Severity::Goal, reports),
        CompileError::Failure(reports) => (Severity::Error, reports),
    };

    reports
        .into_iter()
        .map(|report| Diagnostic { severity, report })
        .collect()
}

/// Every unit reading through `overlay`.
pub(crate) fn overlaid(units: Vec<RootSource>, overlay: &Overlay) -> Vec<RootSource> {
    units
        .into_iter()
        .map(|unit| unit.with_overlay(overlay.clone()))
        .collect()
}

/// A cache that answers about the disk alone and never records: what a query is allowed to do with the store.
///
/// **A unit the overlay reaches is a miss.** A stored unit is believed on a re-read of every file it was compiled from, and that re-read knows only the disk — so a unit whose source an editor holds unsaved would be handed back against text nobody asked about, and the document being edited is exactly the one whose verdicts were asked for. Refusing the hit costs that one unit's compilation; every unit the overlay does not reach still comes from the store, which is what keeps a check per keystroke from rebuilding a package's dependencies.
pub(crate) struct ReadOnly<'a> {
    pub(crate) cache: &'a dyn Cache,
    pub(crate) overlay: &'a Overlay,
}

impl Cache for ReadOnly<'_> {
    fn get(&self, source: &UnitSource<'_>) -> Option<Unit> {
        match self.overlay.reaches(&source.directories()) {
            true => None,
            false => self.cache.get(source),
        }
    }

    fn put(&self, _source: &UnitSource<'_>, _unit: &Unit) {}
}
