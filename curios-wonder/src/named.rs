//! The `at` and `declarations` queries: what a file names, read off its subject as the lowering left it.
//!
//! **Asked of the lowering, and of no checker.** A written name is resolved where it is lowered, and a file being edited seldom checks. So the fold compiles what the subject is compiled against and lowers the subject itself, no further (`Fold::lowered`, `Fold::lowered_unit`): a subject that would be refused answers as one that compiles, and only one that does not lower has nothing to read.
//!
//! **A name is answered with where it is declared, as a place in a file that holds it.** A declaration of the subject's own stands in text this question read, and one of a unit in scope in the text that unit was compiled from, which is what the disk or an editor holds: a store's hit is believed on a re-read of every file, and a session's unit was compiled from the overlay. The units a compiler was built with are believed on no read, so a declaration of one is placed where its file still holds the text it was read in, and is left unplaced otherwise: under an installed release their sources are another machine's.
//!
//! **One name stands at an offset** ([`Named::at`]): the narrowest that holds it. The rule is stated here, over the records, since both transports ask it and a server asks it of records alone.

use {
    crate::{Diagnosis, Overlaid, Subject, of_error, open, overlaid},
    curios_core::{Anchors, Declared, Global},
    curios_pipeline::{Cache, Fold},
    curios_text::{Overlay, PreparedText, identity},
    curios_unit::Unit,
    curios_utilities::{Source, Span},
    curios_verdicts::Verdicts,
    std::{
        cell::RefCell,
        collections::{BTreeMap, HashMap},
        fs,
        path::{Path, PathBuf},
        ptr,
        sync::Arc,
    },
};

/// One name a file writes, as a question about a place answers it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Named {
    /// Where the name is written, in the file asked about.
    pub written: Span,
    /// The declaration it names: by the path it is declared at, or for a witness, which has none, by its head as written.
    pub name: String,
    /// What the declaration is declared as.
    pub declared: Declared,
    /// Whether `written` is where the declaration is named, rather than a use of it.
    pub declares: bool,
    /// Where the declaration's name is written, where a file holds it — see the module documentation.
    pub declaration: Option<Span>,
}

impl Named {
    /// The name among `names` that stands at `offset` of their file, an offset at a name's end being at the name: the narrowest that holds it, and a use before the definition named around it, as the concept a `satisfy` names is before the witness its head declares.
    pub fn at<'a>(names: impl IntoIterator<Item = &'a Named>, offset: usize) -> Option<&'a Named> {
        names
            .into_iter()
            .filter(|named| named.written.start <= offset && offset <= named.written.end)
            .min_by_key(|named| {
                let written = &named.written;

                (
                    written.end - written.start,
                    offset == written.end,
                    named.declares,
                )
            })
    }
}

/// One text of a file, a source declaration's or the term a program closes with, and every name written in it, in the order they are written.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Written {
    /// Where the text lies in the file.
    pub text: Span,
    pub names: Vec<Named>,
}

/// Every text of `file` that `subject` was lowered from, with the names each writes, in the order they are written — none for a file the subject is not written in. `overlay` and `cache` behave as they do for `diagnostics`, and nothing is filed of the subject itself, which is not compiled.
///
/// The refusal is a subject that has nothing to read: one that does not parse or lower, or whose scope could not be compiled.
pub fn written(
    budget: u64,
    subject: Subject,
    file: &Path,
    overlay: &Overlay,
    cache: Option<&Verdicts>,
) -> Result<Vec<Written>, Vec<Diagnosis>> {
    let store = cache.map(|cache| Overlaid::over(cache, overlay));
    let through = store.as_ref().map(|store| store as &dyn Cache);
    let reading = Reading {
        file: identity(file),
        overlay,
        lying: RefCell::new(HashMap::new()),
        standing: RefCell::new(BTreeMap::new()),
    };

    match subject.formed(overlay) {
        Subject::Unit { units } => {
            let units = overlaid(units, overlay);

            Fold::new(budget, &units, through)
                .lowered_unit(|_| {}, |scope, text| reading.read(scope, text, None))
        }
        Subject::Entry {
            units,
            origin,
            declares,
            ..
        } => {
            let (entrypoint, loader) = open(origin, declares, overlay)?;
            let units = overlaid(units, overlay);

            Fold::new(budget, &units, through).lowered(
                &entrypoint,
                &loader,
                |_| {},
                |scope, text, entry| reading.read(scope, text, Some(entry)),
            )
        }
    }
    .map_err(of_error)
}

/// One question's reads of the disk, each asked once: which source is the file asked about, and which file still holds the text a unit the compiler was built with read.
struct Reading<'a> {
    /// The file asked about, under the one spelling two paths to it share.
    file: PathBuf,
    overlay: &'a Overlay,
    /// Whether each source lies in the file asked about, by the source's own allocation: a unit's texts share their file's.
    lying: RefCell<HashMap<*const Source, bool>>,
    /// Whether the file at each path holds the text a source of that path was read as.
    standing: RefCell<BTreeMap<PathBuf, bool>>,
}

impl Reading<'_> {
    /// What the file asked about writes in the subject `text` is the lowering of, compiled against `scope`, with the term its program closes with where it is one.
    fn read(
        &self,
        scope: &[&Unit],
        text: &PreparedText,
        entry: Option<&curios_core::Entrypoint>,
    ) -> Vec<Written> {
        text.texts(entry, |source| self.lies(source))
            .iter()
            .map(|passage| Written {
                text: passage.span().clone(),
                names: passage
                    .mentions()
                    .into_iter()
                    .filter_map(|mention| {
                        let (declared, declaration) = match mention.declared {
                            Some(declared) => (declared, Some(mention.span.clone())),
                            None => self.declaration(scope, text, &mention.name)?,
                        };

                        Some(Named {
                            name: match mention.name.qualifier() {
                                Some(path) => path.join(),
                                None => {
                                    let written = &mention.span;

                                    written.source.text[written.start..written.end].to_string()
                                }
                            },
                            written: mention.span,
                            declared,
                            declares: mention.declared.is_some(),
                            declaration,
                        })
                    })
                    .collect(),
            })
            .collect()
    }

    /// What `name` is declared as and where, by the unit that declares it: the subject's own, then the units in scope. `None` for a name no lowered item declares, which is one an item the parser could not read declared.
    fn declaration(
        &self,
        scope: &[&Unit],
        own: &PreparedText,
        name: &Global,
    ) -> Option<(Declared, Option<Span>)> {
        if let Some(declared) = own.declared_as(name) {
            return Some((declared, Anchors::label(&[own.anchors()], name)));
        }

        let (unit, declared) = scope
            .iter()
            .find_map(|unit| Some((unit, unit.text().declared_as(name)?)))?;
        let placed = Anchors::label(&[unit.text().anchors()], name)
            .filter(|span| !unit.fixed() || self.stands(span));

        Some((declared, placed))
    }

    /// Whether `source` is the file asked about.
    fn lies(&self, source: &Source) -> bool {
        *self
            .lying
            .borrow_mut()
            .entry(ptr::from_ref(source))
            .or_insert_with(|| {
                source
                    .path
                    .as_deref()
                    .is_some_and(|path| identity(path) == self.file)
            })
    }

    /// Whether the file `span` names still holds the text the span was taken in, an editor's text standing for the disk's.
    fn stands(&self, span: &Span) -> bool {
        let source: &Arc<Source> = &span.source;
        let Some(path) = &source.path else {
            return false;
        };

        *self
            .standing
            .borrow_mut()
            .entry(path.clone())
            .or_insert_with(|| match self.overlay.get(path) {
                Some(held) => held == source.text,
                None => fs::read_to_string(path).is_ok_and(|held| held == source.text),
            })
    }
}
