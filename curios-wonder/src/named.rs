//! The `at` and `declarations` queries: what a file names, read off its subject as the lowering left it.
//!
//! **Asked of the lowering, and of no checker.** A written name is resolved where it is lowered, and a file being edited seldom checks. So the fold compiles what the subject is compiled against and lowers the subject itself, no further (`Fold::lowered`, `Fold::lowered_unit`): a subject that would be refused answers as one that compiles, and only one that does not lower has nothing to read.
//!
//! **A name is answered with where it is declared, as a place in a file that holds it.** A declaration of the subject's own stands in text this question read, and one of a unit in scope in the text that unit was compiled from, which is what the disk or an editor holds: a store's hit is believed on a re-read of every file, and a session's unit was compiled from the overlay. A unit the compiler was built with was read in a checkout no question names, or on another machine, so its sources hold their names and no file, and a declaration of one is left unplaced: it is answered by the path it is declared at, its head and its prose read off its text all the same.
//!
//! **A declaration's head and prose are read off its own text**, which the unit that declares it read and its record still holds ([`Declaration::heading`]): a declaration is answered with them whether or not a file holds it, the library under an installed release included, and whether or not a documentation page shows it.
//!
//! **One name stands at an offset** ([`Named::at`]): the narrowest that holds it. The rule is stated here, over the records, since both transports ask it and a server asks it of records alone.
//!
//! **A declaration asked about by its path is answered off the unit that declares it** ([`described`]), its type as that unit was elaborated: the subject is lowered as a question about a place lowers it, which finds the unit, and is elaborated only where it declares the name itself, so a file being edited never stands between a question and a declaration of a unit in scope.

use {
    crate::{Diagnosis, Overlaid, Subject, of_error, open, overlaid, tested_last},
    curios_common::{Plicity, Qualifier, Source, Span},
    curios_core::{
        Anchors, Certification, Declared, Definitions, Entrypoint, Global, HeadKey, Imports,
        Intrinsic, Lexicon, Places, Spellings, Subterm, Term, Totality, UnitPaths, WitnessKey,
    },
    curios_document::{Documentation, ModuleDocumentation},
    curios_module::{Heading, Overlay, PreparedText, RootSource, SYNTAX, heading, identity},
    curios_pipeline::{
        Cache, CompileError, EntryTail, Fold, Progress, prelude_documentation, spelling_at,
    },
    curios_surface::{IntrinsicForm, Spellable, intrinsic_form, intrinsic_global, spell},
    curios_unit::Unit,
    curios_verdict::Verdicts,
    std::{
        cell::RefCell,
        collections::HashMap,
        path::{Path, PathBuf},
        ptr,
        rc::Rc,
    },
};

/// One name a file writes, as a question about a place answers it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Named {
    /// Where the name is written, in the file asked about.
    pub written: Span,
    /// The declaration it names: by the path it is declared at, or for a witness, which has none, by its head as written.
    pub name: String,
    /// The declaration it names, as the lowering resolved it.
    pub named: Global,
    /// What the declaration is declared as.
    pub declared: Declared,
    /// Whether `written` is where the declaration is named, rather than a use of it.
    pub declares: bool,
    /// Where the declaration is written, for one a name was written for.
    pub declaration: Option<Declaration>,
}

/// Where a declaration is written, in the text the unit that declares it read.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Declaration {
    /// The text of the source declaration that declares it: its own, or its owner's for a constructor or a method.
    pub text: Span,
    /// Where its name stands in that text.
    pub label: Span,
    /// Whether the declaration has a place to open: the subject's own always, and a unit in scope's where a file holds its text — see the module documentation. Where it has none, the declaration is read and answered by its path.
    pub held: bool,
}

impl Declaration {
    /// Where the declaration's name is written, as a place in a file that holds it.
    pub fn place(&self) -> Option<&Span> {
        self.held.then_some(&self.label)
    }

    /// The declaration's whole text as its author wrote it, read off the text its unit read: its owner's for a constructor or a method.
    pub fn written(&self) -> &str {
        &self.text.source.text()[self.text.start..self.text.end]
    }

    /// The declaration's head as its author wrote it, and the prose above it: read off its own text by the printers a documentation page is printed by (`curios_module::heading`), so it is what a page says of it, and is said of a declaration no page shows. Parsed where it is asked for, the one declaration's text.
    pub fn heading(&self) -> Option<Heading> {
        let text = &self.text;

        heading(
            &text.source.text()[text.start..text.end],
            self.label.start.checked_sub(text.start)?,
        )
    }
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

/// The declaration the name written at `line` and `column` of `file` names, as [`described`] answers one but for its type and its totality: what the lowering says of it, the subject lowered and elaborated nowhere, so a file that does not check answers as one that does. `None` where no name stands there, and where the subject is not written in `file`. `overlay` and `cache` behave as they do for `diagnostics`.
///
/// The refusal is a subject that has nothing to read: one that does not parse or lower, or whose scope could not be compiled.
pub fn placed(
    budget: u64,
    subject: Subject,
    file: &Path,
    (line, column): (usize, usize),
    overlay: &Overlay,
    cache: Option<&Verdicts>,
    progress: &mut dyn FnMut(Progress<'_>),
) -> Result<Option<Described>, Vec<Diagnosis>> {
    let reading = Reading::of(file);

    lowering(
        budget,
        subject,
        overlay,
        cache,
        progress,
        |scope, own, entry| {
            let texts = reading.read(scope, own, entry);
            let offset = texts.first()?.text.source.offset_of(line, column)?;
            let named = Named::at(texts.iter().flat_map(|text| &text.names), offset)?;
            let (said, _) = said_of(scope, own, named.named)?;

            Some(Described {
                asked: named.name.clone(),
                path: named.name.clone(),
                declared: said.declared,
                public: said.public,
                declaration: said.declaration,
                type_: None,
                totality: None,
                members: said.members,
                witnesses: said.witnesses,
            })
        },
    )
}

/// `subject` lowered, no further, handed to `then` with the units in scope, its own lowering and the term its program closes with where it is one: what a question about what a file writes reads, asked of no checker.
fn lowering<T>(
    budget: u64,
    subject: Subject,
    overlay: &Overlay,
    cache: Option<&Verdicts>,
    progress: &mut dyn FnMut(Progress<'_>),
    then: impl FnOnce(&[&Unit], &PreparedText, Option<&Entrypoint>) -> T,
) -> Result<T, Vec<Diagnosis>> {
    let store = cache.map(|cache| Overlaid::over(cache, overlay));
    let through = store.as_ref().map(|store| store as &dyn Cache);

    match subject.formed(overlay) {
        Subject::Unit { units } => {
            let units = tested_last(overlaid(units, overlay));

            Fold::new(budget, &units, through)
                .lowered_unit(&mut *progress, |scope, own| then(scope, own, None))
        }
        Subject::Entry {
            units,
            origin,
            declares,
            package,
        } => {
            let (entrypoint, loader) = open(origin, declares, package.as_ref(), overlay)?;
            let units = overlaid(units, overlay);

            Fold::new(budget, &units, through).lowered(
                &entrypoint,
                &loader.tested(),
                &mut *progress,
                |scope, own, entry| then(scope, own, Some(entry)),
            )
        }
    }
    .map_err(of_error)
}

/// One question's reads of what a file writes, each asked once: which source is the file asked about.
pub struct Reading {
    /// The file asked about, under the one spelling two paths to it share.
    file: PathBuf,
    /// What was asked about, as it was written: the name a text no file holds is asked about by, `<stdin>`.
    asked: PathBuf,
    /// Whether each source lies in the file asked about, by the source's own allocation: a unit's texts share their file's.
    lying: RefCell<HashMap<*const Source, bool>>,
}

impl Reading {
    /// What reads the file at `file` as a question about what it writes.
    pub fn of(file: &Path) -> Self {
        Self {
            file: identity(file),
            asked: file.to_path_buf(),
            lying: RefCell::new(HashMap::new()),
        }
    }

    /// What the file asked about writes in the subject `text` is the lowering of, compiled against `scope`, with the term its program closes with where it is one.
    pub fn read(
        &self,
        scope: &[&Unit],
        text: &PreparedText,
        entry: Option<&Entrypoint>,
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
                            Some(declared) => (
                                declared,
                                Some(Declaration {
                                    text: passage.span().clone(),
                                    label: mention.span.clone(),
                                    held: true,
                                }),
                            ),
                            None => found(scope, text, &mention.name)
                                .map(|found| (found.declared, found.declaration))?,
                        };

                        Some(Named {
                            name: match mention.name.qualifier() {
                                Some(path) => path.join(),
                                None => {
                                    let written = &mention.span;

                                    written.source.text()[written.start..written.end].to_string()
                                }
                            },
                            written: mention.span,
                            named: mention.name,
                            declared,
                            declares: mention.declared.is_some(),
                            declaration,
                        })
                    })
                    .collect(),
            })
            .collect()
    }

    /// Whether `source` is what was asked about: the file, under its one spelling, and for a text no file holds, the name it was asked by.
    fn lies(&self, source: &Source) -> bool {
        *self
            .lying
            .borrow_mut()
            .entry(ptr::from_ref(source))
            .or_insert_with(|| match source.file() {
                Some(file) => identity(file) == self.file,
                None => source
                    .name()
                    .is_some_and(|name| Path::new(name.as_str()) == self.asked),
            })
    }
}

/// Where a name is declared: what it is declared as, where it is written, and the lowering that declares it, beside the unit of the scope that holds that lowering — none for the subject's own.
struct Found<'s> {
    declared: Declared,
    declaration: Option<Declaration>,
    lowered: &'s PreparedText,
    unit: Option<&'s Unit>,
}

/// What `name` is declared as and where, by the unit that declares it: the subject's own lowering `own`, placed always, then the units in scope, each placed where a file holds its text. `None` for a name no lowered item declares, which is one an item the parser could not read declared.
fn found<'s>(scope: &[&'s Unit], own: &'s PreparedText, name: &Global) -> Option<Found<'s>> {
    let written = |anchors: &Anchors, own: bool| {
        let (text, label) = anchors.written(name)?;
        let held = own || label.source.file().is_some();

        Some(Declaration { text, label, held })
    };

    if let Some(declared) = own.declared_as(name) {
        return Some(Found {
            declared,
            declaration: written(own.anchors(), true),
            lowered: own,
            unit: None,
        });
    }

    // A unit read from Core text holds no lowering, and so declares nothing a name is found by.
    let (unit, lowered, declared) = scope.iter().find_map(|unit| {
        let lowered = unit.text()?;
        Some((*unit, lowered, lowered.declared_as(name)?))
    })?;

    Some(Found {
        declared,
        declaration: written(lowered.anchors(), false),
        lowered,
        unit: Some(unit),
    })
}

/// A declaration asked about by its path ([`described`]): what it is declared as and where, as a question about a place answers a name written there, the type it was elaborated at, and what the lowering says of it beside.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Described {
    /// The path it was asked by.
    pub asked: String,
    /// The path it is declared at: the one asked, or that of the declaration a module exports there.
    pub path: String,
    pub declared: Declared,
    /// Whether it is written `pub`, where it is a binding of its module.
    pub public: Option<bool>,
    /// Where it is written, for one a name was written for.
    pub declaration: Option<Declaration>,
    /// The type it was elaborated at, spelled for a reader standing at it; none where the unit that declares it holds no definition under the name.
    pub type_: Option<String>,
    /// Whether a definition terminates on every input, as the certifier recorded it of the unit that declares it: `Partial` where it is not known to, and none for anything but a definition, or where the record files none under the name.
    pub totality: Option<Totality>,
    /// Its members by name, in the order they are declared: a family's constructors, a structure's fields, a concept's methods.
    pub members: Vec<String>,
    /// The witnesses in scope it takes part in, each unit's together in scope order and by place within it: a concept's, and a type's — none where it is neither.
    pub witnesses: Option<Vec<Witnessed>>,
}

/// One witness in scope a declaration takes part in ([`Described::witnesses`]).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Witnessed {
    /// Where its head is written, as a place in a file that holds it.
    pub place: Option<Span>,
    /// The module that declares it.
    pub module: String,
    /// What it says of the declaration: a concept's witness by the heads it is keyed on, and a type's by the concept it satisfies, with the heads it is keyed on where that concept has more than one — each name as a reader outside every module writes it: the shortest path a module exports it at that every module may write.
    pub keyed: String,
}

/// What the lowering says of a declaration: all a [`Described`] says but its type and how it was asked for.
struct Lowered {
    name: Global,
    declared: Declared,
    public: Option<bool>,
    declaration: Option<Declaration>,
    members: Vec<String>,
    witnesses: Option<Vec<Witnessed>>,
}

/// What `asked` names, declared, where, at what type, and with which members and witnesses, by the unit of the subject's scope that declares it — the subject itself among them. A path names a declaration at it, or one some module exports there for a reader outside every module ([`PreparedText::named_by`]). The subject is lowered, which finds the unit, and elaborated only where it declares the name itself: a subject that does not elaborate then stops the question as it stops a build, and a name a unit in scope declares is answered off that unit whatever the subject is. `None` where the path names nothing in scope. `overlay` and `cache` behave as they do for `diagnostics`.
pub fn described(
    budget: u64,
    subject: Subject,
    asked: &Qualifier,
    overlay: &Overlay,
    cache: Option<&Verdicts>,
    progress: &mut dyn FnMut(Progress<'_>),
) -> Result<Option<Described>, Vec<Diagnosis>> {
    let store = cache.map(|cache| Overlaid::over(cache, overlay));
    let through = store.as_ref().map(|store| store as &dyn Cache);

    match subject.formed(overlay) {
        Subject::Unit { units } => described_unit(
            budget,
            &tested_last(overlaid(units, overlay)),
            asked,
            through,
            progress,
        ),
        Subject::Entry {
            units,
            origin,
            declares,
            package,
        } => {
            let (entrypoint, loader) = open(origin, declares, package.as_ref(), overlay)?;

            described_entry(
                budget,
                &overlaid(units, overlay),
                &entrypoint,
                &loader.tested(),
                asked,
                through,
                progress,
            )
        }
    }
}

/// What `asked` names in the scope of the library that is the last of `units`, compiled over `through` as [`described`] answers it: off the unit in scope that declares it, or off the library's own module, elaborated, where the library declares it.
pub fn described_unit(
    budget: u64,
    units: &[RootSource],
    asked: &Qualifier,
    through: Option<&dyn Cache>,
    progress: &mut dyn FnMut(Progress<'_>),
) -> Result<Option<Described>, Vec<Diagnosis>> {
    let said = match Fold::new(budget, units, through)
        .lowered_unit(&mut *progress, |scope, own| said_by(asked, scope, own))
        .map_err(of_error)?
    {
        None => return Ok(None),
        Some(Ok(described)) => return Ok(Some(described)),
        Some(Err((said, _))) => said,
    };

    Fold::new(budget, units, through)
        .units(&mut *progress, |prelude, produced| {
            Ok(produced.split_last().map(|(last, before)| {
                let scope = prelude
                    .iter()
                    .map(|unit| unit.lexicon())
                    .chain(before.iter().map(Unit::lexicon))
                    .collect::<Vec<_>>();

                spelled(
                    asked,
                    said,
                    last.core(),
                    Some((last.lexicon(), last.places())),
                    &scope,
                    Some(last.certification()),
                )
            }))
        })
        .map_err(of_error)
}

/// The declaration `asked` names, by the subject's own lowering `own` and the units in `scope`, beside the lowering that declares it and the unit in scope it is, none for the subject's own: what a rung narrowed to a declaration reads its part off. `None` for a name no lowered item declares.
pub fn declared_where<'s>(
    scope: &[&'s Unit],
    own: &'s PreparedText,
    asked: &Qualifier,
) -> Option<(Global, &'s PreparedText, Option<&'s Unit>)> {
    let name = resolved(scope, own, asked);
    let found = found(scope, own, &name)?;

    Some((name, found.lowered, found.unit))
}

/// What `asked` names in the scope of the program `entrypoint`, compiled against `units` over `through` as [`described`] answers it: off the unit in scope that declares it, or off the program's own lowering, elaborated, where the program declares it. `loader` is the program's source as a question reads it, [`tested`](RootSource::tested).
pub fn described_entry(
    budget: u64,
    units: &[RootSource],
    entrypoint: &curios_surface::Entrypoint,
    loader: &RootSource,
    asked: &Qualifier,
    through: Option<&dyn Cache>,
    progress: &mut dyn FnMut(Progress<'_>),
) -> Result<Option<Described>, Vec<Diagnosis>> {
    let (said, scope) = match Fold::new(budget, units, through)
        .lowered(entrypoint, loader, &mut *progress, |scope, own, _| {
            said_by(asked, scope, own)
        })
        .map_err(of_error)?
    {
        None => return Ok(None),
        Some(Ok(described)) => return Ok(Some(described)),
        Some(Err(own)) => own,
    };
    let checked = Fold::new(budget, units, through)
        .check(entrypoint, loader, EntryTail::Program, |step| {
            progress(step)
        })
        .and_then(|checked| checked.verdict)
        .map_err(of_error)?;
    let scope = scope.iter().collect::<Vec<_>>();

    Ok(Some(spelled(
        asked,
        said,
        &checked.program.definitions,
        None,
        &scope,
        Some(&checked.certification),
    )))
}

/// What the lowering says of `asked` over `scope` and the subject's own lowering `own`: the answer off the unit in scope that declares it, or what the lowering says of the subject's own declaration with the lexicons its scope keeps, for its elaboration to type; `None` where nothing is declared at it.
fn said_by(
    asked: &Qualifier,
    scope: &[&Unit],
    own: &PreparedText,
) -> Option<Result<Described, (Lowered, Vec<Lexicon>)>> {
    let (said, unit) = said_of(scope, own, resolved(scope, own, asked))?;

    Some(match unit {
        Some(unit) => {
            let others = scope
                .iter()
                .filter(|other| !ptr::eq(**other, unit))
                .map(|other| other.lexicon())
                .collect::<Vec<_>>();

            Ok(spelled(
                asked,
                said,
                unit.core(),
                Some((unit.lexicon(), unit.places())),
                &others,
                Some(unit.certification()),
            ))
        }
        None => Err((
            said,
            scope
                .iter()
                .map(|unit| unit.lexicon().clone())
                .collect::<Vec<_>>(),
        )),
    })
}

/// What a consumer reaches through the module `asked` names, as the documentation record of the unit holding it states it ([`ModuleDocumentation`]): a unit in scope whose mount is the module's root, its record made over the units before it as `curios document` makes one ([`Fold::documentation`]), and otherwise the prelude's, which the image carries. A unit in scope comes first, so a package named `std` answers its own modules where the archived library stands behind it. `None` where no module a consumer reaches stands at the path. `overlay` and `cache` behave as they do for `diagnostics`.
pub fn exported(
    budget: u64,
    subject: Subject,
    asked: &Qualifier,
    overlay: &Overlay,
    cache: Option<&Verdicts>,
    progress: &mut dyn FnMut(Progress<'_>),
) -> Result<Option<Exported>, CompileError> {
    let root = Qualifier::from([asked.head()]);
    let module = |record: Documentation| {
        record
            .modules
            .into_iter()
            .find(|module| module.path == *asked)
    };
    let units = match subject {
        Subject::Entry { units, .. } | Subject::Unit { units } => units,
    };
    let store = cache.map(|cache| Overlaid::over(cache, overlay));
    let through = store.as_ref().map(|store| store as &dyn Cache);
    let Some(holding) = units
        .iter()
        .position(|unit| unit.mounts().iter().any(|mount| mount.prefix == root))
    else {
        let Some(module) = prelude_documentation()
            .into_iter()
            .find(|record| record.prefix == root)
            .and_then(module)
        else {
            return Ok(None);
        };
        // The prelude's own unit holding the root reads its names, the others standing before it.
        let declared = Fold::new(budget, &[], through).units(progress, |prelude, _| {
            let own = prelude
                .iter()
                .find(|unit| unit.mounts().iter().any(|mount| mount.prefix == root))
                .and_then(|unit| unit.text())
                .expect("a prelude record is a prelude unit's");
            Ok(declared_by(prelude, own, &module))
        })?;

        return Ok(Some(Exported { module, declared }));
    };
    let mut units = overlaid(units, overlay);
    units.truncate(holding + 1);
    let Some(module) = module(Fold::new(budget, &units, through).documentation(&mut *progress)?)
    else {
        return Ok(None);
    };
    let declared = Fold::new(budget, &units, through)
        .lowered_unit(progress, |scope, own| declared_by(scope, own, &module))?;

    Ok(Some(Exported { module, declared }))
}

/// What a consumer reaches through a module ([`exported`]): its documentation record, and what each name the record lists is declared as, in the order it lists them, read as `--name` reads one.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Exported {
    pub module: ModuleDocumentation,
    pub declared: Vec<Declared>,
}

/// What each name `module` lists is declared as, by the path a consumer writes it at, read off `own` and the units in `scope` as [`described`] reads one: a witness, which has no name to be asked by, as what its record says it is.
fn declared_by(scope: &[&Unit], own: &PreparedText, module: &ModuleDocumentation) -> Vec<Declared> {
    module
        .declarations
        .iter()
        .map(|declaration| match declaration.kind {
            curios_document::Kind::Witness => Declared::Witness,
            _ => {
                let asked = Qualifier::from(
                    declaration
                        .home
                        .iter()
                        .chain([declaration.name.as_str()])
                        .collect::<Vec<_>>(),
                );
                found(scope, own, &resolved(scope, own, &asked))
                    .unwrap_or_else(|| panic!("{} is exposed and declared nowhere", asked.join()))
                    .declared
            }
        })
        .collect()
}

/// The declaration `asked` names, a path read as a reader outside every module writes one: a declaration at it, or one some module exports there ([`PreparedText::named_by`]), by `own` and the units in `scope`.
fn resolved(scope: &[&Unit], own: &PreparedText, asked: &Qualifier) -> Global {
    let texts = scope
        .iter()
        .filter_map(|unit| unit.text())
        .collect::<Vec<_>>();

    Global::Authored(own.named_by(&texts, asked).unwrap_or(*asked))
}

/// What the lowering says of `name`: what it is declared as and where, whether it is written `pub`, its members and the witnesses in scope it takes part in — by the subject's own lowering `own`, then the units in `scope` — beside the unit in scope that declares it, none for the subject's own. `None` for a name no lowered item declares.
fn said_of<'s>(
    scope: &[&'s Unit],
    own: &'s PreparedText,
    name: Global,
) -> Option<(Lowered, Option<&'s Unit>)> {
    let texts = scope
        .iter()
        .filter_map(|unit| unit.text())
        .collect::<Vec<_>>();
    let found = found(scope, own, &name)?;
    let spellings = own.spellings(&texts);
    let witnesses = witnesses(scope, own, &spellings, &found, &name);

    Some((
        Lowered {
            name,
            declared: found.declared,
            public: name
                .qualifier()
                .and_then(|path| found.lowered.is_public(path)),
            declaration: found.declaration,
            members: members(&found.lowered.lexicon(), &name, found.declared),
            witnesses,
        },
        found.unit,
    ))
}

/// The members of `name`, declared as `declared`, by the names `lexicon` writes them by, in the order they are declared: a family's constructors, a structure's fields, a concept's methods — a field that is a superclass edge is none — and a field written without a label by its position. None for anything else.
fn members(lexicon: &Lexicon, name: &Global, declared: Declared) -> Vec<String> {
    let members = &lexicon.members;

    match declared {
        Declared::Inductive => members
            .payloads
            .get(name)
            .into_iter()
            .flat_map(|payloads| payloads.iter().map(|(tag, _)| tag.to_string()))
            .collect(),
        Declared::Struct | Declared::Concept => members
            .fields
            .get(name)
            .into_iter()
            .flat_map(|fields| fields.iter().enumerate())
            .filter(|(_, (mark, _))| *mark != Plicity::Witness)
            .map(|(position, (_, label))| label.clone().unwrap_or_else(|| position.to_string()))
            .collect(),
        _ => Vec::new(),
    }
}

/// The witnesses the units in `scope`, then `own`, declare that `name`, `found` where it is declared, takes part in, each unit's by place, placed where a file holds its text, the subject's own always: a concept's own, and a type's — those keyed on the head it stands for, a nominal type's name, or what the value of a definition whose type is a sort is keyed by. None for anything else. Each name in them is written as a reader outside every module writes it, over `spellings` ([`absolute`]).
fn witnesses(
    scope: &[&Unit],
    own: &PreparedText,
    spellings: &Spellings,
    found: &Found<'_>,
    name: &Global,
) -> Option<Vec<Witnessed>> {
    let head = match found.declared {
        Declared::Concept => None,
        Declared::Inductive | Declared::Struct => Some(HeadKey::Nominal(*name)),
        Declared::Type => Some(found.lowered.lowered(name).and_then(|item| {
            let definition = item
                .definitions()
                .into_iter()
                .find(|definition| definition.name == *name)?;
            let result = match &*definition.type_ {
                Subterm::FuncType(func) => func.telescope.terminal(),
                _ => &definition.type_,
            };

            matches!(&**result, Subterm::Sort(_))
                .then(|| HeadKey::of_whnf(&definition.body))
                .flatten()
        })?),
        _ => return None,
    };
    let outside = Spellings {
        imports: Imports::default(),
        units: spellings
            .units
            .iter()
            .map(|unit| UnitPaths {
                own: false,
                ..unit.clone()
            })
            .collect(),
        closed: spellings.closed.clone(),
    };
    let heads = |key: &WitnessKey| {
        let heads = key
            .heads()
            .iter()
            .map(|head| match head {
                HeadKey::Nominal(nominal) => absolute(&outside, nominal),
                carrier => carrier_former(carrier)
                    .map_or_else(|| carrier.to_string(), |former| absolute(&outside, &former)),
            })
            .collect::<Vec<_>>();

        match heads.as_slice() {
            [head] => head.clone(),
            _ => format!("({})", heads.join(", ")),
        }
    };
    let keyed = |concept: &Global, key: &WitnessKey| match (&head, key.heads()) {
        (None, _) => (concept == name).then(|| heads(key)),
        (Some(head), _) if !key.heads().contains(head) => None,
        (_, [_]) => Some(absolute(&outside, concept)),
        _ => Some(format!("{}{}", absolute(&outside, concept), heads(key))),
    };
    // Each unit's places, the lowering its witnesses are written in where it holds one, and whether it is the subject's own.
    let units = scope
        .iter()
        .map(|unit| (unit.places(), unit.text(), false))
        .chain([(own.places(), Some(own), true)]);

    Some(
        units
            .flat_map(|(places, lowered, owned)| {
                // In the order they are written, placed or not, then by module and by what each is keyed on.
                let mut witnessed = places
                    .witnesses()
                    .filter_map(|witness| {
                        let (concept, key) = witness.witness_key()?;
                        let Global::Witness(id) = witness else {
                            return None;
                        };
                        let written = lowered
                            .and_then(|lowered| lowered.anchors().written(witness))
                            .map(|(_, label)| label);
                        let order = written.as_ref().map(|label| {
                            (
                                label.source.name().cloned(),
                                label.source.file().map(Path::to_path_buf),
                                label.start,
                            )
                        });
                        let witnessed = Witnessed {
                            place: written.filter(|label| owned || label.source.file().is_some()),
                            module: id.module().join(),
                            keyed: keyed(&concept, &key)?,
                        };

                        Some((
                            (order, witnessed.module.clone(), witnessed.keyed.clone()),
                            witnessed,
                        ))
                    })
                    .collect::<Vec<_>>();
                witnessed.sort_by(|(one, _), (other, _)| one.cmp(other));
                witnessed.into_iter().map(|(_, witnessed)| witnessed)
            })
            .collect(),
    )
}

/// How a reader outside every module, with nothing imported, writes `global` over `spellings`: the shortest of its own path and the paths a module exports it at that every module may write (`curios_surface::spell`), and its own path where none is.
/// The declaration under `/sys` that forms the carrier `head` keys on, `/sys/Nat/Nat` for `Nat`, read off the path its former is written at. None for a head no intrinsic former keys: a nominal one, or a tuple's shape.
fn carrier_former(head: &HeadKey) -> Option<Global> {
    let former = match head {
        HeadKey::Nat => Intrinsic::NatType,
        HeadKey::Byte => Intrinsic::ByteType,
        HeadKey::Int => Intrinsic::IntType,
        HeadKey::Flt => Intrinsic::FltType,
        HeadKey::Bool => Intrinsic::BoolType,
        HeadKey::Bytes => Intrinsic::BytesType,
        HeadKey::Handle => Intrinsic::HandleType,
        HeadKey::List => Intrinsic::ListType(Term::type_ground()),
        HeadKey::Cell => Intrinsic::CellType(Term::type_ground()),
        HeadKey::Channel => Intrinsic::ChannelType(Term::type_ground()),
        HeadKey::Io => Intrinsic::IoType(Term::type_ground()),
        HeadKey::Nominal(_) | HeadKey::TupleType(_) => return None,
    };

    match intrinsic_form(former, false) {
        IntrinsicForm::Named(path) => Some(intrinsic_global(path)),
        IntrinsicForm::Call { path, .. } => Some(intrinsic_global(&path)),
        _ => None,
    }
}

fn absolute(spellings: &Spellings, global: &Global) -> String {
    spell(spellings, &Qualifier::empty(), None, global).unwrap_or_else(|| match global {
        Global::Authored(path) => path.join(),
        Global::Witness(_) => global.faithful().to_string(),
    })
}

/// `said` of the declaration `asked` names, with its type spelled for a reader standing at it ([`spelling_at`]), over `definitions`, the module of the unit that declares it, kept under `kept` where it keeps one, and `scope`, and its totality as `certification`, that unit's record, files it. The type is a definition's, or a constructor's as its family's entry makes it, and none where the module holds no definition under the name.
fn spelled(
    asked: &Qualifier,
    said: Lowered,
    definitions: &Definitions,
    kept: Option<(&Lexicon, &Places)>,
    scope: &[&Lexicon],
    certification: Option<&Certification>,
) -> Described {
    let name = said.name;
    let spelling = Rc::new(spelling_at(definitions, kept, scope, &SYNTAX, name));
    let type_ = definitions.items.iter().find_map(|item| {
        item.definitions()
            .into_iter()
            .find(|definition| definition.name == name)
            .map(|definition| definition.type_)
            .or_else(|| item.made(&name).map(|made| made.type_))
    });

    Described {
        asked: asked.join(),
        path: name.path(),
        declared: said.declared,
        public: said.public,
        declaration: said.declaration,
        type_: type_.map(|type_| type_.spelled(&spelling).to_string()),
        totality: certification
            .filter(|_| said.declared.is_defined())
            .and_then(|certification| certification.totality(&name)),
        members: said.members,
        witnesses: said.witnesses,
    }
}
