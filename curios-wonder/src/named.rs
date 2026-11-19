//! The `at` query and what a check leaves of a file for the server's requests: what stands at a place, read off the subject as the check's fold left it.
//!
//! **A place answers an occurrence, and what it refers to is stated once** ([`Placed`]): each text of a file with the names and the terms it writes, and what each declaration they name is answered with, made once a check by the function a declaration asked about by its path is answered by, however often the file names it. A type is spelled for the reader of each text, where the name is written, so it pastes back there.
//!
//! **The lowering's answer always, and the check's where it established more.** The fold tells its reader the subject's lowering as soon as it is made or restored (`Fold::reading`), so a subject that is refused answers as one that compiles; a declaration of a unit in scope, compiled and certified before the subject, answers its type and its totality whatever the subject is; and once elaboration has established the subject's own declarations, the fold tells that too (`Fold::elaborating`), each answering its type, and its totality where the certifier judged it ([`Placed::establish`]). A declaration elaboration refused, or withheld for reaching one it refused, answers the lowering's facts and no type. Only a subject that does not lower has nothing to read.
//!
//! **A name is answered with where it is declared, as a place in a file that holds it.** A declaration of the subject's own stands in text this question read, and one of a unit in scope in the text that unit was compiled from, which is what the disk or an editor holds: a store's hit is believed on a re-read of every file, and a session's unit was compiled from the overlay. A unit the compiler was built with was read in a checkout no question names, or on another machine, so its sources hold their names and no file, and a declaration of one is left unplaced: it is answered by its path, its head and its prose read off its text all the same.
//!
//! **A name answers the path its declaration is exposed at and the path it is declared at.** Exposed is how a reader outside every module writes it ([`absolute`]), the path `--name` resolves: `List/map` answers `/std/List/map`, declared at `/sys/List/map`.
//!
//! **A declaration's head and prose are read off its own text**, which the unit that declares it read and its record still holds ([`Declaration::heading`]): a declaration is answered with them whether or not a file holds it, the library under an installed release included, and whether or not a documentation page shows it.
//!
//! **One thing stands at an offset** ([`Placed::answer`]): the narrowest name that holds it ([`Named::at`]), and where none does the narrowest term. The rule is stated here, over the records, since both transports ask it and a server asks it of records alone.
//!
//! **A declaration asked about by its path is answered off the unit that declares it** ([`described`]), its type as that unit was elaborated: the subject is lowered as a question about a place lowers it, which finds the unit, and is elaborated only where it declares the name itself, so a file being edited never stands between a question and a declaration of a unit in scope.

use {
    crate::{
        Diagnosed, Diagnosis, Origin, Overlaid, Severity, Subject, checked, of_error, open,
        overlaid, tested_last,
    },
    curios_common::{Code, Plicity, Qualifier, Report, Said, Source, Span},
    curios_core::{
        Anchors, Certification, Declared, Definitions, Entrypoint, Global, HeadKey, Imports,
        Intrinsic, Item, Lexicon, Places, Spellings, Subterm, Term, Totality, UnitPaths,
        WitnessKey,
    },
    curios_document::{Documentation, ModuleDocumentation},
    curios_module::{Heading, Overlay, PreparedText, RootSource, SYNTAX, heading, identity},
    curios_pipeline::{
        Cache, CompileError, EntryTail, Fold, Progress, prelude_documentation, spelling_at,
        spelling_for,
    },
    curios_surface::{
        IntrinsicForm, ReaderPosition, Spellable, Spelling, intrinsic_form, intrinsic_global, spell,
    },
    curios_unit::Unit,
    curios_verdict::Verdicts,
    std::{
        cell::RefCell,
        collections::{BTreeMap, BTreeSet, HashMap},
        path::{Path, PathBuf},
        ptr,
        rc::Rc,
    },
};

/// One name a file writes: where, the declaration it names, and whether it is named there.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Named {
    /// Where the name is written, in the file asked about.
    pub written: Span,
    /// The declaration it names, as the lowering resolved it.
    pub named: Global,
    /// Whether `written` is where the declaration is named, rather than a use of it.
    pub declares: bool,
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

/// One text of a file, a source declaration's or the term a program closes with: every name and term written in it, in the order they are written, and the type of each declaration it names as its reader writes it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Written {
    /// Where the text lies in the file.
    pub text: Span,
    /// The definition a reader of the text stands in, which each type it names is spelled for: none for the term a program closes with.
    pub owner: Option<Global>,
    pub names: Vec<Named>,
    /// Where each term the text writes stands, a term before the ones it holds (`curios_module::Text::terms`).
    pub terms: Vec<Span>,
    /// The type of each declaration the text names, spelled for a reader standing in the text: none for one no compilation typed.
    pub types: BTreeMap<Global, String>,
}

/// What a check left of one file: each of its texts with what it writes, and what each declaration they name is answered with, but for its type, which each text spells for its own reader.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Placed {
    pub texts: Vec<Written>,
    pub declarations: BTreeMap<Global, Described>,
}

/// What stands at a place: the range it covers, and what it is.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Answer {
    pub range: Span,
    pub stands: Stands,
}

/// What stands at a place ([`Answer`]).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Stands {
    /// A name, and the declaration it names, its type spelled where the name is written.
    Declaration {
        declares: bool,
        described: Described,
    },
    /// A term no name stands at, as it is written.
    Term { written: String },
}

impl Placed {
    /// What stands at `offset` of the text `written`, one of this file's: the name [`Named::at`] finds, and where none holds the offset the narrowest term that does, a term at its end being at it as a name is. `None` where neither does.
    pub fn answer(&self, written: &Written, offset: usize) -> Option<Answer> {
        if let Some(named) = Named::at(&written.names, offset) {
            let mut described = self.declarations.get(&named.named)?.clone();
            described.type_ = written.types.get(&named.named).cloned();

            return Some(Answer {
                range: named.written.clone(),
                stands: Stands::Declaration {
                    declares: named.declares,
                    described,
                },
            });
        }
        let term = written
            .terms
            .iter()
            .filter(|term| term.start <= offset && offset <= term.end)
            .min_by_key(|term| (term.end - term.start, offset == term.end))?;

        Some(Answer {
            range: term.clone(),
            stands: Stands::Term {
                written: term.source.text()[term.start..term.end].to_string(),
            },
        })
    }

    /// Complete what the lowering answered of the subject's own declarations with what elaboration established of them: `definitions`, the subject's module or, where it was refused, the declarations that elaborated, each named one's type spelled for each text's reader; and `certification`, where the certifier judged the module, each definition's totality. `scope` and `own` are the units the subject was compiled against and its lowering. A declaration `definitions` holds nothing of — refused, or withheld for reaching one refused — keeps the lowering's facts alone.
    pub fn establish(
        &mut self,
        scope: &[&Unit],
        own: &PreparedText,
        definitions: &Definitions,
        certification: Option<&Certification>,
    ) {
        let saying = Saying::over(scope, own);
        let places = Places::of(definitions);
        let owned = |name: &Global| own.declared_as(name).is_some();
        for written in &mut self.texts {
            let reader = saying.reader(written.owner);
            let named = written
                .names
                .iter()
                .map(|named| named.named)
                .filter(owned)
                .collect::<BTreeSet<_>>();
            for name in named {
                if let Some(type_) = declared_type(definitions, &places, &name) {
                    written
                        .types
                        .insert(name, type_.spelled(&reader).to_string());
                }
            }
        }
        if let Some(certification) = certification {
            for (name, described) in &mut self.declarations {
                if owned(name) && described.declared.is_defined() {
                    described.totality = certification.totality(name);
                }
            }
        }
    }

    /// What stands at `offset` of the file: what [`Placed::answer`] finds in the text that holds it.
    pub fn at(&self, offset: usize) -> Option<Answer> {
        self.texts
            .iter()
            .filter(|written| written.text.start <= offset && offset <= written.text.end)
            .find_map(|written| self.answer(written, offset))
    }
}

/// What stands at `line` and `column` of `file`: a name and the declaration it names, as [`described`] answers one, or the narrowest term written there, read off the check of the subject as the server's is (`checked`), so a file that does not check answers as one that does, and a declaration of its own that elaborated answers its type. `None` where nothing stands there, and where the subject is not written in `file`. `overlay` and `cache` behave as they do for `diagnostics`.
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
) -> Result<Option<Answer>, Vec<Diagnosis>> {
    let subject = subject.formed(overlay);
    if let Subject::CoreText { .. } = subject {
        return Err(unlowered());
    }
    let Diagnosed {
        diagnostics,
        placed,
        ..
    } = checked(budget, subject, overlay, cache, file, progress);
    let Some(placed) = placed else {
        return Err(diagnostics);
    };
    let offset = placed
        .texts
        .first()
        .and_then(|written| written.text.source.offset_of(line, column));

    Ok(offset.and_then(|offset| placed.at(offset)))
}

/// The refusal of a question answered off a lowering, asked of Core text, which is lowered by nothing.
fn unlowered() -> Vec<Diagnosis> {
    let mut said = Said::line("no lowering");
    said.stating("Core text is lowered by nothing, and this question is answered off a lowering");

    vec![Diagnosis {
        severity: Severity::Error,
        report: Report::of(None, said),
    }]
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

    /// What the file asked about writes in the subject `text` is the lowering of, compiled against `scope`, with the term its program closes with where it is one: each of its texts with the names and terms it writes, and what each declaration they name is answered with, made once whatever the number of its occurrences — its type spelled for each text's reader where a unit in scope declares it.
    pub fn read(&self, scope: &[&Unit], text: &PreparedText, entry: Option<&Entrypoint>) -> Placed {
        curios_profile::profile!("placed");
        let saying = Saying::over(scope, text);
        let mut declarations = BTreeMap::new();
        // Whether a lowered item declares each name met, and the unit in scope declaring it, each asked once.
        let mut declared = HashMap::new();
        let mut declaring = HashMap::new();
        let texts = text
            .texts(entry, |source| self.lies(source))
            .iter()
            .map(|passage| {
                let names = passage
                    .mentions()
                    .into_iter()
                    .filter(|mention| {
                        *declared.entry(mention.name).or_insert_with(|| {
                            let Some((described, unit)) = saying.described(mention.name) else {
                                return false;
                            };
                            declarations.insert(mention.name, described);
                            if let Some(unit) = unit {
                                declaring.insert(mention.name, unit);
                            }

                            true
                        })
                    })
                    .map(|mention| Named {
                        written: mention.span,
                        named: mention.name,
                        declares: mention.declared.is_some(),
                    })
                    .collect::<Vec<_>>();
                let reader = saying.reader(passage.owner());
                let types = names
                    .iter()
                    .map(|named| named.named)
                    .collect::<BTreeSet<_>>()
                    .into_iter()
                    .filter_map(|name| {
                        let unit: &Unit = declaring.get(&name)?;
                        let type_ = declared_type(unit.core(), unit.places(), &name)?;

                        Some((name, type_.spelled(&reader).to_string()))
                    })
                    .collect();

                Written {
                    text: passage.span().clone(),
                    owner: passage.owner(),
                    names,
                    terms: passage.terms(),
                    types,
                }
            })
            .collect();

        Placed {
            texts,
            declarations,
        }
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

/// What a check reads of the declarations a file names, made once for the check: the units in scope, the subject's lowering, how a reader outside every module writes each name, and the spelling a type is printed in for a reader of the subject before one is placed.
struct Saying<'s> {
    scope: &'s [&'s Unit],
    own: &'s PreparedText,
    outside: Spellings,
    spelling: Spelling,
}

impl<'s> Saying<'s> {
    /// What a check reads of the declarations named in `own`, the subject's lowering, over the units in `scope`.
    fn over(scope: &'s [&'s Unit], own: &'s PreparedText) -> Self {
        let texts = scope
            .iter()
            .filter_map(|unit| unit.text())
            .collect::<Vec<_>>();
        let spellings = own.spellings(&texts);
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
        let lexicons = scope.iter().map(|unit| unit.lexicon()).collect::<Vec<_>>();
        let lexicon = own.lexicon();
        let spelling = spelling_for(
            own.core(),
            Some((&lexicon, own.places())),
            &lexicons,
            &Rc::new(spellings),
            &SYNTAX,
        );

        Self {
            scope,
            own,
            outside,
            spelling,
        }
    }

    /// What the lowering says of `name` ([`said_of`]) and its totality, where the unit in scope that declares it recorded one, beside that unit: none for the subject's own. Asked by the path it is exposed at, or for a witness, which has none, by its head as written; its type is each text's to spell for its own reader. `None` for a name no lowered item declares.
    fn described(&self, name: Global) -> Option<(Described, Option<&'s Unit>)> {
        let (said, unit) = said_of(self, name)?;
        let head = said.declaration.as_ref().map(|declaration| {
            let label = &declaration.label;
            label.source.text()[label.start..label.end].to_string()
        });
        let asked = match (name, head) {
            (Global::Witness(_), Some(head)) => head,
            _ => absolute(&self.outside, &name),
        };

        Some((
            Described {
                asked,
                path: name.path(),
                declared: said.declared,
                public: said.public,
                declaration: said.declaration,
                type_: None,
                totality: unit
                    .filter(|_| said.declared.is_defined())
                    .and_then(|unit| unit.certification().totality(&name)),
                members: said.members,
                witnesses: said.witnesses,
            },
            unit,
        ))
    }

    /// The spelling a type is printed in for a reader standing in `owner`, the definition a text is read in, or at the root where the text is the term a program closes with.
    fn reader(&self, owner: Option<Global>) -> Rc<Spelling> {
        Rc::new(self.spelling.clone().for_reader(ReaderPosition {
            owner,
            witnesses: Rc::default(),
            locals: Rc::default(),
        }))
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

/// One thing a block or a hover states of a declaration, a line: what leads it, and the values it shows after it, each value of the program quoted. A block lays several values over as many lines as keep each within its width, and a hover joins them.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Fact {
    pub lead: String,
    pub values: Vec<String>,
}

impl Described {
    /// What is stated of the declaration, a fact a line in the order a block states them: what it is declared as and whether it is written `pub`, with the path it is declared at where that is not the one it is answered by; its type; its totality; its members; and how many witnesses in scope it takes part in. The one statement both transports render ([`Placed::answer`]).
    pub fn facts(&self) -> Vec<Fact> {
        let declared = declared_as(self.declared);
        let mut kind = match self.public {
            Some(true) => format!("public {declared}"),
            Some(false) => format!("private {declared}"),
            None => declared.to_string(),
        };
        if self.path != self.asked {
            kind.push_str(&format!(", declared at {}", Code(&self.path)));
        }
        let fact = |lead: &str, values: Vec<String>| Fact {
            lead: lead.to_string(),
            values,
        };

        let mut facts = vec![fact(&kind, Vec::new())];
        if let Some(type_) = &self.type_ {
            facts.push(fact("type:", vec![Code(type_).to_string()]));
        }
        match self.totality {
            Some(Totality::Total) => facts.push(fact("totality:", vec!["total".to_string()])),
            Some(Totality::Partial) => facts.push(fact(
                "totality:",
                vec!["partial, not known to terminate".to_string()],
            )),
            None => {}
        }
        let members = match self.declared {
            Declared::Inductive => Some("constructors:"),
            Declared::Struct => Some("fields:"),
            Declared::Concept => Some("methods:"),
            _ => None,
        };
        if let Some(members) = members.filter(|_| !self.members.is_empty()) {
            let quoted = self
                .members
                .iter()
                .map(|member| Code(member).to_string())
                .collect();
            facts.push(fact(members, quoted));
        }
        match self.witnesses.as_ref().map(Vec::len) {
            Some(0) => facts.push(fact("witnesses:", vec!["none in scope".to_string()])),
            Some(count) => facts.push(fact("witnesses:", vec![format!("{count} in scope")])),
            None => {}
        }

        facts
    }
}

/// The word a declaration is said to be declared as by.
pub fn declared_as(declared: Declared) -> &'static str {
    match declared {
        Declared::Function | Declared::Value | Declared::Type => "definition",
        Declared::Inductive => "inductive",
        Declared::Struct => "structure",
        Declared::Concept => "concept",
        Declared::Constructor => "constructor",
        Declared::Method => "method",
        Declared::Witness => "witness",
        Declared::Test => "test",
    }
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
        Subject::CoreText { origin } => {
            described_core_text(budget, &origin, asked, overlay, (cache, through), progress)
        }
    }
}

/// What `asked` names in the scope of a text of Core text, as [`described`] answers it: a declaration the text writes, which Core text names by its own path, answered off the unit it reads to — what it is declared as, its type, its totality as the certifier recorded it, its members, and where the text writes it, the path it is declared at being the one asked, since Core text exports nothing at another — and any other name as the prelude's scope answers it, a text being compiled against the prelude alone.
pub fn described_core_text(
    budget: u64,
    origin: &Origin,
    asked: &Qualifier,
    overlay: &Overlay,
    (cache, through): (Option<&Verdicts>, Option<&dyn Cache>),
    progress: &mut dyn FnMut(Progress<'_>),
) -> Result<Option<Described>, Vec<Diagnosis>> {
    let (source, module) = origin.read(overlay).map_err(|refusal| vec![*refusal])?;
    let own = Fold::new(budget, &[], through)
        .core_text_unit(&source, module, &mut *progress, |prelude, unit| {
            described_in_text(prelude, unit, asked)
        })
        .map_err(of_error)?;

    match own {
        Some(described) => Ok(Some(described)),
        None => described(budget, Subject::prelude(), asked, overlay, cache, progress),
    }
}

/// The declaration `asked` names in `unit`, a unit read from Core text over the prelude's roots `prelude`, as [`described`] answers it, where the text declares one at that path: off the unit, placed where the text writes it.
pub fn described_in_text(prelude: &[&Unit], unit: &Unit, asked: &Qualifier) -> Option<Described> {
    let name = Global::Authored(*asked);
    let place = unit.places().place(&name)?;
    let declared = unit.core().items.declared_at(&name, place);
    let said = Lowered {
        name,
        declared,
        public: None,
        declaration: unit
            .anchors()
            .written(&name)
            .map(|(text, label)| Declaration {
                text,
                label,
                held: true,
            }),
        members: members(unit.lexicon(), &name, declared),
        witnesses: None,
    };
    let scope = prelude
        .iter()
        .map(|unit| unit.lexicon())
        .collect::<Vec<_>>();

    Some(spelled(
        asked,
        said,
        unit.core(),
        Some((unit.lexicon(), unit.places())),
        &scope,
        Some(unit.certification()),
    ))
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
    let (said, unit) = said_of(&Saying::over(scope, own), resolved(scope, own, asked))?;

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
        // A text of Core text is compiled against the prelude alone.
        Subject::CoreText { .. } => Vec::new(),
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

/// What the lowering says of `name`: what it is declared as and where, whether it is written `pub`, its members and the witnesses in scope it takes part in — by the subject's own lowering, then the units in scope, as `saying` reads them — beside the unit in scope that declares it, none for the subject's own. `None` for a name no lowered item declares.
fn said_of<'s>(saying: &Saying<'s>, name: Global) -> Option<(Lowered, Option<&'s Unit>)> {
    let found = found(saying.scope, saying.own, &name)?;
    let witnesses = witnesses(saying, &found, &name);

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

/// The witnesses the units in scope, then the subject, declare that `name`, `found` where it is declared, takes part in, each unit's by place, placed where a file holds its text, the subject's own always: a concept's own, and a type's — those keyed on the head it stands for, a nominal type's name, or what the value of a definition whose type is a sort is keyed by. None for anything else. Each name in them is written as a reader outside every module writes it ([`absolute`]).
fn witnesses(saying: &Saying<'_>, found: &Found<'_>, name: &Global) -> Option<Vec<Witnessed>> {
    let Saying {
        scope,
        own,
        outside,
        ..
    } = saying;
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
    let heads = |key: &WitnessKey| {
        let heads = key
            .heads()
            .iter()
            .map(|head| match head {
                HeadKey::Nominal(nominal) => absolute(outside, nominal),
                carrier => carrier_former(carrier)
                    .map_or_else(|| carrier.to_string(), |former| absolute(outside, &former)),
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
        (_, [_]) => Some(absolute(outside, concept)),
        _ => Some(format!("{}{}", absolute(outside, concept), heads(key))),
    };
    // Each unit's places, the lowering its witnesses are written in where it holds one, and whether it is the subject's own.
    let units = scope
        .iter()
        .map(|unit| (unit.places(), unit.text(), false))
        .chain([(own.places(), Some(*own), true)]);

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
                        let keyed = keyed(&concept, &key)?;
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
                            keyed,
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

/// How a reader outside every module, with nothing imported, writes `global` over `spellings`: the shortest of its own path and the paths a module exports it at that every module may write (`curios_surface::spell`), and its own path where none is.
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
    let type_ = match kept {
        Some((_, places)) => declared_type(definitions, places, &name),
        None => definitions
            .items
            .iter()
            .find_map(|item| item_type(item, &name)),
    };

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

/// The type `definitions` holds of `name`, read off the one item `places` say holds it ([`item_type`]).
fn declared_type(definitions: &Definitions, places: &Places, name: &Global) -> Option<Term> {
    item_type(&definitions.items[places.place(name)?.item()], name)
}

/// The type `item` holds of `name`: a definition's, or a constructor's as its family's entry makes it. None where the item holds no definition under the name.
fn item_type(item: &Item, name: &Global) -> Option<Term> {
    item.definitions()
        .into_iter()
        .find(|definition| definition.name == *name)
        .map(|definition| definition.type_)
        .or_else(|| item.made(name).map(|made| made.type_))
}
