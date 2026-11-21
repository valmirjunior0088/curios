//! Core text read for the product: a text parsed with each item's span, read by `curios-surface`'s reader over the units in scope, its foreign rows gathered and its declarations anchored where the text writes them, then compiled as a unit is where a program's is lowered and elaborated ([`compile_read`]). The one door a unit read from Core text, or a term put to the checkers from one, passes through: the soundness board and its histories read through it as `wonder` does.
//!
//! **The reader stands outside the trusted base.** `curios-cert`'s normal dependencies hold no parser, so its base is what [The trusted base is a crate boundary](../../curios-cert/README.md#the-trusted-base-is-a-crate-boundary) enumerates: the reader is composed with the certifier's production walks here, above both. A term the certifier admits at `False` is a flaw whatever text it was read from, so nothing the certifier is trusted for rests on the reader; what a refusal of a text says rests on the round trip that holds the reader to the writer.

use {
    crate::{
        CompileError, DumpScope, Earlier, Over, Placing, Progress, Stage, certifier_refusal,
        erasure_reported, globals_after, judged, recheck_over,
    },
    curios_abi::ForeignStore,
    curios_cert::{Rechecked, Refusal, certify_definitions},
    curios_common::{Mount, Qualifier, Report, RootKind, Said, Source, Span, SyntaxRegistry},
    curios_core::{
        Anchor, Anchored, Anchors, Definitions, Entrypoint, Global, Intrinsic, Places, Program,
        Term,
    },
    curios_erase::{Erasures, erase_unit},
    curios_module::Compiled,
    curios_surface::{CoreFile, CoreItem, CoreNominals, FormScope, ReadError, to_core_at},
    curios_unit::{Predecessors, Unit},
    std::sync::Arc,
};

/// A text of Core text read: the module it writes, the rows its foreign forms declare, where its text writes each declaration and the term it closes with, and that term, put at the type its question puts it at, where it closes with one.
#[derive(Debug)]
pub struct Read {
    pub definitions: Definitions,
    pub foreigns: ForeignStore,
    pub anchors: Anchors,
    pub entry: Option<Entrypoint>,
}

/// Why a text of Core text wrote no module: it does not parse, as the parser reports it, or the reader refused it, where the refusal stands in it.
#[derive(Debug)]
pub enum Unread {
    Parse(Box<Report>),
    Read(ReadError),
}

impl Unread {
    /// The refusal as a compilation reports it, at the place in the text it stands where it names one.
    pub fn report(&self) -> Report {
        match self {
            Unread::Parse(report) => (**report).clone(),
            Unread::Read(error) => match &error.span {
                Some(span) => Report::at(span.clone(), error.to_string()),
                None => Report::of(None, Said::line(error.to_string())),
            },
        }
    }
}

/// The Core text `source` holds, read over `scope`, the forms a test's and a foreign row's definitions are made against, and its closing term, where it closes with one, put at `at`, the type its question puts it at ([`to_core_at`]).
pub fn read_core_text(source: &Arc<Source>, scope: &FormScope, at: &Term) -> Result<Read, Unread> {
    let (file, spans) =
        CoreFile::parse_spanned(source).map_err(|error| Unread::Parse(Box::new(error.report())))?;
    let (definitions, entry) = to_core_at(&file, Some(scope), at).map_err(Unread::Read)?;
    let foreigns = rows(&definitions, scope.syntax);
    let anchors = anchored(&file, &spans);

    Ok(Read {
        definitions,
        foreigns,
        anchors,
        entry,
    })
}

/// Where `file`'s text writes each declaration, each item a text anchored by the first name it declares and each name it declares labelled where it is written, and the term it closes with, as a lowering anchors a program's ([`Anchors`]): what a report about the module it reads to is placed by. `spans` is each item's span, in the order of the items.
fn anchored(file: &CoreFile, spans: &[Span]) -> Anchors {
    let mut anchors = Anchors::default();
    for (item, span) in file.items.iter().zip(spans) {
        let heads = heads(item);
        let Some((first, _)) = heads.first() else {
            continue;
        };
        // A witness is named by its identity, under no path a declaration's anchor could take.
        let anchored = match first.qualifier() {
            Some(path) => Anchored::Declaration(*path),
            None => Anchored::Unnamed(Qualifier::empty()),
        };
        let within = anchors.site(Anchor::of(anchored), span);
        for (name, label) in heads {
            if let Some(label) = within.extent(label.unwrap_or(span)) {
                anchors.name(name, label);
            }
        }
    }
    if let Some(span) = file.entry.as_ref().and_then(|entry| entry.body.span()) {
        anchors.site(Anchor::of(Anchored::Entry), span);
    }

    anchors
}

/// Each name `item` declares by a head of its own, beside where its label is written: a group's members, a nominal group's declarations, a foreign row and a test.
fn heads(item: &CoreItem) -> Vec<(Global, Option<&Span>)> {
    match item {
        CoreItem::Lets(members) => members
            .iter()
            .map(|member| (member.core.name, member.label.span()))
            .collect(),
        CoreItem::Nominals(CoreNominals::Induct(families)) => families
            .iter()
            .map(|family| (family.core.name, family.label.span()))
            .collect(),
        CoreItem::Nominals(CoreNominals::Struct(records)) => records
            .iter()
            .map(|record| (record.core.name, record.label.span()))
            .collect(),
        CoreItem::Nominals(CoreNominals::Concept(concepts)) => concepts
            .iter()
            .map(|concept| (concept.core.name, concept.label.span()))
            .collect(),
        CoreItem::Foreign(row) => vec![(row.core.name, row.label.span())],
        CoreItem::Test(test) => vec![(test.core.name, test.label.span())],
    }
}

/// The foreign rows `definitions` declares, in the order it declares them: what a unit read from Core text declares for a program built over it, as a lowered unit declares the rows its `foreign` declarations state.
fn rows(definitions: &Definitions, syntax: SyntaxRegistry) -> ForeignStore {
    let scope = FormScope::over(syntax, [definitions]);
    let mut rows = ForeignStore::new();
    for item in definitions.items.iter() {
        for name in item.declared_names() {
            if let Some(row) = scope.rows.get(&name) {
                rows.register((**row).clone());
            }
        }
    }

    rows
}

/// A text of Core text entered on top of a fold ([`enter_core_text`]): the unit it reads to, and the program it links to where it closes with a term.
pub struct EnteredText {
    pub unit: Unit,
    pub linked: Option<curios_anf::Module>,
}

/// The Core text `source` holds, entered on top of `predecessors` as a program is: read over them, compiled as a unit, and the term it closes with, where it closes with one, judged at the program contract over that unit and erased, `observe` handed the program as the certifier accepted it, its `core-elab` rung. A text that closes with no term is a module, a unit claiming `module`; one that closes with a term is a program's declarations, which claim the empty prefix as a loose program's do. `progress` is told it is taken on.
///
/// **Its term is judged as it is written.** Core text states every node, so the tails of what it closes with are not handed to `/std/Program`'s `execute`, which is the elaborator's rewriting of a program's tails: what it closes with is put at `Io({})` as it stands.
pub fn enter_core_text(
    budget: u64,
    predecessors: Predecessors<'_>,
    syntax: &SyntaxRegistry,
    source: &Arc<Source>,
    module: Qualifier,
    observe: &mut dyn FnMut(Stage<'_>),
    progress: &mut dyn FnMut(Progress<'_>),
) -> Result<EnteredText, CompileError> {
    let scope = FormScope::over(*syntax, predecessors.cores());
    let contract = Term::intrinsic(Intrinsic::io_type(Term::tuple_type_unit()));
    let read = read_core_text(source, &scope, &contract)
        .map_err(|unread| CompileError::Failure(vec![unread.report()]))?;
    let mount = match read.entry {
        Some(_) => {
            progress(Progress::Entry { held: false });
            Qualifier::empty()
        }
        None => {
            // Core text is read once, as it is written: it is the subject of no test compilation, whatever tests it writes.
            progress(Progress::Compiling {
                unit: &module,
                compiled: Compiled::Built,
            });
            module
        }
    };
    let unit = compile_read(
        budget,
        predecessors,
        syntax,
        vec![Mount::new(mount, RootKind::Ordinary)],
        &read,
        None,
    )
    .map_err(|unmade| unmade.reported(&read, predecessors, syntax))?;
    let Some(entry) = read.entry else {
        return Ok(EnteredText { unit, linked: None });
    };

    let units = predecessors
        .units()
        .iter()
        .copied()
        .chain([&unit])
        .collect::<Vec<_>>();
    let (program, erased) = judged(
        budget,
        (Predecessors::over(&units), predecessors),
        syntax,
        entry,
        (Placing::Read(unit.anchors()), true),
    )?;
    observe(Stage::CoreElab(
        &Program {
            definitions: unit.core().clone(),
            entrypoint: program.as_program().entrypoint.clone(),
        },
        DumpScope {
            syntax,
            predecessors,
        },
    ));
    let linked = erased.expect("a program read from Core text is erased")?;

    Ok(EnteredText {
        unit,
        linked: Some(linked),
    })
}

/// Why a unit read from Core text was not made: the certifier refused it, what elaboration would have solved among what it refuses, or it could not be erased. Each held by pointer, a refusal being the large case of what a compile answers.
#[derive(Debug)]
pub enum Unmade {
    Certifier(Box<Refusal>),
    Erased(Box<curios_erase::Error>),
}

impl Unmade {
    /// The refusal as a compilation reports it, of the module `read` writes over `predecessors`: placed where its text writes the declaration it is about, as a lowered unit's is placed where its lowering wrote it.
    pub fn reported(
        &self,
        read: &Read,
        predecessors: Predecessors<'_>,
        syntax: &SyntaxRegistry,
    ) -> CompileError {
        let anchors = std::iter::once(&read.anchors)
            .chain(predecessors.anchors())
            .collect::<Vec<_>>();
        match self {
            Unmade::Certifier(verdict) => certifier_refusal(
                verdict,
                &read.definitions,
                &predecessors.lexicons(),
                &anchors,
                syntax,
            ),
            Unmade::Erased(error) => erasure_reported(
                error,
                &read.definitions,
                &predecessors.lexicons(),
                &anchors,
                syntax,
            ),
        }
    }
}

/// Compile a unit read from Core text, `read` being what the reader made of its text: what stands where a program's unit holds its lowering and its elaboration, judged and erased as any unit is from there, over `predecessors`, and claiming `mounts`. Over `baseline`, the unit the cabinet offers for the text's earlier spelling, the certifier is handed its module and record whole and adopts each verdict that still stands, the units in scope it was not compiled after being moved, as a recompile over a baseline is judged (`recheck_over`).
pub fn compile_read(
    budget: u64,
    predecessors: Predecessors<'_>,
    syntax: &SyntaxRegistry,
    mounts: Vec<Mount>,
    read: &Read,
    baseline: Option<&Earlier>,
) -> Result<Unit, Unmade> {
    let core = &read.definitions;
    let moved = predecessors
        .units()
        .iter()
        .map(|unit| baseline.is_some_and(|earlier| earlier.after.moved(unit)))
        .collect::<Vec<_>>();
    let globals = globals_after(predecessors, &moved);
    let places = Places::of(core);
    let (rechecked, mut certifier) = match baseline {
        Some(earlier) => recheck_over(
            core,
            &places,
            budget,
            &globals,
            Over {
                baseline: &earlier.unit,
                moved: &moved,
                candidates: earlier.candidates.as_deref(),
            },
            syntax,
        ),
        None => certify_definitions(core, budget, &globals, *syntax),
    };
    // The unit is the module the certifier stated, so every mark it carries is the walk's own.
    let core = match rechecked {
        Rechecked::Accepted(accepted) => accepted,
        Rechecked::Refused(verdicts) => {
            let verdict = verdicts.into_iter().next();
            return Err(Unmade::Certifier(Box::new(
                verdict.expect("a walk that accepted nothing refused something"),
            )));
        }
    };

    let cores = predecessors.cores();
    let scope_places = predecessors.places();
    let certifications = predecessors.certifications();
    let erased = predecessors.erased();
    let fixed = predecessors.fixed();
    let scope = Erasures::of(&cores, &scope_places, &certifications, &erased).fixed(&fixed);
    let anf = erase_unit(&mut certifier, scope, &core)
        .map_err(|error| Unmade::Erased(Box::new(error)))?;

    Ok(Unit::read(
        mounts,
        read.foreigns.clone(),
        core,
        anf,
        read.anchors.clone(),
    ))
}
