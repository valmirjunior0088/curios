//! The finished program: a flat list of top-level [`Item`]s, each nominal declaration's entry held by the definition that is its former.
//!
//! This is what a checker is handed. Elaboration produces it and erasure consumes it, but the shape itself is representation — a [`Definition`] is a name, a universe context, a type, and a body, and a [`RecItem`] is the same for a recursive group whose members reference each other through one shared [`RecGroup`] binder rather than through free names. Both checkers walk this structure, which is why it lives here rather than beside either of them.
//!
//! Items are stored in dependency order, every item after the items it reaches and otherwise by name ([`Module::ordered`]), and every walk reads them as stored. The entry an item's types may name ([`InductDecl`], [`StructDecl`], [`ConceptDecl`]) is carried by the type former that declares it ([`Nominal`]), and beside its items a [`Module`] carries its mounts, the witness set and its tests. The one compilation with an entrypoint holds a [`Program`], the module beside the entry's type and body.
//!
//! Well-formedness that *judges* rather than describes is not decided here. Whether a universe context is satisfiable is decided by each checker for itself, the elaborator's solver and the certifier's loop check; whether a definition terminates runs the size-change engine in `curios-analysis`, which both checkers drive. [`Totality`] is the classification those judgments record onto a definition, and the enum lives here because the field does.

use {
    super::{
        Atom, Bound, ConceptDecl, ConceptSpelling, Declarations, Enter, FieldSpelling, Free,
        FuncType, Global, InductDecl, Items, Lexicon, Many, Members, Outline, Place, Places,
        Polarity, RecGroup, RecMemberScopes, Scope, Sharing, Spelling, StructDecl, StructType,
        Subterm, Symbols, Telescope, Term, UniverseContext, UniverseError, Variance, WitnessId,
        WitnessSpelling, build_shorten,
    },
    curios_utilities::{Mount, Plicity, Qualifier, SyntaxRegistry},
    std::{
        collections::{BTreeMap, BTreeSet, HashMap, HashSet},
        fmt, mem,
        rc::Rc,
    },
};

/// Whether a definition is known to terminate on every input.
///
/// `Partial` is "not proven total", never "proven divergent": a productive corecursive definition and a genuine infinite loop are both `Partial`, and both remain legal wherever erasure keeps them.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
#[curios_archive::archived]
pub enum Totality {
    /// Every recursive group this definition contains descends, it calls no diverging host row, and neither does anything it reaches.
    Total,
    /// Not proven total. The conservative default: a definition whose classification is unknown is `Partial`, never `Total`.
    #[default]
    Partial,
}

impl Totality {
    pub fn is_total(self) -> bool {
        matches!(self, Totality::Total)
    }
}

/// What the certifier concluded about the declarations one of its walks judged: each definition's totality, closed over everything it mentions, with what judging it read of other items, and each `induct` and `struct` entry's polarity vector.
///
/// Only the certifier's walk makes one — `curios_cert::certify_module` — and a later walk reads it as the verdicts on the definitions it covers, rather than the stamp elaboration writes onto each [`Definition`]. It is filed with the unit whose definitions it covers, and that unit's address is its identity: a stored unit is found only under the compiler that judged it, and the fixed prelude's record is a constant of the build that certified it. It is read only where it [covers](Certification::covers) its unit, never one name at a time — one naming fewer definitions than its unit holds was not made by a walk over that unit, so none of its entries is known to be the closure it claims — and a unit without a covering record is one the reading walk classifies for itself, never one it takes elaboration's word for.
#[derive(Debug, Clone, Default)]
#[curios_archive::archived]
pub struct Certification {
    /// Each judged definition's totality, closed over everything it mentions.
    totalities: BTreeMap<Global, Totality>,
    /// How each `induct` and `struct` entry the walk accepted uses its parameters, as the walk computed it: what a later walk composes through where a family it judges mentions this one. The vector the entry itself carries is elaboration's, which no walk reads.
    polarities: BTreeMap<Global, Vec<Polarity>>,
    /// How two instances of each `induct` and `struct` entry the walk accepted compare in each of its universe parameters, as the walk computed it: what a later walk compares two instances of the family by. The vector the entry itself carries is elaboration's, which the walk holds to this one and no judgment reads.
    variances: BTreeMap<Global, Vec<Variance>>,
    /// What judging each definition read of other items: what a later walk over the unit decides by which of these verdicts stand, and what nothing compiled after the unit reads.
    ///
    /// **Of the unit's baseline half**, as its traces are: skipped where the record is stored, stored beside it ([`Certification::reads_bytes`]), handed back as bytes where it is restored ([`Certification::with_reads`]), and read out of them when a walk over the unit first asks.
    #[archived_with(curios_archive::Skip)]
    reads: curios_archive::Deferred<Read>,
}

/// What a walk's judgments read, held apart from what they concluded: of the items of its own unit, by name, and of the declarations of other units, each by the digest of the part read.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
#[curios_archive::archived]
pub struct Read {
    /// What judging each definition read of other items.
    by: BTreeMap<Global, Reads>,
    /// What the walk read of each declaration its unit does not hold, another unit's. See [`Abroad`].
    abroad: BTreeMap<Global, Abroad>,
}

/// What a walk read of one declaration of another unit: the digest of what a signature read reads of it, and of what a body read does, each where some judgment read it. The certifier makes both, from the terms in its scope and its own verdict on them, and compares them where a walk over a baseline meets a unit that is not the one the baseline's walk read.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
#[curios_archive::archived]
pub struct Abroad {
    pub signature: Option<String>,
    pub body: Option<String>,
}

/// Two records are one where they conclude the same of every definition and every entry and judging each read the same: a record's reads are part of what it says, wherever they are held.
impl PartialEq for Certification {
    fn eq(&self, other: &Self) -> bool {
        self.totalities == other.totalities
            && self.polarities == other.polarities
            && self.variances == other.variances
            && self.read() == other.read()
    }
}

impl Eq for Certification {}

/// What the certifier concluded about one definition it judged.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
#[curios_archive::archived]
pub struct Certified {
    /// Whether the definition terminates, closed over everything it mentions.
    pub totality: Totality,
    /// What judging it read of other items. A recursive group is judged as one item, so each of its members holds the group's reads; a declaration's registry entry is accepted as part of its type former, whose entry holds what that acceptance read.
    pub reads: Reads,
}

/// The items one judgment read, and how: its type, or its body.
///
/// Recorded where the kernel consults its environment — a name's type or universe scheme, a declaration's registry entry, a definition's body — so a read counts however the judgment reached it. A remembered reduct included: every memo lives one declaration, so the bodies a remembered reduct unfolds were asked for within the judgment that hits it. A body counts as read whenever it is asked for, unfolded or not, because asking is what makes the answer depend on it.
///
/// A definition's classification also reads the verdict of every name it mentions, which the totality closure runs over. That is not a third kind: typing a mention reads its type, so each such verdict read is already a signature read.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
#[curios_archive::archived]
pub struct Reads {
    /// The items whose type, universe scheme or registry entry was read.
    pub signatures: BTreeSet<Global>,
    /// The definitions whose body was asked for.
    pub bodies: BTreeSet<Global>,
}

impl Reads {
    /// These reads with `other`'s beside them.
    pub fn extend(&mut self, other: Reads) {
        self.signatures.extend(other.signatures);
        self.bodies.extend(other.bodies);
    }
}

impl Certification {
    /// A record of each of `definitions`, of the vectors of each entry in `polarities` and in `variances`, and of what the walk read of each declaration of another unit, `abroad`.
    pub fn of(
        definitions: impl IntoIterator<Item = (Global, Certified)>,
        polarities: impl IntoIterator<Item = (Global, Vec<Polarity>)>,
        variances: impl IntoIterator<Item = (Global, Vec<Variance>)>,
        abroad: impl IntoIterator<Item = (Global, Abroad)>,
    ) -> Self {
        let (mut totalities, mut by) = (BTreeMap::new(), BTreeMap::new());
        for (name, certified) in definitions {
            totalities.insert(name, certified.totality);
            by.insert(name, certified.reads);
        }

        Self {
            totalities,
            polarities: polarities.into_iter().collect(),
            variances: variances.into_iter().collect(),
            reads: curios_archive::Deferred::held(Read {
                by,
                abroad: abroad.into_iter().collect(),
            }),
        }
    }

    /// What the walk's judgments read, read out of its bytes now where the record was restored and no walk has asked before.
    fn read(&self) -> &Read {
        self.reads
            .get()
            .unwrap_or_else(|error| panic!("a certifier's reads failed to restore: {error}"))
    }

    /// The bytes of what judging each definition read: stored apart from the record, since only a walk over its unit as a baseline reads them.
    pub fn reads_bytes(&self) -> Result<std::borrow::Cow<'_, [u8]>, String> {
        self.reads.bytes()
    }

    /// This record with `bytes` as what judging each definition read ([`Certification::reads_bytes`]), read when a walk over its unit first asks: what a restored record is given, since restoring it leaves it with none.
    pub fn with_reads(mut self, bytes: impl Into<curios_archive::Bytes>) -> Self {
        self.reads = curios_archive::Deferred::stored(bytes);
        self
    }

    /// The polarity vector the certifier computed for the registry entry `name`, when this record holds one.
    pub fn polarities(&self, name: &Global) -> Option<&[Polarity]> {
        self.polarities.get(name).map(Vec::as_slice)
    }

    /// The variance vector the certifier computed for the registry entry `name`, when this record holds one.
    pub fn variances(&self, name: &Global) -> Option<&[Variance]> {
        self.variances.get(name).map(Vec::as_slice)
    }

    /// The certifier's classification of `name`, when this record covers it.
    pub fn totality(&self, name: &Global) -> Option<Totality> {
        self.totalities.get(name).copied()
    }

    /// What judging `name` read of other items, when this record covers it.
    pub fn reads(&self, name: &Global) -> Option<&Reads> {
        self.read().by.get(name)
    }

    /// What the walk read of `name`, a declaration of another unit, where it read it and recorded so.
    pub fn abroad(&self, name: &Global) -> Option<&Abroad> {
        self.read().abroad.get(name)
    }

    /// The declarations of other units the walk recorded a read of.
    pub fn read_abroad(&self) -> impl Iterator<Item = &Global> {
        self.read().abroad.keys()
    }

    /// Whether this record classifies every definition `module` holds.
    pub fn covers(&self, module: &Module) -> bool {
        let classified = |name: &Global| self.totalities.contains_key(name);

        (0..module.items.len()).all(|place| module.items.names(place).into_iter().all(classified))
    }

    /// Every classification this record holds.
    pub fn iter(&self) -> impl Iterator<Item = (&Global, Totality)> {
        self.totalities
            .iter()
            .map(|(name, totality)| (name, *totality))
    }
}

/// Where a module holds each name it declares, as [`Module::lexicon`] finds one: the item's place, and the member's within it.
type Located = HashMap<Global, (usize, usize)>;

/// How a lowered definition was introduced.
///
/// This is elaboration metadata, not a fact inferred from the flattened qualified name. In particular, a module and a nominal type may share a qualifier without turning ordinary module members into generated nominal members.
///
/// A generated member names its origin in full: `InductiveConstructor` carries both the inductive it belongs to *and* which constructor it is, so the registry correspondence is read off the pair rather than re-synthesized by joining an owner and a tag into a name and looking that name up.
#[derive(Debug, Clone, PartialEq, Eq)]
#[curios_archive::archived]
pub enum DefinitionKind {
    Authored,
    /// A nominal declaration's former, with the entry it is the former of: what says a definition is one, so no former is spelled without its entry and no entry without its former.
    Nominal(Rc<Nominal>),
    InductiveConstructor {
        owner: Qualifier,
        tag: Atom,
    },
    ConceptMethod {
        owner: Qualifier,
    },
    Witness,
    Test,
}

impl DefinitionKind {
    /// The entry a former carries.
    pub fn nominal(&self) -> Option<&Nominal> {
        match self {
            DefinitionKind::Nominal(nominal) => Some(nominal),
            _ => None,
        }
    }

    /// What the definition is declared as, with nothing of its entry in it.
    pub fn declared(&self) -> Declared {
        match self {
            DefinitionKind::Authored => Declared::Authored,
            DefinitionKind::Nominal(nominal) => match &**nominal {
                Nominal::Inductive(_) => Declared::Inductive,
                Nominal::Struct(_) => Declared::Struct,
                Nominal::Concept { .. } => Declared::Concept,
            },
            DefinitionKind::InductiveConstructor { .. } => Declared::Constructor,
            DefinitionKind::ConceptMethod { .. } => Declared::Method,
            DefinitionKind::Witness => Declared::Witness,
            DefinitionKind::Test => Declared::Test,
        }
    }
}

/// Each field of `fields` under the witness mark as its position beside the name its type applies, read where the field is written: a concept's superclass edges, where the elaborator has not recorded what they reach. A field whose type applies no name is left out.
fn written_edges(fields: &Telescope<()>) -> Vec<(usize, Global)> {
    let mut edges = Vec::new();
    let mut position = 0;
    let mut rest = fields;
    while let Telescope::Cons(mark, type_, after) = rest {
        let applied = match &**type_ {
            Subterm::StructType(applied) => Some(applied.name),
            _ => type_.head_name().and_then(Free::as_global).copied(),
        };
        if let (Plicity::Witness, Some(applied)) = (mark, applied) {
            edges.push((position, applied));
        }
        position += 1;
        rest = after.body();
    }

    edges
}

/// What a name is declared as: a [`DefinitionKind`] without the entry a former's carries, so it is read and compared with no declaration in hand.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[curios_archive::archived]
pub enum Declared {
    Authored,
    Inductive,
    Struct,
    Concept,
    Constructor,
    Method,
    Witness,
    Test,
}

impl Declared {
    /// Whether the name is a nominal declaration's former.
    pub fn is_former(self) -> bool {
        matches!(
            self,
            Declared::Inductive | Declared::Struct | Declared::Concept
        )
    }
}

/// A nominal declaration's registry entry, held by the definition that is its former ([`DefinitionKind::Nominal`]).
///
/// **One fact, held once.** A former among a module's items and its entry in a registry beside them are two facts under one name with nothing holding one to the other; held by the former, an item's entries are the ones its formers carry, and a name declares at most one. A concept's record is held with it for the same reason: a concept is its resolution metadata and the record a witness is a value of, and neither is declared alone. Why is the crate `README.md`'s "A nominal declaration's entry is held by its type former".
#[derive(Debug, Clone, PartialEq, Eq)]
#[curios_archive::archived]
pub enum Nominal {
    Inductive(InductDecl),
    Struct(StructDecl),
    Concept {
        concept: ConceptDecl,
        record: StructDecl,
    },
}

impl Nominal {
    /// The inductive family's entry, where the declaration is one.
    pub fn inductive(&self) -> Option<&InductDecl> {
        match self {
            Nominal::Inductive(declaration) => Some(declaration),
            _ => None,
        }
    }

    /// The record's entry: a structure's own, or the record a concept's witnesses are values of.
    pub fn record(&self) -> Option<&StructDecl> {
        match self {
            Nominal::Struct(record) | Nominal::Concept { record, .. } => Some(record),
            Nominal::Inductive(_) => None,
        }
    }

    /// The concept's resolution metadata, where the declaration is one.
    pub fn concept(&self) -> Option<&ConceptDecl> {
        match self {
            Nominal::Concept { concept, .. } => Some(concept),
            _ => None,
        }
    }

    /// Every top-level name the entry's telescopes reach.
    pub fn reaches(&self) -> BTreeSet<Global> {
        match self {
            Nominal::Inductive(declaration) => declaration.reaches(),
            Nominal::Struct(record) => record.reaches(),
            Nominal::Concept { concept, record } => {
                let mut names = record.reaches();
                names.extend(concept.reaches());
                names
            }
        }
    }

    /// The entry with every term hash-consed against `sharing` — see [`Module::shared`].
    pub fn shared(&self, sharing: &Sharing) -> Self {
        match self {
            Nominal::Inductive(declaration) => Nominal::Inductive(declaration.shared(sharing)),
            Nominal::Struct(record) => Nominal::Struct(record.shared(sharing)),
            Nominal::Concept { concept, record } => Nominal::Concept {
                concept: concept.shared(sharing),
                record: record.shared(sharing),
            },
        }
    }
}

/// A single top-level definition: `name` bound to `body` of declared `type_`.
///
/// A standalone top-level `let` uses free `Var`s keyed by `name`. A definition returned by [`RecItem::definitions`] is the opened view of a scoped recursive member and likewise uses the group's export names; the authoritative recursive type and body remain in [`RecItem::group`].
#[derive(Debug, Clone, PartialEq)]
#[curios_archive::archived]
pub struct Definition {
    pub name: Global,
    pub kind: DefinitionKind,
    pub universe_context: UniverseContext,
    /// This definition's declaring module — `name`'s qualifier prefix, precomputed once by `into_core` (before `name` was flattened) rather than re-derived from it later. Stamped into `Context::island` per item by `elaborate_module_suffix` for the representation-privacy checks, which test subtree containment against it rather than equality; the same value `InductDecl::module` and `StructDecl::module` carry for type declarations. Islands are surface-elaboration state: erasure re-derives types with privacy suppressed and never stamps them.
    pub island: Qualifier,
    /// Whether this definition terminates on every input, together with everything it reaches. Written by `curios-elab` as the definition's item is published, once — like `polarities` on a declaration, and for the same reason: the analysis needs final, meta-free terms, so construction cannot know the answer. It defaults to [`Totality::Partial`], which is what makes a site that forgets to stamp it fail closed rather than open.
    ///
    /// This is the cross-module summary elaboration's own gates read: a user program that mentions a prelude definition inherits the flag rather than re-analyzing the prelude, which is sound because "partial" already means "something partial is in its closure". The certifier reads it only as a claim to contradict, and nothing below erasure reads it: an erased function's termination flag is marked from the certifier's record.
    pub totality: Totality,
    /// Which of this definition's universe parameters its value holds, one entry per parameter of `universe_context`: two instances apart in an irrelevant one alone are one term, unfolding either giving the same value. Written back by `curios-elab` where the definition's levels have closed, read off its body as [`InductDecl::variances`](super::InductDecl::variances) is read off a family's telescopes. Empty until then, for a definition that takes no universe parameter, and on a recursive member's opened view, both checkers comparing two projections of one group at every level; a position it does not reach is invariant.
    ///
    /// The elaborator's level alignment is its one reader. The kernel compares two instances of a definition by their spines and levels and unfolds where that decides nothing, so it holds none of this: a vector claiming an irrelevance the body denies costs a kernel refusal and admits nothing.
    pub variances: Vec<Variance>,
    /// Whether this definition is a proof a declaration wrote: one introduced as [`DefinitionKind::Authored`] whose type is a proposition, or a function into one. Its body is then read by its own check and by nothing after it: a judgment that asks what the name unfolds to is answered with nothing, in both checkers, so the name stands for a constant of its type. No judgment reads a proof's value, which is what a binder's mark rests on too ([`Scope::binds_proof`](super::Scope::binds_proof)), so a body withheld refuses nothing a body unfolded would admit, and what a later declaration reads of this one is its type.
    ///
    /// Written by `curios-elab` as the definition's item is published, with its binders' marks, and `false` until then. A constructor of a proposition and a law's projection are the compiler's own terms and stay what they unfold to. The certifier holds the mark one way, as it holds a binder's: a definition marked at a type it reads as no proposition is refused, and one left unmarked unfolds and is refused nowhere.
    pub proof: bool,
    pub type_: Term,
    pub body: Term,
}

/// Export metadata for one member of a flat top-level recursive group. The member's type and body live only in [`RecItem::group`], scoped over every export in the group.
#[derive(Debug, Clone, PartialEq, Eq)]
#[curios_archive::archived]
pub struct RecDefinition {
    pub name: Global,
    pub kind: DefinitionKind,
    pub island: Qualifier,
    /// Per member, not per group. The group's *descent* is decided once for all of them, but the transitive closure is not: an accepted group can still have one member that reaches something partial while its sibling does not. See [`Definition::totality`].
    pub totality: Totality,
    /// Per member, as [`Definition::proof`] is: a group may hold a lemma beside the function it is about, and the function still unfolds.
    pub proof: bool,
}

/// A flat top-level recursive item backed by the same structural fixed-point representation as a local [`super::Rec`]. Keeping the export metadata separate preserves the module's flat architecture without retaining a second, free-name copy of each recursive type and body.
#[derive(Debug, Clone, PartialEq)]
#[curios_archive::archived]
pub struct RecItem {
    pub definitions: Vec<RecDefinition>,
    pub group: RecGroup,
}

impl RecItem {
    /// This group with universe data projected out of every member, in place.
    ///
    /// The universe context is cleared here as it is on a [`Definition`]: the projection's whole purpose is that no universe data survives into Ersd.
    pub fn projected(&self) -> Self {
        Self {
            definitions: self.definitions.clone(),
            group: self.group.projected(),
        }
    }

    pub fn new(definitions: Vec<Definition>) -> Self {
        Self::try_new(definitions).expect("a recursive group has one valid universe context")
    }

    pub fn try_new(definitions: Vec<Definition>) -> Result<Self, UniverseError> {
        let universe_context = definitions
            .first()
            .map(|definition| definition.universe_context.clone())
            .unwrap_or_default();
        if !definitions
            .iter()
            .all(|definition| definition.universe_context == universe_context)
        {
            return Err(UniverseError::MismatchedRecursiveContexts);
        }
        let names = definitions
            .iter()
            .map(|definition| Free::from(&definition.name))
            .collect::<Vec<_>>();
        let members = names.iter().collect::<Vec<_>>();
        let arity = Many(members.len());
        let group = RecGroup::new(
            definitions
                .iter()
                .map(|definition| RecMemberScopes {
                    type_: Scope::close(arity, &members, definition.type_.clone()),
                    body: Scope::close(arity, &members, definition.body.clone()),
                })
                .collect(),
        )
        .with_universe_context(universe_context);
        let definitions = definitions
            .into_iter()
            .map(|definition| RecDefinition {
                name: definition.name,
                kind: definition.kind,
                island: definition.island,
                totality: definition.totality,
                proof: definition.proof,
            })
            .collect();

        Ok(Self { definitions, group })
    }

    /// Open the recursive scopes against their exported names.
    ///
    /// The returned definitions are a read-only projection; the authoritative types and bodies remain structurally shared in the group's scheme.
    pub fn definitions(&self) -> Vec<Definition> {
        let names = self
            .definitions
            .iter()
            .map(|definition| Term::free_var(&Free::from(&definition.name)))
            .collect::<Vec<_>>();
        let name_refs = names.iter().collect::<Vec<_>>();

        self.definitions
            .iter()
            .zip(self.group.iter())
            .map(|(definition, member)| Definition {
                name: definition.name,
                kind: definition.kind.clone(),
                universe_context: self.group.universe_context().clone(),
                island: definition.island,
                totality: definition.totality,
                proof: definition.proof,
                variances: Vec::new(),
                type_: member.type_.open(&name_refs),
                body: member.body.open(&name_refs),
            })
            .collect()
    }

    pub fn island(&self) -> Qualifier {
        self.definitions
            .first()
            .map(|definition| definition.island)
            .unwrap_or_default()
    }
}

impl Definition {
    /// The former of the nominal declaration `name` whose entry is `nominal`, spelled from the entry: declared at its parameters, then for a family its indices, ending in its sort, and unfolding to the nominal type over them at the declaration's own levels.
    ///
    /// **The one spelling of a former from an entry.** A former says nothing its entry does not: the lowering writes both from one telescope, and what an elaborated former holds beside its entry is measured against this (`std_former_derivation`).
    pub fn former(name: Global, island: Qualifier, nominal: Nominal) -> Self {
        let (universe_context, arity, result_sort) = match &nominal {
            Nominal::Inductive(declaration) => (
                &declaration.universe_context,
                &declaration.arity,
                &declaration.result_sort,
            ),
            Nominal::Struct(record) | Nominal::Concept { record, .. } => {
                (&record.universe_context, &record.arity, &record.result_sort)
            }
        };
        let universes = universe_context.identity_instance();

        // The entry's telescopes are closed, so any binders open them: counted from zero, under the hints the entry keeps.
        let mut params = Vec::new();
        let mut cursor = arity.cursor();
        while let (Some((hint, domain)), Some(mark)) = (cursor.entry(), cursor.mark()) {
            let binder = Free::local(params.len() as u32, hint);
            cursor.advance(Term::free_var(&binder));
            params.push((mark, binder, domain));
        }
        let within = cursor
            .body()
            .expect("a cursor past every entry holds its payload");
        // A family's indices follow its parameters, each explicit; a record's fields are no part of its former.
        let mut indices = Vec::new();
        if matches!(nominal, Nominal::Inductive(_)) {
            let mut cursor = within.cursor();
            while let Some((hint, domain)) = cursor.entry() {
                let binder = Free::local((params.len() + indices.len()) as u32, hint);
                cursor.advance(Term::free_var(&binder));
                indices.push((Plicity::Explicit, binder, domain));
            }
        }

        let variables = |binders: &[(Plicity, Free, Term)]| {
            binders
                .iter()
                .map(|(_, binder, _)| Term::free_var(binder))
                .collect::<Vec<_>>()
        };
        let applied = match &nominal {
            Nominal::Inductive(_) => {
                Term::induct_type_at(name, universes, variables(&params), variables(&indices))
            }
            Nominal::Struct(_) | Nominal::Concept { .. } => {
                Term::from(Subterm::StructType(StructType {
                    name,
                    universes,
                    params: variables(&params),
                }))
            }
        };
        // One call for the parameters and one for the indices, the indices innermost: `Vec : (T : Type) -> (n : Nat) -> Type`.
        let (type_, body) = [params, indices]
            .into_iter()
            .filter(|group| !group.is_empty())
            .rev()
            .fold((result_sort.clone(), applied), |(type_, body), group| {
                (
                    Term::func_type_marked(group.clone(), type_),
                    Term::func_marked(group, body),
                )
            });

        Self {
            name,
            universe_context: universe_context.clone(),
            kind: DefinitionKind::Nominal(Rc::new(nominal)),
            island,
            totality: Totality::default(),
            variances: Vec::new(),
            proof: false,
            type_,
            body,
        }
    }

    /// Every top-level name this definition mentions, by free variable.
    pub fn mentions(&self) -> BTreeSet<Global> {
        self.body
            .free_vars()
            .into_iter()
            .chain(self.type_.free_vars())
            .filter_map(|free| free.as_global().cloned())
            .collect()
    }

    /// Every top-level name this definition's elaboration consumed: what it [`mentions`](Self::mentions), plus the nominal heads its constructions and type-former normal forms name. Those live in the registry rather than the variable graph, so `mentions` cannot see them — and an item that builds a `Struct` or matches a `Variant` depends on that declaration exactly as one that names it does.
    ///
    /// This is the edge set an invalidation is closed over, a refusal is poisoned along and, with what an item's registry entries reach ([`Module::reaches`]), the kernel's order follows. [`mentions`](Self::mentions) is what the totality closure needs, which the variable edges alone serve.
    pub fn reaches(&self) -> BTreeSet<Global> {
        let mut names = self.mentions();
        names.extend(construction_heads(&self.type_));
        names.extend(construction_heads(&self.body));
        names
    }

    fn print(&self, formatter: &mut fmt::Formatter<'_>, spelling: &Rc<Spelling>) -> fmt::Result {
        write!(
            formatter,
            "{} : {} = {}",
            self.name.faithful(),
            self.type_.spelled(spelling),
            self.body.spelled(spelling)
        )
    }
}

/// A top-level item: a single `let` definition, or a `rec` group of mutually-recursive definitions (which may reference each other by `name`).
#[derive(Debug, Clone, PartialEq)]
#[curios_archive::archived]
pub enum Item {
    Let(Definition),
    Rec(RecItem),
}

impl Item {
    /// How a diagnostic names this item.
    ///
    /// An authored declaration is named by its path. A witness has no authored path — that is the point of `satisfy` — so it is named by the module it was declared in, which is the coordinate a reader can actually act on.
    pub fn describe(&self) -> String {
        let described = |definition: &Definition| match definition.name.qualifier() {
            Some(path) => path.join(),
            None => match definition.island.is_root() {
                true => "the witness in the entry module".to_string(),
                false => format!("the witness in '{}'", definition.island.join()),
            },
        };
        match self {
            Item::Let(definition) => described(definition),
            Item::Rec(rec) => rec
                .definitions()
                .iter()
                .map(described)
                .collect::<Vec<_>>()
                .join(", "),
        }
    }

    /// The names exported by this top-level item, in declaration order.
    pub fn declared_names(&self) -> Vec<&Global> {
        match self {
            Item::Let(definition) => vec![&definition.name],
            Item::Rec(rec) => rec
                .definitions
                .iter()
                .map(|definition| &definition.name)
                .collect(),
        }
    }

    /// The entries this item's formers carry, each under the name that declares it, in declaration order.
    pub fn nominals(&self) -> Vec<(&Global, &Nominal)> {
        match self {
            Item::Let(definition) => definition
                .kind
                .nominal()
                .map(|nominal| (&definition.name, nominal))
                .into_iter()
                .collect(),
            Item::Rec(rec) => rec
                .definitions
                .iter()
                .filter_map(|member| Some((&member.name, member.kind.nominal()?)))
                .collect(),
        }
    }

    /// The entry the former `name` carries, where this item declares a nominal under that name.
    pub fn nominal(&self, name: &Global) -> Option<&Nominal> {
        self.nominals()
            .into_iter()
            .find_map(|(held, nominal)| (held == name).then_some(nominal))
    }

    /// This item with the entry each of its formers carries replaced by what `entry` answers for the former's name and the entry it carries now, where it answers one.
    pub fn with_nominals(mut self, entry: impl Fn(&Global, &Nominal) -> Option<Nominal>) -> Self {
        let carried = |name: &Global, kind: &mut DefinitionKind| {
            if let DefinitionKind::Nominal(nominal) = kind
                && let Some(replaced) = entry(name, nominal)
            {
                *nominal = Rc::new(replaced);
            }
        };
        match &mut self {
            Item::Let(definition) => carried(&definition.name, &mut definition.kind),
            Item::Rec(rec) => {
                for member in &mut rec.definitions {
                    carried(&member.name, &mut member.kind);
                }
            }
        }

        self
    }

    /// The definitions this top-level item declares, in the same order as [`Item::declared_names`] — one for a `let`, one per member for a `rec`.
    ///
    /// Written out at each caller, the fan-out would be one more place a new `Item` variant could be missed. It belongs here beside `declared_names` for the same reason that one does: what an item declares is the item's own question.
    ///
    /// Owned rather than borrowed, because a `rec` member's [`Definition`] is *materialized* from the group rather than stored — there is nothing to hand a reference to.
    pub fn definitions(&self) -> Vec<Definition> {
        match self {
            Item::Let(definition) => vec![definition.clone()],
            Item::Rec(rec) => rec.definitions(),
        }
    }

    /// Every term this item holds, offered to `visit`: the positions a module is validated over (`item_positions`), each read as the terms it is made of. What a read of the item as it was written goes over, a term holding where it was written.
    pub fn each_term(&self, mut visit: impl FnMut(&Term)) {
        item_positions(self, &|_| false, &mut |_, carried: &dyn Carried| {
            carried.terms(&mut visit)
        });
    }
}

/// A program's entrypoint: the expression it closes with, with the type it is judged at. One type rather than two fields, so a type without an entry — a type for no expression — is unspellable rather than asserted away.
#[derive(Debug, Clone, PartialEq)]
pub struct Entrypoint {
    pub body: Term,
    /// The type the body is judged at. Before elaboration it is what whoever compiles the entry states — the program contract, or the empty proposition for an entry put as a proof — and absent only where nothing is stated, for elaboration to infer. Elaboration writes the type the body was judged at either way, so every stage after it reads the entry's type off the entry: [`Zonked::project`] refuses a program whose entry states none, and [`Zonked::entry`] hands it back beside the body.
    pub type_: Option<Term>,
}

/// A program: the unit it is compiled from, and the term it closes with.
///
/// The one compilation that has an entrypoint is the one that holds a `Program`; every other unit — a library, a prelude root, a recompile's reused items — is a [`Module`] alone. An optional entry on every module would be `None` for all of them but one, and each stage reading a module would carry a case for the entry it almost never has. Carried beside the module instead, the entry exists exactly where a program is compiled, and a stored unit cannot hold one.
#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub module: Module,
    pub entry: Entrypoint,
}

/// The whole of one unit as a *flat* list of top-level `items`. A nominal declaration's registry entry is held by the definition that is its former ([`Nominal`]), so the registries a stage checks against are read off the items and a module holds none beside them.
///
/// Flat rather than one N-deep nested `Subterm::Let`/`Rec` term: folding a unit into one would make its construction (`Scope::close` over the whole accumulator at each step) and every pass that recursed along its `.tail` spine O(N) in stack, overflowing at prelude depth. `Subterm::Let`/`Rec` remain for genuine *local*, in-expression bindings, which are shallow.
///
/// The default is the empty unit: no item and no mount. Every field is a collection, so it states nothing, and a module built by hand names only what it holds.
#[derive(Debug, Clone, Default, PartialEq)]
#[curios_archive::archived]
pub struct Module {
    /// The module's items, in the order it stores them: every one in hand where the module was made in this process, and each restored the first time it is read where the module was stored ([`Items`]).
    #[archived_with(crate::AsVector)]
    pub items: Items,
    /// The prefixes this module's compilation unit claims, each with whether it is a root only the compiler supplies.
    ///
    /// Carried here, once, rather than stamped onto every declaration. Which mount owns a declaration is [`Mount::owning`] over the declaration's own name, so a stamp beside the name would only restate the name's leading segment — and, archived, mean something solely in the compilation that wrote it. A later stage that needs a mount's kind reads it out of this list; nothing derives one from a string.
    pub mounts: Vec<Mount>,
    /// The definition names that are witness declarations. Elaboration registers each into the witness table when its signature elaborates — carried as names (not keys) because the table key needs the *elaborated* head, which only exists once elaboration runs.
    pub witnesses: BTreeSet<Global>,
    /// The definition names that are `test` declarations, in declaration order — the order the synthesized test tail schedules them and the runner reports them, so a `Vec` rather than a set.
    pub tests: Vec<Global>,
}

impl Module {
    /// The module archived as `bytes`, read by item where the bytes outlive every reader: its outline restored now — each item's names, what each is declared as and whether it is a group, which every compilation reads and which is small — and each item the first time it is read ([`Items`]), the entry a former carries with it, all through one pool, so a node an item shares with another is one allocation as it is in a module restored whole. `Err` is bytes that hold no module.
    ///
    /// **By item where the bytes are a constant of the executable, and whole otherwise.** An item is restored out of a view of the archive that was checked whole, and a view borrows its bytes: of a constant it lives as long as the process, and of bytes read for one unit it would have to be held beside what it borrows. So a module read out of a store's slot is restored whole, as every module was.
    pub fn stored(bytes: impl Into<curios_archive::Bytes>) -> Result<Self, String> {
        let archived = match bytes.into() {
            curios_archive::Bytes::Constant(bytes) => {
                curios_archive::access::<ArchivedModule>(bytes)?
            }
            read => return curios_archive::from_bytes::<Module>(read.as_slice()),
        };
        let pooled = std::rc::Rc::new(curios_archive::Pooled::new());
        let outlines = archived
            .items
            .iter()
            .map(|item| {
                // What a name is declared as is read off the archive where it lies: an entry a former carries is restored with its item, the first time one is read.
                let declared = |kind: &ArchivedDefinitionKind| match kind {
                    ArchivedDefinitionKind::Authored => Declared::Authored,
                    ArchivedDefinitionKind::Nominal(nominal) => match &**nominal {
                        ArchivedNominal::Inductive(_) => Declared::Inductive,
                        ArchivedNominal::Struct(_) => Declared::Struct,
                        ArchivedNominal::Concept { .. } => Declared::Concept,
                    },
                    ArchivedDefinitionKind::InductiveConstructor { .. } => Declared::Constructor,
                    ArchivedDefinitionKind::ConceptMethod { .. } => Declared::Method,
                    ArchivedDefinitionKind::Witness => Declared::Witness,
                    ArchivedDefinitionKind::Test => Declared::Test,
                };
                let (names, declared) = match item {
                    ArchivedItem::Let(definition) => (
                        vec![pooled.restore::<Global>(&definition.name)?],
                        vec![declared(&definition.kind)],
                    ),
                    ArchivedItem::Rec(rec) => (
                        rec.definitions
                            .iter()
                            .map(|definition| pooled.restore::<Global>(&definition.name))
                            .collect::<Result<_, _>>()?,
                        rec.definitions
                            .iter()
                            .map(|definition| declared(&definition.kind))
                            .collect(),
                    ),
                };

                Ok(Outline {
                    names,
                    declared,
                    group: matches!(item, ArchivedItem::Rec(_)),
                })
            })
            .collect::<Result<Vec<_>, String>>()?;
        let items = {
            let pooled = std::rc::Rc::clone(&pooled);

            Items::stored(outlines, move |place| {
                curios_profile::sample!("module::restored", place);
                pooled
                    .restore::<Item>(&archived.items[place])
                    .unwrap_or_else(|error| {
                        panic!("a stored module's item failed to restore: {error}")
                    })
            })
        };

        Ok(Self {
            items,
            mounts: pooled.restore(&archived.mounts)?,
            witnesses: pooled.restore(&archived.witnesses)?,
            tests: pooled.restore(&archived.tests)?,
        })
    }
}

impl Module {
    /// The entry the definition `name` carries, read at the place `places` keeps for it: one item read, and of a stored module one item restored.
    pub fn nominal(&self, places: &Places, name: &Global) -> Option<&Nominal> {
        match places.place(name)? {
            Place::Let(item) => match &self.items[item as usize] {
                Item::Let(definition) => definition.kind.nominal(),
                Item::Rec(_) => None,
            },
            Place::Member { item, member, .. } => match &self.items[item as usize] {
                Item::Rec(rec) => rec.definitions[member as usize].kind.nominal(),
                Item::Let(_) => None,
            },
        }
    }

    /// The inductive family `name` declares, read where `places` holds it.
    pub fn induct_decl(&self, places: &Places, name: &Global) -> Option<&InductDecl> {
        self.nominal(places, name)?.inductive()
    }

    /// The record `name` declares, a structure's or the one a concept's witnesses are values of, read where `places` holds it.
    pub fn struct_decl(&self, places: &Places, name: &Global) -> Option<&StructDecl> {
        self.nominal(places, name)?.record()
    }

    /// The concept `name` declares, read where `places` holds it.
    pub fn concept(&self, places: &Places, name: &Global) -> Option<&ConceptDecl> {
        self.nominal(places, name)?.concept()
    }

    /// The names the module declares a nominal under, each with which kind of declaration it is the former of, in the order the module stores them: read off what each place says it declares, with no item read.
    pub fn formers(&self) -> Vec<(Global, Declared)> {
        (0..self.items.len())
            .flat_map(|place| {
                let names = self.items.names(place).into_iter().copied();

                names.zip(self.items.declared(place))
            })
            .filter(|(_, declared)| declared.is_former())
            .collect()
    }

    /// Every entry the module's formers carry, each under the name that declares it, in the order the module stores its items. Reads every item: for a module in hand, never for a unit in scope, which is asked by name.
    pub fn nominals(&self) -> impl Iterator<Item = (&Global, &Nominal)> {
        self.items.iter().flat_map(Item::nominals)
    }

    /// Every inductive family the module declares, by name — see [`Module::nominals`].
    pub fn induct_decls(&self) -> BTreeMap<Global, &InductDecl> {
        self.nominals()
            .filter_map(|(name, nominal)| Some((*name, nominal.inductive()?)))
            .collect()
    }

    /// Every record the module declares, a structure's or a concept's, by name — see [`Module::nominals`].
    pub fn struct_decls(&self) -> BTreeMap<Global, &StructDecl> {
        self.nominals()
            .filter_map(|(name, nominal)| Some((*name, nominal.record()?)))
            .collect()
    }

    /// Every concept the module declares, by name — see [`Module::nominals`].
    pub fn concepts(&self) -> BTreeMap<Global, &ConceptDecl> {
        self.nominals()
            .filter_map(|(name, nominal)| Some((*name, nominal.concept()?)))
            .collect()
    }

    /// This module with every term hash-consed against `sharing` — one shared allocation for each structure as it is spelled, under no position — and with no universe constraint saying where it was raised.
    ///
    /// Built for an elaborated module about to be archived. Elaboration constructs the same types, telescopes, and proof spines independently in definition after definition, and nothing deduplicates them, because `Rc` sharing only ever arises from *cloning* a value: two definitions that build the same type build it twice. Unshared, the prelude's nodes outnumber its distinct structures several times over (the prelude build reports each root's distinct count), and the archive would store that expansion in full and every restored traversal walk it in full.
    ///
    /// Not for a lowered module, which a lint and a report are located in: a consed term says nothing of where it was written.
    pub fn shared(&self, sharing: &Sharing) -> Module {
        let kind = |kind: &DefinitionKind| match kind {
            DefinitionKind::Nominal(nominal) => {
                DefinitionKind::Nominal(Rc::new(nominal.shared(sharing)))
            }
            other => other.clone(),
        };
        let definition = |definition: &Definition| Definition {
            name: definition.name,
            kind: kind(&definition.kind),
            universe_context: definition.universe_context.unplaced(),
            island: definition.island,
            totality: definition.totality,
            proof: definition.proof,
            variances: definition.variances.clone(),
            type_: sharing.share(&definition.type_),
            body: sharing.share(&definition.body),
        };

        Module {
            items: self
                .items
                .iter()
                .map(|item| match item {
                    Item::Let(let_) => Item::Let(definition(let_)),
                    Item::Rec(rec) => Item::Rec(RecItem {
                        definitions: rec
                            .definitions
                            .iter()
                            .map(|member| RecDefinition {
                                name: member.name,
                                kind: kind(&member.kind),
                                island: member.island,
                                totality: member.totality,
                                proof: member.proof,
                            })
                            .collect(),
                        // Mapped in place rather than opened and re-closed: the round trip rebuilds every node twice and drops every memoized derivation with it, and the rebuilt nodes would escape this very pass.
                        group: rec
                            .group
                            .map_members(|term| sharing.share(term))
                            .with_universe_context(rec.group.universe_context().unplaced()),
                    }),
                })
                .collect(),
            mounts: self.mounts.clone(),
            witnesses: self.witnesses.clone(),
            tests: self.tests.clone(),
        }
    }

    /// Each nominal declaration's argument plicities, keyed by the family's name — parameters then indices, in the order a use site supplies them.
    ///
    /// Read off the type constructor's own definition, whose declared type is the one or two `FuncType`s lowering nests — the parameters', then an indexed family's indices' — ending in the sort: the parameters keep their declared marks and the indices are always explicit. That type is the marks' one home a checker establishes: `InductType` carries none (for a fixed name they are a function of the name, so storing them per-occurrence would be derived data that conversion must then either compare pointlessly or exclude from `Hash`, and excluding them lets hash-consing collapse differently-marked equal nodes), and the marks a registry entry's arity states for the elaborator ([`InductDecl::arity`], [`StructDecl::arity`]) are checked by no one, so a report does not read them.
    ///
    /// The formers are the definitions declared as one, which each place of the module says of its names with no item read ([`Items::declared`]), each then read where the module holds it and no other item with it: a member of a `rec` item for an inductive, which refers to itself, and a plain `let` for a structure or a concept. A nullary declaration has no `FuncType` wrapper at all and contributes nothing.
    fn nominal_plicities(&self, located: &Located) -> BTreeMap<Global, Vec<Plicity>> {
        let mut marks = BTreeMap::new();

        // Which definitions are type formers is what each place says it declares, so the formers are read and no other item is.
        let formers = self.declared_as(Declared::is_former);
        for name in &formers {
            let Some(def) = self.definition_at(located, name) else {
                continue;
            };
            // A type former's result is a sort, so the walk ends at the first node that is not a function type.
            let mut collected = Vec::new();
            let mut type_ = &def.type_;
            while let Subterm::FuncType(FuncType { telescope }) = &**type_ {
                collected.extend(telescope.marks());
                type_ = telescope.terminal();
            }
            if !collected.is_empty() {
                marks.insert(def.name, collected);
            }
        }

        marks
    }

    /// Where the module holds each name it declares, read off what each place declares with no item read: what lets a lexicon be made of the declarations it is about and no other.
    ///
    /// **Its own index, where [`Places`] would be the table.** A table of places is made of no module that declares a name twice, and such a module is refused in words that still have to be spelled; here the later declaration answers, as it does for every reader of a report.
    fn located(&self) -> Located {
        let mut located = HashMap::new();
        for place in 0..self.items.len() {
            for (member, name) in self.items.names(place).into_iter().enumerate() {
                located.insert(*name, (place, member));
            }
        }

        located
    }

    /// The names the module declares as a kind `wanted` holds of, in the order it stores them: read off what each place says it declares ([`Items::declared`]), with no item read.
    fn declared_as(&self, wanted: impl Fn(Declared) -> bool) -> Vec<Global> {
        (0..self.items.len())
            .flat_map(|place| {
                let names = self.items.names(place).into_iter();

                names.zip(self.items.declared(place))
            })
            .filter(|(_, declared)| wanted(*declared))
            .map(|(name, _)| *name)
            .collect()
    }

    /// The definition `name` names, read where `located` holds it, a group's member as [`RecItem::definitions`] materializes it: the one item is read, and where the module was stored the one item is restored.
    fn definition_at(&self, located: &Located, name: &Global) -> Option<Definition> {
        let &(place, member) = located.get(name)?;
        match &self.items[place] {
            Item::Let(definition) => Some(definition.clone()),
            Item::Rec(rec) => rec.definitions().into_iter().nth(member),
        }
    }

    /// What a report spells this unit's names by — see [`Lexicon`].
    pub fn lexicon(&self, syntax: &SyntaxRegistry) -> Lexicon {
        let located = self.located();

        Lexicon {
            symbols: Rc::new(Symbols::of(self.module_symbols())),
            plicities: Rc::new(self.nominal_plicities(&located)),
            witnesses: Rc::new(self.witness_spelling(&located, syntax)),
            members: Rc::new(self.members(&located)),
            declarations: Rc::new(Declarations {
                items: self.items.clone(),
                located,
            }),
        }
    }

    /// What this unit's structures and constructors are written by ([`Members`]): each structure's fields under their marks and labels, a concept's record among them, and the marks each constructor is called by.
    fn members(&self, located: &Located) -> Members {
        let mut members = Members::default();
        for name in &self.declared_as(Declared::is_former) {
            let Some(nominal) = self
                .definition_at(located, name)
                .and_then(|former| former.kind.nominal().cloned())
            else {
                continue;
            };
            if let Some(record) = nominal.record() {
                let fields = record.fields();
                let written = fields
                    .marks()
                    .into_iter()
                    .zip(fields.labels())
                    .map(|(mark, label)| (mark, (!label.is_empty()).then(|| label.to_string())))
                    .collect();
                members.fields.insert(*name, written);
            }
            if let Some(family) = nominal.inductive() {
                let constructors = family
                    .constructors
                    .iter()
                    .map(|(tag, case)| (tag.clone(), case.telescope.marks()))
                    .collect();
                members.payloads.insert(*name, constructors);
            }
        }

        members
    }

    /// What a report spells this unit's witnesses against (axis (h)): each concept's fields — a superclass edge's concept, or a method's wrapper, whether that wrapper takes the method's own parameters in its group, and the operator `syntax` names for it — beside its record, and each witness's declared type. `syntax` is the registry's because this crate may not spell a prelude declaration.
    fn witness_spelling(&self, located: &Located, syntax: &SyntaxRegistry) -> WitnessSpelling {
        let mut spelling = WitnessSpelling::default();
        for name in &self.declared_as(|declared| declared == Declared::Concept) {
            let Global::Authored(path) = name else {
                continue;
            };
            let Some(former) = self.definition_at(located, name) else {
                continue;
            };
            let Some(Nominal::Concept { concept, record }) = former.kind.nominal() else {
                continue;
            };
            let parameters = concept.params.len();
            // What an edge reaches is the elaborator's to record, so a module as it was lowered holds none: there an edge is a field under the witness mark, read by the name its type applies, which the table takes for a concept only where it knows it as one.
            let edges = match concept.supers.is_empty() {
                false => concept.edges(record.fields()),
                true => written_edges(record.fields()),
            };
            let fields = concept
                .fields
                .iter()
                .enumerate()
                .map(|(index, label)| {
                    if let Some((_, reached)) = edges.iter().find(|(position, _)| *position == index)
                    {
                        return FieldSpelling::Super(*reached);
                    }
                    let wrapper = Global::Authored(path.with(label));
                    // A method's wrapper takes the method's parameters beside the concept's and its witness; any other field's takes those alone.
                    let merged = self.definition_at(located, &wrapper).is_some_and(|def| {
                        matches!(&*def.type_, Subterm::FuncType(function) if function.plicities().len() > parameters + 1)
                    });
                    FieldSpelling::Method {
                        wrapper,
                        merged,
                        operator: syntax.operator.operator_for(path, label),
                    }
                })
                .collect();
            spelling.concepts.insert(
                *name,
                ConceptSpelling {
                    fields,
                    arity: record.arity.clone(),
                },
            );
        }

        // Which definitions are witnesses is what each place says it declares, as which are type formers is.
        for name in &self.declared_as(|declared| declared == Declared::Witness) {
            if let Some(def) = self.definition_at(located, name) {
                spelling.witnesses.insert(def.name, def.type_);
            }
        }

        spelling
    }

    /// Every top-level name `item`'s elaboration consumed: what the item [reaches](Item::reaches) through its definitions, plus what the registry entries it declares reach — a struct's field types, an inductive's constructor payloads and a concept's parameters live only in the entry, and an item whose entry names another declaration depends on it as its body would.
    pub fn reaches(&self, item: &Item) -> BTreeSet<Global> {
        let mut names = item.reaches();

        for (_, nominal) in item.nominals() {
            names.extend(nominal.reaches());
        }

        names
    }

    /// This module with its items in the order a unit is stored in: every item after the items it [reaches](Self::reaches), and where that leaves a choice, the one whose first declared name is least.
    ///
    /// The order is a function of what the items declare and name, and of nowhere they were written: a unit holds the same module wherever its declarations sit in its sources, and whether it was compiled whole or over a baseline. It is given once, where a module is made, and every walk after that takes the items as they come: the certifier, which refuses a name an item reaches before it is defined, and erasure.
    ///
    /// Items that reach one another have no such order. The least-named of what is left then goes first, so the walk ends, and the certifier refuses the name it finds unbound.
    pub fn ordered(mut self) -> Self {
        let count = self.items.len();
        let owner = self
            .items
            .iter()
            .enumerate()
            .flat_map(|(index, item)| {
                item.declared_names()
                    .into_iter()
                    .map(move |name| (*name, index))
            })
            .collect::<BTreeMap<_, _>>();
        let mut waiting = vec![0usize; count];
        let mut dependents = vec![Vec::new(); count];
        for (index, item) in self.items.iter().enumerate() {
            let reached = self
                .reaches(item)
                .into_iter()
                .filter_map(|name| owner.get(&name).copied())
                .filter(|dependency| *dependency != index)
                .collect::<BTreeSet<_>>();
            waiting[index] = reached.len();
            for dependency in reached {
                dependents[dependency].push(index);
            }
        }

        // By first name, then by place for items that declare none or share one, which no stored unit holds.
        let ranks = self
            .items
            .iter()
            .enumerate()
            .map(|(index, item)| (item.declared_names().first().map(|name| **name), index))
            .collect::<Vec<_>>();
        let mut left = ranks.iter().copied().collect::<BTreeSet<_>>();
        let mut ready = ranks
            .iter()
            .copied()
            .filter(|(_, index)| waiting[*index] == 0)
            .collect::<BTreeSet<_>>();
        let mut order = Vec::with_capacity(count);
        while !left.is_empty() {
            // Nothing ready with items left is a cycle, which the least-named of them breaks.
            let next = ready
                .pop_first()
                .unwrap_or_else(|| *left.first().expect("an item is left"));
            left.remove(&next);
            order.push(next.1);
            for dependent in &dependents[next.1] {
                waiting[*dependent] -= 1;
                if waiting[*dependent] == 0 && left.contains(&ranks[*dependent]) {
                    ready.insert(ranks[*dependent]);
                }
            }
        }

        let mut items = mem::take(&mut self.items)
            .into_iter()
            .map(Some)
            .collect::<Vec<_>>();
        self.items = order
            .into_iter()
            .map(|index| items[index].take().expect("an item is ordered once"))
            .collect();

        self
    }

    /// Every global qualified name in `self`: each definition (`let`/`rec`), each inductive type, each struct type. The universe a global is shortened *against*.
    pub fn module_symbols(&self) -> Vec<Global> {
        let mut symbols = Vec::new();
        // Asked of what each place declares, which a stored module knows without its item.
        for place in 0..self.items.len() {
            symbols.extend(self.items.names(place).into_iter().copied());
        }
        symbols
    }
}

impl Module {
    /// The spelling this module prints its terms in.
    ///
    /// Shortened against this module's own symbols (axis (b)) and nothing else, which is the one shortening site in the workspace that does not union its scope — both checkers' `format_with` take `&[&Module]` and merge. Deliberate, on three grounds. A value printing itself has no scope to be handed without ceasing to be `Display`. No ambiguity can follow from the narrower table: `build_shorten` records only names that actually shorten, and `Spelling::symbol` falls back to the full path, so a name from outside the unit prints qualified rather than misleadingly short. And a dump is read *about* the compiler, where a qualified `/std/Str/concat` beside a bare `append` says which unit each came from — the distinction a scope-wide table would erase. Its universes stay visible for the same reason a diagnostic suppresses them.
    fn spelling(&self) -> Rc<Spelling> {
        Rc::new(
            Spelling::default().with_short_names(Rc::new(build_shorten(&self.module_symbols()))),
        )
    }

    /// The items, one per line — printed by *iterating* the flat items (never re-folding into a nested term), so `wonder stage core` stays O(N) and never builds the nested term a deep module overflows on.
    fn print_items(
        &self,
        formatter: &mut fmt::Formatter<'_>,
        spelling: &Rc<Spelling>,
    ) -> fmt::Result {
        for item in &self.items {
            match item {
                Item::Let(def) => {
                    write!(formatter, "let ")?;
                    def.print(formatter, spelling)?;
                    writeln!(formatter, ";")?;
                }
                Item::Rec(rec) => {
                    write!(formatter, "rec ")?;
                    for (index, def) in rec.definitions().iter().enumerate() {
                        if index > 0 {
                            write!(formatter, "and ")?;
                        }
                        def.print(formatter, spelling)?;
                        write!(formatter, " ")?;
                    }
                    writeln!(formatter, ";")?;
                }
            }
        }

        Ok(())
    }
}

impl fmt::Display for Module {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.print_items(formatter, &self.spelling())
    }
}

impl fmt::Display for Program {
    /// The module's items, then the entry in the module's spelling, with the type it states beneath it.
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        let spelling = self.module.spelling();
        self.module.print_items(formatter, &spelling)?;
        write!(formatter, "{}", self.entry.body.spelled(&spelling))?;
        if let Some(type_) = &self.entry.type_ {
            write!(formatter, "\n: {}", type_.spelled(&spelling))?;
        }

        Ok(())
    }
}

impl Term {
    /// The top-level names this term reaches: its global free variables, and the nominal heads of its constructions and type-former normal forms, which live in the registry rather than the variable graph.
    pub fn reaches(&self) -> BTreeSet<Global> {
        term_reaches(self)
    }
}

impl Entrypoint {
    /// Every free local the entry's body or stated type mentions — what a walk judging the entry beside its module refuses, as [`free_locals_outside`] is for the module.
    pub fn free_locals(&self) -> BTreeSet<Free> {
        std::iter::once(&self.body)
            .chain(&self.type_)
            .flat_map(Term::free_vars)
            .filter(Free::is_local)
            .collect()
    }

    /// The top-level names the entry reaches, through its body and the type it states.
    pub fn reaches(&self) -> BTreeSet<Global> {
        let mut names = term_reaches(&self.body);
        names.extend(self.type_.iter().flat_map(term_reaches));
        names
    }
}

impl Item {
    /// The top-level names this item's definitions reach — [`Definition::reaches`] for a `let`; for a `rec`, its members read under the group's own binders rather than opened, so a member's reference to a sibling is not an edge to itself.
    pub fn reaches(&self) -> BTreeSet<Global> {
        match self {
            Item::Let(definition) => definition.reaches(),
            Item::Rec(rec) => {
                let mut names = BTreeSet::new();
                for member in rec.group.iter() {
                    names.extend(term_reaches(member.type_.body()));
                    names.extend(term_reaches(member.body.body()));
                }
                names
            }
        }
    }
}

impl InductDecl {
    /// The top-level names this declaration reaches: through its arity, its result sort and its constructors' signatures.
    pub fn reaches(&self) -> BTreeSet<Global> {
        let mut names = arity_reaches(&self.arity);
        names.extend(term_reaches(&self.result_sort));
        for (_, constructor) in &self.constructors {
            names.extend(payload_reaches(&constructor.telescope));
        }
        names
    }
}

impl StructDecl {
    /// The top-level names this declaration reaches: through its arity, fields included, and its result sort.
    pub fn reaches(&self) -> BTreeSet<Global> {
        let mut names = arity_reaches(&self.arity);
        names.extend(term_reaches(&self.result_sort));
        names
    }
}

impl ConceptDecl {
    /// The top-level names this declaration reaches through its parameters; its fields are its record entry's.
    pub fn reaches(&self) -> BTreeSet<Global> {
        fields_reaches(&self.params)
    }
}

/// The names `term` reaches: its global free variables and its construction heads.
fn term_reaches(term: &Term) -> BTreeSet<Global> {
    let mut names = term
        .free_vars()
        .into_iter()
        .filter_map(|free| free.as_global().cloned())
        .collect::<BTreeSet<_>>();
    names.extend(construction_heads(term));

    names
}

/// The nominal heads `term`'s constructions and type-former normal forms name.
///
/// On the shared walk driver, deduplicated on node identity, for the reason every walk over shared structure is: a string literal's scan chain is linear in nodes and quadratic in paths, and a walk that revisits shared nodes pays the square.
fn construction_heads(term: &Term) -> BTreeSet<Global> {
    let mut state: (HashSet<Term>, BTreeSet<Global>) = (HashSet::new(), BTreeSet::new());
    term.walk(
        &mut state,
        |state, term| {
            if !state.0.insert(term.clone()) {
                return Enter::Skip(());
            }
            match &**term {
                Subterm::InductType(node) => {
                    state.1.insert(node.name);
                }
                Subterm::Variant(node) => {
                    state.1.insert(node.name);
                }
                Subterm::StructType(node) => {
                    state.1.insert(node.name);
                }
                Subterm::Struct(node) => {
                    state.1.insert(node.name);
                }
                _ => {}
            }
            Enter::Descend
        },
        |_, _, _| (),
    );

    state.1
}

/// A telescope's entry types in order, and what it ends in.
fn entries<B: Bound>(telescope: &Telescope<B>) -> (Vec<&Term>, &B) {
    let mut types = Vec::new();
    let mut rest = telescope;
    loop {
        match rest {
            Telescope::Cons(_, type_, scope) => {
                types.push(type_);
                rest = scope.body();
            }
            Telescope::Done(done) => return (types, done),
        }
    }
}

/// The names a field telescope reaches.
fn fields_reaches(fields: &Telescope<()>) -> BTreeSet<Global> {
    entries(fields)
        .0
        .into_iter()
        .flat_map(term_reaches)
        .collect()
}

/// The names a declaration's arity reaches: its parameters, then the telescope they end in.
fn arity_reaches(arity: &Telescope<Telescope<()>>) -> BTreeSet<Global> {
    let (params, fields) = entries(arity);
    params
        .into_iter()
        .flat_map(term_reaches)
        .chain(fields_reaches(fields))
        .collect()
}

/// The names a constructor's signature reaches: its payloads, then the index targets they end in.
fn payload_reaches(signature: &Telescope<Vec<Term>>) -> BTreeSet<Global> {
    let (payloads, targets) = entries(signature);
    payloads
        .into_iter()
        .chain(targets.iter())
        .flat_map(term_reaches)
        .collect()
}

/// What sits at a module position, read as the terms it is made of: a term is itself, a list each of its own, and a telescope each entry's type and then what it ends in.
trait Terms {
    fn terms(&self, visit: &mut dyn FnMut(&Term));
}

impl Terms for Term {
    fn terms(&self, visit: &mut dyn FnMut(&Term)) {
        visit(self);
    }
}

impl Terms for Vec<Term> {
    fn terms(&self, visit: &mut dyn FnMut(&Term)) {
        self.iter().for_each(visit);
    }
}

impl Terms for () {
    fn terms(&self, _: &mut dyn FnMut(&Term)) {}
}

impl<B: Bound + Terms> Terms for Telescope<B> {
    fn terms(&self, visit: &mut dyn FnMut(&Term)) {
        let (types, done) = entries(self);
        types.into_iter().for_each(&mut *visit);
        done.terms(visit);
    }
}

/// One question a walk asks of whatever sits at a module position.
///
/// A [`Bound`] behind a trait object rather than a generic parameter, so [`item_positions`] can offer one list of positions to more than one collector. Three reads: the two identities findable by looking at a term, which are the local binder indices it mentions and whether a metavariable node survives in it, and the terms themselves, for a read that asks where each was written.
trait Carried {
    fn free_vars(&self) -> BTreeSet<Free>;
    fn has_metavar(&self) -> bool;
    fn terms(&self, visit: &mut dyn FnMut(&Term));
}

impl<B: Bound + Terms> Carried for B {
    fn free_vars(&self) -> BTreeSet<Free> {
        Bound::free_vars(self)
    }

    fn has_metavar(&self) -> bool {
        Bound::has_metavar(self)
    }

    fn terms(&self, visit: &mut dyn FnMut(&Term)) {
        Terms::terms(self, visit);
    }
}

/// Every position of `item` that can hold a bound value, offered to `visit` with the top-level name owning it, skipping whatever `in_scope` already answers for: each definition's type and body, a recursive group's opened against its members' names, then what the entry of each former it declares carries.
///
/// One list, read by every walk that asks what a module or one item of it carries — the kernel's free-local refusal below it, the storage refusal beside that, and the read of an item's terms as they were written ([`Item::each_term`]). A second copy is how a position quietly stops being covered, which is the failure the enumeration exists to prevent, so a new question about an item's contents is a new `visit` and never a new walk.
fn item_positions(
    item: &Item,
    in_scope: &impl Fn(&Global) -> bool,
    visit: &mut impl FnMut(Option<&Global>, &dyn Carried),
) {
    let names = item.declared_names();
    if names.is_empty() || !names.into_iter().all(in_scope) {
        for definition in item.definitions() {
            visit(Some(&definition.name), &definition.type_);
            visit(Some(&definition.name), &definition.body);
        }
    }

    for (name, nominal) in item
        .nominals()
        .into_iter()
        .filter(|(name, _)| !in_scope(name))
    {
        if let Some(declaration) = nominal.inductive() {
            visit(Some(name), &declaration.arity);
            visit(Some(name), &declaration.result_sort);
            for (_, constructor) in &declaration.constructors {
                visit(Some(name), &constructor.telescope);
            }
        }
        if let Some(record) = nominal.record() {
            visit(Some(name), &record.arity);
            visit(Some(name), &record.result_sort);
        }
        if let Some(concept) = nominal.concept() {
            visit(Some(name), &concept.params);
        }
    }
}

/// Every position in `module` that can hold a bound value, item by item — see [`item_positions`]. `None` is the entrypoint, which belongs to the module rather than to any name it declares.
fn module_positions(
    module: &Module,
    in_scope: impl Fn(&Global) -> bool,
    mut visit: impl FnMut(Option<&Global>, &dyn Carried),
) {
    for item in module.items.iter() {
        item_positions(item, &in_scope, &mut visit);
    }
}

/// An identity a stored unit may not carry: one meaningful only in the compilation that assigned it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Positional {
    /// A free local binder. Its index came from one compilation's binder counter, and every walk mints from a counter of its own that starts at zero — so a local surviving into stored output is an index two compilations can both hand out.
    FreeLocal { owner: Option<Global>, index: u32 },
    /// A term metavariable. Zonking is contracted to substitute every solution and to refuse an unsolved hole, so one reaching here is that contract broken rather than a hole still to be solved.
    Metavar { owner: Option<Global> },
    /// A witness this module declares, scoped to a mount it does not own. Its name holds the module that declares it, so one carrying somebody else's is a name two units can both declare — the aliasing that would silently rebind a coherence-table entry.
    UnscopedWitness { witness: Global },
    /// A witness this module declares under its place among its module's witnesses ([`WitnessId::Unspelled`]): its signature spelled no key, which registration refuses, so one reaching a store is a name the same text written in another order gives to another declaration.
    PlacedWitness { witness: Global },
}

impl fmt::Display for Positional {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (owner, carried) = match self {
            Positional::FreeLocal { owner, index } => (owner, format!("free local binder {index}")),
            Positional::Metavar { owner } => (owner, "an unsolved metavariable".to_string()),
            Positional::UnscopedWitness { witness } => {
                return write!(
                    formatter,
                    "{} is declared here and scoped to a mount this module does not own",
                    witness.faithful()
                );
            }
            Positional::PlacedWitness { witness } => {
                return write!(
                    formatter,
                    "{} is declared here under its place in its module, its signature spelling no key",
                    witness.faithful()
                );
            }
        };

        match owner {
            Some(name) => write!(formatter, "{} carries {carried}", name.faithful()),
            None => write!(formatter, "the entrypoint carries {carried}"),
        }
    }
}

/// Refuse `module` if it carries an identity meaningful only in the compilation that produced it.
///
/// **A unit may be stored only if it carries no positional identity.** Storing one is how rustc came to need `cnum_map` — an index another compilation reads and then has to remap — and it is the property deciding whether a stored unit is portable at all.
///
/// Four of the classes are refused here. The remaining one, an unsolved universe metavariable, is refused at the same seam by `curios-elab`'s `validate_universes`, which names it in as many words; restating it would be a second implementation of one predicate rather than a second opinion about it, which is the standing [`UniverseContext::is_closed`] holds for the same reason.
///
/// The two witness classes are asked differently from the other two, and deliberately. Those are read off the module's *terms*, because a metavariable or a free local anywhere in one is disqualifying. A witness reference is not: a stored unit legitimately mentions witnesses its predecessors declared, scoped to *their* mounts. What must hold is that every witness this module **declares** is scoped to a mount it owns and named by what it witnesses, which are questions about `Module::witnesses` rather than about any position — so they are asked over the declarations and not through the walk.
///
/// It refuses the free locals [`free_locals_outside`] reports, over the same positions, at the other seam: the kernel refuses one where it walks a module, and this refuses one where a module is stored. Neither has a safe direction to degrade in — a local reaching either aliases silently with a binder some later walk mints, which admits rather than crashes. It still *describes* rather than judges by this module's rule — whether a node is a metavariable, and whether a variable is local, are properties of the representation, taking no reduction, no conversion and no `Env`.
pub fn validate_stored_identities(module: &Module) -> Result<(), Positional> {
    let mut found: Option<Positional> = None;

    module_positions(
        module,
        |_| false,
        |owner, carried| {
            if found.is_some() {
                return;
            }

            if carried.has_metavar() {
                found = Some(Positional::Metavar {
                    owner: owner.cloned(),
                });
                return;
            }

            if let Some(index) = carried
                .free_vars()
                .into_iter()
                .find_map(|free| free.local_index())
            {
                found = Some(Positional::FreeLocal {
                    owner: owner.cloned(),
                    index,
                });
            }
        },
    );

    if let Some(witness) = module.witnesses.iter().find(|witness| match witness {
        // Containment rather than equality: the identity names the *module* that declares it, which lies within one of this unit's mounts rather than being one.
        Global::Witness(id) => Mount::owning(&module.mounts, id.module()).is_none(),
        Global::Authored(_) => false,
    }) {
        return Err(Positional::UnscopedWitness { witness: *witness });
    }
    if let Some(witness) = module
        .witnesses
        .iter()
        .find(|witness| matches!(witness, Global::Witness(WitnessId::Unspelled { .. })))
    {
        return Err(Positional::PlacedWitness { witness: *witness });
    }

    found.map_or(Ok(()), Err)
}

/// Every free local `module`'s terms mention outside what `in_scope` already answers for, each with the top-level name owning the position it was found in.
///
/// **What a checker refuses before it mints a binder of its own.** Both checkers mint from a counter no predecessor raised — the kernel's from zero, the elaborator's from its own unit's lowering count — because no term a walk is handed from elsewhere carries a local ([`validate_stored_identities`]). A free local reaching a walk is therefore one its counter could hand out again, and the binder it mints while comparing under a telescope or eta-contracting would silently identify two terms that differ. The judgment could not see it: by the time it looks the local up, the local is bound. So the refusal is taken at the boundary, over every position a term can sit in, rather than where a judgment meets it.
///
/// It lives here by this module's own rule, the one stated at the top: it *describes* rather than judges. Whether a variable is local asks nothing of a kernel — no reduction, no conversion, no `Env` — so it is a property of the data, and a second implementation would be a second run of the same function rather than a second opinion. That is the standing `UniverseContext::is_closed` has for the same reason.
///
/// The predicate is over *names* rather than over a position, which is what lets one answer cover four namespaces at once: a name is one declaration, and the walk handing the predicate in answers for the declarations it does not judge, the ones a baseline holds term for term. An item is skipped only when *every* name it declares is already answered for, since the walk that answered for it refused a local of its own.
pub fn free_locals_outside(
    module: &Module,
    in_scope: impl Fn(&Global) -> bool,
) -> BTreeSet<(Option<Global>, Free)> {
    let mut found = BTreeSet::new();

    module_positions(module, in_scope, |owner, carried| {
        found.extend(
            carried
                .free_vars()
                .into_iter()
                .filter(Free::is_local)
                .map(|local| (owner.copied(), local)),
        );
    });

    found
}

/// Evidence that a module is finished with elaboration's own syntax — the kernel's whole `NotCore` class: no `Metavar` and no `Transient` node survives in any term-bearing position, no level in one holds a universe metavariable, and a program's entry states the type it was judged at. `curios-elab`'s elaboration and zonk are the passes that make a module satisfy this; the validating [`Zonked::project`] is how any holder of a `Module` re-establishes it at a stage boundary, cheaply, because `has_metavar`, `has_transient` and `has_universe_meta` are per-node cached derivations.
///
/// The wrapper is interface-level evidence, never a license to trust: the kernel keeps its own metavariable refusals, so a `Zonked` constructed wrongly is still caught where soundness lives.
#[derive(Debug, Clone)]
pub struct Zonked<T>(T);

/// What [`Zonked`] can be evidence of: something elaboration and zonk are answerable for, which names the first place it is not finished.
pub trait Zonkable: Clone {
    /// The first place elaboration-only syntax survives — or, for a program, where its entry states no type — or `None` when there is none.
    fn unfinished(&self) -> Option<String>;
}

impl Zonkable for Module {
    fn unfinished(&self) -> Option<String> {
        zonked_refusal(self)
    }
}

impl Zonkable for Program {
    /// The module's first unfinished place, then the entry's: a program is finished when its module is and its entry states the type it was judged at, meta-free.
    fn unfinished(&self) -> Option<String> {
        zonked_refusal(&self.module).or_else(|| {
            let unfinished = |term: &Term| {
                term.has_metavar() || term.has_transient() || term.has_universe_meta()
            };
            match &self.entry.type_ {
                None => Some("the entrypoint states no type it was judged at".to_owned()),
                Some(type_) if unfinished(&self.entry.body) || unfinished(type_) => {
                    Some("elaboration-only syntax survives in the entrypoint".to_owned())
                }
                Some(_) => None,
            }
        })
    }
}

impl<T: Zonkable> Zonked<T> {
    /// Validate that `value` is zonked and take evidence of it. The clone is cheap — items share their terms by `Rc`.
    pub fn project(value: &T) -> Result<Self, ZonkedRefusal> {
        match value.unfinished() {
            Some(place) => Err(ZonkedRefusal { place }),
            None => Ok(Self(value.clone())),
        }
    }
}

impl Zonked<Module> {
    pub fn as_module(&self) -> &Module {
        &self.0
    }

    pub fn into_module(self) -> Module {
        self.0
    }

    /// The carried module rewritten by a transform that preserves meta-freedom. The preservation claim is the caller's; debug builds re-validate it.
    pub fn map(self, rewrite: impl FnOnce(Module) -> Module) -> Self {
        let rewritten = rewrite(self.0);
        debug_assert!(
            zonked_refusal(&rewritten).is_none(),
            "a Zonked::map rewrite must preserve meta-freedom"
        );
        Self(rewritten)
    }
}

impl Zonked<Program> {
    pub fn as_program(&self) -> &Program {
        &self.0
    }

    /// The program's module, with the evidence its projection established. The clone is cheap — items share their terms by `Rc`.
    pub fn module(&self) -> Zonked<Module> {
        Zonked(self.0.module.clone())
    }

    /// The entrypoint's body and the type it was judged at.
    pub fn entry(&self) -> (&Term, &Term) {
        let entry = &self.0.entry;
        let type_ = entry
            .type_
            .as_ref()
            .expect("projection refuses an entry that states no type");
        (&entry.body, type_)
    }
}

/// Why [`Zonked::project`] refused: the first term-bearing place where a metavariable or a transient survived, or the uncleared seeds.
#[derive(Debug)]
pub struct ZonkedRefusal {
    place: String,
}

impl fmt::Display for ZonkedRefusal {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(formatter, "the module is not zonked: {}", self.place)
    }
}

/// The term-bearing places of a module elaboration and zonk are answerable for — a definition's type and body and the telescopes of the entry a former carries — walked with the cached `has_metavar`, `has_transient` and `has_universe_meta` bits, first offender wins.
fn zonked_refusal(module: &Module) -> Option<String> {
    fn unfinished(term: &Term) -> bool {
        term.has_metavar() || term.has_transient() || term.has_universe_meta()
    }
    fn unfinished_bound<B: Bound>(value: &B) -> bool {
        value.has_metavar() || value.has_transient() || value.has_universe_meta()
    }

    for item in &module.items {
        let survives = match item {
            Item::Let(definition) => unfinished(&definition.type_) || unfinished(&definition.body),
            Item::Rec(rec) => rec
                .group
                .iter()
                .any(|member| unfinished(member.type_.body()) || unfinished(member.body.body())),
        };
        if survives {
            return Some(format!(
                "elaboration-only syntax survives in {}",
                item.describe()
            ));
        }
    }
    for (name, decl) in module.induct_decls() {
        if unfinished_bound(&decl.arity)
            || unfinished(&decl.result_sort)
            || decl
                .constructors
                .iter()
                .any(|(_, param)| unfinished_bound(&param.telescope))
        {
            return Some(format!(
                "elaboration-only syntax survives in the registry entry for {}",
                name.faithful()
            ));
        }
    }
    for (name, decl) in module.struct_decls() {
        if unfinished_bound(&decl.arity) || unfinished(&decl.result_sort) {
            return Some(format!(
                "elaboration-only syntax survives in the registry entry for {}",
                name.faithful()
            ));
        }
    }
    None
}

#[cfg(test)]
mod tests;
