//! The written sites an elaboration opened, each with the type it was opened at: what is classified where the site's declaration files its record, which the binder lints it is filed with are worded by ([`SiteClass`], [`binder_lints`]).
//!
//! **A class is a lint's question, which never changes a verdict.** It is decided under an allowance of its own ([`LINT_ALLOWANCE`]), separate from the declaration's budget, which it leaves as it found it; whatever it solves is rolled back and nothing is credited; and an allowance run out is no class. What it reads of other declarations — a struct's fields, whether a monad is one — joins the declaration's trace, so a record a recompile keeps is still true of what it read.
//!
//! **Where a site is opened.** A `let`'s binder where `elaborate_let` binds it, against the region its block is checked at; a lambda's parameter where its type is checked; a match pattern's binder where the matrix compiler binds it, by a `let` of its own, as a slot ([`Context::note_pattern_site`]); and a match pattern's `_` where the matrix compiler reads the column it stands at, whose type is inferred under the allowance as a class is decided ([`Context::open_column_site`]). An arm's whole `_` holds no place and opens nothing.

use {
    super::Spend,
    crate::{
        Context, LINT_ALLOWANCE, Mode, SiteClass, Traces, binder_lints, elaborate, reduce_with,
        zonk_solved_term_metas,
    },
    curios_core::{Global, HeadKey, Lint, Subterm, Term, WitnessKey},
    std::{
        collections::{BTreeMap, BTreeSet},
        rc::Rc,
    },
};

/// The binder lints an elaboration that ran holds, whatever it concluded: those of every declaration its walk's traces hold — elaborated now, adopted from a store or kept from a baseline — and those of an entry's final term, which no record holds, decided once it has run. What a unit is made with, and what a refused one is reported with.
#[derive(Debug, Clone, Default)]
pub struct Observed {
    pub lints: Vec<Lint>,
}

/// How many structs of one field a class reads through to the action one holds: `Try(Io, E, A)` holds `Io(Result(E, A))` one deep.
const STRUCT_DEPTH: usize = 4;

/// One written site as it was opened: its declaration (`None` for an entry's final term), its place among the declaration's written binders, the type it binds, and what it is opened as.
#[derive(Debug, Clone)]
struct Opened {
    declaration: Option<Global>,
    place: u32,
    type_: Term,
    binding: Binding,
}

/// What a site is opened as, which says what its class is asked for.
#[derive(Debug, Clone)]
pub enum Binding {
    /// A `let`'s binder, which may hold an action: its type is reduced, through the one field of a struct and against the region its block is checked at, where it is (`Some`).
    Let(Option<Term>),
    /// A parameter, or a pattern's binder, which a lint asks only whether it binds `{}`: read off its type as it stands, so deciding it reads no declaration and adds nothing to what its declaration's reuse depends on. A `{}` spelled through an alias has no class.
    Slot,
}

/// The written sites an elaboration opened and has not classified yet, in the order they were opened: a list, so a rollback truncates it with the solutions the sites were opened beside.
#[derive(Debug, Default)]
pub struct Sites {
    opened: Vec<Opened>,
    /// The places of a match pattern's binders, which the matrix compiler binds by `let`s of its own: what [`Context::site_binding`] opens as slots. A place is a pattern's or a `let`'s, never both, so nothing here is rolled back.
    patterns: BTreeSet<(Option<Global>, u32)>,
}

impl Sites {
    /// How many sites stand opened: the watermark a rollback takes them back to.
    pub fn mark(&self) -> usize {
        self.opened.len()
    }

    /// Take back every site opened since `mark`: one opened inside what is rolled back binds nothing that stands.
    pub fn truncate(&mut self, mark: usize) {
        self.opened.truncate(mark);
    }

    /// Take out every site whose declaration `of` holds of.
    fn take(&mut self, of: impl Fn(&Option<Global>) -> bool) -> Vec<Opened> {
        let (taken, kept) = std::mem::take(&mut self.opened)
            .into_iter()
            .partition(|site| of(&site.declaration));
        self.opened = kept;
        taken
    }
}

impl Context<'_> {
    /// Note that the written binder at `place` is a match pattern's, which the matrix compiler binds by a `let` of its own.
    pub(crate) fn note_pattern_site(&mut self, place: Option<u32>) {
        if let Some(place) = place {
            self.underway
                .sites
                .patterns
                .insert((self.underway.declaration, place));
        }
    }

    /// What the written binder at `place`, bound by a `let` in a block checked at `region`, is opened as: a slot where it is a match pattern's ([`Context::note_pattern_site`]), a `let`'s binder otherwise.
    pub(crate) fn site_binding(&self, place: Option<u32>, region: Option<&Term>) -> Binding {
        match place.is_some_and(|place| {
            self.underway
                .sites
                .patterns
                .contains(&(self.underway.declaration, place))
        }) {
            true => Binding::Slot,
            false => Binding::Let(region.cloned()),
        }
    }

    /// Record the written `_` at `place` as standing at `column`, a column of a match the matrix compiler is reading, which binds nothing for it: a slot of the column's type, inferred under [`LINT_ALLOWANCE`] with the declaration's budget set aside and whatever it solved rolled back.
    pub(crate) fn open_column_site(&mut self, place: u32, column: &Term) {
        let budget = std::mem::replace(&mut self.typing.spend, Spend::new(LINT_ALLOWANCE));
        let mark = self.solution_mark();
        let typed = elaborate(self, column, Mode::Infer)
            .map(|(_, type_)| zonk_solved_term_metas(self, &type_));
        self.rollback_solutions(mark);
        self.end_solutions(mark);
        curios_profile::sample!("sites::column", self.typing.spend.consumed().units());
        self.typing.spend = budget;
        if let Ok(type_) = typed {
            self.open_site(Some(place), &type_, Binding::Slot);
        }
    }

    /// Record that the written site at `place`, where the lowering wrote one, was opened at `type_` as `binding`.
    pub(crate) fn open_site(&mut self, place: Option<u32>, type_: &Term, binding: Binding) {
        let Some(place) = place else {
            return;
        };
        self.underway.sites.opened.push(Opened {
            declaration: self.underway.declaration,
            place,
            type_: type_.clone(),
            binding,
        });
    }

    /// The class of each site of a definition `names` names, decided now and taken out, each by its definition and its place: what the record of the declaration declaring them holds. A place opened more than once — a binder in an arm of alternatives — keeps a class only where every opening agrees.
    pub(crate) fn classes_in(&mut self, names: &[Global]) -> Vec<(Global, u32, SiteClass)> {
        let opened = self.underway.sites.take(|declaration| {
            declaration.is_some_and(|declaration| names.contains(&declaration))
        });

        self.classes_of(opened)
            .into_iter()
            .filter_map(|((declaration, place), class)| Some((declaration?, place, class)))
            .collect()
    }

    /// The binder lints of the declaration declaring `names`, decided as its record is filed: the sites its lowering found no written name reads ([`curios_core::Mints::unread`]), less every binder a proof it wrote read, worded by the class each site's openings decide now. Asked before the declaration's trace is taken, since what deciding a class reads is the declaration's too.
    pub(crate) fn lints_in(&mut self, names: &[Global]) -> Vec<Lint> {
        let classes = self
            .classes_in(names)
            .into_iter()
            .map(|(name, place, class)| ((name, place), class))
            .collect::<BTreeMap<_, _>>();
        let credited = self.credited_in(names).into_iter().collect::<BTreeSet<_>>();
        // A declaration's names share what it minted, so its first holds the sites of every one.
        let minted = Rc::clone(&self.underway.minted);
        let sites = names
            .first()
            .map_or(&[][..], |name| minted.of(name).unread.as_slice());

        binder_lints(
            sites,
            |binder| {
                binder
                    .declaration
                    .is_some_and(|name| credited.contains(&(name, binder.ordinal)))
            },
            |binder| classes.get(&(binder.declaration?, binder.ordinal)).copied(),
        )
    }

    /// Hold the binder lints of every declaration `traces` holds, the walk's as it ends, for [`Context::observed`] to hand out whatever the walk concluded.
    pub(crate) fn hold_lints(&mut self, traces: &Traces) {
        self.underway.held = traces
            .declarations()
            .flat_map(|(_, record)| record.concluded().lints.iter().cloned())
            .collect();
    }

    /// The binder lints this elaboration holds ([`Observed`]): the declarations' its walk held, and an entry's final term's, decided now from what its lowering found no written name reads and the proofs written in it, worded by the class of each of its sites, taken out. Asked once the elaboration has run, since only elaboration writes a proof.
    pub fn observed(&mut self) -> Observed {
        let opened = self.underway.sites.take(Option::is_none);
        let classes = self
            .classes_of(opened)
            .into_iter()
            .map(|((_, place), class)| (place, class))
            .collect::<BTreeMap<_, _>>();
        let credited = self.credited();
        let minted = Rc::clone(&self.underway.minted);
        let entry = binder_lints(
            &minted.entry.unread,
            |binder| credited.contains(&(binder.declaration, binder.ordinal)),
            |binder| {
                binder
                    .declaration
                    .is_none()
                    .then(|| classes.get(&binder.ordinal).copied())
                    .flatten()
            },
        );

        Observed {
            lints: std::mem::take(&mut self.underway.held)
                .into_iter()
                .chain(entry)
                .collect(),
        }
    }

    /// The class each site of `opened` agrees on.
    fn classes_of(&mut self, opened: Vec<Opened>) -> BTreeMap<(Option<Global>, u32), SiteClass> {
        let mut classes = BTreeMap::<(Option<Global>, u32), Option<SiteClass>>::new();
        for site in opened {
            let class = match &site.binding {
                Binding::Let(region) => self.class_of(&site.type_, region.as_ref()),
                Binding::Slot => Self::slot_class(&zonk_solved_term_metas(self, &site.type_)),
            };
            classes
                .entry((site.declaration, site.place))
                .and_modify(|held| {
                    if *held != class {
                        *held = None;
                    }
                })
                .or_insert(class);
        }

        classes
            .into_iter()
            .filter_map(|(site, class)| Some((site, class?)))
            .collect()
    }

    /// The class of a site opened at `type_`, decided under [`LINT_ALLOWANCE`] with the declaration's budget set aside, and whatever it solved rolled back.
    fn class_of(&mut self, type_: &Term, region: Option<&Term>) -> Option<SiteClass> {
        let budget = std::mem::replace(&mut self.typing.spend, Spend::new(LINT_ALLOWANCE));
        let mark = self.solution_mark();
        let type_ = zonk_solved_term_metas(self, type_);
        let region = region.map(|region| zonk_solved_term_metas(self, region));
        let class = self.classify(&type_, region.as_ref(), STRUCT_DEPTH);
        self.rollback_solutions(mark);
        self.end_solutions(mark);
        curios_profile::sample!("sites::classify", self.typing.spend.consumed().units());
        self.typing.spend = budget;

        class
    }

    /// What a slot of `type_` is to a lint: `{}` where it is written so, and nothing else asked of it.
    fn slot_class(type_: &Term) -> Option<SiteClass> {
        matches!(&**type_, Subterm::TupleType(tuple) if tuple.telescope.is_empty())
            .then_some(SiteClass::Unit)
    }

    /// What `type_` is to a lint: `{}`, an action, or neither. An action is an `Io`, a struct of one field holding an action, or an application of the monad of `region` — the type a `!` reads its region off, a monad where a `Monad` witness is declared at its head.
    fn classify(&mut self, type_: &Term, region: Option<&Term>, depth: usize) -> Option<SiteClass> {
        let reduced = reduce_with(self, type_).ok()?;
        if let Subterm::TupleType(tuple) = &*reduced
            && tuple.telescope.is_empty()
        {
            return Some(SiteClass::Unit);
        }
        let head = HeadKey::of_whnf(&reduced);
        if head == Some(HeadKey::Io) {
            return Some(SiteClass::Action);
        }
        if let (Some(head), Some(region)) = (&head, region)
            && self.is_monad_head(head)
            && reduce_with(self, region)
                .ok()
                .and_then(|region| HeadKey::of_whnf(&region))
                .as_ref()
                == Some(head)
        {
            return Some(SiteClass::Action);
        }
        let Subterm::StructType(structure) = &*reduced else {
            return None;
        };
        let declaration = self.struct_decl(&structure.name)?.clone();
        if depth == 0 || declaration.field_count() != 1 {
            return None;
        }
        let field = declaration
            .at(&structure.universes)
            .fields_at(&structure.params)
            .ok()?
            .entry_type(0)?
            .clone();

        (self.classify(&field, None, depth - 1) == Some(SiteClass::Action))
            .then_some(SiteClass::Action)
    }

    /// Whether a `Monad` witness is declared at `head`: what makes a region headed by it one a `!` sequences in.
    fn is_monad_head(&self, head: &HeadKey) -> bool {
        let concept = Global::Authored(self.syntax().monad.bind.concept.qualifier());

        self.witness(&concept, &WitnessKey::from([head.clone()]))
            .is_some()
    }
}
