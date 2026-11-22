//! What exists because a unit is being elaborated.
//!
//! Elaboration turns what an author wrote into finished terms, and while it does it holds what no finished term has: the metavariables an attempt at a declaration mints and the levels it has yet to settle, which of the unit's declarations have not finished and what the attempt read of them, the witnesses the unit declares before they elaborate, the module a use site is judged from, and what it notes on the way for a lint, an obligation or a report. [`Underway`] is all of it. A context that only reads finished terms holds one that is empty, and no judgment it makes finds anything here to read.

use {
    super::{
        Attempt, CheckedTerms, Credits, DeclaredWitnesses, EmbeddingSite, Needs, Posture, Sites,
    },
    crate::{Resolved, SettledPlace, Traces},
    curios_common::Qualifier,
    curios_core::{Declarations, Definitions, Extent, Free, Global, Imports, Lint, Minted},
    std::{collections::BTreeSet, fmt, rc::Rc},
};

/// Whether the unit's reader, standing in a module and inside a declaration of it (the entry's final term where `None`), writes a declaration by some name: the driver's answer, which holds the tables resolution reads a name by ([`Context::set_reader`](super::Context::set_reader)).
pub type ReaderWrites<'s> = Box<dyn Fn(&Qualifier, Option<&Global>, &Global) -> bool + 's>;

/// What the unit's reader writes, where a driver handed it in ([`ReaderWrites`]).
#[derive(Default)]
pub struct Reader<'s>(pub Option<ReaderWrites<'s>>);

impl fmt::Debug for Reader<'_> {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.debug_struct("Reader").finish_non_exhaustive()
    }
}

/// How far a unit's walk over its declarations has come: `done` of the `of` declarations it holds are finished, and `on` is the one it is asking for, where it is asking for one.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Walked<'a> {
    pub done: usize,
    pub of: usize,
    pub on: Option<&'a Global>,
}

/// Whoever is told how far the walk has come ([`Walked`]), installed by a driver that reports it: told as each declaration is asked for and as each finishes.
pub type WalkListener<'s> = Box<dyn FnMut(Walked<'_>) + 's>;

/// Whoever the walk tells how far it has come, where a driver installed one ([`WalkListener`]).
#[derive(Default)]
pub struct Listening<'s>(pub Option<WalkListener<'s>>);

impl fmt::Debug for Listening<'_> {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.debug_struct("Listening").finish_non_exhaustive()
    }
}

/// The half of a context that exists because a unit is being elaborated.
#[derive(Debug)]
pub struct Underway<'s> {
    // What the attempt under way mints and solves — its metavariables with their solutions, its universe levels with their solver, what it noted of its goals; flat and frame-independent. See [`Attempt`].
    pub attempt: Attempt,
    // Which of the unit's declarations have not finished, and what the attempt under way read of them; see [`Needs`].
    pub needs: Needs,
    // The witnesses the unit declares, as they are known before they elaborate; see [`DeclaredWitnesses`].
    pub witnesses: DeclaredWitnesses,
    // The written binders the proofs the elaborator wrote read, which `unused-binder` does not report; see [`Credits`].
    pub credits: Credits,
    // The written sites opened and not yet classified, each with the type it was opened at; see [`Sites`].
    pub sites: Sites,
    /// The binder lints of every declaration the walk's traces hold — elaborated now, adopted from a cabinet or kept from a baseline — taken where the walk ends (`Context::hold_lints`), whatever it concluded: what `Context::observed` hands out beside the final term's.
    pub held: Vec<Lint>,
    /// The type each written place settled at, of the items published since the declaration they belong to was last filed ([`SettledPlace`]): taken into its record where it is filed (`Context::take_settled`).
    pub settled: Vec<SettledPlace>,
    /// What each word written in the declaration under way that elaboration resolved names, where it is written ([`Resolved`]): taken into its record where it is filed (`Context::take_resolved`), and taken back with what an item that is refused or void wrote (`ItemMark`).
    pub resolved: Vec<(Extent, Resolved)>,
    /// The declarations that elaborated, where the walk refused, with the record of each, taken where it ends (`Context::hold_elaborated`): what `Context::observed` hands out beside the lints.
    pub elaborated: Option<(Definitions, Traces)>,
    // Every term elaboration settled, with the type it settled at — the seed of the erasure obligations; see [`CheckedTerms`].
    pub checked: CheckedTerms,
    // Where surface elaboration stands: the module whose item is being elaborated, and whether the prover is running; see [`Posture`].
    pub posture: Posture,
    /// The declarations the unit's items make, as the lowering hands them on, by which each name the unit declares is placed among its items — every item's, the ones a recompile keeps among them: which names are the unit's own (`Context::proof_may_apply`), and where a goal's report is placed (`Context::written_at`).
    pub declarations: Declarations,
    /// Whether the lemmas the unit being elaborated declares are on offer to the proofs the elaborator writes: not while its walk elaborates the prover's vocabulary, and from then on (`Context::offer_lemmas`). A context no unit was begun in offers whatever is in scope.
    pub offering: bool,
    /// What the unit's lowering minted, declaration by declaration: what each declaration's state starts above (`Context::begin_state`).
    pub minted: Rc<Minted>,
    /// The names whose declarations the parser could not read, handed in before elaboration so their dependents are withheld from the start. Empty until a lowering reports them.
    pub broken: BTreeSet<Global>,
    /// The declaration being elaborated (`Context::enter_declaration`); `None` for an entry's final term.
    pub declaration: Option<Global>,
    /// Whether the value being elaborated is a definition's its author wrote at the top level (`Context::enter_value`): a region of its own, which a `!` refused there names.
    pub defining: bool,
    /// What each auto-lift site named as written, under the extent its `lift` wrapper took — see [`EmbeddingSite`]. One entry per such site the unit elaborates, read only where a missing embedding is reported, so a scan serves.
    pub embedding_sites: Vec<(Extent, EmbeddingSite)>,
    /// The members of the group being elaborated, each beside the local its group names it by while it elaborates ([`Context::within_group`](super::Context::within_group)). Empty outside one.
    pub group: Vec<(Global, Free)>,
    // What the unit's `use` declarations brought into scope, where, and under which spelling — the text stage's table, installed by the driver before elaboration. Read by goal suggestions alone, as the pool a candidate may come from beyond the names the program already mentions. Empty means nothing was imported, which is also what every embedding that never installs one gets: the pools then stop at the referenced globals.
    pub imports: Imports,
    /// What the unit's reader writes, the driver's answer installed before elaboration ([`Reader`]). Asked where a report's side is reduced for display alone, of a declaration `/sys` writes an operation by.
    pub reader: Reader<'s>,
    /// Whoever is told how far the walk over the unit's declarations has come, where a driver reports it ([`Listening`]).
    pub listening: Listening<'s>,
}

impl Default for Underway<'_> {
    /// The elaboration of nothing: no unit begun, no attempt made, standing at the root module with whatever is in scope on offer.
    fn default() -> Self {
        Self {
            attempt: Attempt::default(),
            needs: Needs::default(),
            witnesses: DeclaredWitnesses::default(),
            credits: Credits::default(),
            sites: Sites::default(),
            held: Vec::new(),
            settled: Vec::new(),
            resolved: Vec::new(),
            elaborated: None,
            checked: CheckedTerms::default(),
            posture: Posture::default(),
            declarations: Declarations::default(),
            offering: true,
            minted: Rc::default(),
            broken: BTreeSet::new(),
            declaration: None,
            defining: false,
            embedding_sites: Vec::new(),
            group: Vec::new(),
            imports: Imports::default(),
            reader: Reader::default(),
            listening: Listening::default(),
        }
    }
}
