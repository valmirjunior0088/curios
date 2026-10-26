//! What exists because a unit is being elaborated.
//!
//! Elaboration turns what an author wrote into finished terms, and while it does it holds what no finished term has: the metavariables an attempt at a declaration mints and the levels it has yet to settle, which of the unit's declarations have not finished and what the attempt read of them, the witnesses the unit declares before they elaborate, the module a use site is judged from, and what it notes on the way for a lint, an obligation or a report. [`Underway`] is all of it. A context that only reads finished terms holds one that is empty, and no judgment it makes finds anything here to read.

use {
    super::{Attempt, CheckedTerms, Credits, DeclaredWitnesses, EmbeddingSite, Needs, Posture},
    curios_core::{Declarations, Extent, Global, Imports, Minted},
    std::{collections::BTreeSet, rc::Rc},
};

/// The half of a context that exists because a unit is being elaborated.
#[derive(Debug)]
pub struct Underway {
    // What the attempt under way mints and solves — its metavariables with their solutions, its universe levels with their solver, what it noted of its goals; flat and frame-independent. See [`Attempt`].
    pub attempt: Attempt,
    // Which of the unit's declarations have not finished, and what the attempt under way read of them; see [`Needs`].
    pub needs: Needs,
    // The witnesses the unit declares, as they are known before they elaborate; see [`DeclaredWitnesses`].
    pub witnesses: DeclaredWitnesses,
    // The written binders the proofs the elaborator wrote read, which `unused-binder` does not report; see [`Credits`].
    pub credits: Credits,
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
    // What the unit's `use` declarations brought into scope, where, and under which spelling — the text stage's table, installed by the driver before elaboration. Read by goal suggestions alone, as the pool a candidate may come from beyond the names the program already mentions. Empty means nothing was imported, which is also what every embedding that never installs one gets: the pools then stop at the referenced globals.
    pub imports: Imports,
}

impl Default for Underway {
    /// The elaboration of nothing: no unit begun, no attempt made, standing at the root module with whatever is in scope on offer.
    fn default() -> Self {
        Self {
            attempt: Attempt::default(),
            needs: Needs::default(),
            witnesses: DeclaredWitnesses::default(),
            credits: Credits::default(),
            checked: CheckedTerms::default(),
            posture: Posture::default(),
            declarations: Declarations::default(),
            offering: true,
            minted: Rc::default(),
            broken: BTreeSet::new(),
            declaration: None,
            defining: false,
            embedding_sites: Vec::new(),
            imports: Imports::default(),
        }
    }
}
