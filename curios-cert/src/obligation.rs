//! Type totality and proof totality: what the erased half of a program unfolds must be total — a proof reaching, as the checkers reduce it, nothing not known to terminate, a type no recursion of its own declaration that does not descend — decided here from the module alone.
//!
//! Erasure deletes proofs and types. A proof that may not terminate therefore proves anything, and a type that may not terminate reties the negative knot strict positivity exists to forbid — so both halves carry a termination obligation that the halves the machine actually runs do not.
//!
//! # Seeded from the certifier's own typing
//!
//! `curios-elab` seeds these from a hook its elaborator fires at every check site, on the argument that a later pass can only re-derive which terms are propositions incompletely. That argument is about a *syntactic walk*, and it does not reach this crate: the certifier is itself a typechecker, and it types every term in the module. So both positions come from one record kept during its own walk — a term checked against a `Prop`-sorted type is a proof, and a term checked against a sort is a type — and the coverage is exactly the coverage of the walk that produced it.
//!
//! Deciding it here rather than believing the elaborator is the point: taking this obligation on another crate's word would make an elaborator-only analysis the single defense for a whole class of `False`.
//!
//! # What is already in scope
//!
//! A compile judges only the unit's own items, so the classification of what is already in scope arrives rather than being recomputed — as the certifier's own record, filed with each unit by the walk that judged it ([`Certification`](curios_core::Certification)). Trusting it is trusting a verdict this crate already reached about those exact terms, the same structure as the rest of the archive-verdict pattern, and the record answers for every name its unit holds ([`Filed`](crate::Filed)). No [`Definition`](curios_core::Definition) carries elaboration's totality, judged or carried, so every verdict here is this crate's own.

use {
    super::{ByValue, Certifier, Error, Globals, Position, declared_into_prop},
    curios_analysis::{Erased, reached_as_reduced},
    curios_core::{
        Enter, Foreign, FuncType, Global, Item, RecGroup, Reducer, Sharing, Subterm, Term, Totality,
    },
    std::collections::{BTreeMap, BTreeSet, HashMap},
};

/// Whether `group`'s calls, as this walk typed them, close to a descent on every cycle — the verdict `check_group` recorded.
///
/// A group this walk never checked has no verdict and does not descend: its calls were never typed here, and nothing else is a source of them, so the answer for one is the refusing direction.
fn descends(certifier: &Certifier, group: &RecGroup) -> bool {
    certifier.group_verdict(group) == Some(Totality::Total)
}

/// Whether the recursion of the item `group` holds, with its `members`, does not descend: the group does not, or a member encloses a group that does not. Every definition of such an item is `Unfounded`, and unfolds to nothing, whether or not its check refused it.
pub fn group_unfounded(certifier: &Certifier, group: &RecGroup, members: usize) -> bool {
    !descends(certifier, group)
        || (0..members).any(|index| certifier.member_encloses_partial(group, index))
}

/// [`group_unfounded`] of a `let`: it encloses a group that does not descend.
pub fn definition_unfounded(certifier: &Certifier, name: &Global) -> bool {
    certifier.definition_encloses_partial(name)
}

/// Whether a term calls a host row that diverges — partial in itself, with no name to blame.
///
/// Post-order over the term's DAG on the shared [`Term::walk`] driver. The memo is structural and caller-owned, carried across the whole module rather than per walk — definitions share subterms heavily, and a node settled for one is settled for all.
///
/// The other way a term is partial in itself, an inline group that does not descend, is not read here: the walk notes it where it typed the group, against the definition and the positions that enclosed it, because the group is typed with the binders around it opened and a term holds it closed — the two spellings meet in no lookup.
fn calls_a_diverging_row(term: &Term, memo: &mut HashMap<Term, bool>) -> bool {
    term.walk(
        memo,
        |memo, term| match memo.get(term) {
            Some(&diverges) => Enter::Skip(diverges),
            None => Enter::Descend,
        },
        |memo, term, mut children| {
            let diverges = matches!(&**term, Subterm::Foreign(Foreign { function, .. }) if function.diverges())
                || children.any(|child| child);
            memo.insert(term.clone(), diverges);
            diverges
        },
    )
}

/// The verdicts on the definitions not known to terminate, as a walk comes to know them: the environment's, then each judged item's as it is classified.
///
/// What an environment already answers for is read rather than recomputed: a name of a unit in scope is classified as the certifier's record of that unit classifies it, asked where a judgment reaches the name, and of a module's items only the judged ones are classified — the walk's own answer to which are new, the rest being the environment's own. That is what keeps a compile from re-analyzing the standard library, and from copying its classification.
///
/// **Reading the environment is load-bearing rather than an optimization.** A definition is partial where anything it *mentions* is, and the already-judged items are not carried inside the module, so nothing else in a walk knows that a partial definition, a parser of `/std/Json/decode`, is partial. A user proof reaching it would then be read against a set the name is absent from and pass as total, which is exactly the identification type totality and proof totality exist to prevent.
pub struct Totals<'g> {
    /// The units in scope, whose records answer for their own definitions.
    scope: &'g Globals<'g>,
    /// What this walk classified short of `Total`: the items it judged and the verdicts it adopted.
    classified: BTreeMap<Global, Totality>,
    /// Which terms call a diverging host row, carried across every item the walk classifies: definitions share subterms heavily, and a node settled for one is settled for all.
    memo: HashMap<Term, bool>,
}

impl<'g> Totals<'g> {
    /// What `globals` holds classified, read where it stands, every unit's record answering for each name it holds, and nothing this walk has classified yet.
    pub fn seeded(globals: &'g Globals<'g>) -> Self {
        Self {
            scope: globals,
            classified: BTreeMap::new(),
            memo: HashMap::new(),
        }
    }

    /// The verdict on `name`, as far as the walk has come: its own classification, the baseline's record where `certifier` reads `name` as a declaration it does not visit, or the record of the unit in scope that defines it; `Total` where none of them classifies it short of that.
    pub fn totality(&self, certifier: &Certifier, name: &Global) -> Totality {
        self.classified
            .get(name)
            .copied()
            .or_else(|| certifier.passed_totality(name))
            .or_else(|| self.scope.totality(name))
            .unwrap_or(Totality::Total)
    }

    /// Whether `name` is not known to terminate, as far as the walk has come ([`Totals::totality`]).
    pub fn partial(&self, certifier: &Certifier, name: &Global) -> bool {
        !self.totality(certifier, name).is_total()
    }

    /// Take `totality` for `name` on the word of an earlier walk's record: a definition whose verdict this walk adopts.
    pub fn adopt(&mut self, name: Global, totality: Totality) {
        if !totality.is_total() {
            self.classified.insert(name, totality);
        }
    }

    /// Classify a judged `item`.
    ///
    /// Asked as the item's check ends, so what its check enclosed was noted as it went, and after every definition it mentions outside itself was classified: the walk judges an item after the items it reaches, and what a judged item mentions of the environment its unit's record answers for. So one pass over the item settles it, its members closed over one another where it is a group.
    pub fn classify(&mut self, certifier: &Certifier, item: &Item) {
        let mut mentions = BTreeMap::new();
        self.locally(certifier, item, &mut mentions);
        self.close(certifier, &mentions);
    }

    /// Classify each definition of `item`, which this walk checked, that is partial on its own account, and record what each mentions: what its check enclosed was noted as it went.
    ///
    /// The item's recursion is one: where its group does not descend, or a definition it holds encloses a group that does not, every definition it holds is `Unfounded`, a sibling reaching the loop through the group's own spelling as readily as by its name. A definition that calls a host row that diverges is `Partial`, the row being no recursion a checker could unfold.
    fn locally(
        &mut self,
        certifier: &Certifier,
        item: &Item,
        mentions: &mut BTreeMap<Global, BTreeSet<Global>>,
    ) {
        let definitions = item.definitions();
        let unfounded = match item {
            Item::Rec(rec) => group_unfounded(certifier, rec.group(), definitions.len()),
            Item::Let(definition) => definition_unfounded(certifier, &definition.name),
            Item::Nominal(_) => false,
        };

        let own = item.declared_names();
        for definition in definitions {
            if unfounded {
                self.classified.insert(definition.name, Totality::Unfounded);
            } else if calls_a_diverging_row(&definition.body, &mut self.memo)
                || calls_a_diverging_row(&definition.type_, &mut self.memo)
            {
                self.classified.insert(definition.name, Totality::Partial);
            }
            let mentioned = definition.mentions();
            for name in mentioned.iter().filter(|name| !own.contains(name)) {
                certifier.read_verdict(name);
            }
            mentions.insert(definition.name, mentioned);
        }
    }

    /// Close the verdicts over `mentions`: a definition that mentions a partial one is `Partial`, iterated until nothing joins.
    fn close(&mut self, certifier: &Certifier, mentions: &BTreeMap<Global, BTreeSet<Global>>) {
        loop {
            let mut changed = false;
            for (name, reached) in mentions {
                if self.classified.contains_key(name) {
                    continue;
                }
                if reached.iter().any(|other| self.partial(certifier, other)) {
                    self.classified.insert(*name, Totality::Partial);
                    changed = true;
                }
            }
            if !changed {
                break;
            }
        }
    }
}

/// What [`calls_a_diverging_row`] found of each node of the positions a walk records, carried across the whole walk and keyed by the positions' canonical nodes: a position's term is recorded where it was typed, built there, so a type recorded at every level of a nesting is a copy at each, and two copies are compared node by node at every probe that finds one.
#[derive(Default)]
pub struct PositionVerdicts {
    keys: Sharing,
    verdicts: HashMap<Term, bool>,
}

/// Type totality and proof totality over the positions one item's check recorded: each must reach nothing its obligation holds partial, and must not be partial in itself — enclose no group that does not descend, as the walk noted where it typed one, and call no host row that diverges.
///
/// A proof reaches the definitions not known to terminate that it reaches as the checkers reduce it ([`reached_as_reduced`]), read so only where it names one, and a type only `own`'s, the names of the item whose check recorded it, where one does not descend: a finished declaration whose recursion does not descend unfolds to nothing ([`define_as_stated`](crate::define_as_stated)), so a type naming it reaches nothing that loops.
pub fn check_positions(
    certifier: &mut Certifier,
    positions: &[Position],
    totals: &Totals<'_>,
    own: &[Global],
    memo: &mut PositionVerdicts,
) -> Result<(), Error> {
    curios_tracing::profile!("check_positions");
    let partial = |certifier: &Certifier, name: &Global| totals.partial(certifier, name);
    for position in positions {
        let written = position
            .term
            .first_declaration(|name| match position.erased {
                Erased::Type => {
                    own.contains(name) && totals.totality(certifier, name).is_unfounded()
                }
                Erased::Proof => partial(certifier, name),
            });
        let partial_in_itself = position.encloses_partial
            || calls_a_diverging_row(&memo.keys.share(&position.term), &mut memo.verdicts);
        // A proof is read as the checkers reduce it, where it names a definition not known to terminate, and by the first it names where it is partial in itself, a group it encloses that does not descend unfolding without end, or where the budget is spent reading it.
        let reached = match (position.erased, written) {
            (Erased::Proof, Some(written)) if !partial_in_itself => {
                reached_as_reduced(certifier, &position.term, &partial).unwrap_or(Some(written))
            }
            (_, written) => written,
        };
        if let Some(reached) = reached {
            return Err(Error::NotTotal {
                erased: position.erased,
                reached: Some(reached),
            });
        }
        if partial_in_itself {
            return Err(Error::NotTotal {
                erased: position.erased,
                reached: None,
            });
        }
    }

    Ok(())
}

impl ByValue<'_, '_> {
    /// Whether a value of `type_` is a proof: `type_` is a proposition, or a function that ultimately answers one. It is what this certifier states a binder binds where it marks one ([`curios_core::Scope::binds_proof`]), read of the binder's type by its own rules, and never what a module arriving says of it (`certifier::stating`). A sort is neither, so a binder at a type bears no mark: conversion reads a type's reduct, and a type is evaluated before it is bound.
    pub(crate) fn bears_proof(&mut self, type_: &Term) -> Result<bool, Error> {
        if matches!(self.erased_half(type_)?, Some(Erased::Proof)) {
            return Ok(true);
        }
        let reduced = self.reduce_forced(type_.clone())?;
        let Subterm::FuncType(FuncType { telescope }) = &*reduced else {
            return Ok(false);
        };

        self.scoped(|certifier| {
            let mut cursor = telescope.cursor();
            while let Some((_, domain)) = cursor.entry() {
                certifier.advance_assumed(&mut cursor, &domain);
            }
            let codomain = cursor.body().expect("a cursor past every entry");

            certifier.bears_proof(&codomain)
        })
    }

    /// The erased half a term judged at `type_` belongs to, or `None` when the type is relevant and the obligations have nothing to say about it.
    ///
    /// Decided where the position is recorded, while the binders its type mentions are still assumed. A failure propagates rather than reading as "unconstrained": everywhere else in this crate an exhausted budget refuses the item, and this is not the place to make a resource limit read as a pass.
    pub(crate) fn erased_half(&mut self, type_: &Term) -> Result<Option<Erased>, Error> {
        // A proposition known by its head is recorded with nothing run ([`declared_into_prop`]); no proposition is a sort, so the test below has nothing to say about it.
        if declared_into_prop(self, type_)?.is_some() {
            return Ok(Some(Erased::Proof));
        }

        let reduced = self.reduce_forced(type_.clone())?;
        // A term at a sort is a type, and erasure deletes it wholesale. This is the one question the structural test answers — what the *runtime* observes — and it is not the question [`carries_information`](crate::carries_information) asks, which is what *conversion* observes and where a type counts in full. Reading the two as one predicate would certify a closed inhabitant of `False`.
        if matches!(&*reduced, Subterm::Sort(_)) {
            return Ok(Some(Erased::Type));
        }

        Ok(self.sort_of(type_)?.is_prop().then_some(Erased::Proof))
    }
}
