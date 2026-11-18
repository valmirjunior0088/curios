//! What the certifier states of a declaration beside its terms, by its own reading of them: which binders a function literal or a `let` binds are proofs, which of a family's parameters and of the payloads of the constructors it declares are, whether each name the declaration declares is a proof, and the sort a type-valued definition's value inhabits.
//!
//! **Its own, never elaboration's.** A module arrives with what its elaborator stated of each, and the certifier reads none of it: it states each again, by the reading it judges with, before the closed machine runs on the declaration's terms, so every run reads what this certifier stated ([The closed machine binds a proof unreduced](../../../documentation/design/compilation/the-closed-machine-binds-a-proof-unreduced.md)). The module a walk hands back holds what it stated, and is the one a unit stores, so a walk over a later unit reads a unit in scope by what the walk that certified it stated.
//!
//! **Carried where a walk over a baseline judges nothing.** An item the walk does not judge again — adopted, or never visited — that the elaborator made again arrives saying what the elaborator stated. The baseline holds it to every judgment ([`Filed::holds`](crate::Filed::holds)), so what the walk that certified the baseline stated of it is what this certifier would state: a mark changes no reduct, and a binder's name no judgment. It takes those statements, keeping its own names and places ([`carried_marks`]).
//!
//! **A mark refuses nothing.** A binder's type, or a payload's, that this certifier's reading does not answer within the budget is stated as binding no proof, which costs a reduction where the binder is substituted and changes no reduct. A refused type is the judgment's to refuse, where it is judged.

#[cfg(test)]
mod tests;

use {
    super::Certifier,
    crate::bears_proof,
    curios_core::{
        Bound, Definition, Entrypoint, Free, InductDecl, Item, Nominal, ProofBinders, Telescope,
        Term,
    },
    std::convert::Infallible,
};

/// The marking walk's questions, answered by this certifier's reading under the binders the walk has entered.
struct Asking<'k, 's> {
    certifier: &'k mut Certifier<'s>,
    /// The binders entered, each with its type where the walk gave one, innermost last: what a question about a type naming one reads its type by.
    entered: Vec<(Free, Option<Term>)>,
}

impl ProofBinders for Asking<'_, '_> {
    type Refusal = Infallible;

    fn proves(&mut self, type_: &Term) -> Result<bool, Infallible> {
        let entered = &self.entered;

        Ok(self.certifier.scoped(|certifier| {
            for (binder, type_) in entered {
                if let Some(type_) = type_ {
                    certifier.assume(binder, type_);
                }
            }

            bears_proof(certifier, type_).unwrap_or(false)
        }))
    }

    fn enter(&mut self, hint: Option<&str>, type_: Option<&Term>) -> Free {
        let binder = self.certifier.fresh(hint);
        self.entered.push((binder, type_.cloned()));

        binder
    }

    fn leave(&mut self, count: usize) {
        self.entered.truncate(self.entered.len() - count);
    }
}

/// `term`, a scope's body with `binders[k]` standing for its loose index `k`, with every binder a function literal or a `let` binds in it stating whether it binds a proof.
fn stated_term(certifier: &mut Certifier, term: &Term, binders: &[Term]) -> Term {
    let asked = &mut Asking {
        certifier,
        entered: Vec::new(),
    };
    match term.stating_proofs_under(asked, binders) {
        Ok(term) => term,
        Err(never) => match never {},
    }
}

/// `entry` with every binder its body and its stated type bind stating whether it binds a proof.
pub fn stated_entrypoint(certifier: &mut Certifier, entry: &Entrypoint) -> Entrypoint {
    Entrypoint {
        body: stated_term(certifier, &entry.body, &[]),
        type_: entry
            .type_
            .as_ref()
            .map(|type_| stated_term(certifier, type_, &[])),
    }
}

/// Whether each binder of `telescope` binds a proof, each asked at its type under the binders before it.
fn telescope_proofs<B: Bound>(certifier: &mut Certifier, telescope: &Telescope<B>) -> Vec<bool> {
    certifier.scoped(|certifier| {
        let mut proofs = Vec::new();
        let mut cursor = telescope.cursor();
        while let Some((_, type_)) = cursor.entry() {
            proofs.push(bears_proof(certifier, &type_).unwrap_or(false));
            certifier.advance_assumed(&mut cursor, &type_);
        }

        proofs
    })
}

/// `declaration` with each of its parameters and each payload binder of each case stating whether it binds a proof, each asked at its type under the binders before it.
pub fn stated_entry(certifier: &mut Certifier, declaration: &InductDecl) -> InductDecl {
    let params = telescope_proofs(certifier, declaration.arity());

    stating_parameters(declaration.clone(), &params).with_cases(|case| {
        let proofs = telescope_proofs(certifier, &declaration.signature(case));
        case.with_proofs(&proofs[declaration.param_count()..])
    })
}

/// `declaration` with no parameter and no payload binder stating that it binds a proof: what a family stands as from where the walk registers it to where the declaration that declares it is judged and states its own.
pub fn unstated_entry(declaration: &InductDecl) -> InductDecl {
    stating_parameters(declaration.clone(), &vec![false; declaration.param_count()])
        .with_cases(|case| case.with_proofs(&vec![false; case.proofs().len()]))
}

/// `declaration` with each of its parameters stating whether it binds a proof as `proofs` says: the declaration itself where each states so already.
fn stating_parameters(declaration: InductDecl, proofs: &[bool]) -> InductDecl {
    if declaration.arity().proofs() == proofs {
        return declaration;
    }
    let arity = declaration.arity().clone().with_proofs(proofs);

    declaration.with_arity(arity)
}

/// `item` with every binder its terms bind and every payload of the families it declares stating whether it binds a proof, as this certifier reads each. What it states of each name the item defines is the judgment's ([`check_definition`](super::check_definition), [`check_rec_group`](super::check_rec_group)).
pub fn stated_marks(certifier: &mut Certifier, item: &Item) -> Item {
    match item {
        Item::Let(definition) => Item::Let(Box::new(Definition {
            type_: stated_term(certifier, &definition.type_, &[]),
            body: stated_term(certifier, &definition.body, &[]),
            ..(**definition).clone()
        })),
        Item::Rec(rec) => {
            let members = rec
                .exports()
                .iter()
                .map(|definition| {
                    Term::instance(definition.name, rec.universe_context().identity_instance())
                })
                .collect::<Vec<_>>();
            let group = rec
                .group()
                .map_members(|body| stated_term(certifier, body, &members));

            Item::Rec(rec.with_group(group))
        }
        Item::Nominal(nominals) => {
            Item::Nominal(nominals.clone().with_entries(|_, entry| match entry {
                Nominal::Inductive(declaration) => {
                    Some(Nominal::Inductive(stated_entry(certifier, declaration)))
                }
                Nominal::Struct(_) | Nominal::Concept { .. } => None,
            }))
        }
    }
}

/// `item` stating what this certifier stated of `stated`, the item the baseline holds in its place, which is `item` to every judgment: each mark of its terms, of its families' payloads and of each name it declares is `stated`'s, and every other part of it, its names and places among them, its own.
///
/// # Panics
///
/// Where the two are not of one kind, which an item the baseline holds in another's place always is.
pub fn carried_marks(item: &Item, stated: &Item) -> Item {
    match (item, stated) {
        (Item::Let(definition), Item::Let(from)) => Item::Let(Box::new(Definition {
            proof: from.proof,
            value_sort: from.value_sort.clone(),
            type_: definition.type_.with_proofs_of(&from.type_),
            body: definition.body.with_proofs_of(&from.body),
            ..(**definition).clone()
        })),
        (Item::Rec(rec), Item::Rec(from)) => {
            let mut carried = rec.with_group(rec.group().with_proofs_of(from.group()));
            for (member, from) in carried.exports_mut().iter_mut().zip(from.exports()) {
                member.proof = from.proof;
            }

            Item::Rec(carried)
        }
        (Item::Nominal(nominals), Item::Nominal(from)) => {
            let mut stated = from.entries().iter();
            Item::Nominal(nominals.clone().with_entries(|_, entry| {
                let (_, from) = stated
                    .next()
                    .expect("an item the baseline holds in another's place is of its shape");
                carried_entry(entry, from)
            }))
        }
        _ => panic!("an item the baseline holds in another's place is of its kind"),
    }
}

/// `nominal`, a family, stating of its payloads what `stated`, the family the baseline holds in its place, does: `None` for another kind of entry, which states no mark.
fn carried_entry(nominal: &Nominal, stated: &Nominal) -> Option<Nominal> {
    let (Nominal::Inductive(declaration), Nominal::Inductive(from)) = (nominal, stated) else {
        return None;
    };

    let mut stated = from.signatures();
    let carried =
        stating_parameters(declaration.clone(), &from.arity().proofs()).with_cases(|case| {
            let proofs = stated
                .next()
                .expect("an item the baseline holds in another's place is of its shape")
                .proofs();
            case.with_proofs(&proofs)
        });

    Some(Nominal::Inductive(carried))
}
