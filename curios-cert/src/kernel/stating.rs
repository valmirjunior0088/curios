//! What the kernel states of a declaration beside its terms, by its own reading of them: which binders a function literal or a `let` binds are proofs, which payloads of the constructors a family declares are, whether each name the declaration declares is a proof, and the sort a type-valued definition's value inhabits.
//!
//! **Its own, never elaboration's.** A module arrives with what its elaborator stated of each, and the kernel reads none of it: it states each again, by the reading it judges with, before the closed machine runs on the declaration's terms, so every run reads what this kernel stated ([The closed machine binds a proof unreduced](../../../documentation/design/compilation/the-closed-machine-binds-a-proof-unreduced.md)). The module a walk hands back holds what it stated, and is the one a unit stores, so a walk over a later unit reads a unit in scope by what the walk that certified it stated.
//!
//! **Carried where a walk over a baseline judges nothing.** An item the walk does not judge again — adopted, or never visited — that the elaborator made again arrives saying what the elaborator stated. The baseline holds it to every judgment ([`Baseline::holds`](crate::Baseline::holds)), so what the walk that certified the baseline stated of it is what this kernel would state: a mark changes no reduct, and a binder's name no judgment. It takes those statements, keeping its own names and places ([`carried_marks`]).
//!
//! **A mark refuses nothing.** A binder's type, or a payload's, that this kernel's reading does not answer within the budget is stated as binding no proof, which costs a reduction where the binder is substituted and changes no reduct. A refused type is the judgment's to refuse, where it is judged.

#[cfg(test)]
mod tests;

use {
    super::Kernel,
    crate::bears_proof,
    curios_core::{
        Definition, DefinitionKind, Entrypoint, Free, InductDecl, Item, Nominal, ProofBinders,
        RecDefinition, RecItem, Term,
    },
    std::{convert::Infallible, rc::Rc},
};

/// The marking walk's questions, answered by this kernel's reading under the binders the walk has entered.
struct Asking<'k, 's> {
    kernel: &'k mut Kernel<'s>,
    /// The binders entered, each with its type where the walk gave one, innermost last: what a question about a type naming one reads its type by.
    entered: Vec<(Free, Option<Term>)>,
}

impl ProofBinders for Asking<'_, '_> {
    type Refusal = Infallible;

    fn proves(&mut self, type_: &Term) -> Result<bool, Infallible> {
        let entered = &self.entered;

        Ok(self.kernel.scoped(|kernel| {
            for (binder, type_) in entered {
                if let Some(type_) = type_ {
                    kernel.assume(binder, type_);
                }
            }

            bears_proof(kernel, type_).unwrap_or(false)
        }))
    }

    fn enter(&mut self, hint: Option<&str>, type_: Option<&Term>) -> Free {
        let binder = self.kernel.fresh(hint);
        self.entered.push((binder, type_.cloned()));

        binder
    }

    fn leave(&mut self, count: usize) {
        self.entered.truncate(self.entered.len() - count);
    }
}

/// `term`, a scope's body with `binders[k]` standing for its loose index `k`, with every binder a function literal or a `let` binds in it stating whether it binds a proof.
fn stated_term(kernel: &mut Kernel, term: &Term, binders: &[Term]) -> Term {
    let asked = &mut Asking {
        kernel,
        entered: Vec::new(),
    };
    match term.stating_proofs_under(asked, binders) {
        Ok(term) => term,
        Err(never) => match never {},
    }
}

/// `entry` with every binder its body and its stated type bind stating whether it binds a proof.
pub(crate) fn stated_entrypoint(kernel: &mut Kernel, entry: &Entrypoint) -> Entrypoint {
    Entrypoint {
        body: stated_term(kernel, &entry.body, &[]),
        type_: entry
            .type_
            .as_ref()
            .map(|type_| stated_term(kernel, type_, &[])),
    }
}

/// `declaration` with each binder of each constructor's signature stating whether it binds a proof, each asked at its type under the binders before it.
pub(crate) fn stated_entry(kernel: &mut Kernel, declaration: &InductDecl) -> InductDecl {
    let mut stated = declaration.clone();
    for constructor in stated.signatures_mut() {
        let proofs = kernel.scoped(|kernel| {
            let mut proofs = Vec::new();
            let mut cursor = constructor.telescope.cursor();
            while let Some((_, type_)) = cursor.entry() {
                proofs.push(bears_proof(kernel, &type_).unwrap_or(false));
                kernel.advance_assumed(&mut cursor, &type_);
            }

            proofs
        });
        if proofs != constructor.telescope.proofs() {
            constructor.telescope = constructor.telescope.clone().with_proofs(&proofs);
        }
    }

    stated
}

/// `declaration` with no binder of any constructor's signature stating that it binds a proof: what a family stands as from where the walk registers it to where the declaration that declares it is judged and states its own.
pub(crate) fn unstated_entry(declaration: &InductDecl) -> InductDecl {
    let mut unstated = declaration.clone();
    for constructor in unstated.signatures_mut() {
        let none = vec![false; constructor.telescope.proofs().len()];
        constructor.telescope = constructor.telescope.clone().with_proofs(&none);
    }

    unstated
}

/// `kind`, with the family it carries stating its payloads' marks where it carries one.
fn stated_kind(kernel: &mut Kernel, kind: &DefinitionKind) -> DefinitionKind {
    match kind {
        DefinitionKind::Nominal(nominal) => match &**nominal {
            Nominal::Inductive(declaration) => DefinitionKind::Nominal(Rc::new(
                Nominal::Inductive(stated_entry(kernel, declaration)),
            )),
            Nominal::Struct(_) | Nominal::Concept { .. } => kind.clone(),
        },
        _ => kind.clone(),
    }
}

/// `item` with every binder its terms bind and every payload of the families it declares stating whether it binds a proof, as this kernel reads each. What it states of each name the item declares is the judgment's ([`check_definition`](super::check_definition), [`check_rec_group`](super::check_rec_group)).
pub(crate) fn stated_marks(kernel: &mut Kernel, item: &Item) -> Item {
    match item {
        Item::Let(definition) => Item::Let(Box::new(Definition {
            kind: stated_kind(kernel, &definition.kind),
            type_: stated_term(kernel, &definition.type_, &[]),
            body: stated_term(kernel, &definition.body, &[]),
            ..(**definition).clone()
        })),
        Item::Rec(rec) => {
            let members = rec
                .definitions
                .iter()
                .map(|definition| {
                    Term::instance(definition.name, rec.universe_context.identity_instance())
                })
                .collect::<Vec<_>>();
            let group = rec
                .group
                .map_members(|body| stated_term(kernel, body, &members));
            let mut definitions = rec.definitions.clone();
            for definition in &mut definitions {
                definition.kind = stated_kind(kernel, &definition.kind);
            }

            Item::Rec(RecItem {
                definitions,
                group,
                universe_context: rec.universe_context.clone(),
            })
        }
    }
}

/// `item` stating what this kernel stated of `stated`, the item the baseline holds in its place, which is `item` to every judgment: each mark of its terms, of its families' payloads and of each name it declares is `stated`'s, and every other part of it, its names and places among them, its own.
///
/// # Panics
///
/// Where the two are not of one kind, which an item the baseline holds in another's place always is.
pub(crate) fn carried_marks(item: &Item, stated: &Item) -> Item {
    match (item, stated) {
        (Item::Let(definition), Item::Let(from)) => Item::Let(Box::new(Definition {
            kind: carried_kind(&definition.kind, &from.kind),
            proof: from.proof,
            value_sort: from.value_sort.clone(),
            type_: definition.type_.with_proofs_of(&from.type_),
            body: definition.body.with_proofs_of(&from.body),
            ..(**definition).clone()
        })),
        (Item::Rec(rec), Item::Rec(from)) => Item::Rec(RecItem {
            definitions: rec
                .definitions
                .iter()
                .zip(&from.definitions)
                .map(|(member, from)| RecDefinition {
                    kind: carried_kind(&member.kind, &from.kind),
                    proof: from.proof,
                    ..member.clone()
                })
                .collect(),
            group: rec.group.with_proofs_of(&from.group),
            universe_context: rec.universe_context.clone(),
        }),
        _ => panic!("an item the baseline holds in another's place is of its kind"),
    }
}

/// `kind`, with the family it carries stating of its payloads what the one `stated` carries does.
fn carried_kind(kind: &DefinitionKind, stated: &DefinitionKind) -> DefinitionKind {
    let (DefinitionKind::Nominal(nominal), DefinitionKind::Nominal(from)) = (kind, stated) else {
        return kind.clone();
    };
    let (Nominal::Inductive(declaration), Nominal::Inductive(from)) = (&**nominal, &**from) else {
        return kind.clone();
    };

    let mut carried = declaration.clone();
    for (constructor, from) in carried.signatures_mut().zip(from.signatures()) {
        let proofs = from.telescope.proofs();
        if proofs != constructor.telescope.proofs() {
            constructor.telescope = constructor.telescope.clone().with_proofs(&proofs);
        }
    }

    DefinitionKind::Nominal(Rc::new(Nominal::Inductive(carried)))
}
