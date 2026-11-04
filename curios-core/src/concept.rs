//! The registry entry for a concept — a record-shaped interface — carried on the [`Definitions`](super::Definitions) beside `induct_decls` and `struct_decls`.
//!
//! A concept lowers to a representation-public nominal structure, whose [`StructDecl`] entry drives literals and projections; this entry adds what resolution needs on top: the field labels, what each superclass edge reaches, and the parameter telescope. Resolution itself — witness keys, head keys, and the instance search — is elaboration machinery and lives in `curios-elab`.

use {
    super::{
        Field, Free, Global, Proj, Sharing, StructDecl, Subterm, Telescope, Term, UniverseContext,
    },
    curios_common::Plicity,
};

/// One concept declaration's registry entry.
#[derive(Debug, Clone, PartialEq, Eq)]
#[curios_archive::archived]
pub struct ConceptDecl {
    pub universe_context: UniverseContext,
    /// The declaration's parameter telescope, e.g. `(A : Type)` for `concept Show(A : Type)`. Ends in `()` like a `StructDecl`'s.
    pub params: Telescope<()>,
    /// Field labels in declaration order — the positions witness struct literals fill and method wrappers project.
    pub fields: Vec<String>,
    /// What each superclass edge reaches: the super concept's qualified name, one per `use` field of the concept's field telescope ([`StructDecl::fields`](super::StructDecl::fields)), in field order. Where an edge stands is its field's mark, which the telescope states; what it reaches is read off the field's elaborated type where the concept's fields are elaborated — so an alias of a concept application is an edge — and is empty until then. The graph over all concepts is acyclic, checked as each concept's edges are recorded.
    pub supers: Vec<Global>,
}

impl ConceptDecl {
    /// Each superclass edge as its position among `fields` — the concept's own field telescope — beside the concept it reaches. A concept whose edges are not recorded yet answers with none.
    pub fn edges(&self, fields: &Telescope<()>) -> Vec<(usize, Global)> {
        fields
            .marks()
            .into_iter()
            .enumerate()
            .filter(|(_, mark)| *mark == Plicity::Witness)
            .map(|(position, _)| position)
            .zip(self.supers.iter().copied())
            .collect()
    }

    /// This concept with every term hash-consed against `sharing`. See [`Definitions::shared`](crate::Definitions::shared).
    pub fn shared(&self, sharing: &Sharing) -> Self {
        Self {
            universe_context: self.universe_context.unplaced(),
            params: sharing.share(&self.params),
            fields: self.fields.clone(),
            supers: self.supers.clone(),
        }
    }
}

/// The method at field `field` of the concept `name`, whose entries are `concept` and its `record`, as a call of it fills it, at the concept's own levels: its type, the one group a call fills — the concept's parameters, each hidden, the witness the call is to find, then where the field's type is a function its parameters, under their marks — ending in the field's type or its function's result; and its η-form, the field projected off the witness, applied to those parameters where it has them. `None` where the concept declares no such field, and for a superclass edge, which has no label.
///
/// **The one statement of a method's call.** A method named by its path is its concept's field, so no definition states this: elaboration writes a method as its η-form at an instance of the concept's levels, and a call of it opened at its arguments is the projection off the witness the call fills.
pub fn method_spelling(
    name: Global,
    concept: &ConceptDecl,
    record: &StructDecl,
    field: &str,
) -> Option<(Term, Term)> {
    let index = concept
        .fields
        .iter()
        .position(|label| !label.is_empty() && label == field)?;

    // The record's telescopes are closed, so any binders open them: counted from zero, under the hints the entry keeps.
    let mut hidden = Vec::new();
    let mut parameters = Vec::new();
    let mut cursor = record.arity.cursor();
    while let Some((hint, domain)) = cursor.entry() {
        let binder = Free::local(hidden.len() as u64, hint);
        cursor.advance(Term::free_var(&binder));
        parameters.push(Term::free_var(&binder));
        hidden.push((Plicity::Implicit, binder, domain));
    }
    let witness = Free::local(hidden.len() as u64, None);
    let value = Term::free_var(&witness);
    hidden.push((
        Plicity::Witness,
        witness,
        Term::struct_type_at(
            name,
            record.universe_context.identity_instance(),
            parameters.clone(),
        ),
    ));
    let projected = Field::Struct(name, index);
    let field_type = record
        .fields()
        .field_type_beneath(&parameters, &value, &projected)?;
    let method = Term::from(Subterm::Proj(Proj {
        head: value,
        field: projected,
    }));

    Some(match &*field_type {
        Subterm::FuncType(function) => {
            let mut cursor = function.telescope.cursor();
            let mut own = Vec::with_capacity(function.telescope.len());
            while let (Some((hint, domain)), Some(mark)) = (cursor.entry(), cursor.mark()) {
                let binder = Free::local((hidden.len() + own.len()) as u64, hint);
                cursor.advance(Term::free_var(&binder));
                own.push((mark, binder, domain));
            }
            let output = cursor.body().expect("a cursor past every entry");
            let arguments = own
                .iter()
                .map(|(mark, binder, _)| (*mark, Term::free_var(binder)))
                .collect::<Vec<_>>();
            let group = hidden.into_iter().chain(own).collect::<Vec<_>>();
            (
                Term::func_type_marked(group.clone(), output),
                Term::func_marked(group, Term::apply_marked(method, arguments)),
            )
        }
        _ => (
            Term::func_type_marked(hidden.clone(), field_type),
            Term::func_marked(hidden, method),
        ),
    })
}
