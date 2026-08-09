//! Display-only folding of elaboration internals back into source-shaped spelling.
//!
//! An infix operator elaborates to a projection of its `/syn` concept witness applied to the operands (`a + b` ≙ `Add/add(a, b)` — see `elaborate_infix`), so a goal report would spell `0 + 0` as `(witness2).0(0, 0)`: an anonymous witness projection no reader should have to decode. The fold reverses exactly that rebuild, recognizing the three forms the call reaches a report in — a still-unsolved witness metavariable, whose `WitnessOrigin` carries the operator symbol; a solved-and-substituted witness global, resolved back to its operator through the witness table and the concept's field roster; and an *abstract* witness, a `use` binder standing for a witness the caller will supply, resolved through the concept its declared type names.
//!
//! The abstract case is the one reduction cannot reach. Where the operand type is concrete the projection reduces to its intrinsic and prints infix without any of this, so the fold only earns its keep under a `use Add(A)` parameter — exactly where the reader has least else to go on.
//!
//! The folded term reintroduces the elaboration-transient `Infix` node (under [`Subterm::Transient`]) purely for observation: it only ever meets the printer, never checking, reduction, or erasure.

#[cfg(test)]
mod tests;

use {
    super::{Context, TermBuilders},
    curios_base::NumOp,
    curios_core::{
        Apply, Bound, Field, Free, Global, Metavar, MetavarOrigin, Proj, StructType, Subterm, Term,
        UniverseInst, Visit,
    },
    std::{collections::BTreeMap, rc::Rc},
};

/// The operator behind a witness projection, keyed both ways a report can arrive at one. Built once and shared across a batch's display calls; the metavariable-origin case needs no entry because the insertion provenance itself names the operator.
#[derive(Debug, Default)]
pub(crate) struct OperatorTable {
    /// (witness name, projected field index) → operator, for every registered witness of a `/syn` operator concept.
    by_witness: BTreeMap<(Global, usize), NumOp>,
    /// (concept name, field index) → operator, for every `/syn` operator concept. Keyed on the concept rather than on any witness of it, because an abstract witness has none: the binder's type names the concept and the projection's index picks the method out of it.
    by_concept: BTreeMap<(Global, usize), NumOp>,
}

pub(crate) type Operators = Rc<OperatorTable>;

/// Declared types of the binders visible at the display site, so an abstract witness resolves to its concept. The goal path supplies the goal's birth telescope, the mismatch path the live local context; a binder missing from the map simply declines to fold.
pub(crate) type BinderTypes = Rc<BTreeMap<Free, Term>>;

pub(crate) fn operator_table(context: &Context) -> Operators {
    let mut table = OperatorTable::default();

    // The concept rows come first and carry the whole operator lookup: a witness of an operator concept answers for the same method at the same index, so the witness rows are a re-keying of these rather than a second scan of the `/syn` roster.
    for (concept_name, concept) in context.concepts() {
        let Global::Authored(concept_path) = concept_name else {
            continue;
        };
        for (index, field) in concept.fields.iter().enumerate() {
            if let Some(op) = context.syntax().operator.operator_for(concept_path, field) {
                table.by_concept.insert((concept_name.clone(), index), op);
            }
        }
    }

    for (concept_name, witness) in context.witness_entries() {
        let Some(concept) = context.concept(concept_name) else {
            continue;
        };
        for index in 0..concept.fields.len() {
            if let Some(op) = table.by_concept.get(&(concept_name.clone(), index)) {
                table.by_witness.insert((witness.name.clone(), index), *op);
            }
        }
    }

    Rc::new(table)
}

/// Fold every operator witness projection in `term` back to its infix spelling. Display-only — see the module documentation. Runs after tolerant materialization, so solved witnesses arrive as globals, unsolved ones as origin-marked metavariables, and abstract ones as binders; all three fold.
pub(crate) fn denoise_for_display(table: &Operators, binders: &BinderTypes, term: &Term) -> Term {
    let captured = Rc::clone(table);
    let scope = Rc::clone(binders);
    let mut visit = Visit::rewriting(
        |_, _| None,
        Box::new(move |_, term| fold(&captured, &scope, term)),
    );
    term.traverse(&mut visit)
}

/// The node-level fold. A substituted node is not descended into, so a folded call denoises its own operands before wrapping them.
fn fold(table: &Operators, binders: &BinderTypes, term: &Term) -> Option<Term> {
    let (op, left, right) = operator_call(table, binders, term)?;
    Some(Term::infix(
        op,
        denoise_for_display(table, binders, left),
        denoise_for_display(table, binders, right),
    ))
}

/// Recognize `Apply(Proj(witness, index), [left, right])` — the exact shape `elaborate_infix` rebuilds — and name its operator. `!=` needs no special case: `Neq` has its own concept slot, so [`OperatorSyntax::operator_for`](curios_base::OperatorSyntax::operator_for) keys the `neq` projection exactly.
fn operator_call<'a>(
    table: &Operators,
    binders: &BinderTypes,
    term: &'a Term,
) -> Option<(NumOp, &'a Term, &'a Term)> {
    let Subterm::Apply(Apply { head, params, .. }) = &**term else {
        return None;
    };
    let [left, right] = params.as_slice() else {
        return None;
    };
    let Subterm::Proj(Proj {
        head: witness,
        field,
    }) = &**head
    else {
        return None;
    };

    // A solved witness reference may carry its occurrence's universe instance; the identity lives in the head.
    let witness = match &**witness {
        Subterm::UniverseInst(UniverseInst { head, .. }) => head,
        _ => witness,
    };

    let op = match &**witness {
        // Unsolved: the insertion provenance rides the metavariable and names the operator directly.
        Subterm::Metavar(Metavar {
            origin: Some(MetavarOrigin::Witness(origin)),
            ..
        }) => NumOp::from_symbol(&origin.func)?,
        Subterm::Var(var) => {
            let Field::Index(index) = field else {
                return None;
            };
            match var.as_free()? {
                // Solved and substituted: the witness global names its operator through the precomputed table.
                Free::Global(global) => *table.by_witness.get(&(global.clone(), *index))?,
                // Abstract: a `use` binder standing for a witness the caller supplies. It has no registered name to key on, so its declared type — a concept application — names the concept instead.
                name @ Free::Local(_) => *table
                    .by_concept
                    .get(&(concept_of(binders.get(name)?)?, *index))?,
            }
        }
        _ => return None,
    };

    Some((op, left, right))
}

/// The concept a witness binder's declared type names, in either spelling a report can reach it in: unreduced — the declaration's global applied to the concept parameters, carrying the occurrence's universe instance — or reduced to the record the concept denotes. A binder's declared type is whatever elaboration assumed it as, and a `use` parameter is never forced on the way to a diagnostic, so the unreduced form is the common one. Anything else is not a witness binder and declines.
fn concept_of(type_: &Term) -> Option<Global> {
    let head = match &**type_ {
        Subterm::Apply(Apply { head, .. }) => head,
        _ => type_,
    };
    let head = match &**head {
        Subterm::UniverseInst(UniverseInst { head, .. }) => head,
        _ => head,
    };
    match &**head {
        Subterm::StructType(StructType { name, .. }) => Some(name.clone()),
        Subterm::Var(var) => match var.as_free()? {
            Free::Global(global) => Some(global.clone()),
            Free::Local(_) => None,
        },
        _ => None,
    }
}
