//! Display-only folding of elaboration internals back into source-shaped spelling.
//!
//! An infix operator elaborates to a projection of its `/syn` concept witness applied to the operands (`a + b` ≙ `Add/add(a, b)` — see `elaborate_infix`), so a goal report would spell `0 + 0` as `(witness2).0(0, 0)`: an anonymous witness projection no reader should have to decode. The fold reverses exactly that rebuild, recognizing both forms the call reaches a report in — a still-unsolved witness metavariable, whose `WitnessOrigin` carries the operator symbol, and a solved-and-substituted witness global, resolved back to its operator through the witness table and the concept's field roster.
//!
//! The folded term reintroduces the elaboration-transient [`Subterm::Infix`] node purely for observation: it only ever meets the printer, never checking, reduction, or erasure.

use {
    super::{Context, TermBuilders},
    curios_base::NumOp,
    curios_core::{
        Apply, Bound, Field, Free, Global, Metavar, MetavarOrigin, Prim, Proj, Subterm, Term,
        UniverseInst, Visit,
    },
    std::{collections::BTreeMap, rc::Rc},
};

/// (witness name, projected field index) → operator, for every registered witness of a `/syn` operator concept. Built once and shared across a batch's display calls; the metavariable-origin case needs no table because the insertion provenance itself names the operator.
pub(crate) type OperatorWitnesses = Rc<BTreeMap<(Global, usize), NumOp>>;

pub(crate) fn operator_witness_table(context: &Context) -> OperatorWitnesses {
    let mut table = BTreeMap::new();
    for (concept_name, witness) in context.witness_entries() {
        let Global::Authored(concept_path) = concept_name else {
            continue;
        };
        let Some(concept) = context.concept(concept_name) else {
            continue;
        };
        for (index, field) in concept.fields.iter().enumerate() {
            if let Some(op) = NumOp::from_concept_field(concept_path, field) {
                table.insert((witness.name.clone(), index), op);
            }
        }
    }
    Rc::new(table)
}

/// Fold every operator witness projection in `term` back to its infix spelling. Display-only — see the module documentation. Runs after tolerant materialization, so solved witnesses arrive as globals and unsolved ones as origin-marked metavariables; both fold.
pub(crate) fn denoise_for_display(table: &OperatorWitnesses, term: &Term) -> Term {
    let captured = Rc::clone(table);
    let mut visit = Visit::rewriting(|_, _| None, Box::new(move |_, term| fold(&captured, term)));
    term.traverse(&mut visit)
}

/// The node-level fold. A substituted node is not descended into, so a folded call denoises its own operands before wrapping them.
fn fold(table: &OperatorWitnesses, term: &Term) -> Option<Term> {
    // `a != b` elaborates as `BoolXor(<Eql call>, true)` — no `BoolNot` prim exists (`elaborate_infix`). Match the wrapper before the bare call so the pair folds to `!=` rather than to a stray xor around `==`.
    if let Subterm::Prim(Prim::BoolXor(call, negate)) = &**term
        && matches!(&**negate, Subterm::Prim(Prim::Bool(true)))
        && let Some((op, left, right)) = operator_call(table, call)
    {
        let op = match op {
            NumOp::Eql => NumOp::Neq,
            op => op,
        };
        return Some(Term::infix(
            op,
            denoise_for_display(table, left),
            denoise_for_display(table, right),
        ));
    }

    let (op, left, right) = operator_call(table, term)?;
    Some(Term::infix(
        op,
        denoise_for_display(table, left),
        denoise_for_display(table, right),
    ))
}

/// Recognize `Apply(Proj(witness, index), [left, right])` — the exact shape `elaborate_infix` rebuilds — and name its operator. The `Neq` spelling normalizes to `Eql` here (the two share a concept entry, and the bare call *is* the equality); the xor wrapper above restores `!=`.
fn operator_call<'a>(
    table: &OperatorWitnesses,
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
        // Solved and substituted: the witness global names its operator through the precomputed table.
        Subterm::Var(var) => {
            let Some(Free::Global(global)) = var.as_free() else {
                return None;
            };
            let Field::Index(index) = field else {
                return None;
            };
            *table.get(&(global.clone(), *index))?
        }
        _ => return None,
    };

    let op = match op {
        NumOp::Neq => NumOp::Eql,
        op => op,
    };
    Some((op, left, right))
}
