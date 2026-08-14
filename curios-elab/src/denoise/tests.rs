use {
    super::{OperatorTable, denoise_for_display},
    curios_core::{Free, Global, Infix, Subterm, Term, Transient},
    curios_utilities::{InfixOp, Qualifier},
    std::{collections::BTreeMap, rc::Rc},
};

/// Fold `Apply(Proj(witness, 0), [a, b])` through a table registering the witness's field 0 as `op`.
fn fold_projection(op: InfixOp) -> Term {
    let witness = Global::Authored(Qualifier::from(["std", "Nat", "w"]));
    let mut table = OperatorTable::default();
    table.by_witness.insert((witness.clone(), 0), op);

    let call = Term::apply(
        Term::proj(Term::free_var(&Free::Global(witness)), 0),
        [
            Term::free_var(&Free::local(0, Some("a"))),
            Term::free_var(&Free::local(1, Some("b"))),
        ],
    );
    denoise_for_display(&Rc::new(table), &Rc::new(BTreeMap::new()), &call)
}

fn folded_op(term: &Term) -> InfixOp {
    let Subterm::Transient(Transient::Infix(Infix { op, .. })) = &**term else {
        panic!("the projection folds to an infix node");
    };
    *op
}

// `Neq` has its own concept slot, so a `neq` projection keeps the disequality spelling rather than folding to an equality the reader would have to un-negate.
#[test]
fn a_neq_witness_projection_folds_to_neq() {
    assert_eq!(folded_op(&fold_projection(InfixOp::Neq)), InfixOp::Neq);
}

#[test]
fn an_eql_witness_projection_folds_to_eql() {
    assert_eq!(folded_op(&fold_projection(InfixOp::Eql)), InfixOp::Eql);
}
