use {
    super::{Method, MethodTable, denoise_for_display},
    curios_core::{Apply, Free, Global, Infix, Subterm, Term, Transient},
    curios_utilities::{InfixOp, Qualifier},
    std::{collections::BTreeMap, rc::Rc},
};

/// Fold `Apply(Proj(witness, 0), arguments)` through a table registering the witness's field 0 as `method`.
fn fold_projection(method: Method, arguments: Vec<Term>) -> Term {
    let witness = Global::Authored(Qualifier::from(["std", "Nat", "w"]));
    let mut table = MethodTable::default();
    table.by_witness.insert((witness.clone(), 0), method);

    let call = Term::apply(
        Term::proj(Term::free_var(&Free::Global(witness)), 0),
        arguments,
    );
    denoise_for_display(&Rc::new(table), &Rc::new(BTreeMap::new()), &call)
}

fn operands() -> Vec<Term> {
    vec![
        Term::free_var(&Free::local(0, Some("a"))),
        Term::free_var(&Free::local(1, Some("b"))),
    ]
}

fn operator(op: InfixOp) -> Method {
    Method {
        wrapper: None,
        operator: Some(op),
    }
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
    let folded = fold_projection(operator(InfixOp::Neq), operands());
    assert_eq!(folded_op(&folded), InfixOp::Neq);
}

#[test]
fn an_eql_witness_projection_folds_to_eql() {
    let folded = fold_projection(operator(InfixOp::Eql), operands());
    assert_eq!(folded_op(&folded), InfixOp::Eql);
}

// A method no operator dispatches through reads as the call a program writes, not as the witness's minted name.
#[test]
fn a_method_witness_projection_folds_to_its_wrapper_call() {
    let wrapper = Global::Authored(Qualifier::from(["std", "ops", "Div", "Div", "Ok"]));
    let method = Method {
        wrapper: Some(wrapper.clone()),
        operator: None,
    };
    let folded = fold_projection(method, vec![Term::free_var(&Free::local(0, Some("n")))]);

    let Subterm::Apply(Apply { head, arguments }) = &*folded else {
        panic!("the projection folds to a call");
    };
    assert!(
        matches!(&**head, Subterm::Var(var) if var.as_free() == Some(&Free::Global(wrapper)))
            && arguments.len() == 1,
        "unexpected fold: {folded}"
    );
}
