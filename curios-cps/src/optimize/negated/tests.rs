//! A comparison's negation folded into the comparison that negates it, and what is left as it stands.

use {
    super::*,
    crate::{Droppable, Edge, Function, NodeId},
};

/// `main(x, y)` computing `op(x, y)`, then `NatXor` of its answer with `operand`, which it returns: the module, the negation's node, and the two parameters.
fn negated(op: Intrinsic, operand: Literal) -> (Module, NodeId, ValueId, ValueId) {
    let mut module = Module::new();
    let entry = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let x = module.add_value(Some("x".into()));
    let y = module.add_value(Some("y".into()));
    let compared = module.add_value(Some("compared".into()));
    let flipped = module.add_value(Some("flipped".into()));
    let return_node = module.add_node(Node::ApplyCont(Edge {
        target: return_cont,
        args: vec![Atom::Value(flipped)],
    }));
    let negation = module.add_node(Node::LetIntrinsic {
        result: flipped,
        op: Intrinsic::NatXor,
        args: vec![Atom::Value(compared), Atom::Literal(operand)],
        next: return_node,
    });
    let comparison = module.add_node(Node::LetIntrinsic {
        result: compared,
        op,
        args: vec![Atom::Value(x), Atom::Value(y)],
        next: negation,
    });
    module.define_function(
        entry,
        Function {
            lineage: None,
            debug_name: Some("main".into()),
            params: vec![x, y],
            return_cont,
            body: comparison,
            droppable: Droppable::Never,
            terminates: false,
        },
    );
    module.set_entry(entry);
    (module, negation, x, y)
}

fn one() -> Literal {
    Literal::Nat(Natural::from(1u32))
}

/// Each comparison on a total order folds with its negation into the comparison true where it is false: an equality into its disequality over the operands as they were, an order into the other strictness over them swapped.
#[test]
fn a_negated_comparison_is_the_comparison_that_negates_it() {
    for (op, folded, swapped) in [
        (Intrinsic::NatEql, Intrinsic::NatNeq, false),
        (Intrinsic::NatNeq, Intrinsic::NatEql, false),
        (Intrinsic::NatLt, Intrinsic::NatLe, true),
        (Intrinsic::NatLe, Intrinsic::NatLt, true),
        (Intrinsic::IntEql, Intrinsic::IntNeq, false),
        (Intrinsic::IntNeq, Intrinsic::IntEql, false),
        (Intrinsic::IntLt, Intrinsic::IntLe, true),
        (Intrinsic::IntLe, Intrinsic::IntLt, true),
    ] {
        let (mut module, negation, x, y) = negated(op, one());
        assert!(fold_negated_comparisons(&mut module), "{op:?}");
        let expected = match swapped {
            true => vec![Atom::Value(y), Atom::Value(x)],
            false => vec![Atom::Value(x), Atom::Value(y)],
        };
        assert!(
            matches!(
                module.node(negation),
                Some(Node::LetIntrinsic { op, args, .. }) if *op == folded && *args == expected
            ),
            "{op:?}: {:?}",
            module.node(negation)
        );
        module.verify().unwrap();
    }
}

/// What is no negation of a comparison on a total order stands: a `Flt` comparison, which against a NaN is false both ways round, an `xor` with zero, and the negation of an answer no comparison gave.
#[test]
fn what_negates_no_comparison_on_a_total_order_stands() {
    for (op, operand) in [
        (Intrinsic::FltLt, one()),
        (Intrinsic::NatEql, Literal::Nat(Natural::from(0u32))),
        (Intrinsic::NatAdd, one()),
    ] {
        let (mut module, negation, _, _) = negated(op, operand);
        assert!(!fold_negated_comparisons(&mut module), "{op:?}");
        assert!(matches!(
            module.node(negation),
            Some(Node::LetIntrinsic {
                op: Intrinsic::NatXor,
                ..
            })
        ));
    }
}
