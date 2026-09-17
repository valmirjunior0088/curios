//! The driver over the whole pass suite, and the budgets that bound its fixpoint.

use {
    super::test_support::{PolymorphicLoop, polymorphic_loop, tagged_consumer, tagged_join},
    crate::cps::optimize::optimize,
    crate::{Callee, Intrinsic, Node, ValueExpr},
};

#[test]
fn optimization_specializes_away_the_polymorphic_indirect_call() {
    // With each caller peeled into its own clone, invariant-known propagation resolves every `op` to a direct callee, leaving no closure calls.
    let PolymorphicLoop { mut module, .. } = polymorphic_loop(true, 0);
    optimize(&mut module);
    module.verify().unwrap();
    assert!(
        module.nodes().iter().flatten().all(|node| !matches!(
            node,
            Node::ApplyFun {
                callee: Callee::Closure(_),
                ..
            }
        )),
        "specialization turned every recursive indirect call into a direct call"
    );
}

#[test]
fn optimization_eliminates_a_constructor_dispatch() {
    // A multi-site, oversized-for-inlining consumer: only specialization can resolve the tagged dispatch, and folding then removes every switch.
    let (mut module, _, _) = tagged_consumer(8, &[0, 0]);
    optimize(&mut module);
    module.verify().unwrap();
    assert!(
        module
            .nodes()
            .iter()
            .flatten()
            .all(|node| !matches!(node, Node::Switch { .. })),
        "specialization and folding leave no residual tag dispatch"
    );
}

#[test]
fn optimization_collapses_the_tagged_join_outright() {
    let (mut module, _, _, _, _) = tagged_join();
    optimize(&mut module);

    // The whole allocate-then-rescrutinize shape dissolves: no tuple is built and no projection or tag dispatch survives.
    for node in module.nodes().iter().flatten() {
        assert!(
            !matches!(
                node,
                Node::LetValue {
                    value: ValueExpr::Tuple(_),
                    ..
                } | Node::LetIntrinsic {
                    op: Intrinsic::TupleGet(_),
                    ..
                }
            ),
            "expected the join's tuples and projections to collapse, found {node:?}"
        );
    }
}
