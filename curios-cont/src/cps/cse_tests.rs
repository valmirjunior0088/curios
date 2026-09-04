//! Common-subexpression elimination: what dominates, what commutes, and what may not be shared.

use {
    super::test_support::duplicate_pair_module,
    crate::cps::cse::dedupe_intrinsics,
    crate::{
        CpsAtom, CpsContinuation, CpsEdge, CpsFunction, CpsIntrinsic, CpsLiteral, CpsModule,
        CpsNode, CpsValueId,
    },
    curios_utilities::{Grain, PackedBin},
    std::collections::BTreeMap,
};

#[test]
fn merges_dominated_duplicates_onto_the_first_binder() {
    let (mut module, first_node, second_node, add) =
        duplicate_pair_module(CpsIntrinsic::NatMul, CpsIntrinsic::NatMul, false);

    assert!(dedupe_intrinsics(&mut module));
    assert!(matches!(
        module.node(first_node),
        Some(CpsNode::LetIntrinsic { .. })
    ));
    assert!(module.node(second_node).is_none());
    let first = CpsValueId(2);
    assert!(matches!(
        module.node(add),
        Some(CpsNode::LetIntrinsic { args, .. })
            if args == &[CpsAtom::Value(first), CpsAtom::Value(first)]
    ));
    module.verify().unwrap();
    assert!(
        !dedupe_intrinsics(&mut module),
        "a second run finds nothing"
    );
}

#[test]
fn normalizes_commutative_operand_order() {
    let (mut module, _, second_node, _) =
        duplicate_pair_module(CpsIntrinsic::NatAdd, CpsIntrinsic::NatAdd, true);

    assert!(dedupe_intrinsics(&mut module));
    assert!(module.node(second_node).is_none());
    module.verify().unwrap();
}

#[test]
fn keeps_noncommutative_swapped_operands_distinct() {
    let (mut module, first_node, second_node, _) =
        duplicate_pair_module(CpsIntrinsic::NatSub, CpsIntrinsic::NatSub, true);

    assert!(!dedupe_intrinsics(&mut module));
    assert!(module.node(first_node).is_some());
    assert!(module.node(second_node).is_some());
}

#[test]
fn reuses_a_dominating_may_trap_result() {
    let (mut module, first_node, second_node, _) =
        duplicate_pair_module(CpsIntrinsic::NatDiv, CpsIntrinsic::NatDiv, false);

    assert!(dedupe_intrinsics(&mut module));
    assert!(module.node(first_node).is_some());
    assert!(module.node(second_node).is_none());
    module.verify().unwrap();
}

#[test]
fn keeps_allocating_ops_distinct() {
    let (mut module, first_node, second_node, _) =
        duplicate_pair_module(CpsIntrinsic::ListAppend, CpsIntrinsic::ListAppend, false);

    assert!(!dedupe_intrinsics(&mut module));
    assert!(module.node(first_node).is_some());
    assert!(module.node(second_node).is_some());
}

#[test]
fn reaches_a_dominated_continuation_but_not_a_sibling() {
    let mut module = CpsModule::new();
    let entry = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let x = module.add_value(Some("x".into()));
    let dominating = module.add_value(Some("dominating".into()));
    let dominated = module.add_value(Some("dominated".into()));
    let sibling = module.add_value(Some("sibling".into()));

    // Reached continuation: recomputes the dominating op (must merge) and binds the first sibling occurrence of the shift (must stay).
    let shl_first = module.add_value(Some("shl first".into()));
    let reached = module.reserve_continuation();
    let reached_return = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: return_cont,
        args: vec![CpsAtom::Value(dominated)],
    }));
    let reached_shl = module.add_node(CpsNode::LetIntrinsic {
        result: shl_first,
        op: CpsIntrinsic::NatShl,
        args: vec![CpsAtom::Value(x), CpsAtom::Value(x)],
        next: reached_return,
    });
    let reached_body = module.add_node(CpsNode::LetIntrinsic {
        result: dominated,
        op: CpsIntrinsic::NatMul,
        args: vec![CpsAtom::Value(x), CpsAtom::Value(x)],
        next: reached_shl,
    });
    module.define_continuation(
        reached,
        CpsContinuation {
            debug_name: Some("reached".into()),
            params: vec![],
            body: reached_body,
        },
    );

    // Sibling continuation: computes the same shift as its sibling with no occurrence dominating both — must stay.
    let other = module.reserve_continuation();
    let other_return = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: return_cont,
        args: vec![CpsAtom::Value(sibling)],
    }));
    let other_body = module.add_node(CpsNode::LetIntrinsic {
        result: sibling,
        op: CpsIntrinsic::NatShl,
        args: vec![CpsAtom::Value(x), CpsAtom::Value(x)],
        next: other_return,
    });
    module.define_continuation(
        other,
        CpsContinuation {
            debug_name: Some("other".into()),
            params: vec![],
            body: other_body,
        },
    );

    // The dominating binding sits above the `LetCont`, so it is in scope — and dominates — both members; a binding inside the `LetCont`'s own body subtree would be neither.
    let switch = module.add_node(CpsNode::Switch {
        scrutinee: CpsAtom::Value(x),
        cases: BTreeMap::from([(
            0,
            CpsEdge {
                target: reached,
                args: vec![],
            },
        )]),
        default: Some(CpsEdge {
            target: other,
            args: vec![],
        }),
    });
    let letcont = module.add_node(CpsNode::LetCont {
        continuations: vec![reached, other],
        body: switch,
    });
    let bind = module.add_node(CpsNode::LetIntrinsic {
        result: dominating,
        op: CpsIntrinsic::NatMul,
        args: vec![CpsAtom::Value(x), CpsAtom::Value(x)],
        next: letcont,
    });
    module.define_function(
        entry,
        CpsFunction {
            debug_name: Some("main".into()),
            params: vec![x],
            return_cont,
            body: bind,
        },
    );
    module.set_entry(entry);

    assert!(dedupe_intrinsics(&mut module));
    assert!(module.node(reached_body).is_none(), "dominated dup merges");
    assert!(
        module.node(reached_shl).is_some(),
        "first sibling shl stays"
    );
    assert!(
        module.node(other_body).is_some(),
        "second sibling shl stays"
    );
    let forwards = module.nodes().iter().flatten().any(|node| {
        matches!(node, CpsNode::ApplyCont(edge) if edge.target == return_cont && edge.args == vec![CpsAtom::Value(dominating)])
    });
    assert!(forwards, "the merged use forwards the dominating result");
    module.verify().unwrap();
}

/// `b[1]` and `b[1, 0]` pack into the same byte, so an operand key built from packed bytes made these two comparisons duplicates and the second answered the first's result. Compiled, the program printed `false` for `x == b[1, 0]` where `x` was `b[1, 0]`.
#[test]
fn keeps_bit_literals_of_equal_packing_and_unequal_length_distinct() {
    let mut module = CpsModule::new();
    let entry = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let x = module.add_value(Some("x".into()));
    let first = module.add_value(Some("first".into()));
    let second = module.add_value(Some("second".into()));

    let return_node = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: return_cont,
        args: vec![CpsAtom::Value(second)],
    }));
    let second_node = module.add_node(CpsNode::LetIntrinsic {
        result: second,
        op: CpsIntrinsic::BinEql(Grain::B),
        args: vec![
            CpsAtom::Value(x),
            CpsAtom::Literal(CpsLiteral::Bin(
                Grain::B,
                PackedBin::from_bits([true, false]),
            )),
        ],
        next: return_node,
    });
    let first_node = module.add_node(CpsNode::LetIntrinsic {
        result: first,
        op: CpsIntrinsic::BinEql(Grain::B),
        args: vec![
            CpsAtom::Value(x),
            CpsAtom::Literal(CpsLiteral::Bin(Grain::B, PackedBin::from_bits([true]))),
        ],
        next: second_node,
    });
    module.define_function(
        entry,
        CpsFunction {
            debug_name: Some("main".into()),
            params: vec![x],
            return_cont,
            body: first_node,
        },
    );
    module.set_entry(entry);

    assert!(!dedupe_intrinsics(&mut module));
    assert!(module.node(first_node).is_some());
    assert!(module.node(second_node).is_some());
}
