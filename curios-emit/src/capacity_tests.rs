//! The construction boundary: logical length arithmetic stays wide until checked, including fused flat lists.

use {
    super::test_support::*,
    curios_num::{Binary, Natural},
};

/// One construction over a bound input, ending without an exit-code conversion so every narrowing belongs to the construction itself.
fn construction(
    op: curios_cps::Intrinsic,
    input: curios_cps::ValueExpr,
    arity: usize,
    append: bool,
) -> String {
    let mut module = curios_cps::Module::new();
    let value = module.add_value(Some("input".into()));
    let result = module.add_value(Some("result".into()));
    let end = module.add_node(curios_cps::Node::Unreachable);
    let args = (0..arity)
        .map(|index| {
            if append && index == 1 {
                nat(1)
            } else {
                curios_cps::Atom::Value(value)
            }
        })
        .collect();
    let operation = module.add_node(curios_cps::Node::LetIntrinsic {
        result,
        op,
        args,
        next: end,
    });
    let body = module.add_node(curios_cps::Node::LetValue {
        result: value,
        value: input,
        next: operation,
    });
    let return_cont = module.reserve_continuation();
    let main = module.add_function(curios_cps::Function {
        debug_name: Some("main".into()),
        params: vec![],
        return_cont,
        body,
        droppable: false,
    });
    module.set_entry(main);
    wat(&module)
}

#[test]
fn every_length_sum_is_checked_before_its_node_or_flat_allocation() {
    for op in [
        curios_cps::Intrinsic::BytesConcat(2),
        curios_cps::Intrinsic::BytesAppend,
    ] {
        let append = matches!(op, curios_cps::Intrinsic::BytesAppend);
        let wat = construction(
            op,
            curios_cps::ValueExpr::Literal(curios_cps::Literal::Bytes(Binary::from_bytes(vec![
                1, 2, 3, 4,
            ]))),
            2,
            append,
        );
        assert_contains(&wat, "i64.add");
        assert_contains(&wat, "i64.gt_u");
        assert_contains(&wat, "call $refuse/capacity");
    }
    for op in [
        curios_cps::Intrinsic::ListConcat(2),
        curios_cps::Intrinsic::ListAppend,
        curios_cps::Intrinsic::ListFlat(2),
    ] {
        let append = matches!(op, curios_cps::Intrinsic::ListAppend);
        let wat = construction(op, curios_cps::ValueExpr::List(vec![nat(1)]), 2, append);
        assert_contains(&wat, "i64.add");
        assert_contains(&wat, "call $refuse/capacity");
        if matches!(op, curios_cps::Intrinsic::ListFlat(_)) {
            assert!(
                wat.find("call $refuse/capacity").unwrap()
                    < wat.find("array.new_default $elems").unwrap()
            );
        }
    }
}

#[test]
fn a_requested_count_reaches_its_capacity_check_without_position_saturation() {
    let wat = construction(
        curios_cps::Intrinsic::BytesReplicate,
        curios_cps::ValueExpr::Literal(curios_cps::Literal::Nat(Natural::from(1u64 << 32))),
        2,
        true,
    );
    assert_contains(&wat, "call $big/count");
    // The element is a word; only the count preceding its capacity guard must avoid position saturation.
    let guard = wat.find("call $refuse/capacity").unwrap();
    assert_absent(&wat[..guard], "call $big/word");
    assert!(guard < wat.find("call $bytes/replicate").unwrap());
}
