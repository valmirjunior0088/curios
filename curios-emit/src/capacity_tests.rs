//! The construction boundary: logical length arithmetic stays wide until checked, including host bit payloads and fused flat lists.

use {
    super::{EmissionModule, RopeEmitter, Table, test_support::*},
    curios_num::{Binary, Grain, Natural},
    std::collections::HashMap,
};

/// One construction over a bound input, ending without an exit-code conversion so every narrowing belongs to the construction itself.
fn construction(
    op: curios_cont::Intrinsic,
    input: curios_cont::ValueExpr,
    arity: usize,
    append: bool,
) -> String {
    let mut module = curios_cont::Module::new();
    let value = module.add_value(Some("input".into()));
    let result = module.add_value(Some("result".into()));
    let end = module.add_node(curios_cont::Node::Unreachable);
    let args = (0..arity)
        .map(|index| {
            if append && index == 1 {
                nat(1)
            } else {
                curios_cont::Atom::Value(value)
            }
        })
        .collect();
    let operation = module.add_node(curios_cont::Node::LetIntrinsic {
        result,
        op,
        args,
        next: end,
    });
    let body = module.add_node(curios_cont::Node::LetValue {
        result: value,
        value: input,
        next: operation,
    });
    let return_cont = module.reserve_continuation();
    let main = module.add_function(curios_cont::Function {
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
    for grain in [Grain::B, Grain::X] {
        for op in [
            curios_cont::Intrinsic::BinConcat(grain, 2),
            curios_cont::Intrinsic::BinAppend(grain),
        ] {
            let append = matches!(op, curios_cont::Intrinsic::BinAppend(_));
            let wat = construction(
                op,
                curios_cont::ValueExpr::Literal(curios_cont::Literal::Bin(
                    grain,
                    Binary::from_bytes(vec![1, 2, 3, 4]),
                )),
                2,
                append,
            );
            assert_contains(&wat, "i64.add");
            assert_contains(&wat, "i64.gt_u");
            assert_contains(&wat, "call $refuse/capacity");
        }
    }
    for op in [
        curios_cont::Intrinsic::ListConcat(2),
        curios_cont::Intrinsic::ListAppend,
        curios_cont::Intrinsic::ListFlat(2),
    ] {
        let append = matches!(op, curios_cont::Intrinsic::ListAppend);
        let wat = construction(op, curios_cont::ValueExpr::List(vec![nat(1)]), 2, append);
        assert_contains(&wat, "i64.add");
        assert_contains(&wat, "call $refuse/capacity");
        if matches!(op, curios_cont::Intrinsic::ListFlat(_)) {
            assert!(
                wat.find("call $refuse/capacity").unwrap()
                    < wat.find("array.new_default $elems").unwrap()
            );
        }
    }
}

#[test]
fn a_requested_count_reaches_its_capacity_check_without_position_saturation() {
    for grain in [Grain::B, Grain::X] {
        let wat = construction(
            curios_cont::Intrinsic::BinReplicate(grain),
            curios_cont::ValueExpr::Literal(curios_cont::Literal::Nat(Natural::from(1u64 << 32))),
            2,
            true,
        );
        assert_contains(&wat, "call $big/count");
        // The element is a word; only the count preceding its capacity guard must avoid position saturation.
        let guard = wat.find("call $refuse/capacity").unwrap();
        assert_absent(&wat[..guard], "call $big/word");
        assert!(
            wat.find("call $refuse/capacity").unwrap()
                < wat
                    .find("call $bits/replicate")
                    .or_else(|| wat.find("call $bytes/replicate"))
                    .unwrap()
        );
    }
}

#[test]
fn a_byte_to_bit_scale_is_checked_before_forcing_the_payload() {
    let wat = construction(
        curios_cont::Intrinsic::BinReinterp(Grain::X),
        curios_cont::ValueExpr::Literal(curios_cont::Literal::Bin(
            Grain::X,
            Binary::from_bytes(vec![1, 2, 3, 4]),
        )),
        1,
        false,
    );
    assert_contains(&wat, "i64.shl");
    assert!(wat.find("call $refuse/capacity").unwrap() < wat.find("call $bytes/force").unwrap());
}

#[test]
fn a_host_bit_payload_is_checked_at_both_embedding_depths() {
    for deep in [false, true] {
        let emission = EmissionModule::new();
        let raw = HashMap::new();
        let table = Table::new(&emission, &raw);
        let mut module = curios_wasm::Module::new("embed");
        let mut ropes = RopeEmitter::new(&table, &mut module);
        if deep {
            ropes.emit_list_bin_embed_func(Grain::B, table.list_bits_embed_func());
        } else {
            ropes.emit_bits_embed_func(table.bits_embed_func());
        }
        let wat = module.to_string();
        assert_contains(&wat, "i64.shl");
        assert_contains(&wat, "call $refuse/capacity");
        assert!(
            wat.find("call $refuse/capacity").unwrap()
                < wat.find("struct.new $rope/bin/leaf").unwrap()
        );
    }
}
