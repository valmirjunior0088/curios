//! The shared rope helpers a `Bin` or `List` operation calls, and the chain depth they lower on.

use {super::test_support::*, curios_num::Grain};

#[test]
fn bin_slice_calls_the_shared_slice_helper() {
    let wat = wat(&intrinsic_main(
        curios_cont::Intrinsic::BinSlice(Grain::X),
        vec![bin_lit(vec![1, 2, 3]), nat(0), nat(2)],
    ));
    assert_contains(&wat, "call $bytes/slice");
    // The helper is emitted with its own declared type — the ABI both ends share.
    assert_contains(&wat, "(func $bytes/slice (type $bytes/slice)");
}

#[test]
fn bin_read_calls_the_read_helper_and_forces_its_input() {
    let wat = wat(&intrinsic_main(
        curios_cont::Intrinsic::BinGet(Grain::X),
        vec![bin_lit(vec![1, 2, 3]), nat(1)],
    ));
    assert_contains(&wat, "call $bytes/read");
    // Consuming a rope forces its outstanding view/node chain first.
    assert_contains(&wat, "call $bytes/force");
}

#[test]
fn bin_eql_calls_the_equality_helper() {
    let wat = wat(&intrinsic_main(
        curios_cont::Intrinsic::BinEql(Grain::X),
        vec![bin_lit(vec![1, 2]), bin_lit(vec![1, 2])],
    ));
    assert_contains(&wat, "call $bytes/eql");
}

#[test]
fn bin_concat_builds_o1_nodes_inline_without_a_helper() {
    let wat = wat(&intrinsic_main(
        curios_cont::Intrinsic::BinConcat(Grain::X, 2),
        vec![bin_lit(vec![1]), bin_lit(vec![2])],
    ));
    assert_contains(&wat, "struct.new $rope/bin/node");
    assert_absent(&wat, "call $bytes/slice");
}

/// Two immediates that fit their grain's envelope join without leaving it: the arm that tests both operands and their lengths stands ahead of the node, and ends in the immediate it builds. One arm a pair, so a join of three holds two.
#[test]
fn bin_concat_joins_two_immediates_by_arithmetic_ahead_of_the_node() {
    for grain in [Grain::X, Grain::B] {
        let operand = |bytes: Vec<u8>| {
            curios_cont::Atom::Literal(curios_cont::Literal::Bin(
                grain,
                curios_num::Binary::from_bytes(bytes),
            ))
        };

        let pair = wat(&intrinsic_main(
            curios_cont::Intrinsic::BinConcat(grain, 2),
            vec![operand(vec![1]), operand(vec![2])],
        ));
        assert_eq!(count(&pair, "block $bin_concat_rope"), 1, "{grain:?}");
        assert_eq!(count(&pair, "br $bin_concat"), 1, "{grain:?}");
        assert_contains(&pair, "struct.new $rope/bin/node");

        let triple = wat(&intrinsic_main(
            curios_cont::Intrinsic::BinConcat(grain, 3),
            vec![operand(vec![1]), operand(vec![2]), operand(vec![3])],
        ));
        assert_eq!(count(&triple, "block $bin_concat_rope"), 2, "{grain:?}");
    }
}

#[test]
fn list_read_calls_the_list_read_helper() {
    assert_contains(&wat(&list_read()), "call $list/read");
}

#[test]
fn list_map_calls_the_map_helper() {
    assert_contains(&wat(&list_map()), "call $list/map");
}

#[test]
fn deep_rope_chain_lowers_on_the_default_stack() {
    let wat = wat(&deep_bin_chain(5000));
    assert_contains(&wat, "struct.new $rope/bin/node");
}
