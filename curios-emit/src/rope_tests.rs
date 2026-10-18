//! The shared rope helpers a `Bin` or `List` operation calls, and the chain depth they lower on.

use {
    super::test_support::*,
    curios_abi::{WireLeaf, WireResults, WireType},
    curios_num::Grain,
};

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

/// The text of one emitted function, from its header to the next function's.
fn func<'a>(wat: &'a str, name: &str) -> &'a str {
    let start = wat
        .find(&format!("(func {name} "))
        .expect("the function is emitted");
    let end = wat[start + 1..]
        .find("(func ")
        .map_or(wat.len(), |next| start + 1 + next);

    &wat[start..end]
}

/// A read of a node that was never flattened looks in its two children before it reaches for `force`, in each carrier: a read that falls in a flat child copies nothing.
#[test]
fn a_read_of_an_unflattened_node_looks_in_its_children_before_it_forces() {
    let packed = wat(&intrinsic_main(
        curios_cont::Intrinsic::BinGet(Grain::X),
        vec![bin_lit(vec![1, 2, 3]), nat(1)],
    ));

    for (wat, read, node, force) in [
        (packed, "$bytes/read", "$rope/bin/node", "$bytes/force"),
        (
            wat(&list_read()),
            "$list/read",
            "$rope/list/node",
            "$list/force",
        ),
    ] {
        let read = func(&wat, read);
        let left = read
            .find(&format!("struct.get {node} $left"))
            .expect("the helper reads the left child");
        let right = read
            .find(&format!("struct.get {node} $right"))
            .expect("the helper reads the right child");
        let force = read
            .find(&format!("call {force}"))
            .expect("the helper still forces");

        assert!(left < right && right < force, "{read}");
    }
}

/// A consumer of a forced payload bounds its walk by the run's `len` and never by its array's length, which a flattened payload may hold room past: equality, the pointwise combiners, a map, and the host's deep and scalar forms read no `array.len` at all, and a force reads two, its worklist's and, where it claims its first piece's room, the capacity of that piece's payload.
#[test]
fn a_consumer_of_a_forced_payload_reads_the_runs_length() {
    let packed = |op: fn(Grain) -> curios_cont::Intrinsic| {
        wat(&intrinsic_main(
            op(Grain::X),
            vec![bin_lit(vec![1, 2, 3, 4]), bin_lit(vec![5, 6, 7, 8])],
        ))
    };
    let host = wat(&foreign_call_to(row(
        "both",
        vec![
            WireType::List(WireLeaf::Bytes),
            WireType::List(WireLeaf::Nat),
        ],
        WireResults::single(WireType::Nat),
    )));
    let eql = packed(curios_cont::Intrinsic::BinEql);
    let and = packed(curios_cont::Intrinsic::BinAnd);
    let map = wat(&list_map());

    for (wat, helper) in [
        (&eql, "$bytes/eql"),
        (&and, "$bin/and"),
        (&map, "$list/map"),
        (&host, "$list/bytes/force"),
        (&host, "$list/nat/to_longs"),
    ] {
        assert_absent(func(wat, helper), "array.len");
    }
    for (wat, force) in [
        (&eql, "$bytes/force"),
        (&and, "$bytes/force"),
        (&map, "$list/force"),
    ] {
        assert_eq!(count(func(wat, force), "array.len"), 2, "{force}");
    }
}

/// What leaves the rope as a flat array takes the run exact: a settle seals its leaf over `exact`'s payload, and a host operand, a host list's elements among them, crosses as it — which answers the forced payload where its array is the run's length, and a copy of the run otherwise.
#[test]
fn what_leaves_the_rope_is_its_exact_payload() {
    let settle = wat(&list_settle());
    let host = wat(&foreign_call_to(row(
        "send",
        vec![WireType::Bytes, WireType::List(WireLeaf::Bytes)],
        WireResults::single(WireType::Nat),
    )));

    assert_contains(func(&settle, "$func/main"), "call $list/exact");
    assert_contains(func(&host, "$func/main"), "call $bytes/exact");
    assert_contains(func(&host, "$list/bytes/force"), "call $bytes/exact");

    for (wat, exact, force) in [
        (&settle, "$list/exact", "call $list/force"),
        (&host, "$bytes/exact", "call $bytes/force"),
    ] {
        let exact = func(wat, exact);
        assert_contains(exact, force);
        assert_contains(exact, "array.len");
        assert_contains(exact, "array.copy");
    }
}

/// A flattening reads its first piece's written extent before it allocates, so a run that ends at its payload's extent extends that payload instead; and a fresh payload's room is twice the run, clamped to the largest length a run holds.
#[test]
fn a_flattening_reads_its_first_pieces_extent_before_it_allocates() {
    let bytes = wat(&intrinsic_main(
        curios_cont::Intrinsic::BinGet(Grain::X),
        vec![bin_lit(vec![1, 2, 3, 4]), nat(1)],
    ));
    let list = wat(&list_read());

    for (wat, force, payload) in [
        (&bytes, "$bytes/force", "$bytes"),
        (&list, "$list/force", "$elems"),
    ] {
        let force = func(wat, force);
        let extent = force
            .find("struct.get $rope/room $extent")
            .expect("the force reads the extent");
        // The run's payload, which the worklist ahead of the walk may share a type with.
        assert!(
            force[extent..].contains(&format!("array.new_default {payload}")),
            "the force allocates the run's payload after the extent"
        );
        assert_contains(force, "struct.set $rope/room $extent");
        assert_contains(force, "struct.new $rope/room");
        assert_contains(force, &format!("i64.const {}", u32::MAX));
    }
}

/// A node is built with no cache and no room, both written by the flattening that memoizes it.
#[test]
fn a_node_is_built_with_no_room() {
    let wat = wat(&intrinsic_main(
        curios_cont::Intrinsic::BinConcat(Grain::X, 2),
        vec![bin_lit(vec![1, 2, 3, 4]), bin_lit(vec![5, 6, 7, 8])],
    ));
    let room = wat
        .find("ref.null $rope/room")
        .expect("the node's room is null");
    let node = wat
        .find("struct.new $rope/bin/node")
        .expect("the concatenation is a node");
    assert!(room < node);
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

/// Two immediates that fit the envelope join without leaving it: the arm that tests both operands and their lengths stands ahead of the node, and ends in the immediate it builds. One arm a pair, so a join of three holds two.
#[test]
fn bin_concat_joins_two_immediates_by_arithmetic_ahead_of_the_node() {
    let pair = wat(&intrinsic_main(
        curios_cont::Intrinsic::BinConcat(Grain::X, 2),
        vec![bin_lit(vec![1]), bin_lit(vec![2])],
    ));
    assert_eq!(count(&pair, "block $bin_concat_rope"), 1);
    assert_eq!(count(&pair, "br $bin_concat"), 1);
    assert_contains(&pair, "struct.new $rope/bin/node");

    let triple = wat(&intrinsic_main(
        curios_cont::Intrinsic::BinConcat(Grain::X, 3),
        vec![bin_lit(vec![1]), bin_lit(vec![2]), bin_lit(vec![3])],
    ));
    assert_eq!(count(&triple, "block $bin_concat_rope"), 2);
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
