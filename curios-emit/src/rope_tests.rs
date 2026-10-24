//! The shared rope helpers a `Bytes` or `List` operation calls, and the chain depth they lower on.

use {
    super::test_support::*,
    curios_abi::{WireLeaf, WireResults, WireType},
};

#[test]
fn bytes_slice_calls_the_shared_slice_helper() {
    let wat = wat(&intrinsic_main(
        curios_cont::Intrinsic::BytesSlice,
        vec![bytes_lit(vec![1, 2, 3]), nat(0), nat(2)],
    ));
    assert_contains(&wat, "call $bytes/slice");
    // The helper is emitted with its own declared type — the ABI both ends share.
    assert_contains(&wat, "(func $bytes/slice (type $bytes/slice)");
}

/// A cut copies nothing of its base, however short the window: a window is a view, so a loop that cuts from one base at every step pays O(1) a cut.
#[test]
fn a_cut_copies_nothing_of_its_base() {
    let wat = wat(&intrinsic_main(
        curios_cont::Intrinsic::BytesSlice,
        vec![bytes_lit(vec![1, 2, 3]), nat(0), nat(2)],
    ));

    assert_absent(func(&wat, "$bytes/slice"), "array.copy");
}

#[test]
fn bytes_read_calls_the_read_helper_and_forces_its_input() {
    let wat = wat(&intrinsic_main(
        curios_cont::Intrinsic::BytesGet,
        vec![bytes_lit(vec![1, 2, 3]), nat(1)],
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
        curios_cont::Intrinsic::BytesGet,
        vec![bytes_lit(vec![1, 2, 3]), nat(1)],
    ));

    for (wat, read, node, force) in [
        (packed, "$bytes/read", "$rope/bytes/node", "$bytes/force"),
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

/// A consumer of a forced payload bounds its walk by the run's `len` and never by its array's length, which a flattened payload may hold room past: equality, the pointwise combiners, a map, the host's deep and scalar forms and the write a force fills its payload by read no `array.len` at all, and a force reads one, the capacity of its first piece's payload, where it claims that piece's room.
#[test]
fn a_consumer_of_a_forced_payload_reads_the_runs_length() {
    let packed = |op: curios_cont::Intrinsic| {
        wat(&intrinsic_main(
            op,
            vec![bytes_lit(vec![1, 2, 3, 4]), bytes_lit(vec![5, 6, 7, 8])],
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
    let eql = packed(curios_cont::Intrinsic::BytesEql);
    let and = packed(curios_cont::Intrinsic::BytesAnd);
    let map = wat(&list_map());

    for (wat, helper) in [
        (&eql, "$bytes/eql"),
        (&and, "$bytes/and"),
        (&map, "$list/map"),
        (&host, "$list/bytes/force"),
        (&host, "$list/nat/to_longs"),
        (&eql, "$bytes/write"),
        (&map, "$list/write"),
    ] {
        assert_absent(func(wat, helper), "array.len");
    }
    for (wat, force) in [
        (&eql, "$bytes/force"),
        (&and, "$bytes/force"),
        (&map, "$list/force"),
    ] {
        assert_eq!(count(func(wat, force), "array.len"), 1, "{force}");
    }
}

/// A flattening allocates nothing but its payload: the force decides where the run goes and calls the write, which copies each piece where its offset falls, recursing into the shorter of two halves still to be walked rather than keeping a worklist, in each carrier.
#[test]
fn a_flattening_allocates_nothing_but_its_payload() {
    let bytes = wat(&intrinsic_main(
        curios_cont::Intrinsic::BytesGet,
        vec![bytes_lit(vec![1, 2, 3, 4]), nat(1)],
    ));
    let list = wat(&list_read());

    for (wat, force, write, payload) in [
        (&bytes, "$bytes/force", "$bytes/write", "$bytes"),
        (&list, "$list/force", "$list/write", "$elems"),
    ] {
        let forced = func(wat, force);
        assert_eq!(
            count(forced, &format!("array.new_default {payload}")),
            2,
            "a force allocates its payload, roomy or exact, and nothing else: {forced}"
        );
        assert_contains(forced, &format!("call {write}"));

        let written = func(wat, write);
        assert_absent(written, "array.new");
        assert_absent(written, "struct.new");
        assert_contains(written, &format!("call {write}"));
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
        curios_cont::Intrinsic::BytesGet,
        vec![bytes_lit(vec![1, 2, 3, 4]), nat(1)],
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
        curios_cont::Intrinsic::BytesConcat(2),
        vec![bytes_lit(vec![1, 2, 3, 4]), bytes_lit(vec![5, 6, 7, 8])],
    ));
    let room = wat
        .find("ref.null $rope/room")
        .expect("the node's room is null");
    let node = wat
        .find("struct.new $rope/bytes/node")
        .expect("the concatenation is a node");
    assert!(room < node);
}

#[test]
fn bytes_eql_calls_the_equality_helper() {
    let wat = wat(&intrinsic_main(
        curios_cont::Intrinsic::BytesEql,
        vec![bytes_lit(vec![1, 2]), bytes_lit(vec![1, 2])],
    ));
    assert_contains(&wat, "call $bytes/eql");
}

#[test]
fn bytes_concat_builds_o1_nodes_inline_without_a_helper() {
    let wat = wat(&intrinsic_main(
        curios_cont::Intrinsic::BytesConcat(2),
        vec![bytes_lit(vec![1]), bytes_lit(vec![2])],
    ));
    assert_contains(&wat, "struct.new $rope/bytes/node");
    assert_absent(&wat, "call $bytes/slice");
}

/// Two immediates that fit the envelope join without leaving it: the arm that tests both operands and their lengths stands ahead of the node, and ends in the immediate it builds. One arm a pair, so a join of three holds two.
#[test]
fn bytes_concat_joins_two_immediates_by_arithmetic_ahead_of_the_node() {
    let pair = wat(&intrinsic_main(
        curios_cont::Intrinsic::BytesConcat(2),
        vec![bytes_lit(vec![1]), bytes_lit(vec![2])],
    ));
    assert_eq!(count(&pair, "block $bytes_concat_rope"), 1);
    assert_eq!(count(&pair, "br $bytes_concat"), 1);
    assert_contains(&pair, "struct.new $rope/bytes/node");

    let triple = wat(&intrinsic_main(
        curios_cont::Intrinsic::BytesConcat(3),
        vec![bytes_lit(vec![1]), bytes_lit(vec![2]), bytes_lit(vec![3])],
    ));
    assert_eq!(count(&triple, "block $bytes_concat_rope"), 2);
}

/// A packed operand a lowering reads more than once is boxed once, since a box of an immediate is a fresh leaf: a concatenation's rope arm boxes each operand once for its empty tests, its length sum and its node, a pointwise combiner its left operand once for its force and its two lengths, and a suffix its carrier once for its window and its length.
#[test]
fn a_packed_operand_is_boxed_once_however_often_it_is_read() {
    let boxes = |op: curios_cont::Intrinsic, args| {
        count(
            func(&wat(&intrinsic_main(op, args)), "$func/main"),
            "call $bytes/box",
        )
    };

    assert_eq!(
        boxes(
            curios_cont::Intrinsic::BytesConcat(2),
            vec![bytes_lit(vec![1, 2]), bytes_lit(vec![3, 4])]
        ),
        2
    );
    assert_eq!(
        boxes(
            curios_cont::Intrinsic::BytesAnd,
            vec![bytes_lit(vec![1, 2]), bytes_lit(vec![3, 4])]
        ),
        2
    );
    assert_eq!(
        boxes(
            curios_cont::Intrinsic::BytesRest,
            vec![bytes_lit(vec![1, 2]), nat(1)]
        ),
        1
    );
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
    let wat = wat(&deep_bytes_chain(5000));
    assert_contains(&wat, "struct.new $rope/bytes/node");
}
