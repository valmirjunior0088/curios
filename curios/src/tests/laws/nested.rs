//! What a term costs to class by its layers: an operation whose operands hold operations in turn, each commuted against its mirror, a sum of many operations of one kind, and a sum of many calls of one head.
//!
//! A node is held to another by its own reader over operands spelled already, so each layer of a term is read once, from its leaves, and only against the nodes that stand over its atoms. Two nodes put to a checker as two atoms are would have their operands classed again inside each comparison, and the cost would double with every level; a node read against every earlier node of its operation would be read pair by pair at every fold above it. A stuck sum is classified from the sum it extends, so a sum of calls of one head puts each pair of calls to its checker once, where classifying every level from nothing asks the classes below again. The fixtures hold the three shapes at sizes none answers at otherwise; the bench's `nested_operation_measurements` reports what each size of the first two costs.

use {crate::test_support::*, curios_common::fill};

/// A sum of `summands` calls `g(xk)` of one head, against the sum written in the other order, closed by `Eq/refl()`: any two calls may be one, so every call is put to its checker against the classes opened before it.
fn called(summands: usize) -> String {
    let binders = (1..=summands)
        .map(|at| format!("x{at}: Nat"))
        .collect::<Vec<_>>()
        .join(", ");
    let left = (1..=summands)
        .map(|at| format!("g(x{at})"))
        .collect::<Vec<_>>()
        .join(" + ");
    let right = (1..=summands)
        .rev()
        .map(|at| format!("g(x{at})"))
        .collect::<Vec<_>>()
        .join(" + ");
    fill!(
        "use /std/{Nat, Eq, Io};\nlet called(g: (Nat) -> Nat, ${binders}) -> Eq()(${left}, ${right}) => Eq/refl();\nIo/pure(())",
        binders,
        left,
        right
    )
}

#[test]
fn an_operation_nested_and_commuted_at_every_level_is_one_with_its_mirror() {
    if let Err(error) = typecheck(&nested(32)) {
        panic!("{error}");
    }
}

#[test]
fn a_sum_of_operations_each_commuted_is_one_with_the_sum_written_backwards() {
    if let Err(error) = typecheck(&summed(64)) {
        panic!("{error}");
    }
}

/// Each stuck sum of calls is classified from the sum it extends, so each pair of calls is put to its checker once; classified from nothing at every level, the classes of every sum below are asked again, and the compile takes twenty times as long.
#[test]
fn a_sum_of_calls_of_one_head_is_one_with_the_sum_written_backwards() {
    if let Err(error) = typecheck(&called(256)) {
        panic!("{error}");
    }
}
