//! How a value's carrier is lowered: scalars, a match's dispatch, the encodings of a family's constructors, a sequence's grain, a halt and an integer's switch.

use {
    super::test_support::*,
    crate::*,
    curios_abi::{ForeignFunction, WireReturn, WireSignature, WireType},
    curios_num::{Binary, Integer},
    std::sync::Arc,
};

fn has_intrinsic(
    module: &curios_cps::Module,
    predicate: impl Fn(&curios_cps::Intrinsic) -> bool,
) -> bool {
    has_node(module, |node| match node {
        curios_cps::Node::LetIntrinsic { op, .. } => predicate(op),
        _ => false,
    })
}

fn has_value(
    module: &curios_cps::Module,
    predicate: impl Fn(&curios_cps::ValueExpr) -> bool,
) -> bool {
    has_node(module, |node| match node {
        curios_cps::Node::LetValue { value, .. } => predicate(value),
        _ => false,
    })
}

/// Every intrinsic the lowering emitted, so a test can say what it did *not* emit by saying what it did.
fn intrinsics(module: &curios_cps::Module) -> Vec<curios_cps::Intrinsic> {
    module
        .nodes()
        .iter()
        .flatten()
        .filter_map(|node| match node {
            curios_cps::Node::LetIntrinsic { op, .. } => Some(*op),
            _ => None,
        })
        .collect()
}

/// Whether a `Nat` literal of `value` is an operand anywhere — how a mask the lowering synthesizes is found, since nothing names it.
fn has_nat_operand(module: &curios_cps::Module, value: u32) -> bool {
    module.nodes().iter().flatten().any(|node| {
        curios_cps::atoms(node).into_iter().any(|atom| {
            matches!(atom, curios_cps::Atom::Literal(curios_cps::Literal::Nat(literal)) if u32::try_from(literal).ok() == Some(value))
        })
    })
}

/// `match 0x[1, 2, 3] | 0x[] => 0 | 0x[h, ..t] => ...` over `Bytes`, its cons arm's answer built by `cons` from the element and the suffix.
fn bytes_case_split(cons: impl FnOnce(&mut Builder, ValueId, ValueId) -> Atom) -> Module {
    let mut builder = Builder::new();
    builder.open_block();
    let bytes = builder.constant(Constant::Bytes(Binary::from_bytes(vec![1, 2, 3])));
    builder.open_block();
    let zero = nat(&mut builder, 0);
    let empty = builder.seal_block(Terminator::Return(zero));
    let element = builder.value(Some("h".into()));
    let suffix = builder.value(Some("t".into()));
    builder.open_block();
    let answer = cons(&mut builder, element, suffix);
    let block = builder.seal_block(Terminator::Return(answer));
    let peeled = builder.let_value(
        None,
        Rhs::UnconsSequence {
            grain: SequenceGrain::Bytes,
            scrutinee: Atom::Constant(bytes),
            empty,
            cons: UnconsSequenceStep {
                element,
                suffix,
                block,
            },
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(peeled)));
    builder.set_entry(entry);
    builder.finalize().expect("verifies")
}

#[test]
fn a_scalar_module_lowers_to_verified_cps() {
    let mut builder = Builder::new();
    let one = nat(&mut builder, 1);
    let bound = builder.item_value(Some("one".into()), Rhs::Alias(one));
    builder.open_block();
    let doubled = builder.let_value(
        Some("doubled".into()),
        Rhs::Operation {
            operation: Operation::NatAdd,
            operands: vec![Atom::Value(bound), Atom::Value(bound)],
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(doubled)));
    builder.set_entry(entry);
    let module = builder.finalize().expect("verifies");

    let cps = lowered(&module);
    assert!(
        intrinsics(&cps).contains(&curios_cps::Intrinsic::NatAdd),
        "{cps}"
    );
}

#[test]
fn bool_and_byte_collapse_onto_the_nat_carrier() {
    let mut builder = Builder::new();
    builder.open_block();
    let t = builder.constant(Constant::Bool(true));
    let f = builder.constant(Constant::Bool(false));
    let both = builder.let_value(
        None,
        Rhs::Operation {
            operation: Operation::BoolAnd,
            operands: vec![Atom::Constant(t), Atom::Constant(f)],
        },
    );
    let masked = builder.let_value(
        None,
        Rhs::Operation {
            operation: Operation::NatToByte,
            operands: vec![Atom::Value(both)],
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(masked)));
    builder.set_entry(entry);
    let module = builder.finalize().expect("verifies");

    let cps = lowered(&module);
    // Bool ops ride Nat bit ops and `NatToByte` masks, so no Bool- or Byte-shaped operation survives the door. The mask is what makes the narrowing total *here* — Core refuses an out-of-range operand and `NatToByte`'s own `below` field promises this can only ever be narrowing a value already in range, so the instruction is provably redundant rather than load-bearing, and it is emitted because the carrier is shared rather than because anything could be out of range.
    assert!(
        intrinsics(&cps).contains(&curios_cps::Intrinsic::NatAnd),
        "{cps}"
    );
    assert!(has_nat_operand(&cps, 255), "{cps}");
}

#[test]
fn byte_to_nat_is_the_identity() {
    let mut builder = Builder::new();
    builder.open_block();
    let byte = builder.constant(Constant::Byte(9));
    let widened = builder.let_value(
        None,
        Rhs::Operation {
            operation: Operation::ByteToNat,
            operands: vec![Atom::Constant(byte)],
        },
    );
    let sum = builder.let_value(
        None,
        Rhs::Operation {
            operation: Operation::NatAdd,
            operands: vec![Atom::Value(widened), Atom::Value(widened)],
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(sum)));
    builder.set_entry(entry);
    let module = builder.finalize().expect("verifies");
    let cps = lowered(&module);
    assert_eq!(
        intrinsics(&cps),
        vec![curios_cps::Intrinsic::NatAdd],
        "{cps}"
    );
}

#[test]
fn a_variant_match_lowers_to_tag_dispatch() {
    let mut builder = Builder::new();
    let family = builder.family(Some("Opt".into()));
    let none = builder.constructor(family, Some("none".into()), vec![]);
    let some = builder.constructor(
        family,
        Some("some".into()),
        vec![Field::opaque(Some("x".into()))],
    );

    builder.open_block();
    let six = nat(&mut builder, 6);
    let value = builder.let_value(
        None,
        Rhs::Construct {
            constructor: some,
            fields: vec![six],
        },
    );
    builder.open_block();
    let zero = nat(&mut builder, 0);
    let none_arm = builder.seal_block(Terminator::Return(zero));
    builder.open_block();
    let x = builder.value(Some("x".into()));
    let some_arm = builder.seal_block(Terminator::Return(Atom::Value(x)));
    let matched = builder.let_value(
        None,
        Rhs::MatchVariant {
            family,
            scrutinee: Atom::Value(value),
            arms: vec![
                VariantArm {
                    constructor: none,
                    bindings: vec![],
                    block: none_arm,
                },
                VariantArm {
                    constructor: some,
                    bindings: vec![x],
                    block: some_arm,
                },
            ],
            default: None,
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(matched)));
    builder.set_entry(entry);
    let module = builder.finalize().expect("verifies");

    let cps = lowered(&module);
    // The constructor is its family's own value — the tag at slot 0, payloads above it, padded to the family's width — so dispatch reads the tag and the payload through the family rather than through the arity roster.
    assert!(
        has_intrinsic(
            &cps,
            |op| matches!(op, curios_cps::Intrinsic::RowGet(row, slot) if row.index() == 0 && *slot == 0)
        ),
        "{cps}"
    );
    assert!(
        has_intrinsic(
            &cps,
            |op| matches!(op, curios_cps::Intrinsic::RowGet(row, slot) if row.index() == 0 && *slot == 1)
        ),
        "{cps}"
    );
    assert!(
        !has_intrinsic(&cps, |op| matches!(op, curios_cps::Intrinsic::TupleGet(_))),
        "{cps}"
    );
    assert!(
        has_node(&cps, |node| matches!(node, curios_cps::Node::Switch { .. })),
        "{cps}"
    );
}

#[test]
fn a_single_constructor_family_collapses_to_its_bare_payload() {
    let mut builder = Builder::new();
    let family = builder.family(Some("Id".into()));
    let wrap = builder.constructor(
        family,
        Some("wrap".into()),
        vec![Field::immediate(Some("x".into()))],
    );

    builder.open_block();
    let six = nat(&mut builder, 6);
    let value = builder.let_value(
        None,
        Rhs::Construct {
            constructor: wrap,
            fields: vec![six],
        },
    );
    builder.open_block();
    let x = builder.value(Some("x".into()));
    let arm = builder.seal_block(Terminator::Return(Atom::Value(x)));
    let matched = builder.let_value(
        None,
        Rhs::MatchVariant {
            family,
            scrutinee: Atom::Value(value),
            arms: vec![VariantArm {
                constructor: wrap,
                bindings: vec![x],
                block: arm,
            }],
            default: None,
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(matched)));
    builder.set_entry(entry);
    let module = builder.finalize().expect("verifies");

    let cps = lowered(&module);
    // The collapsed value is the payload itself: no tuple is built, no tag is read, and the match never dispatches.
    assert!(
        !has_value(&cps, |value| matches!(
            value,
            curios_cps::ValueExpr::Tuple(_)
        )),
        "{cps}"
    );
    assert!(
        !has_intrinsic(&cps, |op| matches!(op, curios_cps::Intrinsic::TupleGet(_))),
        "{cps}"
    );
    assert!(
        !has_node(&cps, |node| matches!(node, curios_cps::Node::Switch { .. })),
        "{cps}"
    );
}

#[test]
fn an_immediate_constructor_rides_its_payload() {
    let mut builder = Builder::new();
    let family = builder.family(Some("Tree".into()));
    let leaf = builder.constructor(
        family,
        Some("leaf".into()),
        vec![Field::immediate(Some("n".into()))],
    );
    let node = builder.constructor(
        family,
        Some("node".into()),
        vec![
            Field::immediate(Some("v".into())),
            Field::opaque(Some("l".into())),
            Field::opaque(Some("r".into())),
        ],
    );

    builder.open_block();
    let six = nat(&mut builder, 6);
    let value = builder.let_value(
        None,
        Rhs::Construct {
            constructor: leaf,
            fields: vec![six],
        },
    );
    builder.open_block();
    let n = builder.value(Some("n".into()));
    let leaf_arm = builder.seal_block(Terminator::Return(Atom::Value(n)));
    builder.open_block();
    let v = builder.value(Some("v".into()));
    let l = builder.value(Some("l".into()));
    let r = builder.value(Some("r".into()));
    let node_arm = builder.seal_block(Terminator::Return(Atom::Value(v)));
    let matched = builder.let_value(
        None,
        Rhs::MatchVariant {
            family,
            scrutinee: Atom::Value(value),
            arms: vec![
                VariantArm {
                    constructor: leaf,
                    bindings: vec![n],
                    block: leaf_arm,
                },
                VariantArm {
                    constructor: node,
                    bindings: vec![v, l, r],
                    block: node_arm,
                },
            ],
            default: None,
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(matched)));
    builder.set_entry(entry);
    let module = builder.finalize().expect("verifies");

    let cps = lowered(&module);
    // The leaf construct builds nothing, the dispatch is an `IsImmediate` test, and with exactly one boxed constructor the tag is never read — the node arm's payloads still project at their tagged offsets.
    assert!(
        !has_value(&cps, |value| matches!(
            value,
            curios_cps::ValueExpr::Tuple(_)
        )),
        "{cps}"
    );
    assert!(
        intrinsics(&cps).contains(&curios_cps::Intrinsic::IsImmediate),
        "{cps}"
    );
    assert!(
        !has_intrinsic(
            &cps,
            |op| matches!(op, curios_cps::Intrinsic::RowGet(row, slot) if row.index() == 0 && *slot == 0)
        ),
        "{cps}"
    );
    assert!(
        has_intrinsic(
            &cps,
            |op| matches!(op, curios_cps::Intrinsic::RowGet(row, slot) if row.index() == 0 && *slot == 1)
        ),
        "{cps}"
    );
    // The immediate arm's payload is *read*, not aliased to the scrutinee. Without this node the binder and the scrutinee are one value, and a raw demand from the arm reaches the scrutinee's own definition — which on the boxed path builds a tuple. See `an_immediate_arm_payload_survives_arithmetic_in_a_loop` in `curios`'s matching tests for what that emits.
    assert!(
        intrinsics(&cps).contains(&curios_cps::Intrinsic::ImmediateGet),
        "{cps}"
    );
}

#[test]
fn an_immediate_family_with_two_boxed_constructors_keeps_the_inner_tag_dispatch() {
    let mut builder = Builder::new();
    let family = builder.family(Some("Wide".into()));
    let one = builder.constructor(
        family,
        Some("one".into()),
        vec![Field::immediate(Some("n".into()))],
    );
    let empty = builder.constructor(family, Some("empty".into()), vec![]);
    let pair = builder.constructor(
        family,
        Some("pair".into()),
        vec![
            Field::opaque(Some("a".into())),
            Field::opaque(Some("b".into())),
        ],
    );

    builder.open_block();
    let six = nat(&mut builder, 6);
    let value = builder.let_value(
        None,
        Rhs::Construct {
            constructor: one,
            fields: vec![six],
        },
    );
    builder.open_block();
    let n = builder.value(Some("n".into()));
    let one_arm = builder.seal_block(Terminator::Return(Atom::Value(n)));
    builder.open_block();
    let zero = nat(&mut builder, 0);
    let empty_arm = builder.seal_block(Terminator::Return(zero));
    builder.open_block();
    let a = builder.value(Some("a".into()));
    let b = builder.value(Some("b".into()));
    let pair_arm = builder.seal_block(Terminator::Return(Atom::Value(a)));
    let matched = builder.let_value(
        None,
        Rhs::MatchVariant {
            family,
            scrutinee: Atom::Value(value),
            arms: vec![
                VariantArm {
                    constructor: one,
                    bindings: vec![n],
                    block: one_arm,
                },
                VariantArm {
                    constructor: empty,
                    bindings: vec![],
                    block: empty_arm,
                },
                VariantArm {
                    constructor: pair,
                    bindings: vec![a, b],
                    block: pair_arm,
                },
            ],
            default: None,
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(matched)));
    builder.set_entry(entry);
    let module = builder.finalize().expect("verifies");

    let cps = lowered(&module);
    // Two boxed constructors remain behind the test, so the tag dispatch survives on that side.
    assert!(
        intrinsics(&cps).contains(&curios_cps::Intrinsic::IsImmediate),
        "{cps}"
    );
    assert!(
        has_intrinsic(
            &cps,
            |op| matches!(op, curios_cps::Intrinsic::RowGet(row, slot) if row.index() == 0 && *slot == 0)
        ),
        "{cps}"
    );
}

#[test]
fn two_immediate_constructors_decline_the_encoding() {
    let mut builder = Builder::new();
    let family = builder.family(Some("Either".into()));
    let left = builder.constructor(
        family,
        Some("left".into()),
        vec![Field::immediate(Some("a".into()))],
    );
    let _right = builder.constructor(
        family,
        Some("right".into()),
        vec![Field::immediate(Some("b".into()))],
    );

    builder.open_block();
    let six = nat(&mut builder, 6);
    let value = builder.let_value(
        None,
        Rhs::Construct {
            constructor: left,
            fields: vec![six],
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(value)));
    builder.set_entry(entry);
    let module = builder.finalize().expect("verifies");

    let cps = lowered(&module);
    // Two immediate constructors would collide on the same i31 values, so the family stays tagged — a row of its own family rather than a structural tuple.
    assert!(
        has_value(
            &cps,
            |value| matches!(value, curios_cps::ValueExpr::Row(row, _) if row.index() == 0)
        ),
        "{cps}"
    );
    assert!(
        !intrinsics(&cps).contains(&curios_cps::Intrinsic::IsImmediate),
        "{cps}"
    );
}

#[test]
fn a_collapsed_pair_is_an_untagged_tuple() {
    let mut builder = Builder::new();
    let family = builder.family(Some("Pair".into()));
    let both = builder.constructor(
        family,
        Some("both".into()),
        vec![
            Field::immediate(Some("a".into())),
            Field::immediate(Some("b".into())),
        ],
    );

    builder.open_block();
    let six = nat(&mut builder, 6);
    let seven = nat(&mut builder, 7);
    let value = builder.let_value(
        None,
        Rhs::Construct {
            constructor: both,
            fields: vec![six, seven],
        },
    );
    builder.open_block();
    let a = builder.value(Some("a".into()));
    let b = builder.value(Some("b".into()));
    let arm = builder.seal_block(Terminator::Return(Atom::Value(b)));
    let matched = builder.let_value(
        None,
        Rhs::MatchVariant {
            family,
            scrutinee: Atom::Value(value),
            arms: vec![VariantArm {
                constructor: both,
                bindings: vec![a, b],
                block: arm,
            }],
            default: None,
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(matched)));
    builder.set_entry(entry);
    let module = builder.finalize().expect("verifies");

    let cps = lowered(&module);
    // Untagged: the collapsed family is its own nominal row with no tag slot, so the second payload reads at slot 1 where the tagged encoding would put it at 2 — and nothing dispatches.
    assert!(
        has_intrinsic(
            &cps,
            |op| matches!(op, curios_cps::Intrinsic::RowGet(row, slot) if row.index() == 0 && *slot == 1)
        ),
        "{cps}"
    );
    assert!(
        !has_intrinsic(
            &cps,
            |op| matches!(op, curios_cps::Intrinsic::RowGet(row, slot) if row.index() == 0 && *slot == 2)
        ),
        "{cps}"
    );
    assert!(
        !has_intrinsic(&cps, |op| matches!(op, curios_cps::Intrinsic::TupleGet(_))),
        "{cps}"
    );
    assert!(
        !has_node(&cps, |node| matches!(node, curios_cps::Node::Switch { .. })),
        "{cps}"
    );
}

#[test]
fn a_collapsed_nullary_constructor_is_the_interned_zero() {
    let mut builder = Builder::new();
    let family = builder.family(Some("One".into()));
    let only = builder.constructor(family, Some("only".into()), vec![]);

    builder.open_block();
    let value = builder.let_value(
        None,
        Rhs::Construct {
            constructor: only,
            fields: vec![],
        },
    );
    builder.open_block();
    let five = nat(&mut builder, 5);
    let arm = builder.seal_block(Terminator::Return(five));
    let matched = builder.let_value(
        None,
        Rhs::MatchVariant {
            family,
            scrutinee: Atom::Value(value),
            arms: vec![VariantArm {
                constructor: only,
                bindings: vec![],
                block: arm,
            }],
            default: None,
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(matched)));
    builder.set_entry(entry);
    let module = builder.finalize().expect("verifies");

    let cps = lowered(&module);
    // The value carries zero information and rides the cheapest carrier — the Unit encoding — so nothing is allocated and nothing dispatches.
    assert!(
        !has_value(&cps, |value| matches!(
            value,
            curios_cps::ValueExpr::Tuple(_)
        )),
        "{cps}"
    );
    assert!(
        !has_node(&cps, |node| matches!(node, curios_cps::Node::Switch { .. })),
        "{cps}"
    );
}

/// The anonymous tuple row is shared by every tuple of its width, so its fields name no type: a list read by length out of one pair and a scalar stored into another meet at the same field key, and a settle inserted on that verdict would wrap the scalar. Neither construction settles.
#[test]
fn a_shared_tuple_field_is_never_settled() {
    let mut builder = Builder::new();
    let pair = builder.product(ProductSchema {
        debug_name: None,
        fields: ProductFields::Shared(2),
    });

    builder.open_block();
    let one = nat(&mut builder, 1);
    let list = builder.let_value(
        None,
        Rhs::Sequence {
            operation: SequenceOp::ListBuild,
            operands: vec![one],
        },
    );
    let zero = nat(&mut builder, 0);
    let listed = builder.let_value(
        None,
        Rhs::Product {
            schema: pair,
            fields: vec![Atom::Value(list), zero],
        },
    );
    let seven = nat(&mut builder, 7);
    let scalar = builder.let_value(
        None,
        Rhs::Product {
            schema: pair,
            fields: vec![seven, zero],
        },
    );
    let head = builder.let_value(
        None,
        Rhs::Project {
            schema: pair,
            product: Atom::Value(listed),
            field: 0,
        },
    );
    let length = builder.let_value(
        None,
        Rhs::Sequence {
            operation: SequenceOp::ListLen,
            operands: vec![Atom::Value(head)],
        },
    );
    let other = builder.let_value(
        None,
        Rhs::Project {
            schema: pair,
            product: Atom::Value(scalar),
            field: 1,
        },
    );
    let total = builder.let_value(
        None,
        Rhs::Operation {
            operation: Operation::NatAdd,
            operands: vec![Atom::Value(length), Atom::Value(other)],
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(total)));
    builder.set_entry(entry);
    let module = builder.finalize().expect("verifies");

    let cps = lowered(&module);
    assert!(
        !intrinsics(&cps).contains(&curios_cps::Intrinsic::ListSettle),
        "{cps}"
    );
}

#[test]
fn a_sequence_case_split_reads_through_its_grain() {
    // The arm reads its suffix, which is what makes the suffix read part of this claim: an unused one is not emitted at all, per `an_unused_peel_suffix_is_not_built`.
    let module = bytes_case_split(|builder, _, suffix| {
        Atom::Value(builder.let_value(
            None,
            Rhs::Sequence {
                operation: SequenceOp::BytesLen,
                operands: vec![Atom::Value(suffix)],
            },
        ))
    });
    let cps = lowered(&module);
    assert!(
        has_intrinsic(&cps, |op| matches!(op, curios_cps::Intrinsic::BytesLen)),
        "{cps}"
    );
    assert!(
        has_intrinsic(&cps, |op| matches!(op, curios_cps::Intrinsic::BytesGet)),
        "{cps}"
    );
    // A suffix, not a window: the peel names a start and lets the value decide how much follows, so no lowering derives a count. A `BytesSlice` here would mean a lowering derives one.
    assert!(
        has_intrinsic(&cps, |op| matches!(op, curios_cps::Intrinsic::BytesRest)),
        "{cps}"
    );
    assert!(
        !has_intrinsic(&cps, |op| matches!(op, curios_cps::Intrinsic::BytesSlice)),
        "{cps}"
    );
}

/// A case split whose cons arm ignores its suffix pays nothing for it.
///
/// The suffix is a view, allocated each time the arm runs — once per element in a loop peeling its sequence. Many such arms discard it, so emitting it unconditionally would put an allocation per element into a loop for a value nothing reads.
///
/// **No later pass can undo that**, which is why the check belongs at the point of emission: `BytesRest` is `MayTrap`, so dead-result elimination must keep one, and the reason this one cannot trap — it reads past the head of a sequence the dispatch found non-empty — is a property of the dispatch this same function emits rather than a range fact recoverable downstream.
#[test]
fn an_unused_peel_suffix_is_not_built() {
    let module = bytes_case_split(|_, element, _| Atom::Value(element));
    let cps = lowered(&module);
    // The element is still read through its grain; only the discarded suffix is gone.
    assert!(
        has_intrinsic(&cps, |op| matches!(op, curios_cps::Intrinsic::BytesGet)),
        "{cps}"
    );
    assert!(
        !has_intrinsic(&cps, |op| matches!(op, curios_cps::Intrinsic::BytesRest)),
        "{cps}"
    );
}

#[test]
fn a_halt_lowers_to_its_node() {
    let mut builder = Builder::new();
    builder.open_block();
    let three = nat(&mut builder, 3);
    let exit = builder.foreign(Arc::new(ForeignFunction {
        name: "/exit".to_string(),
        signature: WireSignature {
            params: vec![WireType::Byte],
            returns: WireReturn::Never,
        },
    }));
    let entry = builder.seal_block(Terminator::Halt {
        foreign: exit,
        operands: vec![three],
    });
    builder.set_entry(entry);
    let module = builder.finalize().expect("verifies");
    let cps = lowered(&module);
    assert!(
        has_node(&cps, |node| matches!(node, curios_cps::Node::Halt { .. })),
        "{cps}"
    );
}

#[test]
fn io_constants_ride_the_binary_carrier() {
    let mut builder = Builder::new();
    builder.open_block();
    let stdout = builder.constant(Constant::Handle(1));
    let width = builder.let_value(
        None,
        Rhs::Sequence {
            operation: SequenceOp::BytesLen,
            operands: vec![Atom::Constant(stdout)],
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(width)));
    builder.set_entry(entry);
    let module = builder.finalize().expect("verifies");
    let cps = lowered(&module);
    assert!(
        has_intrinsic(&cps, |op| matches!(op, curios_cps::Intrinsic::BytesLen)),
        "{cps}"
    );
}

/// An integer's switch is decided by one `IntEql` test a key, since no signed narrowing of the scrutinee indexes a table.
#[test]
fn an_integers_switch_lowers_to_one_equality_test_a_key() {
    let mut builder = Builder::new();
    builder.open_block();
    let ten = nat(&mut builder, 10);
    let below = builder.seal_block(Terminator::Return(ten));
    builder.open_block();
    let twenty = nat(&mut builder, 20);
    let above = builder.seal_block(Terminator::Return(twenty));
    builder.open_block();
    let rest = nat(&mut builder, 99);
    let default = builder.seal_block(Terminator::Return(rest));
    builder.open_block();
    let seven = builder.constant(Constant::Int(Integer::from(7i32)));
    let switched = builder.let_value(
        None,
        Rhs::Switch {
            scrutinee: Atom::Constant(seven),
            keys: Keys::Int(vec![
                Case {
                    key: Integer::from(-1i32),
                    block: below,
                },
                Case {
                    key: Integer::from(2i32),
                    block: above,
                },
            ]),
            default,
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(switched)));
    builder.set_entry(entry);
    let module = builder.finalize().expect("verifies");

    let cps = lowered(&module);
    let tests = intrinsics(&cps)
        .into_iter()
        .filter(|op| *op == curios_cps::Intrinsic::IntEql)
        .count();
    assert_eq!(tests, 2, "{cps}");
}
