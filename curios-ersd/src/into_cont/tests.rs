use {
    crate::*,
    curios_num::{Binary, Grain, Natural},
};

fn nat(builder: &mut ErsdBuilder, value: u32) -> Atom {
    let constant = builder.constant(Constant::Nat(Natural::from(value)));
    Atom::Constant(constant)
}

/// Every completed lowering has already passed `curios_cont::Module::verify` inside `lower_to_cont`; the shape assertions on top ask the graph.
///
/// They ask the graph rather than the printed text, which they once did. A spelling belongs to the printer, and when the printer changed, every one of these turned out to have been a claim about a Rust variant name — three of them would have gone quietly vacuous rather than failing. The printed module is still what a failure reports, because that is what a reader needs in front of them.
fn lowered(module: &Module) -> curios_cont::Module {
    lower_to_cont(module)
}

fn has_node(module: &curios_cont::Module, predicate: impl Fn(&curios_cont::Node) -> bool) -> bool {
    module.nodes().iter().flatten().any(predicate)
}

fn has_intrinsic(
    module: &curios_cont::Module,
    predicate: impl Fn(&curios_cont::Intrinsic) -> bool,
) -> bool {
    has_node(module, |node| match node {
        curios_cont::Node::LetIntrinsic { op, .. } => predicate(op),
        _ => false,
    })
}

fn has_value(
    module: &curios_cont::Module,
    predicate: impl Fn(&curios_cont::ValueExpr) -> bool,
) -> bool {
    has_node(module, |node| match node {
        curios_cont::Node::LetValue { value, .. } => predicate(value),
        _ => false,
    })
}

/// Every intrinsic the lowering emitted, so a test can say what it did *not* emit by saying what it did.
fn intrinsics(module: &curios_cont::Module) -> Vec<curios_cont::Intrinsic> {
    module
        .nodes()
        .iter()
        .flatten()
        .filter_map(|node| match node {
            curios_cont::Node::LetIntrinsic { op, .. } => Some(*op),
            _ => None,
        })
        .collect()
}

/// Whether a `Nat` literal of `value` is an operand anywhere — how a mask the lowering synthesizes is found, since nothing names it.
fn has_nat_operand(module: &curios_cont::Module, value: u32) -> bool {
    module.nodes().iter().flatten().any(|node| {
        curios_cont::atoms(node).into_iter().any(|atom| {
            matches!(atom, curios_cont::Atom::Literal(curios_cont::Literal::Nat(literal)) if literal.to_u32() == Some(value))
        })
    })
}

#[test]
fn a_scalar_module_lowers_to_verified_cont() {
    let mut builder = ErsdBuilder::new();
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

    let cont = lowered(&module);
    assert!(
        intrinsics(&cont).contains(&curios_cont::Intrinsic::NatAdd),
        "{cont}"
    );
}

#[test]
fn bool_and_byte_collapse_onto_the_nat_carrier() {
    let mut builder = ErsdBuilder::new();
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

    let cont = lowered(&module);
    // Bool ops ride Nat bit ops and `NatToByte` masks, so no Bool- or Byte-shaped operation survives the door. The mask is what makes the narrowing total *here* — Core refuses an out-of-range operand and `NatToByte`'s own `below` field promises this can only ever be narrowing a value already in range, so the instruction is provably redundant rather than load-bearing, and it is emitted because the carrier is shared rather than because anything could be out of range.
    assert!(
        intrinsics(&cont).contains(&curios_cont::Intrinsic::NatAnd),
        "{cont}"
    );
    assert!(has_nat_operand(&cont, 255), "{cont}");
}

#[test]
fn byte_to_nat_is_the_identity() {
    let mut builder = ErsdBuilder::new();
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
    let cont = lowered(&module);
    assert_eq!(
        intrinsics(&cont),
        vec![curios_cont::Intrinsic::NatAdd],
        "{cont}"
    );
}

#[test]
fn a_variant_match_lowers_to_tag_dispatch() {
    let mut builder = ErsdBuilder::new();
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

    let cont = lowered(&module);
    // The constructor is its family's own value — the tag at slot 0, payloads above it, padded to the family's width — so dispatch reads the tag and the payload through the family rather than through the arity roster.
    assert!(
        has_intrinsic(
            &cont,
            |op| matches!(op, curios_cont::Intrinsic::RowGet(row, slot) if row.index() == 0 && *slot == 0)
        ),
        "{cont}"
    );
    assert!(
        has_intrinsic(
            &cont,
            |op| matches!(op, curios_cont::Intrinsic::RowGet(row, slot) if row.index() == 0 && *slot == 1)
        ),
        "{cont}"
    );
    assert!(
        !has_intrinsic(&cont, |op| matches!(
            op,
            curios_cont::Intrinsic::TupleGet(_)
        )),
        "{cont}"
    );
    assert!(
        has_node(&cont, |node| matches!(
            node,
            curios_cont::Node::Switch { .. }
        )),
        "{cont}"
    );
}

#[test]
fn a_single_constructor_family_collapses_to_its_bare_payload() {
    let mut builder = ErsdBuilder::new();
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

    let cont = lowered(&module);
    // The collapsed value is the payload itself: no tuple is built, no tag is read, and the match never dispatches.
    assert!(
        !has_value(&cont, |value| matches!(
            value,
            curios_cont::ValueExpr::Tuple(_)
        )),
        "{cont}"
    );
    assert!(
        !has_intrinsic(&cont, |op| matches!(
            op,
            curios_cont::Intrinsic::TupleGet(_)
        )),
        "{cont}"
    );
    assert!(
        !has_node(&cont, |node| matches!(
            node,
            curios_cont::Node::Switch { .. }
        )),
        "{cont}"
    );
}

#[test]
fn an_immediate_constructor_rides_its_payload() {
    let mut builder = ErsdBuilder::new();
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

    let cont = lowered(&module);
    // The leaf construct builds nothing, the dispatch is an `IsImmediate` test, and with exactly one boxed constructor the tag is never read — the node arm's payloads still project at their tagged offsets.
    assert!(
        !has_value(&cont, |value| matches!(
            value,
            curios_cont::ValueExpr::Tuple(_)
        )),
        "{cont}"
    );
    assert!(
        intrinsics(&cont).contains(&curios_cont::Intrinsic::IsImmediate),
        "{cont}"
    );
    assert!(
        !has_intrinsic(
            &cont,
            |op| matches!(op, curios_cont::Intrinsic::RowGet(row, slot) if row.index() == 0 && *slot == 0)
        ),
        "{cont}"
    );
    assert!(
        has_intrinsic(
            &cont,
            |op| matches!(op, curios_cont::Intrinsic::RowGet(row, slot) if row.index() == 0 && *slot == 1)
        ),
        "{cont}"
    );
    // The immediate arm's payload is *read*, not aliased to the scrutinee. Without this node the binder and the scrutinee are one value, and a raw demand from the arm reaches the scrutinee's own definition — which on the boxed path built a tuple. See `an_immediate_arm_payload_survives_arithmetic_in_a_loop` in `curios`'s matching tests for what that emitted.
    assert!(
        intrinsics(&cont).contains(&curios_cont::Intrinsic::ImmediateGet),
        "{cont}"
    );
}

#[test]
fn an_immediate_family_with_two_boxed_constructors_keeps_the_inner_tag_dispatch() {
    let mut builder = ErsdBuilder::new();
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

    let cont = lowered(&module);
    // Two boxed constructors remain behind the test, so the tag dispatch survives on that side.
    assert!(
        intrinsics(&cont).contains(&curios_cont::Intrinsic::IsImmediate),
        "{cont}"
    );
    assert!(
        has_intrinsic(
            &cont,
            |op| matches!(op, curios_cont::Intrinsic::RowGet(row, slot) if row.index() == 0 && *slot == 0)
        ),
        "{cont}"
    );
}

#[test]
fn two_immediate_constructors_decline_the_encoding() {
    let mut builder = ErsdBuilder::new();
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

    let cont = lowered(&module);
    // Two immediate constructors would collide on the same i31 values, so the family stays tagged — which since family keying means a `Variant` of its own family rather than a structural tuple.
    assert!(
        has_value(
            &cont,
            |value| matches!(value, curios_cont::ValueExpr::Row(row, _) if row.index() == 0)
        ),
        "{cont}"
    );
    assert!(
        !intrinsics(&cont).contains(&curios_cont::Intrinsic::IsImmediate),
        "{cont}"
    );
}

#[test]
fn a_collapsed_pair_is_an_untagged_tuple() {
    let mut builder = ErsdBuilder::new();
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

    let cont = lowered(&module);
    // Untagged: the collapsed family is its own nominal row with no tag slot, so the second payload reads at slot 1 where the tagged encoding would put it at 2 — and nothing dispatches.
    assert!(
        has_intrinsic(
            &cont,
            |op| matches!(op, curios_cont::Intrinsic::RowGet(row, slot) if row.index() == 0 && *slot == 1)
        ),
        "{cont}"
    );
    assert!(
        !has_intrinsic(
            &cont,
            |op| matches!(op, curios_cont::Intrinsic::RowGet(row, slot) if row.index() == 0 && *slot == 2)
        ),
        "{cont}"
    );
    assert!(
        !has_intrinsic(&cont, |op| matches!(
            op,
            curios_cont::Intrinsic::TupleGet(_)
        )),
        "{cont}"
    );
    assert!(
        !has_node(&cont, |node| matches!(
            node,
            curios_cont::Node::Switch { .. }
        )),
        "{cont}"
    );
}

#[test]
fn a_collapsed_nullary_constructor_is_the_interned_zero() {
    let mut builder = ErsdBuilder::new();
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

    let cont = lowered(&module);
    // The value carries zero information and rides the cheapest carrier — the Unit encoding — so nothing is allocated and nothing dispatches.
    assert!(
        !has_value(&cont, |value| matches!(
            value,
            curios_cont::ValueExpr::Tuple(_)
        )),
        "{cont}"
    );
    assert!(
        !has_node(&cont, |node| matches!(
            node,
            curios_cont::Node::Switch { .. }
        )),
        "{cont}"
    );
}

/// The anonymous tuple row is shared by every tuple of its width, so its fields name no type: a list read by length out of one pair and a scalar stored into another meet at the same field key, and a settle inserted on that verdict would wrap the scalar. Neither construction settles.
#[test]
fn a_shared_tuple_field_is_never_settled() {
    let mut builder = ErsdBuilder::new();
    let pair = builder.product(ProductSchema {
        debug_name: None,
        fields: vec![Field::opaque(None); 2],
        shared: true,
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

    let cont = lowered(&module);
    assert!(
        !intrinsics(&cont).contains(&curios_cont::Intrinsic::ListSettle),
        "{cont}"
    );
}

#[test]
fn folds_lower_to_accumulator_loops() {
    let mut builder = ErsdBuilder::new();
    builder.open_block();
    let five = nat(&mut builder, 5);
    builder.open_block();
    let zero = nat(&mut builder, 0);
    let zero_block = builder.seal_block(Terminator::Return(zero));
    let predecessor = builder.value(Some("pred".into()));
    let hypothesis = builder.value(Some("ih".into()));
    builder.open_block();
    let two = nat(&mut builder, 2);
    let stepped = builder.let_value(
        None,
        Rhs::Operation {
            operation: Operation::NatAdd,
            operands: vec![Atom::Value(hypothesis), two],
        },
    );
    let step_block = builder.seal_block(Terminator::Return(Atom::Value(stepped)));
    let folded = builder.let_value(
        None,
        Rhs::FoldNat {
            scrutinee: five,
            zero: zero_block,
            step: FoldNatStep {
                predecessor,
                hypothesis,
                block: step_block,
            },
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(folded)));
    builder.set_entry(entry);
    let module = builder.finalize().expect("verifies");
    // Completing at all means the synthesized loop verified.
    let cont = lowered(&module);
    assert!(
        intrinsics(&cont).contains(&curios_cont::Intrinsic::NatEql),
        "{cont}"
    );
}

#[test]
fn a_sequence_fold_reads_through_its_grain() {
    let mut builder = ErsdBuilder::new();
    builder.open_block();
    let bytes = builder.constant(Constant::Bin(Grain::X, Binary::from_bytes(vec![1, 2, 3])));
    builder.open_block();
    let zero = nat(&mut builder, 0);
    let empty_block = builder.seal_block(Terminator::Return(zero));
    let element = builder.value(Some("h".into()));
    let suffix = builder.value(Some("t".into()));
    let accumulator = builder.value(Some("ih".into()));
    builder.open_block();
    // The step reads its suffix, which is what makes the suffix read part of this claim: an unused one is not emitted at all, per `an_unused_fold_suffix_is_not_sliced`.
    let remaining = builder.let_value(
        None,
        Rhs::Sequence {
            operation: SequenceOp::BinLen(Grain::X),
            operands: vec![Atom::Value(suffix)],
        },
    );
    let stepped = builder.let_value(
        None,
        Rhs::Operation {
            operation: Operation::NatAdd,
            operands: vec![Atom::Value(accumulator), Atom::Value(remaining)],
        },
    );
    let step_block = builder.seal_block(Terminator::Return(Atom::Value(stepped)));
    let folded = builder.let_value(
        None,
        Rhs::FoldSequence {
            grain: SequenceGrain::Bin(Grain::X),
            scrutinee: Atom::Constant(bytes),
            empty: empty_block,
            step: FoldSequenceStep {
                element,
                suffix,
                accumulator,
                block: step_block,
            },
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(folded)));
    builder.set_entry(entry);
    let module = builder.finalize().expect("verifies");
    let cont = lowered(&module);
    assert!(
        has_intrinsic(&cont, |op| matches!(op, curios_cont::Intrinsic::BinLen(_))),
        "{cont}"
    );
    assert!(
        has_intrinsic(&cont, |op| matches!(op, curios_cont::Intrinsic::BinGet(_))),
        "{cont}"
    );
    // A suffix, not a window: the peel names a start and lets the value decide how much follows, so no lowering derives a count. A `BinSlice` here would mean one had gone back to deriving one.
    assert!(
        has_intrinsic(&cont, |op| matches!(op, curios_cont::Intrinsic::BinRest(_))),
        "{cont}"
    );
    assert!(
        !has_intrinsic(&cont, |op| matches!(
            op,
            curios_cont::Intrinsic::BinSlice(_)
        )),
        "{cont}"
    );
}

/// A fold whose step ignores its suffix pays nothing for it.
///
/// The suffix is a slice, and a slice allocates a view — once per element, inside the loop. Nearly every fold discards it (`Bytes/fold`, `List/fold`, and `/std/Str/fold`'s per-character walk, whose `t` survives only in erased `Prop` positions), so emitting it unconditionally put one allocation and one runtime allocation call per element into the hottest loops in the corpus, for a value nothing reads.
///
/// **No later pass can undo that**, which is why the check belongs at the point of emission: `BinSlice` is `MayTrap`, so dead-result elimination must keep one, and the reason this one cannot trap — the loop below indexes `[i, len)` with `1 <= i <= len` — is a property of the loop this same function emits rather than a range fact recoverable downstream.
#[test]
fn an_unused_fold_suffix_is_not_sliced() {
    let mut builder = ErsdBuilder::new();
    builder.open_block();
    let bytes = builder.constant(Constant::Bin(Grain::X, Binary::from_bytes(vec![1, 2, 3])));
    builder.open_block();
    let zero = nat(&mut builder, 0);
    let empty_block = builder.seal_block(Terminator::Return(zero));
    let element = builder.value(Some("h".into()));
    let suffix = builder.value(Some("t".into()));
    let accumulator = builder.value(Some("ih".into()));
    builder.open_block();
    // Reads the element and the hypothesis, never the suffix.
    let stepped = builder.let_value(
        None,
        Rhs::Operation {
            operation: Operation::NatAdd,
            operands: vec![Atom::Value(accumulator), Atom::Value(element)],
        },
    );
    let step_block = builder.seal_block(Terminator::Return(Atom::Value(stepped)));
    let folded = builder.let_value(
        None,
        Rhs::FoldSequence {
            grain: SequenceGrain::Bin(Grain::X),
            scrutinee: Atom::Constant(bytes),
            empty: empty_block,
            step: FoldSequenceStep {
                element,
                suffix,
                accumulator,
                block: step_block,
            },
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(folded)));
    builder.set_entry(entry);
    let module = builder.finalize().expect("verifies");
    let cont = lowered(&module);
    // The element is still read through its grain; only the discarded suffix is gone.
    assert!(
        has_intrinsic(&cont, |op| matches!(op, curios_cont::Intrinsic::BinGet(_))),
        "{cont}"
    );
    assert!(
        !has_intrinsic(&cont, |op| matches!(
            op,
            curios_cont::Intrinsic::BinSlice(_)
        )),
        "{cont}"
    );
}

/// A group with a function member and a computed member ties through the same cells a value-only knot does: the function member's forward reference to the computed member is a cell read at its entry, never a direct capture of a value that does not exist yet.
#[test]
fn a_mixed_recursive_group_lowers_through_cells() {
    let mut builder = ErsdBuilder::new();
    let produce = builder.reserve_function();
    let consume = builder.value(Some("consume".into()));
    builder.open_block();
    let body = builder.seal_block(Terminator::Return(Atom::Value(consume)));
    builder.define_function(produce, Some("produce".into()), vec![], body);
    builder.open_block();
    let init = builder.seal_block(Terminator::Return(Atom::Function(produce)));
    let group = builder.rec_group(vec![produce], vec![(consume, init)]);
    builder.item_rec(group);
    builder.open_block();
    let entry = builder.seal_block(Terminator::Return(Atom::Value(consume)));
    builder.set_entry(entry);
    let module = builder.finalize().expect("verifies");
    let cont = lowered(&module);
    assert!(
        has_node(&cont, |node| matches!(
            node,
            curios_cont::Node::Cell {
                op: curios_cont::CellOp::Get,
                ..
            }
        )),
        "{cont}"
    );
    assert!(
        has_node(&cont, |node| matches!(
            node,
            curios_cont::Node::Cell {
                op: curios_cont::CellOp::Set,
                ..
            }
        )),
        "{cont}"
    );
    assert!(
        has_node(&cont, |node| matches!(
            node,
            curios_cont::Node::LetFun { .. }
        )),
        "{cont}"
    );
}

#[test]
fn a_value_only_knot_lowers_through_cells() {
    // rec { lazy = Pair { force: fn() = lazy } } — the knot ties through a compiler-internal cell: allocated, stored once, read at closure entry.
    let mut builder = ErsdBuilder::new();
    let schema = builder.product(ProductSchema {
        debug_name: Some("Lazy".into()),
        fields: vec![
            Field::opaque(Some("force".into())),
            Field::opaque(Some("mark".into())),
        ],
        shared: false,
    });
    let lazy = builder.value(Some("lazy".into()));
    builder.open_block();
    let force = builder.reserve_function();
    builder.open_block();
    let force_body = builder.seal_block(Terminator::Return(Atom::Value(lazy)));
    builder.define_function(force, Some("force".into()), vec![], force_body);
    builder.let_functions(vec![force]);
    let mark = nat(&mut builder, 0);
    let boxed = builder.let_value(
        None,
        Rhs::Product {
            schema,
            fields: vec![Atom::Function(force), mark],
        },
    );
    let init = builder.seal_block(Terminator::Return(Atom::Value(boxed)));
    let group = builder.rec_group(vec![], vec![(lazy, init)]);
    builder.item_rec(group);
    builder.open_block();
    let entry = builder.seal_block(Terminator::Return(Atom::Value(lazy)));
    builder.set_entry(entry);
    let module = builder.finalize().expect("verifies");

    let cont = lowered(&module);
    assert!(
        has_node(&cont, |node| matches!(node, curios_cont::Node::Cell { .. })),
        "{cont}"
    );
}

#[test]
fn exit_and_unreachable_lower_to_their_nodes() {
    let mut builder = ErsdBuilder::new();
    builder.open_block();
    let three = nat(&mut builder, 3);
    let entry = builder.seal_block(Terminator::Exit(three));
    builder.set_entry(entry);
    let module = builder.finalize().expect("verifies");
    let cont = lowered(&module);
    assert!(
        has_node(&cont, |node| matches!(node, curios_cont::Node::Exit { .. })),
        "{cont}"
    );
}

#[test]
fn io_constants_ride_the_binary_carrier() {
    let mut builder = ErsdBuilder::new();
    builder.open_block();
    let stdout = builder.constant(Constant::Handle(1));
    let width = builder.let_value(
        None,
        Rhs::Sequence {
            operation: SequenceOp::BinLen(Grain::X),
            operands: vec![Atom::Constant(stdout)],
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(width)));
    builder.set_entry(entry);
    let module = builder.finalize().expect("verifies");
    let cont = lowered(&module);
    assert!(
        has_intrinsic(&cont, |op| matches!(op, curios_cont::Intrinsic::BinLen(_))),
        "{cont}"
    );
}

#[test]
fn a_knot_member_read_only_by_an_item_is_kept() {
    // rec { lazy = Pair { force: fn() = lazy, mark: 0 } }; let read = lazy.mark — the only read is a top-level item, which lives in no block. A member is dropped when nothing outside its initializer reads it, and a scan of the blocks alone missed this read, so the member vanished and the item's operand lowered to a value the arena lacked.
    let mut builder = ErsdBuilder::new();
    let schema = builder.product(ProductSchema {
        debug_name: Some("Lazy".into()),
        fields: vec![
            Field::opaque(Some("force".into())),
            Field::opaque(Some("mark".into())),
        ],
        shared: false,
    });
    let lazy = builder.value(Some("lazy".into()));
    builder.open_block();
    let force = builder.reserve_function();
    builder.open_block();
    let force_body = builder.seal_block(Terminator::Return(Atom::Value(lazy)));
    builder.define_function(force, Some("force".into()), vec![], force_body);
    builder.let_functions(vec![force]);
    let mark = nat(&mut builder, 0);
    let boxed = builder.let_value(
        None,
        Rhs::Product {
            schema,
            fields: vec![Atom::Function(force), mark],
        },
    );
    let init = builder.seal_block(Terminator::Return(Atom::Value(boxed)));
    let group = builder.rec_group(vec![], vec![(lazy, init)]);
    builder.item_rec(group);
    let read = builder.item_value(
        Some("read".into()),
        Rhs::Project {
            schema,
            product: Atom::Value(lazy),
            field: 1,
        },
    );
    builder.open_block();
    let entry = builder.seal_block(Terminator::Return(Atom::Value(read)));
    builder.set_entry(entry);
    let module = builder.finalize().expect("verifies");

    let cont = lowered(&module);
    assert!(
        has_node(&cont, |node| matches!(node, curios_cont::Node::Cell { .. })),
        "{cont}"
    );
}

/// The lowering stamps each continuation with the role it fills, which is what lets a dump name a block instead of numbering it. Nothing below reads these — `curios-emit` takes function names only — so they are display and no more; contification adds the contified function's own name on top of them.
#[test]
fn every_lowered_continuation_carries_the_role_it_fills() {
    let mut builder = ErsdBuilder::new();
    let identity = builder.reserve_function();
    let subject = builder.value(Some("subject".into()));
    builder.open_block();
    let identity_body = builder.seal_block(Terminator::Return(Atom::Value(subject)));
    builder.define_function(
        identity,
        Some("identity".into()),
        vec![subject],
        identity_body,
    );
    builder.item_functions(vec![identity]);

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
    // A call on the match's result, and a statement after the call: the first keeps the match's result flowing into a join rather than straight out of the function, the second keeps the call's result flowing into a continuation rather than taking `split`'s tail bypass.
    let called = builder.let_value(
        None,
        Rhs::Apply {
            callee: Atom::Function(identity),
            arguments: vec![Atom::Value(matched)],
        },
    );
    let one = nat(&mut builder, 1);
    let bumped = builder.let_value(
        None,
        Rhs::Operation {
            operation: Operation::NatAdd,
            operands: vec![Atom::Value(called), one],
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(bumped)));
    builder.set_entry(entry);
    let module = builder.finalize().expect("verifies");

    let cont = lowered(&module);
    let mut roles: Vec<&str> = cont
        .continuations()
        .iter()
        .flatten()
        .map(|continuation| {
            continuation
                .debug_name
                .as_deref()
                .expect("every lowered continuation names its role")
        })
        .collect();
    roles.sort_unstable();
    assert_eq!(
        roles,
        ["apply/resume", "arm/none", "arm/some", "join"],
        "{cont}"
    );
}
