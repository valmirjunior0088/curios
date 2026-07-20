use {super::*, curios_base::Grain};

fn nat(builder: &mut ErsdBuilder, value: u32) -> ErasedAtom {
    let constant = builder.constant(Constant::Nat(value));
    ErasedAtom::Constant(constant)
}

/// Every completed lowering has already passed `CpsModule::verify` inside
/// `lower_to_cont`; the shape assertions on top read the printed module.
fn lowered(module: &ErasedModule) -> String {
    lower_to_cont(module).to_string()
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
            operands: vec![ErasedAtom::Value(bound), ErasedAtom::Value(bound)],
        },
    );
    let entry = builder.seal_block(Terminator::Return(ErasedAtom::Value(doubled)));
    builder.set_entry(entry);
    let module = builder.finalize().expect("verifies");

    let printed = lowered(&module);
    assert!(printed.contains("NatAdd"), "{printed}");
}

#[test]
fn bln_and_byte_collapse_onto_the_nat_carrier() {
    let mut builder = ErsdBuilder::new();
    builder.open_block();
    let t = builder.constant(Constant::Bln(true));
    let f = builder.constant(Constant::Bln(false));
    let both = builder.let_value(
        None,
        Rhs::Operation {
            operation: Operation::BlnAnd,
            operands: vec![ErasedAtom::Constant(t), ErasedAtom::Constant(f)],
        },
    );
    let byte = builder.constant(Constant::Byte(7));
    let masked = builder.let_value(
        None,
        Rhs::Operation {
            operation: Operation::NatToByte,
            operands: vec![ErasedAtom::Value(both)],
        },
    );
    let compared = builder.let_value(
        None,
        Rhs::Operation {
            operation: Operation::ByteEql,
            operands: vec![ErasedAtom::Value(masked), ErasedAtom::Constant(byte)],
        },
    );
    let entry = builder.seal_block(Terminator::Return(ErasedAtom::Value(compared)));
    builder.set_entry(entry);
    let module = builder.finalize().expect("verifies");

    let printed = lowered(&module);
    // Bln ops ride Nat bit ops; NatToByte masks; Byte comparisons are Nat
    // comparisons. No Bln- or Byte-shaped operation survives the door.
    assert!(printed.contains("NatAnd"), "{printed}");
    assert!(printed.contains("255"), "{printed}");
    assert!(printed.contains("NatEql"), "{printed}");
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
            operands: vec![ErasedAtom::Constant(byte)],
        },
    );
    let sum = builder.let_value(
        None,
        Rhs::Operation {
            operation: Operation::NatAdd,
            operands: vec![ErasedAtom::Value(widened), ErasedAtom::Value(widened)],
        },
    );
    let entry = builder.seal_block(Terminator::Return(ErasedAtom::Value(sum)));
    builder.set_entry(entry);
    let module = builder.finalize().expect("verifies");
    let printed = lowered(&module);
    assert!(!printed.contains("ByteToNat"), "{printed}");
}

#[test]
fn a_variant_match_lowers_to_tag_dispatch() {
    let mut builder = ErsdBuilder::new();
    let family = builder.family(Some("Opt".into()));
    let none = builder.constructor(family, Some("none".into()), vec![]);
    let some = builder.constructor(family, Some("some".into()), vec![Some("x".into())]);

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
    let some_arm = builder.seal_block(Terminator::Return(ErasedAtom::Value(x)));
    let matched = builder.let_value(
        None,
        Rhs::MatchVariant {
            family,
            scrutinee: ErasedAtom::Value(value),
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
    let entry = builder.seal_block(Terminator::Return(ErasedAtom::Value(matched)));
    builder.set_entry(entry);
    let module = builder.finalize().expect("verifies");

    let printed = lowered(&module);
    // The constructor is the flat tuple (tag, payload…); dispatch reads the
    // tag with TplGet(0) and the payload with TplGet(1).
    assert!(printed.contains("TplGet(0)"), "{printed}");
    assert!(printed.contains("TplGet(1)"), "{printed}");
    assert!(printed.contains("switch"), "{printed}");
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
            operands: vec![ErasedAtom::Value(hypothesis), two],
        },
    );
    let step_block = builder.seal_block(Terminator::Return(ErasedAtom::Value(stepped)));
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
    let entry = builder.seal_block(Terminator::Return(ErasedAtom::Value(folded)));
    builder.set_entry(entry);
    let module = builder.finalize().expect("verifies");
    // Completing at all means the synthesized loop verified.
    let printed = lowered(&module);
    assert!(printed.contains("NatEql"), "{printed}");
}

#[test]
fn a_sequence_fold_reads_through_its_grain() {
    let mut builder = ErsdBuilder::new();
    builder.open_block();
    let bytes = builder.constant(Constant::Bin(
        Grain::X,
        curios_base::PackedBin::from_bytes(vec![1, 2, 3]),
    ));
    builder.open_block();
    let zero = nat(&mut builder, 0);
    let empty_block = builder.seal_block(Terminator::Return(zero));
    let element = builder.value(Some("h".into()));
    let suffix = builder.value(Some("t".into()));
    let accumulator = builder.value(Some("ih".into()));
    builder.open_block();
    let one = nat(&mut builder, 1);
    let stepped = builder.let_value(
        None,
        Rhs::Operation {
            operation: Operation::NatAdd,
            operands: vec![ErasedAtom::Value(accumulator), one],
        },
    );
    let step_block = builder.seal_block(Terminator::Return(ErasedAtom::Value(stepped)));
    let folded = builder.let_value(
        None,
        Rhs::FoldSequence {
            grain: SequenceGrain::Bin(Grain::X),
            scrutinee: ErasedAtom::Constant(bytes),
            empty: empty_block,
            step: FoldSequenceStep {
                element,
                suffix,
                accumulator,
                block: step_block,
            },
        },
    );
    let entry = builder.seal_block(Terminator::Return(ErasedAtom::Value(folded)));
    builder.set_entry(entry);
    let module = builder.finalize().expect("verifies");
    let printed = lowered(&module);
    assert!(printed.contains("BinLen"), "{printed}");
    assert!(printed.contains("BinGet"), "{printed}");
    assert!(printed.contains("BinSlice"), "{printed}");
}

#[test]
fn a_mixed_recursive_group_lowers_to_rec_init() {
    let mut builder = ErsdBuilder::new();
    let produce = builder.reserve_function();
    let consume = builder.value(Some("consume".into()));
    builder.open_block();
    let body = builder.seal_block(Terminator::Return(ErasedAtom::Value(consume)));
    builder.define_function(produce, Some("produce".into()), vec![], body);
    builder.open_block();
    let init = builder.seal_block(Terminator::Return(ErasedAtom::Function(produce)));
    let group = builder.rec_group(vec![produce], vec![(consume, init)]);
    builder.item_rec(group);
    builder.open_block();
    let entry = builder.seal_block(Terminator::Return(ErasedAtom::Value(consume)));
    builder.set_entry(entry);
    let module = builder.finalize().expect("verifies");
    let printed = lowered(&module);
    assert!(printed.contains("rec-init"), "{printed}");
}

#[test]
fn a_value_only_knot_lowers_through_cells() {
    // rec { lazy = Pair { force: fn() = lazy } } — the knot ties through a
    // compiler-internal cell: allocated, stored once, read at closure entry.
    let mut builder = ErsdBuilder::new();
    let schema = builder.product(ProductSchema {
        debug_name: Some("Lazy".into()),
        fields: vec![Some("force".into()), Some("mark".into())],
    });
    let lazy = builder.value(Some("lazy".into()));
    builder.open_block();
    let force = builder.reserve_function();
    builder.open_block();
    let force_body = builder.seal_block(Terminator::Return(ErasedAtom::Value(lazy)));
    builder.define_function(force, Some("force".into()), vec![], force_body);
    builder.let_functions(vec![force]);
    let mark = nat(&mut builder, 0);
    let boxed = builder.let_value(
        None,
        Rhs::Product {
            schema,
            fields: vec![ErasedAtom::Function(force), mark],
        },
    );
    let init = builder.seal_block(Terminator::Return(ErasedAtom::Value(boxed)));
    let group = builder.rec_group(vec![], vec![(lazy, init)]);
    builder.item_rec(group);
    builder.open_block();
    let entry = builder.seal_block(Terminator::Return(ErasedAtom::Value(lazy)));
    builder.set_entry(entry);
    let module = builder.finalize().expect("verifies");

    let printed = lowered(&module);
    assert!(printed.contains("cell"), "{printed}");
}

#[test]
fn exit_and_unreachable_lower_to_their_nodes() {
    let mut builder = ErsdBuilder::new();
    builder.open_block();
    let three = nat(&mut builder, 3);
    let entry = builder.seal_block(Terminator::Exit(three));
    builder.set_entry(entry);
    let module = builder.finalize().expect("verifies");
    let printed = lowered(&module);
    assert!(printed.contains("exit"), "{printed}");
}

#[test]
fn io_constants_ride_the_binary_carrier() {
    let mut builder = ErsdBuilder::new();
    builder.open_block();
    let stdout = builder.constant(Constant::Io(1));
    let stderr = builder.constant(Constant::Io(2));
    let same = builder.let_value(
        None,
        Rhs::Operation {
            operation: Operation::IoEql,
            operands: vec![ErasedAtom::Constant(stdout), ErasedAtom::Constant(stderr)],
        },
    );
    let entry = builder.seal_block(Terminator::Return(ErasedAtom::Value(same)));
    builder.set_entry(entry);
    let module = builder.finalize().expect("verifies");
    let printed = lowered(&module);
    assert!(printed.contains("BinEql"), "{printed}");
}
