use {
    crate::*,
    curios_base::{Flt, Grain, PackedBin},
};

#[test]
fn constants_intern_by_exact_bitwise_identity() {
    let mut module = Module::new();
    let a = module.intern_constant(Constant::Nat(7));
    let b = module.intern_constant(Constant::Nat(7));
    assert_eq!(a, b);
    assert_ne!(a, module.intern_constant(Constant::Nat(8)));
    assert_ne!(
        module.intern_constant(Constant::Nat(0)),
        module.intern_constant(Constant::Int(0)),
        "shapes are distinct even at equal bits"
    );
    assert_eq!(module.constants().len(), 4);
}

#[test]
fn float_constants_intern_by_bit_pattern() {
    let mut module = Module::new();
    let positive = module.intern_constant(Constant::Flt(Flt::from_f32(0.0)));
    let negative = module.intern_constant(Constant::Flt(Flt::from_f32(-0.0)));
    assert_ne!(positive, negative, "signed zeros are distinct constants");
    let nan = module.intern_constant(Constant::Flt(Flt::from_f32(f32::NAN)));
    assert_eq!(
        nan,
        module.intern_constant(Constant::Flt(Flt::from_f32(f32::NAN))),
        "an identical NaN bit pattern shares one identity"
    );
}

#[test]
fn binary_constants_intern_by_logical_bits_across_windows() {
    let mut module = Module::new();
    let direct = PackedBin::from_bits([true, false, true]);
    let framed = PackedBin::from_bits([false, true, false, true, true])
        .window(1, 3)
        .unwrap();
    let a = module.intern_constant(Constant::Bin(Grain::B, direct));
    let b = module.intern_constant(Constant::Bin(Grain::B, framed));
    assert_eq!(a, b, "equal logical bits share one identity");
}

#[test]
fn a_hand_built_module_round_trips_through_its_accessors() {
    let mut module = Module::new();

    // let one = 1; let doubled = NatAdd(one, one); return doubled
    let one = module.intern_constant(Constant::Nat(1));
    let bound = module.add_value(Some("one".into()));
    let alias = module.add_statement(Statement::Let {
        result: bound,
        rhs: Rhs::Alias(Atom::Constant(one)),
    });
    let doubled = module.add_value(Some("doubled".into()));
    let add = module.add_statement(Statement::Let {
        result: doubled,
        rhs: Rhs::Operation {
            operation: Operation::NatAdd,
            operands: vec![Atom::Value(bound), Atom::Value(bound)],
        },
    });
    module.push_item(alias);
    let entry = module.add_block(Block {
        statements: vec![add],
        terminator: Terminator::Return(Atom::Value(doubled)),
    });
    module.set_entry(entry);

    assert_eq!(module.items(), &[alias]);
    assert_eq!(module.entry(), Some(entry));
    let statement = module.statement(add).unwrap();
    let Statement::Let { result, .. } = statement else {
        panic!("expected a let statement");
    };
    assert_eq!(*result, doubled);
    assert_eq!(
        module.value(bound).unwrap().debug_name.as_deref(),
        Some("one")
    );
}

#[test]
fn functions_reserve_before_they_define() {
    let mut module = Module::new();
    let function = module.reserve_function();
    assert!(module.function(function).is_none(), "reserved, not defined");

    // fn self(n) = self(n) — the body references the reserved identity.
    let n = module.add_value(Some("n".into()));
    let result = module.add_value(None);
    let call = module.add_statement(Statement::Let {
        result,
        rhs: Rhs::Apply {
            callee: Atom::Function(function),
            arguments: vec![Atom::Value(n)],
        },
    });
    let body = module.add_block(Block {
        statements: vec![call],
        terminator: Terminator::Return(Atom::Value(result)),
    });
    module.define_function(
        function,
        Function {
            debug_name: Some("self".into()),
            params: vec![n],
            body,
        },
    );
    assert_eq!(module.function(function).unwrap().params, vec![n]);
}

#[test]
fn constructors_register_in_discriminant_order() {
    let mut module = Module::new();
    let family = module.add_family(Some("Shape".into()));
    let circle = module.add_constructor(family, Some("circle".into()), vec![Some("radius".into())]);
    let square = module.add_constructor(family, Some("square".into()), vec![Some("side".into())]);
    assert_eq!(
        module.family(family).unwrap().constructors,
        vec![circle, square]
    );
    assert_eq!(module.constructor(square).unwrap().family, family);
    assert_eq!(module.constructor(circle).unwrap().width(), 1);
}

#[test]
fn identities_mint_monotonically_per_arena() {
    let mut module = Module::new();
    let first = module.add_value(None);
    let second = module.add_value(None);
    assert_eq!(first.index(), 0);
    assert_eq!(second.index(), 1);
    let statement = module.add_statement(Statement::Let {
        result: first,
        rhs: Rhs::Alias(Atom::Value(second)),
    });
    assert_eq!(statement.index(), 0, "arenas mint independently");
}

#[test]
fn operation_arities_are_exact() {
    assert_eq!(Operation::NatAdd.arity(), 2);
    assert_eq!(Operation::FltSqrt.arity(), 1);
    assert_eq!(Operation::HandleEql.arity(), 2);
    assert_eq!(
        SequenceOp::BinSlice(Grain::X).arity(),
        SequenceArity::Exactly(3)
    );
    assert_eq!(SequenceOp::LstConcat.arity(), SequenceArity::AnyCount);
    assert_eq!(CellOperation::Set.arity(), 2);
    assert_eq!(Intrinsic::LstMap.arity(), 2);
}
