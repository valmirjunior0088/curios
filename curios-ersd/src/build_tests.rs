use super::*;

/// let one = 1 (item); entry { let doubled = NatAdd(one, one); return doubled }
fn doubling_module() -> Result<Module, VerifyError> {
    let mut builder = ErsdBuilder::new();
    let one = builder.constant(Constant::Nat(1));
    let bound = builder.item_value(Some("one".into()), Rhs::Alias(Atom::Constant(one)));
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
    builder.finalize()
}

#[test]
fn a_representative_module_builds_and_verifies() {
    let module = doubling_module().expect("the module verifies");
    assert_eq!(module.items().len(), 1);
    assert!(module.entry().is_some());
}

#[test]
fn a_recursive_function_builds_through_reserve_and_define() {
    let mut builder = ErsdBuilder::new();
    let function = builder.reserve_function();

    // fn loop(n) = switch n { 0 => 0, _ => loop(NatSub(n, 1)) }
    let n = builder.value(Some("n".into()));
    let zero = builder.constant(Constant::Nat(0));
    let one = builder.constant(Constant::Nat(1));
    builder.open_block();
    let zero_case = builder.seal_block(Terminator::Return(Atom::Constant(zero)));
    builder.open_block();
    let predecessor = builder.let_value(
        None,
        Rhs::Operation {
            operation: Operation::NatSub,
            operands: vec![Atom::Value(n), Atom::Constant(one)],
        },
    );
    let recur = builder.let_value(
        None,
        Rhs::Apply {
            callee: Atom::Function(function),
            arguments: vec![Atom::Value(predecessor)],
        },
    );
    let default = builder.seal_block(Terminator::Return(Atom::Value(recur)));
    builder.open_block();
    let result = builder.let_value(
        None,
        Rhs::SwitchNat {
            scrutinee: Atom::Value(n),
            cases: vec![NatCase {
                key: 0,
                block: zero_case,
            }],
            default,
        },
    );
    let body = builder.seal_block(Terminator::Return(Atom::Value(result)));
    builder.define_function(function, Some("loop".into()), vec![n], body);
    builder.item_functions(vec![function]);

    builder.open_block();
    let ten = builder.constant(Constant::Nat(10));
    let run = builder.let_value(
        None,
        Rhs::Apply {
            callee: Atom::Function(function),
            arguments: vec![Atom::Constant(ten)],
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(run)));
    builder.set_entry(entry);

    let module = builder.finalize().expect("the module verifies");
    assert_eq!(
        module.function(function).unwrap().debug_name.as_deref(),
        Some("loop")
    );
}

#[test]
fn a_mixed_recursive_group_builds() {
    let mut builder = ErsdBuilder::new();

    // rec { fn produce() = consume; consume = produce() dormant knot }
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

    let module = builder.finalize().expect("the module verifies");
    let group = module.rec_group(group).unwrap();
    assert_eq!(group.functions, vec![produce]);
    assert_eq!(group.values[0].value, consume);
}

#[test]
fn finalize_rejects_a_dangling_reservation() {
    let mut builder = ErsdBuilder::new();
    builder.reserve_function();
    builder.open_block();
    let unit = builder.constant(Constant::Unit);
    let entry = builder.seal_block(Terminator::Return(Atom::Constant(unit)));
    builder.set_entry(entry);
    let error = builder
        .finalize()
        .expect_err("a reservation must be defined");
    assert!(error.0.contains("reserved but never defined"), "{error}");
}

#[test]
fn finalize_rejects_an_unsealed_block() {
    let mut builder = ErsdBuilder::new();
    builder.open_block();
    let error = builder.finalize().expect_err("open blocks must be sealed");
    assert!(error.0.contains("unsealed"), "{error}");
}

#[test]
fn finalize_rejects_a_missing_entry() {
    let builder = ErsdBuilder::new();
    let error = builder.finalize().expect_err("the entry block is required");
    assert!(error.0.contains("no entry"), "{error}");
}
