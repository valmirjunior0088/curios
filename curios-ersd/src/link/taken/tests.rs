//! The fragment linked last taken back out of a program, as it was linked and after its items were evaluated where it stood.

use {
    crate::{test_support::shape, *},
    curios_num::Natural,
};

fn nat(builder: &mut ErsdBuilder, value: u32) -> Atom {
    Atom::Constant(builder.constant(Constant::Nat(Natural::from(value))))
}

/// A fragment defining `adder`, a function of one parameter that hands back a function adding it to its argument, and the function's identity in it.
fn adder() -> (Module, FunctionId) {
    let mut builder = ErsdBuilder::new();
    let outer = builder.reserve_function();
    let inner = builder.reserve_function();
    let n = builder.value(Some("n".into()));
    let m = builder.value(Some("m".into()));
    builder.open_block();
    builder.open_block();
    let sum = builder.let_value(
        None,
        Rhs::Operation {
            operation: Operation::NatAdd,
            operands: vec![Atom::Value(n), Atom::Value(m)],
        },
    );
    let inner_body = builder.seal_block(Terminator::Return(Atom::Value(sum)));
    builder.define_function(inner, Some("adder/1".into()), vec![m], inner_body);
    builder.let_functions(vec![inner]);
    let outer_body = builder.seal_block(Terminator::Return(Atom::Function(inner)));
    builder.define_function(outer, Some("adder".into()), vec![n], outer_body);
    builder.item_functions(vec![outer]);

    (builder.into_module().expect("the fragment verifies"), outer)
}

/// A fragment whose one item applies an imported function of one parameter, under `interface`, to `1`, and the value the item binds.
fn applying(interface: Interface) -> (Module, ValueId) {
    let mut builder = ErsdBuilder::new();
    let callee = builder.import_function(Some("adder".into()), 1, interface);
    let one = nat(&mut builder, 1);
    let result = builder.item_value(
        Some("result".into()),
        Rhs::Apply {
            callee: Atom::Function(callee),
            arguments: vec![one],
        },
    );

    (
        builder.into_module().expect("the fragment verifies"),
        result,
    )
}

/// `adder` and a fragment applying it linked into one program, with where the program stood between the two and where the second landed.
fn linked() -> (Module, Mark, Linked, FunctionId, Interface) {
    let (definition, function) = adder();
    let interface = definition.interface(function);
    let (applying, _) = applying(interface.clone());

    let mut program = Module::new();
    let defined = program.link(&definition, &Resolution::default());
    let function = defined.function(function);
    let mark = program.mark();
    let landed = program.link(
        &applying,
        &Resolution {
            functions: vec![function],
            ..Default::default()
        },
    );

    (program, mark, landed, function, interface)
}

#[test]
fn a_fragment_taken_back_out_is_the_fragment_that_was_linked() {
    let (program, mark, _, function, interface) = linked();
    let (applying, _) = applying(interface.clone());

    let taken = program
        .taken(&mark, &[], |_| false, |_| Some(interface.clone()))
        .expect("the suffix names one function, which is importable");
    taken
        .module
        .verify_fragment()
        .expect("what is taken is a fragment");

    assert_eq!(taken.functions, [function]);
    assert!(taken.values.is_empty());
    assert_eq!(shape(&taken.module), shape(&applying));
}

/// The item applies a function to a constant, so a round folds it: to a function of its own, the one `adder` hands back with `1` in the place of its parameter. Taken out, the fragment computes nothing and imports nothing, since the copy names no function of the program.
#[test]
fn an_item_evaluated_where_it_stands_is_taken_as_what_it_computed() {
    let (mut program, mark, landed, _, interface) = linked();
    let (_, result) = applying(interface);
    let result = landed.atom(Atom::Value(result));

    assert!(evaluate_since(&mut program, &mark));
    let taken = program
        .taken(&mark, &[result], |_| false, |_| None)
        .expect("what the rounds appended names nothing before the mark");
    taken
        .module
        .verify_fragment()
        .expect("what is taken is a fragment");

    assert!(taken.module.imports().is_empty());
    assert!(!taken.module.computes());
    assert_eq!(taken.module.function_ids().count(), 1);
    assert_eq!(
        taken.module.items().len(),
        2,
        "the copy, then the alias of it"
    );
    assert!(matches!(taken.landed.atom(result), Atom::Value(_)));
}

#[test]
fn nothing_before_the_mark_is_a_candidate() {
    let (definition, function) = adder();
    let (applying, _) = applying(definition.interface(function));

    let mut program = Module::new();
    let defined = program.link(&definition, &Resolution::default());
    program.link(
        &applying,
        &Resolution {
            functions: vec![defined.function(function)],
            ..Default::default()
        },
    );
    let before = shape(&program);
    let mark = program.mark();

    assert!(!evaluate_since(&mut program, &mark));
    assert_eq!(shape(&program), before);
}

#[test]
fn a_suffix_that_names_what_cannot_be_imported_is_not_taken() {
    let (program, mark, ..) = linked();

    assert!(program.taken(&mark, &[], |_| false, |_| None).is_none());
}
