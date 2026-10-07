//! Fragments linked into programs: what an import stands for, what is interned, and what a fragment is held to through an interface.

use {
    crate::{test_support::shape, *},
    curios_num::Natural,
};

fn nat(builder: &mut ErsdBuilder, value: u32) -> Atom {
    Atom::Constant(builder.constant(Constant::Nat(Natural::from(value))))
}

/// A fragment defining `successor`, a function adding one to its argument, and the function's identity in it.
fn successor() -> (Module, FunctionId) {
    let mut builder = ErsdBuilder::new();
    let function = builder.reserve_function();
    let n = builder.value(Some("n".into()));
    let one = nat(&mut builder, 1);
    builder.open_block();
    let sum = builder.let_value(
        None,
        Rhs::Operation {
            operation: Operation::NatAdd,
            operands: vec![Atom::Value(n), one],
        },
    );
    let body = builder.seal_block(Terminator::Return(Atom::Value(sum)));
    builder.define_function(function, Some("successor".into()), vec![n], body);
    builder.item_functions(vec![function]);

    (
        builder.into_module().expect("the fragment verifies"),
        function,
    )
}

/// A fragment whose entry applies an imported function of one parameter, under `interface`, to `1`.
fn caller(interface: Interface) -> Module {
    let mut builder = ErsdBuilder::new();
    let callee = builder.import_function(Some("successor".into()), 1, interface);
    let one = nat(&mut builder, 1);
    builder.open_block();
    let result = builder.let_value(
        Some("result".into()),
        Rhs::Apply {
            callee: Atom::Function(callee),
            arguments: vec![one],
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(result)));
    builder.set_entry(entry);

    builder.into_module().expect("the fragment verifies")
}

/// The same program built as one module, for a linked one to be held to.
fn whole() -> Module {
    let mut builder = ErsdBuilder::new();
    let function = builder.reserve_function();
    let n = builder.value(Some("n".into()));
    let one = nat(&mut builder, 1);
    builder.open_block();
    let sum = builder.let_value(
        None,
        Rhs::Operation {
            operation: Operation::NatAdd,
            operands: vec![Atom::Value(n), one],
        },
    );
    let body = builder.seal_block(Terminator::Return(Atom::Value(sum)));
    builder.define_function(function, Some("successor".into()), vec![n], body);
    builder.item_functions(vec![function]);
    builder.open_block();
    let result = builder.let_value(
        Some("result".into()),
        Rhs::Apply {
            callee: Atom::Function(function),
            arguments: vec![one],
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(result)));
    builder.set_entry(entry);

    builder.finalize().expect("the program verifies")
}

#[test]
fn a_linked_program_is_the_program_built_as_one_module() {
    let (definition, function) = successor();
    let caller = caller(definition.interface(function));

    let mut program = Module::new();
    let linked = program.link(&definition, &Resolution::default());
    program.link(
        &caller,
        &Resolution {
            functions: vec![linked.function(function)],
            ..Default::default()
        },
    );
    program.verify().expect("the linked program verifies");

    assert!(program.imports().is_empty());
    assert_eq!(shape(&program), shape(&whole()));
}

#[test]
fn a_fragment_with_imports_verifies_alone_and_is_no_program() {
    let caller = caller(Interface::default());

    assert_eq!(caller.imports().functions().len(), 1);
    assert!(matches!(
        caller.structure_fault(Entry::Required),
        Some(StructuralFault {
            rule: StructuralRule::UnresolvedImport,
            ..
        })
    ));
}

#[test]
fn a_constant_two_fragments_spell_is_one_in_the_program() {
    let (definition, function) = successor();
    let caller = caller(definition.interface(function));

    let mut program = Module::new();
    let linked = program.link(&definition, &Resolution::default());
    program.link(
        &caller,
        &Resolution {
            functions: vec![linked.function(function)],
            ..Default::default()
        },
    );

    assert_eq!(program.constants().len(), 1);
}

/// A fragment constructing a pair over a schema of its own, bound as its one item.
fn pair() -> (Module, ProductId) {
    let mut builder = ErsdBuilder::new();
    let schema = builder.product(ProductSchema {
        debug_name: Some("Pair".into()),
        fields: vec![Field::opaque(None), Field::opaque(None)],
        shared: false,
    });
    let one = nat(&mut builder, 1);
    builder.item_value(
        None,
        Rhs::Product {
            schema,
            fields: vec![one, one],
        },
    );

    (
        builder.into_module().expect("the fragment verifies"),
        schema,
    )
}

#[test]
fn a_schema_is_registered_once_where_the_resolution_says_it_is_held() {
    let (first, schema) = pair();
    let (second, again) = pair();

    let mut program = Module::new();
    let linked = program.link(&first, &Resolution::default());
    program.link(
        &second,
        &Resolution {
            products: [(again, linked.product(schema))].into(),
            ..Default::default()
        },
    );
    assert_eq!(program.products().len(), 1);

    // Unresolved, the second fragment's schema is another: two structures of one shape are two schemas.
    let mut apart = Module::new();
    apart.link(&first, &Resolution::default());
    apart.link(&second, &Resolution::default());
    assert_eq!(apart.products().len(), 2);
}

#[test]
fn a_fragments_rows_are_linked_without_its_items() {
    let (fragment, schema) = pair();

    let mut program = Module::new();
    let linked = program.link_rows(&fragment, &Resolution::default());

    assert_eq!(program.products().len(), 1);
    assert_eq!(linked.product(schema), ProductId(0));
    assert!(program.items().is_empty());
    assert!(program.constants().is_empty());
}

#[test]
fn an_imported_value_stands_for_the_atom_it_resolves_to() {
    let mut defining = ErsdBuilder::new();
    let one = nat(&mut defining, 1);
    let bound = defining.item_value(
        Some("one".into()),
        Rhs::Operation {
            operation: Operation::NatAdd,
            operands: vec![one, one],
        },
    );
    let defining = defining.into_module().expect("the fragment verifies");

    let mut using = ErsdBuilder::new();
    let imported = using.import_value(Some("one".into()));
    using.open_block();
    let entry = using.seal_block(Terminator::Return(Atom::Value(imported)));
    using.set_entry(entry);
    let using = using.into_module().expect("the fragment verifies");

    let mut program = Module::new();
    let linked = program.link(&defining, &Resolution::default());
    let resolved = linked.atom(Atom::Value(bound));
    program.link(
        &using,
        &Resolution {
            values: vec![resolved],
            ..Default::default()
        },
    );
    program.verify().expect("the linked program verifies");

    let entry = program.entry().expect("the program has an entry");
    let block = program.block(entry).expect("the entry is live");
    assert!(matches!(&block.terminator, Terminator::Return(atom) if *atom == resolved));
}

/// A fragment whose one item is a computed member initialized by applying an imported function of no parameter, under `interface`.
fn initialized_through(interface: Interface) -> Result<Module, VerifyError> {
    let mut builder = ErsdBuilder::new();
    let callee = builder.import_function(Some("callee".into()), 0, interface);
    let member = builder.value(Some("member".into()));
    builder.open_block();
    let result = builder.let_value(
        None,
        Rhs::Apply {
            callee: Atom::Function(callee),
            arguments: Vec::new(),
        },
    );
    let init = builder.seal_block(Terminator::Return(Atom::Value(result)));
    let group = builder.rec_group(Vec::new(), vec![(member, init)]);
    builder.item_rec(group);

    builder.into_module()
}

#[test]
fn an_initializer_is_refused_for_the_effect_an_imported_callee_performs() {
    initialized_through(Interface::default()).expect("a callee that performs nothing is admitted");

    let refused = initialized_through(Interface {
        performs: true,
        applies: Vec::new(),
    });
    assert!(matches!(
        refused,
        Err(VerifyError::InitializerPerformsEffect { .. })
    ));
}

#[test]
fn a_function_that_applies_its_parameter_says_so_in_its_interface() {
    let mut builder = ErsdBuilder::new();
    let function = builder.reserve_function();
    let callback = builder.value(Some("callback".into()));
    let ignored = builder.value(Some("ignored".into()));
    builder.open_block();
    let result = builder.let_value(
        None,
        Rhs::Apply {
            callee: Atom::Value(callback),
            arguments: Vec::new(),
        },
    );
    let body = builder.seal_block(Terminator::Return(Atom::Value(result)));
    builder.define_function(function, Some("call".into()), vec![ignored, callback], body);
    builder.item_functions(vec![function]);
    let module = builder.into_module().expect("the fragment verifies");

    assert_eq!(
        module.interface(function),
        Interface {
            performs: false,
            applies: vec![1],
        }
    );
}

#[test]
fn only_an_item_that_could_be_observable_computes() {
    let (definition, _) = successor();
    assert!(!definition.computes());

    let (constructed, _) = pair();
    assert!(!constructed.computes());

    let mut applying = ErsdBuilder::new();
    let callee = applying.import_function(None, 0, Interface::default());
    applying.item_value(
        None,
        Rhs::Apply {
            callee: Atom::Function(callee),
            arguments: Vec::new(),
        },
    );
    let applying = applying.into_module().expect("the fragment verifies");
    assert!(applying.computes());
}
