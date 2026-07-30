use crate::*;

fn doubling_module() -> Module {
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
    builder.finalize().expect("the module verifies")
}

#[test]
fn use_counts_are_exact() {
    let module = doubling_module();
    let analysis = Analysis::analyze(&module);
    let one = ValueId(0);
    let doubled = ValueId(1);
    assert_eq!(analysis.value_uses(one), 2, "both NatAdd operands");
    assert_eq!(analysis.value_uses(doubled), 1, "the entry terminator");
}

/// outer(p) { x = alias glob; functions inner; return inner } inner() { s = NatAdd(x, p); return s } Free values: inner frees x and p; outer frees only glob (x and p are bound in outer, and inner's frees propagate into outer before subtraction).
#[test]
fn free_values_derive_transitively_through_nested_functions() {
    let mut builder = ErsdBuilder::new();
    let zero = builder.constant(Constant::Nat(0));
    let glob = builder.item_value(Some("glob".into()), Rhs::Alias(Atom::Constant(zero)));

    let outer = builder.reserve_function();
    let inner = builder.reserve_function();
    let p = builder.value(Some("p".into()));

    builder.open_block();
    let x = builder.let_value(Some("x".into()), Rhs::Alias(Atom::Value(glob)));

    builder.open_block();
    let s = builder.let_value(
        None,
        Rhs::Operation {
            operation: Operation::NatAdd,
            operands: vec![Atom::Value(x), Atom::Value(p)],
        },
    );
    let inner_body = builder.seal_block(Terminator::Return(Atom::Value(s)));
    builder.define_function(inner, Some("inner".into()), vec![], inner_body);
    builder.let_functions(vec![inner]);

    let outer_body = builder.seal_block(Terminator::Return(Atom::Function(inner)));
    builder.define_function(outer, Some("outer".into()), vec![p], outer_body);
    builder.item_functions(vec![outer]);

    builder.open_block();
    let entry = builder.seal_block(Terminator::Return(Atom::Function(outer)));
    builder.set_entry(entry);
    let module = builder.finalize().expect("the module verifies");

    let analysis = Analysis::analyze(&module);
    assert_eq!(
        analysis
            .free_values(inner)
            .iter()
            .copied()
            .collect::<Vec<_>>(),
        vec![p, x],
        "inner frees the outer parameter and the outer local, in identity order"
    );
    assert_eq!(
        analysis
            .free_values(outer)
            .iter()
            .copied()
            .collect::<Vec<_>>(),
        vec![glob],
        "outer frees only the top-level value"
    );
}

/// Recursion through nesting is visible in the reference graph: outer binds inner (edge) and inner calls outer (edge), so they form one recursive component.
#[test]
fn nesting_recursion_forms_one_component() {
    let mut builder = ErsdBuilder::new();
    let outer = builder.reserve_function();
    let inner = builder.reserve_function();

    builder.open_block();
    builder.open_block();
    let call = builder.let_value(
        None,
        Rhs::Apply {
            callee: Atom::Function(outer),
            arguments: vec![],
        },
    );
    let inner_body = builder.seal_block(Terminator::Return(Atom::Value(call)));
    builder.define_function(inner, Some("inner".into()), vec![], inner_body);
    builder.let_functions(vec![inner]);
    let outer_body = builder.seal_block(Terminator::Return(Atom::Function(inner)));
    builder.define_function(outer, Some("outer".into()), vec![], outer_body);
    builder.item_functions(vec![outer]);

    builder.open_block();
    let entry = builder.seal_block(Terminator::Return(Atom::Function(outer)));
    builder.set_entry(entry);
    let module = builder.finalize().expect("the module verifies");

    let analysis = Analysis::analyze(&module);
    assert!(analysis.references(outer).contains(&inner), "binding edge");
    assert!(analysis.references(inner).contains(&outer), "call edge");
    let component = analysis.component_of(outer).unwrap();
    assert_eq!(analysis.component_of(inner), Some(component));
    assert!(analysis.is_recursive(component));
}

#[test]
fn components_distinguish_recursion_from_plain_calls() {
    let mut builder = ErsdBuilder::new();
    let looping = builder.reserve_function();
    let plain = builder.reserve_function();

    // fn looping() = looping()
    builder.open_block();
    let recur = builder.let_value(
        None,
        Rhs::Apply {
            callee: Atom::Function(looping),
            arguments: vec![],
        },
    );
    let looping_body = builder.seal_block(Terminator::Return(Atom::Value(recur)));
    builder.define_function(looping, Some("looping".into()), vec![], looping_body);
    builder.item_functions(vec![looping]);

    // fn plain() = looping()
    builder.open_block();
    let call = builder.let_value(
        None,
        Rhs::Apply {
            callee: Atom::Function(looping),
            arguments: vec![],
        },
    );
    let plain_body = builder.seal_block(Terminator::Return(Atom::Value(call)));
    builder.define_function(plain, Some("plain".into()), vec![], plain_body);
    builder.item_functions(vec![plain]);

    builder.open_block();
    let entry = builder.seal_block(Terminator::Return(Atom::Function(plain)));
    builder.set_entry(entry);
    let module = builder.finalize().expect("the module verifies");

    let analysis = Analysis::analyze(&module);
    let looping_component = analysis.component_of(looping).unwrap();
    let plain_component = analysis.component_of(plain).unwrap();
    assert_ne!(looping_component, plain_component);
    assert!(analysis.is_recursive(looping_component), "self-call");
    assert!(!analysis.is_recursive(plain_component), "plain call");
    assert!(
        looping_component < plain_component,
        "condensation order is reverse topological: callee first"
    );
}

#[test]
fn analysis_is_deterministic() {
    let module = doubling_module();
    assert_eq!(Analysis::analyze(&module), Analysis::analyze(&module));
}

/// Analyzing a deep module costs heap, not native stack.
#[test]
fn a_deep_module_analyzes_without_native_stack() {
    let mut builder = ErsdBuilder::new();
    let zero = builder.constant(Constant::Nat(0));
    let scrutinee = builder.item_value(Some("scrutinee".into()), Rhs::Alias(Atom::Constant(zero)));
    builder.open_block();
    let mut chain = builder.seal_block(Terminator::Return(Atom::Value(scrutinee)));
    for _ in 0..50_000 {
        builder.open_block();
        let leaf = builder.seal_block(Terminator::Return(Atom::Value(scrutinee)));
        builder.open_block();
        let switched = builder.let_value(
            None,
            Rhs::SwitchNat {
                scrutinee: Atom::Value(scrutinee),
                cases: vec![NatCase {
                    key: 0,
                    block: leaf,
                }],
                default: chain,
            },
        );
        chain = builder.seal_block(Terminator::Return(Atom::Value(switched)));
    }
    builder.set_entry(chain);
    let module = builder.finalize().expect("the deep module verifies");

    let analysis = Analysis::analyze(&module);
    assert_eq!(
        analysis.value_uses(scrutinee),
        50_000 * 2 + 1,
        "one scrutinee use and one leaf return per level, plus the innermost return"
    );
}
