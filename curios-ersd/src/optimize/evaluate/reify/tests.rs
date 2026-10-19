use {
    super::{ReifyScope, deep_copy_function},
    crate::*,
    curios_num::Natural,
    std::collections::{BTreeMap, BTreeSet},
};

fn add(builder: &mut ErsdBuilder, left: ValueId, right: ValueId) -> ValueId {
    builder.let_value(
        None,
        Rhs::Operation {
            operation: Operation::NatAdd,
            operands: vec![Atom::Value(left), Atom::Value(right)],
        },
    )
}

/// `outer(x)` binds `middle(y)`, which binds `inner(z) = x + y + z`: a region three functions deep, each naming what the ones around it bind.
fn nested_regions() -> (Module, [FunctionId; 3], [ValueId; 2]) {
    let mut builder = ErsdBuilder::new();
    let outer = builder.reserve_function();
    let middle = builder.reserve_function();
    let inner = builder.reserve_function();
    let x = builder.value(Some("x".into()));
    let y = builder.value(Some("y".into()));
    let z = builder.value(Some("z".into()));

    builder.open_block();
    let partial = add(&mut builder, x, y);
    let sum = add(&mut builder, partial, z);
    let inner_body = builder.seal_block(Terminator::Return(Atom::Value(sum)));
    builder.define_function(inner, Some("inner".into()), vec![z], inner_body);

    builder.open_block();
    builder.let_functions(vec![inner]);
    let middle_body = builder.seal_block(Terminator::Return(Atom::Function(inner)));
    builder.define_function(middle, Some("middle".into()), vec![y], middle_body);

    builder.open_block();
    builder.let_functions(vec![middle]);
    let outer_body = builder.seal_block(Terminator::Return(Atom::Function(middle)));
    builder.define_function(outer, Some("outer".into()), vec![x], outer_body);
    builder.item_functions(vec![outer]);

    let zero = builder.constant(Constant::Nat(Natural::from(0u32)));
    builder.open_block();
    let entry = builder.seal_block(Terminator::Return(Atom::Constant(zero)));
    builder.set_entry(entry);
    let module = builder.finalize().expect("the fixture verifies");
    (module, [outer, middle, inner], [x, y])
}

fn arena_entries(module: &Module) -> usize {
    module.functions().len()
        + module.blocks().len()
        + module.statements().len()
        + module.values().len()
        + module.rec_groups().len()
}

/// A region's weight is summed from the regions bound in it, and a copy gathers the region whole: the two count the same entries at every depth.
#[test]
fn a_region_weighs_what_copying_it_adds() {
    let (module, functions, _) = nested_regions();
    for function in functions {
        let mut copied = module.clone();
        let weight = ReifyScope::new().weigh(&copied, function);
        let before = arena_entries(&copied);
        deep_copy_function(&mut copied, function, &BTreeMap::new(), None)
            .expect("the region copies");

        assert_eq!(weight, Some(arena_entries(&copied) - before));
    }
}

/// What a region names from outside is what the functions around it bind, never what it binds itself, however deep the binding.
#[test]
fn a_nested_region_names_what_encloses_it() {
    let (module, [outer, middle, inner], [x, y]) = nested_regions();
    let mut scope = ReifyScope::new();
    let mut free = |function| {
        scope
            .free(&module, function)
            .cloned()
            .expect("no region reaches a tombstone")
    };

    assert_eq!(free(outer), (BTreeSet::new(), BTreeSet::new()));
    assert_eq!(free(middle), (BTreeSet::new(), BTreeSet::from([x])));
    assert_eq!(free(inner), (BTreeSet::new(), BTreeSet::from([x, y])));
}
