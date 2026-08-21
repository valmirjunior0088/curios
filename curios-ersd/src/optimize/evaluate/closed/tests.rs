use {super::evaluate_closed_terms, crate::*};

/// `make(x) = (y) => x + y`, bound as an item.
///
/// Applying it folds to a closure over `x`, so reifying the result deep-copies `inner` with the capture substituted — the shape whose duplication the reification memo exists to remove.
fn define_maker(builder: &mut ErsdBuilder) -> FunctionId {
    let make = builder.reserve_function();
    let inner = builder.reserve_function();
    let x = builder.value(Some("x".into()));
    let y = builder.value(Some("y".into()));

    builder.open_block();
    let sum = builder.let_value(
        None,
        Rhs::Operation {
            operation: Operation::NatAdd,
            operands: vec![Atom::Value(x), Atom::Value(y)],
        },
    );
    let inner_body = builder.seal_block(Terminator::Return(Atom::Value(sum)));
    builder.define_function(inner, Some("inner".into()), vec![y], inner_body);

    builder.open_block();
    builder.let_functions(vec![inner]);
    let make_body = builder.seal_block(Terminator::Return(Atom::Function(inner)));
    builder.define_function(make, Some("make".into()), vec![x], make_body);

    builder.item_functions(vec![make]);
    make
}

/// A function bound by its own item whose body applies `make` to a literal — one block-owned candidate.
fn define_host(builder: &mut ErsdBuilder, make: FunctionId, one: ConstantId, name: &str) {
    let host = builder.reserve_function();
    builder.open_block();
    let applied = builder.let_value(
        None,
        Rhs::Apply {
            callee: Atom::Function(make),
            arguments: vec![Atom::Constant(one)],
        },
    );
    let body = builder.seal_block(Terminator::Return(Atom::Value(applied)));
    builder.define_function(host, Some(name.into()), vec![], body);
    builder.item_functions(vec![host]);
}

/// Two candidates, each inside a block of its own item, folding to one specialization.
fn two_block_candidates() -> Module {
    let mut builder = ErsdBuilder::new();
    let make = define_maker(&mut builder);
    let one = builder.constant(Constant::Nat(1));
    define_host(&mut builder, make, one, "first");
    define_host(&mut builder, make, one, "second");

    let zero = builder.constant(Constant::Nat(0));
    builder.open_block();
    let entry = builder.seal_block(Terminator::Return(Atom::Constant(zero)));
    builder.set_entry(entry);
    builder.finalize().expect("the fixture verifies")
}

fn live_functions(module: &Module) -> usize {
    module.functions().iter().flatten().count()
}

/// One copy serves both candidates, though neither stands at item level.
///
/// **The whole of what let-insertion buys, at the smallest size that shows it.** Both candidates fold to a closure over the same capture and so reach one specialization; the copy is bound ahead of the item enclosing the first, where the second can see it. Bound inside the first candidate's own block it would be in scope for nothing else, and the second would copy `inner` again — which is the `n² + 2` this pass used to produce on a grammar, one re-materialization per definition reaching the chain.
///
/// Counted in live functions because that is what a copy costs the module: `make`, `inner`, the two hosts, and *one* specialization.
///
/// Mutation-checked two ways, and the second is the more interesting. Withholding the position from a block-owned candidate — the behaviour before let-insertion — makes the second candidate copy `inner` again and the delta two. Keeping the position while splicing back into the candidate's own block does not merely lose the sharing: it panics in `Module::verify` with `statement ~s5 references ~f4 out of scope`, because the second candidate then names a function bound in a block that does not dominate it. **The position and the placement are one decision, and the verifier is what says so** — which is the backstop the reification memo has always rested on, now with a second dependent.
#[test]
fn one_copy_serves_two_block_candidates() {
    let mut module = two_block_candidates();
    let before = live_functions(&module);

    assert!(evaluate_closed_terms(&mut module), "both candidates fold");

    assert_eq!(
        live_functions(&module) - before,
        1,
        "expected a single shared specialization:\n{module}"
    );
}

/// The copy is bound at item level, not in the block whose candidate asked for it.
///
/// **The placement is what makes the sharing legal rather than merely desirable.** `Module::verify` treats the top level as the items in order followed by the entry block, with each item's bindings ambient afterwards, so a copy bound ahead of an item is in scope at every candidate after it — and a copy bound inside a block that need not dominate anything is not. This asserts the placement directly, because the count above would also be met by a copy reached twice from within one replacement.
#[test]
fn a_block_candidates_copy_is_bound_at_item_level() {
    let mut module = two_block_candidates();
    let items_before = module.items().len();

    assert!(evaluate_closed_terms(&mut module), "both candidates fold");

    assert_eq!(
        module.items().len() - items_before,
        1,
        "expected one item-level binding:\n{module}"
    );

    let bound_at_item_level = module
        .items()
        .iter()
        .filter_map(|&item| match module.statement(item) {
            Some(Statement::Functions { functions }) => Some(functions.len()),
            _ => None,
        })
        .sum::<usize>();
    assert_eq!(
        bound_at_item_level, 4,
        "make, the two hosts, and the shared copy:\n{module}"
    );
}
