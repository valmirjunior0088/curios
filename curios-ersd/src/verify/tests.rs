use crate::*;

fn nat_atom(builder: &mut ErsdBuilder, value: u32) -> Atom {
    let constant = builder.constant(Constant::Nat(value));
    Atom::Constant(constant)
}

/// The `Async/join_all` idiom: a recursive group function called by the computed member's eager initializer. The first run's verifier over-rejected this shape; it is a supported program.
#[test]
fn a_join_all_shaped_knot_verifies() {
    let mut builder = ErsdBuilder::new();

    // rec { fn go(i) = go(i); result = go(0) }
    let go = builder.reserve_function();
    let i = builder.value(Some("i".into()));
    builder.open_block();
    let recur = builder.let_value(
        None,
        Rhs::Apply {
            callee: Atom::Function(go),
            arguments: vec![Atom::Value(i)],
        },
    );
    let body = builder.seal_block(Terminator::Return(Atom::Value(recur)));
    builder.define_function(go, Some("go".into()), vec![i], body);

    let result = builder.value(Some("result".into()));
    builder.open_block();
    let zero = nat_atom(&mut builder, 0);
    let call = builder.let_value(
        None,
        Rhs::Apply {
            callee: Atom::Function(go),
            arguments: vec![zero],
        },
    );
    let init = builder.seal_block(Terminator::Return(Atom::Value(call)));
    let group = builder.rec_group(vec![go], vec![(result, init)]);
    builder.item_rec(group);

    builder.open_block();
    let entry = builder.seal_block(Terminator::Return(Atom::Value(result)));
    builder.set_entry(entry);
    builder.finalize().expect("the join_all shape is supported");
}

/// The self-referential lazy value: a value-only group whose knot closes through a closure constructed by the initializer. The reference to the member under initialization is dormant, so it is admitted; the lowering knots it through a cell.
#[test]
fn a_self_referential_lazy_value_verifies() {
    let mut builder = ErsdBuilder::new();
    let schema = builder.product(ProductSchema {
        debug_name: Some("Lazy".into()),
        fields: vec![Field::opaque(Some("force".into()))],
        shared: false,
    });

    // rec { lazy = Lazy { force: fn() = lazy } }
    let lazy = builder.value(Some("lazy".into()));
    builder.open_block();
    let force = builder.reserve_function();
    builder.open_block();
    let force_body = builder.seal_block(Terminator::Return(Atom::Value(lazy)));
    builder.define_function(force, Some("force".into()), vec![], force_body);
    builder.let_functions(vec![force]);
    let boxed = builder.let_value(
        None,
        Rhs::Product {
            schema,
            fields: vec![Atom::Function(force)],
        },
    );
    let init = builder.seal_block(Terminator::Return(Atom::Value(boxed)));
    let group = builder.rec_group(vec![], vec![(lazy, init)]);
    builder.item_rec(group);

    builder.open_block();
    let entry = builder.seal_block(Terminator::Return(Atom::Value(lazy)));
    builder.set_entry(entry);
    builder
        .finalize()
        .expect("the self-referential lazy value is supported");
}

/// A later computed member may evaluate an earlier one; only forward and self evaluation are unsatisfiable.
#[test]
fn a_backward_computed_reference_verifies() {
    let mut builder = ErsdBuilder::new();
    let first = builder.value(Some("first".into()));
    let second = builder.value(Some("second".into()));
    builder.open_block();
    let one = nat_atom(&mut builder, 1);
    let init_first = builder.seal_block(Terminator::Return(one));
    builder.open_block();
    let init_second = builder.seal_block(Terminator::Return(Atom::Value(first)));
    let group = builder.rec_group(vec![], vec![(first, init_first), (second, init_second)]);
    builder.item_rec(group);
    builder.open_block();
    let entry = builder.seal_block(Terminator::Return(Atom::Value(second)));
    builder.set_entry(entry);
    builder
        .finalize()
        .expect("backward evaluation is supported");
}

/// A forward reference reached through a call is an evaluation, not a dormant reference: `rec { table = build(0); fn build(n) = n + size; size = 1 }` runs `build` inside `table`'s initializer, and `build` reads `size`. Forced by need, that is legal — reading `size` runs its initializer first — where it once read an unfilled cell (`1` where the language says `43`, on the `curios` probe that found it) and was then refused for a while.
#[test]
fn a_forward_reference_through_a_called_function_is_forced_first() {
    let mut builder = ErsdBuilder::new();
    let build = builder.reserve_function();
    let table = builder.value(Some("table".into()));
    let size = builder.value(Some("size".into()));

    let n = builder.value(Some("n".into()));
    builder.open_block();
    let sum = builder.let_value(
        None,
        Rhs::Operation {
            operation: Operation::NatAdd,
            operands: vec![Atom::Value(n), Atom::Value(size)],
        },
    );
    let body = builder.seal_block(Terminator::Return(Atom::Value(sum)));
    builder.define_function(build, Some("build".into()), vec![n], body);

    builder.open_block();
    let zero = nat_atom(&mut builder, 0);
    let call = builder.let_value(
        None,
        Rhs::Apply {
            callee: Atom::Function(build),
            arguments: vec![zero],
        },
    );
    let init_table = builder.seal_block(Terminator::Return(Atom::Value(call)));
    builder.open_block();
    let one = nat_atom(&mut builder, 1);
    let init_size = builder.seal_block(Terminator::Return(one));
    let group = builder.rec_group(vec![build], vec![(table, init_table), (size, init_size)]);
    builder.item_rec(group);
    builder.open_block();
    let entry = builder.seal_block(Terminator::Return(Atom::Value(table)));
    builder.set_entry(entry);
    builder
        .finalize()
        .expect("the read through the call forces `size` first");
}

/// The same knot closed into a cycle — `size = table` — is what no forcing can satisfy: `table`'s initializer evaluates `size` through `build`, and `size`'s evaluates `table`. The refusal names the path and the call.
#[test]
fn a_cycle_through_a_called_function_is_rejected() {
    let mut builder = ErsdBuilder::new();
    let build = builder.reserve_function();
    let table = builder.value(Some("table".into()));
    let size = builder.value(Some("size".into()));

    let n = builder.value(Some("n".into()));
    builder.open_block();
    let sum = builder.let_value(
        None,
        Rhs::Operation {
            operation: Operation::NatAdd,
            operands: vec![Atom::Value(n), Atom::Value(size)],
        },
    );
    let body = builder.seal_block(Terminator::Return(Atom::Value(sum)));
    builder.define_function(build, Some("build".into()), vec![n], body);

    builder.open_block();
    let zero = nat_atom(&mut builder, 0);
    let call = builder.let_value(
        None,
        Rhs::Apply {
            callee: Atom::Function(build),
            arguments: vec![zero],
        },
    );
    let init_table = builder.seal_block(Terminator::Return(Atom::Value(call)));
    builder.open_block();
    let init_size = builder.seal_block(Terminator::Return(Atom::Value(table)));
    let group = builder.rec_group(vec![build], vec![(table, init_table), (size, init_size)]);
    builder.item_rec(group);
    builder.open_block();
    let entry = builder.seal_block(Terminator::Return(Atom::Value(table)));
    builder.set_entry(entry);
    let error = builder
        .finalize()
        .expect_err("`table` evaluates itself through `build` and `size`");
    assert!(
        error.0.contains("evaluate each other") && error.0.contains("through a call"),
        "{error}"
    );
}

/// The same knot with `size` ahead of `table`, which by need makes no difference to.
#[test]
fn a_backward_reference_through_a_called_function_verifies() {
    let mut builder = ErsdBuilder::new();
    let build = builder.reserve_function();
    let size = builder.value(Some("size".into()));
    let table = builder.value(Some("table".into()));

    let n = builder.value(Some("n".into()));
    builder.open_block();
    let sum = builder.let_value(
        None,
        Rhs::Operation {
            operation: Operation::NatAdd,
            operands: vec![Atom::Value(n), Atom::Value(size)],
        },
    );
    let body = builder.seal_block(Terminator::Return(Atom::Value(sum)));
    builder.define_function(build, Some("build".into()), vec![n], body);

    builder.open_block();
    let one = nat_atom(&mut builder, 1);
    let init_size = builder.seal_block(Terminator::Return(one));
    builder.open_block();
    let zero = nat_atom(&mut builder, 0);
    let call = builder.let_value(
        None,
        Rhs::Apply {
            callee: Atom::Function(build),
            arguments: vec![zero],
        },
    );
    let init_table = builder.seal_block(Terminator::Return(Atom::Value(call)));
    let group = builder.rec_group(vec![build], vec![(size, init_size), (table, init_table)]);
    builder.item_rec(group);
    builder.open_block();
    let entry = builder.seal_block(Terminator::Return(Atom::Value(table)));
    builder.set_entry(entry);
    builder
        .finalize()
        .expect("a call reading an earlier member is supported");
}

/// The fold shape: `rec { table = each(step); fn step(n) = n + table }` where `each(f)` nests a `go` that captures `f` and applies it, and `step` reads `table` — so `table`'s initializer evaluates `table`. The stepper is handed to a combinator rather than called, and the combinator applies it only through a capture two functions down — which the summary follows, because an applied capture names a value of the scope that binds it, and that scope is `each`'s parameter list.
#[test]
fn a_stepper_handed_to_a_combinator_that_applies_it_is_evaluated_by_the_initializer() {
    let mut builder = ErsdBuilder::new();
    let each = builder.reserve_function();
    let step = builder.reserve_function();
    let table = builder.value(Some("table".into()));
    let size = builder.value(Some("size".into()));

    // each(f) = { fn go(x) = f(x); go(0) }
    let f = builder.value(Some("f".into()));
    let go = builder.reserve_function();
    let x = builder.value(Some("x".into()));
    builder.open_block();
    let applied = builder.let_value(
        None,
        Rhs::Apply {
            callee: Atom::Value(f),
            arguments: vec![Atom::Value(x)],
        },
    );
    let go_body = builder.seal_block(Terminator::Return(Atom::Value(applied)));
    builder.define_function(go, Some("go".into()), vec![x], go_body);
    builder.open_block();
    builder.let_functions(vec![go]);
    let zero = nat_atom(&mut builder, 0);
    let gone = builder.let_value(
        None,
        Rhs::Apply {
            callee: Atom::Function(go),
            arguments: vec![zero],
        },
    );
    let each_body = builder.seal_block(Terminator::Return(Atom::Value(gone)));
    builder.define_function(each, Some("each".into()), vec![f], each_body);

    // step(n) = n + table
    let n = builder.value(Some("n".into()));
    builder.open_block();
    let sum = builder.let_value(
        None,
        Rhs::Operation {
            operation: Operation::NatAdd,
            operands: vec![Atom::Value(n), Atom::Value(table)],
        },
    );
    let step_body = builder.seal_block(Terminator::Return(Atom::Value(sum)));
    builder.define_function(step, Some("step".into()), vec![n], step_body);

    builder.open_block();
    let call = builder.let_value(
        None,
        Rhs::Apply {
            callee: Atom::Function(each),
            arguments: vec![Atom::Function(step)],
        },
    );
    let init_table = builder.seal_block(Terminator::Return(Atom::Value(call)));
    builder.open_block();
    let one = nat_atom(&mut builder, 1);
    let init_size = builder.seal_block(Terminator::Return(one));
    let group = builder.rec_group(vec![step], vec![(table, init_table), (size, init_size)]);
    builder.item_functions(vec![each]);
    builder.item_rec(group);
    builder.open_block();
    let entry = builder.seal_block(Terminator::Return(Atom::Value(table)));
    builder.set_entry(entry);
    let error = builder
        .finalize()
        .expect_err("the combinator applies the stepper, which evaluates `table`");
    assert!(
        error.0.contains("evaluate each other") && error.0.contains("through a call"),
        "{error}"
    );
}

/// The bind shape: a stepper over a sibling handed to a combinator that only *stores* it — `keep(f) = Box { f }` — stays dormant, which is what keeps a parser knot built over its own members legal.
#[test]
fn a_stepper_handed_to_a_combinator_that_stores_it_stays_dormant() {
    let mut builder = ErsdBuilder::new();
    let schema = builder.product(ProductSchema {
        debug_name: Some("Box".into()),
        fields: vec![Field::opaque(Some("held".into()))],
        shared: false,
    });
    let keep = builder.reserve_function();
    let step = builder.reserve_function();
    let table = builder.value(Some("table".into()));
    let size = builder.value(Some("size".into()));

    let f = builder.value(Some("f".into()));
    builder.open_block();
    let boxed = builder.let_value(
        None,
        Rhs::Product {
            schema,
            fields: vec![Atom::Value(f)],
        },
    );
    let keep_body = builder.seal_block(Terminator::Return(Atom::Value(boxed)));
    builder.define_function(keep, Some("keep".into()), vec![f], keep_body);

    let n = builder.value(Some("n".into()));
    builder.open_block();
    let sum = builder.let_value(
        None,
        Rhs::Operation {
            operation: Operation::NatAdd,
            operands: vec![Atom::Value(n), Atom::Value(size)],
        },
    );
    let step_body = builder.seal_block(Terminator::Return(Atom::Value(sum)));
    builder.define_function(step, Some("step".into()), vec![n], step_body);

    builder.open_block();
    let call = builder.let_value(
        None,
        Rhs::Apply {
            callee: Atom::Function(keep),
            arguments: vec![Atom::Function(step)],
        },
    );
    let init_table = builder.seal_block(Terminator::Return(Atom::Value(call)));
    builder.open_block();
    let one = nat_atom(&mut builder, 1);
    let init_size = builder.seal_block(Terminator::Return(one));
    let group = builder.rec_group(vec![step], vec![(table, init_table), (size, init_size)]);
    builder.item_functions(vec![keep]);
    builder.item_rec(group);
    builder.open_block();
    let entry = builder.seal_block(Terminator::Return(Atom::Value(table)));
    builder.set_entry(entry);
    builder
        .finalize()
        .expect("a stored stepper is dormant until something applies it");
}

/// An initializer performs no effect: forced by need it runs later than it is written, or never, and an exit — or a host call, or a cell — cannot be moved that way. It is refused where it is declared.
#[test]
fn an_initializer_that_performs_an_effect_is_rejected() {
    let mut builder = ErsdBuilder::new();
    let member = builder.value(Some("member".into()));
    builder.open_block();
    let zero = nat_atom(&mut builder, 0);
    let init = builder.seal_block(Terminator::Exit(zero));
    let group = builder.rec_group(vec![], vec![(member, init)]);
    builder.item_rec(group);
    builder.open_block();
    let entry = builder.seal_block(Terminator::Return(Atom::Value(member)));
    builder.set_entry(entry);
    let error = builder
        .finalize()
        .expect_err("an initializer that exits cannot be forced by need");
    assert!(error.0.contains("performs an effect"), "{error}");
}

/// A computed-only evaluation cycle has no satisfiable initialization order.
#[test]
fn a_computed_only_cycle_is_rejected() {
    let mut builder = ErsdBuilder::new();
    let first = builder.value(Some("first".into()));
    let second = builder.value(Some("second".into()));
    builder.open_block();
    let init_first = builder.seal_block(Terminator::Return(Atom::Value(second)));
    builder.open_block();
    let init_second = builder.seal_block(Terminator::Return(Atom::Value(first)));
    let group = builder.rec_group(vec![], vec![(first, init_first), (second, init_second)]);
    builder.item_rec(group);
    builder.open_block();
    let entry = builder.seal_block(Terminator::Return(Atom::Value(second)));
    builder.set_entry(entry);
    let error = builder.finalize().expect_err("the cycle cannot initialize");
    assert!(error.0.contains("evaluate each other"), "{error}");
}

#[test]
fn a_direct_self_knot_is_admitted() {
    // `rec loop = loop` is language-accepted: unused, the lowering drops it; used, the lowering rejects it. The verifier admits the shape.
    let mut builder = ErsdBuilder::new();
    let value = builder.value(Some("value".into()));
    builder.open_block();
    let init = builder.seal_block(Terminator::Return(Atom::Value(value)));
    let group = builder.rec_group(vec![], vec![(value, init)]);
    builder.item_rec(group);
    builder.open_block();
    let zero = builder.constant(Constant::Nat(0));
    let entry = builder.seal_block(Terminator::Return(Atom::Constant(zero)));
    builder.set_entry(entry);
    builder.finalize().expect("the self-knot is admitted");
}

#[test]
fn an_unbound_value_use_is_out_of_scope() {
    let mut builder = ErsdBuilder::new();
    let unbound = builder.value(Some("unbound".into()));
    builder.open_block();
    let early = builder.let_value(None, Rhs::Alias(Atom::Value(unbound)));
    let entry = builder.seal_block(Terminator::Return(Atom::Value(early)));
    builder.set_entry(entry);
    let error = builder
        .finalize()
        .expect_err("an unbound use is out of scope");
    assert!(error.0.contains("out of scope"), "{error}");
}

#[test]
fn a_value_bound_inside_a_function_is_out_of_scope_after_it() {
    let mut builder = ErsdBuilder::new();
    let function = builder.reserve_function();
    builder.open_block();
    let one = nat_atom(&mut builder, 1);
    let local = builder.let_value(Some("local".into()), Rhs::Alias(one));
    let body = builder.seal_block(Terminator::Return(Atom::Value(local)));
    builder.define_function(function, None, vec![], body);
    builder.item_functions(vec![function]);
    builder.open_block();
    let entry = builder.seal_block(Terminator::Return(Atom::Value(local)));
    builder.set_entry(entry);
    let error = builder.finalize().expect_err("the local escaped its scope");
    assert!(error.0.contains("out of scope"), "{error}");
}

#[test]
fn an_operation_arity_mismatch_is_rejected() {
    let mut builder = ErsdBuilder::new();
    builder.open_block();
    let one = nat_atom(&mut builder, 1);
    let sum = builder.let_value(
        None,
        Rhs::Operation {
            operation: Operation::NatAdd,
            operands: vec![one],
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(sum)));
    builder.set_entry(entry);
    let error = builder.finalize().expect_err("NatAdd takes two operands");
    assert!(error.0.contains("arity"), "{error}");
}

#[test]
fn an_unsaturated_direct_application_is_rejected() {
    let mut builder = ErsdBuilder::new();
    let function = builder.reserve_function();
    let param = builder.value(None);
    builder.open_block();
    let body = builder.seal_block(Terminator::Return(Atom::Value(param)));
    builder.define_function(function, None, vec![param], body);
    builder.item_functions(vec![function]);
    builder.open_block();
    let call = builder.let_value(
        None,
        Rhs::Apply {
            callee: Atom::Function(function),
            arguments: vec![],
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(call)));
    builder.set_entry(entry);
    let error = builder
        .finalize()
        .expect_err("direct application is saturated");
    assert!(error.0.contains("arity"), "{error}");
}

/// The mapper is `ListMap`'s second operand — the list comes first, like every sequence operation. The rule once read the first operand, whose list value never looks like a function atom, and so checked nothing.
#[test]
fn a_two_parameter_mapper_is_rejected() {
    let mut builder = ErsdBuilder::new();
    let mapper = builder.reserve_function();
    let left = builder.value(None);
    let right = builder.value(None);
    builder.open_block();
    let body = builder.seal_block(Terminator::Return(Atom::Value(left)));
    builder.define_function(mapper, Some("mapper".into()), vec![left, right], body);
    builder.item_functions(vec![mapper]);

    builder.open_block();
    let list = builder.let_value(
        None,
        Rhs::Sequence {
            operation: SequenceOp::ListBuild,
            operands: vec![],
        },
    );
    let mapped = builder.let_value(
        None,
        Rhs::Intrinsic {
            intrinsic: Intrinsic::ListMap,
            operands: vec![Atom::Value(list), Atom::Function(mapper)],
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(mapped)));
    builder.set_entry(entry);
    let error = builder.finalize().expect_err("a mapper takes one element");
    assert!(error.0.contains("mapper takes one element"), "{error}");
}

#[test]
fn a_match_missing_a_constructor_is_rejected() {
    let mut builder = ErsdBuilder::new();
    let family = builder.family(Some("Shape".into()));
    let circle = builder.constructor(family, Some("circle".into()), vec![Field::opaque(None)]);
    let _square = builder.constructor(family, Some("square".into()), vec![Field::opaque(None)]);

    builder.open_block();
    let one = nat_atom(&mut builder, 1);
    let shape = builder.let_value(
        None,
        Rhs::Construct {
            constructor: circle,
            fields: vec![one],
        },
    );
    builder.open_block();
    let radius = builder.value(Some("radius".into()));
    let arm = builder.seal_block(Terminator::Return(Atom::Value(radius)));
    let matched = builder.let_value(
        None,
        Rhs::MatchVariant {
            family,
            scrutinee: Atom::Value(shape),
            arms: vec![VariantArm {
                constructor: circle,
                bindings: vec![radius],
                block: arm,
            }],
            default: None,
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(matched)));
    builder.set_entry(entry);
    let error = builder.finalize().expect_err("the square arm is missing");
    assert!(error.0.contains("without arm or default"), "{error}");
}

#[test]
fn duplicate_switch_keys_are_rejected() {
    let mut builder = ErsdBuilder::new();
    builder.open_block();
    let zero = nat_atom(&mut builder, 0);
    builder.open_block();
    let first = builder.seal_block(Terminator::Return(zero));
    builder.open_block();
    let second = builder.seal_block(Terminator::Return(zero));
    builder.open_block();
    let default = builder.seal_block(Terminator::Return(zero));
    let switched = builder.let_value(
        None,
        Rhs::SwitchNat {
            scrutinee: zero,
            cases: vec![
                NatCase {
                    key: 3,
                    block: first,
                },
                NatCase {
                    key: 3,
                    block: second,
                },
            ],
            default,
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(switched)));
    builder.set_entry(entry);
    let error = builder.finalize().expect_err("keys must be distinct");
    assert!(error.0.contains("two cases"), "{error}");
}

#[test]
fn a_block_with_two_owners_is_rejected() {
    let mut builder = ErsdBuilder::new();
    builder.open_block();
    let zero = nat_atom(&mut builder, 0);
    builder.open_block();
    let shared = builder.seal_block(Terminator::Return(zero));
    let condition = nat_bool(&mut builder);
    let switched = builder.let_value(
        None,
        Rhs::SwitchBool {
            scrutinee: condition,
            if_false: shared,
            if_true: shared,
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(switched)));
    builder.set_entry(entry);
    let error = builder
        .finalize()
        .expect_err("both arms own the same block");
    assert!(error.0.contains("more than one owner"), "{error}");
}

fn nat_bool(builder: &mut ErsdBuilder) -> Atom {
    let constant = builder.constant(Constant::Bool(true));
    Atom::Constant(constant)
}

#[test]
fn a_leaked_block_is_rejected() {
    let mut builder = ErsdBuilder::new();
    builder.open_block();
    let zero = nat_atom(&mut builder, 0);
    let leaked = builder.seal_block(Terminator::Return(zero));
    let _ = leaked;
    builder.open_block();
    let entry = builder.seal_block(Terminator::Return(zero));
    builder.set_entry(entry);
    let error = builder
        .finalize()
        .expect_err("the sealed block has no owner");
    assert!(error.0.contains("no owner"), "{error}");
}

#[test]
fn a_projection_out_of_range_is_rejected() {
    let mut builder = ErsdBuilder::new();
    let schema = builder.product(ProductSchema {
        debug_name: None,
        fields: vec![Field::opaque(None), Field::opaque(None)],
        shared: false,
    });
    builder.open_block();
    let zero = nat_atom(&mut builder, 0);
    let one = nat_atom(&mut builder, 1);
    let pair = builder.let_value(
        None,
        Rhs::Product {
            schema,
            fields: vec![zero, one],
        },
    );
    let projected = builder.let_value(
        None,
        Rhs::Project {
            schema,
            product: Atom::Value(pair),
            field: 2,
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(projected)));
    builder.set_entry(entry);
    let error = builder.finalize().expect_err("field 2 does not exist");
    assert!(error.0.contains("width"), "{error}");
}

/// A deeply nested module verifies (and a malformed one diagnoses) on the default test-thread stack: the walk is iterative, so nesting depth costs heap, not native stack.
#[test]
fn a_deep_module_verifies_without_native_stack() {
    let builder = deep_switch_chain(50_000, false);
    builder.finalize().expect("the deep module verifies");
}

#[test]
fn a_deep_malformed_module_diagnoses_without_native_stack() {
    let builder = deep_switch_chain(50_000, true);
    let error = builder
        .finalize()
        .expect_err("the innermost block is malformed");
    assert!(error.0.contains("out of scope"), "{error}");
}

/// Build a `depth`-deep chain of nested `SwitchNat` defaults. With `malformed` the innermost block references a value that is never bound.
fn deep_switch_chain(depth: usize, malformed: bool) -> ErsdBuilder {
    let mut builder = ErsdBuilder::new();
    let zero = nat_atom(&mut builder, 0);
    let scrutinee = builder.item_value(Some("scrutinee".into()), Rhs::Alias(zero));

    builder.open_block();
    let innermost = if malformed {
        Atom::Value(builder.value(Some("ghost".into())))
    } else {
        Atom::Value(scrutinee)
    };
    let mut chain = builder.seal_block(Terminator::Return(innermost));
    for _ in 0..depth {
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
    builder
}
