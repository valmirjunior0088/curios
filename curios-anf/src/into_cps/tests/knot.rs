//! A recursive group lowered through cells: a knot of values and functions, a top-level value run by its force, a construction by need, and the protocol a force keeps — every initializer set first, the cached and the reentrant paths, captures of storage alone.

use {super::test_support::*, crate::*, curios_num::Natural, std::collections::BTreeSet};

/// `f() = 1`, an item `x = f()` and the functions `g() = x` and `h() = x`, read by an entry `g() + h()`.
fn read_twice() -> curios_cps::Module {
    let mut builder = Builder::new();
    let f = builder.reserve_function();
    builder.open_block();
    let one = nat(&mut builder, 1);
    let body = builder.seal_block(Terminator::Return(one));
    builder.define_function(f, Some("f".into()), vec![], body);
    builder.item_functions(vec![f]);
    let x = builder.item_value(
        Some("x".into()),
        Rhs::Apply {
            callee: Atom::Function(f),
            arguments: vec![],
        },
    );
    let readers: Vec<FunctionId> = ["g", "h"]
        .into_iter()
        .map(|name| {
            let reader = builder.reserve_function();
            builder.open_block();
            let body = builder.seal_block(Terminator::Return(Atom::Value(x)));
            builder.define_function(reader, Some(name.into()), vec![], body);
            reader
        })
        .collect();
    builder.item_functions(readers.clone());
    builder.open_block();
    let [g, h] = [readers[0], readers[1]].map(|reader| {
        builder.let_value(
            None,
            Rhs::Apply {
                callee: Atom::Function(reader),
                arguments: vec![],
            },
        )
    });
    let sum = builder.let_value(
        None,
        Rhs::Operation {
            operation: Operation::NatAdd,
            operands: vec![Atom::Value(g), Atom::Value(h)],
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(sum)));
    builder.set_entry(entry);
    lowered(&builder.finalize().expect("verifies"))
}

/// The function of `cps` its lowering named `name`, with its identity.
fn named<'m>(
    cps: &'m curios_cps::Module,
    name: &str,
) -> Option<(curios_cps::FunctionId, &'m curios_cps::Function)> {
    cps.function_ids()
        .filter_map(|id| Some((id, cps.function(id)?)))
        .find(|(_, function)| function.debug_name.as_deref() == Some(name))
}

/// How many calls of `function` `cps` makes.
fn calls_of(cps: &curios_cps::Module, function: curios_cps::FunctionId) -> usize {
    cps.nodes()
        .iter()
        .flatten()
        .filter(|node| {
            matches!(node, curios_cps::Node::ApplyFun { callee: curios_cps::Callee::Known(callee), .. } if *callee == function)
        })
        .count()
}

// The knot protocol: every initializer set before forcing, storage-only captures, and the cached and reentrant paths.

fn knot() -> curios_cps::Module {
    let mut builder = Builder::new();
    let constant = builder.constant(Constant::Nat(Natural::from(41u32)));
    let seed = builder.item_value(Some("seed".into()), Rhs::Alias(Atom::Constant(constant)));
    let left = builder.value(Some("left".into()));
    let right = builder.value(Some("right".into()));
    let read = builder.reserve_function();
    builder.open_block();
    let body = builder.seal_block(Terminator::Return(Atom::Value(left)));
    builder.define_function(read, Some("read".into()), vec![], body);
    builder.open_block();
    let left_init = builder.seal_block(Terminator::Return(Atom::Value(seed)));
    builder.open_block();
    let value = builder.let_value(
        None,
        Rhs::Apply {
            callee: Atom::Function(read),
            arguments: vec![],
        },
    );
    let right_init = builder.seal_block(Terminator::Return(Atom::Value(value)));
    let group = builder.rec_group(vec![read], vec![(left, left_init), (right, right_init)]);
    builder.item_rec(group);
    builder.open_block();
    let sum = builder.let_value(
        None,
        Rhs::Operation {
            operation: Operation::NatAdd,
            operands: vec![Atom::Value(left), Atom::Value(right)],
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(sum)));
    builder.set_entry(entry);
    lower_to_cps(&builder.finalize().expect("a pure acyclic knot"))
}

fn force(module: &curios_cps::Module) -> &curios_cps::Function {
    module
        .functions()
        .iter()
        .flatten()
        .find(|function| function.debug_name.as_deref() == Some("left/force"))
        .expect("left's forcing function")
}

fn executable(module: &curios_cps::Module, mut node: curios_cps::NodeId) -> &curios_cps::Node {
    loop {
        match module.node(node).unwrap() {
            curios_cps::Node::LetCont { body, .. } => node = *body,
            node => return node,
        }
    }
}

fn resumed(module: &curios_cps::Module, id: curios_cps::ContinuationId) -> &curios_cps::Node {
    executable(module, module.continuation(id).unwrap().body)
}

/// A group with a function member and a computed member ties through the same cells a value-only knot does: the function member's forward reference to the computed member is a cell read at its entry, never a direct capture of a value that does not exist yet.
#[test]
fn a_mixed_recursive_group_lowers_through_cells() {
    let mut builder = Builder::new();
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
    let cps = lowered(&module);
    assert!(
        has_node(&cps, |node| matches!(
            node,
            curios_cps::Node::Cell {
                op: curios_cps::CellOp::Poll,
                ..
            }
        )),
        "{cps}"
    );
    assert!(
        has_node(&cps, |node| matches!(
            node,
            curios_cps::Node::Cell {
                op: curios_cps::CellOp::Fill,
                ..
            }
        )),
        "{cps}"
    );
    assert!(
        has_node(&cps, |node| matches!(node, curios_cps::Node::LetFun { .. })),
        "{cps}"
    );
}

#[test]
fn a_value_only_knot_lowers_through_cells() {
    // rec { lazy = Pair { force: fn() = lazy } } — the knot ties through a compiler-internal cell: allocated, stored once, read at closure entry.
    let mut builder = Builder::new();
    let schema = builder.product(ProductSchema {
        debug_name: Some("Lazy".into()),
        fields: ProductFields::Declared(vec![
            Field::opaque(Some("force".into())),
            Field::opaque(Some("mark".into())),
        ]),
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

    let cps = lowered(&module);
    assert!(
        has_node(&cps, |node| matches!(node, curios_cps::Node::Cell { .. })),
        "{cps}"
    );
}

#[test]
fn a_knot_member_read_only_by_an_item_is_kept() {
    // rec { lazy = Pair { force: fn() = lazy, mark: 0 } }; let read = lazy.mark — the only read is a top-level item, which lives in no block. A member is dropped when nothing outside its initializer reads it, and a scan of the blocks alone would miss this read, dropping the member and lowering the item's operand to a value the arena lacks.
    let mut builder = Builder::new();
    let schema = builder.product(ProductSchema {
        debug_name: Some("Lazy".into()),
        fields: ProductFields::Declared(vec![
            Field::opaque(Some("force".into())),
            Field::opaque(Some("mark".into())),
        ]),
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

    let cps = lowered(&module);
    assert!(
        has_node(&cps, |node| matches!(node, curios_cps::Node::Cell { .. })),
        "{cps}"
    );
}

/// A top-level value that computes is a knot of its own: its statement is its one initializer, which its force alone runs, so nothing applies `f` but that initializer, and each function that reads `x` forces it.
#[test]
fn a_top_level_value_that_computes_is_run_by_its_force_alone() {
    let cps = read_twice();
    let (f, _) = named(&cps, "f").expect("f is lowered");
    let (init, initializer) =
        named(&cps, "x/init").unwrap_or_else(|| panic!("x is lowered by need: {cps}"));
    let (force, _) = named(&cps, "x/force").expect("x has a force");

    assert!(
        matches!(executable(&cps, initializer.body), curios_cps::Node::ApplyFun { callee: curios_cps::Callee::Known(callee), .. } if *callee == f),
        "x's initializer applies f: {cps}"
    );
    assert_eq!(calls_of(&cps, f), 1, "{cps}");
    assert_eq!(
        calls_of(&cps, init),
        0,
        "the initializer is run through its box: {cps}"
    );
    assert_eq!(calls_of(&cps, force), 2, "g and h each force x: {cps}");
}

/// What only builds from what is bound ahead is bound where it stands, and what builds from a value lowered by need is lowered so, since binding it where it stands would force that value: `table`, a row of constants, is no knot, and `held`, a row holding `x`, is one.
#[test]
fn a_construction_is_by_need_where_it_holds_a_value_lowered_so() {
    let mut builder = Builder::new();
    let schema = builder.product(ProductSchema {
        debug_name: Some("Pair".into()),
        fields: ProductFields::Declared(vec![
            Field::opaque(Some("left".into())),
            Field::opaque(Some("right".into())),
        ]),
    });
    let f = builder.reserve_function();
    builder.open_block();
    let one = nat(&mut builder, 1);
    let body = builder.seal_block(Terminator::Return(one));
    builder.define_function(f, Some("f".into()), vec![], body);
    builder.item_functions(vec![f]);
    let x = builder.item_value(
        Some("x".into()),
        Rhs::Apply {
            callee: Atom::Function(f),
            arguments: vec![],
        },
    );
    let (one, two) = (nat(&mut builder, 1), nat(&mut builder, 2));
    let table = builder.item_value(
        Some("table".into()),
        Rhs::Product {
            schema,
            fields: vec![one, two],
        },
    );
    let held = builder.item_value(
        Some("held".into()),
        Rhs::Product {
            schema,
            fields: vec![Atom::Value(x), one],
        },
    );
    builder.open_block();
    let [left, right] = [(held, 0), (table, 1)].map(|(product, field)| {
        builder.let_value(
            None,
            Rhs::Project {
                schema,
                product: Atom::Value(product),
                field,
            },
        )
    });
    let sum = builder.let_value(
        None,
        Rhs::Operation {
            operation: Operation::NatAdd,
            operands: vec![Atom::Value(left), Atom::Value(right)],
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(sum)));
    builder.set_entry(entry);
    let cps = lowered(&builder.finalize().expect("verifies"));

    assert!(named(&cps, "table/init").is_none(), "{cps}");
    assert!(named(&cps, "held/init").is_some(), "{cps}");
}

#[test]
fn every_initializer_is_set_before_the_first_force() {
    let module = knot();
    let named = |id: curios_cps::FunctionId| {
        module
            .function(id)
            .unwrap()
            .debug_name
            .clone()
            .unwrap_or_default()
    };
    let mut node = module.function(module.entry().unwrap()).unwrap().body;
    let mut cells = BTreeSet::new();
    let mut boxes = BTreeSet::new();
    let mut set = BTreeSet::new();
    let mut cycles_bound = false;
    let mut functions_bound = false;
    loop {
        match module.node(node).unwrap() {
            curios_cps::Node::LetValue { next, .. } => node = *next,
            curios_cps::Node::LetCont { body, .. } => node = *body,
            curios_cps::Node::LetFun { functions, body } if !cycles_bound => {
                assert!(functions.iter().all(|&f| named(f).ends_with("/cycle")));
                assert!(cells.is_empty() && boxes.is_empty());
                cycles_bound = true;
                node = *body;
            }
            curios_cps::Node::LetFun { body, .. } => {
                assert_eq!((cells.len(), boxes.len()), (2, 2));
                functions_bound = true;
                node = *body;
            }
            curios_cps::Node::Cell {
                op: curios_cps::CellOp::Reserve,
                return_to,
                ..
            } => {
                assert!(cycles_bound && !functions_bound);
                let continuation = module.continuation(*return_to).unwrap();
                cells.insert(continuation.params[0]);
                node = continuation.body;
            }
            curios_cps::Node::Box {
                op: curios_cps::BoxOp::New,
                args,
                return_to,
            } => {
                assert!(cycles_bound && !functions_bound);
                let [curios_cps::Atom::Fun(cycle)] = args[..] else {
                    panic!("a box made holding its cycle")
                };
                assert!(named(cycle).ends_with("/cycle"));
                let continuation = module.continuation(*return_to).unwrap();
                boxes.insert(continuation.params[0]);
                node = continuation.body;
            }
            curios_cps::Node::Box {
                op: curios_cps::BoxOp::Set,
                args,
                return_to,
            } => {
                assert!(functions_bound);
                let curios_cps::Atom::Value(held) = args[0] else {
                    panic!("box handle")
                };
                assert!(boxes.contains(&held) && set.insert(held));
                let curios_cps::Atom::Fun(initializer) = args[1] else {
                    panic!("an initializer")
                };
                assert!(named(initializer).ends_with("/init"));
                node = module.continuation(*return_to).unwrap().body;
            }
            curios_cps::Node::ApplyFun { .. } => {
                assert_eq!(set, boxes);
                break;
            }
            other => panic!("unexpected initialization operation: {other:?}"),
        }
    }
}

#[test]
fn a_cached_result_returns_without_touching_the_initializer() {
    let module = knot();
    let function = force(&module);
    let curios_cps::Node::Cell {
        op: curios_cps::CellOp::Poll,
        return_to,
        ..
    } = executable(&module, function.body)
    else {
        panic!("poll first")
    };
    let polled = module.continuation(*return_to).unwrap();
    let curios_cps::Node::Switch { cases, .. } = resumed(&module, *return_to) else {
        panic!("dispatch presence")
    };
    let curios_cps::Node::ApplyCont(edge) = resumed(&module, cases[&1].target) else {
        panic!("cached return")
    };
    assert_eq!(edge.target, function.return_cont);
    assert_eq!(edge.args, vec![curios_cps::Atom::Value(polled.params[1])]);
}

#[test]
fn a_force_sets_the_cycle_before_it_runs_what_its_box_held_and_fills_the_result() {
    let module = knot();
    let function = force(&module);
    let curios_cps::Node::Cell {
        args: poll_args,
        return_to,
        ..
    } = executable(&module, function.body)
    else {
        panic!("poll")
    };
    let curios_cps::Node::Switch { cases, .. } = resumed(&module, *return_to) else {
        panic!("presence")
    };
    let curios_cps::Node::Box {
        op: curios_cps::BoxOp::Held,
        args: held_args,
        return_to,
    } = resumed(&module, cases[&0].target)
    else {
        panic!("read what the box holds")
    };
    let read = module.continuation(*return_to).unwrap();
    let curios_cps::Node::Box {
        op: curios_cps::BoxOp::Set,
        args,
        return_to,
    } = resumed(&module, *return_to)
    else {
        panic!("set the cycle")
    };
    let [curios_cps::Atom::Value(held), curios_cps::Atom::Fun(cycle)] = args[..] else {
        panic!("the box and the cycle")
    };
    assert_eq!(held_args, &[curios_cps::Atom::Value(held)]);
    let cycle = module.function(cycle).unwrap();
    assert_eq!(cycle.debug_name.as_deref(), Some("left/cycle"));
    let curios_cps::Node::Panic(curios_cps::Panic::Cycle(Some(member))) =
        executable(&module, cycle.body)
    else {
        panic!("a cycle naming its member")
    };
    assert_eq!(module.members()[member.index()], "left");
    let curios_cps::Node::ApplyFun {
        callee: curios_cps::Callee::Closure(next),
        args,
        return_to,
    } = resumed(&module, *return_to)
    else {
        panic!("run what the box held")
    };
    assert_eq!(*next, read.params[0]);
    assert!(args.is_empty());
    let produced = module.continuation(*return_to).unwrap().params[0];
    let curios_cps::Node::Cell {
        op: curios_cps::CellOp::Fill,
        args,
        return_to,
    } = resumed(&module, *return_to)
    else {
        panic!("cache the result")
    };
    assert_eq!(
        args,
        &[poll_args[0].clone(), curios_cps::Atom::Value(produced)]
    );
    let curios_cps::Node::ApplyCont(edge) = resumed(&module, *return_to) else {
        panic!("return result")
    };
    assert_eq!(edge.target, function.return_cont);
    assert_eq!(edge.args, vec![curios_cps::Atom::Value(produced)]);
}

#[test]
fn forcing_captures_only_the_cell_and_box() {
    let module = knot();
    let function = force(&module);
    let mut used = BTreeSet::new();
    let mut bound = BTreeSet::new();
    let mut storage = BTreeSet::new();
    let mut work = vec![function.body];
    while let Some(id) = work.pop() {
        let node = module.node(id).unwrap();
        for atom in curios_cps::atoms(node) {
            match atom {
                curios_cps::Atom::Value(value) => {
                    used.insert(*value);
                }
                curios_cps::Atom::Fun(id) => assert_eq!(
                    module.function(*id).unwrap().debug_name.as_deref(),
                    Some("left/cycle"),
                    "a force names its member's cycle and no initializer"
                ),
                _ => {}
            }
        }
        match node {
            curios_cps::Node::LetCont {
                continuations,
                body,
            } => {
                work.push(*body);
                for id in continuations {
                    let continuation = module.continuation(*id).unwrap();
                    bound.extend(continuation.params.iter().copied());
                    work.push(continuation.body);
                }
            }
            curios_cps::Node::Cell { args, .. } | curios_cps::Node::Box { args, .. } => {
                let curios_cps::Atom::Value(handle) = args[0] else {
                    panic!("storage handle")
                };
                storage.insert(handle);
            }
            curios_cps::Node::ApplyFun {
                callee: curios_cps::Callee::Closure(value),
                ..
            } => {
                used.insert(*value);
            }
            curios_cps::Node::ApplyCont(_)
            | curios_cps::Node::Switch { .. }
            | curios_cps::Node::Panic(_) => {}
            other => panic!("unexpected force operation: {other:?}"),
        }
    }
    assert_eq!(storage.len(), 2);
    assert_eq!(
        used.difference(&bound).copied().collect::<BTreeSet<_>>(),
        storage
    );
}
