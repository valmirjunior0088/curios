//! The knot protocol: publication before forcing, storage-only captures, and the cached and reentrant paths.

use {
    crate::{AnfBuilder, Atom, Constant, Operation, Rhs, Terminator, lower_to_cps},
    curios_num::Natural,
    std::collections::BTreeSet,
};

fn knot() -> curios_cps::Module {
    let mut builder = AnfBuilder::new();
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

#[test]
fn every_initializer_is_published_before_the_first_force() {
    let module = knot();
    let mut node = module.function(module.entry().unwrap()).unwrap().body;
    let mut cells = BTreeSet::new();
    let mut channels = BTreeSet::new();
    let mut published = BTreeSet::new();
    let mut functions_bound = false;
    loop {
        match module.node(node).unwrap() {
            curios_cps::Node::LetValue { next, .. } => node = *next,
            curios_cps::Node::LetCont { body, .. } => node = *body,
            curios_cps::Node::LetFun { body, .. } => {
                assert_eq!((cells.len(), channels.len()), (2, 2));
                functions_bound = true;
                node = *body;
            }
            curios_cps::Node::Cell {
                op: curios_cps::CellOp::Reserve,
                return_to,
                ..
            } => {
                assert!(!functions_bound);
                let continuation = module.continuation(*return_to).unwrap();
                cells.insert(continuation.params[0]);
                node = continuation.body;
            }
            curios_cps::Node::Channel {
                op: curios_cps::ChannelOp::New,
                args,
                return_to,
            } => {
                assert!(!functions_bound);
                assert_eq!(
                    args,
                    &[curios_cps::Atom::Literal(curios_cps::Literal::Nat(
                        Natural::from(1u32)
                    ))]
                );
                let continuation = module.continuation(*return_to).unwrap();
                channels.insert(continuation.params[0]);
                node = continuation.body;
            }
            curios_cps::Node::Channel {
                op: curios_cps::ChannelOp::Push,
                args,
                return_to,
            } => {
                assert!(functions_bound);
                let curios_cps::Atom::Value(channel) = args[0] else {
                    panic!("channel handle")
                };
                assert!(channels.contains(&channel) && published.insert(channel));
                assert!(matches!(args[1], curios_cps::Atom::Fun(_)));
                node = module.continuation(*return_to).unwrap().body;
            }
            curios_cps::Node::ApplyFun { .. } => {
                assert_eq!(published, channels);
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
fn an_empty_initializer_is_a_cycle_and_a_taken_initializer_fills_the_result() {
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
    let curios_cps::Node::Channel {
        op: curios_cps::ChannelOp::Take,
        return_to,
        ..
    } = resumed(&module, cases[&0].target)
    else {
        panic!("claim initializer")
    };
    let taken = module.continuation(*return_to).unwrap();
    let curios_cps::Node::Switch { cases, .. } = resumed(&module, *return_to) else {
        panic!("take outcome")
    };
    assert!(matches!(
        resumed(&module, cases[&1].target),
        curios_cps::Node::Panic(curios_cps::Panic::Cycle)
    ));
    let curios_cps::Node::ApplyFun {
        callee: curios_cps::Callee::Closure(thunk),
        args,
        return_to,
    } = resumed(&module, cases[&0].target)
    else {
        panic!("run claimed initializer")
    };
    assert_eq!(*thunk, taken.params[1]);
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
fn forcing_captures_only_the_cell_and_channel() {
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
                curios_cps::Atom::Fun(_) => panic!("a force must not capture a named initializer"),
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
            curios_cps::Node::Cell { args, .. } | curios_cps::Node::Channel { args, .. } => {
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
