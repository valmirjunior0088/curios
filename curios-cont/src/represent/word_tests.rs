//! Which values a word holds: a signed one what lies below `2³⁰`, an unsigned one what lies below `2³²`, as the bound on every value reaching a site says.

use {
    super::{Storage, representation, test_support::*},
    crate::{
        Atom, Callee, Continuation, Edge, Intrinsic, Literal, Module, Node, Register, Repr,
        ValueDef, ValueId,
    },
    curios_num::Natural,
};

/// The soundness half of the edge rule: a parameter some argument of which nothing bounds is never a word, here the entry's own parameter, a reference the host passes, plus one. Narrowing it to a word would saturate it, so the parameter stays a reference however its uses read it, or the edge would coerce a value past the i31 into a word and lose it.
#[test]
fn a_parameter_an_unbounded_value_reaches_is_never_held_in_a_word() {
    let mut module = Module::new();
    let host = module.add_value(Some("host".into()));
    let param = module.add_value(Some("p".into()));
    let result = module.add_value(Some("r".into()));
    let carried = module.add_value(Some("carried".into()));

    let head = module.reserve_continuation();
    let done = finish(&mut module, vec![Atom::Value(result)]);
    let head_body = module.add_node(Node::LetIntrinsic {
        result,
        op: Intrinsic::NatAdd,
        args: vec![Atom::Value(param), nat(1)],
        next: done,
    });
    module.define_continuation(
        head,
        Continuation {
            debug_name: Some("head".into()),
            params: vec![param],
            body: head_body,
        },
    );

    let jump = module.add_node(Node::ApplyCont(Edge {
        target: head,
        args: vec![Atom::Value(carried)],
    }));
    let body = module.add_node(Node::LetIntrinsic {
        result: carried,
        op: Intrinsic::NatAdd,
        args: vec![Atom::Value(host), nat(1)],
        next: jump,
    });
    entry(&mut module, vec![host], body);

    let decided = representation(&module).values;
    assert_eq!(decided[&param], Storage::Boxed);
    assert_eq!(decided[&carried], Storage::Boxed);
}

/// A sum of two literals lies two bits wide, so the sum, and the parameter it is the one argument of, ride words.
#[test]
fn a_parameter_a_bounded_sum_reaches_is_held_in_a_word() {
    let mut module = Module::new();
    let param = module.add_value(Some("p".into()));
    let result = module.add_value(Some("r".into()));
    let carried = module.add_value(Some("carried".into()));

    let head = module.reserve_continuation();
    let done = finish(&mut module, vec![Atom::Value(result)]);
    let head_body = module.add_node(Node::LetIntrinsic {
        result,
        op: Intrinsic::NatAdd,
        args: vec![Atom::Value(param), nat(1)],
        next: done,
    });
    module.define_continuation(
        head,
        Continuation {
            debug_name: Some("head".into()),
            params: vec![param],
            body: head_body,
        },
    );

    let jump = module.add_node(Node::ApplyCont(Edge {
        target: head,
        args: vec![Atom::Value(carried)],
    }));
    let body = module.add_node(Node::LetIntrinsic {
        result: carried,
        op: Intrinsic::NatAdd,
        args: vec![nat(1), nat(2)],
        next: jump,
    });
    entry(&mut module, vec![], body);

    let decided = representation(&module).values;
    assert_eq!(decided[&param], Storage::Raw(Repr::Nat));
    assert_eq!(decided[&carried], Storage::Raw(Repr::Nat));
}

/// A loop whose parameter `seed` enters and `op(p, 1)` comes back round as: the parameter and the step.
fn counting_loop(module: &mut Module, seed: Atom, op: Intrinsic) -> (ValueId, ValueId) {
    let param = module.add_value(Some("p".into()));
    let step = module.add_value(Some("step".into()));
    let head = module.reserve_continuation();
    let back = module.add_node(Node::ApplyCont(Edge {
        target: head,
        args: vec![Atom::Value(step)],
    }));
    let head_body = module.add_node(Node::LetIntrinsic {
        result: step,
        op,
        args: vec![Atom::Value(param), nat(1)],
        next: back,
    });
    module.define_continuation(
        head,
        Continuation {
            debug_name: Some("head".into()),
            params: vec![param],
            body: head_body,
        },
    );
    let enter = module.add_node(Node::ApplyCont(Edge {
        target: head,
        args: vec![seed],
    }));
    entry(module, vec![], enter);
    (param, step)
}

/// A counter growing round a loop climbs past every bound, so neither it nor its step is a word, however small it starts.
#[test]
fn a_counter_growing_round_a_loop_is_never_held_in_a_word() {
    let mut module = Module::new();
    let (param, step) = counting_loop(&mut module, nat(0), Intrinsic::NatAdd);

    let decided = representation(&module).values;
    assert_eq!(decided[&param], Storage::Boxed);
    assert_eq!(decided[&step], Storage::Boxed);
}

/// A counter counted down from a literal never passes it, since a monus lies within its first operand, so it and its step ride words.
#[test]
fn a_counter_counted_down_from_a_literal_is_held_in_a_word() {
    let mut module = Module::new();
    let (param, step) = counting_loop(&mut module, nat(17), Intrinsic::NatSub);

    let decided = representation(&module).values;
    assert_eq!(decided[&param], Storage::Raw(Repr::Nat));
    assert_eq!(decided[&step], Storage::Raw(Repr::Nat));
}

/// A length counted down round a loop and read at as a position rides a word read unsigned: a length lies below `2³²`, a monus within its first operand, and the index a read takes is a length's, so the length, the parameter and the step all hold `Repr::Length`, where no word could hold what may lie past `2³⁰`.
#[test]
fn a_length_counted_down_and_read_at_rides_a_word_read_unsigned() {
    let mut module = Module::new();
    let bytes = module.add_value(Some("bytes".into()));
    let length = module.add_value(Some("length".into()));
    let param = module.add_value(Some("p".into()));
    let byte = module.add_value(Some("byte".into()));
    let step = module.add_value(Some("step".into()));

    let head = module.reserve_continuation();
    let back = module.add_node(Node::ApplyCont(Edge {
        target: head,
        args: vec![Atom::Value(step)],
    }));
    let count_down = module.add_node(Node::LetIntrinsic {
        result: step,
        op: Intrinsic::NatSub,
        args: vec![Atom::Value(param), nat(1)],
        next: back,
    });
    let read = module.add_node(Node::LetIntrinsic {
        result: byte,
        op: Intrinsic::BytesGet,
        args: vec![Atom::Value(bytes), Atom::Value(param)],
        next: count_down,
    });
    module.define_continuation(
        head,
        Continuation {
            debug_name: Some("head".into()),
            params: vec![param],
            body: read,
        },
    );
    let enter = module.add_node(Node::ApplyCont(Edge {
        target: head,
        args: vec![Atom::Value(length)],
    }));
    let measure = module.add_node(Node::LetIntrinsic {
        result: length,
        op: Intrinsic::BytesLen,
        args: vec![Atom::Value(bytes)],
        next: enter,
    });
    entry(&mut module, vec![bytes], measure);

    let decided = representation(&module).values;
    for value in [length, param, step] {
        assert_eq!(decided[&value], Storage::Raw(Repr::Length));
    }
}

/// The word beside the float: a parameter every call hands a remainder its literal divisor bounds, read by integer arithmetic, rides a word.
#[test]
fn a_known_parameter_every_call_hands_a_word_is_held_in_a_word() {
    let mut module = Module::new();
    let callee = module.reserve_function();
    let param = module.add_value(Some("n".into()));
    let sum = module.add_value(Some("sum".into()));
    define(
        &mut module,
        callee,
        "counter",
        vec![param],
        |module, sentinel| {
            let done = module.add_node(Node::ApplyCont(Edge {
                target: sentinel,
                args: vec![Atom::Value(sum)],
            }));
            module.add_node(Node::LetIntrinsic {
                result: sum,
                op: Intrinsic::NatAdd,
                args: vec![
                    Atom::Value(param),
                    Atom::Literal(Literal::Nat(Natural::from(1u32))),
                ],
                next: done,
            })
        },
    );
    let bounded = module.add_value(Some("bounded".into()));
    let after = module.reserve_continuation();
    let returned = module.add_value(Some("returned".into()));
    let done = finish(&mut module, vec![Atom::Value(returned)]);
    resume(&mut module, after, vec![returned], done);
    let call = module.add_node(Node::ApplyFun {
        callee: Callee::Known(callee),
        args: vec![Atom::Value(bounded)],
        return_to: after,
    });
    let body = module.add_node(Node::LetIntrinsic {
        result: bounded,
        op: Intrinsic::NatRem,
        args: vec![
            Atom::Literal(Literal::Nat(Natural::from(9u32))),
            Atom::Literal(Literal::Nat(Natural::from(7u32))),
        ],
        next: call,
    });
    entry(&mut module, vec![], body);

    assert_eq!(
        representation(&module).values[&param],
        Storage::Raw(Repr::Nat)
    );
}

/// The word beside the stated float: the entry's parameter arrives as a reference the host passes, and stating a word lets the known parameter it reaches ride one.
#[test]
fn a_known_parameter_a_stated_word_reaches_is_held_in_a_word() {
    let mut module = Module::new();
    let callee = module.reserve_function();
    let param = module.add_value(Some("n".into()));
    let sum = module.add_value(Some("sum".into()));
    define(
        &mut module,
        callee,
        "counter",
        vec![param],
        |module, sentinel| {
            let done = module.add_node(Node::ApplyCont(Edge {
                target: sentinel,
                args: vec![Atom::Value(sum)],
            }));
            module.add_node(Node::LetIntrinsic {
                result: sum,
                op: Intrinsic::NatAdd,
                args: vec![
                    Atom::Value(param),
                    Atom::Literal(Literal::Nat(Natural::from(1u32))),
                ],
                next: done,
            })
        },
    );
    let flag = module.add_value_def(ValueDef {
        debug_name: Some("flag".into()),
        register: Some(Register::Word),
    });
    let after = module.reserve_continuation();
    let returned = module.add_value(Some("returned".into()));
    let done = finish(&mut module, vec![Atom::Value(returned)]);
    resume(&mut module, after, vec![returned], done);
    let call = module.add_node(Node::ApplyFun {
        callee: Callee::Known(callee),
        args: vec![Atom::Value(flag)],
        return_to: after,
    });
    entry(&mut module, vec![flag], call);

    let decided = representation(&module).values;
    assert_eq!(decided[&param], Storage::Raw(Repr::Nat));
    assert_eq!(
        decided[&flag],
        Storage::Boxed,
        "the entry's parameter stays the reference the host passes"
    );
}
