//! Where a use's demand decides a value's storage: operand and scrutinee positions, the edges that carry a demand backwards, a conflict, the entry's parameters and a value free in another function.

use {
    super::{Storage, representation, test_support::*},
    crate::{Atom, Callee, Continuation, Edge, Function, Intrinsic, Literal, Module, Node, Repr},
    curios_num::{Floating, Natural, Rounding},
};

#[test]
fn an_intrinsic_operand_position_demands_the_raw_carrier() {
    let mut module = Module::new();
    let param = module.add_value(Some("x".into()));
    let result = module.add_value(Some("r".into()));

    let done = finish(&mut module, vec![Atom::Value(result)]);
    let body = module.add_node(Node::LetIntrinsic {
        result,
        op: Intrinsic::NatAdd,
        args: vec![
            Atom::Value(param),
            Atom::Literal(Literal::Nat(Natural::from(1u32))),
        ],
        next: done,
    });
    let enter = through_continuation(&mut module, vec![param], body);
    entry(&mut module, vec![], enter);

    assert_eq!(
        representation(&module).values[&param],
        Storage::Raw(Repr::Nat)
    );
}

/// The scrutinee is read as an unsigned tag, so it is a raw position even though nothing arithmetic touches it.
#[test]
fn a_switch_scrutinee_demands_the_raw_carrier() {
    let mut module = Module::new();
    let param = module.add_value(Some("tag".into()));

    let done = finish(&mut module, vec![]);
    let body = module.add_node(Node::Switch {
        scrutinee: Atom::Value(param),
        cases: Default::default(),
        default: Some(Edge {
            target: match module.node(done) {
                Some(Node::ApplyCont(edge)) => edge.target,
                _ => unreachable!("`finish` builds an `ApplyCont`"),
            },
            args: vec![],
        }),
    });
    let enter = through_continuation(&mut module, vec![param], body);
    entry(&mut module, vec![], enter);

    assert_eq!(
        representation(&module).values[&param],
        Storage::Raw(Repr::Nat)
    );
}

/// A call argument's demand is the storage of the callee's parameter it feeds, so where nothing reads that parameter raw, nothing about the argument is raw either.
#[test]
fn a_call_argument_alone_stays_boxed() {
    let mut module = Module::new();
    let param = module.add_value(Some("a".into()));
    let unread = module.add_value(Some("unread".into()));
    let callee = module.reserve_function();
    let resume = module.reserve_continuation();

    define(
        &mut module,
        callee,
        "callee",
        vec![unread],
        |module, sentinel| {
            module.add_node(Node::ApplyCont(Edge {
                target: sentinel,
                args: vec![Atom::Literal(Literal::Nat(Natural::from(0u32)))],
            }))
        },
    );
    let body = module.add_node(Node::ApplyFun {
        callee: Callee::Known(callee),
        args: vec![Atom::Value(param)],
        return_to: resume,
    });
    let enter = through_continuation(&mut module, vec![param], body);
    entry(&mut module, vec![], enter);

    let decided = representation(&module).values;
    assert_eq!(decided[&unread], Storage::Boxed);
    assert_eq!(decided[&param], Storage::Boxed);
}

/// The rule the whole analysis turns on. `carried` is *only* ever passed round an edge — it has no raw use of its own — so a scan over use positions would leave it boxed and the loop would coerce every iteration. Its demand is the storage of the parameter it feeds, and that parameter is raw because an intrinsic reads it, so the decision has to travel backwards across the edge to reach it.
#[test]
fn an_edge_argument_inherits_the_storage_of_the_parameter_it_feeds() {
    let mut module = Module::new();
    let param = module.add_value(Some("p".into()));
    let result = module.add_value(Some("r".into()));
    let carried = module.add_value(Some("carried".into()));

    // The loop head reads its parameter arithmetically, which is what makes the parameter raw.
    let head = module.reserve_continuation();
    let done = finish(&mut module, vec![Atom::Value(result)]);
    let head_body = module.add_node(Node::LetIntrinsic {
        result,
        op: Intrinsic::NatAdd,
        args: vec![
            Atom::Value(param),
            Atom::Literal(Literal::Nat(Natural::from(1u32))),
        ],
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

    // `carried` is a remainder its literal divisor bounds, so it may ride a word, and it reaches the head only as an edge argument.
    let jump = module.add_node(Node::ApplyCont(Edge {
        target: head,
        args: vec![Atom::Value(carried)],
    }));
    let body = module.add_node(Node::LetIntrinsic {
        result: carried,
        op: Intrinsic::NatRem,
        args: vec![
            Atom::Literal(Literal::Nat(Natural::from(9u32))),
            Atom::Literal(Literal::Nat(Natural::from(7u32))),
        ],
        next: jump,
    });
    entry(&mut module, vec![], body);

    assert_eq!(
        representation(&module).values[&carried],
        Storage::Raw(Repr::Nat)
    );
}

/// The top of the lattice. A continuation parameter is the one value with no producer to fix its carrier, so it is the one value two uses can disagree about — and the disagreement has to settle *above* both, because the solver stops when nothing changed, not when nothing is left to change.
#[test]
fn disagreeing_raw_carriers_settle_at_conflict_rather_than_oscillating() {
    let mut module = Module::new();
    let shared = module.add_value(Some("shared".into()));
    let first = module.add_value(Some("a".into()));
    let second = module.add_value(Some("b".into()));

    let done = finish(&mut module, vec![Atom::Value(second)]);
    // Read as a float here...
    let float = module.add_node(Node::LetIntrinsic {
        result: second,
        op: Intrinsic::FltAdd(Rounding::TiesToEven),
        args: vec![
            Atom::Value(shared),
            Atom::Literal(Literal::Flt(Floating::from(1.0))),
        ],
        next: done,
    });
    // ...and as a word here.
    let body = module.add_node(Node::LetIntrinsic {
        result: first,
        op: Intrinsic::NatAdd,
        args: vec![
            Atom::Value(shared),
            Atom::Literal(Literal::Nat(Natural::from(1u32))),
        ],
        next: float,
    });
    let enter = through_continuation(&mut module, vec![shared], body);
    entry(&mut module, vec![], enter);

    let decided = representation(&module).values;
    assert_eq!(decided[&shared], Storage::Conflict);
    assert_eq!(decided[&shared].raw_carrier(), None);
}

/// The entry is entered by the host, through a signature no store site of the module's controls, so however arithmetically its body reads a parameter, the parameter is held as it arrived and each use unboxes.
#[test]
fn the_entrys_parameter_stays_boxed_however_its_body_reads_it() {
    let mut module = Module::new();
    let param = module.add_value(Some("x".into()));
    let result = module.add_value(Some("r".into()));

    let done = finish(&mut module, vec![Atom::Value(result)]);
    let body = module.add_node(Node::LetIntrinsic {
        result,
        op: Intrinsic::NatAdd,
        args: vec![
            Atom::Value(param),
            Atom::Literal(Literal::Nat(Natural::from(1u32))),
        ],
        next: done,
    });
    entry(&mut module, vec![param], body);

    assert_eq!(representation(&module).values[&param], Storage::Boxed);
}

/// `escaping` is bound in the entry function and read arithmetically inside *another* function's body, so `curios-emit`'s `machine::lower` lambda-lifts it onto that function as a parameter of the same name. It is one value wherever it is read, so it is decided once, at the word its definition produces and its use in the callee demands, and the emitter declares the lifted parameter at that carrier.
#[test]
fn a_value_free_in_another_function_is_held_at_its_own_carrier() {
    let mut module = Module::new();
    let escaping = module.add_value(Some("escaping".into()));
    let result = module.add_value(Some("r".into()));
    let callee = module.reserve_function();
    let callee_return = module.reserve_continuation();
    let resume = module.reserve_continuation();

    // The callee reads a value it does not bind, which is what makes it free there.
    let callee_done = finish(&mut module, vec![Atom::Value(result)]);
    let callee_body = module.add_node(Node::LetIntrinsic {
        result,
        op: Intrinsic::NatAdd,
        args: vec![
            Atom::Value(escaping),
            Atom::Literal(Literal::Nat(Natural::from(1u32))),
        ],
        next: callee_done,
    });
    module.define_function(
        callee,
        Function {
            debug_name: Some("callee".into()),
            params: vec![],
            return_cont: callee_return,
            body: callee_body,
            droppable: false,
        },
    );

    // The entry binds it to a remainder its literal divisor bounds, so its definition produces the very word the callee wants.
    let call = module.add_node(Node::ApplyFun {
        callee: Callee::Known(callee),
        args: vec![],
        return_to: resume,
    });
    let body = module.add_node(Node::LetIntrinsic {
        result: escaping,
        op: Intrinsic::NatRem,
        args: vec![
            Atom::Literal(Literal::Nat(Natural::from(9u32))),
            Atom::Literal(Literal::Nat(Natural::from(7u32))),
        ],
        next: call,
    });
    entry(&mut module, vec![], body);

    assert_eq!(
        representation(&module).values[&escaping],
        Storage::Raw(Repr::Nat)
    );
}
