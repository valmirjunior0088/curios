use {
    super::answer_window_guards,
    crate::{
        Atom, Continuation, ContinuationId, Edge, Function, Intrinsic, Literal, Module, Node,
        ValueId,
    },
    curios_num::Natural,
    std::collections::BTreeMap,
};

fn nat(value: u32) -> Atom {
    Atom::Literal(Literal::Nat(Natural::from(value)))
}

/// How the arm holding the guard is reached.
enum Entry {
    /// By the default of a switch on the length with these cases, each to its own exit.
    Switch(Vec<u32>),
    /// By the default of a switch with a case `0`, and by a second jump besides.
    SwitchAndJump,
    /// By a plain jump, which decides nothing.
    Jump,
}

/// `main(n)`, reaching an arm as `entry` says, whose body takes the suffix of a window of length `n` from `start` — `rest = n ∸ start`, then `Window/extent(start, rest, n)` — and returns the guard's answer; directly in the arm, or inside a continuation the arm defines and enters where `nested`. Answers the module, the guard's result and `rest`.
fn guarded(start: u32, entry: Entry, nested: bool) -> (Module, ValueId, ValueId) {
    let mut module = Module::new();
    let main = module.reserve_function();
    let return_cont = module.reserve_continuation();
    let length = module.add_value(Some("n".into()));
    let rest = module.add_value(Some("rest".into()));
    let extent = module.add_value(Some("extent".into()));

    let done = module.add_node(Node::ApplyCont(Edge {
        target: return_cont,
        args: vec![Atom::Value(extent)],
    }));
    let guard = module.add_node(Node::LetIntrinsic {
        result: extent,
        op: Intrinsic::WindowExtent,
        args: vec![nat(start), Atom::Value(rest), Atom::Value(length)],
        next: done,
    });
    let suffix = module.add_node(Node::LetIntrinsic {
        result: rest,
        op: Intrinsic::NatSub,
        args: vec![Atom::Value(length), nat(start)],
        next: guard,
    });
    let arm_body = match nested {
        false => suffix,
        true => {
            let inner = module.add_continuation(Continuation {
                debug_name: Some("inner".into()),
                params: vec![],
                body: suffix,
            });
            let enter = module.add_node(Node::ApplyCont(Edge {
                target: inner,
                args: vec![],
            }));
            module.add_node(Node::LetCont {
                continuations: vec![inner],
                body: enter,
            })
        }
    };
    let arm = module.add_continuation(Continuation {
        debug_name: Some("arm".into()),
        params: vec![],
        body: arm_body,
    });

    let to = |target: ContinuationId| Edge {
        target,
        args: vec![],
    };
    let mut continuations = vec![arm];
    let mut exit = |module: &mut Module| {
        let body = module.add_node(Node::ApplyCont(Edge {
            target: return_cont,
            args: vec![nat(0)],
        }));
        let exit = module.add_continuation(Continuation {
            debug_name: Some("exit".into()),
            params: vec![],
            body,
        });
        continuations.push(exit);
        exit
    };
    let decision = match entry {
        Entry::Switch(cases) => {
            let cases = cases
                .into_iter()
                .map(|case| (case, to(exit(&mut module))))
                .collect::<BTreeMap<_, _>>();
            module.add_node(Node::Switch {
                scrutinee: Atom::Value(length),
                cases,
                default: Some(to(arm)),
            })
        }
        Entry::SwitchAndJump => {
            let zero = exit(&mut module);
            let switch = module.add_node(Node::Switch {
                scrutinee: Atom::Value(length),
                cases: BTreeMap::from([(0, to(zero))]),
                default: Some(to(arm)),
            });
            let around = module.add_continuation(Continuation {
                debug_name: Some("around".into()),
                params: vec![],
                body: switch,
            });
            continuations.push(around);
            let jump = module.add_node(Node::ApplyCont(to(arm)));
            let pick = module.add_continuation(Continuation {
                debug_name: Some("pick".into()),
                params: vec![],
                body: jump,
            });
            continuations.push(pick);
            module.add_node(Node::Switch {
                scrutinee: Atom::Value(length),
                cases: BTreeMap::from([(7, to(pick))]),
                default: Some(to(around)),
            })
        }
        Entry::Jump => module.add_node(Node::ApplyCont(to(arm))),
    };
    let body = module.add_node(Node::LetCont {
        continuations,
        body: decision,
    });
    module.define_function(
        main,
        Function {
            debug_name: Some("main".into()),
            params: vec![length],
            return_cont,
            body,
            droppable: false,
        },
    );
    module.set_entry(main);
    module.verify().unwrap();
    (module, extent, rest)
}

fn guards(module: &Module) -> usize {
    module
        .live_nodes()
        .filter(|(_, node)| {
            matches!(
                node,
                Node::LetIntrinsic {
                    op: Intrinsic::WindowExtent,
                    ..
                }
            )
        })
        .count()
}

fn returns(module: &Module, value: ValueId) -> bool {
    module.live_nodes().any(|(_, node)| {
        matches!(node, Node::ApplyCont(Edge { args, .. }) if args == &[Atom::Value(value)])
    })
}

/// Past a switch's case `0`, a window is at least one long, so taking its suffix from `1` cannot trap: the guard goes and its uses read the count.
#[test]
fn an_arm_past_a_zero_answers_the_guard_of_a_suffix_from_one() {
    let (mut module, extent, rest) = guarded(1, Entry::Switch(vec![0]), false);

    assert!(answer_window_guards(&mut module));
    module.verify().unwrap();
    assert_eq!(guards(&module), 0);
    assert!(returns(&module, rest) && !returns(&module, extent));
}

/// What an arm learns holds inside every continuation it defines, since nothing outside the arm can enter one.
#[test]
fn a_continuation_inside_the_arm_learns_what_the_arm_did() {
    let (mut module, _, rest) = guarded(1, Entry::Switch(vec![0]), true);

    assert!(answer_window_guards(&mut module));
    module.verify().unwrap();
    assert_eq!(guards(&module), 0);
    assert!(returns(&module, rest));
}

/// A suffix from `2` asks the length to be at least two, which cases `0` and `1` decide and case `0` alone does not.
#[test]
fn a_suffix_past_what_the_cases_decide_keeps_its_guard() {
    let (mut module, ..) = guarded(2, Entry::Switch(vec![0]), false);
    assert!(!answer_window_guards(&mut module));
    assert_eq!(guards(&module), 1);

    let (mut module, ..) = guarded(2, Entry::Switch(vec![0, 1]), false);
    assert!(answer_window_guards(&mut module));
    module.verify().unwrap();
    assert_eq!(guards(&module), 0);
}

/// An arm entered by a second transfer, or by a plain jump, has decided nothing of the length, so its guard stays.
#[test]
fn an_arm_entered_otherwise_keeps_its_guard() {
    for entry in [Entry::SwitchAndJump, Entry::Jump] {
        let (mut module, ..) = guarded(1, entry, false);
        assert!(!answer_window_guards(&mut module));
        assert_eq!(guards(&module), 1);
    }
}
