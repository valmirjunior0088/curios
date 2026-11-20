use {super::*, crate::*, curios_common::Qualifier};

fn literal(value: u32) -> Atom {
    Atom::Literal(Literal::Nat(curios_num::Natural::from(value)))
}

fn jump(module: &mut Module, target: ContinuationId, args: Vec<Atom>) -> NodeId {
    module.add_node(Node::ApplyCont(Edge { target, args }))
}

fn define(
    module: &mut Module,
    continuation: ContinuationId,
    body: NodeId,
    lineage: Option<Lineage>,
) {
    module.define_continuation(
        continuation,
        Continuation {
            lineage,
            debug_name: None,
            params: vec![],
            body,
            terminates: false,
        },
    );
}

/// `return add(read, 1)`: the body each arm below writes, reading `read`.
fn adds_one(module: &mut Module, read: ValueId, to: ContinuationId) -> NodeId {
    let sum = module.add_value(None);
    let returned = jump(module, to, vec![Atom::Value(sum)]);
    module.add_node(Node::LetIntrinsic {
        result: sum,
        op: Intrinsic::NatAdd,
        args: vec![Atom::Value(read), literal(1)],
        next: returned,
    })
}

/// `match scrutinee | 0 => first | _ => second end`, the two arms bound in one `LetCont`.
fn split(
    module: &mut Module,
    scrutinee: ValueId,
    first: (ContinuationId, NodeId),
    second: (ContinuationId, NodeId),
    lineage: Option<Lineage>,
) -> NodeId {
    define(module, first.0, first.1, lineage);
    define(module, second.0, second.1, lineage);
    let switch = module.add_node(Node::Switch {
        scrutinee: Atom::Value(scrutinee),
        cases: BTreeMap::from([(
            0,
            Edge {
                target: first.0,
                args: vec![],
            },
        )]),
        default: Some(Edge {
            target: second.0,
            args: vec![],
        }),
    });
    module.add_node(Node::LetCont {
        continuations: vec![first.0, second.0],
        body: switch,
    })
}

/// `main(x, y)`, its body what `body` builds of `x`, `y` and `main`'s return.
fn main_of(body: impl FnOnce(&mut Module, ValueId, ValueId, ContinuationId) -> NodeId) -> Module {
    let mut module = Module::new();
    let main = module.reserve_function();
    let main_return = module.reserve_continuation();
    let x = module.add_value(Some("x".into()));
    let y = module.add_value(Some("y".into()));
    let body = body(&mut module, x, y, main_return);
    module.define_function(
        main,
        Function {
            lineage: None,
            debug_name: Some("main".into()),
            params: vec![x, y],
            return_cont: main_return,
            body,
            droppable: Droppable::Never,
            terminates: false,
        },
    );
    module.set_entry(main);
    module.verify().unwrap();
    module
}

/// How many additions `module` holds live.
fn additions(module: &Module) -> usize {
    module
        .live_nodes()
        .filter(|(_, node)| {
            matches!(
                node,
                Node::LetIntrinsic {
                    op: Intrinsic::NatAdd,
                    ..
                }
            )
        })
        .count()
}

/// Two arms of one match that each add one to `x` and return it, of one lineage where `lineages` is false.
fn two_arms(reads: impl Fn(ValueId, ValueId) -> (ValueId, ValueId), lineages: bool) -> Module {
    main_of(|module, x, y, main_return| {
        let (first_read, second_read) = reads(x, y);
        let (first, second) = (module.reserve_continuation(), module.reserve_continuation());
        let first_body = adds_one(module, first_read, main_return);
        let second_body = adds_one(module, second_read, main_return);
        let lineage = |function| {
            Some(Lineage {
                declaration: Qualifier::empty().with("arms"),
                function,
            })
        };
        define(module, first, first_body, lineage(0));
        let second_lineage = if lineages { lineage(1) } else { lineage(0) };
        define(module, second, second_body, second_lineage);
        let switch = module.add_node(Node::Switch {
            scrutinee: Atom::Value(x),
            cases: BTreeMap::from([(
                0,
                Edge {
                    target: first,
                    args: vec![],
                },
            )]),
            default: Some(Edge {
                target: second,
                args: vec![],
            }),
        });
        module.add_node(Node::LetCont {
            continuations: vec![first, second],
            body: switch,
        })
    })
}

/// Two arms equal up to nothing they bind are one: the module holds one addition, every edge reaches it, and a second run finds nothing.
///
/// Mutation-checked: with every member left as it is, two additions stay.
#[test]
fn two_equal_arms_are_kept_once() {
    let mut module = two_arms(|x, _| (x, x), false);
    assert!(merge_equal_continuations(&mut module));
    module.verify().unwrap();
    assert_eq!(additions(&module), 1, "{module}");
    assert!(!merge_equal_continuations(&mut module), "{module}");
}

/// Two arms reading different values from outside are no class here, and two arms of different lineages are none either.
#[test]
fn arms_reading_apart_or_of_two_lineages_stay_two() {
    for (mut module, case) in [
        (two_arms(|x, y| (x, y), false), "reading `x` and `y`"),
        (two_arms(|x, _| (x, x), true), "of two lineages"),
    ] {
        assert!(!merge_equal_continuations(&mut module), "{case}");
        assert_eq!(additions(&module), 2, "{case}");
    }
}

/// Two equal arms, each a match of two equal arms of its own: the outer class is kept once and the inner class inside the one kept is too, in one run, which a second run finds nothing more of; what the arm that goes defined goes with it.
#[test]
fn a_class_inside_a_member_kept_is_merged_in_the_same_run() {
    let mut module = main_of(|module, x, _, main_return| {
        let mut outer = Vec::new();
        for _ in 0..2 {
            let (first, second) = (module.reserve_continuation(), module.reserve_continuation());
            let first_body = adds_one(module, x, main_return);
            let second_body = adds_one(module, x, main_return);
            let inner = split(module, x, (first, first_body), (second, second_body), None);
            outer.push((module.reserve_continuation(), inner));
        }
        split(module, x, outer[0], outer[1], None)
    });
    assert_eq!(additions(&module), 4);
    assert!(merge_equal_continuations(&mut module));
    module.verify().unwrap();
    assert_eq!(additions(&module), 1, "{module}");
    assert!(!merge_equal_continuations(&mut module), "{module}");
}

/// Two equal arms that each jump to a continuation bound beside them are one, kept where that continuation is in scope: what every member reads is in scope wherever each is bound, so at the innermost `LetCont` enclosing them all.
///
/// Mutation-checked: with no class merged, the merge answers nothing.
#[test]
fn arms_reaching_a_continuation_bound_beside_them_are_kept_where_it_is_in_scope() {
    let mut module = main_of(|module, x, _, main_return| {
        let (first, second, shared) = (
            module.reserve_continuation(),
            module.reserve_continuation(),
            module.reserve_continuation(),
        );
        let shared_body = adds_one(module, x, main_return);
        define(module, shared, shared_body, None);
        let first_body = jump(module, shared, vec![]);
        let second_body = jump(module, shared, vec![]);
        define(module, first, first_body, None);
        define(module, second, second_body, None);
        let switch = module.add_node(Node::Switch {
            scrutinee: Atom::Value(x),
            cases: BTreeMap::from([(
                0,
                Edge {
                    target: first,
                    args: vec![],
                },
            )]),
            default: Some(Edge {
                target: second,
                args: vec![],
            }),
        });
        module.add_node(Node::LetCont {
            continuations: vec![first, second, shared],
            body: switch,
        })
    });
    assert!(merge_equal_continuations(&mut module));
    module.verify().unwrap();
}

/// A match nested `depth` deep, each level two equal arms each holding the next level, the last adding one: `2^depth` leaves.
fn twins(depth: usize) -> Module {
    fn level(module: &mut Module, x: ValueId, to: ContinuationId, depth: usize) -> NodeId {
        if depth == 0 {
            return adds_one(module, x, to);
        }
        let (first, second) = (module.reserve_continuation(), module.reserve_continuation());
        let first_body = level(module, x, to, depth - 1);
        let second_body = level(module, x, to, depth - 1);
        split(module, x, (first, first_body), (second, second_body), None)
    }
    main_of(|module, x, _, main_return| level(module, x, main_return, depth))
}

/// The tokens the pass writes out for its candidates over `module`, every candidate written once.
fn written(module: &Module) -> usize {
    let tree = Tree::of(module);
    let shapes = Shapes::of(module);
    candidates(module, &tree, &shapes)
        .into_iter()
        .flatten()
        .filter_map(|member| Written::of(module, member))
        .map(|written| written.tokens.len())
        .sum()
}

/// Over a geometric grid of nesting depth, where every level holds a class, what the pass writes out grows as the function's size times its depth, the members of a class being disjoint and a class inside a member at most half its size; and the merge leaves one leaf.
#[test]
fn what_the_pass_writes_grows_as_the_size_times_the_depth() {
    let per_node_level = |depth: usize| {
        let module = twins(depth);
        let size = module.live_nodes().count();
        written(&module) as f64 / (size * depth) as f64
    };
    let (shallow, deep) = (per_node_level(4), per_node_level(10));
    assert!(
        deep <= shallow * 1.5,
        "tokens per node and level: {shallow:.2} at depth 4, {deep:.2} at depth 10"
    );

    let mut module = twins(10);
    assert!(merge_equal_continuations(&mut module));
    module.verify().unwrap();
    assert_eq!(additions(&module), 1);
}
