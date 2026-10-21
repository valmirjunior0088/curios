use {
    super::{Storage, representation},
    crate::{
        Atom, Callee, Continuation, ContinuationId, Edge, Function, FunctionId, Intrinsic, Literal,
        Module, Node, NodeId, Register, Repr, ValueDef, ValueExpr, ValueId,
    },
    curios_num::{Floating, Natural, Rounding},
};

/// Make `body` the entry function's body, with `params` as its parameters. The return continuation is reserved and never defined, which is exactly what a function's return sentinel is.
fn entry(module: &mut Module, params: Vec<ValueId>, body: NodeId) {
    let function = module.reserve_function();
    let return_cont = module.reserve_continuation();

    module.define_function(
        function,
        Function {
            debug_name: Some("main".into()),
            params,
            return_cont,
            body,
            droppable: false,
        },
    );
    module.set_entry(function);
}

/// Route `body` through a continuation taking `params`, and answer the node that enters it. A filler stands in for the incoming values: it inhabits whatever carrier the parameter settles at, so entering the continuation demands nothing and withholds nothing of its own.
fn through_continuation(module: &mut Module, params: Vec<ValueId>, body: NodeId) -> NodeId {
    let head = module.reserve_continuation();
    let args = params.iter().map(|_| Atom::Filler).collect();

    module.define_continuation(
        head,
        Continuation {
            debug_name: Some("head".into()),
            params,
            body,
        },
    );

    module.add_node(Node::ApplyCont(Edge { target: head, args }))
}

/// Define the reserved `function` as `name(params)`, its body built by `body` against the function's own return sentinel, which it is handed.
fn define(
    module: &mut Module,
    function: FunctionId,
    name: &str,
    params: Vec<ValueId>,
    body: impl FnOnce(&mut Module, ContinuationId) -> NodeId,
) {
    let return_cont = module.reserve_continuation();
    let body = body(module, return_cont);
    module.define_function(
        function,
        Function {
            debug_name: Some(name.into()),
            params,
            return_cont,
            body,
            droppable: false,
        },
    );
}

/// Define the reserved `continuation` taking `params`, its body `body`.
fn resume(module: &mut Module, continuation: ContinuationId, params: Vec<ValueId>, body: NodeId) {
    module.define_continuation(
        continuation,
        Continuation {
            debug_name: Some("resume".into()),
            params,
            body,
        },
    );
}

/// The `Nat` literal `value`.
fn nat(value: u32) -> Atom {
    Atom::Literal(Literal::Nat(Natural::from(value)))
}

/// Jump to a reserved-but-undefined continuation — the return sentinel, which demands nothing of what it carries.
fn finish(module: &mut Module, args: Vec<Atom>) -> NodeId {
    let sentinel = module.reserve_continuation();
    module.add_node(Node::ApplyCont(Edge {
        target: sentinel,
        args,
    }))
}

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

/// The soundness half of the edge rule. A sum is unbounded, so it is a reference that narrowing to a word would saturate; a parameter it reaches must therefore stay a reference however its uses read it, or the edge would coerce a value past the i31 into a word and lose it.
/// A parameter some argument of which nothing bounds is never a word: the entry's own parameter, a reference the host passes, plus one.
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

/// A call's result reaches its continuation's parameter as a reference where the callee's results are fixed by something other than its own return edges: here a tail call through a closure, which returns at the closure type's one uniform result. The parameter is open to its uses in general, and this is what withdraws it.
#[test]
fn a_call_result_a_closure_tail_call_fixes_stays_boxed() {
    let mut module = Module::new();
    let closure = module.add_value(Some("closure".into()));
    let returned = module.add_value(Some("returned".into()));
    let read = module.add_value(Some("read".into()));
    let callee = module.reserve_function();
    let after = module.reserve_continuation();

    define(
        &mut module,
        callee,
        "callee",
        vec![closure],
        |module, sentinel| {
            module.add_node(Node::ApplyFun {
                callee: Callee::Closure(closure),
                args: vec![],
                return_to: sentinel,
            })
        },
    );
    let done = finish(&mut module, vec![Atom::Value(read)]);
    let after_body = float_read(&mut module, read, returned, done);
    resume(&mut module, after, vec![returned], after_body);
    let outside = module.add_value(Some("outside".into()));
    let body = module.add_node(Node::ApplyFun {
        callee: Callee::Known(callee),
        args: vec![Atom::Value(outside)],
        return_to: after,
    });
    entry(&mut module, vec![outside], body);

    let decided = representation(&module);
    assert_eq!(decided.values[&returned], Storage::Boxed);
    assert_eq!(decided.results[&callee], vec![Storage::Boxed]);
}

/// A join whose parameter is read as a float, entered on one edge with a float a definition produced and on the other with `other`; `params` are the entry's own. Answers the parameter.
fn float_join(module: &mut Module, params: Vec<ValueId>, flag: ValueId, other: Atom) -> ValueId {
    let joined = module.add_value(Some("joined".into()));
    let read = module.add_value(Some("read".into()));
    let float = module.add_value(Some("float".into()));

    let join = module.reserve_continuation();
    let done = finish(module, vec![Atom::Value(read)]);
    let join_body = module.add_node(Node::LetIntrinsic {
        result: read,
        op: Intrinsic::FltAdd(Rounding::TiesToEven),
        args: vec![
            Atom::Value(joined),
            Atom::Literal(Literal::Flt(Floating::from(1.0))),
        ],
        next: done,
    });
    module.define_continuation(
        join,
        Continuation {
            debug_name: Some("join".into()),
            params: vec![joined],
            body: join_body,
        },
    );

    let branch = module.add_node(Node::Switch {
        scrutinee: Atom::Value(flag),
        cases: [(
            0,
            Edge {
                target: join,
                args: vec![Atom::Value(float)],
            },
        )]
        .into(),
        default: Some(Edge {
            target: join,
            args: vec![other],
        }),
    });
    let body = module.add_node(Node::LetIntrinsic {
        result: float,
        op: Intrinsic::FltAdd(Rounding::TiesToEven),
        args: vec![
            Atom::Literal(Literal::Flt(Floating::from(2.0))),
            Atom::Literal(Literal::Flt(Floating::from(3.0))),
        ],
        next: branch,
    });
    entry(module, params, body);

    joined
}

/// The soundness half of the float rule. A reference states no shape: the one reaching this join may be the error a variant's other constructor holds in the payload slot the float shares, or the string a type computes on the other branch. The join reads a float under one branch only and the edge coerces before any branch is taken, so a register would cast the reference, and the parameter stays one.
#[test]
fn a_parameter_a_reference_reaches_is_never_held_in_a_float_register() {
    let mut module = Module::new();
    let flag = module.add_value(Some("flag".into()));
    let reference = module.add_value(Some("reference".into()));
    let joined = float_join(
        &mut module,
        vec![flag, reference],
        flag,
        Atom::Value(reference),
    );

    assert_eq!(representation(&module).values[&joined], Storage::Boxed);
}

/// The precision half: where every edge hands the parameter a float, a definition's on one and a literal on the other, the register is kept.
#[test]
fn a_parameter_every_edge_hands_a_float_is_held_in_a_register() {
    let mut module = Module::new();
    let flag = module.add_value(Some("flag".into()));
    let joined = float_join(
        &mut module,
        vec![flag],
        flag,
        Atom::Literal(Literal::Flt(Floating::from(4.0))),
    );

    assert_eq!(
        representation(&module).values[&joined],
        Storage::Raw(Repr::Flt)
    );
}

/// A reference whose type named a float states it, and feeds the register by one unbox on the edge: the stated half of the float rule, which the unstated reference above is the other side of.
#[test]
fn a_parameter_a_stated_float_reaches_is_held_in_a_register() {
    let mut module = Module::new();
    let flag = module.add_value(Some("flag".into()));
    let stated = module.add_value_def(ValueDef {
        debug_name: Some("stated".into()),
        register: Some(Register::Float),
    });
    let joined = float_join(&mut module, vec![flag, stated], flag, Atom::Value(stated));

    let representation = representation(&module);
    assert_eq!(representation.values[&joined], Storage::Raw(Repr::Flt));
    assert_eq!(
        representation.values[&stated],
        Storage::Boxed,
        "the stated value keeps the reference it arrives as"
    );
}

/// A stated word feeds a float register no more than an unstated reference does.
#[test]
fn a_parameter_a_stated_word_reaches_is_never_held_in_a_float_register() {
    let mut module = Module::new();
    let flag = module.add_value(Some("flag".into()));
    let stated = module.add_value_def(ValueDef {
        debug_name: Some("stated".into()),
        register: Some(Register::Word),
    });
    let joined = float_join(&mut module, vec![flag, stated], flag, Atom::Value(stated));

    assert_eq!(representation(&module).values[&joined], Storage::Boxed);
}

/// Read `value` as a float into `result`, then go on to `next`.
fn float_read(module: &mut Module, result: ValueId, value: ValueId, next: NodeId) -> NodeId {
    module.add_node(Node::LetIntrinsic {
        result,
        op: Intrinsic::FltAdd(Rounding::TiesToEven),
        args: vec![
            Atom::Value(value),
            Atom::Literal(Literal::Flt(Floating::from(1.0))),
        ],
        next,
    })
}

/// Bind `result` to a float a definition produces, then go on to `next`.
fn float_made(module: &mut Module, result: ValueId, next: NodeId) -> NodeId {
    module.add_node(Node::LetIntrinsic {
        result,
        op: Intrinsic::FltAdd(Rounding::TiesToEven),
        args: vec![
            Atom::Literal(Literal::Flt(Floating::from(2.0))),
            Atom::Literal(Literal::Flt(Floating::from(3.0))),
        ],
        next,
    })
}

/// Define `callee(x)`, which reads `x` as a float and returns the sum, and answer `x`.
fn float_reader(module: &mut Module, callee: FunctionId) -> ValueId {
    let param = module.add_value(Some("x".into()));
    let sum = module.add_value(Some("sum".into()));
    define(module, callee, "reader", vec![param], |module, sentinel| {
        let done = module.add_node(Node::ApplyCont(Edge {
            target: sentinel,
            args: vec![Atom::Value(sum)],
        }));
        float_read(module, sum, param, done)
    });
    param
}

/// Call `callee(arg)` from a continuation that reads the result as a float, and answer the call and the result's parameter.
fn float_call(module: &mut Module, callee: FunctionId, arg: Atom) -> (NodeId, ValueId) {
    let returned = module.add_value(Some("returned".into()));
    let read = module.add_value(Some("read".into()));
    let after = module.reserve_continuation();
    let done = finish(module, vec![Atom::Value(read)]);
    let after_body = float_read(module, read, returned, done);
    resume(module, after, vec![returned], after_body);
    let call = module.add_node(Node::ApplyFun {
        callee: Callee::Known(callee),
        args: vec![arg],
        return_to: after,
    });
    (call, returned)
}

/// A function that neither escapes nor is the entry is entered only by calls naming it, each handing over its arguments, so its parameter is a join whose edges are those calls, decided as a loop header's is. Every call here hands it a float a definition produced, and its body reads it as one.
#[test]
fn a_known_parameter_every_call_hands_a_float_is_held_in_a_register() {
    let mut module = Module::new();
    let callee = module.reserve_function();
    let param = float_reader(&mut module, callee);
    let float = module.add_value(Some("float".into()));
    let (call, _) = float_call(&mut module, callee, Atom::Value(float));
    let body = float_made(&mut module, float, call);
    entry(&mut module, vec![], body);

    let decided = representation(&module).values;
    assert_eq!(decided[&param], Storage::Raw(Repr::Flt));
    assert_eq!(decided[&float], Storage::Raw(Repr::Flt));
}

/// The soundness half: a call handing the parameter a reference states nothing of what the reference holds, so the parameter stays one, as a join's does.
#[test]
fn a_known_parameter_a_reference_reaches_stays_boxed() {
    let mut module = Module::new();
    let callee = module.reserve_function();
    let param = float_reader(&mut module, callee);
    let outside = module.add_value(Some("outside".into()));
    let (call, _) = float_call(&mut module, callee, Atom::Value(outside));
    entry(&mut module, vec![outside], call);

    assert_eq!(representation(&module).values[&param], Storage::Boxed);
}

/// A function named as a value escapes: a closure calls it through its arity's uniform signature, whatever every call naming it hands over.
#[test]
fn an_escaping_functions_parameter_stays_boxed() {
    let mut module = Module::new();
    let callee = module.reserve_function();
    let param = float_reader(&mut module, callee);
    let float = module.add_value(Some("float".into()));
    let named = module.add_value(Some("named".into()));
    let (call, _) = float_call(&mut module, callee, Atom::Value(float));
    let name = module.add_node(Node::LetValue {
        result: named,
        value: ValueExpr::Tuple(vec![Atom::Fun(callee)]),
        next: call,
    });
    let body = float_made(&mut module, float, name);
    entry(&mut module, vec![], body);

    let decided = representation(&module);
    assert_eq!(decided.values[&param], Storage::Boxed);
    assert_eq!(decided.results[&callee], vec![Storage::Boxed]);
}

/// A result slot is a join whose edges are the class's return edges and whose uses are the resume parameters of every call into it. Every return here hands it a float, and the caller reads the result as one, so the slot and the resume parameter are both held in a register.
#[test]
fn a_result_every_return_hands_a_float_is_held_in_a_register() {
    let mut module = Module::new();
    let callee = module.reserve_function();
    float_reader(&mut module, callee);
    let float = module.add_value(Some("float".into()));
    let (call, returned) = float_call(&mut module, callee, Atom::Value(float));
    let body = float_made(&mut module, float, call);
    entry(&mut module, vec![], body);

    let decided = representation(&module);
    assert_eq!(decided.values[&returned], Storage::Raw(Repr::Flt));
    assert_eq!(decided.results[&callee], vec![Storage::Raw(Repr::Flt)]);
}

/// One call reads the result as a float and another only stores it in a tuple. The slot is raw, since a use demands it, and the second resume parameter stays boxed: the emitter boxes the result once as it enters, rather than at each use.
#[test]
fn a_resume_parameter_with_only_boxed_uses_beside_a_raw_result_stays_boxed() {
    let mut module = Module::new();
    let callee = module.reserve_function();
    float_reader(&mut module, callee);
    let float = module.add_value(Some("float".into()));
    let stored = module.add_value(Some("stored".into()));
    let tuple = module.add_value(Some("tuple".into()));
    let after = module.reserve_continuation();
    let done = finish(&mut module, vec![Atom::Value(tuple)]);
    let after_body = module.add_node(Node::LetValue {
        result: tuple,
        value: ValueExpr::Tuple(vec![Atom::Value(stored)]),
        next: done,
    });
    resume(&mut module, after, vec![stored], after_body);
    let storing = module.add_node(Node::ApplyFun {
        callee: Callee::Known(callee),
        args: vec![Atom::Value(float)],
        return_to: after,
    });
    let (reading, returned) = float_call(&mut module, callee, Atom::Value(float));
    let reading = edge_to(&mut module, reading);
    let storing = edge_to(&mut module, storing);
    let both = module.add_node(Node::Switch {
        scrutinee: Atom::Literal(Literal::Nat(Natural::from(0u32))),
        cases: [(0, reading)].into(),
        default: Some(storing),
    });
    let body = float_made(&mut module, float, both);
    entry(&mut module, vec![], body);

    let decided = representation(&module);
    assert_eq!(decided.results[&callee], vec![Storage::Raw(Repr::Flt)]);
    assert_eq!(decided.values[&returned], Storage::Raw(Repr::Flt));
    assert_eq!(decided.values[&stored], Storage::Boxed);
}

/// One call reads the result as a float and another reads it not at all. The unread resume parameter has no use to disagree with the register, so it is held as the slot hands it over, and nothing boxes a result no one reads.
#[test]
fn a_resume_parameter_nothing_reads_is_held_as_its_slot_hands_it_over() {
    let mut module = Module::new();
    let callee = module.reserve_function();
    float_reader(&mut module, callee);
    let float = module.add_value(Some("float".into()));
    let unread = module.add_value(Some("unread".into()));
    let after = module.reserve_continuation();
    let done = finish(&mut module, vec![]);
    resume(&mut module, after, vec![unread], done);
    let discarding = module.add_node(Node::ApplyFun {
        callee: Callee::Known(callee),
        args: vec![Atom::Value(float)],
        return_to: after,
    });
    let (reading, _) = float_call(&mut module, callee, Atom::Value(float));
    let reading = edge_to(&mut module, reading);
    let discarding = edge_to(&mut module, discarding);
    let both = module.add_node(Node::Switch {
        scrutinee: Atom::Literal(Literal::Nat(Natural::from(0u32))),
        cases: [(0, reading)].into(),
        default: Some(discarding),
    });
    let body = float_made(&mut module, float, both);
    entry(&mut module, vec![], body);

    let decided = representation(&module);
    assert_eq!(decided.results[&callee], vec![Storage::Raw(Repr::Flt)]);
    assert_eq!(decided.values[&unread], Storage::Raw(Repr::Flt));
}

/// An argumentless edge into a continuation whose body is `body`.
fn edge_to(module: &mut Module, body: NodeId) -> Edge {
    let target = module.reserve_continuation();
    module.define_continuation(
        target,
        Continuation {
            debug_name: Some("arm".into()),
            params: vec![],
            body,
        },
    );
    Edge {
        target,
        args: vec![],
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
