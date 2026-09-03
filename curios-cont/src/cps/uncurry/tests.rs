use {
    super::{uncurry_returns, uncurryable},
    crate::{
        CpsAtom, CpsCallee, CpsContinuation, CpsEdge, CpsFunId, CpsFunction, CpsLiteral, CpsModule,
        CpsNode, CpsNodeId, CpsValueExpr,
    },
};

/// How the caller of a class member uses what it hands back.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Use {
    /// Applied at arity one, right where it arrives — what the transform absorbs.
    Applied,
    /// Kept, so the closure is a value and the member is inadmissible.
    Stored,
    /// Applied at arity one where it arrives, and applied again inside a function defined below that application — a use no walk that stops at the `LetFun` sees.
    Captured,
    /// Not called from outside the class at all, so nothing observes a width for it directly.
    Unobserved,
}

/// A two-member tail-forwarding chain: `forwarder` returns whatever `leader` returns, by tail-calling it.
///
/// Each member also has its own non-tail caller, so each is observable on its own and the class is a *decision* rather than the only reading available. `use_of` says how each caller treats what it receives, which is what lets one member disagree with the other.
fn chain(leader_use: Use, forwarder_use: Use) -> (CpsModule, CpsFunId, CpsFunId) {
    let mut module = CpsModule::default();

    // The closure both members hand back: it takes the argument the callers apply.
    let step_param = module.add_value(Some("s".into()));
    let step = module.reserve_function();
    let step_sentinel = module.reserve_continuation();
    let step_body = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: step_sentinel,
        args: vec![CpsAtom::Value(step_param)],
    }));
    module.define_function(
        step,
        CpsFunction {
            debug_name: Some("step".into()),
            params: vec![step_param],
            return_cont: step_sentinel,
            body: step_body,
        },
    );

    let leader_param = module.add_value(Some("n".into()));
    let leader = module.reserve_function();
    let leader_sentinel = module.reserve_continuation();
    let returns = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: leader_sentinel,
        args: vec![CpsAtom::Fun(step)],
    }));
    let leader_body = module.add_node(CpsNode::LetFun {
        functions: vec![step],
        body: returns,
    });
    module.define_function(
        leader,
        CpsFunction {
            debug_name: Some("leader".into()),
            params: vec![leader_param],
            return_cont: leader_sentinel,
            body: leader_body,
        },
    );

    // The forwarding edge: a tail call, which is what puts the two in one class.
    let forwarder_param = module.add_value(Some("n".into()));
    let forwarder = module.reserve_function();
    let forwarder_sentinel = module.reserve_continuation();
    let forwarder_body = module.add_node(CpsNode::ApplyFun {
        callee: CpsCallee::Known(leader),
        args: vec![CpsAtom::Value(forwarder_param)],
        return_to: forwarder_sentinel,
    });
    module.define_function(
        forwarder,
        CpsFunction {
            debug_name: Some("forwarder".into()),
            params: vec![forwarder_param],
            return_cont: forwarder_sentinel,
            body: forwarder_body,
        },
    );

    // One entry calling both, each through its own resume, so the two observations are independent.
    let entry = module.reserve_function();
    let entry_sentinel = module.reserve_continuation();
    let argument = module.add_value(Some("argument".into()));

    let caller = |module: &mut CpsModule, callee: CpsFunId, use_of: Use, next: CpsNodeId| {
        let received = module.add_value(Some("received".into()));
        let resume = module.reserve_continuation();
        let mut bound = vec![resume];
        let body = match use_of {
            Use::Applied => {
                // The application resumes past the call, which is the shape `Resume::Retarget` is written for.
                let ignored = module.add_value(Some("ignored".into()));
                let after = module.add_continuation(CpsContinuation {
                    debug_name: None,
                    params: vec![ignored],
                    body: next,
                });
                bound.push(after);
                module.add_node(CpsNode::ApplyFun {
                    callee: CpsCallee::Closure(received),
                    args: vec![CpsAtom::Value(argument)],
                    return_to: after,
                })
            }
            Use::Captured => {
                // The visible half is `Applied`'s shape exactly; the hidden half is a nested function that applies the closure again and escapes into a tuple, so it is neither dead nor inlined away.
                let ignored = module.add_value(Some("ignored".into()));
                let nested = module.reserve_function();
                let nested_sentinel = module.reserve_continuation();
                let nested_body = module.add_node(CpsNode::ApplyFun {
                    callee: CpsCallee::Closure(received),
                    args: vec![CpsAtom::Value(argument)],
                    return_to: nested_sentinel,
                });
                module.define_function(
                    nested,
                    CpsFunction {
                        debug_name: Some("nested".into()),
                        params: vec![],
                        return_cont: nested_sentinel,
                        body: nested_body,
                    },
                );
                let kept = module.add_value(Some("kept".into()));
                let keep = module.add_node(CpsNode::LetValue {
                    result: kept,
                    value: CpsValueExpr::Tuple(vec![CpsAtom::Fun(nested)]),
                    next,
                });
                let define = module.add_node(CpsNode::LetFun {
                    functions: vec![nested],
                    body: keep,
                });
                // Bound inside the resume rather than beside it, since the nested function names the resume's parameter — the `LetCont`-then-site shape `Resume::Jump` is written for.
                let after = module.add_continuation(CpsContinuation {
                    debug_name: None,
                    params: vec![ignored],
                    body: define,
                });
                let apply = module.add_node(CpsNode::ApplyFun {
                    callee: CpsCallee::Closure(received),
                    args: vec![CpsAtom::Value(argument)],
                    return_to: after,
                });
                module.add_node(CpsNode::LetCont {
                    continuations: vec![after],
                    body: apply,
                })
            }
            Use::Unobserved => unreachable!("an unobserved member is given no caller at all"),
            // A tuple field is a use the lattice cannot call an application, which is exactly what makes the member inadmissible.
            Use::Stored => {
                let kept = module.add_value(Some("kept".into()));
                module.add_node(CpsNode::LetValue {
                    result: kept,
                    value: CpsValueExpr::Tuple(vec![CpsAtom::Value(received)]),
                    next,
                })
            }
        };
        module.define_continuation(
            resume,
            CpsContinuation {
                debug_name: None,
                params: vec![received],
                body,
            },
        );
        let call = module.add_node(CpsNode::ApplyFun {
            callee: CpsCallee::Known(callee),
            args: vec![CpsAtom::Value(argument)],
            return_to: resume,
        });
        module.add_node(CpsNode::LetCont {
            continuations: bound,
            body: call,
        })
    };

    let done = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: entry_sentinel,
        args: vec![CpsAtom::Value(argument)],
    }));
    let second = match forwarder_use {
        Use::Unobserved => done,
        use_of => caller(&mut module, forwarder, use_of, done),
    };
    let first = match leader_use {
        Use::Unobserved => second,
        use_of => caller(&mut module, leader, use_of, second),
    };
    let bound = module.add_node(CpsNode::LetValue {
        result: argument,
        value: CpsValueExpr::Literal(CpsLiteral::Nat(1)),
        next: first,
    });
    // Both members are introduced here, or a `Known` call to either names a function out of scope.
    let entry_body = module.add_node(CpsNode::LetFun {
        functions: vec![leader, forwarder],
        body: bound,
    });
    module.define_function(
        entry,
        CpsFunction {
            debug_name: Some("entry".into()),
            params: vec![],
            return_cont: entry_sentinel,
            body: entry_body,
        },
    );
    module.set_entry(entry);

    // A hand-built module is worth nothing if it is not the module the pass would see, and the scoping rule above is exactly what a `define_function` without a binder gets wrong silently.
    module.verify().expect("the fixture is valid CPS");
    (module, leader, forwarder)
}

/// How many parameters each member gained.
fn widths(module: &CpsModule, leader: CpsFunId, forwarder: CpsFunId) -> (usize, usize) {
    (
        module.function(leader).unwrap().params.len(),
        module.function(forwarder).unwrap().params.len(),
    )
}

/// Two members joined by tail-forwarding are rewritten as one unit.
///
/// This is what the class is for: tail-forwarding means the two return through one another, so uncurrying one without the other would leave a caller passing an argument its callee never grew a parameter for — which is exactly the arity mismatch an earlier attempt crashed on.
#[test]
fn a_tail_forwarded_chain_is_uncurried_together() {
    let (mut module, leader, forwarder) = chain(Use::Applied, Use::Applied);
    assert_eq!(widths(&module, leader, forwarder), (1, 1));

    let admissible = uncurryable(&module);
    assert_eq!(
        (admissible.get(&leader), admissible.get(&forwarder)),
        (Some(&1), Some(&1)),
        "both members are admissible at width one",
    );
    assert!(uncurry_returns(&mut module), "the class is admissible");
    assert_eq!(
        widths(&module, leader, forwarder),
        (2, 2),
        "both members take the absorbed argument, not just the one the caller named",
    );
    module.verify().expect("the rewrite leaves valid CPS");
}

/// A member nothing calls from outside the class takes its width from the class, and is rewritten with it.
///
/// This is what "propagate the decision across tail-forwarding" has to mean. Tail-forwarding makes the members return through one another, so the class shares one stream of returned closures; a member with no caller of its own contributes no reading of that stream and contradicts none, and requiring it to be observed independently would decline a chain for a reason that is not one. `state_monad`'s `{/loop, /loop/2}` only escaped that because each of the two happens to have a caller.
///
/// What such a member is *not* excused is [`super::rewritable`], which its inherited width says nothing about — and which, before this propagated, no unobserved member was ever asked.
#[test]
fn a_member_observed_only_through_forwarding_takes_the_class_width() {
    for (label, uses) in [
        ("nothing calls the leader", (Use::Unobserved, Use::Applied)),
        (
            "nothing calls the forwarder",
            (Use::Applied, Use::Unobserved),
        ),
    ] {
        let (mut module, leader, forwarder) = chain(uses.0, uses.1);
        let unobserved = match uses.0 {
            Use::Unobserved => leader,
            _ => forwarder,
        };
        // The mechanism, not just the outcome: no width is observed for this member, and it is rewritten regardless.
        assert!(
            !uncurryable(&module).contains_key(&unobserved),
            "{label}: no width is observed for it",
        );
        assert!(
            uncurry_returns(&mut module),
            "{label}: the class is admissible",
        );
        assert_eq!(
            widths(&module, leader, forwarder),
            (2, 2),
            "{label}: and both members take the absorbed argument",
        );
        module.verify().expect("the rewrite leaves valid CPS");
    }
}

/// And one member that cannot be uncurried leaves the whole class alone, whichever end of the chain it sits at.
#[test]
fn a_chain_declines_when_either_member_cannot() {
    for (label, uses) in [
        (
            "the forwarder's caller keeps it",
            (Use::Applied, Use::Stored),
        ),
        ("the leader's caller keeps it", (Use::Stored, Use::Applied)),
    ] {
        let (mut module, leader, forwarder) = chain(uses.0, uses.1);
        assert!(
            !uncurryable(&module).contains_key(&leader)
                || !uncurryable(&module).contains_key(&forwarder),
            "{label}: one member is inadmissible",
        );
        assert!(
            !uncurry_returns(&mut module),
            "{label}: so the class declines"
        );
        assert_eq!(
            widths(&module, leader, forwarder),
            (1, 1),
            "{label}: and the admissible member is left alone with it",
        );
    }
}

/// A closure the caller applies once and also captures in a function defined below the application is a value that outlives the site, and the member is inadmissible — at either end of the chain, since the class path plans the declined member on its class-mate's width and has to see the hidden application too.
///
/// It was admitted: the admission walk stopped at the `LetFun`, saw one application, and the nested function went on applying the absorbed answer.
#[test]
fn a_closure_captured_by_a_nested_function_declines_uncurrying() {
    for (label, uses) in [
        (
            "the leader's caller captures it",
            (Use::Captured, Use::Applied),
        ),
        (
            "the forwarder's caller captures it",
            (Use::Applied, Use::Captured),
        ),
    ] {
        let (mut module, leader, forwarder) = chain(uses.0, uses.1);
        let captured = match uses.0 {
            Use::Captured => leader,
            _ => forwarder,
        };
        assert!(
            !uncurryable(&module).contains_key(&captured),
            "{label}: the capture is a use the site cannot absorb",
        );
        assert!(
            !uncurry_returns(&mut module),
            "{label}: so the class declines"
        );
        assert_eq!(
            widths(&module, leader, forwarder),
            (1, 1),
            "{label}: and neither member is rewritten",
        );
    }
}

/// The returned closure is applied — but behind a forwarding jump, in a join point the resume hands it to. The interprocedural demand lattice reads that as `Applied`; this transform moves the application it finds in the resume itself, so admission must recompute the sole-local-application fact syntactically and decline here.
#[test]
fn a_forwarded_application_declines_uncurrying() {
    let mut module = CpsModule::default();

    let inner_param = module.add_value(Some("inner/param".into()));
    let inner = module.reserve_function();
    let inner_ret = module.reserve_continuation();
    let inner_exit = module.add_node(CpsNode::Exit { value: None });
    module.define_function(
        inner,
        CpsFunction {
            debug_name: Some("inner".into()),
            params: vec![inner_param],
            return_cont: inner_ret,
            body: inner_exit,
        },
    );

    let produced_param = module.add_value(Some("producer/param".into()));
    let producer = module.reserve_function();
    let producer_ret = module.reserve_continuation();
    let producer_body = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: producer_ret,
        args: vec![CpsAtom::Fun(inner)],
    }));
    module.define_function(
        producer,
        CpsFunction {
            debug_name: Some("producer".into()),
            params: vec![produced_param],
            return_cont: producer_ret,
            body: producer_body,
        },
    );

    let argument = module.add_value(Some("caller/argument".into()));
    let caller = module.reserve_function();
    let caller_ret = module.reserve_continuation();
    let received = module.add_value(Some("resume/closure".into()));
    let forwarded = module.add_value(Some("join/closure".into()));
    let resume = module.reserve_continuation();
    let join = module.reserve_continuation();

    let apply = module.add_node(CpsNode::ApplyFun {
        callee: CpsCallee::Closure(forwarded),
        args: vec![CpsAtom::Literal(CpsLiteral::Nat(1))],
        return_to: caller_ret,
    });
    module.define_continuation(
        join,
        CpsContinuation {
            debug_name: Some("join".into()),
            params: vec![forwarded],
            body: apply,
        },
    );
    let forward = module.add_node(CpsNode::ApplyCont(CpsEdge {
        target: join,
        args: vec![CpsAtom::Value(received)],
    }));
    module.define_continuation(
        resume,
        CpsContinuation {
            debug_name: Some("resume".into()),
            params: vec![received],
            body: forward,
        },
    );
    let call = module.add_node(CpsNode::ApplyFun {
        callee: CpsCallee::Known(producer),
        args: vec![CpsAtom::Value(argument)],
        return_to: resume,
    });
    let body = module.add_node(CpsNode::LetCont {
        continuations: vec![join, resume],
        body: call,
    });
    module.define_function(
        caller,
        CpsFunction {
            debug_name: Some("caller".into()),
            params: vec![argument],
            return_cont: caller_ret,
            body,
        },
    );
    module.set_entry(caller);

    assert!(
        !uncurryable(&module).contains_key(&producer),
        "an application behind a forwarding jump is not one the transform can move",
    );
}
